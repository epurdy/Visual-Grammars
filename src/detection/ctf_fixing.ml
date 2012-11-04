open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph
open Filtration

(*
  - line_costs + insert_lexical_costs need to be merged, and
  modified. this will slow things down slightly because lifting from
  the bottom takes time, but honestly it doesn't take that much time.
  modify linecost code to treat edges as marked when they go the wrong
  way relative to a chosen thing. might have to integrate with another
  fn, since linecost produces undirected costs (should be done now)

  - need to think briefly if the polarity on the masking is right

*)

module I = Image

exception Parse_failure

let pf = 0.80 (* probability of observing an edge pixel in the foreground *)
let pb = 0.10 (* probability of observing an edge pixel in the background *)
let length_term = -. (log( (1. -. pf) /. (1. -. pb) )) (* negative log likelihood ratio *)
let edge_term = -. (log( pf *. (1. -. pb) /. ((1. -. pf) *. pb) )) (* negative log likelihood ratio *)

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
let canon p q = if p > q then q,p else p,q

type sid = int
type cid = int
type pid = int
type pt = int * int

type image_parse_data = {
  costs: (sid*pid*pid, float) hash;
  bestleftcost: (sid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
}

type outside_parse_data = {
  ocosts: ((sid*pid*pid), float) hash;
  bestocost: ((sid*pid), float) hash;
}

type image_parse_tree = {
  nt: sid;
  bg: pid;
  en: pid;
  yield: (pid*pid) list;
(*   mutable simple: bool; *)
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

type thing = {
  (* do we really want parsedata here? prob not... *)
  parsedata: image_parse_data;
  edgemap: bool Image.t;
  usedpixels: int Image.t;
  mutable directions: (int*int) array; (* q-p *)
  mutable parent_truecost: float;
  mutable iteration: int;
}

let draw_curve im curve closed color = 
  let n = Array.length curve in
  let maxi = if closed then n-1 else n-2 in
  for i = 0 to maxi do
    Draw.draw_line im curve.(i) curve.((i+1) mod n) color;
  done

let new_outside_data (nsyms,npts) =
  let bigsize = min (nsyms*npts*npts) 100000 in 
  {ocosts = mkhash bigsize;
   bestocost = mkhash (nsyms*npts);
  }

let new_parse_data (nsyms,npts) = 
  let bigsize = min (nsyms*npts*npts) 100000 in 
  {costs = mkhash bigsize;
   bestleftcost = mkhash (nsyms*npts);
   split = mkhash bigsize;
   thisbest = mkhash nsyms;
   thisbestqual = mkhash nsyms;
  }

let new_scene edgemap ptruecost = 
  {iteration = -1; 
   parsedata = new_parse_data (1,1);
   edgemap = edgemap;
   usedpixels = I.map (fun x -> (-1)) edgemap;
   directions = [| |];
   parent_truecost = ptruecost;
  }

let copy_parse_data pdata = 
  {costs = Hashtbl.copy pdata.costs;
   bestleftcost = Hashtbl.copy pdata.bestleftcost;
   split = Hashtbl.copy pdata.split;
   thisbest = Hashtbl.copy pdata.thisbest;   
   thisbestqual = Hashtbl.copy pdata.thisbestqual;
  }

let copy_scene scene =
  {iteration = -1;
   parsedata = copy_parse_data scene.parsedata;
   edgemap = Image.copy scene.edgemap;
   usedpixels = Image.copy scene.usedpixels;
   directions = Array.copy scene.directions;
   parent_truecost = scene.parent_truecost;
  }

let add_direction scene p q =
  let scene = copy_scene scene in
  let newid = Array.length scene.directions in
  let line = List.tl (Draw.line p q) in
    List.iter 
      begin fun r ->
	if I.get scene.usedpixels r < 0 then begin 
	  I.set scene.usedpixels newid r;
	end
      end
      line;
    scene.directions <-
      Array.init (newid+1)
      begin fun i -> 
	if i < newid then
	  scene.directions.(i)
	else begin 
	  let (px,py), (qx,qy) = p,q in
	    printf "add_dir: p=(%d,%d), q=(%d,%d)\n%!" px py qx qy;
	    (qx-px,qy-py)
	end
      end;
    scene

let check_direction (vx,vy) (dx,dy) =
  let dprod = dx*vx + dy*vy in
    (dprod >= 0)


let draw_parse ptree points edgemap =
  let im = Image.map (function true -> (0,0,0) | false -> (255,255,255)) edgemap in
  let curve = List.map (fun (pid,qid) -> points.(pid)) ptree.yield in
    draw_curve im (Array.of_list curve) true (255,0,0);
    im

let rec harvest_best_tree gram pdata nt (bg,en) = 
  printf "harvest %d -> p%d p%d\n%!" nt bg en;
  match pdata.split >>! ((nt,bg,en),None) with
      Some (cid, q) ->
	let prod = Frozen.get_composition gram cid in
	let left = harvest_best_tree gram pdata prod.leftsid (bg,q) in
	let right = harvest_best_tree gram pdata prod.rightsid (q,en) in
	  {nt=nt; bg=bg; en=en;
	   yield = left.yield @ right.yield;
	   left = Some left;
	   right = Some right;
	   qual = pdata.costs >>! ((nt,bg,en), infinity);
	   thesplit = Some (cid,q);
	  }
    | None ->
	{nt=nt; bg=bg; en=en;
	 yield = [(bg, en)];
	 left = None;
	 right = None;
	 qual = pdata.costs >>! ((nt,bg,en), infinity);
	 thesplit = None;
	}

let harvest_best_tree_overall gram pdata = 
  try
    let start = Frozen.start gram in
    let p,_ (* also p *) = pdata.thisbest >> start.sid in
    let rv = harvest_best_tree gram pdata start.sid (p, p) in
      rv
  with _ ->
    raise Parse_failure

let hash_of_parse_tree ptree = 
  let h = mkhash 100 in
  let h2 = mkhash 100 in
  let h3 = mkhash 100 in
  let rec proc t = 
    h2 << ((t.bg, t.en, t.nt), ());
    h3 << ((t.bg, t.nt), ());
    match t.thesplit with
	Some (cid,q) ->
	  h << ((t.bg, q, t.en, cid), ());
	  printf "Allowed: c%d with p%d,p%d,p%d\n" t.bg q t.en cid;
	  proc (get t.left);
	  proc (get t.right);
      | None ->
	  ()
  in
    proc ptree;
    h, h2, h3

let print_parse_tree t = 
  let rec printer t tabs =
    printf "%s  s%d -> p%d p%d (|yield|=%d)\n" (String.make (2*tabs) ' ') 
      t.nt t.bg t.en 
      (List.length t.yield);
    if t.left != None then
      printer (get t.left) (tabs+1);
    if t.right != None then
      printer (get t.right) (tabs+1)
  in
    printer t 0

let voutside gram points ranks topnodes masking_left masking_right pdata odata =
  let nnodes = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

  let rule_cost prod (p,q,r) = 
    let shape = Shape.shape_of_ints_bme points.(p) points.(q) points.(r) in
      Models.Simple.prod_cost prod shape 
  in

    (* the start symbol has a trivial outer tree at every point! *)
    Frozen.iter_symbols_rev gram 
      begin fun sym ->
	if sym.startable then begin 
	  Array.iter
	    begin fun pid ->
	      odata.ocosts << ((sym.sid, pid, pid), 0.);
	      odata.bestocost << ((sym.sid, pid), 0.);
	    end topnodes;
	end
      end;

    Frozen.iter_symbols gram
      begin fun sym ->
	Frozen.iter_decompositions gram sym
	  begin fun prod ->
	    printf "\n<<< s%d -> s%d s%d\n%!" prod.topsid prod.leftsid prod.rightsid;

	    Array.iteri begin fun pnum pid ->
	      printf "  nt=%d/%d p=%d/%d\n%!" sym.sid nsymbols pnum nnodes;

	      Array.iter begin fun rid -> 
		if odata.ocosts >>! ((prod.topsid, pid, rid), infinity) < infinity then begin

		  Array.iter begin fun qid ->
		    let rulecost = rule_cost prod (pid,qid,rid) in
		    let cost_left = (odata.ocosts >>! ((prod.topsid, pid, rid), infinity)) +.
		      (pdata.costs >>! ((prod.rightsid,qid,rid), infinity)) +.
		      rulecost
		    in
		    let cost_right = (odata.ocosts >>! ((prod.topsid, pid, rid), infinity)) +.
		      (pdata.costs >>! ((prod.leftsid,pid,qid), infinity)) +.
		      rulecost
		    in
		      if masking_left pid qid rid prod cost_left then begin
			if cost_left < (odata.ocosts >>! ((prod.leftsid, pid, qid), infinity)) then begin
			  odata.ocosts << ((prod.leftsid, pid, qid), cost_left);

			  if cost_left < (odata.bestocost >>! ((prod.leftsid, pid), infinity)) then begin 
			    odata.bestocost << ((prod.leftsid, pid), cost_left);
			  end
			end;
		      end;

		      if masking_right pid qid rid prod cost_right then begin
			if cost_right < (odata.ocosts >>! ((prod.rightsid, qid, rid), infinity)) then begin
			  odata.ocosts << ((prod.rightsid, qid, rid), cost_right);

			  if cost_right < (odata.bestocost >>! ((prod.rightsid, qid), infinity)) then begin
			    odata.bestocost << ((prod.rightsid, qid), cost_right);
			  end
			end
		      end;
		  end topnodes;
		end;
	      end topnodes;
	    end topnodes;
	  end;
      end


let vinside gram points ranks topnodes masking masking2 masking3 data showwork prefix dispim name ubound = 
  let npts = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

  let nmasktot, nnomask, nmask2tot, nnomask2, nmask3tot, nnomask3 = 
    ref 0, ref 0, ref 0, ref 0, ref 0, ref 0 
  in

  let rule_cost pdata gram sym prod (p,q,r) = 
    let leftkey, rightkey = (prod.leftsid, p, q), (prod.rightsid, q, r) in
    let leftcost  = data.costs >>! (leftkey,  infinity) in
    let rightcost = data.costs >>! (rightkey, infinity) in
    let shape = Shape.shape_of_ints_bme points.(p) points.(q) points.(r) in
    let shapecost = Models.Simple.prod_cost prod shape in
       leftcost +. rightcost +. shapecost
  in

  let tryout sid pid qid qual thesplit = 
      (* vvvv this was <=, not sure why... *)
      (* (prevents key error when all costs infinite) *)
      if qual < (data.costs >>! ((sid, pid, qid), infinity)) then begin
	data.costs << ((sid, pid, qid), qual);
	data.split << ((sid, pid, qid), thesplit);
	if qual < (data.thisbestqual >>! (sid, infinity)) then begin
	  data.thisbestqual << (sid, qual);
	  data.thisbest << (sid, (pid,qid));
	end;

	if qual < (data.bestleftcost >>! ((sid,pid), infinity)) then begin 
	  data.bestleftcost << ((sid,pid), qual);
	end
      end
  in

    printf "About to start actual parsing!\n%!";
    Frozen.iter_symbols_rev gram
      begin fun sym ->

	Frozen.iter_decompositions gram sym
	  begin fun prod ->
	    printf "\n>>> s%d -> s%d s%d\n%!" prod.topsid prod.leftsid prod.rightsid;
	    printf "%d/%d (%d)\n%!" sym.sid nsymbols prod.cid;

	    Array.iteri begin fun pnum pid ->
	      incr nmask3tot;
	      if masking3 pid prod.leftsid 
		(data.bestleftcost >>! ((prod.leftsid,pid), infinity)) then begin
		  incr nnomask3;


		printf "%s\t" name;
		printf "  nt=%d/%d pnum=%d/%d (s%d -> s%d s%d)\t" 
		  sym.sid nsymbols pnum npts prod.topsid prod.leftsid prod.rightsid;

		printf "ub=%0.3f\t" ubound;

		printf "mask1=%d/%d=%0.3f\t" (!nmasktot - !nnomask) !nmasktot
		  ((float_of_int (!nmasktot - !nnomask)) /. (float_of_int !nmasktot));
		printf "mask2=%d/%d=%0.3f\t" (!nmask2tot - !nnomask2) !nmask2tot
		  ((float_of_int (!nmask2tot - !nnomask2)) /. (float_of_int !nmask2tot));
		printf "mask3=%d/%d=%0.3f\n%!" (!nmask3tot - !nnomask3) !nmask3tot
		  ((float_of_int (!nmask3tot - !nnomask3)) /. (float_of_int !nmask3tot));

		Array.iter begin fun qid ->
		  let leftcost = data.costs >>! ((prod.leftsid, pid, qid),infinity) in
		    incr nmask2tot;
		    if masking2 pid qid prod.leftsid leftcost then begin
		      incr nnomask2;
		      if leftcost < infinity then begin

			Array.iter begin fun rid ->
			  if not sym.startable || (pid=rid) then begin
			    let cost = rule_cost data gram sym prod (pid,qid,rid) in
			      incr nmasktot;
			      if masking pid qid rid prod cost then begin
				tryout sym.sid pid rid cost (Some (prod.cid, qid));
				incr nnomask;
			      end
			  end
			end topnodes; (* end for r *)
		      end
		    end
		end topnodes; (* end for q *)
		end;
	    end topnodes; (* end for p *)

	  end; (* end iter_decompositions *)

	if data.thisbest >>? sym.sid then begin 
	  let p,q = data.thisbest >> sym.sid in
	    printf "\ts%d -> p%d p%d: %f\n%!" sym.sid p q (data.thisbestqual >> sym.sid);
	end
      end; (* end iter_symbols *)
    ()


(* let line_costs net scenedata =  *)
(*   (\* use length-based formulation or symmetric formulation? *\) *)
(*   let pixel_cost r = *)
(*     assert (I.inside scenedata.onpixels r); *)
(*     if (I.get scenedata.onpixels r) then  *)
(*       0. (\* already marked foreground, all factors in formula cancel *\) *)
(*     else begin *)
(*       if (I.get scenedata.edgemap r) then *)
(* 	edge_term +. length_term *)
(*       else *)
(* 	length_term *)
(*     end *)
(*   in *)

(*   let fnet = { *)
(*     vertices = Array.copy net.vertices; *)
(*     edges = mkhash (Hashtbl.length net.edges); *)
(*     nbrs = Hashtbl.copy net.nbrs; *)
(*   } in *)
    
(*     printf "Have to process %d lines\n%!" (Hashtbl.length net.edges); *)
(*     Hashtbl.iter *)
(*       begin fun (pi,qi) () -> *)
(* 	let p, q = net.vertices.(pi), net.vertices.(qi) in *)
(* 	let line = Draw.line p q in *)
(* 	let cost = ref 0. in *)
(* 	let non = ref 0. in *)
(* 	let ntot = ref 0. in *)
(* 	  List.iter *)
(* 	    begin fun r -> *)
(* 	      cost := !cost +. (pixel_cost r); *)
(* 	      if (I.get scenedata.edgemap r) then begin *)
(* 		non := !non +. 1.; *)
(* 	      end; *)
(* 	      ntot := !ntot +. 1.; *)
(* 	    end *)
(* 	    line; *)

(* (\* 	  if (!non /. !ntot >= 0.75) || (!ntot <= 3.) then begin *\) *)
(* (\* 	  if !cost < 0. then begin *\) *)
(* (\* 	  if true then begin *\) *)
(* 	  if (!non /. !ntot >= 0.5) || (!ntot <= 10.) then begin *)
(* 	    fnet.edges << ((pi,qi), !cost); *)
(* 	  end *)
(*       end *)
(*       net.edges; *)
(*     fnet *)

let new_insert_lexical_costs gram net pdata state =
  Hashtbl.iter 
    begin fun (pid,qid) () ->
      let p,q = net.vertices.(pid), net.vertices.(qid) in
      let line = Draw.line p q in
      let cost_fwd, cost_rev = ref 0., ref 0. in
      let non = ref 0. in
      let ntot = ref 0. in
      let (px,py), (qx,qy) = p, q in
      let (vx,vy) = (qx-px, qy-py) in
	List.iter
	  begin fun r ->
	    ntot := !ntot +. 1.;
	    if I.get state.edgemap r then begin 
	      non := !non +. 1.;
	    end;
	    if I.get state.usedpixels r >= 0 then begin 
	      (* directional masking *)
	      let idx = I.get state.usedpixels r in
		if check_direction (vx,vy) state.directions.(idx) then begin 
		  if I.get state.edgemap r then
		    cost_fwd := !cost_fwd +. edge_term +. length_term
		  else
		    cost_fwd := !cost_fwd +. length_term;
		end
		else begin 
		  if I.get state.edgemap r then
		    cost_rev := !cost_rev +. edge_term +. length_term
		  else
		    cost_rev := !cost_rev +. length_term;
		end
	    end
	    else begin (* no masking *)
	      if I.get state.edgemap r then begin
		cost_fwd := !cost_fwd +. edge_term +. length_term;
		cost_rev := !cost_rev +. edge_term +. length_term;
	      end
	      else begin
		cost_fwd := !cost_fwd +. length_term;
		cost_rev := !cost_rev +. length_term;
	      end
	    end
	  end
	  line;

(* 	  if (!non /. !ntot >= 0.75) || (!ntot <= 3.) then begin *)
(* 	  if true then begin *)
	if (!non /. !ntot >= 0.5) || (!ntot <= 10.) then begin
	  Frozen.iter_symbols gram 
	    begin fun sym ->
	      if (not sym.startable && sym.sdata.straightcost < infinity) then begin
		let cost_fwd = !cost_fwd +. sym.sdata.straightcost in
		let cost_rev = !cost_rev +. sym.sdata.straightcost in
		  if cost_fwd < (pdata.costs >>! ((sym.sid,pid,qid), infinity)) then begin 
		    pdata.costs << ((sym.sid,pid,qid), cost_fwd);
		    pdata.split << ((sym.sid,pid,qid), None);	

		    if cost_fwd < (pdata.bestleftcost >>! ((sym.sid,pid), infinity)) then begin 
		      pdata.bestleftcost << ((sym.sid,pid), cost_fwd);
		    end;
		  end;
		  if cost_rev < (pdata.costs >>! ((sym.sid,qid,pid), infinity)) then begin 
		    pdata.costs << ((sym.sid,qid,pid), cost_rev);
		    pdata.split << ((sym.sid,qid,pid), None);	

		    if cost_rev < (pdata.bestleftcost >>! ((sym.sid,qid), infinity)) then begin 
		      pdata.bestleftcost << ((sym.sid,qid), cost_rev);
		    end;
		  end;
	      end
	    end
	end;

    end
    net.edges;
  ()

(* let insert_lexical_costs gram pdata linecosts = *)
(*   let lexical sym =  *)
(*     (not sym.startable && sym.sdata.straightcost < infinity) *)
(*   in *)
(*     Frozen.iter_symbols gram *)
(*       begin fun sym -> *)
(* 	if lexical sym then begin  *)
(* 	  Hashtbl.iter *)
(* 	    begin fun (pid,qid) cost -> *)
(* 	      let cost = cost +. sym.sdata.straightcost in *)
(* 		if cost < (pdata.costs >>! ((sym.sid,pid,qid), infinity)) then begin  *)
(* 		  pdata.costs << ((sym.sid, pid, qid), cost); *)
(* 		  pdata.costs << ((sym.sid, qid, pid), cost); *)
(* 		  pdata.split << ((sym.sid, pid, qid), None); *)
(* 		  pdata.split << ((sym.sid, qid, pid), None); *)

(* 		  if cost < (pdata.bestleftcost >>! ((sym.sid, pid), infinity)) then begin  *)
(* 		    pdata.bestleftcost << ((sym.sid, pid), cost); *)
(* 		  end; *)
(* 		  if cost < (pdata.bestleftcost >>! ((sym.sid, qid), infinity)) then begin  *)
(* 		    pdata.bestleftcost << ((sym.sid, qid), cost); *)
(* 		  end; *)
(* 		end *)
(* 	    end *)
(* 	    linecosts.edges *)
(* 	end *)
(*       end *)

(* now also coarsening given costs *)
let coarsen_lexical_costs map pdata cpdata =
  Hashtbl.iter
    begin fun (sid,pid,qid) cost ->
      let cpid, cqid = map.(pid), map.(qid) in
	if cost < (cpdata.costs >>! ((sid,cpid,cqid), infinity)) then begin 
	  let split = 
	    match pdata.split >> (sid,pid,qid) with 
		None -> None
	      | Some (cid,rid) -> Some (cid, map.(rid))
	  in
	    cpdata.costs << ((sid,cpid,cqid), cost);
	    cpdata.split << ((sid,cpid,cqid), split);

	    if cost < (cpdata.bestleftcost >>! ((sid,cpid), infinity)) then begin 
	      cpdata.bestleftcost << ((sid,cpid), cost);
	    end
	end
    end
    pdata.costs

let count_topnodes part =
  let n = ref 0 in
    Array.iter 
      begin fun par ->
	if par = None then incr n
      end
      part.parents;
    !n

let propagate_lexical_costs gram filt pdata = 
  let pdatas = Array.init filt.depth 
    begin fun i -> 
      if i=0 then pdata
      else
	let npts = count_topnodes filt.levels.(i) in
	  new_parse_data (Frozen.num_symbols gram, npts)
    end in

    for lvl=1 to filt.depth-1 do 
      printf "propagate_lexical_costs %d\n%!" lvl;
      coarsen_lexical_costs filt.maps.(lvl-1) pdatas.(lvl-1) pdatas.(lvl);
    done;
    pdatas

let find_best_coarse_init gram filt pdata ubound_want = 
  let masking_none p q r prod cost = true in
  let masking2_none p q sid cost = true in
  let masking3_none p sid cost = true in
  let topnodes = top_level_ids filt.levels.(filt.depth-1) in
    vinside gram filt.points filt.levels.(filt.depth-1).ranks topnodes
      masking_none masking2_none masking3_none	
      pdata
      false "noim" (Image.create 70 70 (0,0,0)) "init" ubound_want;

    harvest_best_tree_overall gram pdata

let find_best_coarse gram filt map pdata ubound odata =
  (* can we possibly use this in optimal parse? *)
  let masking_outside p q r prod cost = 
    let minout = odata.ocosts >>! ((prod.topsid,map.(p),map.(r)), infinity) in
(*       (minout < infinity) && *)
	(cost +. minout <= ubound)
  in
  let masking2_outside p q sid cost =
    let minout = odata.ocosts >>! ((sid,map.(p),map.(q)), infinity) in
(*       (minout < infinity) && *)
	(cost +. minout <= ubound)
  in
  let masking3_outside p sid cost = 
    let minout = odata.bestocost >>! ((sid, map.(p)), infinity) in
(*       (minout < infinity) && *)
	(cost +. minout <= ubound)
  in
  let topnodes = top_level_ids filt.levels.(filt.depth-1) in
    vinside gram filt.points filt.levels.(filt.depth-1).ranks topnodes
      masking_outside masking2_outside masking3_outside	
      pdata
      false "noim" (Image.create 70 70 (0,0,0)) "inside" ubound;

    harvest_best_tree_overall gram pdata

let newer_lift_coarse_soln gram filt pdatas ptree =
  let masking_none p q r prod cost = true in
  let masking2_none p q sid cost = true in
  let masking3_none p sid cost = true in
  let ptree = ref ptree in
    begin try begin 
      for i=filt.depth-2 downto 0 do 
	let yield_pts = List.map (fun (pid,qid) -> pid) !ptree.yield in
	let topnodes = 
	  let topnodes = ref [] in
	    Array.iteri 
	      begin fun j par ->
		if par = None && 
		  (List.mem filt.maps.(i).(j) yield_pts) then begin 
		    topnodes := j :: !topnodes;
		  end
	      end
	      filt.levels.(i).parents;
	    Array.of_list !topnodes
	in
	  vinside gram filt.points filt.levels.(i).ranks topnodes
	    masking_none masking2_none masking3_none
	    pdatas.(i)
	    false
	    "noim" (Image.create 70 70 (0,0,0))
	    "lifting" infinity;
	  ptree := harvest_best_tree_overall gram pdatas.(i);
      done;
      Some !ptree, !ptree.qual

    end
    with Parse_failure -> 
      None, infinity
    end
  
let update_outside gram filt pdata odata ubound =
  let masking_inside_left p q r prod cost =
    let minin = pdata.costs >>! ((prod.leftsid, p, q), infinity) in
      if cost +. minin > ubound then
	false
      else
	true
  in

  let masking_inside_right p q r prod cost =
    let minin = pdata.costs >>! ((prod.rightsid, q, r), infinity) in
      if cost +. minin > ubound then
	false
      else
	true
  in
  let topnodes = top_level_ids filt.levels.(filt.depth-1) in

    voutside gram filt.points filt.levels.(filt.depth-1).ranks topnodes
      masking_inside_left masking_inside_right
(*       masking_left_none masking_right_none *)
      pdata odata


let doit gram nlvls net filt state name ubound_want =
(*   let coarsest = copy_partition filt.levels.(filt.depth-1) in *)
  let ubound = ref ubound_want in 
  let iter = ref 0 in
  let outside = ref None in
  let bottom_pdata = new_parse_data 
    (Frozen.num_symbols gram, Array.length filt.points) in
  let old_map = ref None in
  let best_fine = ref None in
    new_insert_lexical_costs gram net bottom_pdata state;

    printf "depth=%d\n%!" filt.depth;
    begin try  
    while true do 
      printf "starting iter %d\n%!" !iter;
      let pdatas = propagate_lexical_costs gram filt bottom_pdata in
      let top_pdata = pdatas.(Array.length pdatas - 1) in
      let best_coarse = 
	match !outside with 
	    Some outside ->  
	      find_best_coarse gram filt (get !old_map)
		top_pdata !ubound outside
	  | None ->
	      find_best_coarse_init gram filt top_pdata !ubound
      in
      let fine_soln, fine_cost = 
	newer_lift_coarse_soln gram filt pdatas best_coarse in
      let _ = printf "done lifting\n%!" in

      let yield_ids = List.map (fun (pid,qid) -> pid) best_coarse.yield in  
      let _ = 
	if fine_cost < !ubound then 
	  best_fine := fine_soln;

	ubound := min fine_cost !ubound;
	printf "ubound=%0.3f\n" !ubound;
	outside := Some (new_outside_data 
			   (Frozen.num_symbols gram, count_topnodes filt.levels.(filt.depth-1)));
	old_map := Some (Array.copy filt.maps.(filt.depth-2));
	update_outside gram filt top_pdata (get !outside) !ubound;
	demote_filtration filt yield_ids;		  
      in

      let im = draw_partition filt.levels.(filt.depth-1) filt.points (70,70) in
      let _ = Pnm.save_ppm im (sprintf "part.%s.%d.ppm" name !iter) in

      let im = draw_parse best_coarse filt.points state.edgemap in
	Pnm.save_ppm im (sprintf "thebest.%s.%d.ppm" name  !iter);
	if !best_fine <> None then begin 
	  let im = draw_parse (get !best_fine) filt.points state.edgemap in
	    Pnm.save_ppm im (sprintf "bestfine.%s.%d.ppm" name !iter);
	end;
	print_parse_tree best_coarse;
	incr iter;
    done;
    with Parse_failure ->
      begin 
	let im = draw_parse (get !best_fine) filt.points state.edgemap in
	  Pnm.save_ppm im (sprintf "bestfine.%s.final.ppm" name);	
      end;
    end;
    get !best_fine

    
let get_conflict state ptree points =
  let yield = Array.of_list ptree.yield in
  let nyield = Array.length yield in
  let lines = Array.map
    begin fun (pid,qid) ->
      (* don't want to see conflicts at endpoints *)
      List.tl (Draw.line points.(pid) points.(qid))
    end
    yield
  in
  let bestpair, bestnum = ref None, ref 0 in

    for i=0 to nyield-1 do 
      for j=i+1 to nyield-1 do 
	let conflicts = ref 0 in
	  List.iter 
	    begin fun ipt ->
	      List.iter 
		begin fun jpt ->
		  if (ipt = jpt) && (I.get state.usedpixels ipt < 0) then
		    incr conflicts;
		end
		lines.(j)
	    end
	    lines.(i);

	  if !conflicts > !bestnum then begin 
	    let pid1,qid1 = yield.(i) in
	    let pid2,qid2 = yield.(j) in
	      bestpair := Some ((pid1,qid1), (pid2,qid2));
	      bestnum := !conflicts;
	  end
      done
    done;

    !bestpair


let true_cost gram ptree scene points =
  let qual = ref 0. in
  let seen = Image.map (fun x -> false) scene.edgemap in
  let rec proc t =
    if t.thesplit = None then begin 
      let p,q = points.(t.bg), points.(t.en) in
      let line = Draw.line p q in
	List.iter
	  begin fun r ->
	    if not (I.get seen r) then begin 
	      if I.get scene.edgemap r then
		qual := !qual +. edge_term +. length_term
	      else
		qual := !qual +. length_term;

	      I.set seen true r;
	    end
	  end 
	  line;
    end
    else begin 
      let cid, md = get t.thesplit in
      let prod = Frozen.get_composition gram cid in
      let shape = Shape.shape_of_ints_bme points.(t.bg) points.(md) points.(t.en) in
	qual := !qual +. Models.Simple.prod_cost prod shape;
    end;

    if t.left <> None then
      proc (get t.left);
    if t.right <> None then
      proc (get t.right);
  in
    proc ptree;
    !qual

let rec copy_tree t = 
  {}

let propose_changes ptree =
  let maxlen = List.length ptree.yield in
  let rec proc t =
    let len = List.length t.yield in
    let newtree = copy_tree ptree in
    let masked = Image.map (fun x -> false) edgemap in
    let rec draw t2 =
      if t2.thesplit = None then begin 
	let p,q = points.(t2.bg), points.(t2.en) in
	let line = Draw.line p q in
	  List.iter
	    begin fun r ->
	      I.set masked true r;
	    end
	    line
      end
      else begin
	(* skip the tree we're considering changing *)
	if t2.nt <> t.nt then begin
	  draw t2.left;
	  draw t2.right;
	end
      end
    in
      draw newtree;
      let linecosts = line_costs net edgemap masked in
      let 

      initialize image with cost(p)=0, cost(everthing else)=infinity
      for i=1 to len do, for every vertex, for every nbr, consider extending by one
	have image of parent pointers (store pid as int)
      when we have done all that, trace parent pointers back from q to get proposal
      modify the parse tree, send a whole one back
	(* write a map-like fn on trees, run it on newtree, copy other
	   nodes, reconstruct relevant nodes. if map-like, we don't actually have to copy newtree *)
      in
	()


let reparse gram nlvls net filt edgemap =
  let bestparse, bestqual = ref None, ref infinity in 
  let sstates = ref [new_scene edgemap infinity] in
  let reiter = ref 0 in
    while !sstates <> [] do
      (*     for foofoofoo = 1 to 20 do *)
      let filt = copy_filtration filt in
      let state = List.hd !sstates in
      let _ = sstates := List.tl !sstates; in

      let bestfine = doit gram nlvls net filt state
	(sprintf "x%d" !reiter) !bestqual in
      let truecost = true_cost gram bestfine state filt.points in

      let bestpair = get_conflict state bestfine filt.points in

      let newstates = 
	if bestpair <> None then begin 
	  let (pid1,qid1), (pid2,qid2) = get bestpair in
	    [add_direction state filt.points.(pid1) filt.points.(qid1);
	     add_direction state filt.points.(pid2) filt.points.(qid2)]
	end
	else begin 
	  (* found simple parse! *)
	  printf "found simple parse!\n";
	  ignore (read_line ());
	  []
	end
      in

	List.iter
	  begin fun ss ->
	    ss.iteration <- !reiter;
	    ss.parent_truecost <- truecost;
	  end 
	  newstates;

	  printf "bestfine.qual=%f\n" bestfine.qual;	
	  printf "true cost=%f\n" truecost;
	  printf "nnew states=%d\n" (List.length newstates);		  
	  printf "nold states=%d\n" (List.length !sstates);		  

(* 	  if truecost < 0. then begin *)
	  if true then begin
	    (* 	    sstates := !sstates @ newstates; *)
	    sstates := !sstates @ newstates;
	  end;
	  sstates := List.sort
	    begin fun x y -> 
	      compare x.parent_truecost y.parent_truecost
	    end
	    !sstates;

	  List.iter
	    begin fun ss ->
	      printf "[x%d: %f],  " ss.iteration ss.parent_truecost;
	    end 
	    !sstates;
	  printf "\n%!";
	  ignore (read_line ());

	  if truecost < !bestqual then begin 
	    bestqual := truecost;
	    bestparse := Some bestfine;
	  end;
	  incr reiter;
    done;
    ()

let _ = 
  (*   let excurve = Curve.load "romer/newann/IMG0000.curve" in *)
  let excurve = Curve.load "romer/newann/IMG0020.curve" in
  let sdf = Sdf.load_family "romer/misc/romer1.sdf" in
  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = merge_leaves gram in
  let _ = reorder_symbols gram in

(*   let imname = "edges0020.pgm" in *)
  let imname = "edges.2.pgm" in
  let edgemap = Pnm.load_pgm imname in
  let edgemap = Image.map (fun x -> x=0) edgemap in
(*   let edgemap = Gridparsing.coarsen_edge_map edgemap 4 in *)
  let _ = 
    Pnm.save_pgm (Image.map (function false -> 255 | true -> 0) edgemap)
      "coarse_edges.pgm"
  in

  let granularity, size = (242/4), (242/4) in
  let net = Gridparsing.make_net granularity size in

(*   let scene = new_scene edgemap in *)
(*   let linecosts = line_costs net scene in *)
(*     (\*   let bestline = find_best_line linecosts in *\) *)


(*   let nlvls = 3 in *)
  let nlvls = 4 in
  let filtration = make_filtration net.vertices nlvls in
    
(*   let _ = doit gram nlvls net filtration linecosts in *)
  let _ = reparse gram nlvls net filtration edgemap in

    printf "hello!\n%!"
