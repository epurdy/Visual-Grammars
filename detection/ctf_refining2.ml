open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph
open Filtration

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
  mutable simple: bool;
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

type image_scene_data = {
  parsedata: image_parse_data;
  mutable clutter_lines: (pt*pt) list;
  mutable edgemap: bool Image.t;
  mutable onpixels: bool Image.t;
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

let new_scene edgemap = 
  {parsedata = new_parse_data (1,1);
   clutter_lines = [];
   edgemap = edgemap;
   onpixels = I.map (fun x -> false) edgemap;
  }

let copy_parse_data pdata = 
  {costs = Hashtbl.copy pdata.costs;
   bestleftcost = Hashtbl.copy pdata.bestleftcost;
   split = Hashtbl.copy pdata.split;
   thisbest = Hashtbl.copy pdata.thisbest;   
   thisbestqual = Hashtbl.copy pdata.thisbestqual;
  }

let draw_parse ptree points edgemap =
  let im = Image.map (function true -> (0,0,0) | false -> (255,255,255)) edgemap in
  let curve = List.map (fun (pid,qid) -> points.(pid)) ptree.yield in
    draw_curve im (Array.of_list curve) true (255,0,0);
    im

let line_costs net scenedata = 
  (* use length-based formulation or symmetric formulation? *)
  let pixel_cost r =
    assert (I.inside scenedata.onpixels r);
    if (I.get scenedata.onpixels r) then 
      0. (* already marked foreground, all factors in formula cancel *)
    else begin
      if (I.get scenedata.edgemap r) then
	edge_term +. length_term
      else
	length_term
    end
  in

  let fnet = {
    vertices = Array.copy net.vertices;
    edges = mkhash (Hashtbl.length net.edges);
    nbrs = Hashtbl.copy net.nbrs;
  } in
    
    printf "Have to process %d lines\n%!" (Hashtbl.length net.edges);
    Hashtbl.iter
      begin fun (pi,qi) () ->
	let p, q = net.vertices.(pi), net.vertices.(qi) in
	let line = Draw.line p q in
	let cost = ref 0. in
	let non = ref 0. in
	let ntot = ref 0. in
	  List.iter
	    begin fun r ->
	      cost := !cost +. (pixel_cost r);
	      if (I.get scenedata.edgemap r) then begin
		non := !non +. 1.;
	      end;
	      ntot := !ntot +. 1.;
	    end
	    line;

(* 	  if (!non /. !ntot >= 0.75) || (!ntot <= 3.) then begin *)
(* 	  if !cost < 0. then begin *)
(* 	  if true then begin *)
	  if (!non /. !ntot >= 0.5) || (!ntot <= 10.) then begin
	    fnet.edges << ((pi,qi), !cost);
	  end
      end
      net.edges;
    fnet

let intersect ls1 ls2 =
  let rv = ref false in
    List.iter 
      begin fun x ->
	if (not !rv) && List.mem x ls2 then begin
	  rv := true;
	  let (a,b) = x in
(* 	    printf "intersect=true, (%d,%d) in ls2\n" a b; *)
(* 	    List.iter (fun (a,b) -> printf "(%d,%d)\n" a b) ls1; *)
(* 	    printf "---\n"; *)
(* 	    List.iter (fun (a,b) -> printf "(%d,%d)\n" a b) ls2; *)
(* 	    printf "---\n"; *)
	    ()
	end
      end
      ls1;
    !rv

let rec harvest_best_tree gram pdata nt (bg,en) = 
  printf "harvest %d -> p%d p%d\n%!" nt bg en;
  match pdata.split >>! ((nt,bg,en),None) with
      Some (cid, q) ->
	let prod = Frozen.get_composition gram cid in
	let left = harvest_best_tree gram pdata prod.leftsid (bg,q) in
	let right = harvest_best_tree gram pdata prod.rightsid (q,en) in
	  {nt=nt; bg=bg; en=en;
	   yield = left.yield @ right.yield;
	   simple = left.simple && right.simple && (not (intersect left.yield right.yield));
	   left = Some left;
	   right = Some right;
	   qual = pdata.costs >>! ((nt,bg,en), infinity);
	   thesplit = Some (cid,q);
	  }
    | None ->
	{nt=nt; bg=bg; en=en;
	 yield = [(bg, en)];
	 simple = true;
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
(*     failwith "Failed to parse!" *)

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

(* let bound_shape_cost2 (p,q,r) (rkp,rkq,rkr) model = *)
(*   let sidelen rk = (1 lsl rk) in *)
(*   let sp, sq, sr = sidelen rkp, sidelen rkq, sidelen rkr in *)
(*   let (px,py), (qx,qy), (rx,ry) = p,q,r in *)
(*   let jig n s = n + Random.int s in *)
(*   let mincost, maxcost = ref infinity, ref neg_infinity in *)
(*     for i = 1 to 10 do *)
(* 	let px,py,qx,qy,rx,ry =  *)
(* 	  jig px sp, jig py sp, jig qx sq, jig qy sq, jig rx sr, jig ry sr  *)
(* 	in *)
(* 	let shape = Shape.shape_of_ints_bme (px,py) (qx,qy) (rx,ry) in *)
(* 	let cost = Watson.cost model shape in *)
(* 	  if cost < !mincost then  *)
(* 	    mincost := cost; *)
(* 	  if cost > !maxcost then  *)
(* 	    maxcost := cost; *)
(*     done; *)
(*     !mincost -. 0.5 *. (!maxcost -. !mincost) *)
(*       (\* 2*diff is slightly safer *\) *)


let voutside gram points ranks topnodes masking_left masking_right pdata odata =
  let nnodes = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

  let rule_cost prod (p,q,r) = 
(*     let shapecost =  *)
(*       match prod.cdata.geom with *)
(* 	  Watson model -> *)
(* 	    bound_shape_cost2  *)
(* 	      (points.(p),points.(q),points.(r))  *)
(* 	      (ranks.(p),ranks.(q),ranks.(r))  *)
(* 	      model *)
(* 	| Improper ->  *)
(* 	    0. *)
(*     in *)
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
      (*     Frozen.iter_symbols_rev gram *)
      begin fun sym ->
	Frozen.iter_decompositions gram sym
	  begin fun prod ->
	    printf "\n<<< s%d -> s%d s%d\n%!" prod.topsid prod.leftsid prod.rightsid;

	    Array.iteri begin fun pnum pid ->
	      printf "  nt=%d/%d p=%d/%d\n%!" sym.sid nsymbols pnum nnodes;

	      Array.iter begin fun rid -> 
(* 		printf "  nt=%d/%d p=%d,r=%d\t%0.3f\n%!" sym.sid nsymbols pid rid (odata.ocosts >>! ((prod.topsid, pid, rid), infinity)); *)
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
(* 		      printf "left: nt=%d/%d p=%d,q=%d,r=%d\t%0.3f\n%!"  *)
(* 			sym.sid nsymbols pid qid rid cost_left; *)
(* 		      printf "right: nt=%d/%d p=%d,q=%d,r=%d\t%0.3f\n%!"  *)
(* 			sym.sid nsymbols pid qid rid cost_right; *)
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
(*     let shapecost =  *)
(*       match prod.cdata.geom with *)
(* 	  Watson model -> *)
(* 	    bound_shape_cost2  *)
(* 	      (points.(p),points.(q),points.(r))  *)
(* 	      (ranks.(p),ranks.(q),ranks.(r))  *)
(* 	      model *)
(* 	| Improper -> *)
(* 	    0. *)
(*     in *)
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

let insert_lexical_costs gram pdata linecosts =
  let lexical sym = 
    (not sym.startable && sym.sdata.straightcost < infinity)
  in
    Frozen.iter_symbols gram
      begin fun sym ->
	if lexical sym then begin 
	  Hashtbl.iter
	    begin fun (pid,qid) cost ->
	      let cost = cost +. sym.sdata.straightcost in
		if cost < (pdata.costs >>! ((sym.sid,pid,qid), infinity)) then begin 
		  pdata.costs << ((sym.sid, pid, qid), cost);
		  pdata.costs << ((sym.sid, qid, pid), cost);
		  pdata.split << ((sym.sid, pid, qid), None);
		  pdata.split << ((sym.sid, qid, pid), None);

		  if cost < (pdata.bestleftcost >>! ((sym.sid, pid), infinity)) then begin 
		    pdata.bestleftcost << ((sym.sid, pid), cost);
		  end;
		  if cost < (pdata.bestleftcost >>! ((sym.sid, qid), infinity)) then begin 
		    pdata.bestleftcost << ((sym.sid, qid), cost);
		  end;
		end
	    end
	    linecosts.edges
	end
      end

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
(*       let im = Image.create 70 70 (0,0,0) in *)
(*       let curve = List.map (fun (pid,qid) -> filt.points.(pid)) !ptree.yield in *)
(* 	draw_curve im (Array.of_list curve) true (255,0,0); *)
(* 	Pnm.save_ppm im "thelift.ppm"; *)

	Some !ptree, !ptree.qual
    end
    with Parse_failure -> 
      None, infinity
    end

(* let new_lift_coarse_soln gram filt pdatas ptree = *)
(*   begin try begin *)
(*     let mapit pid = *)
(*       let rv = ref pid in *)
(* 	for i = 0 to filt.depth-2 do  *)
(* 	  rv := filt.maps.(i).(!rv) *)
(* 	done; *)
(* 	!rv *)
(*     in *)
(*     let treehash, treehash2, treehash3 = hash_of_parse_tree ptree in *)
(*     let topnodes = top_level_ids filt.levels.(filt.depth-1) in *)
(*       vinside gram filt.points topnodes *)
(* 	begin fun p q r prod cost -> *)
(* 	  treehash >>? (mapit p, mapit q, mapit r, prod.cid) *)
(* 	end *)
(* 	begin fun p q sid cost -> *)
(* 	  treehash2 >>? (mapit p, mapit q, sid) *)
(* 	end *)
(* 	begin fun p sid cost -> *)
(* 	  treehash3 >>? (mapit p, sid) *)
(* 	end *)
(* 	pdatas.(0) *)
(* 	false *)
(* 	(sprintf "lifting.%d.%d-" 17 17) (Image.create 70 70 (0,0,0)) *)
(* 	"lifting" ptree.qual; *)
(*       printf "about to harvest!\n%!"; *)
(*       let new_ptree = harvest_best_tree_overall gram pdatas.(0) in *)
(* 	new_ptree.qual *)
(*   end *)
(*   with Parse_failure ->  *)
(*     infinity *)
(*   end *)
  
let update_outside gram filt pdata odata ubound =
(*   let masking_left_none p q r prod cost = true in *)
(*   let masking_right_none p q r prod cost = true in *)

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

(* let coarsen_outer odata map = *)
(*   let odata2 = new_outside_data () in *)
(*     Hashtbl.iter *)
(*       begin fun (sid,pid,qid) cost -> *)
(* 	let pid2, qid2 = map.(pid), map.(qid) in *)
(* 	  if cost < (odata2.ocosts >>! ((sid,pid2,qid2), infinity)) then begin  *)
(* 	    odata2.ocosts << ((sid,pid2,qid2), cost); *)
(* 	  end *)
(*       end *)
(*       odata.ocosts; *)

(*     Hashtbl.iter *)
(*       begin fun (sid,pid) cost -> *)
(* 	let pid2 = map.(pid) in *)
(* 	  if cost < (odata2.bestocost >>! ((sid,pid2), infinity)) then begin  *)
(* 	    odata2.bestocost << ((sid,pid2), cost); *)
(* 	  end *)
(*       end *)
(*       odata.bestocost; *)

(*     odata2 *)

(* change doit code so that it takes a parse tree and inserts free
   assertions. we traverse that tree inserting stuff, have to be
   sure not to insert bottom-most cost. *)
let insert_givens pdata givens =
  List.iter 
    begin fun given ->
      let rec proc t =
(* 	if t.thesplit <> None then begin  *)
	if true then begin
	  pdata.costs << ((t.nt,t.bg,t.en), t.qual);
	  pdata.split << ((t.nt,t.bg,t.en), t.thesplit);
	end
	else begin 
	  pdata.split << ((t.nt,t.bg,t.en), None);
	end;

	if t.left <> None then
	  proc (get t.left);
	if t.right <> None then
	  proc (get t.right);
      in
	proc given
    end
    givens

let doit gram nlvls net filt linecosts givens name ubound_want edgemap =
(*   let coarsest = copy_partition filt.levels.(filt.depth-1) in *)
  let ubound = ref ubound_want in 
  let iter = ref 0 in
  let outside = ref None in
  let bottom_pdata = new_parse_data 
    (Frozen.num_symbols gram, Array.length filt.points) in
  let old_map = ref None in
  let best_fine = ref None in
    insert_lexical_costs gram bottom_pdata linecosts;
    insert_givens bottom_pdata givens;
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
		(* filt.maps.(filt.depth-2) *) top_pdata !ubound outside
	  | None ->
	      find_best_coarse_init gram filt top_pdata !ubound
      in
(*       let fine_cost = new_lift_coarse_soln gram filt pdatas best_coarse in *)
      let fine_soln, fine_cost = 
	newer_lift_coarse_soln gram filt pdatas best_coarse in
      let _ = printf "done lifting\n%!" in

      let yield_ids = List.map (fun (pid,qid) -> pid) best_coarse.yield in  
(*       let old_top_part = copy_partition filt.levels.(filt.depth-1) in *)
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

(* 	let themap = update_filtration filt coarsest yield_ids in *)
(* 	  outside := Some (coarsen_outer (get !outside) themap); *)
		  
      in

      let im = draw_partition filt.levels.(filt.depth-1) filt.points (70,70) in
      let _ = Pnm.save_ppm im (sprintf "part.%s.%d.ppm" name !iter) in

      let im = draw_parse best_coarse filt.points edgemap in
	Pnm.save_ppm im (sprintf "thebest.%s.%d.ppm" name  !iter);
	if !best_fine <> None then begin 
	  let im = draw_parse (get !best_fine) filt.points edgemap in
	    Pnm.save_ppm im (sprintf "bestfine.%s.%d.ppm" name !iter);
	end;
	print_parse_tree best_coarse;
	incr iter;
    done;
    with Parse_failure ->
      begin 
	let im = draw_parse (get !best_fine) filt.points edgemap in
	  Pnm.save_ppm im (sprintf "bestfine.%s.final.ppm" name);	
      end;
    end;
    get !best_fine

let discount_yield points ptree scenedata =
  let segs = List.map (fun (pid,qid) -> (points.(pid), points.(qid))) ptree.yield in
    List.iter
      begin fun (p,q) ->
	let line = Draw.line p q in
	  List.iter
	    begin fun r ->
	      I.set scenedata.onpixels true r;
	    end
	    line;
      end
      segs;
    ()

type search_state = {
  ptrees: image_parse_tree list;
  size: int;
  parent_truecost: float;
}

(* gah, our definition of simple was too naive, we have to check at
   the pixel level, but how do we do that??? can either make lists of
   pixels, or do geometry. or we could just circle around, coloring
   the yields on a map. we could have a single map, and go around
   putting current sid in it. if we see our own sid, we're not simple,
   so we should recurse.

*)
      
let get_fragments state ptree points w h parent_truecost =
  let seen = Image.create w h (-1) in
  let frags = ref [] in
  let _ = 
    List.iter 
      begin fun ptree ->
	List.iter 
	  begin fun (pid,qid) -> 
	    Draw.draw_line seen points.(pid) points.(qid) (-2);
	  end
	  ptree.yield;
      end
      state.ptrees;
  in
  let rec proc t =
    let simple = ref true in
      List.iter 
	begin fun (pid,qid) -> 
	  let line = Draw.line points.(pid) points.(qid) in
	    List.iter 
	      begin fun r ->
		if (I.get seen r = t.nt) ||
		  (I.get seen r = (-2)) 
		then begin 
		  simple := false;
		end
		else begin
		  I.set seen t.nt r;
		end
	      end
	      (List.tl line); (* don't want to notice overlap at ends! *)
	end
	t.yield;

    printf "nt=%d, simple=%b\n%!" t.nt !simple;
    t.simple <- !simple;

    if !simple then begin 
      (* exclude leaf-only trees *)
(*       if t.thesplit <> None then begin  *)
      if true then begin
	let newfrag = {
	  size = List.length t.yield;
	  ptrees = t :: state.ptrees;
	  parent_truecost = parent_truecost;
	}
	in
	  frags := newfrag :: !frags;
      end
    end
    else begin 
      if t.left <> None then
	proc (get t.left);
      if t.right <> None then
	proc (get t.right);
    end
  in
    proc ptree; 
    !frags


let true_cost ptree scene points =
  let diff = ref 0. in
  let w,h = Image.width scene.edgemap, Image.height scene.edgemap in
  let seen_new = Image.create w h false in
    List.iter
      begin fun (pid,qid) ->
	let line = Draw.line points.(pid) points.(qid) in
	  List.iter 
	    begin fun r ->
	      (* if on before, do nothing. is this right??? *)
	      if not (I.get scene.onpixels r) then begin 
		if I.get seen_new r then begin 
		  if I.get scene.edgemap r then
		    (* were paying for edge, now not *)
		    diff := !diff -. (edge_term +. length_term)
		  else
		    (* were paying for non-edge, now not *)
		    diff := !diff -. length_term;
		end
		else begin
		  I.set seen_new true r;
		end
	      end
	    end
	    line
      end
      ptree.yield;
    ptree.qual +. !diff



let reparse gram nlvls net filt edgemap =
    (* create empty scene *)
  let bestparse, bestqual = ref None, ref infinity in 
  let sstates = ref [{ptrees=[]; size=0; parent_truecost=infinity}] in
  let reiter = ref 0 in
    while !sstates <> [] do
(*     for foofoofoo = 1 to 20 do *)
      let filt = copy_filtration filt in
      let scene = new_scene edgemap in
      let state = List.hd !sstates in
	sstates := List.tl !sstates;
	List.iter 
	  begin fun ptree -> 
	    discount_yield filt.points ptree scene
	  end
	  state.ptrees;
	let linecosts = line_costs net scene in
	let bestfine = doit gram nlvls net filt linecosts state.ptrees 
	  (sprintf "x%d" !reiter) !bestqual scene.edgemap in
	let truecost = true_cost bestfine scene filt.points in
	let newstates = get_fragments state bestfine filt.points 70 70 truecost in


	  List.iter
	    begin fun ss ->
	      let t = List.hd ss.ptrees in
		printf "-------\n";
		print_parse_tree t;
		printf "-------\n";
	    end
	    newstates;
	  printf "bestfine.qual=%f\n" bestfine.qual;	
	  printf "true cost=%f\n" truecost;
	  printf "nfrags=%d\n" (List.length newstates);		  
	  printf "nold states=%d\n" (List.length !sstates);		  
(* 	  ignore (read_line ()); *)

	  if truecost < 0. then begin
	    (* 	    sstates := !sstates @ newstates; *)
	    sstates := newstates @ !sstates;
	  end;
	  sstates := List.sort
	    begin fun x y -> 
(* 	      let totsize trees = *)
(* 		let tot = ref 0 in *)
(* 		  List.iter *)
(* 		    begin fun t -> *)
(* 		      tot := !tot + (List.length t.yield); *)
(* 		    end *)
(* 		    trees; *)
(* 		  !tot *)
(* 	      in *)
(* 		if x.parent_truecost = y.parent_truecost then *)
(* 		  compare (totsize y.ptrees) (totsize x.ptrees) *)
(* 		else *)
	      compare x.parent_truecost y.parent_truecost
	    end
	    !sstates;

	  if truecost < !bestqual then begin 
	    bestqual := truecost;
	    bestparse := Some bestfine;
	  end;
	  incr reiter;
    done;
    ()

(* 
   - do parsing, get parse
   - figure out which subtrees to require???
   - partition parse tree into simple pieces
   - for each simple piece, erase the evidence under that piece
   - then redo parsing? but how?

   - the point is that we don't want to reward a parse for
   double-counting evidence. The problem is that with merged leaves,
   we can't single count the evidence easily...

   - if we didn't have merged leaves, it would be relatively easy:
   give the appropriate X->l rule the old cost, but give every other
   lexical rule a cost that doesn't use those pixels.

   - but, relatively easy to get around this? erase the evidence, put
   in X-> C_pq for X one above the top-level, and we're done. So, we
   want to partition into trees one level above the leaves. Could this
   go wrong? yes, but only if none of those assertions are correct.

   - can also just partition into disjoint yields, erase all of the
   evidence, and insert the things. that will never lead to double
   counting, and we can just bypass choices that we don't want

*)


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
