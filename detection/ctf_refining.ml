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

let voutside gram points topnodes masking_left masking_right pdata odata =
  let nnodes = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

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
		if odata.ocosts >>! ((prod.topsid, pid, rid), infinity) < infinity then begin

		  Array.iter begin fun qid ->
		    let shape = Shape.shape_of_ints_bme points.(pid) points.(qid) points.(rid) in
		    let cost_left = (odata.ocosts >>! ((prod.topsid, pid, rid), infinity)) +.
		      (pdata.costs >>! ((prod.rightsid,qid,rid), infinity)) +.
		      (Models.Simple.prod_cost prod shape)
		    in
		    let cost_right = (odata.ocosts >>! ((prod.topsid, pid, rid), infinity)) +.
		      (pdata.costs >>! ((prod.leftsid,pid,qid), infinity)) +.
		      (Models.Simple.prod_cost prod shape)
		    in
		      if masking_left pdata pid qid rid prod cost_left then begin
			if cost_left < (odata.ocosts >>! ((prod.leftsid, pid, qid), infinity)) then begin
			  odata.ocosts << ((prod.leftsid, pid, qid), cost_left);

			  if cost_left < (odata.bestocost >>! ((prod.leftsid, pid), infinity)) then begin 
			    odata.bestocost << ((prod.leftsid, pid), cost_left);
			  end
			end;
		      end;

		      if masking_right pdata pid qid rid prod cost_right then begin
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

let vinside gram points topnodes masking masking2 masking3 data showwork prefix dispim name ubound = 
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
      leftcost +. rightcost +. (Models.Simple.prod_cost prod shape)
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

let coarsen_lexical_costs map pdata cpdata =
  Hashtbl.iter
    begin fun (sid,pid,qid) cost ->
      let cpid, cqid = map.(pid), map.(qid) in
	if cost < (cpdata.costs >>! ((sid,cpid,cqid), infinity)) then begin 
	  cpdata.costs << ((sid,cpid,cqid), cost);
	  cpdata.split << ((sid,cpid,cqid), None);

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
    printf "propagate_lexical_costs 0\n%!";

    for lvl=1 to filt.depth-1 do 
      printf "propagate_lexical_costs %d\n%!" lvl;
      coarsen_lexical_costs filt.maps.(lvl-1) pdatas.(lvl-1) pdatas.(lvl);
    done;
    pdatas

let find_best_coarse_init gram filt pdata = 
  let masking_none p q r prod cost = true in
  let masking2_none p q sid cost = true in
  let masking3_none p sid cost = true in
  let topnodes = top_level_ids filt.levels.(filt.depth-1) in
    vinside gram filt.points topnodes
      masking_none masking2_none masking3_none	
      pdata
      false "noim" (Image.create 70 70 (0,0,0)) "init" infinity;

    harvest_best_tree_overall gram pdata

let find_best_coarse gram filt map pdata ubound odata =
  (* can we possibly use this in optimal parse? *)
  let masking_outside p q r prod cost = 
    let minout = odata.ocosts >>! ((prod.topsid,map.(p),map.(r)), infinity) in
      (minout < infinity) &&
	(cost +. minout <= ubound)
  in
  let masking2_outside p q sid cost =
    let minout = odata.ocosts >>! ((sid,map.(p),map.(q)), infinity) in
      (minout < infinity) &&
	(cost +. minout <= ubound)
  in
  let masking3_outside p sid cost = 
    let minout = odata.bestocost >>! ((sid, map.(p)), infinity) in
      (minout < infinity) &&
	(cost +. minout <= ubound)
  in
  let topnodes = top_level_ids filt.levels.(filt.depth-1) in
    vinside gram filt.points topnodes
      masking_outside masking2_outside masking3_outside	
      pdata
      false "noim" (Image.create 70 70 (0,0,0)) "inside" ubound;

    harvest_best_tree_overall gram pdata

(* let blur map (h1,h2,h3) =  *)
(*   let k1,k2,k3 = mkhash 100, mkhash 100, mkhash 100 in *)
(*     Hashtbl.iter  *)
(*       begin fun (pid,qid,rid,cid) () -> *)
(* 	k1 << ((map.(pid),map.(qid),map.(rid),cid), ()); *)
(*       end *)
(*       h1; *)

(*     Hashtbl.iter  *)
(*       begin fun (pid,qid,sid) () -> *)
(* 	k2 << ((map.(pid),map.(qid),sid), ()); *)
(*       end *)
(*       h2; *)

(*     Hashtbl.iter  *)
(*       begin fun (pid,sid) () -> *)
(* 	k3 << ((map.(pid),sid), ()); *)
(*       end *)
(*       h3; *)

(*     k1,k2,k3 *)

let new_lift_coarse_soln gram filt pdatas ptree =
  begin try begin
    let mapit pid =
      let rv = ref pid in
	for i = 0 to filt.depth-2 do 
	  rv := filt.maps.(i).(!rv)
	done;
	!rv
    in
    let treehash, treehash2, treehash3 = hash_of_parse_tree ptree in
    let topnodes = top_level_ids filt.levels.(filt.depth-1) in
      vinside gram filt.points topnodes
	begin fun p q r prod cost ->
	  treehash >>? (mapit p, mapit q, mapit r, prod.cid)
	end
	begin fun p q sid cost ->
	  treehash2 >>? (mapit p, mapit q, sid)
	end
	begin fun p sid cost ->
	  treehash3 >>? (mapit p, sid)
	end
	pdatas.(0)
	false
	(sprintf "lifting.%d.%d-" 17 17) (Image.create 70 70 (0,0,0))
	"lifting" ptree.qual;
      printf "about to harvest!\n%!";
      let new_ptree = harvest_best_tree_overall gram pdatas.(0) in
	new_ptree.qual
  end
  with Parse_failure -> 
    infinity
  end
    
(* let lift_coarse_soln gram filt pdatas ptree = *)
(*   let ptree = ref ptree in *)
(*   begin try begin *)
(*     for i=filt.depth-2 downto 0 do  *)
(*       List.iter *)
(* 	begin fun (pid,qid) -> *)
(* 	  let p,q = filt.points.(pid), filt.points.(qid) in *)
(* 	  let px,py = p in *)
(* 	  let qx,qy = q in *)
(* 	    printf "(%d,%d) - (%d,%d)\n" px py qx qy; *)
(* 	end *)
(* 	!ptree.yield; *)

(*       let mapit pid = *)
(* 	if i=filt.depth-2 then begin  *)
(* 	  filt.maps.(i).(pid) *)
(* 	end *)
(* 	else begin  *)
(* 	  filt.maps.(i+1).(filt.maps.(i).(pid)) *)
(* 	end *)
(*       in *)

(*       let treehash, treehash2, treehash3 =  *)
(* 	if i=filt.depth-2 then *)
(* 	  hash_of_parse_tree !ptree  *)
(* 	else *)
(* 	  blur filt.maps.(i+1) (hash_of_parse_tree !ptree) *)
(*       in *)
(*       let topnodes = top_level_ids filt.levels.(filt.depth-1) in *)
(* 	vinside gram filt.points topnodes *)
(* 	  begin fun p q r prod cost -> *)
(* 	    treehash >>? (mapit p, mapit q, mapit r, prod.cid) *)
(* 	  end *)
(* 	  begin fun p q sid cost -> *)
(* 	    treehash2 >>? (mapit p, mapit q, sid) *)
(* 	  end *)
(* 	  begin fun p sid cost -> *)
(* 	    treehash3 >>? (mapit p, sid) *)
(* 	  end *)
(* 	  pdatas.(i) *)
(* 	  false *)
(* 	  (sprintf "lifting.%d.%d-" i i) (Image.create 70 70 (0,0,0)) *)
(* 	  "lifting" !ptree.qual; *)
(* 	printf "about to harvest!\n%!"; *)
(* 	ptree := harvest_best_tree_overall gram pdatas.(i); *)
(*     done; *)

(*     !ptree.qual *)
(*   end *)
(*   with Parse_failure ->  *)
(*     infinity *)
(*   end *)
  
let update_outside gram filt pdata odata ubound =
  let masking_inside_left pdata p q r prod cost =
    let minin = pdata.costs >>! ((prod.leftsid, p, q), infinity) in
      if cost +. minin > ubound then
	false
      else
	true
  in

  let masking_inside_right pdata p q r prod cost =
    let minin = pdata.costs >>! ((prod.rightsid, q, r), infinity) in
      if cost +. minin > ubound then
	false
      else
	true
  in
  let topnodes = top_level_ids filt.levels.(filt.depth-1) in

    voutside gram filt.points topnodes
      masking_inside_left masking_inside_right
      pdata odata

let update_filtration f coarsest ids =
  let f2 = copy_filtration f in
  let themap = Array.init (Array.length f.points) (fun i -> i) in
    for i=1 to f.depth-1 do 
      for j=0 to Array.length f.points-1 do 
	(* should we change the parent pointer of this guy? *)
	if f2.levels.(f.depth-1).parents.(j) = None &&
	  f2.levels.(i).parents.(j) = None &&
	  f2.levels.(i-1).parents.(j) = None &&
	  coarsest.parents.(j) <> None
	then begin 
	  let parent = get (coarsest.parents.(j)) in
	    if not (List.mem parent ids) then begin
	      if (f2.levels.(i).ranks.(j) < f2.levels.(i).ranks.(parent)) then begin 
		themap.(j) <- parent;
		for i2=i to f.depth-1 do
		  f2.levels.(i2).parents.(j) <- Some parent;
		done
	      end;

	      if(f2.levels.(i).ranks.(j) = f2.levels.(i).ranks.(parent)) &&
		(f2.levels.(i).ranks.(parent) < 1 + f.levels.(i).ranks.(parent)) then begin
		  themap.(j) <- parent;
		  for i2=i to f.depth-1 do 
		    f2.levels.(i2).parents.(j) <- Some parent;
		    f2.levels.(i2).ranks.(parent) <- 
		      1 + f2.levels.(i2).ranks.(parent);
		  done
		end
	    end
	end
      done
    done;
    rectify_maps f2;
    printf "about to perform validation!\n%!";
    validate_filtration f2;
    f.levels <- f2.levels;
    f.maps <- f2.maps;
    themap

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

(* unlike ctf, we only have one level of pdata at a time? except we
   need lexical data when lifting... *)
let doit gram nlvls net filt linecosts =
  let coarsest = copy_partition filt.levels.(filt.depth-1) in
  let ubound = ref infinity in 
  let iter = ref 0 in
  let outside = ref None in
  let bottom_pdata = new_parse_data 
    (Frozen.num_symbols gram, Array.length filt.points) in
  let old_map = ref None in
    insert_lexical_costs gram bottom_pdata linecosts;
    printf "depth=%d\n%!" filt.depth;
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
	      find_best_coarse_init gram filt top_pdata 
      in
      let fine_cost = new_lift_coarse_soln gram filt pdatas best_coarse in
      let _ = printf "done lifting\n%!" in

      let yield_ids = List.map (fun (pid,qid) -> pid) best_coarse.yield in  
(*       let old_top_part = copy_partition filt.levels.(filt.depth-1) in *)
      let _ = 
	ubound := min fine_cost !ubound;
	outside := Some (new_outside_data 
			   (Frozen.num_symbols gram, count_topnodes filt.levels.(filt.depth-1)));
	old_map := Some (Array.copy filt.maps.(filt.depth-2));
	update_outside gram filt top_pdata (get !outside) !ubound;
	demote_filtration filt yield_ids;

(* 	let themap = update_filtration filt coarsest yield_ids in *)
(* 	  outside := Some (coarsen_outer (get !outside) themap); *)
		  
      in

      let im = draw_partition filt.levels.(filt.depth-1) filt.points (70,70) in
      let _ = Pnm.save_ppm im (sprintf "part.%d.ppm" !iter) in

      let im = Image.create 70 70 (0,0,0) in
      let curve = List.map (fun pid -> filt.points.(pid)) yield_ids in
	draw_curve im (Array.of_list curve) true (255,0,0);
	Pnm.save_ppm im (sprintf "thebest.%d.ppm" !iter);
	print_parse_tree best_coarse;
	incr iter;
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

  let scene = new_scene edgemap in
  let linecosts = line_costs net scene in
    (*   let bestline = find_best_line linecosts in *)


(*   let nlvls = 3 in *)
  let nlvls = 6 in
  let filtration = make_filtration net.vertices nlvls in
    
  let _ = doit gram nlvls net filtration linecosts in

    printf "hello!\n%!"
