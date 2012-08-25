open Printf
open Util.Hops
open Util.Cops
open Util.Misc
open Abstract
open Geometry
open Graph
open Grammar
open Filtration
open Edges
open Local_inference

module I = Image

exception Parse_failure

type sid = int
type pid = int
type cid = int

let anglevectors = Array.map complex_of_point 
  [| (1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1) |]

(* negative log likelihood ratio *)
let length_term_of_probs pf pb = 
  -. (log( (1. -. pf) /. (1. -. pb) ))

(* negative log likelihood ratio *)
let edge_term_of_probs pf pb = 
  -. (log( pf *. (1. -. pb) /. ((1. -. pf) *. pb) )) 

type global_params = {
  maxiter: int;
  max_interior_frac: float;
  max_exterior_frac: float;
  min_on_frac: float;
  max_len_auto_edge: float;
  backwards_factor: float;
  edge_term: float;  
  length_term: float;
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

type image_inside_parse_data = {
  costs: (sid*pid*pid, float) hash;
  bestleftcost: (sid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
}

type image_outside_parse_data = {
  ocosts: ((sid*pid*pid), float) hash;
  bestocost: ((sid*pid), float) hash;
}

(* data carried between iterations of branch and bound *)
type global_interpretation_state = {
  mutable ubound: float;
  mutable best_parse_tree: image_parse_tree option;
  mutable outside: image_outside_parse_data option;
  mutable filtration: (int*int) Filtration.filtration;
  mutable old_filtration: (int*int) Filtration.filtration;
  mutable best_tri_cost: (Abstract.cid*int*int*int) Bloom.t option;
  (* mutable best_tri_cost: (Abstract.cid*int*int*int,float) hash option; *)
}

let new_outside_parse_data (nsyms,npts) =
  let bigsize = min (nsyms*npts*npts) 100000 in 
    {ocosts = mkhash bigsize;
     bestocost = mkhash (nsyms*npts);
    }

let new_inside_parse_data (nsyms,npts) = 
  let bigsize = min (nsyms*npts*npts) 100000 in 
    {costs = mkhash bigsize;
     bestleftcost = mkhash (nsyms*npts);
     split = mkhash bigsize;
     thisbest = mkhash nsyms;
     thisbestqual = mkhash nsyms;
    }

let copy_inside_parse_data pdata = 
  {costs = Hashtbl.copy pdata.costs;
   bestleftcost = Hashtbl.copy pdata.bestleftcost;
   split = Hashtbl.copy pdata.split;
   thisbest = Hashtbl.copy pdata.thisbest;   
   thisbestqual = Hashtbl.copy pdata.thisbestqual;
  }

let draw_curve im curve closed color = 
  let n = Array.length curve in
  let maxi = if closed then n-1 else n-2 in
  for i = 0 to maxi do
    Draw.draw_line im curve.(i) curve.((i+1) mod n) color;
  done

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


let viterbi_outside gram global points topnodes geometry_cost masking_left masking_right pdata odata =
  let nnodes = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

  let rule_cost prod (p,q,r) =
    geometry_cost gram global prod p q r
  in

    (* The start symbol has a trivial outer tree at every point. *)
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


let viterbi_inside gram global points topnodes geometry_cost masking masking2 masking3 data name ubound = 
  let npts = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

  let nmasktot, nnomask, nmask2tot, nnomask2, nmask3tot, nnomask3 = 
    ref 0, ref 0, ref 0, ref 0, ref 0, ref 0 
  in

  let rule_cost pdata gram sym prod (p,q,r) = 
    let leftkey, rightkey = (prod.leftsid, p, q), (prod.rightsid, q, r) in
    let leftcost  = data.costs >>! (leftkey,  infinity) in
    let rightcost = data.costs >>! (rightkey, infinity) in
    let shapecost = geometry_cost gram global prod p q r in
      leftcost +. rightcost +. shapecost
  in

  let tryout sid pid qid qual thesplit = 
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

let cost_of_segment params edgedata local p q =
  let non, ninterior, nexterior, ntot = ref 0., ref 0., ref 0., ref 0. in
  let cost_fwd, cost_rev = ref 0., ref 0. in
  let vec = (complex_of_point q) -& (complex_of_point p) in
    List.iter
      begin fun r ->
	ntot := !ntot +. 1.;

	if I.get edgedata.Edges.edgemap r then  
	  non := !non +. 1.
	else begin
	  (* don't want too many interior or exterior points (note
	     that inside has bogus values at edges) *)
	  if not local.fake then begin
	    if (I.get local.inside r) then 
	      ninterior := !ninterior +. 1.
	    else
	      nexterior := !nexterior +. 1.;
	  end
	end;

	if I.get edgedata.edgemap r then begin 
	  if not local.fake then begin 
	    (* directional masking at edge pixels *)
	    let ang = anglevectors.(round_angle (local.orientations >> r)) in
(* 	    let dotprod = dot vec (local.orientations >> r) in *)
	    let dotprod = dot vec ang in
	      if dotprod >= 0. then begin 
		cost_fwd := !cost_fwd +. params.edge_term +. params.length_term;
		cost_rev := !cost_rev -. 
		  params.backwards_factor *. 
		  (params.edge_term +. params.length_term);
	      end
	      else begin 
		cost_rev := !cost_rev +. params.edge_term +. params.length_term;
		cost_fwd := !cost_fwd -. 
		  params.backwards_factor *. 
		  (params.edge_term +. params.length_term);
	      end
	  end
	  else begin 
	    (* no directional masking *)
	    cost_fwd := !cost_fwd +. params.edge_term +. params.length_term;		
	    cost_rev := !cost_rev +. params.edge_term +. params.length_term;
	  end
	end
	else begin 
	  cost_fwd := !cost_fwd +. params.length_term;
	  cost_rev := !cost_rev +. params.length_term;
	end;
      end
      (Draw.line p q);

    if ((!ninterior /. !ntot < params.max_interior_frac) &&
	  (!nexterior /. !ntot < params.max_exterior_frac))
      &&
      ((!non /. !ntot >= params.min_on_frac) || 
	 (!ntot <= params.max_len_auto_edge)) then 
	!cost_fwd, !cost_rev
    else
      infinity, infinity

let get_lexical_costs params gram net global local edgedata =
  let pdata = new_inside_parse_data 
    (Frozen.num_symbols gram, Array.length global.filtration.points)
  in

  Hashtbl.iter 
    begin fun (pid,qid) () ->
      let cost_fwd, cost_rev = cost_of_segment params edgedata local 
	net.vertices.(pid) net.vertices.(qid) in 

	if cost_fwd < infinity || cost_rev < infinity then begin
	  Frozen.iter_symbols gram 
	    begin fun sym ->
	      if (not sym.startable && sym.sdata.straightcost < infinity) then begin
		let cost_fwd = cost_fwd +. sym.sdata.straightcost in
		let cost_rev = cost_rev +. sym.sdata.straightcost in
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
    pdata

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


let propagate_lexical_costs gram filt pdata = 
  let count_topnodes part =
    let n = ref 0 in
      Array.iter 
	begin fun par ->
	  if par = None then incr n
	end
	part.parents;
      !n
  in
  let pdatas = Array.init filt.depth 
    begin fun i -> 
      if i=0 then pdata
      else
	let npts = count_topnodes filt.levels.(i) in
	  new_inside_parse_data (Frozen.num_symbols gram, npts)
    end in

    for lvl=1 to filt.depth-1 do 
      printf "propagate_lexical_costs %d\n%!" lvl;
      coarsen_lexical_costs filt.maps.(lvl-1) pdatas.(lvl-1) pdatas.(lvl);
    done;
    pdatas

(* let get_geometry_cost_admissible gram global prod pid qid rid = *)
(*   (\* hash table version *\) *)
(*   (\* (get global.best_tri_cost) >> (prod.cid,pid,qid,rid) *\) *)
(*   (\* bloom filter version *\) *)
(*   Bloom.query (get global.best_tri_cost) (prod.cid,pid,qid,rid) *)

let get_geometry_cost_inadmissible gram global prod pid qid rid =
  let points = global.filtration.points in
  let shape = Shape.shape_of_ints_bme points.(pid) points.(qid) points.(rid) in
    Models.Simple.prod_cost prod shape

let get_geometry_cost_admissible = get_geometry_cost_inadmissible

let get_the_tri_bounds gram filt =
  (* admissible *)
  (* (Calc_bounds.calc_bounds gram filt) *)

  (* inadmissible *)
  Calc_bounds.calc_bounds_fake gram filt

(* ^^^ inad->ad was last change after detexout/local_inference4 ^^^ *)

let find_best_coarse_init gram global pdata = 
  let masking_none p q r prod cost = true in
  let masking2_none p q sid cost = true in
  let masking3_none p sid cost = true in
  let topnodes = top_level_ids 
    global.filtration.levels.(global.filtration.depth-1) 
  in
    viterbi_inside gram global global.filtration.points topnodes
      get_geometry_cost_admissible masking_none masking2_none masking3_none	
      pdata
      "init" global.ubound;

    harvest_best_tree_overall gram pdata

let find_best_coarse gram global pdata =
  (* outside had a coarser filtration *)
  let map = global.old_filtration.maps.(global.old_filtration.depth-2) in
  let ubound = global.ubound in
  let odata = get global.outside in

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
  let topnodes = top_level_ids 
    global.filtration.levels.(global.filtration.depth-1) 
  in
    viterbi_inside gram global global.filtration.points topnodes
      get_geometry_cost_admissible masking_outside masking2_outside masking3_outside	
      pdata
      "inside" ubound;

    harvest_best_tree_overall gram pdata

let lift_coarse_soln gram global pdatas ptree =
  let masking_none p q r prod cost = true in
  let masking2_none p q sid cost = true in
  let masking3_none p sid cost = true in
  let ptree = ref ptree in
(*     begin try begin  *)
      for i=global.filtration.depth-2 downto 0 do 
	let yield_pts = List.map (fun (pid,qid) -> pid) !ptree.yield in
	let topnodes = 
	  let topnodes = ref [] in
	    Array.iteri 
	      begin fun j par ->
		if par = None && 
		  (List.mem global.filtration.maps.(i).(j) yield_pts) then begin 
		    topnodes := j :: !topnodes;
		  end
	      end
	      global.filtration.levels.(i).parents;
	    Array.of_list !topnodes
	in
	  viterbi_inside gram global global.filtration.points topnodes
	    get_geometry_cost_inadmissible masking_none masking2_none masking3_none
	    pdatas.(i) "lifting" infinity;
	  ptree := harvest_best_tree_overall gram pdatas.(i);
      done;
(*       Some !ptree, !ptree.qual *)
    !ptree

(*     end *)
(*     with Parse_failure ->  *)
(*       None, infinity *)
(*     end *)
  
let update_outside gram global pdata  =
  let ubound = global.ubound in
  let odata = new_outside_parse_data
    (Frozen.num_symbols gram, Array.length global.filtration.points) in

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
  let topnodes = top_level_ids 
    global.filtration.levels.(global.filtration.depth-1) 
  in

    viterbi_outside gram global global.filtration.points topnodes
      get_geometry_cost_admissible masking_inside_left masking_inside_right
      pdata odata;

    odata


let do_global_inference params gram net filt local edgedata name =
  let global = {
    ubound = infinity;
    best_parse_tree = None;
    outside = None;
    filtration = filt;
    old_filtration = filt;
    best_tri_cost = get_the_tri_bounds gram filt;
  } in

  let finest_inside = get_lexical_costs params gram net global local edgedata in

    begin try  
      for iter = 0 to params.maxiter-1 do

	(* Find the best coarse solution. *)
	let inside_levels = propagate_lexical_costs gram filt finest_inside in
	let coarsest_inside = inside_levels.(Array.length inside_levels - 1) in
	let best_coarse = 
	  if iter = 0 then 
	    find_best_coarse_init gram global coarsest_inside
	  else
	    find_best_coarse gram global coarsest_inside
	in

	(* Lift the coarse solution to a fine one. *)
	let fine_soln = lift_coarse_soln gram global inside_levels best_coarse in
	let _ = 
(* 	  print_the_yield fine_soln filt.points; *)
	  print_parse_tree best_coarse;

	  if fine_soln.qual < global.ubound then begin
	    global.best_parse_tree <- Some fine_soln;
	    global.ubound <- fine_soln.qual;
	  end;
	in

	(* Calculate outside table for bounding purposes. *)
	let _ = 
	  global.outside <- Some (update_outside gram global coarsest_inside);
	in

	(* Enrich filtration in area of the parse. *)
	let _ = 
	  let yield_ids = List.map (fun (pid,qid) -> pid) best_coarse.yield in  
	    global.old_filtration <- copy_filtration global.filtration;
	    demote_filtration global.filtration yield_ids;		  
	    global.best_tri_cost <- get_the_tri_bounds gram filt;
	in
	  ()

      done;

    with Parse_failure -> ()
    end;

    
    let im = draw_parse (get global.best_parse_tree)
      global.filtration.points edgedata.edgemap in
    let pim = draw_partition 
      global.filtration.levels.(global.filtration.depth-1)
      global.filtration.points
      (Image.width im, Image.height im)
    in
      Pnm.save_ppm im (sprintf "%s.bestfine.ppm" name);	
      Pnm.save_ppm pim (sprintf "%s.partition.ppm" name);	
      (* 	  print_the_yield (get global.best_parse_tree) filt.points; *)

      get global.best_parse_tree

let true_cost params gram ptree edgedata points =
  let qual = ref 0. in
  let seen = Image.map (fun x -> false) edgedata.edgemap in
  let rec proc t =
    if t.thesplit = None then begin 
      let p,q = points.(t.bg), points.(t.en) in
      let line = Draw.line p q in
	List.iter
	  begin fun r ->
	    if not (I.get seen r) then begin 
	      if I.get edgedata.edgemap r then
		qual := !qual +. params.edge_term +. params.length_term
	      else
		qual := !qual +. params.length_term;

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
