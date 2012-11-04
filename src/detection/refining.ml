open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph

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

type image_parse_data = {
  costs: (sid*pid*pid, float) hash;
  bestleftcost: (sid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
}

type pt = int * int


type image_scene_data = {
  parsedata: image_parse_data;
  mutable clutter_lines: (pt*pt) list;
  mutable edgemap: bool Image.t;
  mutable onpixels: bool Image.t;
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

let new_parse_data () = 
  {costs = mkhash 1000;
   bestleftcost = mkhash 1000;
   split = mkhash 1000;
   thisbest = mkhash 100;
   thisbestqual = mkhash 100;
  }

let new_scene edgemap = 
  {parsedata = new_parse_data ();
   clutter_lines = [];
   edgemap = edgemap;
   onpixels = I.map (fun x -> false) edgemap;
  }

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


let line_costs net scenedata = 
  let im = Image.map (function false -> (255,255,255) | true -> (0,0,0)) scenedata.edgemap in

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
	  if (!non /. !ntot >= 0.5) || (!ntot <= 10.) then begin
(* 	  if true then begin *)
	    fnet.edges << ((pi,qi), !cost);
	    List.iter
	      begin fun r ->
		I.set im (128,128,128) r;
	      end
	      line;
	  end
      end
      net.edges;
    printf "Done with line_costs\n%!";
    fnet, im

let create_hierarchy gf nlvls =
  let parents = Array.map (fun x -> None) gf.vertices in
  let ranks = Array.map (fun x -> nlvls+1) gf.vertices in
    for i = 1 to nlvls do 
      Array.iteri 
	begin fun vid (x,y) ->
	  if parents.(vid) = None then begin
	    let x', y' = (x asr i) lsl i, (y asr i) lsl i in
	      if (x', y') <> (x,y) then begin 
		let newvid = find (x', y') gf.vertices in
		  parents.(vid) <- Some newvid;
		  ranks.(vid) <- i;
	      end
	  end
	end
	gf.vertices;
    done;
    parents, ranks

let rec findit parents i =
  begin match parents.(i) with 
      Some j -> findit parents j
    | None -> i
  end

let draw_hierarchy (gf,parents) (width,height) =
  let sqsize = 5 in
  let colors = Array.map (fun x -> (Random.int 256, Random.int 256, Random.int 256)) parents in
  let im = Image.create (sqsize*width) (sqsize*height) (255,255,255) in
    Array.iteri 
      begin fun i (x,y) ->
	let j = findit parents i in
	  for a=0 to sqsize-1 do
	    for b=0 to sqsize-1 do
	      I.set im colors.(j) (x*sqsize+a, y*sqsize+b);
	    done
	  done
      end
      gf.vertices;
    im

let lift_costs gf parents =
  let topgf = {
    vertices = Array.copy gf.vertices;
    edges = mkhash (Hashtbl.length gf.edges);
    nbrs = Hashtbl.copy gf.nbrs;
  } in
    Hashtbl.iter
      begin fun (pid,qid) cost ->
	let pid, qid = findit parents pid, findit parents qid in
	  if cost < (topgf.edges >>! ((pid,qid), infinity)) then begin 
	    topgf.edges << ((pid,qid), cost);
	  end
      end
      gf.edges;

    topgf

let vinside gram gf topnodes pdata name = 
(*   let n = Graph.nvertices gf in *)
  let nnodes = Array.length topnodes in
  let nsymbols = Frozen.num_symbols gram in

  let rule_cost sym prod (pid,qid,rid) = 
    let leftkey, rightkey = (prod.leftsid, pid, qid), (prod.rightsid, qid, rid) in
    let leftcost  = pdata.costs >>! (leftkey,  infinity) in
    let rightcost = pdata.costs >>! (rightkey, infinity) in
    let shape = Shape.shape_of_ints_bme 
      gf.vertices.(pid) gf.vertices.(qid) gf.vertices.(rid) in
      leftcost +. rightcost +. (Models.Simple.prod_cost prod shape)
  in

  let tryout sid pid qid qual thesplit = 
    let rv = ref false in
      (* vvvv this was <=, not sure why... *)
      (* (prevents key error when all costs infinite) *)
      if qual < (pdata.costs >>! ((sid, pid, qid), infinity)) then begin
	rv := true;
	pdata.costs << ((sid, pid, qid), qual);
	pdata.split << ((sid, pid, qid), thesplit);
	if qual < (pdata.thisbestqual >>! (sid, infinity)) then begin
	  pdata.thisbestqual << (sid, qual);
	  pdata.thisbest << (sid, (pid,qid));
	end;
      end;
      !rv
  in

    printf "About to start actual parsing!\n%!";

    Frozen.iter_symbols_rev gram
      begin fun sym ->
	Frozen.iter_decompositions gram sym
	  begin fun prod ->
	    printf "\n>>> s%d -> s%d s%d\n%!" prod.topsid prod.leftsid prod.rightsid;

	    Array.iteri
	      begin fun pnum pid ->

		printf "%s\t" name;
		printf "  nt=%d/%d pnum=%d/%d (s%d -> s%d s%d)\n" 
		  sym.sid nsymbols pnum nnodes prod.topsid prod.leftsid prod.rightsid;

		Array.iter
		  begin fun qid ->
		    let leftcost = pdata.costs >>! ((prod.leftsid, pid, qid),infinity) in
		      if leftcost < infinity then begin

			Array.iter
			  begin fun rid ->
			    if not sym.startable || (pid=rid) then begin
			      (* if sym.startable = (p=r) then begin *)
			      let cost = rule_cost sym prod (pid,qid,rid) in
			      let change = tryout sym.sid pid rid cost (Some (prod.cid, qid)) in
				ignore change
			    end
			  end
			  topnodes;(* end for rid *)

		      end
		  end
		  topnodes;(* end for qid *)

	      end
	      topnodes; (* end for pid *)

	  end; (* end iter_decompositions *)

	if pdata.thisbest >>? sym.sid then begin 
	  let p,q = pdata.thisbest >> sym.sid in
	    printf "\ts%d -> p%d p%d: %f\n%!" sym.sid p q (pdata.thisbestqual >> sym.sid);
	end; (* end iter_symbols *)
      end;
    ()
  

let demote (parents, ranks) ids =
  let newparents = Array.copy parents in
  let newranks = Array.copy ranks in
    List.iter
      begin fun pid ->
	newranks.(pid) <- max 1 (ranks.(pid) - 1);
      end
      ids;

    Array.iteri
      begin fun i par ->
	match par with
	    Some j ->
	      if newranks.(i) = newranks.(j) then begin 
		newparents.(i) <- None;
	      end
	  | None -> () 
      end
      parents;

    for i = 0 to Array.length parents - 1 do
	parents.(i) <- newparents.(i);
	ranks.(i) <- newranks.(i);
    done

let refining gram (parents, ranks) linecosts =
  for iter=0 to 10 do
    let im = draw_hierarchy (linecosts,parents) (70,70) in
    let _ = Pnm.save_ppm im (sprintf "partition.%d.ppm" iter) in
    let gf = lift_costs linecosts parents in
    let pdata = new_parse_data () in

    (* compile list of top-level vertices to iterate over in cky *)
    let topnodes = 
      let ls = ref [] in
	Array.iteri 
	  begin fun i _ ->
	    if parents.(i) = None then begin 
	      ls := i :: !ls;
	    end
	  end
	  gf.vertices;
	Array.of_list (List.rev !ls)
    in

    (* put lexical costs in pdata *)
    let _ = 
      Frozen.iter_symbols gram
	begin fun sym ->
	  if (not sym.startable) && sym.sdata.straightcost < infinity then begin 
	    Hashtbl.iter
	      begin fun (pid,qid) cost ->
		let cost = cost +. sym.sdata.straightcost in
		  if cost < (pdata.costs >>! ((sym.sid,pid,qid), infinity)) then begin
		    pdata.costs << ((sym.sid, pid, qid), cost);
		    pdata.costs << ((sym.sid, qid, pid), cost);
		    pdata.split << ((sym.sid, pid, qid), None);
		    pdata.split << ((sym.sid, qid, pid), None);
		  end
	      end
	      gf.edges;      
	  end
	end
    in

    let _ = vinside gram gf topnodes pdata (sprintf "refining %d" iter) in
    let yieldbasic = ref true in 

      while !yieldbasic do 
	yieldbasic := true;
      done
	wtf is with this done here????? or the loop? did we start trying to lift?
      
    let best_tree = harvest_best_tree_overall gram pdata in
    let yieldpts = List.map (fun (pid,qid) -> pid) best_tree.yield in
    let yieldchildren = 
      let yieldchildren = ref yieldpts in
	Array.iter 
	  begin fun id par ->
	    match par with 
		None -> ()
	      | Some par ->
		  if List.mem par yieldpts then begin 
		    yieldbasic := false;
		    yieldchildren := id :: !yieldchildren;
		  end
	  end 
	  parents;
	Array.of_list !yieldchildren
    in

      demote (parents, ranks) yieldpts;
  done


let _ = 
(*   let excurve = Curve.load "romer/newann/IMG0000.curve" in *)
  let excurve = Curve.load "romer/newann/IMG0020.curve" in
  let sdf = Sdf.load_family "romer/misc/romer1.sdf" in
  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = merge_leaves gram in
  let _ = reorder_symbols gram in

  let imname = "edges.2.pgm" in
  let edgemap = Pnm.load_pgm imname in
  let edgemap = Image.map (fun x -> x=0) edgemap in

  let granularity, size = (242/4), (242/4) in
  let net = Gridparsing.make_net granularity size in

  let scene = new_scene edgemap in
  let linecosts, linecostim = line_costs net scene in

(*   let nlvls = 3 in *)
(*   let nlvls = 4 in *)
  let nlvls = 5 in
  let (parents, ranks) = create_hierarchy net nlvls in
    
  let _ = refining gram (parents, ranks) linecosts in

    printf "hello!\n%!"
