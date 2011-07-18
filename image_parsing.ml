open Printf
open Abstract
open Grammar
open Scanf
open Util.Hops
open Util.Misc
open Complex
open Curve_network
module Q = PriorityQueue
let sqrt = Pervasives.sqrt (* Complex has its own *)
let exp = Pervasives.exp (* Complex has its own *)
let log = Pervasives.exp (* Complex has its own *)

let maxedge = 100.
let niter = 3
let dataweight = 1. (* 0. *) (*0.1*)
let gramweight = 1000.
let shapeweight = 1.
let straightweight = 1e5 (* 1e5 *) (* sucks: 1000.*)
let ell_straightcost = -. 0.01

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
let canon p q = if p > q then q,p else p,q

type sid = int
type cid = int
type pid = int

type image_parse_data = {
  costs: (sid*pid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
  mutable bestqual: float;
  mutable beststart: (cid*pid*pid) option;
}

type image_parse_tree = {
  nt: sid;
  bg: pid;
  en: pid;
  yield: (pid*pid) list;
  simple: bool;
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

let intersect ls1 ls2 =
  let rv = ref false in
    List.iter 
      begin fun x ->
	if (not !rv) && List.mem x ls2 then begin
	  rv := true;
	  let (a,b) = x in
	    printf "intersect=true, (%d,%d) in ls2\n" a b;
	    List.iter (fun (a,b) -> printf "(%d,%d)\n" a b) ls1;
	    printf "---\n";
	    List.iter (fun (a,b) -> printf "(%d,%d)\n" a b) ls2;
	    printf "---\n";
	end
      end
      ls1;
    !rv

let rec harvest_best_tree gram pdata nt (bg,en) = 
  match pdata.split >> (nt,bg,en) with
      Some (cid, q) ->
	let prod = gram.get_composition cid in
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
	 yield = [canon bg en];
	 simple = true;
	 left = None;
	 right = None;
	 qual = pdata.costs >>! ((nt,bg,en), infinity);
	 thesplit = None;
	}

let harvest_best_tree_overall gram pdata = 
  let _,p,q = get pdata.beststart in
  let rv = harvest_best_tree gram pdata (gram.start()).sid  (p, p) in
    rv

let print_parse_tree t = 
  let rec printer t tabs =
    printf "%s  s%d -> p%d p%d (simple=%b, |yield|=%d)\n" (String.make (2*tabs) ' ') t.nt t.bg t.en t.simple
      (List.length t.yield);
    if t.left != None then
      printer (get t.left) (tabs+1);
    if t.right != None then
      printer (get t.right) (tabs+1)
  in
    printer t 0

let harvest_best_curve_ids gram split nt (bg,en) =
  let segments = ref [] in
  let rec harvest nt (p,r) = 
    match split >> (nt,p,r) with
	Some (cid, q) ->
	  let prod = gram.get_composition cid in
	    harvest prod.leftsid (p,q);
	    harvest prod.rightsid (q,r);
      | None ->
	  segments := (p,r) :: !segments;
  in
    harvest nt (bg,en);
    Array.of_list (List.rev !segments)

let show_example_curve im sym = 
  let curve = Curve.normalize sym.sdata.Sdf.curve in
  let scale = float_of_int (min (Image.width im) (Image.height im)) in
  let curve = Array.map (fun z -> (int_of_float (z.re*.scale)), (int_of_float (z.im*.scale))) curve in
    for i = 0 to Array.length curve - 2 do
      Draw.draw_line im curve.(i) curve.(i+1) (0,255,0);
    done

let draw_changing_curve im curve= 
  for i=0 to Array.length curve-2 do
    let counter = i mod 6 in
    let color = (counter * 50, 0, 255 - counter * 50) in
      Draw.draw_line im curve.(i) curve.(i+1) color;
  done


let new_parse_data () = 
  {costs = mkhash 1000;
   split = mkhash 1000;
   thisbest = mkhash 100;
   thisbestqual = mkhash 100;
   bestqual = infinity;
   beststart = None;
  }

let rule_cost pdata net gram sym prod (p,q,r) = 
  let left, right = gram.get_symbol prod.leftsid, gram.get_symbol prod.rightsid in
  let lfactor = left.sdata.Sdf.dscale /. sym.sdata.Sdf.dscale in
  let rfactor = right.sdata.Sdf.dscale /. sym.sdata.Sdf.dscale in
  let selfrec = (prod.topsid = prod.leftsid) in
  let selfrecfactor = if selfrec then 0.5 else 1.0 in
  let shape = Shape.shape_of_ints_bme net.points.(p) net.points.(q) net.points.(r) in
  let geocost = shapeweight *. gramweight *. prod_cost prod shape in


  let selfrecfactor, lfactor, rfactor = 1.,1.,1. in

  let cost = 
    selfrecfactor *. 
      (lfactor *. (pdata.costs >>! ((prod.leftsid, p, q),infinity)) +.
	 rfactor *. (pdata.costs >>! ((prod.rightsid, q, r),infinity))) +.
      geocost (* /. sym.sdata.Sdf.dscale *)
  in
    cost

(*   let costfn edgewt sym = *)
(*     let qual = edgewt +. straightweight *. sym.straightcost in *)
(*     let qual = qual /. sym.sdata.Sdf.dscale in *)
(*       qual *)
(*   in *)


let parse_image gram net usededges required_trees tryid = 
  let n = npoints net in
  let data = new_parse_data () in

  (* figure out subtree shape costs *)
  let subtree_shape_costs = Array.init (gram.num_symbols()) (fun _ -> None) in
  let rec populate_ssc nt = 
    if subtree_shape_costs.(nt) = None then begin
      let bestcost = ref infinity in
	gram.iter_decompositions (gram.get_symbol nt)
	  begin fun prod ->
	    if prod.leftsid = prod.topsid then begin
	      bestcost := 0.;
	    end
	    else begin 
	      let shapecost = prod_cost prod Shape.straight in
	      let cost = (populate_ssc prod.leftsid) +. (populate_ssc prod.rightsid) +. shapecost in
		assert(shapecost >= 0.);
		bestcost := min !bestcost cost;
	    end
	  end;
	subtree_shape_costs.(nt) <- Some !bestcost;
	!bestcost
    end
    else 
      get subtree_shape_costs.(nt)
  in
  let _ = populate_ssc (gram.start ()).sid in
  let subtree_shape_costs = Array.map (function None -> infinity | Some x -> x) subtree_shape_costs in

    List.iter 
      begin fun ptree ->
	printf "This tree is required, we hope:\n";
	print_parse_tree ptree;
	printf "done with required tree\n";
      end
      required_trees;
    
  let display iter sym nt (bg,en) =
    let im = Pnm.load_pgm Sys.argv.(2) in
    let im = Image.map Image.color_of_gray im in
    let ptree = harvest_best_tree gram data nt (bg,en) in
      print_parse_tree ptree;
    let curve_ids = harvest_best_curve_ids gram data.split nt (bg,en) in
    let firstid, _ = curve_ids.(0) in
    let curve = Array.map (fun (p,q) -> net.points.(p)) curve_ids in
    let curve = Array.append curve [| net.points.(firstid) |] in
      show_example_curve im sym;
      draw_changing_curve im curve;
      printf "prefix is %s" Sys.argv.(3);
      Pnm.save_ppm im (sprintf "%s.try%d.iter%d.nt%d.ppm" Sys.argv.(3) tryid iter nt)
  in

  let costfn edgewt sym =
(*     let qual = dataweight *. edgewt +. gramweight *. straightweight *. sym.straightcost in *)
    let qual = dataweight *. edgewt +. gramweight *. subtree_shape_costs.(sym.sid) in
(*     let qual = qual /. sym.sdata.Sdf.dscale in *)
      qual
  in

  let tryout sid p q qual thesplit = 
    let rv = ref false in
      if qual < (data.costs >>! ((sid, p, q), infinity)) then 
	rv := true;
      if qual <= (data.costs >>! ((sid, p, q), infinity)) then begin
	data.costs << ((sid, p, q), qual);
	data.split << ((sid, p, q), thesplit);
	if qual < (data.thisbestqual >>! (sid, infinity)) then begin
	  data.thisbestqual << (sid, qual);
	  data.thisbest << (sid, (p,q));
	end;
      end;
      !rv
  in
  let tryout_ignore sid p q qual thesplit = ignore (tryout sid p q qual thesplit) in

  let rec insert_tree ptree =
    tryout_ignore ptree.nt ptree.bg ptree.en ptree.qual ptree.thesplit;
    if ptree.left != None then
      insert_tree (get ptree.left);
    if ptree.right != None then
      insert_tree (get ptree.right);
  in
    List.iter insert_tree required_trees;
    
    gram.iter_symbols 
      begin fun sym ->
	if not sym.closed then begin
	  for p = 0 to n-1 do
	    tryout_ignore sym.sid p p (costfn maxedge sym) None;
	  done;
	  Hashtbl.iter
	    begin fun (p,q) w ->
	      (* neglog -> neglog ratio *)
	      let prob = exp (-.w) in
	      let prob = max prob 0.000000000001 in

	      let w = w +. log (1. -. prob) in

	      let ell = gram.get_symbol 1 in
	      let ell3 = List.hd (gram.get_decompositions ell) in
	      let shapecost = prod_cost ell3 Shape.straight in
		printf "shapecost=%f\n" shapecost;

	      (* switched below - now illegal to reuse edges... *)
	      let len = (sqrt (float_of_int (dist2 net.points.(p) net.points.(q)))) in  
  	      let w = min w (maxedge *. len) in
	      let w = if usededges >>? (canon p q) then infinity else w in
	      let cost = (costfn w sym) +. shapecost *. (len /. 3.) in
		tryout_ignore sym.sid q p cost None;
		tryout_ignore sym.sid p q cost None;
	    end
	    net.segments;
	end
      end;


    let ntchanged = Array.init (gram.num_symbols()) (fun x -> true) in
    let changed = ref true in
    let iter = ref (-1) in
      (*     for iter = 0 to niter do *)
      while !changed do
	incr iter;
	changed := false;

	gram.iter_all_decompositions
	  begin fun sym prod ->
	    if ntchanged.(prod.leftsid) || ntchanged.(prod.rightsid) then begin
	      let ntiter = ref 0 in
	      let nchanged = ref 17 in
		while !nchanged > 0 do
		  nchanged := 0;
		  incr ntiter;
		  printf "%d: %d/%d (%d)\n%!" !iter sym.sid (gram.num_symbols()) prod.cid;

		  for p = 0 to n-1 do
		    for q = 0 to n-1 do 
		      if data.costs >>! ((prod.leftsid, p, q),infinity) < infinity then begin
			for r = 0 to n-1 do
			  let dist_pq = dist2 net.points.(p) net.points.(q) in
			  let dist_qr = dist2 net.points.(q) net.points.(r) in
			  let dist_pr = dist2 net.points.(p) net.points.(r) in
			    if ((sym.closed && p=r) || ((not sym.closed) && p != r)) &&
			      ((prod.topsid != prod.leftsid) || (dist_pq < dist_pr && dist_qr < dist_pr))
			    then begin
			      let cost = rule_cost data net gram sym prod (p,q,r) in

				if tryout sym.sid p r cost (Some (prod.cid, q)) then begin
				  changed := true;
				  incr nchanged;
				end;

				if sym.closed && cost < data.bestqual &&
				  dist2 net.points.(p) net.points.(q) >= 10 then begin
				    data.bestqual <- cost;
				    data.beststart <- Some (prod.cid,p,q);
				    printf "start: p%d p%d: %f\n%!" p q cost;
				  end
			    end

			done
		      end
		    done
		  done;

		  printf "%d changed\n" !nchanged;
		  (* this isn't right if we have multiple rules *)
		  if !nchanged = 0 && !ntiter = 1 then begin
		    ntchanged.(sym.sid) <- false;
		    printf "NO CHANGE!!!\n";
		  end
		  else
		    ntchanged.(sym.sid) <- true;
		done;

		if data.thisbest >>? sym.sid then begin
		  let p,q = data.thisbest >> sym.sid in
		    printf "\ts%d -> p%d p%d: %f\n%!" sym.sid p q (data.thisbestqual >> sym.sid);
		    if sym.sid = 0 && !iter > 0 (* true *) then begin
		      printf "displaying s%d\n" sym.sid;
		      display !iter sym sym.sid (p,q);
		    end
		end
	    end
	  end;
      done;
      data


type queue_entry = {
  ptree: image_parse_tree;
  reqd: image_parse_tree list;
}

let _ = 
  let edgecosts = load_network_float Sys.argv.(1) in

  let curve = Datasets.get_example () in
  let curve = Curve.subsample_dp curve 30 2 in
  let family = Sdf.sparse_family_of_curve_debug curve 2 in
  let gram = grammar_of_family family in
  let gram = merge_leaves gram in
  let ell = gram.get_symbol 1 in
  let ellell = gram.get_composition 0 in
  let _ = 
    ell.straightcost <- ell_straightcost;
    match ellell.geom with
	Parzen g ->
	  printf "ell.sigma = %f\n%!" g.Parzen.baseline_sigma;
	  (* 	  g.Parzen.baseline_sigma <- 3.0; (\* 10.0 pretty good *\) *)
      | Improper -> failwith "improper?"
  in
    
    (*   let gram = Ex_articulator.gram in *)

(*     parse_image gram edgecosts (mkhash 100) [] *)


  let thecounter = ref 0 in
  let counter () = incr thecounter; !thecounter in
  let best_parse_tree = ref None in
  let bestqualseen = ref infinity in

  let queue = ref [] in
  let push x = queue := x :: !queue in
  let pop () = 
    let ls = List.rev !queue in
      queue := List.rev (List.tl ls);
      List.hd ls
  in

(*   let queue = Q.make (fun a b -> a.qual <= b.qual) in *)

  let pdata = parse_image gram edgecosts (mkhash 10) [] (counter()) in
  let ptree = harvest_best_tree_overall gram pdata in
(*     Q.add queue ptree; *)
(*     push ptree; *)
    push {ptree=ptree; reqd=[]};

    try
      while true do 
	printf "POPPING BEST TREE";
(* 	let ptree = (\* Q.pop queue *\) pop () in *)
	let qentry = (* Q.pop queue *) pop () in
	let fragments = ref [] in
	let rec get_fragments ptree = 
	  if ptree.simple then
	    fragments := ptree :: !fragments
	  else begin
	    if ptree.left != None then
	      get_fragments (get ptree.left);
	    if ptree.right != None then 
	      get_fragments (get ptree.right);
	  end
	in
	  print_parse_tree qentry.ptree;

	  get_fragments qentry.ptree;

	  List.iter 
	    begin fun ptree ->
	      let usededges = mkhash 100 in
	      let add_yield t = 
		List.iter 
		  begin fun (p,q) ->
		    usededges << (canon p q, ());
		  end
		  t.yield
	      in
		add_yield ptree;
		List.iter add_yield qentry.reqd;

		let pdata = parse_image gram edgecosts usededges
		  (ptree :: qentry.reqd) (counter()) in
		let new_ptree = harvest_best_tree_overall gram pdata in
		let new_qentry = {
		  ptree=new_ptree; 
		  reqd = ptree :: qentry.reqd;
		} in
		  if not new_ptree.simple && 
		    new_ptree.qual < !bestqualseen then
		      (* Q.add queue ptree; *)
		      push new_qentry;
		  if new_ptree.simple && 
		    new_ptree.qual < !bestqualseen then begin
		      bestqualseen := new_ptree.qual;
		      best_parse_tree := Some new_ptree;
		      printf "NEW BEST!!!\n";
		      print_parse_tree new_ptree;
		  end;
	    end 
	    !fragments;
      done
    with Failure _ ->
      printf "We're done! Hopefully we have a parse...\n";
      print_parse_tree (get !best_parse_tree)

(* at some point, will need to speed this up somehow *)
(* correct way - compute dependency ordering on the nonterminals *)
(* nasty way - kill ptable entries that are too far off from global opt of a nont *)
