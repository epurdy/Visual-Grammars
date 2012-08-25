open Printf
open Util.Misc
open Util.Hops
open Graph
module I = Image
module Q = PriorityQueue
open Abstract
open Sdf
open Shock

let verbose = false 
      
let _ = 
  let imname = Sys.argv.(1) in
  let dir = Sys.argv.(2) in
  let _ = doit (sprintf "mkdir -p %s" dir) in
    
  let im = Pnm.load_pgm imname in
  let im = add_margin im 0 30 in

  let color, seed, out = find_largest_nonblack_component im in
  let _ = if verbose then Pnm.save_ppm out (sprintf "%s/00.components.ppm" dir) in
  let boundary, bpoints = trace_boundary im color seed out in
  let _ = if verbose then Pnm.save_ppm out (sprintf "%s/01.largest.ppm" dir) in
  let dt = Morph.dt boundary in
  let dt = Image.map sqrt dt in
  let _ = if verbose then show_float dt (sprintf "%s/02.dt.pgm" dir) in

  (* compute signed dt by doing dfs on interior and holes *)
  let shock, outshock, sdt = find_interior_and_signed_distance seed boundary color dt in
  let _ = if verbose then show_float sdt (sprintf "%s/03.sdt.pgm" dir) in

  (* compute gradient of signed dt *)
  let dx, dy = get_gradient sdt in
  let _ = if verbose then show_float dx (sprintf "%s/04.dx.pgm" dir) in
  let _ = if verbose then show_float dy (sprintf "%s/05.dy.pgm" dir) in

  (* compute flux of gradient around every point *)
  let flux = compute_flux dx dy in
  let _ = if verbose then show_float flux (sprintf "%s/06.flux.pgm" dir) in
  let outflux = Image.map (fun x -> -.x) flux in

  (* subsample curve *)
  let bcurve = Array.map Geometry.complex_of_point bpoints in
  let sscurve = Curve.subsample_dp bcurve 50 2 in
  let sscurve = Array.map Geometry.point_of_complex sscurve in    
  let nss = Array.length sscurve in

  (* find midpoints of segments *)
  let segpts = Array.init nss 
    begin fun i -> 
      let x1,y1 = sscurve.(i) in
      let x2,y2 = sscurve.((i+1) mod nss) in
	((x1+x2)/2, (y1+y2)/2)
    end
  in

  (* compute inside and outside shock graphs using dijkstra's algorithm *)
  let _ = printf "about to run dijkstra\n%!" in
  let inseeds = get_shock_seeds_in flux in
  let indijk, indist, gfim, in_sgf = get_shock_paths flux segpts inseeds segpts in 
  let _ = if verbose then Pnm.save_ppm gfim (sprintf "%s/07.gfim.ppm" dir) in

  let _ = 
    if verbose then begin
      let _ = printf "about to run dijkstra\n%!" in
      let outseeds = get_shock_seeds_out flux in
      let outdijk, outdist, outgfim, out_sgf = get_shock_paths outflux segpts outseeds segpts in 
      let _ = if verbose then Pnm.save_ppm outgfim (sprintf "%s/08.outgfim.ppm" dir) in

      let bothdijk = Image.mapxy
	begin fun (x,y) color ->
	  if color = (0,0,0) then 
	    I.get outdijk (x,y)
	  else
	    color
	end
	indijk
      in
	Array.iter
	  begin fun p ->
	    Draw.draw_box gfim p 2 (255,0,0);
	    Draw.draw_box indijk p 2 (255,0,0);
	    Draw.draw_box outdijk p 2 (255,0,0);
	    Draw.draw_box bothdijk p 2 (255,0,0);
	  end
	  sscurve;
	Pnm.save_ppm indijk (sprintf "%s/09.indijk.ppm" dir);
	show_float indist (sprintf "%s/10.indist.pgm" dir);
	Pnm.save_ppm outdijk (sprintf "%s/11.outdijk.ppm" dir);
	show_float outdist (sprintf "%s/12.outdist.pgm" dir);
	Pnm.save_ppm bothdijk (sprintf "%s/13.bothdijk.ppm" dir);
	Pnm.save_ppm gfim (sprintf "%s/14.bothgfim.ppm" dir);
    end
  in

  let stree = shock_tree_of_shock_graph in_sgf sscurve segpts in
  let _ = if verbose then draw_shock_tree stree out (sprintf "%s/15.responsible.ppm" dir) in
    
  let _ =
    (* remove vertices with small responsibility *)
    Array.iteri
      begin fun nid node ->
	if node.valid then begin
      	  if node.responsible__ <= 1 then begin
	    absorb_point stree nid;
      	  end
	end
      end
      stree.nodes;
  in
  let _ = if verbose then draw_shock_tree stree out (sprintf "%s/16.trimmed.ppm" dir) in

  let _ = 
    (* delete any vertex that has at most one child and no siblings *)
    Array.iteri
      begin fun nid node ->
	if node.valid &&
	  List.length node.children <= 1 &&
	  node.parent <> None &&
	  List.length stree.nodes.(get node.parent).children <= 1 then begin 
	    absorb_point stree nid;
	  end;
      end
      stree.nodes;
  in
  let stree = prune_shock_tree stree in
  let _ = if verbose then draw_shock_tree stree out (sprintf "%s/17.simplified.ppm" dir) in

  let final_stree = copy_shock_tree stree in
  let finalized = Array.map (fun x -> false) final_stree.nodes in
  let stree_stack = ref [stree] in
  let counter = ref 0 in
  let find_final_id node =
    let id = ref None in 
      Array.iteri
	begin fun fnid fnode ->
	  if fnode.point = node.point then
	    id := Some fnid;
	end
	final_stree.nodes;
      get !id
  in
  let _ = 
    Array.iteri
      begin fun nid node ->
	node.parent <- None;
	node.children <- [];
	node.responsible__ <- 0;
	node.closest__ <- [];
	node.valid <- false;
      end
      final_stree.nodes;

    while !stree_stack <> [] do 
      incr counter;
      let stree = List.hd !stree_stack in
      let this_parent_rootid = stree.scanorder.(Array.length stree.scanorder - 1) in
      let parent_rootid = find_final_id stree.nodes.(this_parent_rootid) in
      let parent_root = final_stree.nodes.(parent_rootid) in
      let _ = 
	printf "point #%d is root of parent!\n%!" parent_rootid;
	finalized.(parent_rootid) <- true;
	parent_root.valid <- true;
	parent_root.responsible__ <- stree.nodes.(this_parent_rootid).responsible__;
	parent_root.closest__ <- stree.nodes.(this_parent_rootid).closest__;
      in
      let sgfs = split_shock_tree stree in
      let strees = 
	List.map
	  (fun sgf -> shock_tree_of_shock_graph sgf stree.curve stree.segpts)
	  sgfs 
      in
      let counter2 = ref 0 in
      let counter3 = ref 0 in
	stree_stack := List.tl !stree_stack;

	printf "root yield of current stree is: ";
	List.iter
	  begin fun id ->
	    printf "%d " id;
	  end
	  parent_root.closest__;
	printf "\n%!";

	List.iter 
	  begin fun sgf ->
	    incr counter2;
	    if verbose then begin
	      let gfim = Image.map (fun x -> (0,0,0)) out in
		draw_shock_graph_2 sgf stree.curve gfim;
		Pnm.save_ppm gfim (sprintf "%s/18.%d.sgf.%d.ppm" dir !counter !counter2);
	    end;
	  end
	  sgfs;

	List.iter
	  begin fun stree ->
	    let rootid = stree.scanorder.(Array.length stree.scanorder - 1) in

	      Array.iteri
		begin fun pid node ->
		  let final_pid = find_final_id node in
		  let final_node = final_stree.nodes.(final_pid) in
		    if not finalized.(final_pid) then begin
		      let x,y = node.point in
			printf "point #%d (%d,%d) valid=%b\n%!"
			  final_pid x y node.valid;

			final_node.valid <- node.valid;
			final_node.responsible__ <- node.responsible__;
			final_node.closest__ <- node.closest__;
			final_node.parent <- 
			  begin match node.parent with 
			      None -> 
				printf "point #%d is root here\n%!" final_pid;
				printf "special edge from point #%d to point #%d\n%!" 
				  final_pid parent_rootid;
				parent_root.children <- final_pid :: parent_root.children;
				finalized.(final_pid) <- true;
				Some parent_rootid

			    | Some qid -> 
				printf "edge from point #%d to point #%d\n%!"
				  final_pid (find_final_id stree.nodes.(qid));
				Some (find_final_id stree.nodes.(qid))
			  end;
			final_node.children <- 
			  List.map
			  (fun qid -> find_final_id stree.nodes.(qid))
			  node.children;
		    end
		end
		stree.nodes;

	      incr counter3;
	      if verbose then begin
		draw_shock_tree stree out (sprintf "%s/19.%d.split.%d.ppm" dir !counter !counter3);
	      end;
	      printf "root yield of stree #%d is: " !counter3;
	      printf "(resp=%d) " stree.nodes.(rootid).responsible__;
	      List.iter
		begin fun id ->
		  printf "%d " id;
		end
		stree.nodes.(rootid).closest__;
	      printf "\n%!";

	      if stree.nodes.(rootid).responsible__ > 2 &&
		Array.length stree.nodes > 2 then		
		  stree_stack := stree :: !stree_stack;

	  end
	  strees;

	if verbose then begin
	  draw_shock_tree final_stree out (sprintf "%s/20.final.%d.ppm" dir !counter);
	end;
    done;


  in
  let stree = final_stree in


  let _ =
    Array.iteri
      begin fun pid node ->
	node.closest__ <- List.sort compare node.closest__;
	printf "point #%d " pid;
	List.iter
	  begin fun id ->
	    printf "%d " id;
	  end
	  node.closest__;
	printf "\n%!";
      end
      stree.nodes;
  in

  (* find contiguous intervals, make right endpoint non-inclusive *)
  let boundary_yields = Array.map
    begin fun node ->
      let ls = node.closest__ in
      let rec fn x = function
	  [] -> assert([] <> []); []
	| hd :: [] -> [ (x,hd + 1) ]
	| hd1 :: (hd2 :: tl) ->
	    if hd2 <= hd1 + 1 then
	      fn x (hd2 :: tl)
	    else
	      (x,hd1 + 1) :: (fn hd2 (hd2 :: tl))
      in
	if ls = [] then
	  []
	else
	  fn (List.hd ls) ls
    end
    stree.nodes
  in

  let boundary_yields = Array.map
    begin fun ls ->
      let wraps (i,j) = ( i=0 || j=nss) in
      let lswrap, lsnowrap = 
	List.filter wraps ls,
	List.filter (fun (i,j) -> not (wraps (i,j))) ls
      in
      let lswrap = 

	assert(List.length lswrap <= 2); 

	if List.length lswrap = 2 then begin 
	  let (ai,aj), (bi,bj) = List.hd lswrap, List.hd (List.tl lswrap) in
	    printf "looking at (%d,%d), (%d,%d), len=%d\n%!" ai aj bi bj nss;
	    if ai=0 && bj = nss then begin
	      [ (bi,aj) ]
	    end
	    else if aj=nss && bi = 0 then begin
	      [ (ai,bj) ]
	    end
	    else begin
	      failwith "shouldn't get here, wrap-around acting weird"
	    end
	end
	else
	  lswrap
      in
	lswrap @ lsnowrap
    end 
    boundary_yields
  in

  (*
    how to use outside skeleton???? we will get some collection of things rooted on 
    the image boundary,
    so we
    could just dfs from each, after having assigned every boundary point to 
    an outside skeleton
    point. this gives us a similarly nested set of intervals. As a first step, we could just 
    make the whole subtree rooted at the image boundary into a constituent
    (or rather, the points
    it is responsible for).
  *)

  let scurves, comps = ref [], ref [] in
  let rectify len = 
    let len = (-1 + nss + len) mod nss in
    let len = float_of_int (len+1) in
      len
  in

    Array.iteri
      begin fun pid node ->
	if node.valid then begin 
	  List.iter
	    begin fun (bg,en) ->
	      let split_pts = ref [] in

		if node.parent <> None then begin
		  printf "[%d,%d]\n%!" bg en;

		  split_pts := bg :: !split_pts;
		  split_pts := en :: !split_pts;
		end
		else begin
		  (* root's endpoints shouldn't be added, they'll just be 0 and n *)
		  printf "ROOT\n%!";
		  assert(bg = 0 && en = nss);
		end;

		List.iter 
		  begin fun qid ->
		    List.iter
		      begin fun (idxa, idxb) ->
			split_pts := idxa :: !split_pts;
			split_pts := idxb :: !split_pts;
		      end
		      boundary_yields.(qid);
		  end
		  node.children;

		let _ =
		  printf "    (%d,%d): " bg en;
		  List.iter (printf "%d ") !split_pts;
		  printf "\n%!";
		in

		(* get rid of split points not inside the interval *)
		let split_pts = 
		  List.filter
		    begin fun x ->
		      (bg <= x && x <= en) ||
			(x <= en && en < bg) ||
			(en < bg && bg <= x)
		    end
		    !split_pts
		in

		(* remove duplicates *)
		let split_pts = List.map (fun x -> x mod nss) split_pts in
		let split_pts = 
		  let rv = ref [] in
		  let seen = mkhash 100 in
		    List.iter 
		      begin fun x ->
			if not (seen >>? x) then begin 
			  rv := x :: !rv;
			  seen << (x, ());
			end
		      end
		      split_pts;
		    !rv
		in

		(* sort them in wrapped-around order *)
		let split_pts = List.map
		  begin fun x ->
		    if en < bg && x <= en then
		      x + nss
		    else
		      x
		  end
		  split_pts
		in
		let split_pts = List.sort compare split_pts in
		let split_pts = List.map (fun x -> x mod nss) split_pts in
		let _ =
		  printf "    (%d,%d): " bg en;
		  List.iter (printf "%d ") split_pts;
		  printf "\n%!";
		in

		let triples = 
		  let triples = ref [] in
		  let split_pts = Array.of_list split_pts in
		  let n = Array.length split_pts in
		    if node.parent = None then begin 
		      for i=0 to n-1 do
			for j=i+1 to i+n do
			  for k=j+1 to i+n do
			    let j,k = j mod n, k mod n in
			      printf "    %d, %d, %d\n" split_pts.(i) split_pts.(j) split_pts.(k);
			      triples := (split_pts.(i), split_pts.(j), split_pts.(k)) ::
				!triples;
			  done 
			done 
		      done;
		    end

		    else begin 
		      for i=0 to n-1 do
			for j=i+1 to n-1 do
			  for k=j+1 to n-1 do
			    printf "    %d, %d, %d\n" split_pts.(i) split_pts.(j) split_pts.(k);
			    triples := (split_pts.(i), split_pts.(j), split_pts.(k)) ::
			      !triples;
			  done 
			done 
		      done;
		    end;

		    
		    !triples
		in

		  List.iter 
		    begin fun (i,j,k) ->
		      printf "    [%d,%d] -> [%d,%d] [%d,%d]\n%!" i k i j j k;
		      scurves := (i,k) :: !scurves;
		      scurves := (i,j) :: !scurves;
		      scurves := (j,k) :: !scurves;

		      comps := (i,j,k) :: !comps;
		    end
		    triples;

	    end
	    boundary_yields.(pid)
	end
      end
      stree.nodes;

    let scurves, comps = !scurves, !comps in

    (* 	      let scurves, comps = rotate_productions nss scurves comps in *)

    let ratio (i,j,k) =
      min ((rectify (j-i)) /. (rectify (k-i)))
	((rectify (k-j)) /. (rectify (k-i)))
    in
      
    let comps = List.sort
      begin fun t1 t2 ->
	compare (ratio t2) (ratio t1)
      end
      comps
    in
    let _ = 
      List.iter
	begin fun (i,j,k) ->
	  printf "[%d,%d] -> [%d,%d] [%d,%d]: %f\n%!"
	    i k i j j k (ratio (i,j,k));
	end
	comps
    in

    let sdf = Sdf.make_restricted_family nss scurves comps in
    let sdf = ensure_reachability sdf in
    let is_leaf = Array.map
      begin fun x ->
	(3 * x.sdata.len < nss)
      end
      sdf.f_symbols
    in
    let ntops = ref 0 in
    let is_top = Array.map 
      begin fun sym -> 
	if sym.startable then 
	  incr ntops; 
	sym.startable 
      end
      sdf.f_symbols 
    in
    let refcounts = Array.map (fun x -> 0) sdf.f_symbols in
    let upcounts = Array.map (fun x -> 0) sdf.f_symbols in
    let deleted = Array.map (fun x -> false) sdf.f_compositions in

    let rec killsid is_top deleted upcounts refcounts ntops sid =
      let dead = ref false in
      let sym = Frozen.get_symbol sdf sid in
	if is_leaf.(sid) then begin
	  printf "We killed (%d,%d), which is a leaf!\n%!"
	    sym.sdata.first sym.sdata.last;
	  dead := true;
	end;

	if is_top.(sid) then begin
	  printf "We killed (%d,%d), which is a top!\n%!"
	    sym.sdata.first sym.sdata.last;
	  decr ntops;
	  is_top.(sid) <- false;
	  if !ntops <= 0 then
	    dead := true;
	end;

	Frozen.iter_decompositions sdf sym
	  begin fun comp ->
	    let dead2 = killcid is_top deleted upcounts refcounts ntops comp.cid in
	      dead := !dead || dead2;
	  end;

	Frozen.iter_left_compositions sdf sym
	  begin fun comp ->
	    let dead2 = killcid is_top deleted upcounts refcounts ntops comp.cid in
	      dead := !dead || dead2;			
	  end;

	Frozen.iter_right_compositions sdf sym
	  begin fun comp ->
	    let dead2 = killcid is_top deleted upcounts refcounts ntops comp.cid in
	      dead := !dead || dead2;			
	  end;

	!dead

    and decr_rhs is_top deleted upcounts refcounts ntops sid =
      let dead = ref false in
	refcounts.(sid) <- refcounts.(sid) - 1;
	(* 		    assert( refcounts.(sid) >= 0); *)

	if refcounts.(sid) <= 0 then begin
	  let dead2 = killsid is_top deleted upcounts refcounts ntops sid in
	    dead := !dead || dead2;
	end;
	!dead

    and decr_lhs is_top deleted upcounts refcounts ntops sid =
      let dead = ref false in
	upcounts.(sid) <- upcounts.(sid) - 1;
	(* 		    assert(upcounts.(sid) >= 0); *)
	
	if upcounts.(sid) <= 0 then begin 
	  let dead2 = killsid is_top deleted upcounts refcounts ntops sid in
	    dead := !dead || dead2;
	end;
	!dead

    and killcid is_top deleted upcounts refcounts ntops cid =
      if not deleted.(cid) then begin
	deleted.(cid) <- true;
	let dcomp = Frozen.get_composition sdf cid in
	let ldead = decr_rhs is_top deleted upcounts refcounts ntops dcomp.leftsid in
	let rdead = decr_rhs is_top deleted upcounts refcounts ntops dcomp.rightsid in
	let tdead = decr_lhs is_top deleted upcounts refcounts ntops dcomp.topsid in
	  ldead || rdead || tdead
      end
      else
	false
    in

    let sorted_comps = Array.copy sdf.f_compositions in

      Array.sort 
	begin fun comp1 comp2 ->
	  let t1 = comp1.cdata.bg, comp1.cdata.md, comp1.cdata.en in
	  let t2 = comp2.cdata.bg, comp2.cdata.md, comp2.cdata.en in
	    compare (ratio t1) (ratio t2)
	end
	sorted_comps;

      
      iter_all_compositions sdf
	begin fun comp ->
	  is_leaf.(comp.topsid) <- false;
	  refcounts.(comp.leftsid) <- 1 + refcounts.(comp.leftsid);
	  refcounts.(comp.rightsid) <- 1 + refcounts.(comp.rightsid);
	  upcounts.(comp.topsid) <- 1 + upcounts.(comp.topsid);
	end;
      Frozen.iter_symbols sdf
	begin fun sym ->
	  printf "(%d,%d) leaf? %b\n%!" sym.sdata.first sym.sdata.last
	    is_leaf.(sym.sid);
	  printf "(%d,%d) top? %b\n%!" sym.sdata.first sym.sdata.last
	    is_top.(sym.sid);
	end;

      Array.iter
	begin fun comp ->
	  if not deleted.(comp.cid) then begin
	    let dead =  
	      killcid
		(Array.copy is_top)
		(Array.copy deleted)
		(Array.copy upcounts)
		(Array.copy refcounts) 
		(ref !ntops)
		comp.cid
	    in

	      if not dead then begin
		printf "We can safely eliminate [%d,%d] -> [%d,%d] [%d,%d]\n%!"
		  comp.cdata.bg comp.cdata.en
		  comp.cdata.bg comp.cdata.md
		  comp.cdata.md comp.cdata.en;
		ignore (killcid is_top deleted upcounts refcounts ntops comp.cid);
	      end
	      else begin
		printf "We are preserving [%d,%d] -> [%d,%d] [%d,%d]\n%!"
		  comp.cdata.bg comp.cdata.en
		  comp.cdata.bg comp.cdata.md
		  comp.cdata.md comp.cdata.en;
	      end;
	  end;


	  Array.iteri
	    begin fun sid rc ->
	      let sym = Frozen.get_symbol sdf sid in
		printf "refcount [%d,%d]: %d (leaf=%b, top=%b)\n%!"
		  sym.sdata.first sym.sdata.last
		  rc is_leaf.(sid) is_top.(sid);
		
	    end
	    refcounts;
	  

	  printf "deleted.(%d) = %b\n%!"
	    comp.cid
	    deleted.(comp.cid);
	  printf "\n%!";
	end
	sorted_comps;

      let sdf = filter_compositions sdf
	(fun comp -> not deleted.(comp.cid))
      in
	iter_all_compositions sdf
	  begin fun comp ->
	    printf "We have [%d,%d] -> [%d,%d] [%d,%d]\n%!"
	      comp.cdata.bg comp.cdata.en
	      comp.cdata.bg comp.cdata.md
	      comp.cdata.md comp.cdata.en;
	  end;


	(* ************************************ *)
	(* 	      let comps =  *)
	(* 		List.sort *)
	(* 		  begin fun (i1,j1,k1) (i2,j2,k2) ->  *)
	(* 		    compare (rectify (k2-i2)) (rectify (k1-i1)) *)
	(* 		  end *)
	(* 		  comps *)
	(* 	      in *)
	(* 	      let comps =  *)
	(* 		let a = Array.of_list comps in *)
	(* 		let len = min (Array.length a) maxcomps in *)
	(* 		let a = Array.sub a 0 len in *)
	(* 		  Array.to_list a *)
	(* 	      in *)

	(* 		    let sdf = ensure_reachability sdf in *)
	(* 		    let sdf = ensure_realizability sdf (fun sym -> is_leaf.(sym.sid)) in *)

	let curve = Array.map Geometry.complex_of_point sscurve in

	  printf "finished making family\n%!";
	  Curve.save (sprintf "%s/boundary.curve" dir) curve;
	  Sdf.save_family sdf (sprintf "%s/shock.sdf" dir);
	  doit
	    (sprintf "./show_sdf.native -sdf %s/shock.sdf -curve %s/boundary.curve -fname %s/sdf.svg"
	       dir dir dir);

	  Pnm.save_ppm out (sprintf "%s/out.ppm" dir)

		
