open Printf
open Util.Misc
open Util.Hops
open Graph
module I = Image
module Q = PriorityQueue
open Abstract
open Sdf

type shock_graph = {
  gf: (int*int, int list) Graph.graph;
  degrees: int array;
  is_branch: bool array;
  is_end: bool array;
  responsible: int array; (* how many curve segments belong to this point? *)
  closest: int list array; (* what are the segpts belonging to this point? *)
}

type nid = int

type shock_tree_node = {
  nid: nid;
  point: int * int;
  mutable valid: bool;
  mutable parent: nid option;
  mutable children: nid list;
  mutable responsible__: int;
  mutable closest__: int list;
  
}

type shock_tree = {
  curve: (int *int) array; (* points of the curve *)
  segpts: (int *int) array; (* midpoint of each segment of the curve *)
(*   points: (int * int) array; (\* points of the shock tree *\) *)
(*   is_valid: bool array; *)
(*   parents: int option array; *)
(*   children: int list array; *)
(*   responsible_: int array; *)
(*   closest_: int list array; *)
  nodes: shock_tree_node array;
  scanorder: int array;
}

let copy_shock_node node = {
  nid = node.nid;
  point = node.point;
  valid = node.valid;
  parent = node.parent;
  children = node.children;
  responsible__ = node.responsible__;
  closest__ = node.closest__;
}

let copy_shock_tree stree = {
  curve = Array.copy stree.curve;
  segpts = Array.copy stree.curve;
  nodes = Array.map copy_shock_node stree.nodes;
(*   points = Array.copy stree.points; *)
(*   is_valid = Array.copy stree.is_valid; *)
(*   parents = Array.copy stree.parents; *)
(*   children = Array.copy stree.children; *)
(*   responsible_ = Array.copy stree.responsible_; *)
(*   closest_ = Array.copy stree.closest_; *)
  scanorder = Array.copy stree.scanorder;
}

let draw_shock_graph gf gfim =
  let colors = [| (255,128,0); (0,255,0); (255,255,255);
		  (255,0,0); (255,255,0);
		  (0,0,255); (255,0,255);
		  (255,0,255);
		  (255,0,255);
		  (255,0,255);
		  (255,0,255);
		  (255,0,255);
		  (255,0,255);
	       |] in
    Array.iteri
      begin fun id v -> 
	let nnbrs = List.length (gf.nbrs >>> id) in
	  I.set gfim colors.(nnbrs) v;
      end
      gf.vertices;
    ()

let draw_shock_graph_2 sgf curve gfim =
  let ncurve = Array.length curve in
  let gf = sgf.gf in
  let colors = [| (255,128,0); (0,255,0); (255,255,255);
		  (255,0,0); (255,255,0);
		  (0,0,255); (255,0,255);
	       |] in
    Array.iteri
      begin fun id ls ->
	let color = (50 + Random.int 206, 50 + Random.int 206, 50 + Random.int 206) in
	  List.iter 
	    begin fun cptid ->
	      Draw.draw_line gfim curve.(cptid) curve.((cptid+1) mod ncurve) color;
	    end
	    ls;
      end
      sgf.closest;

    Array.iteri
      begin fun id v -> 
	List.iter
	  begin fun nid ->
	    Draw.draw_line gfim v gf.vertices.(nid) (255,255,255);
	  end
	  (gf.nbrs >>> id)
      end
      gf.vertices;
    Array.iteri
      begin fun id v -> 
	let nnbrs = List.length (gf.nbrs >>> id) in
	  Draw.draw_box gfim v 2 colors.(nnbrs);
      end
      gf.vertices;
    ()

let draw_shock_tree stree out fname = 
  let ncurve = Array.length stree.curve in
  let responsible_im = Image.map (fun x -> (0,0,0)) out in
    Array.iter
      begin fun pid ->
	let node = stree.nodes.(pid) in
	if node.valid then begin 
	  let color = (50 + Random.int 206, 50 + Random.int 206, 50 + Random.int 206) in

	    if node.parent = None then begin 
	      Draw.draw_box responsible_im node.point 4 color;
	    end
	    else begin 
	      Draw.draw_box responsible_im node.point 2 color;
	    end;

	    List.iter
	      begin fun qi -> 
		List.iter
		  begin fun r ->
		    if I.get responsible_im r = (0,0,0) then begin 
		      I.set responsible_im color r;
		    end
		  end
		  (Draw.line stree.curve.(qi) stree.curve.((qi+1) mod ncurve))
	      end
	      node.closest__;
	end;
      end
      stree.scanorder;

    Array.iter
      begin fun node ->
	if node.valid && node.parent <> None then begin 
	  Draw.draw_line responsible_im 
	    node.point stree.nodes.(get node.parent).point (255,255,255);
	end;
      end
      stree.nodes;
    Pnm.save_ppm responsible_im fname

let absorb_point stree nid =
  let node = stree.nodes.(nid) in
  assert(node.parent <> None);
  let parent = stree.nodes.(get node.parent) in

    parent.children <- 
      (List.filter (fun qid -> qid <> nid) parent.children) @
      node.children;

    List.iter
      begin fun qid -> 
	stree.nodes.(qid).parent <- Some parent.nid;
      end
      node.children;

    node.valid <- false;
    node.children <- [];
    node.parent <- None

(* remove invalid points *)
let prune_shock_tree stree =
  let nextid = ref 0 in
  let newold = mkhash 100 in
  let oldnew = mkhash 100 in
  let _ = Array.iteri
    begin fun oldid node ->
      if node.valid then begin 
	newold << (!nextid, oldid);
	oldnew << (oldid, !nextid);
	incr nextid;
      end
    end
    stree.nodes;
  in
  let newscanorder = Array.init !nextid (fun i -> (-1)) in
  let finger = ref 0 in
    Array.iteri
      begin fun i nid ->
	(* 	printf "%d %d\n%!" i id; *)
	let node = stree.nodes.(nid) in
	  if node.valid then begin 
	    newscanorder.(!finger) <- oldnew >> nid;
	    incr finger;
	  end
      end
      stree.scanorder;
    
    let stree =   {
      curve = stree.curve;
      segpts = stree.segpts;
      nodes = Array.init !nextid 
	begin fun id ->
	  let oldnode = stree.nodes.(newold >> id) in
	    {nid = id;
	     point = oldnode.point;
	     valid = true;
	     children = List.map (fun chid -> oldnew >> chid) oldnode.children;
	     parent = begin match oldnode.parent with None -> None
	       | Some pid -> Some (oldnew >> pid) end;
	     responsible__ = oldnode.responsible__;
	     closest__ = oldnode.closest__;
	    }
	end;
      scanorder = newscanorder;
    }
    in
      stree
  
let shock_tree_of_shock_graph sgf curve segpts = 
  let degrees = Array.map (fun x -> x) sgf.degrees in
  let finished = Array.map (fun x -> false) sgf.degrees in
  let theq = Q.make (fun x y -> x <= y) in
  let num_branch = ref 0 in
  let time = ref 0 in
  let npts = Array.length sgf.degrees in
  let stree = 
    {curve = curve;
     segpts = segpts;
     nodes = Array.init npts 
	begin fun id -> {
	  nid = id;
	  point = sgf.gf.vertices.(id);
	  valid = false;
	  children = [];
	  parent = None;
	  responsible__ = sgf.responsible.(id);
	  closest__ = sgf.closest.(id)
	}
	end;
     scanorder = Array.make npts (-1);
    }
  in
    
    (* queue up all the endpoints, priority will be resp. *)
    Array.iteri
      begin fun pid p ->
	if sgf.is_end.(pid) then begin 
	  Q.add theq (stree.nodes.(pid).responsible__, pid);
	end;
	if sgf.is_branch.(pid) then begin 
	  incr num_branch;
	end
      end
      sgf.gf.vertices;

    while not (Q.is_empty theq) do 
      let (resp, pid) = Q.pop theq in
	(* 	printf "popped point #%d\n%!" pid; *)

	if (Q.is_empty theq && !num_branch = 0) || 
	  (sgf.is_branch.(pid) && !num_branch = 1) then begin 
	    printf "root!\n%!";
	    assert((degrees.(pid) = 0 && Q.is_empty theq) ||
		     degrees.(pid) = 1);	  
	    finished.(pid) <- true;
	    stree.scanorder.(npts-1) <- pid;
	    stree.nodes.(pid).valid <- true;
	  end
	else begin 
	  assert(degrees.(pid) = 1);	  

	  let nbrs = sgf.gf.nbrs >>> pid in
	  let newnbrs = List.filter (fun qid -> not finished.(qid)) nbrs in

	  let qid =
	    if newnbrs = [] then begin 
	      let theroot = stree.scanorder.(npts-1) in
		List.iter (fun x -> printf "#%d  " x) nbrs;
		printf "\n%!";
		assert(theroot >= 0);
		assert(Q.is_empty theq);
		assert(List.mem theroot nbrs);
		assert(degrees.(theroot) = 1);
		theroot
	    end
	    else begin
	      assert(List.length newnbrs = 1);
	      List.hd newnbrs 
	    end
	  in
	    (* remove p, give its points to q, decrement q's
	       degree *)
	  let pnode, qnode = stree.nodes.(pid), stree.nodes.(qid) in

	    finished.(pid) <- true;
	    degrees.(qid) <- degrees.(qid) - 1;
	    stree.scanorder.(!time) <- pid;
	    pnode.parent <- Some qid;
	    qnode.responsible__ <- qnode.responsible__ + pnode.responsible__;
	    qnode.children <- pid :: qnode.children;
	    qnode.closest__ <- qnode.closest__ @ pnode.closest__;
	    pnode.valid <- sgf.is_branch.(pid) || sgf.is_end.(pid);

	    if degrees.(qid) = 1 then begin
	      Q.add theq (qnode.responsible__, qid);
	    end;

	    if sgf.is_branch.(pid) then begin 
	      decr num_branch;
	    end;

	    incr time;
	end
    done;

    Array.iteri
      begin fun nid node ->
	assert(finished.(nid));

	if not node.valid then begin
	  assert(node.parent <> None);
	  absorb_point stree nid;
	end;
	
      end
      stree.nodes;

    prune_shock_tree stree


(* split off most responsible child of the root from the shock tree *)
let split_shock_tree stree = 
  let npts = Array.length stree.nodes in
  let rootid = stree.scanorder.(npts-1) in

  let add_vx gf oldnew id =
    let vid = add_vertex gf stree.nodes.(id).point in
      oldnew << (id,vid);
  in

  let rec visit gf oldnew id =
    add_vx gf oldnew id;
    List.iter
      begin fun chid ->
	add_vx gf oldnew chid;
	add_edge_strict gf stree.nodes.(id).point stree.nodes.(chid).point []; 
	visit gf oldnew chid;
      end
      stree.nodes.(id).children;
  in
  let newthings = 
    List.map
      begin fun chid ->
	let gf = new_live_graph () in
	let oldnew = mkhash 100 in
	  add_vx gf oldnew rootid;
	  add_vx gf oldnew chid;
	  add_edge_strict gf stree.nodes.(rootid).point stree.nodes.(chid).point [];
	  visit gf oldnew chid;

	  gf, oldnew
      end
      stree.nodes.(rootid).children
  in

    Array.iteri
      begin fun pid node ->
	let p = node.point in
	let found = List.map (fun (gf, oldnew) -> oldnew >>? pid) newthings in
	  try
	    assert(List.mem true found);
	  with Assert_failure s ->
	    let x,y = p in
	      printf "point #%d (%d,%d) never found!\n%!" pid x y;
	      assert(List.mem true found);
      end
      stree.nodes;

    let newthings = List.map
      begin fun (gf, oldnew) ->
	let fgf = Graph.finalize gf in
	let degrees = Array.mapi (fun i v -> List.length (fgf.nbrs >>> i)) fgf.vertices in
	let responsible = Array.map (fun x -> 0) fgf.vertices in
	let closest = Array.map (fun x -> []) fgf.vertices in

	  {gf = fgf;
	   degrees = degrees;
	   is_branch = Array.map (fun i -> i > 2) degrees;
	   is_end = Array.map (fun i -> i = 1) degrees;
	   responsible = responsible;
	   closest = closest;
	  },
	oldnew
      end
      newthings
    in

      Array.iteri
	begin fun segi segpt ->
	  let theidx = ref None in
	    Array.iteri
	      begin fun i id ->
		if !theidx = None && List.mem segi stree.nodes.(id).closest__ then begin
		  theidx := Some i;
		end
	      end
	      stree.scanorder;

	    if !theidx <> None then begin
	      
	      let sgf, oldnew = 
		List.find
		  begin fun (sgf, oldnew) ->
		    oldnew >>? stree.scanorder.(get !theidx)
		  end
		  newthings
	      in
	      let newid = oldnew >> stree.scanorder.(get !theidx) in
		
		sgf.responsible.(newid) <- 1 + sgf.responsible.(newid);
		sgf.closest.(newid) <- segi :: sgf.closest.(newid);
	    end;
	end
	stree.segpts;

      List.map (fun (sgf,oldnew) -> sgf) newthings
      


let show_float im fname = 
  let minval, maxval = ref infinity, ref neg_infinity in
  let w,h = Image.width im, Image.height im in
    for y=0 to h-1 do
      for x=0 to w-1 do
	minval := min !minval (I.get im (x,y));
	maxval := max !maxval (I.get im (x,y));
      done
    done;
    printf "%s: min=%f\n" fname !minval;
    printf "%s: max=%f\n" fname !maxval;
    
    let im = Image.map (fun x -> (x -. !minval) /. (!maxval -. !minval)) im in
    let im = Image.map (fun x -> round (255. *. x)) im in
      Pnm.save_pgm im fname

let add_margin im vl size = 
  let im = Image.init ((Image.width im)+2*size) ((Image.height im)+2*size) 
    begin fun (x,y) ->
      if I.inside im (x-size,y-size) then
	I.get im (x-size,y-size)
      else
	vl
    end
  in
    im

let smallcycles8 (x,y) = [
  (x+1,y), (x+1,y+1), (x,y+1);
  (x-1,y), (x-1,y+1), (x,y+1);
  (x-1,y), (x-1,y-1), (x,y-1);
  (x+1,y), (x+1,y-1), (x,y-1);
]


let edges8 (x,y) = [
  (x+1, y),   (x+1, y+1);
  (x+1, y+1), (x,   y+1);
  (x,   y+1), (x-1, y+1);
  (x-1, y+1), (x-1, y);
  (x-1, y),   (x-1, y-1);
  (x-1, y-1), (x,   y-1);
  (x,   y-1), (x+1, y-1);
  (x+1, y-1), (x+1, y);

  (x+1,y), (x,y+1);
  (x-1,y), (x,y+1);
  (x-1,y), (x,y-1);
  (x+1,y), (x,y-1);
]


let edges8_4 (x,y) = [
  (x+1, y),   (x+1, y+1);
  (x+1, y+1), (x,   y+1);
  (x,   y+1), (x-1, y+1);
  (x-1, y+1), (x-1, y);
  (x-1, y),   (x-1, y-1);
  (x-1, y-1), (x,   y-1);
  (x,   y-1), (x+1, y-1);
  (x+1, y-1), (x+1, y);
]


let nbrs8 (x,y) = (* in ccw order *)
  [ x+1,y; x+1,y+1; x,y+1; x-1,y+1; x-1,y; x-1,y-1; x,y-1; x+1,y-1 ]

let nbrs4 (x,y) =
  [ x+1,y; x,y+1; x-1,y; x,y-1; ]


let find_nonblack_component p im color out = 
  let stack = ref [p] in
  let count = ref 0 in

    while !stack <> [] do 
      let q = List.hd !stack in
	stack := List.tl !stack;
	incr count;

	I.set color 1 q;
	I.set out (255,255,255) q;

	List.iter 
	  begin fun r ->
	    if I.inside im r && I.get im r = 0 then begin
	      I.set color 2 q;
	      I.set out (255,255,0) q;
	    end
	  end
	  (nbrs4 q);

	List.iter
	  begin fun r ->
	    if I.inside im r && 
	      I.get im r != 0 && 
	      I.get color r = 0 then begin
		stack := r :: !stack;
	      end
	  end
	  (nbrs4 q);

    done;

    !count


let find_largest_nonblack_component im =
  let w,h = Image.width im, Image.height im in
  let out = Image.map (fun x -> (0,0,0)) im in
  let color = Image.map (fun x -> 0) im in 
    (* colors: 

       0 unvisited+white or black
       1 visited, white, not edge
       2 visited, white, edge
    *)
  let largest = ref 0 in
  let bestseed = ref None in

    (* find largest component *)
    for y=0 to h-1 do
      for x=0 to w-1 do 
	if (I.get im (x,y) != 0) && (I.get color (x,y) = 0) then begin  
	  let num = find_nonblack_component (x,y) im color out in
	    printf "%d pixels\n%!" num;
	    if num > !largest then begin
	      largest := num;
	      bestseed := Some (x,y);
	    end
	end
      done 
    done;

    let seed = get !bestseed in

      color, seed, out

let trace_boundary im color seed out = 
  let rec walk head cur prev boundary =
    let nbrs = nbrs8 cur in
    let nbrs = List.filter 
      (fun (x,y) -> 
	 I.inside color (x,y) && 
	   I.get color (x,y) = 2) nbrs in

      I.set boundary true cur;
      I.set out (0,255,0) cur;

      (* first time, head=cur=prev, pick any neighbor *)
      if head = cur && cur = prev then begin
	walk head (List.hd nbrs) cur boundary
      end

      else begin 
	(* finished going around the boundary *)
	if head = cur then begin
	  [cur]
	end

	else begin
	  (* rotate list until we see prev, and remove prev *)
	  let nbrs = 
	    let front, back = ref [], ref nbrs in
	      while (List.hd !back) <> prev do 
		front := !front @ [List.hd !back];
		back := List.tl !back; 
	      done;

	      (List.tl !back) @ !front 
	  in

	    if nbrs = [] then begin
	      (* we're on a peninsula, go back *)
	      cur :: (walk head prev cur boundary)
	    end
	    else begin
	      (* go to point that comes first going ccw from prev *)
	      cur :: (walk head (List.hd nbrs) cur boundary)
	    end
	end
      end
  in
  let boundary = Image.map (fun x -> false) im in 
  let bpoints = walk seed seed seed boundary in
    boundary, Array.of_list bpoints

let find_interior_and_signed_distance p boundary color dt =  
  let sdt = Image.map (fun x -> x) dt in
  let shock = Image.map (fun x -> false) dt in
  let stack = ref [p] in

    (*   let rec helper p =  *)
    while !stack <> [] do
      let p = List.hd !stack in
	stack := List.tl !stack;
	if not (I.get shock p) then begin
	  let nbrs = nbrs4 p in
	  let nbrs = List.filter
	    begin fun q -> I.inside boundary q && (
	      (* if we're a boundary point, don't go to neighboring
		 background points *)
	      ((I.get boundary p) && (I.get color q != 0)) ||
		(* if we're not a boundary point, OK to go to
		   neighboring background points, they will be holes *)
		(not (I.get boundary p)))
	    end
	    nbrs 
	  in

	    I.set shock true p;
	    I.set sdt (-. (I.get sdt p)) p;
	    List.iter 
	      begin fun q ->
		if not (I.get shock q) then begin
		  (* 	      helper q; *)
		  stack := q :: !stack;
		end
	      end
	      nbrs
	      (*   in *)
	      (*     helper p; *)
	end
    done;

    let outshock = Image.map (fun x -> x >= 0.) sdt in

      shock, outshock, sdt

let get_gradient dt = 
  let w,h = Image.width dt, Image.height dt in
  let dx, dy = Image.map (fun x -> 0.0) dt, Image.map (fun x -> 0.0) dt in
    for y=0 to h-1 do 
      for x=0 to w-1 do 
	(* if computing the gradient would take us out of the image,
	   copy from next location over (want smoothness at
	   boundaries) *)
	if y = 0 then
	  I.set dy ((I.get dt (x,y+2)) -. (I.get dt (x,y))) (x,y)
	else if y = h-1 then
	  I.set dy ((I.get dt (x,y)) -. (I.get dt (x,y-2))) (x,y)
	else
	  I.set dy ((I.get dt (x,y+1)) -. (I.get dt (x,y-1))) (x,y);

	if x = 0 then
	  I.set dx ((I.get dt (x+2,y)) -. (I.get dt (x,y))) (x,y)
	else if x = w-1 then
	  I.set dx ((I.get dt (x,y)) -. (I.get dt (x-2,y))) (x,y)
	else
	  I.set dx ((I.get dt (x+1,y)) -. (I.get dt (x-1,y))) (x,y);

      done
    done;

    dx, dy

let compute_flux dx dy = 
  let w,h = Image.width dx, Image.height dx in
  let rt1_2 = sqrt 0.5 in
  let units = [| 
    (   1.,       0.);
    (   rt1_2,    rt1_2);
    (   0.,       1.);
    (-. rt1_2,    rt1_2);
    (-. 1.,       0.);
    (-. rt1_2, -. rt1_2);
    (   0.,    -. 1.);
    (   rt1_2, -. rt1_2);
  |] in
  let dot (a,b) (c,d) = a *. c +. b *. d in
  let flux = Image.map (fun x -> 0.) dx in
    
    for y=1 to h-2 do 
      for x=1 to w-2 do 
	let tot = ref 0. in
	let nbrs = Array.of_list (nbrs8 (x,y)) in
	let grads = Array.map (fun q -> (I.get dx q, I.get dy q)) nbrs in
	  Array.iteri
	    begin fun i u ->
	      tot := !tot -. (dot u grads.(i));
	    end
	    units;
	  I.set flux !tot (x,y);
      done
    done;

    flux

let get_shock_points bpoints shock flux thresh out = 
  let w,h = Image.width shock, Image.height shock in

  let check p =
    (not (I.inside shock p)) || I.get shock p
  in

  let euler p =
    let count = ref 0 in
      (* +1 per vertex *)
      List.iter
	begin fun q ->
	  if check q then
	    incr count
	end
	(nbrs8 p);
      (* -1 per edge *)
      List.iter
	begin fun (q,r) -> 
	  if check q && check r then
	    decr count
	end
	(edges8 p);
      (* +1 per short cycle *)
      List.iter
	begin fun (q,r,s) ->
	  if check q && check r && check s then 
	    incr count
	end
	(smallcycles8 p);
      !count
  in
    
  let removable p = 
    (euler p) = 1
  in

  let endpoint p = 
    let nbrs = nbrs8 p in
    let nbrs = List.filter check nbrs in
      assert(nbrs <> []);

      if List.length nbrs = 1 then 
	true
      else if List.length nbrs > 2 then
	false
      else
	let q,r = List.hd nbrs, List.hd (List.tl nbrs) in
	  List.mem q (nbrs4 r)
  in

  (* we can assume that p is not removable. *)
  let branch p =
    let count = ref 0 in
      (* +1 per vertex *)
      List.iter
	begin fun q ->
	  if check q then
	    incr count
	end
	(nbrs8 p);
      (* -1 per edge, only count 4-edges *)
      List.iter
	begin fun (q,r) -> 
	  if check q && check r then
	    decr count
	end
	(edges8_4 p);

      !count = 0 || !count > 2
  in

  let theq = Q.make (fun x y -> x >= y) in
  let endpts = ref [] in 
  let branchpts = ref [] in
  let shockpts = ref [] in

    Array.iter 
      begin fun p ->
	I.set out (0,255,0) p;
	Q.add theq (I.get flux p, p)
      end
      bpoints;
    
    (* while not done: dequeue a point, run checks, maintain an
       interior image and a list of endpoints *)
    while not (Q.is_empty theq) do
      let pflux, p = Q.pop theq in
(*       let (x,y) = p in *)
	if (I.get shock p) && (removable p) then begin
(* 	  printf "Popped (%d,%d), flux=%f\n%!" x y pflux; *)
	  if (not (endpoint p)) || (I.get flux p > thresh) then begin
	    I.set shock false p; (* remove p *)
	    I.set out (255,0,0) p;
	    List.iter
	      begin fun q ->
		if (I.inside shock q) && (I.get shock q) && (removable q) then begin 
		  I.set out (0,255,0) q;
		  Q.add theq (I.get flux q, q);
		end
	      end
	      (nbrs8 p);
	  end
	  else begin
	    printf "New endpoint!\n%!";
	    endpts := p :: !endpts;
	    I.set out (0,0,255) p;
	  end
	end
    done;


    for y=0 to h-1 do
      for x=0 to w-1 do 
	if I.get shock (x,y) then begin 
	  printf "%d,%d is in shock graph\n%!" x y;
	  shockpts := (x,y) :: !shockpts;

	  if branch (x,y) then begin 
	    I.set out (0,0,255) (x,y);
	    branchpts := (x,y) :: !branchpts;
	  end

	end;
      done
    done;


    let shockpts = Array.of_list !shockpts in
    let is_end = Array.map (fun p -> List.mem p !endpts) shockpts in
    let is_branch = Array.map (fun p -> List.mem p !branchpts ) shockpts in

      shockpts, is_end, is_branch

let assign_boundary_to_shock_single p (dx,dy) shock shockpts = 
  let x,y = p in
  let _ = printf "start: x=%d, y=%d\n" x y in
  let besti = ref None in
  let dx,dy = I.get dx (x,y), I.get dy (x,y) in
  let x,y = ref (float_of_int x), ref (float_of_int y) in
  let thept = ref None in
  let notdone = ref true in
  let speed = 0.5 in


    if abs_float dx >= 0.01 || abs_float dy >= 0.01 then begin

      while !notdone do
	x := !x -. speed *. dx; (* (I.get dx (round !x, round !y)); *)
	y := !y -. speed *. dy; (* (I.get dy (round !x, round !y)); *)
	(* 	    printf "x=%f, y=%f\n" !x !y; *)
	if not (I.inside shock (round !x, round !y)) then begin 
	  notdone := false;
	end
	else begin

	  let x,y = round !x, round !y in
	    List.iter
	      begin fun (x,y) ->
		if I.inside shock (x,y) && I.get shock (x,y) then begin
		  notdone := false;
		  thept := Some (x,y);
		  printf "found: %d,%d\n" x y;
		end
	      end
	      ((x,y) :: (nbrs4 (x,y)));
	end
      done;

      if !thept <> None then
	besti := Some (find (get !thept) shockpts);

    end;
    
    if !besti = None then begin
      let mindist2 = ref 1000000000 in
	Array.iteri
	  begin fun i s ->
	    let thedist2 = dist2 p s in
	      if thedist2 < !mindist2 then begin
		mindist2 := thedist2;
		besti := Some i;
	      end
	  end
	  shockpts;
    end;
    get !besti

let assign_boundary_to_shock_new shock shockpts sscurve bpoints (dx,dy) =
  let closest = Array.map (fun p -> []) shockpts in
  let responsible = Array.map (fun p -> 0) shockpts in

  let bpoints = 
    let npoints = Array.length bpoints in
    let finger = ref 0 in
      printf "finger=%d\n%!" !finger;
      let (x,y), (x2,y2) = bpoints.(!finger), sscurve.(0) in
	printf "(%d,%d) (%d,%d)\n%!" x y x2 y2;
	while bpoints.(!finger) <> sscurve.(0) do 
	  printf "finger=%d\n%!" !finger;
	  let (x,y), (x2,y2) = bpoints.(!finger), sscurve.(0) in
	    printf "(%d,%d) (%d,%d)\n%!" x y x2 y2;
	    incr finger;
	done;

	let newbpoints = Array.append (Array.sub bpoints !finger (npoints - !finger))
	  (Array.sub bpoints (npoints - !finger) !finger) in
	  newbpoints
  in

  let sfinger = ref 0 in
  let votes = Array.map (fun x -> Array.map (fun x -> 0) shockpts) sscurve in

    Array.iteri
      begin fun i p ->
	let besti = assign_boundary_to_shock_single p (dx,dy) shock shockpts in
	  votes.(!sfinger).(besti) <- 1 + votes.(!sfinger).(besti);

	  if !sfinger+1 < Array.length sscurve && 
	    i+1 < Array.length bpoints &&
	    bpoints.(i+1) = sscurve.(!sfinger+1) then begin 
	    incr sfinger;
	  end;
      end
      bpoints;

    Array.iteri
      begin fun ssid ssp ->
	let best, besti, tot = ref (-1), ref 0, ref 0 in
	  Array.iteri 
	    begin fun i nvotes ->
	      tot := !tot + nvotes;
	      if nvotes > !best then begin 
		best := nvotes;
		besti := i;
	      end
	    end
	    votes.(ssid);
	  
	  closest.(!besti) <- ssp :: closest.(!besti);
	  responsible.(!besti) <- !tot + responsible.(!besti);

      end
      sscurve;

    closest, responsible


let assign_boundary_to_shock shock shockpts bpoints (dx,dy) =
  let closest = Array.map (fun p -> []) shockpts in
  let responsible = Array.map (fun p -> 0) shockpts in

    Array.iter
      begin fun p ->
	let x,y = p in
	let besti = ref None in
	  printf "start: x=%d, y=%d\n" x y;
	  let dx,dy = I.get dx (x,y), I.get dy (x,y) in
	  let x,y = ref (float_of_int x), ref (float_of_int y) in
	  let thept = ref None in
	  let notdone = ref true in
	  let speed = 0.5 in
	    if abs_float dx >= 0.01 || abs_float dy >= 0.01 then begin

	      while !notdone do
		x := !x -. speed *. dx; (* (I.get dx (round !x, round !y)); *)
		y := !y -. speed *. dy; (* (I.get dy (round !x, round !y)); *)
		(* 	    printf "x=%f, y=%f\n" !x !y; *)
		if not (I.inside shock (round !x, round !y)) then begin 
		  notdone := false;
		end
		else begin

		  let x,y = round !x, round !y in
		    List.iter
		      begin fun (x,y) ->
			if I.inside shock (x,y) && I.get shock (x,y) then begin
			  notdone := false;
			  thept := Some (x,y);
			  printf "found: %d,%d\n" x y;
			end
		      end
		      ((x,y) :: (nbrs4 (x,y)));
		end
	      done;

	      if !thept <> None then
		besti := Some (find (get !thept) shockpts);

	    end;
	    
	    if !besti = None then begin
	      let mindist2 = ref 1000000000 in
		Array.iteri
		  begin fun i s ->
		    let thedist2 = dist2 p s in
		      if thedist2 < !mindist2 then begin
			mindist2 := thedist2;
			besti := Some i;
		      end
		  end
		  shockpts;
	    end;

	    let besti = get !besti in
	      closest.(besti) <- p :: closest.(besti);
	      responsible.(besti) <- 1 + responsible.(besti);

      end
      bpoints;

    closest, responsible

let get_shock_graph shock shockpts gfim = 
  let seen = mkhash 100 in
  let black, grey = 0,1 in
    (* black - never explore this point again 

       grey - let no one else explore this point
    *)

  let rec visit_shock p gf = 
    let nbrs_4, nbrs_8 = nbrs4 p, nbrs8 p in
    let nbrs_4, nbrs_8 =
      List.filter (fun q -> I.inside shock q && I.get shock q) nbrs_4,
      List.filter (fun q -> I.inside shock q && I.get shock q) nbrs_8 in
    let nbrs_4 = List.filter (fun q -> not (seen >>? q)) nbrs_4 in

      seen << (p, black);

      (* Mark 4-neighbors so that they don't connect to each other
	 with 8-edges. *)
      List.iter 
	begin fun q ->
	  seen << (q, grey);
	end
	nbrs_4;

      (* Visit 4-neighbors first, so that we only use
	 8-edges if we need them. *)
      List.iter
	begin fun q ->
	  visit_shock q gf;
	  Graph.add_edge_strict gf p q [];
	end
	nbrs_4;
      
      (* Visit unseen 8-neighbors *)
      let nbrs_8 = List.filter (fun q -> not (seen >>? q)) nbrs_8 in

	(* mark them all first so that they can't connect to each other *)
	List.iter
	  begin fun q ->
	    seen << (q, grey);
	  end
	  nbrs_8;

	List.iter
	  begin fun q ->
	    visit_shock q gf;
	    Graph.add_edge_strict gf p q [];
	  end
	  nbrs_8;

  in
  let gf = new_live_graph () in
    Array.iteri
      begin fun pid p ->
	let id = Graph.add_vertex_strict gf p in
	  assert(id = pid);
      end
      shockpts;

    Array.iter
      begin fun p ->
	if not (seen >>? p) then begin
	  visit_shock p gf;
	end
      end
      shockpts;

    Array.iter
      begin fun p ->
	assert(seen >>? p);
      end
      shockpts;

    let fgf = Graph.finalize gf in
    let degrees = Array.map (fun x -> 0) shockpts in
    let colors = [| (255,128,0); (0,255,0); (255,255,255);
		    (255,0,0); (255,255,0);
		    (0,0,255); (255,0,255);
		 |] in

      (* make shockpts purple so they show up if they're not in the graph *)
      Array.iter
	begin fun p ->
	  I.set gfim (255,0,255) p;
	end
	shockpts;

      Array.iteri
	begin fun id v -> 
	  let nnbrs = List.length (fgf.nbrs >>> id) in
	    I.set gfim colors.(nnbrs) v;
	    degrees.(id) <- nnbrs;
	end
	fgf.vertices;

      draw_shock_graph fgf gfim;

      fgf, gfim, degrees



let get_shock_seeds_in flux = 
  let w,h = Image.width flux, Image.height flux in
  let bestp, bestflux = ref (0,0), ref infinity in
    
    for y=0 to h-1 do 
      for x=0 to w-1 do 
	if I.get flux (x,y) < !bestflux then begin 
	  bestp := (x,y);
	  bestflux := I.get flux (x,y);
	end
      done
    done;

    [| !bestp |]

let get_shock_seeds_out flux = 
  let seeds = ref [] in
  let w,h = Image.width flux, Image.height flux in
    for y=0 to h-1 do 
      let p,q = (0,y), (w-1,y) in
	seeds := p :: !seeds;
	seeds := q :: !seeds;
    done;
    for x=1 to w-2 do 
      let p,q = (x,0), (x,h-1) in
	seeds := p :: !seeds;
	seeds := q :: !seeds;
    done;
    Array.of_list !seeds

let get_shock_paths flux pts seeds segpts =
  let theq = Q.make (fun (kx,vx) (ky,vy) -> kx <= ky) in
  let seen = Image.map (fun x -> false) flux in
  let cost = Image.map 
    begin fun x -> 
      (exp (5. +. x)) 
    end 
    flux 
  in
  let dist = Image.map (fun x -> infinity) flux in
  let parent = Image.map (fun x -> None) flux in
  let valid = Image.map (fun x -> false) flux in
  let mindist, maxdist = ref infinity, ref neg_infinity in
  let gf = Graph.new_live_graph () in
  let ptset = mkhash 100 in
  let npts = ref 0 in

    Array.iter 
      begin fun p ->
	ptset << (p,());
	incr npts;
      end
      pts;

    Array.iter
      begin fun p ->
	Q.add theq (0., p);
	I.set seen true p;
	I.set dist 0. p;
      end
      seeds;
    
    printf "dijkstra main loop\n%!";
    while !npts > 0 && not (Q.is_empty theq) do 
      let pcost, p = Q.pop theq in
      let nbrs = nbrs8 p in
      let nbrs = List.filter (fun q -> I.inside seen q && not (I.get seen q) ) nbrs in

	if ptset >>? p then begin 
	  decr npts;
	end;
	
	I.set seen true p;
	
	List.iter
	  begin fun q ->
	    let qcost = pcost +. I.get cost q in
	      Q.add theq (qcost, q);
	      I.set seen true q;
	      I.set dist qcost q;
	      I.set parent (Some p) q;
	  end 
	  nbrs;
    done;
    printf "/dijkstra main loop\n%!";

    Array.iteri
      begin fun i p ->
	let q = ref (Some p) in
	  while !q <> None && not (I.get valid (get !q)) do 
	    let thisq = get !q in
	      mindist := min (I.get dist thisq) !mindist;
	      maxdist := max (I.get dist thisq) !maxdist;
	      I.set valid true thisq;
	      ignore (Graph.add_vertex gf thisq);
	      q := I.get parent thisq;
	      if !q <> None then begin
		ignore (Graph.add_vertex gf (get !q));
		Graph.add_edge_strict gf thisq (get !q) [];
	      end
	  done;
      end
      pts;

    let out = Image.mapxy
      begin fun p v ->
	if not v then 
	  (0,0,0) 
	else begin 
	  let dist = I.get dist p in
	  let dist = (dist -. !mindist) /. (!maxdist -. !mindist) in
	    assert( 0. <= dist && dist <= 1.);
	    (round (dist *. 255.), round ((1. -. dist) *. 255.), 0)
	end
      end 
      valid
    in

    let fgf = Graph.finalize gf in
    let degrees = Array.mapi (fun id x -> List.length (fgf.nbrs >>> id)) fgf.vertices in
    let is_branch = Array.map (fun i -> i > 2) degrees in
    let is_end = Array.map (fun i -> i = 1) degrees in
    let responsible = Array.map (fun i -> 0) degrees in
    let closest = Array.map (fun i -> []) degrees in
    let gfim = Image.map (fun x -> (0,0,0)) out in

      Array.iteri
	begin fun pi p -> 
	  let i = find p fgf.vertices in
	    responsible.(i) <- 1;
	    closest.(i) <- [pi];
	end
	segpts;

      draw_shock_graph fgf gfim;

      out, Image.map (fun x -> min x 2000.) dist, gfim,
  {gf = fgf;
   degrees = degrees;
   is_branch = is_branch;
   is_end = is_end;
   responsible = responsible;
   closest = closest;
  }


let rotate_productions npoints scurves comps = 
  let sdf = Sdf.make_restricted_family npoints scurves comps in
  let scurves, comps = ref [], ref [] in
    iter_all_decompositions sdf 
      begin fun sym dcomp ->
	let lsym = Frozen.get_symbol sdf dcomp.leftsid in
	let rsym = Frozen.get_symbol sdf dcomp.rightsid in
	  scurves := (sym.sdata.first, sym.sdata.last) :: !scurves;
	  scurves := (lsym.sdata.first, lsym.sdata.last) :: !scurves;
	  scurves := (rsym.sdata.first, rsym.sdata.last) :: !scurves;
	  comps := (dcomp.cdata.bg, dcomp.cdata.md, dcomp.cdata.en) :: 
	    !comps;

	  (*		      
			      [i,k] -> [i,j] [j,k]
			      [i,j] -> [i,h] [h,j]
			      ____________________     
			      [i,k] -> [i,h] [h,k]
	  *)		      
	  Frozen.iter_decompositions sdf lsym
	    begin fun dcomp2 ->
	      scurves := (dcomp2.cdata.md, sym.sdata.last) :: !scurves;
	      comps := (dcomp2.cdata.bg, dcomp2.cdata.md, sym.sdata.last) ::
		!comps;
	    end;

	  (*		      
			      [i,k] -> [i,j] [j,k]
			      [j,k] -> [j,h] [h,k]
			      ____________________     
			      [i,k] -> [i,h] [h,k]
	  *)		      
	  Frozen.iter_decompositions sdf rsym
	    begin fun dcomp2 ->
	      scurves := (sym.sdata.first, dcomp2.cdata.md) :: !scurves;
	      comps := (sym.sdata.first, dcomp2.cdata.md, dcomp2.cdata.en) ::
		!comps;
	    end;
      end;
    !scurves, !comps
