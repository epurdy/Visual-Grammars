open Printf
open Scanf
open Util.Misc
open Util.Hops
open Util.Cops
open Sdf
open Abstract
open Grammar
module Q = PriorityQueue
open Complex
open Geometry

let log = Pervasives.log
let exp = Pervasives.exp

(* need to make subdivide edges finer *)

let nsubsample = 4

let maxp = 0.9
let max_neglog = -. log (0.5)
let maxcurves = 1000
let maxiter = 10000000
let startcost = 1.
let maxd = 3
let minlen = 3
let qthresh = 3.0

type sid = int
type cid = int
type pid = int
type pt = int * int

let spt (x,y) = sprintf "%d,%d" x y
let dist2 (ax,ay) (bx,by) = (ax-bx)*(ax-bx) + (ay-by)*(ay-by)

type state = { (* X_{p,q} -> \alpha_{p,q} * \beta_{q,?} *)
  density: float;
  qual: float;
  start: pt;
  p: pt;
  q: pt; 
  len: int; (* number of line segments parsed *)
}

type ('a,'b,'c) manager = {
  best_density: (pt * pt * pt * int, float) hash;
  good_states: (pt * pt * int, state) hash;
  queues: (state PriorityQueue.t) DynArray.t;
  mutable finger: int;
  nlpb: float Image.t;
  mutable best_cost_density: float;
  mutable best_state: state option;
  w: int;
  h: int;
}

let keep_going manager =
  DynArray.exists 
    begin fun q -> 
      Q.length q >= 1 &&
	manager.best_cost_density > (Q.first q).density
    end
    manager.queues

let print_state manager state =
  let sx, sy = state.start in
  let (px,py), (qx,qy) = state.p, state.q in
    printf "[dens=%f = %f/%d] (%d,%d) -- .. -- (%d,%d) -- (%d,%d)]\n%!"
      state.density state.qual state.len
      sx sy px py qx qy 

let reconstruct manager = 
  let state = ref (get manager.best_state) in
  let start = !state.start in
  let rv = ref [] in
    while !state.len > 1 do
      rv := !state.q :: !rv;
      printf "RECONSTRUCT:  ";
      print_state manager !state;
      state := manager.good_states >> (!state.p, !state.start, !state.len - 1);
    done;
    (start :: !rv, !state.density)

let newqueue () = Q.make (fun s1 s2 -> s1.density <= s2.density)

let create_manager nlpb = {
  w = Image.width nlpb;
  h = Image.height nlpb;
  best_density = mkhash 1000;
  good_states = mkhash 1000;
  queues = DynArray.make 0 (newqueue ());
  finger = 0;
  best_cost_density = infinity;
  best_state = None;
  nlpb = nlpb;
}

let add_state manager state =
  let key = (state.p,state.q,state.start,state.len) in
  let key2 = (state.q,state.start,state.len) in
    if state.density < manager.best_cost_density &&
      state.density < (manager.best_density >>! (key, infinity)) then begin
      manager.best_density << (key, state.density);
      manager.good_states << (key2, state);

      while DynArray.length manager.queues <= state.len do
	DynArray.add manager.queues (newqueue ());
      done;
      Q.add (DynArray.get manager.queues state.len) state;

      let cost_density = ((state.qual +. startcost) /. (float_of_int state.len)) in
	if cost_density < manager.best_cost_density then begin
	  manager.best_cost_density <- cost_density;
	  manager.best_state <- Some state
	end
    end

let bounds manager (x,y) = 
  (x >= 0) && (y >= 0) && (x < manager.w) && (y < manager.h)

let data_term manager p q =
    Image.get manager.nlpb q
(*   if Image.get manager.nlpb q < infinity then *)
(*     let pts = Draw.line p q in *)
(*     let pts = List.tl pts in (\* should leave off q *\) *)
(*     let terms = List.map (Image.get manager.nlpb) pts in *)
(*     let terms = List.map (min max_neglog) terms in *)
(*       List.fold_left (+.) 0. terms *)
(*   else *)
(*     infinity *)

let erase ?(ends=true) im pts newv =
  let p = ref (List.hd pts) in
  let w, h = Image.width im, Image.height im in
  let spread (x,y) = 
    let rv = ref [] in
      for dy = -1 to 1 do
	for dx = -1 to 1 do
	  if 0 <= x+dx && x+dx < w && 0 <= y+dy && y+dy < h then
	    rv := (x+dx,y+dy) :: !rv;
	done
      done;
      !rv
  in
  let pts = 
    if (ends || List.length pts <= 2) then pts
    else List.tl (List.rev (List.tl pts))
  in

    List.iter
      begin fun q ->
	let line = Draw.line !p q in
	  List.iter
	    begin fun pt ->
	      List.iter 
		(fun pt2 -> Image.set im newv pt2)
		(spread pt)
	    end
	    line; (* should leave off q *)
	  p := q;
      end
      pts

let insert_start manager = 
  let npoints = ref 0 in
    for y = 0 to manager.h-1 do
      (*     printf "y=%d\n%!" y; *)
      for x = 0 to manager.w-1 do
	for dy = -maxd to maxd do
	  for dx = -maxd to maxd do
	    let qx,qy = x+dx, y+dy in
	      if (dx*dx + dy*dy >= minlen) && (dx != 0 || dy != 0) && bounds manager (qx,qy) then begin
		let qual = 
		  data_term manager (qx,qy) (x,y) +.
		  data_term manager (x,y) (qx,qy) in
		  if qual < qthresh (* infinity *) then begin
		    let state = {
		      density = qual;
		      qual = qual;
		      start = (x,y);
		      p = (x,y);
		      q = (qx,qy);
		      len = 1;
		    } in
(* 		      printf "    START: "; *)
(* 		      print_state manager state; *)
		      add_state manager state;
		      incr npoints
		  end
	      end
	  done
	done
      done
    done;
    printf "%d points\n%!" !npoints;
    (* are we out of points? *)
    (not (keep_going manager))

let smooth_term (px,py) (qx,qy) (rx,ry) =
  let dx1, dy1 = qx - px, qy - py in
  let dx2, dy2 = rx - qx, ry - qy in
    if (abs(dx1-dx2) > 1 ||
	  abs(dy1-dy2) > 1) then 
      infinity
    else
      let dz1 = {re=float_of_int dx1;im=float_of_int dy1} in
      let dz2 = {re=float_of_int dx2;im=float_of_int dy2} in
      let rv = 1. *. (dot dz1 dz2) /. (norm dz1) /. (norm dz2) in
      let rv = 1. -. rv in
	rv

let continue manager state =
  let sx, sy = state.start in
  let (px,py), (qx,qy) = state.p, state.q in
    for dy = -maxd to maxd do
      for dx = -maxd to maxd do
	if dy * dy + dx * dx >= minlen*minlen then begin
	  let (rx,ry) = (qx+dx,qy+dy) in
	    if (dx != 0 || dy != 0) && 
	      (rx != sx || ry != sy) &&
	      ((rx-sx)*(rx-sx) + (ry-sy)*(ry-sy) >= (qx-sx)*(qx-sx) + (qy-sy)*(qy-sy)) &&
	      bounds manager (rx,ry) then begin
		let qual = state.qual +.
		  data_term manager (qx,qy)  (rx,ry) +. (smooth_term state.p state.q (rx,ry)) in
		let state = {
		  density = qual /. (float_of_int (state.len + 1));
		  qual = qual;
		  start = state.start;
		  p = (qx,qy);
		  q = (rx,ry);
		  len = state.len + 1;
		} in
		  add_state manager state
	      end
	end
      done
    done

let rec next_state manager = 
  try
    let q = DynArray.get manager.queues manager.finger in
      manager.finger <- (manager.finger + 1) mod DynArray.length (manager.queues);
      Q.pop q
  with Failure _ ->
    next_state manager

let astar_loop manager maxiter =
  let n = ref 0 in
    while !n < maxiter && 
       (keep_going manager) do
	let state = next_state manager in
	  continue manager state;
	  incr n;
    done;
    (!n >= maxiter)

let rec last = function [hd] -> hd | hd :: tl -> last tl | [] -> failwith "empty"

(* let nbrs (x,y) =  *)
(*   [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y); *)
(*    (x-1,y+1);(x,y+1);(x+1,y+1);] *)

let nbrs (x,y) = 
  let rv = ref [] in
    for dy = -2 to 2 do
      for dx = -2 to 2 do
	rv := (x+dx,y+dy) :: !rv;
      done
    done;
    !rv

let construct_graph curves im =
  (* first, collapse endpoints that are close together *)
  let nudge = mkhash 1000 in
  let curves = 
    List.map 
      begin fun (c,wts) ->
	let c = Array.of_list c in
	let donudge i = 
	  let z = c.(i) in
	    if nudge >>? z then
	      c.(i) <- nudge >> z
	    else begin
	      List.iter
		(fun z' -> nudge << (z',z))
		(nbrs z)
	    end
	in
	  donudge 0;
	  donudge (Array.length c - 1);
	  (Array.to_list c,wts)
      end
      curves
  in

  let simplified = mkhash 1000 in
  let additional = mkhash 1000 in
  let final = mkhash 1000 in

  (* second, add some edges to support t-junctions *)
  let newpts = ref [] in
  let canonical p q = 
    if p > q then q,p
    else p,q
  in
  let add_edge edges (p,q) wt =
    let p,q = canonical p q in
      if (p <> q &&
	    ((not (edges >>? (p,q))) ||  wt < (edges >> (p,q)))) then begin
	edges << ((p,q), wt);
	if wt < infinity then
	  Draw.draw_line im p q  (255,255,255)
	else 
	  Draw.draw_line im p q  (255,128,128);
	erase im [p] (0,0,255);
	erase im [q] (0,0,255);
      end
  in
    List.iter 
      begin fun (c,_) ->
	let p, q = List.hd c, last c in
	let pnbr, pdist, qnbr, qdist = ref None, ref 1000000, ref None, ref 1000000 in
	  (* 	  add_edge ((px,py),(qx,qy)); *)
	  List.iter
	    begin fun (c2,_) ->
	      if c2 != c then begin
		let check z r distref zref = 
		  let rzdist = dist2 z r in
		    if rzdist < !distref then begin
		      zref := Some r;
		      distref := rzdist;
		    end
		in
		  List.iter
		    begin fun r ->
		      check p r pdist pnbr;
		      check q r qdist qnbr;
		    end
		    c2
	      end
	    end
	    curves;
	  begin match !pnbr with 
	      None -> ()
	    | Some r -> 
		add_edge additional (p,r) infinity;
		newpts := r :: !newpts;
	  end;
	  begin match !qnbr with 
	      None -> ()
	    | Some r -> 
		add_edge additional (q,r) infinity;
		newpts := r :: !newpts;
	  end
      end
      curves;

    (* third, break curves at new t-junctions *)
    List.iter
      begin fun (c,wts) ->
	let wts = Array.of_list wts in
	let clast = List.hd (List.rev c) in
	let carr = Array.of_list (List.rev (List.tl (List.rev c))) in
	let fingers = ref [] in
	  List.iter
	    begin fun p ->
	      Array.iteri
		begin fun i q ->
		  if p = q then
		    fingers := i :: !fingers;
		end
		carr
	    end
	    !newpts;
	  let prev = ref 0 in
	    List.iter
	      begin fun i ->
		add_edge simplified (carr.(!prev), carr.(i)) wts.(!prev);
		prev := i;
	      end
	      !fingers;
	    add_edge simplified (carr.(!prev) , clast) wts.(!prev);
      end
      curves;

    (*     fourth, save to file, run CDT, and read back edges  *)
    let chan = open_out "/var/tmp/epurdy/tmp.txt" in
    let print_edges edges =
      Hashtbl.iter
    	(fun ((px,py), (qx,qy)) _ ->
    	   fprintf chan "%d,%d %d,%d\n" px py qx qy) edges in
      print_edges simplified;
      print_edges additional;
      close_out chan;

      doit "./cdt/constrained /var/tmp/epurdy/tmp.txt > /var/tmp/epurdy/tmp2.txt";

      let chan = open_in "/var/tmp/epurdy/tmp2.txt" in
      let input_point x1 y1 x2 y2 =
	let p, q = canonical (x1,y1) (x2,y2) in
	  if not (simplified >>? (p,q)) then
	    add_edge final (p,q) infinity;
      in
	begin try
	  while true do
	    let line = input_line chan in
	      sscanf line "%d,%d %d,%d" input_point
	  done
	with End_of_file -> close_in chan;
	end;

	(* fifth, write down full curves *)
	List.iter
	  begin fun (c,wts) ->
	    let c, wts = Array.of_list c, Array.of_list wts in
	    let n = Array.length c in
	    let ofinger, finger = ref 0, ref 1 in
	    let tot = ref 0. in
	    let emit () = 
	      add_edge final (c.(!ofinger), c.(!finger)) !tot;
	      ofinger := !finger;
	      tot := 0.
	    in
	      while !finger < n-1 do
		tot := !tot +. wts.(!finger);
		if !finger mod nsubsample == 0 then 
		  emit ();
		incr finger
	      done;
	      emit ();
	  end
	  curves;

	final

let do_thing ipb nlpb =
  let gf = ref None in
  let ncurves = ref 0 in
  let the_curves = ref [] in
    while !ncurves < maxcurves do 
      let manager = create_manager nlpb in
      let nopoints = insert_start manager in
      let gaveup = astar_loop manager maxiter in
	if gaveup || nopoints then
	  ncurves := maxcurves
	else begin
	  printf "__________________________\n";
	  printf "%d CURVES\n" !ncurves;
	  printf "__________________________\n";
	  print_state manager (get manager.best_state);
	  let pts, density = reconstruct manager in

	    if List.length pts >= 2 then begin
	      let neglogs = List.map (fun (x,y) -> Image.get nlpb (x,y)) pts in
	      let probs = List.map (fun x -> exp (-. x)) neglogs in
		List.iter (printf "%f ") probs;
	      let neglogratios = List.map (fun p -> (* (log (1.-.p))*) -. (log p) ) probs in
(* 	      let ratiocost = List.fold_left (+.) 0. neglogratios in *)
(* 		printf "ratiocost = %f\n%!" ratiocost; *)
		the_curves := (pts,neglogratios) :: !the_curves;
		incr ncurves;
	    end;

	    erase ~ends:false nlpb pts infinity;
	    erase ipb pts 0;
	end;


	let im = Image.map Image.color_of_gray ipb in
	  (* 	let im = Image.create (Image.width ipb) (Image.height ipb) (0,0,0) in *)
	  List.iter 
	    begin fun (pts,w) ->
	      let p, q = List.hd pts, last pts in
	      let col = (Random.int 255, Random.int 255, Random.int 255) in
		erase im [p;q] (* pts *) col;
	    end
	    !the_curves;
	  gf := Some (construct_graph !the_curves im);
	  Pnm.save_ppm im Sys.argv.(4)
    done;
    printf "_______________________________________________\n%!";
    printf "_______________________________________________\n%!";
    printf "_______________________________________________\n%!";
    let gf = get !gf in
      gf

let nms theta im =
  let d = [| (0,1); (1,1); (1,0); (1,-1); |] in
    (*   let d = [| (1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1) |] in *)
  let w,h = Image.width im, Image.height im in
  let im2 = Image.create w h 0 in
    for y = 1 to h-2 do
      for x = 1 to w-2 do
	let v = Image.get im (x,y) in
	  if v > 0 then begin
	    let theta = Image.get theta (x,y) in
	    let theta = theta / 2 in
	    let dx,dy = d.(theta) in
	    let v1, v2 = Image.get im (x+dx,y+dy), Image.get im (x-dx,y-dy) in
	      assert((0 <= theta) && (theta <= 7));
	      if v > v1 && v > v2 then
		Image.set im2 v (x,y);
	  end
      done
    done;
    im2

let _ =
  let chan = open_out Sys.argv.(1) in 
  let im = Pnm.load_pgm Sys.argv.(2) in
  let theta = Pnm.load_pgm Sys.argv.(3) in

  let ipb = nms theta im in
  let nlpb = Image.map (fun x -> -.log (min maxp ((float_of_int x) /. 255.))) ipb in

  let gf = do_thing ipb nlpb in
    Hashtbl.iter
      (fun (p, q) wt -> fprintf chan "%s %s [%f]\n" (spt p) (spt q) wt)
      gf;
    close_out chan


    
