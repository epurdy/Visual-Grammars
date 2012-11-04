open Printf
open Util.Misc
open Util.Hops
open Util.Cops
open Abstract
open Grammar
open Graph

module I = Image

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)


let make_short_edge_graph (w,h) maxdist = 
(*   let step = size / gran in *)
(*   let foo (x,y) = (step*x,step*y) in *)
  let net = Graph.new_live_graph () in

    printf "making network\n%!";

    for x = 0 to w-1 do
      for y = 0 to h-1 do
	ignore (Graph.add_vertex net (x,y));
      done;
    done;

    (* start at 1, go to gran-1 to avoid edge effects *)
    let minx, maxx, miny, maxy = 1, w-2, 1, h-2 in

      for x1 = minx to maxx do
	for y1 = miny to maxy do
	  for x2 = (max minx (x1-maxdist)) to (min maxx (x1+maxdist)) do
	    for y2 = (max miny (y1-maxdist)) to (min maxy (y1+maxdist)) do
	      let p, q = (x1,y1), (x2,y2) in
		if dist2 p q <= maxdist * maxdist then begin
		  if not (Graph.is_edge net p q) then
		    Graph.add_edge_strict net p q ();
	      end
	    done;
	  done;
	done;
      done;

      printf "done making network\n%!";

      Graph.finalize net

let draw_plane_partition partition points (width,height) =
  let sqsize = 5 in
  let colors = Array.map (fun x -> (Random.int 256, Random.int 256, Random.int 256)) 
    partition.Filtration.parents 
  in
  let im = Image.create (sqsize*width) (sqsize*height) (255,255,255) in
    Array.iteri
      begin fun i (x,y) ->
	let j = Filtration.pfind partition i in
	  for a=0 to sqsize-1 do
	    for b=0 to sqsize-1 do
	      I.set im colors.(j) (x*sqsize+a, y*sqsize+b);
	    done
	  done
      end
      points;
    im

let make_plane_coarsenings nlvls =
  let coarsenings = 
    Array.init
      (nlvls-1)
      begin fun i ->
	let i = i+1 in
	let coarsenint x = (x asr i) lsl i in
	let coarsen (x,y) = (coarsenint x, coarsenint y) in
	  coarsen
      end
  in
    coarsenings


let make_plane_filtration points nlvls = 
  Filtration.create_filtration (points: (int*int) array) (make_plane_coarsenings nlvls)
