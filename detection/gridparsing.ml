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
