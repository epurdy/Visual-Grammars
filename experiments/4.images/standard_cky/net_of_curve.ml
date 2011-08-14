open Printf
open Util.Misc
open Util.Hops
open Util.Cops
open Abstract
open Grammar
open Graph

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
let canon p q = if p > q then q,p else p,q

let net_of_curve c gran size imname = 
  let step = size / gran in
  let foo (x,y) = (step*x,step*y) in
  let im = Image.create (size+1) (size+1) 255 in
  let net = Graph.new_live_graph () in
  let c = Curve.normalize ~scale:(float_of_int gran) c in
  let c = Array.map Geometry.point_of_complex c in
  let c = Array.map foo c in
  let c = Curve.uniqify c in
  let n = Array.length c in
  let maxdist = 3 in

    for x = 0 to gran-1 do
      for y = 0 to gran-1 do
	ignore (Graph.add_vertex net (foo (x,y)));
	Image.set im 0 (x*step,y*step);
      done;
    done;

    for i = 0 to n-1 do 
      Graph.add_edge_strict net c.(i) c.((i+1) mod n) 1.;
      Draw.draw_line im c.(i) c.((i+1) mod n) 128;
    done;

    for x1 = 0 to gran-1 do
      for y1 = 0 to gran-1 do
	for x2 = 0 to gran-1 do
	  for y2 = 0 to gran-1 do
	    if dist2 (x1,y1) (x2,y2) < maxdist * maxdist then begin
	      let p, q = foo (x1,y1), foo (x2,y2) in
	      if not (Graph.is_edge net p q) then
		Graph.add_edge_strict net p q 100.;
	    end
	  done;
	done;
      done;
    done;

    Pnm.save_pgm im imname;
    printf "Finished building network!\n%!";
    Graph.finalize net

let _ = 
  let granularity = 16 in
  let size = 256 in

  let curve = Curve.load Sys.argv.(1) in
  let imname = Sys.argv.(4) in
  let net = net_of_curve curve granularity size imname in

    Graph.save_point_float_graph

