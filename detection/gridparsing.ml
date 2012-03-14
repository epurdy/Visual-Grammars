open Printf
open Util.Misc
open Util.Hops
open Util.Cops
open Abstract
open Grammar
open Graph

module I = Image

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

let coarsen_edge_map edgemap factor = 
  printf "starting coarsening\n%!";
  let w,h = I.width edgemap, I.height edgemap in
  let w2, h2 = w/factor, h/factor in
  let newmap = I.create w2 h2 false in
    for y=0 to (h2-1) * factor do
      for x=0 to (w2-1) * factor do
	if (I.get edgemap (x,y)) then begin
	  I.set newmap true (x/factor, y/factor)
	end;
      done
    done;

    printf "finished coarsening\n%!";
    newmap

let make_net gran size = 
  let step = size / gran in
  let foo (x,y) = (step*x,step*y) in
  let net = Graph.new_live_graph () in
(*   let maxdist = 10 in *)
  let maxdist = 15 in

    printf "making network\n%!";

    for x = 0 to gran-1 do
      for y = 0 to gran-1 do
	ignore (Graph.add_vertex net (foo (x,y)));
      done;
    done;

    (* start at 1, go to gran-1 to avoid edge effects *)
    let minx, maxx, miny, maxy = 1, gran-2, 1, gran-2 in

      for x1 = minx to maxx do
	for y1 = miny to maxy do
	  for x2 = (max minx (x1-maxdist)) to (min maxx (x1+maxdist)) do
	    for y2 = (max miny (y1-maxdist)) to (min maxy (y1+maxdist)) do
	      if dist2 (x1,y1) (x2,y2) <= maxdist * maxdist then begin
		let p, q = foo (x1,y1), foo (x2,y2) in
		  if not (Graph.is_edge net p q) then
		    Graph.add_edge_strict net p q ();
	      end
	    done;
	  done;
	done;
      done;


      printf "done making network\n%!";

      Graph.finalize net


(* let inside_pass gram gf nodes pdata name = *)
(*   let nnodes = Array.length nodes in *)
(*   let nsymbols = Frozen.num_symbols gram in *)
    
(*     Frozen.iter_symbols_rev gram begin fun sym -> *)
(*       Frozen.iter_decompositions gram sym begin fun prod -> *)
(* 	Array.iteri begin fun pnum pid -> *)
(* 	  printf "%s\t" name; *)
(* 	  printf "  nt=%d/%d pnum=%d/%d (s%d -> s%d s%d)\n"  *)
(* 	    sym.sid nsymbols pnum nnodes prod.topsid prod.leftsid prod.rightsid; *)

(* 	  Array.iter begin fun qid -> *)
	    
(* 	    Array.iter begin fun rid -> *)
(* 	      if not sym.startable || (pid=rid) then begin  *)
		
(* 		let leftcost = pdata.costs >>! ((prod.leftsid,pid,qid), infinity) in *)
(* 		let rightcost = pdata.costs >>! ((prod.rightsid,qid,rid), infinity) in *)
(* 		let shape = Shape.shape_of_ints_bme *)
(* 		  gf.vertices.(pid) gf.vertices.(qid) gf.vertices.(rid) in *)
(* 		let cost = leftcost +. rightcost +. (Models.Simple.prod_cost prod shape) in *)
		  
(* 		  if cost < (pdata.costs >>! ((sid,pid,rid), infinity)) then begin  *)
(* 		    pdata.costs << ((sid, pid, rid), cost); *)
(* 		    pdata.split << ((sid, pid, rid), Some (prod.cid, qid)); *)

(* 		    if cost < (pdata.thisbestqual >>! (sid, infinity)) then begin  *)
(* 		      pdata.thisbestqual << (sid, qual); *)
(* 		      pdata.thisbest << (sid, (pid,qid)); *)
(* 		    end *)
(* 		  end *)
(* 	      end *)
(* 	    end *)
(* 	      nodes *)
(* 	  end *)
(* 	    nodes *)
(* 	end *)
(* 	  nodes *)
(*       end *)
(*     end *)
      
