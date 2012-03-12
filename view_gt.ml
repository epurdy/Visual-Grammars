open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph

module I = Image
module G = Geometry

let _ = 
  let excurve = Curve.load "romer/newann/IMG0020.curve" in
  let excurve = Array.map G.point_of_complex excurve in 
  let excurve = Array.map (fun (x,y) -> (x/4, y/4)) excurve in

  let sigma = ref 1.0 in
  let gridsize = ref 1 in
    for i=1 to 3 do 
      sigma := 2.0 *. !sigma;
      let im = Pnm.load_pgm "romer/clean/IMG0020.PGM" in
      let hi = 100. in
      let lo = 50. in
      let im = Image.map (float_of_int) im in
      let im = Filter.smooth Image.float_ops im !sigma in
      let edges = Canny.canny im hi lo in
      let edges = Gridparsing.coarsen_edge_map edges 4 in

      let out = Image.map (fun _ -> (255,255,255)) edges in
      let curvemap = Image.map (fun _ -> false) edges in
      let _ = 
	let n = Array.length excurve in
	  for i=0 to n-1 do 
	    Draw.draw_line curvemap excurve.(i) excurve.((i+1) mod n) true;
	  done;

	  gridsize := 2 * !gridsize;
      in


      let w,h = Image.width out, Image.height out in
      let tt,tf,ft,ff = ref 0, ref 0, ref 0, ref 0 in
	for y=0 to h-1 do    
	  for x=0 to w-1 do
	    let e = I.get edges (x,y) in
	    let c = I.get curvemap (x,y) in
	    let color =
	      match (e,c) with
		  true,true -> 
		    incr tt;
		    (0,255,0)
		| false,true ->
		    incr ft;
		    (255,0,0)
		| true,false -> 
		    incr tf;
		    (0,0,0)
		| false,false -> 
		    incr ff;
		    (255,255,255)
	    in
	      I.set out color (x,y);
	  done
	done;

	printf "scale #%d\n" i;
	printf "tt=%d, ft=%d\n" !tt !ft;
	printf "tf=%d, ff=%d\n" !tf !ff;

	Pnm.save_ppm out (sprintf "gtedges.%d.ppm" i);

	let edges = Image.map (function true -> 0 | false -> 255) edges in
	  Pnm.save_pgm edges (sprintf "edges.%d.pgm" i);
    done
