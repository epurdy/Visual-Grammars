open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf
open Models.Simple

let scale = 100.

let one_to_one incurve_name excurve_name sdf_name out_name = 
  let x = Svg.create_sized out_name (3000.,3000.) in
  let _ = Cairo.scale x.Svg.ctx scale scale in

  let curve = Curve.load incurve_name in
  let n = Array.length curve in
  let family = Sdf.make_full_family n in
  let family = Models.Simple.add_curve_data_to_family curve family in
    
  let excurve = Curve.load excurve_name in
  let gram = Models.Simple.make_grammar 
    (excurve, Sdf.load_family sdf_name) in


    let qual, pairs = Parsing.viterbi gram family Models.Simple.strat in
      printf "qual = %f\n#pairs = %d\n%!" qual (List.length pairs);

      let find x arr = 
	let rv = ref None in
	Array.iteri
	  begin fun i y ->
	    if x = y then begin
	      rv := Some i
	    end
	  end arr;
	  get !rv
      in
      let thebigcurve = (Frozen.start gram).sdata.curve in
      let labels = mkhash 100 in

      List.iter 
	begin fun (sid, scid, _, _) ->
	  let symbol = Frozen.get_symbol gram sid in
 	  let scurve = Frozen.get_symbol family scid in 
	  let thecurve = symbol.sdata.curve in
	  let bg, en = 
	    if symbol.startable then 
	      thecurve.(0), thecurve.(0)
	    else
	      thecurve.(0), thecurve.(Array.length thecurve - 1) 
	  in
	  let ibg, ien = find bg thebigcurve, find en thebigcurve in

(* 	    labels <<+ (ibg, scurve.Sdf.first); *)
(* 	    labels <<+ (ien, scurve.Sdf.last); *)
	    labels <<+ (scurve.sdata.first_, ibg);
	    labels <<+ (scurve.sdata.last_, ien);
	    printf "s%d (%d,%d) -> sc%d (%d,%d)\n" sid
	      ibg ien
	      scid
	      (Frozen.get_symbol family scid).sdata.first_
	      (Frozen.get_symbol family scid).sdata.last_;
	    Geometry.print_cpt bg;
	    Geometry.print_cpt en;
	    printf "---\n%!";
	end
	pairs;

	Hashtbl.iter 
	  begin fun k v ->
	    printf "%d -> %d\n%!" k v;
	  end
	  labels;

	let labels = 
	  Array.init (Array.length curve)
	  begin fun i ->
	    if labels >>? i then 
	      (sprintf "%d" (labels >> i))
	    else
	      ""
	  end
	in

	let excurve = Curve.flip_xy excurve in
	let curve = Curve.flip_xy curve in
	  
	  Viz.show_labeled_curve x excurve (Array.init (Array.length excurve) (sprintf "%d"));
	  Cairo.translate x.Svg.ctx 10. 0.;
	  Viz.show_labeled_curve x curve labels;
	  Svg.finish x;

;;    

let _ =
  let incurve_name, excurve_name, sdf_name, out_name = 
    try
     Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4)
    with _ ->
      printf "usage: ./%s in.curve ex.curve ex.sdf out.svg" Sys.argv.(0);
      "", "", "", ""
  in
    one_to_one incurve_name excurve_name sdf_name out_name
