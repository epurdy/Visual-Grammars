open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops

let one_to_one incurve_name excurve_name sdf_name out_name = 
  let x = Svg.create out_name in

  let curve = Curve.load incurve_name in
  let n = Array.length curve in
  let family = Sdf.make_full_family n in
  let family = Sdf.add_curve_data_to_family_debug curve family in

  let excurve = Curve.load excurve_name in
  let exfamily = Sdf.load_family sdf_name in
  let exfamily = Sdf.add_curve_data_to_family_debug excurve exfamily in
  let gram = Grammar.grammar_of_family exfamily in
    

    let qual, pairs = Parsing.viterbi gram family in
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
      let thebigcurve = (gram.Abstract.get_symbol 0).Grammar.sdata.Sdf.curve in
      let labels = mkhash 100 in

      List.iter 
	begin fun (sid, scid) ->
	  let symbol = gram.Abstract.get_symbol sid in
 	  let scurve = family.Abstract.get_symbol scid in 
	  let thecurve = symbol.Grammar.sdata.Sdf.curve in
	  let bg, en = 
	    if symbol.Grammar.closed then 
	      thecurve.(0), thecurve.(0)
	    else
	      thecurve.(0), thecurve.(Array.length thecurve - 1) 
	  in
	  let ibg, ien = find bg thebigcurve, find en thebigcurve in

(* 	    labels <<+ (ibg, scurve.Sdf.first); *)
(* 	    labels <<+ (ien, scurve.Sdf.last); *)
	    labels <<+ (scurve.Sdf.first, ibg);
	    labels <<+ (scurve.Sdf.last, ien);
	    printf "s%d (%d,%d) -> sc%d (%s)\n" sid
	      ibg ien
	      scid
	      (Sdf.symbol_name (family.Abstract.get_symbol scid));
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

	let excurve = Array.map (fun {C.re=x;C.im=y} -> {C.re=y;C.im=x}) excurve in
	let curve = Array.map (fun {C.re=x;C.im=y} -> {C.re=y;C.im=x}) curve in
	  
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
