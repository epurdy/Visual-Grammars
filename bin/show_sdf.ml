open Util.Misc
open Printf
open Curve
open Abstract
open Sdf

let ncols = 5
let scale = 100.

let _ = 
  let sdf = ref "" in
  let curve = ref "" in
  let fname = ref "foo.svg" in

  let _ = Arg.parse ["-sdf", Arg.Set_string sdf, "SDF file.";
		     "-curve", Arg.Set_string curve, "Curve file.";
		     "-fname", Arg.Set_string fname, "File to write to.";]
    (placeholder "Arg.parse") "./prog -fname fname -sdf foo.sdf"
  in

  let curve = Curve.load !curve in
  let curve = Curve.flip_best curve in
  let curve = Curve.normalize curve in
  let doubled = Array.append curve curve in
  let sdf = Sdf.load_family !sdf in

  let scurves = 
    Array.map
      begin fun dcomp ->
	let scurve = Frozen.get_symbol sdf dcomp.topsid in
	let thecurve = Array.sub doubled 
	  scurve.sdata.first
	  (scurve.sdata.len + 1)
	in
	  thecurve
      end
      sdf.f_compositions
  in
  let midpoints = 
    Array.map
      begin fun dcomp ->
	[| curve.(dcomp.cdata.md) |]
      end
      sdf.f_compositions
  in

  let nrules = Array.length midpoints in

    
  let x = Svg.create_sized !fname
    ((float_of_int ncols) *. 1.5 *. scale, 
     float_of_int (5 + (nrules/ncols)) *. 4.0 *. scale) in
  let _ = Cairo.scale x.Svg.ctx scale scale in


    Viz.show_samples_midpoints x "" 6 curve
      (scurves, midpoints) 1.0;
    Svg.finish x;

    printf "\n>>> I was supposed to save to file %s, but maybe I didn't?\n\n" !fname
