open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf

let parse sdf_name excurve_name curve_name qual_name = 
  let excurve = Curve.load excurve_name in
  let curve = Curve.load curve_name in
  let n = Array.length curve in
  let family = Sdf.make_full_family n in
  let family = Models_mpeg7.add_curve_data_to_family curve family in
  let gfamily = Sdf.load_family sdf_name in
  let gfamily = Sdf.bottom_out_family gfamily in
  let gram = Models_mpeg7.make_grammar (excurve, gfamily) in
  let qual, pairs = Parsing.viterbi gram family Models_mpeg7.strat in
  let qualfile = open_out qual_name in 
    printf "qual=%f\n%!" qual;
    fprintf qualfile "%f\n%!" qual;
    close_out qualfile
      
let _ =
  let sdf_name, excurve_name, curve_name, qual_name = 
    try
      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4)
    with _ ->
      printf "usage: ./%s ex.sdf excurve.curve curve.curve out.qual" Sys.argv.(0);
      "", "", "", ""
  in
    parse sdf_name excurve_name curve_name qual_name
