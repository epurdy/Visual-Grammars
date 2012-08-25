open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf
open Models.Simple

let make_grammar excurve_name sdf_name gram_name = 
  let curve = Curve.load excurve_name in
  let family = Sdf.load_family sdf_name in
  let family = Sdf.bottom_out_family family in 
  let gram = Models.LLL_both.make_grammar (curve, family) in
  let file = open_out gram_name in
    Marshal.to_channel file gram [];
    close_out file

let _ =
  let excurve_name, sdf_name, gram_name = 
    try
     Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
    with _ ->
      printf "usage: ./%s ex.curve ex.sdf out.gram" Sys.argv.(0);
      "", "", ""
  in
    make_grammar excurve_name sdf_name gram_name
