open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf
open Models.Simple

let parse gram_name curve_name qual_name = 
  let curve = Curve.load curve_name in
  let n = Array.length curve in
  let family = Sdf.make_full_family n in
  let family = Models.Mpeg7.add_curve_data_to_family curve family in
  let gramfile = open_in gram_name in
  let gram = Marshal.from_channel gramfile in
  let qual, pairs = Parsing.viterbi gram family Models.Mpeg7.strat in
  let qualfile = open_out qual_name in 
    printf "qual=%f\n%!" qual;
    fprintf qualfile "%f\n%!" qual;
    close_in gramfile;
    close_out qualfile
      
let _ =
  let gram_name, curve_name, qual_name = 
    try
      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
    with _ ->
      printf "usage: ./%s ex.gram curve.curve out.qual" Sys.argv.(0);
      "", "", ""
  in
    parse gram_name curve_name qual_name
