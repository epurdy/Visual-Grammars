open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf

module Model = Models.Mpeg7
open Model

module Vizparse = Visualization.Vizparse

(* let do_parse incurve_name excurve_name sdf_name out_name qual_name =  *)
(*   let x = Svg.create out_name in *)

(*   let curve = Curve.load incurve_name in *)
(*   let curve = Curve.flip_xy curve in *)

(*   let excurve = Curve.load excurve_name in *)
(*   let gfamily = Sdf.load_family sdf_name in *)
(*   let gfamily = Sdf.bottom_out_family gfamily in *)
(*   let gram = Model.make_grammar (excurve, gfamily) in *)

(*   let model = { *)
(*     Vizparse.add_curve_data_to_family = Model.add_curve_data_to_family; *)
(*     Vizparse.strat = Model.strat; *)
(*   } *)
(*   in *)

(*   let qual = Vizparse.display_parse model gram excurve curve x in *)
(*     Svg.finish x; *)

(*     () *)

(* let _ = *)
(*   let incurve_name, excurve_name, sdf_name, out_name, qual_name =  *)
(*     try *)
(*      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4), Sys.argv.(5) *)
(*     with _ -> *)
(*       printf "usage: ./%s in.curve ex.curve ex.sdf out.svg out.qual" Sys.argv.(0); *)
(*       "", "", "", "", "" *)
(*   in *)
(*     do_parse incurve_name excurve_name sdf_name out_name qual_name *)
