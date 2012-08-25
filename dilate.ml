open Printf
open Util.Misc
module Shock = Structure.Shock



let _ = 
  let imname = Sys.argv.(1) in
  let curvename = Sys.argv.(2) in

(*   let dir = Sys.argv.(2) in *)
(*   let _ = doit (sprintf "mkdir -p %s" dir) in *)

  let im = Pnm.load_pgm imname in
  let w,h = Image.width im, Image.height im in
  let im = Shock.add_margin im 0 30 in

  (* dilation distance proportional to smaller dimension *)
  let dist = 10. *. (foi (min w h)) /. 550. in
  let dist2 = dist *. dist in
    
  let color, seed, out = Shock.find_largest_nonblack_component im in
  let boundary, bpoints = Shock.trace_boundary im color seed out in
  let dt = Morph.dt boundary in
  let dilated = Image.map (fun x -> if (x <= dist2) then 255 else 0) dt in
(*   let _ = Pnm.save_pgm dilated "01.dilated.pgm" in *)

  let color, seed, out = Shock.find_largest_nonblack_component dilated in
  let boundary, bpoints = Shock.trace_boundary dilated color seed out in
(*   let _ = Pnm.save_ppm out "02.dlargest.ppm" in *)

  (* subsample curve *)
  let bcurve = Array.map Geometry.complex_of_point bpoints in
  let sscurve = Curve.subsample_dp bcurve 50 2 in
  let _ = Curve.save curvename sscurve in
(*   let _ = Pnm.save_pgm (Curve.draw sscurve 255 0) "03.curve.pgm" in *)

    ()
