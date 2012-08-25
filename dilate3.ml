open Printf
open Util.Misc
module Shock = Structure.Shock
module I = Image

let dilate im dist =
  let dist2 = dist *. dist in
  let color, seed, out = Shock.find_largest_nonblack_component im in
  let boundary, bpoints = Shock.trace_boundary im color seed out in
  let dt = Morph.dt boundary in
  let _,_, sdt = Shock.find_interior_and_signed_distance seed boundary color dt in
  let dilated = Image.map (fun x -> if (x <= dist2) then 255 else 0) sdt in

    dilated

let erode im dist =
  let dist2 = dist *. dist in
  let color, seed, out = Shock.find_largest_nonblack_component im in
  let boundary, bpoints = Shock.trace_boundary im color seed out in
  let dt = Morph.dt boundary in
  let _,_, sdt = Shock.find_interior_and_signed_distance seed boundary color dt in
  let eroded = Image.map (fun x -> if (x <= -. dist2) then 255 else 0) sdt in

    eroded

let trace_curve im = 
  let color, seed, out = Shock.find_largest_nonblack_component im in
  let boundary, bpoints = Shock.trace_boundary im color seed out in
    Array.map Geometry.complex_of_point bpoints

let consider thing im =
  let color, seed, out = Shock.find_largest_nonblack_component im in
  let boundary, bpoints = Shock.trace_boundary im color seed out in
  let dt = Morph.dt boundary in
    Image.iterxy
      begin fun (x,y) oldv ->
	let dt2 = I.get dt (x,y) in
	  I.set thing (oldv +. dt2) (x,y)
      end
      thing;

    ()

let _ = 
  let imname = Sys.argv.(1) in
(*   let curvename = Sys.argv.(2) in *)

  let dir = Sys.argv.(2) in
  let _ = doit (sprintf "mkdir -p %s" dir) in

  let im = Pnm.load_pgm imname in
  let w,h = Image.width im, Image.height im in
  let im = Shock.add_margin im 0 30 in

  let thing = Image.map (fun x -> 0.0) im in

  (* dilation distance proportional to smaller dimension *)
  let dist = 10. *. (foi (min w h)) /. 550. in

  let fnames = ref "" in

    for i = 1 to 5 do 
      let idist = dist *. (foi i) in
      let dilated = erode (dilate im idist) idist in
      let eroded = dilate (erode im idist) idist in
      let _ = Pnm.save_pgm dilated (sprintf "%s/%d.dilated.pgm" dir i) in
      let _ = Pnm.save_pgm eroded (sprintf "%s/%d.eroded.pgm" dir i) in
      let dcurve = trace_curve dilated in
      let ecurve = trace_curve eroded in
      let dname = sprintf "%s/%d.dilated.curve" dir i in
      let ename = sprintf "%s/%d.eroded.curve" dir i in
	Curve.save dname dcurve;
	Curve.save ename ecurve;
	fnames := !fnames ^ (sprintf " %s %s " dname ename);

	consider thing dilated;
	consider thing eroded;
    done;

    let thing = Image.map (fun x -> (round (255. *. ((exp (-. (x/.500.))))))) thing in
      
      Pnm.save_pgm thing (sprintf "%s/thing.pgm" dir);

      doit (sprintf "./show_superimposed.native -fname %s/curves.svg %s" dir !fnames);

    ()
