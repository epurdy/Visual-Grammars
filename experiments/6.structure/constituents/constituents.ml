(* Something seems subtly wrong with the pictures, maybe an off by one error *)

open Printf
open Util.Hops
open Abstract
open Sdf
open Geometry
open Util.Cops

module C = Complex
module S = Sdf

let nl_watson_const sigma =
  log 2. +. 2. *. log Con.pi -. log sigma +.
    sigma

let nl_watson sigma x mean =
  let x, mean = Shape.get_pt x, Shape.get_pt mean in
  let sigma = sigma /. 10. in
  let conc = 1. /. (2. *. sigma *. sigma) in
  let canon a = center_and_scale (c0, a, c1) in
  let z0,z1,z2 = canon x in
  let m0,m1,m2 = canon mean in
  let dprod = (~& z0) *& m0 +& (~& z1) *& m1 +& (~& z2) *& m2 in
    (nl_watson_const conc) -. conc *. (C.norm2 dprod)

let invsq2 = {C.re=sqrt 0.5; C.im=0.}
let invsq6 = {C.re=sqrt (1./.6.); C.im=0.}

let helmert shape = 
  let x = Shape.get_pt shape in
  let z0, z1, z2 = center_and_scale (c0, x, c1) in
    (invsq2 *& (z0 -& z1), invsq6 *& (z0 +& z1 -& z2 -& z2))

let _ = 
  let x = Svg.create Sys.argv.(1) in

  let excurve = Curve.load "romer/ann/curve0000.curve" in
  let doubled = Array.append excurve excurve in

  let curves = Array.init 15
    begin fun i -> 
      let idx = (i+1) * 10 in
      let fname = sprintf "romer/ann/curve%04d.curve" idx in
	printf "romer/ann/curve%04d.curve\n%!" idx;
	Curve.load fname
    end
  in

  let n = Array.length excurve in
  let family = Sdf.make_full_family n in
    
  let tab = mkhash 100 in
  let bestdcomp = mkhash 100 in
  let best = ref 0. in
  let besttop = ref 0 in

    family.iter_symbols
      begin fun scurve ->
	if family.x.lexical_ok scurve then begin
	  tab << (scurve.S.sid, 0.);
	end;

	if family.x.binary_ok scurve then begin
	  family.iter_decompositions scurve
	    begin fun dcomp ->
	      let bg, md, en = dcomp.bg, dcomp.md, dcomp.en in
	      let model = Shape.shape_of_complex_bme
		excurve.(bg) excurve.(md) excurve.(en) in
	      let shapes = Array.map 
		(fun c -> Shape.shape_of_complex_bme c.(bg) c.(md) c.(en))
		(Array.append [| excurve |] curves)
	      in
(* 		printf "dcomp =[%d,%d,%d]\n%!" bg md en; *)

		let s11, s12, s21, s22 = ref c0, ref c0, ref c0, ref c0 in
		  Array.iter 
		    begin fun shape ->
		      let z1,z2 = helmert shape in
(* 			print_cpt z1; *)
(* 			print_cpt z2; *)
			s11 := !s11 +& (z1 *& (~& z1));
			s12 := !s12 +& (z1 *& (~& z2));
			s21 := !s21 +& (z2 *& (~& z1));
			s22 := !s22 +& (z2 *& (~& z2));
(* 			printf "---\n%!"; *)
(* 			print_cpt !s11; *)
(* 			print_cpt !s12; *)
(* 			print_cpt !s21; *)
(* 			print_cpt !s22; *)
(* 			printf "---\n%!"; *)
		    end
		    shapes;

		  let det = !s11 *& !s22 -& !s12 *& !s21 in
		  let tr = !s11 +& !s22 in
		    (* det and trace are real, because eigenvalues are
		       real for hermitian *)
		  let tr, det = tr.C.re, det.C.re in
		  let large_eval = 0.5 *. tr +. sqrt (0.25 *. tr *. tr -. det) in
		  let num = float_of_int (Array.length shapes) in
		  let conc = 
		    if large_eval >= 0.999 *. num then
		      1000.
		    else
		      num /. (num -. large_eval) 
		  in
		  let cost = ref 
		    ((-. conc *. large_eval -. num *. (nl_watson_const conc)) +.
		       (tab >> dcomp.leftsid) +. (tab >> dcomp.rightsid)) in
		    
(* 		    printf "num = %f\n" num; *)
(* 		    printf "eval = %f\n" large_eval; *)
(* 		    printf "conc = %f\n" conc; *)
(* 		    printf "left = %f\n" (tab >>! (dcomp.leftsid, nan)); *)
(* 		    printf "right = %f\n" (tab >>! (dcomp.rightsid,nan)); *)
(* 		    printf "cost = %f\n" !cost; *)


		    (* 	      let cost = ref ((tab >> dcomp.leftsid) +. (tab >> dcomp.rightsid)) in *)
		    (* 		Array.iter  *)
		    (* 		  begin fun shape -> *)
		    (* 		    let shapecost = nl_watson 1.0 model shape in *)
		    (* 		      cost := !cost +. shapecost; *)
		    (* 		  end *)
		    (* 		  shapes; *)

		    if !cost <= (tab >>! (scurve.S.sid, infinity)) then begin
		      tab << (scurve.S.sid, !cost);
		      bestdcomp << (scurve.S.sid, dcomp.S.cid);
		    end
	    end
	end;
	
	if family.x.goal_ok scurve then begin
	  if tab >>! (scurve.S.sid,infinity) < !best then begin 
	    best := tab >> scurve.S.sid;
	    besttop := scurve.S.sid;
	  end
	end
      end;

    let excurve = Curve.normalize excurve in
    let doubled = Array.append excurve excurve in

    let scurves = ref [] in
    let midpoints = ref [] in

    let rec visit sid = 
      let scurve = family.get_symbol sid in
	if bestdcomp >>? sid then begin
	  let dcompid = bestdcomp >> sid in
	  let dcomp = family.get_composition dcompid in
	  let thecurve = Array.sub doubled scurve.first (scurve.len + 1) in
	  let thecurve = Curve.flip_xy thecurve in
	    scurves := thecurve :: !scurves;
	    midpoints := [| Geometry.flip_xy excurve.(dcomp.md) |] :: !midpoints;
	    printf "[%d,%d] -> [%d,%d] [%d,%d]\n" 
	      dcomp.bg dcomp.en dcomp.bg dcomp.md dcomp.md dcomp.en;
	    visit dcomp.leftsid;
	    visit dcomp.rightsid;
	end
	else begin
	  (* 	let scurve = family.get_symbol sid in *)
	  (* 	  printf "[%d,%d] x\n" scurve.first scurve.last *)
	end
    in
      visit !besttop;

      Viz.show_samples_midpoints x "" 6 (Curve.flip_xy excurve)
	(Array.of_list (List.rev !scurves), Array.of_list (List.rev !midpoints)) 1.0;
      Svg.finish x;
;;
