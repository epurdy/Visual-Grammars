open Grammar
open Svg
open Util.Misc
open Printf
open Util.Cops
module C = Complex
module N = Parzen
module Geo = Geometry
open Sdf
open Grammar
open Abstract

let granularity = 100
let smallsigma = (* 1.0 *) 0.01
let baseline_weight = 0.01

let curve = 
  Array.map (fun x -> Geo.complex_of_point x)

[|
(201, 626);     (* 0  bottom left left finger / middle left hand *)
(197, 772); 	(* 1  top left left finger *)
(239, 772); 	(* 2  top right left finger *)
(242, 618); 	(* 3  bottom right left finger *)
(283, 619); 	(* 4  bottom left right finger *)
(282, 771); 	(* 5  top left right finger *)
(322, 770); 	(* 6  top right right finger *)
(325, 614); 	(* 7  bottom right right finger *)
(432, 628); 	(* 8  top thumb reach	 *)
(439, 576); 	(* 9  bottom thumb reach	 *)
(331, 568); 	(* 10 bottom thumb anchor	 *)
(332, 470); 	(* 11 bottom right hand	 *)
(205, 477); 	(* 12 bottom left hand	 *)
(* (201, 626);     (\* 13 bottom left left finger / middle left hand *\) *)
|]

let p,q = curve.(0), curve.(7)

let gram = 
  let doubled = Array.append curve curve in

  let make_shape (i,j,k) = 
    Shape.shape_of_complex_bme curve.(i) curve.(j) curve.(k)
  in    

  let rot_shape s rot =
    let p = Shape.get_pt s in
    let p = p *& rot in
      Shape.shape_of_complex_bme c0 p c1
  in

  let n = Array.length curve in
  let d i j = 
    let rv = (j - i + n) mod n in
    let rv = (rv + n - 1) mod n in
      rv + 1
  in

  let gram = Abstract.new_live_grammar ()
    (*     {dscale=1.; curve=curve;}  *)
    (*     {dscale=0.; curve=[||];}  *)
    (*     {dshape=Shape.default_shape; lcurve=[||]; rcurve=[||]; ocurve=[||];}  *)
  in

  let sdata = {
    closed = true;
    straightprob = 0.;
    straightcost = infinity;
    curve = curve
  }
  in
  let _ =
    Abstract.make_new_symbol gram sdata true
  in

  (*   let gram = Grammar.make_live_shape_grammar   *)
  (*     {dscale=1.; curve=curve;}  *)
  (*     {dscale=0.; curve=[||];}  *)
  (*     {dshape=Shape.default_shape; lcurve=[||]; rcurve=[||]; ocurve=[||];}  *)
  (*   in *)
    
  let make_state sprob (i,j) = 
    let	curve = Array.sub doubled i ((d i j) + 1) in
    let sdata = {
      closed = false;
      straightprob = sprob;
      straightcost = -. log sprob;
      curve = curve;
    }
    in
      (*       gram.make_new_state false (-. log sprob) {dscale=0.; curve=curve;} *)
      Abstract.make_new_symbol gram sdata false
  in

  let make_cprod (i,j,k) p x (y,z) =
    let	lcurve = Array.sub doubled i ((d i j) + 1) in
    let rcurve = Array.sub doubled j ((d j k) + 1) in
    let ocurve = Array.sub doubled k ((d k i) + 1) in 
      Abstract.imake_new_composition gram x.sid (y.sid,z.sid)
	{ prob = p;
	  cost = -. log p;
	  geom = Improper;
	  lcurve = lcurve;
	  rcurve = rcurve;
	  ocurve = ocurve;
	};
  in

  let make_prod shapes (i, j, k) p x (y, z) = 
    let	lcurve = Array.sub doubled i ((d i j) + 1) in
    let rcurve = Array.sub doubled j ((d j k) + 1) in
    let ocurve = Array.sub doubled k ((d k i) + 1) in 
    let geom = Parzen (N.make_model shapes smallsigma granularity 
			 (shapes.(0), smallsigma) 
			 Con.default_baseline_weight false)
    in
      Abstract.imake_new_composition gram x.sid (y.sid,z.sid)
	{
	  prob = p;
	  cost = -.log p;
	  geom = geom;
	  lcurve = lcurve;
	  rcurve = rcurve;
	  ocurve = ocurve;
	};
  in
    
  let ell = make_state 0.9999 (2,3) in

  let top = make_state 0.0 (0,7) in
  let top2 = make_state 0.0 (3,7) in
  let finger = make_state 0.0 (0,3) in    
  let finger2 = make_state 0.0 (1,3) in
  let bottom = make_state 0.0 (7,0) in
  let bottom2 = make_state 0.0 (11,0) in
  let bottomright = make_state 0.5 (7,11) in
  let thumb = make_state 0.0 (7,10) in
  let thumb2 = make_state 0.0 (8,10) in

  let thumb_shape = make_shape (7,8,10) in
  let theta = (Con.pi/. 8.0) in
  let thumb_shapes = [| thumb_shape; 
			rot_shape thumb_shape (C.polar 1.0 (-. theta));
			rot_shape thumb_shape (C.polar 1.0 (-. 2. *. theta));
			rot_shape thumb_shape (C.polar 1.0 theta);
			rot_shape thumb_shape (C.polar 1.0 (2. *. theta)); |] in

  let finger_shape1 = make_shape (0,1,3) in
  let finger_shape2 = make_shape (4,5,7) in
  let finger_shapes = [| finger_shape1; finger_shape2;
			 rot_shape finger_shape1 (C.polar 1.0 theta);
			 rot_shape finger_shape1 (C.polar 1.0 (-. theta));
			 rot_shape finger_shape2 (C.polar 1.0 theta);
			 rot_shape finger_shape2 (C.polar 1.0 (-. theta)); |] in

    make_cprod (0,7,0) 1.0 (Live.start gram) (top,bottom);
    make_prod [| make_shape (0,3,7) |] (0,3,7) 1.0 top (finger,top2);
    make_prod [| make_shape (3,4,7) |] (3,4,7) 1.0 top2 (ell, finger);
    make_prod [| make_shape (1,2,3); make_shape (5,6,7) |] (1,2,3) 1.0 finger2 (ell, ell);
    make_prod [| make_shape (7,11,0) |] (7,11,0) 1.0 bottom (bottomright, bottom2);
    make_prod [| make_shape (11,12,0) |] (11,12,0) 1.0 bottom2 (ell, ell);
    make_prod [| make_shape (7,10,11) |] (7,10,11) 0.5 bottomright (thumb, ell);
    make_prod [| make_shape (8,9,10) |] (8,9,10) 1.0 thumb2 (ell, ell);
    make_prod finger_shapes (0,1,3) 1.0 finger (ell, finger2);
    make_prod thumb_shapes (7,8,10) 1.0 thumb (ell, thumb2);
    
    Abstract.finalize gram
