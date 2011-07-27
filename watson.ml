open Printf
open Geometry
open Util.Cops

module C = Complex

type watson_fit_data = {
  ssap: cpt * cpt * cpt * cpt;
  n: int;
  eval: float; 
  evec: cpt * cpt;
  mode: cpt * cpt * cpt;
  conc: float;
  cost: float;
}

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
let invsq3 = {C.re=sqrt (1./.3.); C.im=0.}
let invsq6 = {C.re=sqrt (1./.6.); C.im=0.}
let sq2_3  = {C.re=sqrt (2./.3.); C.im=0.}

let helmert (z0,z1,z2) = 
(*   let z0, z1, z2 = center_and_scale (z0, z1, z2) in *)
  (* this isn't SUPPOSED to matter, but it seems like it might *)
  let shape = Shape.shape_of_complex_bme z0 z1 z2 in
  let x = Shape.get_pt shape in
  let z0, z1, z2 = center_and_scale (c0, x, c1) in

    (invsq2 *& (z0 -& z1), invsq6 *& (z0 +& z1 -& z2 -& z2))

(* 
H (z0,z1,z2)t = (0,x1,x2)t 
(We assume that z0,z1,z2 is centered, hence the 0)
(z0, z1, z2) = Hinv (0,x1,x2)t
Hinv = (acc. to wolfram alpha)
"1/6 * 
(2 sqrt(3) | 3 sqrt(2)  | sqrt(6)
 2 sqrt(3) | -3 sqrt(2) | sqrt(6)
 2 sqrt(3) | 0          | -2 sqrt(6))" = 

 1/sqrt(3)   1/sqrt(2)  1/sqrt(6)
 1/sqrt(3)  -1/sqrt(2)  1/sqrt(6)
 1/sqrt(3)   0         -sqrt(2/3)
*)
let unhelmert (x1,x2) =
  let z1,z2,z3 = 
  (      invsq2 *& x1 +& invsq6 *& x2,
   c0 -& invsq2 *& x1 +& invsq6 *& x2,
   c0                 -& sq2_3  *& x2)
  in
(*     printf "helmert!\n"; *)
(*     print_cpt x1; *)
(*     print_cpt x2; *)
(*     printf "unhelmert!\n"; *)
(*     print_cpt z1; *)
(*     print_cpt z2; *)
(*     print_cpt z3; *)
    (z1,z2,z3)
  

let ssap triples = 
  let s11, s12, s21, s22 = ref c0, ref c0, ref c0, ref c0 in
    Array.iter 
      begin fun triple ->
	let z1,z2 = helmert triple in
	  s11 := !s11 +& (z1 *& (~& z1));
	  s12 := !s12 +& (z1 *& (~& z2));
	  s21 := !s21 +& (z2 *& (~& z1));
	  s22 := !s22 +& (z2 *& (~& z2));
      end
      triples;

    (!s11, !s12, !s21, !s22)

(*
  http://www.math.harvard.edu/archive/21b_fall_04/exhibits/2dmatrices/index.html
*)
let find_eval (s11, s12, s21, s22) =
  let det = s11 *& s22 -& s12 *& s21 in
  let tr = s11 +& s22 in
    (* det and trace are real, because eigenvalues are
       real for hermitian *)
  let tr, det = tr.C.re, det.C.re in
  let large_eval = 0.5 *. tr +. sqrt (0.25 *. tr *. tr -. det) in

  let large_evec = 
    let clarge_eval = {C.re=large_eval; C.im=0.} in
    if C.norm2 s21 > 0.01 then
      (clarge_eval -& s22, s21)
    else begin
      printf "hopefully s21 is large and this doesn't happen!\n";
      if C.norm2 s12 > 0.01 then 
	(s12, clarge_eval -& s11)
      else begin 
	printf "hopefully s12 is large and this doesn't happen!\n";
	(c1, c0)
      end
    end
  in
    large_eval, large_evec
    
let mle_conc eval n = 
  let num = float_of_int n in
  let conc = 
    if eval >= 0.999 *. num then
      1000.
    else
      num /. (num -. eval) 
  in
    conc

let fit_watson triples = 
  let smat = ssap triples in
  let n = Array.length triples in
  let eval, evec = find_eval smat in
  let conc = mle_conc eval n in
    
  let cost = -. (conc *. eval +. 
		   (float_of_int n) *. (nl_watson_const conc)) in

  let mode = unhelmert evec in

    {ssap = smat;
     n = n;
     eval = eval;
     evec = evec;
     mode = mode;
     conc = conc;
     cost = cost;
    }

