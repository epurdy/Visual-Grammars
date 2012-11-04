open Printf
open Geometry
open Util.Cops
open Util.Misc

module C = Complex
module D = Distro

type watson_distro = {
  mean: Shape.shape;
  conc_ : float;
}

type watson_build_data = {
  mean_shape: Shape.shape;
  concentration: float;
}

type watson_inference_input = {
  samples: (float * Shape.shape) array;
  gamma_shape_param: float;
  gamma_mean_param: float;
}

type watson_inference_output = {
  ssap: cpt * cpt * cpt * cpt;
  totwt: float;
  eval: float; 
  evec: cpt * cpt;
  mode: cpt * cpt * cpt;
  conc: float;
  cost: float;
}

let default_watson_distro = {
  mean = Shape.shape_of_complex_bme c0 (cxre 0.5) c1;
  conc_ = 1000.;
}

(******************************************************)
(* Building and unpacking watson distribution         *)
(******************************************************)
let build build_data = {
  mean = build_data.mean_shape;
  conc_ = build_data.concentration;
}

let unpack distro = {
  mean_shape = distro.mean;
  concentration = distro.conc_;
}

(******************************************************)
(* Evaluating watson distributions at a point         *)
(******************************************************)

(* The negative log of the normalizing constant.
   
   See Mardia and Dryden

   c(\sigma) = \frac{\sigma}{2\pi^2 e^{\sigma}} *)
let nl_watson_const sigma =
  log 2. +. 2. *. log pi -. log sigma +.
    sigma

(* negative log of probability *)
let cost distro z =
  let z, mean = Shape.get_pt z, Shape.get_pt distro.mean in
  let canon a = center_and_scale (c0, a, c1) in
  let z0,z1,z2 = canon z in
  let m0,m1,m2 = canon mean in
  let dprod = (~& z0) *& m0 +& (~& z1) *& m1 +& (~& z2) *& m2 in
    (nl_watson_const distro.conc_) -. distro.conc_ *. (C.norm2 dprod)
  
let prob distro z = 
  let cost = cost distro z in
    exp (-. cost)

(******************************************************)
(* Sampling from watson distributions                 *)
(******************************************************)

(* metropolis-hastings with an adaptive gaussian step. note that the
   proposal distro is symmetric.
   
   We choose the initial sigma slightly intelligently based on
   Mardia and Dryden, Result 6.7.

   We can do this ~ 500x/sec on altair, so that's reasonably
   efficient.
*)
let sample distro =
  let z = ref distro.mean in
  let oldcost = ref (cost distro !z) in
  let sigma = ref (0.5 /. distro.conc_) in
  let dsigma = 0.1 *. !sigma in
  let step sigma = {
    C.re = Sample.gaussian sigma;
    C.im = Sample.gaussian sigma;}
  in

  let window = 30 in
  (* let thresh = 5 in *)
  let thresh = 15 in

  let i = ref 0 in
  let acceptance = ref (Array.init window (fun x -> true)) in
  let shift newv = 
    acceptance := Array.init window
      begin fun i -> 
	if i>0 then !acceptance.(i-1)
	else newv
      end
  in

  let update_sigma () = 
    let nacc = Array.fold_right (fun x tot -> if x then tot+1 else tot) 
      !acceptance 0 in
      if nacc < thresh then (* (window/2) then *)
	sigma := max dsigma (!sigma -. dsigma);
      if nacc > (window-thresh) then (* (window/2) then *)
	sigma := !sigma +. dsigma;
  in

    while !i < 1000 do
      let zp = Shape.get_pt !z in
      let zp' = zp +& (step !sigma) in
      let z' = Shape.shape_of_complex_bme c0 zp' c1 in
      let thecost = cost distro z' in
      let thresh = exp (-. (thecost -. !oldcost)) in
      let u = Random.float 1.0 in

	if u < thresh then begin 
	  z := z';
	  oldcost := thecost;
	  shift true;
	end
	else begin 
	  shift false;
	end;
	if !i > window then begin
	  update_sigma ();
	end;
	incr i
    done;

    !z

(******************************************************)
(* Inferring watson distributions from data           *)
(******************************************************)

let invsq2 = {C.re=sqrt 0.5; C.im=0.}
let invsq3 = {C.re=sqrt (1./.3.); C.im=0.}
let invsq6 = {C.re=sqrt (1./.6.); C.im=0.}
let sq2_3  = {C.re=sqrt (2./.3.); C.im=0.}

let helmert (z0,z1,z2) = 
  (* canonicalize the triple *)
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
  

(* Sum of squares and products:

   A = \sum_{i} \alpha_i v_i v_i^*,

   where ^* denotes the conjugate transpose and \alpha_i is the
   weight of the vector v_i

*)
(* let ssap_unweighted triples =  *)
(*   let s11, s12, s21, s22 = ref c0, ref c0, ref c0, ref c0 in *)
(*     Array.iter  *)
(*       begin fun triple -> *)
(* 	let z1,z2 = helmert triple in *)
(* 	  s11 := !s11 +& (z1 *& (~& z1)); *)
(* 	  s12 := !s12 +& (z1 *& (~& z2)); *)
(* 	  s21 := !s21 +& (z2 *& (~& z1)); *)
(* 	  s22 := !s22 +& (z2 *& (~& z2)); *)
(*       end *)
(*       triples; *)

(*     (!s11, !s12, !s21, !s22) *)

let ssap_neglog_weighted nlwt_shapes =
  let s11, s12, s21, s22 = ref c0, ref c0, ref c0, ref c0 in
    Array.iter 
      begin fun (nlwt,shape) ->
	let wt = cxre (exp (-. nlwt)) in
	let z1,z2 = helmert (c0,Shape.get_pt shape,c1) in
	  s11 := !s11 +& wt *& (z1 *& (~& z1));
	  s12 := !s12 +& wt *& (z1 *& (~& z2));
	  s21 := !s21 +& wt *& (z2 *& (~& z1));
	  s22 := !s22 +& wt *& (z2 *& (~& z2));
      end
      nlwt_shapes;
    (!s11, !s12, !s21, !s22)  

let ssap_unweighted shapes =
  let nlwt_shapes = Array.map (fun x -> (0.0, x)) shapes in
    ssap_neglog_weighted nlwt_shapes


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

let mle_conc eval fn gamma_shape_param gamma_rate_param = 
  let min_conc = 0.1 in
  let max_conc = 100. *. 100. *. 100. in
  let conc = (gamma_shape_param +. fn) /. (gamma_rate_param +. fn -. eval) in
    if conc < min_conc then
      min_conc
    else
      if conc > max_conc then
	max_conc
      else
	conc
  (* let conc =  *)
  (*   if eval >= 0.999 *. fn then *)
  (*     1000.  *)
  (*   else *)
  (*     fn /. (fn -. eval)  *)
  (* in *)
  (*   conc *)

let fit_neglog_weighted indata =
  let nlwt_shapes = indata.samples in
  let gamma_rate_param = indata.gamma_shape_param /. indata.gamma_mean_param in

  let smat = ssap_neglog_weighted nlwt_shapes in
  let neglog_fn = Array.fold_left (fun nltot (nlwt,_) -> neglogadd nlwt nltot) 
    infinity nlwt_shapes in
  let eval, evec = find_eval smat in
  let conc = mle_conc eval (exp (-.neglog_fn)) 
    indata.gamma_shape_param gamma_rate_param 
  in

  let _ = printf "watson conc=%f\n%!" conc in
    
  let mode = unhelmert evec in
  let z0,z1,z2 = mode in 
  let mean = Shape.shape_of_complex_bme z0 z1 z2 in

  let cost = -. (conc *. eval +. 
		   (neglog_fn) *. (nl_watson_const conc)) in

    ({
       conc_ = conc;
       mean = mean
     },
     {
       ssap = smat;
       totwt = neglog_fn;
       eval = eval;
       evec = evec;
       mode = mode;
       conc = conc;
       cost = cost;
     })

(* let fit_watson triples =  *)
(*   assert(Array.length triples > 0); *)
(*   let n = Array.length triples in *)

(*   let shapes = Array.map (fun x -> (0.0, x)) triples in *)

(*     fit_neglog_weighted shapes *)

(*   (\* let smat = ssap_unweighted triples in *\) *)
(*   (\* let fn = float_of_int n in *\) *)
(*   (\* let eval, evec = find_eval smat in *\) *)
(*   (\* let conc = mle_conc eval fn in *\) *)
    
(*   (\* let cost = -. (conc *. eval +.  *\) *)
(*   (\* 		   (float_of_int n) *. (nl_watson_const conc)) in *\) *)

(*   (\* let mode = unhelmert evec in *\) *)

(*     (\* {ssap = smat; *\) *)
(*     (\*  totwt = fn; *\) *)
(*     (\*  eval = eval; *\) *)
(*     (\*  evec = evec; *\) *)
(*     (\*  mode = mode; *\) *)
(*     (\*  conc = conc; *\) *)
(*     (\*  cost = cost; *\) *)
(*     (\* } *\) *)

let scale = 100.

let save_triangle shape fname = 
  let p = Shape.get_pt shape in
  let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in
    Curve.save fname (Curve.flip_xy curve)

let show_watson dist prefix =
  let curvename = sprintf "%s.curve" prefix in
  let svgname = sprintf "%s.svg" prefix in
  let picstring = 
    sprintf "\\includegraphics[width=0.6in]{%s.png}" prefix
  in
  let concstring = sprintf "%0.2f" dist.conc_ in
    save_triangle dist.mean curvename;
    doit (sprintf "./show_curve.native %s.curve %s.svg" prefix prefix);

    [picstring; concstring]

let watson_distro_family = {
  D.default_distro = (fun () -> default_watson_distro);
  D.build = (fun build_data -> build build_data);
  D.unpack = (fun distro -> unpack distro);
  D.sample = (fun distro -> sample distro);
  D.infer = (fun things -> fit_neglog_weighted things);
  D.prob = (fun distro thing -> prob distro thing);
  D.neglog_prob = (fun distro thing -> cost distro thing);
  D.show = (fun dist prefix -> show_watson dist prefix);
}
