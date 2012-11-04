open Util.Misc
open Util.Cops

module C = Complex

type gaussian = {
  mean: Geometry.cpt;
  s1: float;
  s2: float;
  rho: float;
}

type gaussian_curve = gaussian array

let default_gaussian () = {
  mean = c0; 
  s1 = 1.0;
  s2 = 1.0;
  rho = 0.0;
}

let default_distro () = [| default_gaussian () |]

let center_the_curve c = 
  let tot = ref c0 in
  let _ =
    Array.iter
      begin fun p ->
	tot := !tot +& p;
      end
      c;
  in
  let center = !tot /& (cxre (float_of_int (Array.length c))) in
    Array.map (fun p -> p -& center) c

let neglog_prob gc c =
  let c = center_the_curve c in
  let totcost = ref 0. in
    Array.iteri
      begin fun i p ->
	let cov_11 = gc.(i).s1 *. gc.(i).s1 in
	let cov_12 = gc.(i).rho *. gc.(i).s1 *. gc.(i).s2 in
	let cov_22 = gc.(i).s2 *. gc.(i).s2 in
	let det = cov_11 *. cov_22 -. cov_12 *. cov_12 in

	let icov_11 = cov_22 /. det in
	let icov_22 = cov_11 /. det in
	let icov_12 = -. cov_12 /. det in

	let diff = p -& gc.(i).mean in

	  totcost := !totcost +. 0.5 *. 
	    (icov_11 *. diff.C.re *. diff.C.re +.
	       2. *. icov_12 *. diff.C.re *. diff.C.im +.
	       icov_22 *. diff.C.im *. diff.C.im);
	  totcost := !totcost +. (log (2. *. pi)) +. (sqrt det)
      end
      c;
    !totcost

let prob gc c = 
  let nlp = neglog_prob gc c in
    exp (-. nlp)

(* See: http://stackoverflow.com/a/6553368 *)
let sample gc =
  Array.map
    begin fun g ->
      let x = Sample.gaussian 1.0 in
      let y = Sample.gaussian 1.0 in
	g.mean +&
	  {C.re = g.s1 *. x;
	   C.im = g.s2 *. (g.rho *. x +. (sqrt (1.0 -. g.rho *. g.rho)) *. y)}
    end
    gc

let sample_dec_variance gc factor =
  Array.map
    begin fun g ->
      let x = Sample.gaussian factor in
      let y = Sample.gaussian factor in
	g.mean +&
	  {C.re = g.s1 *. x;
	   C.im = g.s2 *. (g.rho *. x +. (sqrt (1.0 -. g.rho *. g.rho)) *. y)}
    end
    gc

let sample_no_variance gc =
  Array.map
    begin fun g ->
      g.mean
    end
    gc

let infer curves =
  let num_curves = Array.length curves in

  (* recenter each curve to have mean 0 *)
  let curves = Array.map center_the_curve curves in

  let models =
    Array.mapi
      begin fun i p ->
	let tot = ref c0 in
	let _ =
	  Array.iter
	    begin fun curve ->
	      tot := !tot +& curve.(i);
	    end
	    curves;
	in
	let mean = !tot /& (cxre (float_of_int num_curves)) in
	let tot_diff_11 = ref 0. in
	let tot_diff_12 = ref 0. in
	let tot_diff_22 = ref 0. in
	let _ =
	  Array.iter
	    begin fun curve ->
	      let diff = curve.(i) -& mean in
		tot_diff_11 := !tot_diff_11 +. diff.C.re *. diff.C.re;
		tot_diff_12 := !tot_diff_12 +. diff.C.re *. diff.C.im;
		tot_diff_22 := !tot_diff_22 +. diff.C.im *. diff.C.im;
	    end
	    curves;
	in

	let sigmax = sqrt !tot_diff_11 in
	let sigmay = sqrt !tot_diff_22 in
	let rho = !tot_diff_12 /. (sigmax *. sigmay) in
	  {
	    mean = mean; 
	    s1 = sigmax;
	    s2 = sigmay;
	    rho = rho;
	  }
      end
      curves.(0)
  in

    models, true

let gaussian_curve_distro_family = {
  Distro.default_distro = default_distro;
  Distro.build = (fun x -> x);
  Distro.unpack = (fun x -> x);
  Distro.sample = sample;
  Distro.infer = infer;
  Distro.prob = prob;
  Distro.neglog_prob = neglog_prob;
  Distro.show = (fun x -> failwith "show gaussian not implemented");
}
