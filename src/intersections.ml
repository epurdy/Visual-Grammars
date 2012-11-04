open Util.Misc
open Distro


let count_self_intersections curve =
  let nintersect = ref 0 in
  let n = Array.length curve in
    for i = 0 to n-1 do
      for j = i+2 to n-1 do
	let i', j' = (i+1) mod n, (j+1) mod n in
	let flag = Geometry.intersect (curve.(i), curve.(i')) (curve.(j), curve.(j')) in
	  if flag then begin 
	    incr nintersect;
	  end
      done
    done;

    !nintersect

type intersection_thing = Curve.t

type intersection_distro = {
  nlogp: float;
  nlogp': float;
}

type intersection_build_data = float
type intersection_inference_input = int array
type intersection_inference_output = unit

let neglog_prob distro curve =
  let nintersect = count_self_intersections curve in
    ((float_of_int nintersect) *. distro.nlogp') +. distro.nlogp

let prob distro curve =
  let nlp = neglog_prob distro curve in
    exp (-. nlp)

let build_distro p = {
  nlogp = -. log p; 
  nlogp' = -. log (1. -. p);
}

let unpack_distro d = 
  exp (-. d.nlogp)

let infer_distro nums =
  let sum = 
    let sum = ref 1 in (* 1 instead of 0 to simulate beta prior(?) *)
      Array.iter
	begin fun k ->
	  sum := !sum + k;
	end
	nums;
      !sum
  in
  let pinv = 1.0 +. ((float_of_int sum) /. (float_of_int (Array.length nums))) in
    build_distro (1.0 /. pinv), ()
    

let default_distro () = build_distro 0.1

let intersection_distro_family = {
  default_distro = default_distro;
  build = build_distro;
  unpack = unpack_distro;
  sample = (fun x -> [| |]);
  infer = infer_distro;
  prob = prob;
  neglog_prob = neglog_prob;
  show = (fun d s -> [ "intersections" ]);
}
