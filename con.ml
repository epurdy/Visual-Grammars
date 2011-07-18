module C = Complex

let throttle = 40

let nclasses = 15
let ntrain = 2
let ntest = 10
let ntotal = 75

let nump = 40

let pi = 4.0 *. atan 1.0

let regweight = 1.0

let spfactor = 20.

(* let default_sigma = 0.00000001  *)
(* let default_sigma = 0.01 *)
let default_sigma = 0.05
(* let default_sigma = 0.1 *)
(* let default_sigma = 0.2 *)
(* let default_sigma = 0.3 *)
(* let default_sigma = 1.0 *)

(* let default_granularity = 20 *)
let default_granularity = 100 


(* let default_baseline_weight = 0.001 *)
let default_baseline_weight = 0.1
let default_baseline_mean = {C.re=0.5; C.im=0.0}
let default_baseline_sigma = 0.05
(* let default_baseline_sigma = 1.0 *)


let em_smoothing_nlog = log (100.)
