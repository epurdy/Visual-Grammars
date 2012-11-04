open Util.Misc
open Printf

(** correlated_repetition gram ncopies nrulecopies *)
type init_build = {
  excurve: Curve.t;
  family: Sdf.family;
  name: string;
}

val init_heuristic : (init_build, init_build, init_build) Xform.xform_heuristic
