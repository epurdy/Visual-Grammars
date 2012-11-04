
(** correlated_repetition gram ncopies nrulecopies *)
type correlated_repetition = {
  ncopies: int;
  nrulecopies: int;
  name: string;
}

val correlated_repetition_heuristic : (correlated_repetition, correlated_repetition, correlated_repetition) 
  Xform.xform_heuristic
