
(** correlated_repetition gram ncopies nrulecopies *)
type merge_replace_build = {
  families: Sdf.family array;  
}

type merge_replace_internal

type merge_replace_xform = 
    Merge of int * int
  | Replace of int * int

val merge_replace_heuristic : (merge_replace_build, merge_replace_internal, merge_replace_xform) 
  Xform.xform_heuristic
