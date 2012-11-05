exception Parse_failure

open Abstract


type ('mod_sym,'mod_comp,'tgt_sym,'tgt_comp,'tgt_glob) strategy = {
  lexical_ok :  'tgt_sym symbol -> bool;
  lexical_cost : 'mod_sym symbol -> 'tgt_sym symbol -> float;
  binary_ok : 'tgt_sym symbol -> bool;
  binary_cost : 'mod_comp composition -> 'tgt_comp composition -> float;
  goal_ok : 'tgt_sym symbol -> bool;
  goal_cost : 'tgt_glob -> 'tgt_sym symbol -> float;
  compatible : 'mod_sym symbol -> 'tgt_sym symbol -> bool;
  getshape : 'tgt_comp -> Shape.shape;
  fit_midpoint_distro : 'mod_comp composition -> (float * Shape.shape) list -> Shape.shape option ->
				       float -> float ->
				       'mod_comp;
  tgt_sym_namer : 'tgt_sym symbol -> string;

}

type parse_table = {
  na: int;
  nb: int;
  data: float array;  
  name: string;
  verbose: bool;
}

type sparse_inside_table = {
  qual : float;
  inside : parse_table;
}

val new_parse_table : int -> int -> string -> bool -> parse_table
val get : parse_table -> int *int-> float
val set : parse_table -> int *int-> float -> unit

type 'a sparse_counts_table = {
  mutable qual__ : float;
  lexical_counts : float array;
  binary_counts : float array;
  mpchoices : (float * 'a) list array;
  mpmodes : (float * 'a option) array;
}
val make_soft_counts : int -> int -> 'a sparse_counts_table

val combine_soft_counts : 'a sparse_counts_table -> 'a sparse_counts_table -> 'a sparse_counts_table

val inside : 
  ('mod_sym,'mod_comp,'mod_glob) frozen_grammar -> 
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar -> 
  ('mod_sym,'mod_comp,'tgt_sym,'tgt_comp,'tgt_glob) strategy ->
  sparse_inside_table

val sparse_inside_outside : 
 ('mod_sym,'mod_comp,'mod_glob) frozen_grammar -> 
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar -> 
  Shape.shape sparse_counts_table ->
  ('mod_sym,'mod_comp,'tgt_sym,'tgt_comp,'tgt_glob) strategy ->
  unit

val sparse_parse_cost : 
  ('mod_sym,'mod_comp,'mod_glob) frozen_grammar -> 
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar -> 
  ('mod_sym,'mod_comp,'tgt_sym,'tgt_comp,'tgt_glob) strategy ->
  float

val viterbi : 
  ('mod_sym,'mod_comp,'mod_glob) frozen_grammar -> 
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar -> 
  ('mod_sym,'mod_comp,'tgt_sym,'tgt_comp,'tgt_glob) strategy ->
  float * (int * int * int option * int option) list
