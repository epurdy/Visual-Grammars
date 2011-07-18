exception Parse_failure


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

val inside : ('a,'b,'c) Grammar.grammar -> Sdf.debug_family -> 
  sparse_inside_table

val sparse_inside_outside : ('a,'b,'c) Grammar.grammar -> Sdf.debug_family -> 
  Sdf.sdf_debug_cdata sparse_counts_table -> unit

val sparse_parse_cost : ('a,'b,'c) Grammar.grammar -> Sdf.debug_family -> float

