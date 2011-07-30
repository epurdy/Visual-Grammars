(** Sparse decomposition families *)

open Abstract

(** {2 Types} *)

type sid = int
type cid = int
type index = int

(** Subcurve metadata *) 
type sdata = {
  first : index;
  last : index;
  len : int;
}

(** Composition metadata *) 
type cdata = {
  bg : index;
  md : index;
  en : index;
}

(** Global metadata *)
type gdata = { n : int; }

(* type ('a, 'b, 'c) family = ('a, 'b, 'c) frozen_grammar *)

type family = (sdata, cdata, gdata) frozen_grammar

(** {2 Functions} *)

val symbol_name_long : sdata symbol -> string

val is_closed : sdata symbol -> bool

val make_coarse_curves : int -> int -> int array array

val print_levels : int array array -> unit

val make_full_family :  int -> family
(* (\*  (sdata, cdata, sdf_gdata)  *\)family *)

val make_restricted_family : int -> (int * int) list -> (int * int * int) list ->
 family

val make_sparse_family : int ->  int ->(* (sdata, cdata, sdf_gdata) *) family

val load_family : string -> (* (sdata, cdata, sdf_gdata) *) family
