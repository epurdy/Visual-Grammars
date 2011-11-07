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

type family = (sdata, cdata, gdata) frozen_grammar

(** {2 Functions} *)

val symbol_name_long : sdata symbol -> string

val is_closed : sdata symbol -> bool

val make_coarse_curves : int -> int -> int array array

val print_levels : int array array -> unit

val make_full_family :  int -> family

val make_restricted_family : int -> (int * int) list -> (int * int * int) list ->
 family

val bottom_out_family : family -> family

val make_sparse_family : int ->  int -> family

type live_family = {
  gram: (sdata,cdata,gdata) live_grammar;
  sclookup: (index*index,sdata symbol) Hashtbl.t;
  make_new_scurve: int * int -> int -> sdata symbol;
  make_new_comp: int * int * int -> cdata composition;
  imake_new_scurve: int * int -> int -> unit;
  imake_new_comp: int * int * int -> unit;
}    

val make_live_family : int -> int -> live_family

val load_family : string -> family

val save_family : family -> string -> unit 
