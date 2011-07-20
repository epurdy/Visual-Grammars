(** Sparse decomposition families *)

(** {2 Types} *)

type sid = int
type cid = int
type index = int

(** The type of a subcurve, parameterized by metadata ['a]. *) 
type 'a symbol = {
  mutable sdata : 'a;
  first : index;
  last : index;
  len : int;
  mutable sid : sid;
  mutable dcompids : cid list;
  mutable lcompids : cid list;
  mutable rcompids : cid list;
}

(** The type of a composition, parameterized by metadata ['b]. *) 
type 'b composition = {
  mutable cdata : 'b;
  bg : index;
  md : index;
  en : index;
  mutable cid : cid;
  mutable topsid : sid;
  mutable leftsid : sid;
  mutable rightsid : sid;
}

(** {2 Normal metadata} *)
type sdf_gdata = { n : int; }
type sdf_sdata = { scale : float; }
type sdf_cdata = { shape : Shape.shape; }

(** {2 Debugging metadata} *)
type sdf_debug_sdata = { dscale : float; curve : Curve.t; }
type sdf_debug_cdata = {
  dshape : Shape.shape;
  lcurve : Curve.t;
  rcurve : Curve.t;
  ocurve : Curve.t;
}
type sdf_debug_gdata = { nn : int; thecurve : Curve.t; }
val ex_debug_sdata : sdf_debug_sdata
val ex_debug_cdata : sdf_debug_cdata


(** {2 The type of an SDF} *)
(* type ('a, 'b, 'c) live_family = { *)
(*   gram : ('a symbol, 'b composition, 'c) Abstract.live_grammar; *)
(*   sclookup : (index * index, 'a symbol) Util.Hops.hash; *)
(*   make_new_scurve : int * int -> int -> 'a -> 'a symbol; *)
(*   make_new_comp : int * int * int -> 'b -> 'b composition; *)
(* } *)
type ('a, 'b, 'c) family = ('a symbol, 'b composition, 'c) Abstract.frozen_grammar
type debug_family = (sdf_debug_sdata, sdf_debug_cdata, sdf_debug_gdata) family
type unit_family = (unit, unit, sdf_gdata) family
type normal_family = (sdf_sdata, sdf_cdata, sdf_gdata) family

val marshal : ('a,'b,'c) family -> string -> unit
val unmarshal : 'a -> 'b -> string -> ('a,'b,'c) family

val copy_symbol' : 'a symbol -> 'b -> 'b symbol
val copy_comp' : 'a composition -> 'b -> 'b composition
val copy_symbol : 'a symbol -> 'a symbol
val copy_comp : 'a composition -> 'a composition
val is_closed : 'a symbol -> bool
val symbol_name : 'a symbol -> string
val composition_name : 'a composition -> string
val lexical_ok : 'a symbol -> bool
val binary_ok : 'a symbol -> bool
val goal_ok : 'a symbol -> bool

val goal_cost : ('a, 'b, sdf_debug_gdata, 'c) Abstract.grammar -> 'd -> float
val add_curve_data_to_family : Curve.t -> ('a, 'b, sdf_gdata) family -> normal_family
val add_curve_data_to_family_debug : Curve.t ->  ('a, 'b, sdf_gdata) family -> debug_family
val make_coarse_curves : int -> int -> int array array
val print_levels : int array array -> unit
val make_full_family :  int -> unit_family
val make_restricted_family : int -> (int * int) list -> (int * int * int) list -> unit_family
val make_sparse_family : int ->  int -> unit_family
val sparse_family_of_curve : Curve.t -> int -> normal_family
val sparse_family_of_curve_debug : Curve.t -> int -> debug_family
val full_family_of_curve : Curve.t -> normal_family
val full_family_of_curve_debug : Curve.t -> debug_family

val load_family : string -> unit_family
