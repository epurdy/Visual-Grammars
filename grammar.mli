(** Shape grammars! *)

type sid = int
type cid = int
type 'a symbol = {
  mutable sdata : 'a;
  mutable closed : bool;
  mutable straightprob : float;
  mutable straightcost : float;
  mutable sid : sid;
  mutable dcompids : cid list;
  mutable lcompids : cid list;
  mutable rcompids : cid list;
}
type ssg_geom_data = Improper | Parzen of Parzen.model
type 'a composition = {
  mutable cdata : 'a;
  mutable prob : float;
  mutable cost : float;
  mutable geom : ssg_geom_data;
  mutable cid : cid;
  mutable topsid : sid;
  mutable leftsid : sid;
  mutable rightsid : sid;
}
type ('a,'b,'c) grammar = ('a symbol, 'b composition, 'c) Abstract.frozen_grammar

val copy_symbol' : 'a symbol -> 'b -> 'b symbol
val copy_comp' : 'a composition -> 'b -> 'b composition
val copy_symbol : 'a symbol -> 'a symbol
val copy_comp : 'a composition -> 'a composition
val symbol_name : 'a symbol -> string
val composition_name : 'a composition -> string
val lexical_ok : 'a symbol -> bool
val binary_ok : 'a -> bool
val goal_ok : 'a symbol -> bool

val lexical_cost : 'a symbol -> 'b -> float
val prod_cost : 'a composition -> Shape.shape -> float
val binary_cost : 'a composition -> Sdf.sdf_debug_cdata Sdf.composition -> float
val compatible : 'a symbol -> 'b Sdf.symbol -> bool

type ('a, 'b, 'c) live_shape_grammar = {
  gram : ('a symbol, 'b composition, 'c) Abstract.live_grammar;
  make_new_state : bool -> float -> 'a -> 'a symbol;
  make_new_prod : sid -> sid * sid -> float -> ssg_geom_data -> 'b -> 'b composition;
  imake_new_state : bool -> float -> 'a -> unit;
  imake_new_prod : sid -> sid * sid -> float -> ssg_geom_data -> 'b -> unit;
  start_: 'a symbol;
}
val make_live_shape_grammar :  'a -> 'a -> 'b -> ('a, 'b, unit) live_shape_grammar

val grammar_of_family : ?spfactor:float -> Sdf.debug_family -> 
  (Sdf.sdf_debug_sdata, Sdf.sdf_debug_cdata, unit) grammar

val marshal : ('a,'b,'c) grammar -> string -> unit
val unmarshal : 'a -> 'b -> string -> ('a,'b,'c) grammar

val merge_tops : ('a,'b,'c) grammar array -> ('a,'b,'c) grammar

val merge_leaves : ('a,'b,'c) grammar -> ('a,'b,'c) grammar

val merge_symbols_left : ('a,'b,'c) grammar -> 'a symbol -> 'a symbol -> renorm:bool -> unit

val replace_symbol : ('a,'b,'c) grammar -> 'a symbol -> 'a symbol -> unit

val sample : ('a, 'b, 'c) grammar -> Complex.t -> Complex.t -> Curve.t

val print_grammar : ('a, 'b, 'c) grammar -> unit

val draw_grammar_best_rules :
  (int -> int -> cid -> string) -> ('a,Sdf.sdf_debug_cdata,'c) grammar -> unit

val draw_grammar : (int -> string) -> ('a, Sdf.sdf_debug_cdata,'c) grammar -> unit

val finalize: ('a,'b,'c) live_shape_grammar -> ('a,'b,'c) grammar

val prune :  ('a,'b,'c) grammar -> float -> ('a,'b,'c) grammar
