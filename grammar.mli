(** Shape grammars! *)

open Abstract

(** {2 Types} *)

type sdata = {
  closed : bool;
  straightprob : float;
  straightcost : float;
  curve : Curve.t;
}
type ssg_geom_data =
    Improper
  | Parzen of Parzen.model
  | Watson of Watson.distro
type cdata = {
  prob : float;
  cost : float;
  geom : ssg_geom_data;
  ocurve : Curve.t;
  lcurve : Curve.t;
  rcurve : Curve.t;
}
type gdata = unit
type grammar = (sdata, cdata, gdata) Abstract.frozen_grammar

(** {2 Printing} *)

val symbol_name_long : sdata Abstract.symbol -> string

val print_grammar : grammar -> unit

(** {2 Building grammars} *)
val grammar_of_family :
  ('a, 'b, 'c) frozen_grammar ->
  sdata ->
  ('a symbol -> sdata option) ->
  ('a symbol -> 'b composition -> cdata) ->
  grammar

(** {2 Manipulating grammars} *)

val renormalize : grammar -> sdata Abstract.symbol -> unit

val renormalize_binary : grammar -> sdata Abstract.symbol -> unit

val merge_symbols_left : grammar -> sdata symbol -> sdata symbol -> 
  renorm:bool -> unit

val replace_symbol : grammar -> sdata symbol -> sdata symbol -> unit

val merge_leaves : grammar -> grammar

val merge_tops : grammar array -> grammar

val prune : grammar -> float -> grammar

(** {2 Sampling from grammars} *)

val sampling_thresh : float

val sample_prod : cdata composition ->
  Geometry.cpt -> Geometry.cpt -> Geometry.cpt * Geometry.cpt * Geometry.cpt

val sample_state : grammar ->
  sdata symbol -> Geometry.cpt -> Geometry.cpt -> Geometry.cpt list

val sample : grammar ->
  Geometry.cpt -> Geometry.cpt -> Geometry.cpt array

val sample_counts : grammar ->
  (int * cid) list

