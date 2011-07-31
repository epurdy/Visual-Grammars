(** Abstract grammars *)

(** 
   It is >>> VERY IMPORTANT <<< that the data inside sdata and cdata
   be immutable. The abstract grammar code is written under this
   assumption, and violating it will cause shared reference bugs.

*)

(** Unique identifier for a symbol *)
type sid = int

(** Unique identifier for a composition *)
type cid = int

type 'sdata symbol = {
  mutable sid : sid;
  mutable dcompids : cid list;
  mutable lcompids : cid list;
  mutable rcompids : cid list;
  mutable sdata : 'sdata;
  mutable startable: bool;
}

type 'cdata composition = {
  mutable cid : cid;
  mutable topsid : sid;
  mutable leftsid : sid;
  mutable rightsid : sid;
  mutable cdata : 'cdata;
}

val copy_symbol : 'sdata symbol -> 'sdata symbol
val copy_composition : 'cdata composition -> 'cdata composition

val symbol_name : 'sdata symbol -> string
val composition_name : 'cdata composition -> string

(** remap sids and cids in place *)
val change_labels_symbol : 'sdata symbol -> (sid -> sid) -> (cid -> cid) -> unit
val change_labels_composition :
  'cdata composition -> (sid -> sid) -> (cid -> cid) -> unit

val map_labels_symbol :
  'sdata symbol -> (sid -> sid) -> (cid -> cid) -> 'sdata symbol
val map_labels_composition :
  'cdata composition -> (sid -> sid) -> (cid -> cid) -> 'cdata composition

(** {2 Live Grammars} *) 

type ('sdata, 'cdata, 'gdata) live_grammar

module Live :
  sig
    val get_gdata : ('sdata, 'cdata, 'gdata) live_grammar -> 'gdata
    val num_symbols : ('sdata, 'cdata, 'gdata) live_grammar -> int
    val num_compositions : ('sdata, 'cdata, 'gdata) live_grammar -> int
    val get_symbol : ('sdata, 'cdata, 'gdata) live_grammar -> sid -> 'sdata symbol
    val get_composition :
      ('sdata, 'cdata, 'gdata) live_grammar -> cid -> 'cdata composition
    val start : ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata symbol
    val iter_symbols :
      ('sdata, 'cdata, 'gdata) live_grammar -> ('sdata symbol -> unit) -> unit
    val iter_symbols_rev :
      ('sdata, 'cdata, 'gdata) live_grammar -> ('sdata symbol -> unit) -> unit
    val get_decompositions :
      ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata symbol -> 'cdata composition list
    val get_left_compositions :
      ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata symbol -> 'cdata composition list
    val get_right_compositions :
      ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata symbol -> 'cdata composition list
    val iter_decompositions :
      ('sdata, 'cdata, 'gdata) live_grammar ->
      'sdata symbol -> ('cdata composition -> unit) -> unit
    val iter_left_compositions :
      ('sdata, 'cdata, 'gdata) live_grammar ->
      'sdata symbol -> ('cdata composition -> unit) -> unit
    val iter_right_compositions :
      ('sdata, 'cdata, 'gdata) live_grammar ->
      'sdata symbol -> ('cdata composition -> unit) -> unit
    val get_lhs :
      ('sdata, 'cdata, 'gdata) live_grammar -> 'cdata composition -> 'sdata symbol
    val get_rhs :
      ('sdata, 'cdata, 'gdata) live_grammar ->
      'cdata composition -> 'sdata symbol * 'sdata symbol
    val validate_sym : ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata symbol -> unit
    val sample_path : ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata symbol list
    val print :
      ('sdata symbol -> string) -> ('sdata, 'cdata, 'gdata) live_grammar -> unit
  end
val new_live_grammar : ?nsyms:int -> ?ncomps:int -> 'gdata -> ('sdata, 'cdata, 'gdata) live_grammar

val validate_live : ('sdata, 'cdata, 'gdata) live_grammar -> unit
val make_new_symbol : ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata -> bool -> 'sdata symbol
val make_new_composition :
  ('sdata, 'cdata, 'gdata) live_grammar -> sid -> sid * sid -> 'cdata ->
  'cdata composition
val imake_new_symbol : ('sdata, 'cdata, 'gdata) live_grammar -> 'sdata -> bool -> unit
val imake_new_composition :
  ('sdata, 'cdata, 'gdata) live_grammar -> sid -> sid * sid -> 'cdata -> unit
type ('sdata, 'cdata, 'gdata) frozen_grammar = {
  f_gdata : 'gdata;
  f_symbols : 'sdata symbol array;
  f_compositions : 'cdata composition array;
}

(** {2 Frozen Grammars} *) 

module Frozen :
  sig
    val get_gdata : ('sdata, 'cdata, 'gdata) frozen_grammar -> 'gdata
    val num_symbols : ('sdata, 'cdata, 'gdata) frozen_grammar -> int
    val num_compositions : ('sdata, 'cdata, 'gdata) frozen_grammar -> int
    val get_symbol : ('sdata, 'cdata, 'gdata) frozen_grammar -> sid -> 'sdata symbol
    val get_composition :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> cid -> 'cdata composition
    val start : ('sdata, 'cdata, 'gdata) frozen_grammar -> 'sdata symbol
    val iter_symbols :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> ('sdata symbol -> unit) -> unit
    val iter_symbols_rev :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> ('sdata symbol -> unit) -> unit
    val get_decompositions :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> 'sdata symbol -> 'cdata composition list
    val get_left_compositions :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> 'sdata symbol -> 'cdata composition list
    val get_right_compositions :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> 'sdata symbol -> 'cdata composition list
    val iter_decompositions :
      ('sdata, 'cdata, 'gdata) frozen_grammar ->
      'sdata symbol -> ('cdata composition -> unit) -> unit
    val iter_left_compositions :
      ('sdata, 'cdata, 'gdata) frozen_grammar ->
      'sdata symbol -> ('cdata composition -> unit) -> unit
    val iter_right_compositions :
      ('sdata, 'cdata, 'gdata) frozen_grammar ->
      'sdata symbol -> ('cdata composition -> unit) -> unit
    val get_lhs :
      ('sdata, 'cdata, 'gdata) frozen_grammar -> 'cdata composition -> 'sdata symbol
    val get_rhs :
      ('sdata, 'cdata, 'gdata) frozen_grammar ->
      'cdata composition -> 'sdata symbol * 'sdata symbol
    val validate_sym : ('sdata, 'cdata, 'gdata) frozen_grammar -> 'sdata symbol -> unit
    val sample_path : ('sdata, 'cdata, 'gdata) frozen_grammar -> 'sdata symbol list
    val print :
      ('sdata symbol -> string) -> ('sdata, 'cdata, 'gdata) frozen_grammar -> unit
  end

val new_frozen_grammar :
  'gdata ->
  'sdata symbol array -> 'cdata composition array -> ('sdata, 'cdata, 'gdata) frozen_grammar

val validate_frozen : ('sdata, 'cdata, 'gdata) frozen_grammar -> unit

val iter_all_compositions :
  ('sdata, 'cdata, 'gdata) frozen_grammar -> ('cdata composition -> unit) -> unit

val iter_all_decompositions :
  ('sdata, 'cdata, 'gdata) frozen_grammar ->
  ('sdata symbol -> 'cdata composition -> unit) -> unit

(** {2 Various Useful Functions} *)

(** . *) 
val map_frozen_grammar :
  ('sdata, 'cdata, 'gdata) frozen_grammar ->
  ('gdata -> 'gdata2) ->
  ('sdata symbol -> 'sdata2 symbol) ->
  ('cdata composition -> 'cdata2 composition) -> 
  ('sdata2, 'cdata2, 'gdata2) frozen_grammar

val compactify : ('sdata, 'cdata, 'gdata) live_grammar -> unit

val finalize : ('sdata, 'cdata, 'gdata) live_grammar -> ('sdata, 'cdata, 'gdata) frozen_grammar

val enliven : ('sdata, 'cdata, 'gdata) frozen_grammar -> ('sdata, 'cdata, 'gdata) live_grammar

val merge_frozen_grams :
  ('sdata, 'cdata, 'gdata) frozen_grammar array -> 'gdata -> 
  ('sdata, 'cdata, 'gdata) frozen_grammar

val check_reachability :
  ('sdata, 'cdata, 'gdata) frozen_grammar -> bool array

val ensure_reachability :
  ('sdata, 'cdata, 'gdata) frozen_grammar ->
  ('sdata, 'cdata, 'gdata) frozen_grammar

val filter_compositions :
  ('sdata, 'cdata, 'gdata) frozen_grammar ->
  ('cdata composition -> bool) ->
  ('sdata, 'cdata, 'gdata) frozen_grammar
