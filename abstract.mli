open Hops
open Util
open Printf

type sid = int
(** Unique identifier for a symbol *)

type cid = int
(** Unique identifier for a composition *)

type ('symbol,'composition) friend = {
  __sym__: 'symbol;
  __comp__: 'composition;

  sid_: 'symbol -> int; (* read sid *)
  _sid: 'symbol -> int -> unit; (* set sid *)
  dcompids_: 'symbol -> int list;
  _dcompids: 'symbol -> int list -> unit;
  lcompids_: 'symbol -> int list;
  _lcompids: 'symbol -> int list -> unit;
  rcompids_: 'symbol -> int list;
  _rcompids: 'symbol -> int list -> unit;

  cid_: 'composition -> int;
  _cid: 'composition -> int -> unit;
  topsid_: 'composition -> int;
  _topsid: 'composition -> int -> unit;
  leftsid_: 'composition -> int;
  _leftsid: 'composition -> int -> unit;
  rightsid_: 'composition -> int;
  _rightsid: 'composition -> int -> unit;

  copy_symbol: 'symbol -> 'symbol;
  copy_comp: 'composition -> 'composition;
  
  symbol_name: 'symbol -> string;
  symbol_name_long: 'symbol -> string;
  composition_name: 'composition -> string;

  lexical_ok: 'symbol -> bool;
  binary_ok: 'symbol -> bool;
  goal_ok: 'symbol -> bool;
}

(* val change_labels_symbol : ('symbol,'composition) friend -> 'symbol -> (int,int) hash -> (int,int) hash -> unit *)

(* val change_labels_composition : ('symbol,'composition) friend -> *)
(*   'composition ->  *)
(*   (int,int) hash -> (int,int) hash -> unit *)

type ('symbol,'composition,'c,'self) grammar = {
  mutable self: 'self;
  mutable x: ('symbol, 'composition) friend;
  mutable get_gdata: unit -> 'c;
  mutable num_symbols : unit -> int;
  mutable num_compositions : unit -> int;
  mutable get_symbol : sid -> 'symbol;
  mutable get_composition : cid -> 'composition;
  mutable get_lhs : 'composition -> 'symbol;
  mutable get_rhs : 'composition -> 'symbol * 'symbol;
  mutable get_decompositions : 'symbol -> 'composition list;
  mutable iter_symbols : ('symbol -> unit) -> unit;
  mutable iter_symbols_rev : ('symbol -> unit) -> unit;
  mutable iter_decompositions : 'symbol -> ('composition -> unit) -> unit;
  mutable iter_left_compositions : 'symbol -> ('composition -> unit) -> unit;
  mutable iter_right_compositions : 'symbol -> ('composition -> unit) -> unit;
  mutable iter_all_compositions : ('composition -> unit) -> unit;
  mutable iter_all_decompositions : ('symbol -> 'composition -> unit) -> unit;
  mutable start: unit -> 'symbol;
}

val sample_path : ('symbol,'composition,'c,'self) grammar -> 'symbol list
val print : ('symbol,'composition,'c,'self) grammar -> unit

type ('symbol,'composition,'c) frozen_grammar_data = {
  f_gdata: 'c;
  f_symbols: 'symbol array;
  f_compositions: 'composition array;
}
type ('symbol,'composition,'c) frozen_grammar = 
    ('symbol,'composition,'c, ('symbol,'composition,'c) frozen_grammar_data) grammar

val new_frozen_grammar : ('symbol,'composition) friend -> 'c -> 'symbol array ->
  'composition array -> ('symbol,'composition,'c) frozen_grammar

val marshal_frozen_grammar :
('symbol,'composition,'c) frozen_grammar -> string -> unit

val unmarshal_frozen_grammar :
('symbol,'composition) friend -> string ->
('symbol,'composition,'c) frozen_grammar

val map_frozen_grammar : 
  ('symbol2, 'composition2) friend ->
  ('symbol,'composition,'c) frozen_grammar ->
  ('c -> 'c2) -> ('symbol -> 'symbol2) ->
  ('composition -> 'composition2) ->
  ('symbol2,'composition2,'c2) frozen_grammar

val merge_frozen_grams : ('symbol, 'composition, 'c) frozen_grammar array ->
    ('symbol, 'composition, 'c) frozen_grammar


type ('symbol,'composition,'c) live_grammar_data = {
  mutable l_gdata: 'c;
  mutable l_symbols: (sid, 'symbol) hash;
  mutable l_compositions: (cid, 'composition) hash;
  mutable next_sid: int;
  mutable next_cid: int;
  mutable insert_symbol : 'symbol -> 'symbol;
  mutable insert_composition :
    'composition -> 'composition;
}
type ('symbol,'composition,'c) live_grammar = 
    ('symbol,'composition,'c, ('symbol,'composition,'c) live_grammar_data) grammar

val new_live_grammar : ('symbol,'composition) friend -> 'c -> 'symbol -> 'composition -> int -> int -> 
  ('symbol,'composition,'c) live_grammar

val finalize : ('symbol,'composition,'c) live_grammar ->
  ('symbol,'composition,'c) frozen_grammar 

val compactify : ('symbol,'composition,'c) live_grammar -> unit

val check_reachability : ('symbol,'composition,'c) frozen_grammar ->
  bool array

val ensure_reachability : ('symbol,'composition,'c) frozen_grammar ->
  ('symbol,'composition,'c) frozen_grammar

val filter_compositions : ('symbol,'composition,'c) frozen_grammar ->
  ('composition -> bool) ->
  ('symbol,'composition,'c) frozen_grammar
