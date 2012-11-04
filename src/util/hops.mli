(** Operator syntax for hash tables *)

type ('a,'b) hash = ('a,'b) Hashtbl.t

val keys : ('a,'b) hash -> 'a list

(** [mkhash n] creates a hash table that is optimized to contain
    [n] elements. *)
val mkhash : int -> ('a, 'b) Hashtbl.t

(* (\** [tbl = !>> n] creates a hash table that is optimized to contain *)
(*     [n] elements. Equivalent to Hashtbl.create. *\) *)
(* val ( !>> ) : int -> ('a, 'b) Hashtbl.t *)

(** [tbl >> k] returns the current binding of [k] in [tbl], or raises
    Not_found. Equivalent to Hashtbl.find. *)
val ( >> ) : ('a, 'b) Hashtbl.t -> 'a -> 'b

(** [tbl >>! (k,d)] returns the current binding of [k] in [tbl], or
    [d] if [k] is not bound in [tbl]. *)
val ( >>! ) : ('a,'b) Hashtbl.t -> 'a * 'b -> 'b

(** [tbl >>> k] returns all bindings of [k] in [tbl], in reverse
    order of introduction. Equivalent to Hashtbl.find_all. *)
val ( >>> ) : ('a, 'b) Hashtbl.t -> 'a -> 'b list

(** [tbl >>? k] checks if [k] is bound in [tbl]. Equivalent to
    Hashtbl.mem. *)
val ( >>? ) : ('a, 'b) Hashtbl.t -> 'a -> bool


(** [tbl << (k,v)] replaces the current binding of [k] to
    [v]. Equivalent to Hashtbl.replace. *)
val ( << ) : ('a, 'b) Hashtbl.t -> 'a * 'b -> unit

(** [tbl <<+ (k,v)] adds a binding of [k] to [v]. Equivalent to
    Hashtbl.add. *)
val ( <<+ ) : ('a, 'b) Hashtbl.t -> 'a * 'b -> unit

(** [tbl <<- k] removes the current binding of [k], restoring the
    previous one if it exists. Equivalent to Hashtbl.remove. *)
val ( <<- ) : ('a, 'b) Hashtbl.t -> 'a -> unit

(** [!<< tbl] deletes all bindings in [tbl]. Equivalent to
    Hashtbl.clear. *)
val ( !<< ) : ('a, 'b) Hashtbl.t -> unit

(** [tbl <<: f] iterates over [tbl], applying [f] to keys and
    values. Equivalent to Hashtbl.iter. *)
val ( <<: ) : ('a, 'b) Hashtbl.t -> ('a -> 'b -> unit) -> unit

(** [~<<>> tbl f id] computes [(f k0 v0 (f k1 v1 (f k2 v2 (... (f kn
    vn id)...))))], where [k0, v0, ..., kn, vn] are the bindings of [tbl],
    and [id] is an identity-like value. Equivalent to Hashtbl.fold.  *)
val ( ~<<>> ) : ('a, 'b) Hashtbl.t -> ('a -> 'b -> 'c -> 'c) -> 'c -> 'c


(** {2 Functorial interface} *)
(** For use when we want to override the default hashing
    behavior. Otherwise exactly equivalent to the polymorphic
    interface. *)

module Make : functor (M : Hashtbl.HashedType) ->
sig
  type key = M.t
  type 'a t = 'a Hashtbl.Make(M).t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length : 'a t -> int
  val ( !>> ) : int -> 'a t
  val ( >> ) : 'a t -> key -> 'a
  val ( >>> ) : 'a t -> key -> 'a list
  val ( >>? ) : 'a t -> key -> bool
  val ( << ) : 'a t -> key * 'a -> unit
  val ( <<+ ) : 'a t -> key * 'a -> unit
  val ( <<- ) : 'a t -> key -> unit
  val ( !<< ) : 'a t -> unit
  val ( <<: ) : 'a t -> (key -> 'a -> unit) -> unit
  val ( ~<<>> ) : 'a t -> (key -> 'a -> 'b -> 'b) -> 'b -> 'b
end
