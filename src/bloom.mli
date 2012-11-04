(* A Bloom filter that gives a lower bound on H[x]. Note that it is
   impossible to increase the value of H[x], but it is possible to
   decrease it. *)
type 'a t = {
  size: int;
  nfuncs: int;
  buckets: float array;
  hash1: 'a -> int;
  hash2: 'a -> int;
}

val new_bloom : int -> int -> ('a -> 'a) -> 'a t

val insert : 'a t -> 'a -> float -> unit

val query : 'a t -> 'a -> float

val test_lt : 'a t -> 'a -> float -> bool

val test_gt : 'a t -> 'a -> float -> bool
