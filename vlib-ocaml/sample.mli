(** Random sampling *)

val uniform : float -> float -> float
  (** [uniform a b] returns a sample from a uniform distribution. *)

val gaussian : float -> float
  (** [gaussian sigma] returns a sample from a Gaussian distribution. *)

val choose : float array -> int -> int -> int
  (** [choose dist i j] returns a sample from a discrete distribution defined
      by [dist.(i) ... dist(j)]. *)

val choose_any : float array -> int
  (** [choose_any dist] returns a sample from a discrete distribution defined
      by [dist.(0) ... dist(n)]. *)


