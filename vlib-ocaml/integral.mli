(** Integral image and filters *)

type t
  (** The type of an integral image. *)

val create : float Image.t -> t
  (** [create img] computes the integral image of [img]. *)

val box : t -> (int * int) -> int -> int -> float
  (** [box vals (x,y) w h] computes a box average from an integral image. *)
