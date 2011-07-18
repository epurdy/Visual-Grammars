(** Gaussian pyramid *)

val create : 'a Image.ops -> 'a Image.t -> int -> ('a Image.t) array
  (** [pyramid ops img levels] creates a pyramid. *)

val output : ('a Image.t) array -> 'a -> 'a Image.t
  (** [output p empty] makes a picture of the pyramid [p] using
      [empty] to fill-in empty spots in the output image. *)
  
