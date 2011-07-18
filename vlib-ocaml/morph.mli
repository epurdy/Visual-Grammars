(** Morphological filters *)

val gdt : float Image.t -> float Image.t
  (** [gdt img] computes the generalized distance transform of [img], with
      the squared euclidean distance. *)

val dt : bool Image.t -> float Image.t
  (** [dt img] computes the squared euclidian distance transform of [img]. *)
