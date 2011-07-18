(** Low-level filters *)

val smooth : 'a Image.ops -> 'a Image.t -> float -> 'a Image.t
  (** [smooth ops img s] convolves the image [img] with a Gaussian
      with sigma [s] using the operations in [ops]. *)

type grad = 
    { mag : float;
      theta : float }
  (** Gradient vector. *)

val grad_float : float Image.t -> grad Image.t
  (** Compute gradients of a float image. *)

val grad_rgbfloat : Image.rgbfloat Image.t -> grad Image.t
  (** Compute gradients of an rgbfloat image. *)
