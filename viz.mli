
val show_labeled_curve : Svg.t -> Curve.t -> string array -> unit

val show_samples : Svg.t -> ?bottom_buffer:float -> string -> int -> Curve.t array -> unit

val show_samples_midpoints : Svg.t ->  string -> int -> Curve.t ->
  (Curve.t array) * (Geometry.cpt array array) -> float -> unit


val save_samples : string -> int -> Curve.t array -> unit
