
type t = {
  svg:Cairo_svg.surface;
  ctx:Cairo.t}

val create : string -> t
val create_sized : string -> float * float -> t

val finish : t -> unit

val draw_curve : t -> Curve.t -> unit

val draw_closed_curve : t -> Curve.t -> unit

val draw_curve_wt : t -> Curve.t -> float -> unit

val draw_closed_curve_wt : t -> Curve.t -> float -> unit

val make_the_curve : t -> Curve.t -> unit

val show_title : t -> string -> unit

val draw_in_table : t -> int -> int -> (t -> int -> unit) -> unit

val draw_circle_filled : t -> Complex.t -> float -> unit
val draw_circle : t -> Complex.t -> float -> unit
