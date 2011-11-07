(** Raster drawing operations *)

val line : (int * int) -> (int * int) -> (int * int) list
  (** [line (x1,y1) (x2,y2)] returns a list of points corresponding to
      the raster line from [(x1,y1)] to [(x2,y2)]. *)

val box : (int * int) -> int -> (int * int) list
  (** [box (x,y) r] returns a list of points corresponding to a box of
      radius [r] centered at [(x,y)]. *)

val draw_line : 'a Image.t -> (int * int) -> (int * int) -> 'a -> unit

val draw_box : 'a Image.t -> (int * int) -> int -> 'a -> unit
