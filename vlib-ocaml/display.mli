(** Graphics display *)

val open_graph : int -> int -> unit
  (** [open_graph w h] opens a graphics window of size [w] * [h]. *)

val close_graph : unit -> unit
  (** [close_graph ()] closes the graphics window. *)

val show_gray : int Image.t -> unit
  (** [show_gray img] displays a gray image in the graphics window. *)

val show_color : Image.rgb Image.t -> unit
  (** [show_color img] displays a color image in the graphics window. *)

val wait_mouse_down : unit -> int * int
  (** [wait_mouse_down ()] waits for a mouse button click and returns
      its coordinates. *)

val wait_mouse_up : unit -> int * int
  (** [wait_mouse_up ()] waits for a mouse button release and returns
      its coordinates. *)
