(** Image datatype *)

type 'a t
  (** The type of an image with pixels of type ['a]. *)

val create : int -> int -> 'a -> 'a t
  (** [create width height v] returns a fresh image of size
      [(width,height)] where each pixel has value [v]. *)

val init : int -> int -> ((int * int) -> 'a) -> 'a t
  (** [init width height f] returns a fresh image of size
      [(width,height)] where pixel [(x,y)] has value [f (x,y)]. *)

val copy : 'a t -> 'a t
  (** [copy img] returns a copy of [img]. *)

val width : 'a t -> int
  (** [width img] returns the width of [img]. *)

val height : 'a t -> int
  (** [height img] returns the height of [img]. *)

val get : 'a t -> (int * int) -> 'a
  (** [get img (x,y)] returns the value of pixel [(x,y)] in
      image [img]. *)

val set : 'a t -> 'a -> (int * int) -> unit
  (** [set img v (x,y)] sets the value of pixel [(x,y)] in image
      [img] to [v]. *)

val get_many : 'a t -> (int * int) list -> 'a list
  (** [get_pixels img pts] returns a list with the value of pixels
      [pts] in image [img]. *)

val set_many : 'a t -> 'a -> (int * int) list -> unit
  (** [set_pixels img v pts] sets the value of pixels [pts] in image
      [img] to [v]. *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f img] applies [f] to all pixels in image [img]. *)

val iterc : ('a t -> int -> int -> unit) -> 'a t -> int * int -> int * int -> unit

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f img] applies [f] to all pixels in image [img] and
      creates a new image with the results returned by [f]. *)

val iterxy : ((int * int) -> 'a -> unit) -> 'a t -> unit
  (** Same as [iter] but the function takes the coordinate of a
      pixel as a first argument and the pixel as a second argument. *)

val mapxy : ((int * int) -> 'a -> 'b) -> 'a t -> 'b t
  (** Same as [map] but the function takes the coordinate of a
      pixel as a first argument and the pixel as a second argument. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold f v img] computes [f (... (f (f v img(0,0)) img(1,0)) ...)
      img(width-1, height-1)] where [(width,height)] is the size of
      the image [img].  The iteration goes over the pixels in
      row-major order. *)

val blit : 'a t -> 'a t -> (int * int) -> unit
  (** [blit src dst p] copies src into dst starting at point p. *) 

val sub : 'a t -> (int * int) -> (int * int) -> 'a t
  (** [sub img (x,y) (w,h)] returns a fresh image of size [w] by [h]
      containing the part of [img] starting at [(x,y)]. *)

val inside : 'a t -> (int * int) -> bool
  (** Checks if a point is inside an image. *)

val combine : 'a t -> 'a t -> ('a -> 'a -> 'b) -> 'b t
  (** [combine img1 img2 f] combines two images to generate a fresh
      image.  The value of the result at [(x,y)] is the value of [f] 
      applied to the value of [img1] and [img2] at [(x,y)]. *)

type rgb = int * int * int
  (** rgb value. *)

type rgbfloat = float * float * float
  (** rgb float value. *)

val color_of_gray : 'a -> ('a * 'a * 'a)
  (** Maps a gray value to a color value. *)

val rgbfloat_of_rgb : rgb -> rgbfloat
  (** Maps an rgb value to an rgbfloat value. *)

val rgb_of_rgbfloat : rgbfloat -> rgb
  (** Maps an rgbfloat value to an rgb value. *)

type 'a ops =
    { zero : 'a;
      mul : float -> 'a -> 'a;
      add : 'a -> 'a -> 'a;
      sub : 'a -> 'a -> 'a;
      dot : 'a -> 'a -> float }
  (** Pixel operations for defining generic image 
      processing functions. *)

val float_ops : float ops
  (** Usual operations on float values. *)

val rgbfloat_ops : rgbfloat ops
  (** Usual operations on rgb_float values. *)
