(** Canny edge detection *)

open Image

(** {2 Helper Functions} *)

val gradient : float Image.t -> (float Image.t * float Image.t)
(** [gradient img] returns the horizontal and vertical gradients
    as a pair of images. The first image is the horizontal 
    gradient. *)

val gradient_magnitude : float Image.t -> float Image.t
(** [gradient_magnitude img] returns a new image where
    each pixel is the gradient magnitude of the pixel in
    [img]. *)

val grad_and_mag : float Image.t -> (float Image.t * float Image.t * float Image.t)
(** [grad_and_mag img] returns a triple where the first image is the
    horizontal gradient, second is the vertical gradient and the third
    is the gradient magnitude. *)

val edge_list : bool Image.t -> (int * int) list
(** [edge_list img] returns a list of the pixels with value
    [true] in [img]. The pixels are represented as coordinate 
    pairs [(x,y)]. *)

(** {2 Edge Detection} *)

val canny : float Image.t -> float -> float -> bool Image.t
(** [canny img thresh] computes canny edge detection of
    [img]. *)
