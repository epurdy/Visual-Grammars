(** Representing shapes (in modified Bookstein coordinates). *)

type shape (* = Geometry.cpt *)

(** [shape_of_complex_bem p r q] is the shape of the triangle with
    vertices [p], [q], [r]. The order of the points is (b)eginning
    (e)nd (m)idpoint. *)
val shape_of_complex_bem : Geometry.cpt -> Geometry.cpt -> Geometry.cpt -> shape

(** [shape_of_complex_bme p q r] is the shape of the triangle with
    vertices [p], [q], [r]. The order of the points is (b)eginning
    (m)idpoint (e)nd. *)
val shape_of_complex_bme : Geometry.cpt -> Geometry.cpt -> Geometry.cpt -> shape

(** Like [shape_of_complex_bem], but for int tuples. *)
val shape_of_ints_bem : Geometry.ipt -> Geometry.ipt -> Geometry.ipt -> shape

(** Like [shape_of_complex_bme], but for int tuples. *)
val shape_of_ints_bme : Geometry.ipt -> Geometry.ipt -> Geometry.ipt -> shape

(** The shape of a straight line. *)
val straight : shape

(** Given the shape of a triangle, and its two endpoints, where should
    the third point be? *)
val place : shape -> Geometry.cpt -> Geometry.cpt -> Geometry.cpt
val place_unsafe : shape -> Geometry.cpt -> Geometry.cpt -> Geometry.cpt

val string_of_shape : shape -> string 

(** Given the shape of a triangle, and with its two endpoints at 0 and
    1, where should the third point be? *)
val get_pt : shape -> Geometry.cpt

val get_scale : shape -> float


val default_shape : shape
