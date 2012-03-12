(** Bounding boxes - axis-aligned rectangular regions. *)

(** A rectangle in R^2 *)
type bounds = {
     xmin : float;
     xmax : float;
     ymin : float;
     ymax : float;
}

(** [Out_of_bounds (bounds,pt)] can be raised when the point [pt] is not
    inside of [bounds]. *)
exception Out_of_bounds of bounds * Complex.t

(** {2 Special bounding boxes} *)

(** A sensible default *)
val defaultbounds : bounds

(** [ \[0.0, 1.0\] x \[0.0, 1.0\] ]  *)
val unitsquare : bounds

(** {2 Mapping functions} 

    There is a unique affine map [f] that maps the corners of one bounding
    box to those of another. These functions apply that map. *)

(** Let [f] be the unique affine map from bounding box [bds] to the
    unit square. [map_to_unit_square_strict bds z] returns [(f z)], but
    raises [Out_of_bounds (bds,z)] if [z] is not in [bds]. *)
val map_to_unit_square_strict : bounds -> Geometry.cpt -> Geometry.cpt 

(** Like [map_to_unit_square_strict], but return [Some (f z)] if [z]
    is in [bds], and [None] otherwise. *)
val map_to_unit_square : bounds -> Geometry.cpt -> Geometry.cpt option

(** Like [map_to_unit_square_strict bds z], but silently move [z] into
    [bds] if [z] is not in [bounds]. *)
val map_to_unit_square_nice : bounds -> Geometry.cpt -> Geometry.cpt 

(** Let [f] be the unique affine map from the unit square to the
    bounding box [bds]. [map_from_unit_square bds z] returns [(f
    z)]. If [z] is not in the unit square, raise Out_of_bounds. *)
val map_from_unit_square : bounds -> Geometry.cpt -> Geometry.cpt

(** {2 Integer mapping functions} 

    For any given bounding box [bds] and integer [n], we can
    (approximately) represent points in [bds] with a pair [(i,j)] of
    integers between [0] and [n], inclusive. [(xmin,ymin)] is mapped
    to [(0,0)], and [(xmax,ymax)] is mapped to [(n,n)].
*)

(** [unit_square_to_array n z] returns indices (i,j) for [z]. *)
val unit_square_to_array : int -> Geometry.cpt -> int * int

val bounds_to_array_strict : bounds -> int -> Geometry.cpt -> int * int
val bounds_to_array_nice : bounds -> int -> Geometry.cpt -> int * int

val array_to_bounds : bounds -> int -> int * int -> Geometry.cpt

(** {2 Operations on Bounds} *)

(** The smallest bounding box that contains both arguments. *)
val union : bounds -> bounds -> bounds

(** A degenerate bounding box containing only a single point. *)
val bounds_of_pt : Geometry.cpt -> bounds

(** An empty bounding box containing no points. *)
val null_bounds : bounds

(** Expand one dimension of [bds] so that it has a 1:1 aspect ratio. *)
val squarify : bounds -> bounds

(** [marginify bds t] expand [bds] slightly in all directions, so that
it is larger by a factor of [1.0 +. t]. *)
val marginify : bounds -> float -> bounds

(** Make bounds that contain all the given points. Then apply
    [squarify bds] and [marginify bds 0.1] for increased niceness. *)
val nice_curve_bounds : Complex.t array -> bounds

(** undoc *)
val move_into_bounds : bounds -> Geometry.cpt -> Geometry.cpt
