(** Points and such. *) 

(** An integer-valued point *)
type ipt = int * int

(** A float-valued point in R^2. *)
type cpt = Complex.t

(** {2 Vector arithmetic} *)

val flip_x : cpt -> cpt

val flip_xy : cpt -> cpt

val scale : cpt -> float -> cpt

val dot : cpt -> cpt -> float

val cross : cpt -> cpt -> float

val dist2 : cpt -> cpt -> float

val intersect : (cpt * cpt) -> (cpt * cpt) -> bool

(** {2 Conversion functions} *)

(** int tuples to complex points *)
val complex_of_point : ipt -> cpt

(** complex points to int tuples *)
val point_of_complex : cpt -> ipt

val center_and_scale : cpt * cpt * cpt -> cpt * cpt * cpt

(** {2 Printing} 
    Self explanatory. *)

val string_of_cpt : cpt -> string
val print_cpt : cpt -> unit

(** {2 Miscellaneous functions and constants} *)

(** [linecostfn curve i j] returns the sum from [k = i to j] of the
    squared distances of each point [curve.(k)] from the straight line
    joining [curve.(i)] and [curve.(j)]. Used for the subsampling
    dynamic program. *)
val linecostfn : cpt array -> int -> int -> float

(** (0,0) *)
val c0 : cpt

(** (1,0) *)
val c1 : cpt

