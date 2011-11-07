(** Representing curves *)

(** {2 Types} *)

(** A float-valued curve *)
type t = Complex.t array

(** {2 Operations on Curves} *) 

val width : t -> float
val height : t -> float

(** Break a closed curve into two open curves of equal length. *)
val open_of_closed : 'a array -> 'a array * 'a array

(** Remove duplicate points from a curve. *)
val uniqify : 'a array -> 'a array

(** [rotate_indices curve k] is [curve.(k) curve.(k+1) .. curve.(n-1)
    curve.(0) .. curve.(k-1)] *)
val rotate_indices : 'a array -> int -> 'a array

(** Reverse an array. *)
val reverse : 'a array -> 'a array

(** Flip a curve left-to-right by negating the x-coordinate and
    reversing the curve. *)
val flip : t -> t

(** Flip x and y coordinates *)
val flip_xy : t -> t

(** If it's wider than it is tall, apply flip_xy *)
val flip_best : t -> t

(** [align a b] rotates, scales, translates, and flips [a] so that it
    lines up more or less with [b] *)
val align : t -> t -> t

(** Scale and translate curve coordinates so that entire curve is inside the
    unit square. *)
val normalize : ?scale:float -> t -> t

(** {2 Subsampling curves} *)

(** Reduce the number of points on a curve, in a trivial fashion. [subsample_trivial c n] will
    return a curve with roughly [2^n] points. *)
val subsample_trivial : 'a array -> int -> 'a array

(** Reduce the number of points on a curve, while trying to
    approximate the shape of the curve. [subsample_dp curve n minlen]
    returns a curve with roughly [n] points. No segment has length
    less than [minlen]. *)
val subsample_dp : t -> int -> ?maxlen:int -> int -> t

(** Reduce the number of points on a curve, in a crude
    fashion. [subsample_ultracrude c n] will return a curve with
    exactly [n] points. *)
val subsample_ultracrude : 'a array -> int -> 'a array

(** {2 Input and Output} *)

(** [load fname] loads a curve from a file named [fname]. *)
val load : string -> t

(** [load fname] loads a curve in LabelMe XML format from a file named
    [fname]. *)
val load_from_xml : string -> t

(** Input a set of curves.  [load_all name first num] loads [num]
    curves, where [name] is a formatting string that evaluates to
    all of the filenames, and [first] is the index of the first. *)
val load_all : (int -> string) -> int -> int -> t array

(** [save fname c] saves the curve [c] to a file named [fname]. *)
val save : string -> t -> unit

val save_all : (int -> string) -> t array -> unit

(** Output a set of curves. [output curves name] saves each curve in
    [curves] to a different save file, which is named using [name]
    as a format string. *)
val draw_all : t array -> (int -> string) -> unit

(** {2 Drawing} *)

(** [ draw c off on ] produces an image where every pixel on the
    curve is colored [on] and every pixel off the curve is colored
    [off] *)
val draw : t -> 'a -> 'a -> 'a Image.t

(** Like [draw], but with an additional segment from [c.(n)] to [c.(0)]. *)
val draw_closed : t -> 'a -> 'a -> 'a Image.t

(* val draw_svg : Svg.t -> t -> unit *)
(* val draw_closed_svg : Svg.t -> t -> unit *)
(* val draw_svg_wt : Svg.t -> t -> float -> unit *)
(* val draw_closed_svg_wt : Svg.t -> t -> float -> unit *)

(** {2 Printing} *)

val print_curve : t -> unit

