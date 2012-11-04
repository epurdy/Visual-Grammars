(** {2 Subsampling curves and other arrays} *)

(** Reduce the number of points on a curve, in a trivial fashion. [subsample_trivial c n] will
    return a curve with roughly [2^n] points. *)
val subsample_trivial : 'a array -> int -> 'a array

(** Reduce the number of points on a curve, while trying to
    approximate the shape of the curve. [subsample_dp curve n minlen]
    returns a curve with roughly [n] points. No segment has length
    less than [minlen]. *)
val subsample_dp : Curve.t -> int -> ?maxlen:int -> int -> Curve.t

(** Reduce the number of points on a curve, in a crude
    fashion. [subsample_ultracrude c n] will return a curve with
    exactly [n] points. *)
val subsample_ultracrude : 'a array -> int -> 'a array
