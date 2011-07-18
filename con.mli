(** Constants! *)

(** the one and only *)
val pi : float

(** {2 Curve Subsampling Parameters} *)

(** Desired number of points on a curve in the subsampling
    algorithm. *)
val nump : int

(** Regularization weight for subsampling dynamic program. *)
val regweight : float

(** {2 Initial Grammar Heuristic Parameters} *)

(** Factor used in straightcost definition. *)
val spfactor : float

(** Factor used in concentration definition. *)
(* val concfactor : float *)

(** Default sigma for non-parametric distribution. *)
val default_sigma : float

(** granularity of non-parametric distribution approximation *)
val default_granularity : int

(** Default baseline weight for non-parametric distribution. *)
val default_baseline_weight : float

val default_baseline_mean : Complex.t
val default_baseline_sigma : float

val em_smoothing_nlog : float

(** {2 Experimental Design} *)

val nclasses : int
val ntrain : int
val ntest : int
val ntotal : int

(** {2 Misc} *)

(** Limit on number of parallel tasks. *)
val throttle : int
