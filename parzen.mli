(** The Parzen estimator to a non-parametric distribution *)

type density = float array array

(** {2 Distributions} *)

(** A Parzen density estimator with a cached probability map. Density
    will be approximated by a grid of size [(granularity + 1) x
    (granularity + 1)]

    To smooth the distribution, we model it as being a mixture between
    a classic Parzen estimator and a gaussian with much higher
    variance, which we will call the baseline gaussian.

    The value [baseline_weight] gives the mixture weight of the
    baseline gaussian.

    The value [sigma] is the bandwidth of the Parzen estimator.
*)
type model  = 
    {centers : Geometry.cpt array;
     nl_weights : float array;
     nl_weights_cdf : float array; (** allows efficient sampling *)
     sigma : float;
     granularity : int;
     bounds : Bounds.bounds;
     baseline_mean : Geometry.cpt;
     mutable baseline_sigma : float;
     baseline_weight : float;
     density : density}


(** A model that produces relatively straight shapes. The parameter is
    the standard deviation / bandwidth. *)
val new_straight_model : float -> model

(** [make_model centers sigma granularity (baseline_mean, baseline_sigma) baseline_weight] creates the Parzen density
    estimator with points [centers] and standard deviation / bandwidth
    [sigma]. We compute a discretized density map; the level of
    discretization is given by [granularity]. 

    The baseline value is given by [baseline].
*)
val make_model : Shape.shape array -> float -> int -> Shape.shape * float -> float -> bool -> model

val make_model_neglog_weighted : (float * Shape.shape) array -> float -> int -> Shape.shape * float -> float -> bool -> model

(** [make_density bounds centers granularity sigma] computes a density
    map. [granularity] specifies the level of discretization. *)
(* val make_density : bounds -> (float * Geometry.cpt) array -> int -> float -> float array array *)

(** Pick a random shape according to [model]. This is an exact sample,
    and is not based on the cached density map. *)
val sample : model -> Shape.shape
val sample_pt : model -> Geometry.cpt

(** [probability model shape] gives the probability of [shape]
    according to [model]. This is done by looking up the cached value
    in [model.density]. Currently this is done by rounding rather than
    interpolation. *)
(* val probability_approx : model -> Shape.shape -> float *)

(* (\** [probability model shape] gives the probability of [shape] *)
(*     according to [model]. This is done by summing over the centers *)
(*     that define the distribution. *\) *)
(* val probability_exact : model -> Shape.shape -> float *)

(** [probability model shape] gives the probability of [shape]
    according to [model]. Currently this is [probability_approx]. *)
(* val probability : model -> Shape.shape -> float *)

(** [cost model shape] gives the negative log probability of [shape]
    according to [model].  *)
val cost : model -> Shape.shape -> float

val force : model -> unit

(* (\** [cost model shape] gives the negative log probability of [shape] *)
(*     according to [model], but may return neg_infinity.  *\) *)
(* val cost_lazy : model -> Shape.shape -> float *)

(** {2 Printing and Display} *)


val draw_model : model -> (int * int * int) Image.t
(* val draw_model : model -> int Image.t *)
val draw_centers : model -> int Image.t

val string_of_model : model -> string
val print_model : model -> unit
