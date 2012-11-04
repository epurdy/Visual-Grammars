type scale_build = {
  n: int;  (* smallest scale is 1/n *)
  max_lvl: int; (* log_2 n *)
  max_len_lvl: int; (* max length of curve relative to its level *)
  curves: Curve.t array;
}

type scale_sdf

val texture_heuristic: (scale_build, scale_sdf, scale_sdf) Xform.xform_heuristic
