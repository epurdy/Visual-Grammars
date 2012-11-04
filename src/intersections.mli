
type intersection_distro

val count_self_intersections : Curve.t -> int

val intersection_distro_family : (Curve.t, intersection_distro, float, int array, unit) Distro.distro_family
