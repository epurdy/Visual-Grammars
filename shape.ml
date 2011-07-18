(** Representing shapes (in modified Bookstein coordinates). *)
open Printf
open Util.Cops
module C = Complex
module G = Geometry

type shape =
    { p : Geometry.cpt; (* modified bookstein *)
      scale : float
    }

let get_pt shape = shape.p

let get_scale shape = shape.scale

let place shape a b =
  let rv = (shape.p *& (b -& a)) +& a in
    assert(rv <> a && rv <> b);
    rv

let shape_of_complex_bme bg mi en =
  let bg_mi = mi -& bg in
  let bg_en = en -& bg in
    (try
       assert(bg_en <> c0);
       { p = bg_mi /& bg_en;
	 scale = C.norm bg_en
       }
     with Assert_failure _ ->
       (* printf "soc_bme af\n%!"; *)
       (* G.print_cpt bg; *)
       (* G.print_cpt mi; *)
       (* G.print_cpt en; *)
       (* assert(bg_en <> c0); *)
       { p = c0;
	 scale = 0.;
       }
    )

let shape_of_complex_bem bg en mi = shape_of_complex_bme bg mi en

let shape_of_ints_bem p q r = 
  shape_of_complex_bem 
    (G.complex_of_point p) (G.complex_of_point q) (G.complex_of_point r)

let shape_of_ints_bme p q r = 
  shape_of_ints_bem p r q

let straight = shape_of_ints_bme (0,0) (0,1) (0,2)

let string_of_shape (shape : shape) : string =
  sprintf "<%s (%f)>\n" (G.string_of_cpt shape.p) shape.scale

let default_shape = {
  p = Con.default_baseline_mean;
  scale = 1.0;
}
