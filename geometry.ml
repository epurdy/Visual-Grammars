open Util.Cops
open Printf

type ipt = int * int

type cpt = Complex.t

let string_of_cpt p =
  sprintf "(%f,%f)" p.Complex.re p.Complex.im

let print_cpt p = 
  printf "%s\n%!" (string_of_cpt p)

(** VECTOR ARITHMETIC
    ------------------------------------------------------------- *)

let flip_x z =
  {Complex.re = -. z.Complex.re;
   Complex.im = z.Complex.im}

let flip_xy z = 
  {Complex.re = z.Complex.im;
   Complex.im = z.Complex.re}

(* multiply complex number by scalar *)
let scale x s = 
  { Complex.re = x.Complex.re *. s;
    Complex.im = x.Complex.im *. s }

(* dot product between vectors represented as complex numbers *)  
let dot x y = 
  x.Complex.re *. y.Complex.re +. x.Complex.im *. y.Complex.im

let dist2 p q = 
  Complex.norm2 (p -& q)

let linecostfn (curve : cpt array) (bg : int) (en : int) : float =
  let p = curve.(bg) in
  let q = curve.(en) in
  let pq = q -& p in
  let upq = scale pq (1.0 /. Complex.norm pq) in
  let tot = ref 0. in
    for i=bg to en do
      let z = curve.(i) in
      let displacement = ((z -& p) /& upq).Complex.im in
	tot := !tot +. displacement *. displacement;
    done;
    !tot

(** Bookstein coordinates! *)

let normalize (p, q, r) = 
  let pnorm2 = Complex.norm2 p in
  let qnorm2 = Complex.norm2 q in
  let rnorm2 = Complex.norm2 r in
  let size = 1.0 /. sqrt (pnorm2 +. qnorm2 +. rnorm2) in
    (scale p size,
     scale q size,
     scale r size)

let normalize2 p q = 
  let pnorm2 = Complex.norm2 p in
  let qnorm2 = Complex.norm2 q in
  let size = 1.0 /. sqrt (pnorm2 +. qnorm2) in
    (scale p size,
     scale q size)

let center (p, q, r) =
  let mean = scale (p +& q +& r) (1.0 /. 3.0) in
  let p = p -& mean in
  let q = q -& mean in
  let r = r -& mean in
    (p,q,r)

let center_and_scale (p, q, r) =
  let p,q,r = center (p, q, r) in
    normalize (p, q, r)


(** CONVERSION FUNCTIONS
    -------------------------------------------------------------*)

(* complex representation of point *) 
let complex_of_point (x, y) =
  { Complex.re = float_of_int x; Complex.im = float_of_int y }

(* point representation of complex number, should round! *)
let point_of_complex p =
  (int_of_float p.Complex.re, int_of_float p.Complex.im)



let c0 = c0
let c1 = c1
