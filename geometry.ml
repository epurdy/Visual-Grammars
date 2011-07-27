open Util.Cops
open Printf

module C = Complex

type ipt = int * int

type cpt = C.t

let string_of_cpt p =
  sprintf "(%f,%f)" p.C.re p.C.im

let print_cpt p = 
  printf "%s\n%!" (string_of_cpt p)

(** VECTOR ARITHMETIC
    ------------------------------------------------------------- *)

let flip_x z =
  {C.re = -. z.C.re;
   C.im = z.C.im}

let flip_xy z = 
  {C.re = z.C.im;
   C.im = z.C.re}

(* multiply complex number by scalar *)
let scale x s = 
  { C.re = x.C.re *. s;
    C.im = x.C.im *. s }

(* dot product between vectors represented as complex numbers *)  
let dot x y = 
  x.C.re *. y.C.re +. x.C.im *. y.C.im

let dist2 p q = 
  C.norm2 (p -& q)

let linecostfn (curve : cpt array) (bg : int) (en : int) : float =
  let p = curve.(bg) in
  let q = curve.(en) in
  let pq = q -& p in
  let upq = scale pq (1.0 /. C.norm pq) in
  let tot = ref 0. in
    for i=bg to en do
      let z = curve.(i) in
      let displacement = ((z -& p) /& upq).C.im in
	tot := !tot +. displacement *. displacement;
    done;
    !tot

(** Bookstein coordinates! *)

let normalize (p, q, r) = 
  let pnorm2 = C.norm2 p in
  let qnorm2 = C.norm2 q in
  let rnorm2 = C.norm2 r in
  let size = 1.0 /. sqrt (pnorm2 +. qnorm2 +. rnorm2) in
    (scale p size,
     scale q size,
     scale r size)

let normalize2 p q = 
  let pnorm2 = C.norm2 p in
  let qnorm2 = C.norm2 q in
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
  { C.re = float_of_int x; C.im = float_of_int y }

(* point representation of complex number, should round! *)
let point_of_complex p =
  (int_of_float p.C.re, int_of_float p.C.im)



let c0 = c0
let c1 = c1
