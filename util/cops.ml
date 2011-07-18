open Printf
module C = Complex

let (c0 : C.t) = C.zero
let (c1 : C.t) = C.one
let (ci : C.t) = C.i

let cxre (a : float) : C.t = {C.re=a; C.im=0.}
let cxim (a : float) : C.t = {C.re=0.; C.im=a}

let (~-&) (a : C.t) : C.t = C.neg a

let (~&) (a : C.t) : C.t = C.conj a

let (+&) (a : C.t) (b : C.t) : C.t = C.add a b

let (-&) (a : C.t) (b : C.t) : C.t = C.sub a b

let ( *&) (a : C.t) (b : C.t) : C.t = C.mul a b

let (~/&) (a : C.t) : C.t = C.inv a

let (/&) (a : C.t) (b : C.t) : C.t = C.div a b

(* sqrt? *)

(* norm2? *)
let (~||^&) (a : C.t) : float = C.norm2 a

(* norm? *)
let (~||&) (a : C.t) : float = C.norm a

(* arg? *)
let (~|/&) (a : C.t) : float = C.arg a

(* polar? *)
(* exp? *)
(* log? *)
(* pow? *)
let ( **&) (a : C.t) (b : C.t) : C.t = C.pow a b
