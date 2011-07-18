(** Operator syntax for complex numbers *)

(** Each operator is the usual symbol ([+],[-],[*],[/], etc.) followed
    by [&]. Also, [~&] is complex conjugation. 

    [c0],[c1],[ci] are [0],[1],[i], respectively.

    Operator precedence seems to automatically be what you would
    expect, at least for addition and multiplication. *)

open Complex

(** [cxre x] yields [x + 0 i] *)
val cxre : float -> t

(** [cxim x] yields [0 + x i] *)
val cxim : float -> t

(** [0 + 0 i] *)
val c0 : t

(** [1 + 0 i] *)
val c1 : t

(** [0 + 1 i] *)
val ci : t

(** Unary negation. [~-& z] *)
val (~-&) : t -> t

(** Conjugation. [~& z] *)
val (~&) : t -> t

(** Addition. [z1 +& z2] *)
val (+&) : t -> t -> t

(** Subtraction. [z1 -& z2] *)
val (-&) : t -> t -> t

(** Multiplication. [z1 *& z2] *)
val ( *&) : t -> t -> t

(** Multiplicative inverse. [~/& z = 1 /& z] *)
val (~/&) : t -> t 

(** Division. [z1 /& z2] *)
val (/&) : t -> t -> t

(* (\** Norm squared. *\) *)
(* val (~||^&) : t -> float *)

(* (\** norm *\) *)
(* val (~||&) : t -> float *)

(* (\** arg *\) *)
(* val (~|/&) : t -> float *)

(** Exponentiation. [z1 ^& z2] *)
val ( **&) : t -> t -> t
