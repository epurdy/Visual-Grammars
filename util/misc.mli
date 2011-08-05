(** Miscellaneous utility functions *)

exception Not_Implemented of string

type ('a,'b) hash = ('a,'b) Hashtbl.t

val find : 'a -> 'a array -> int

val fn_of_hash : ('a,'b) hash -> ('a -> 'b)
val invert_array : 'a array -> ('a, int) Hashtbl.t

val square : float -> float

val take_best : int -> 'a list -> 'a list

val isfinite : float -> bool

val random_choice : 'a array -> 'a

val round : float -> int

val logadd : float -> float -> float
val neglogadd : float -> float -> float

val listmin : 'a list -> 'a

(** {2 Strings} *)

(** [randstr n] produces a random alphanumeric string of [n]
    characters. *)
val randstr : int -> string

val string_of_array : char array -> string

(** {2 Commands and Files} *)

(** [doit cmd]: Print [cmd], do [cmd], then throw an exception if the
    return code was anything other than [0]. *)
val doit : string -> unit

(** [doit_ok cmd]: *)
val doit_ok : string -> unit

(** [doitq cmd]: Do [cmd], then throw an exception if the
    return code was anything other than [0]. *)
val doitq : string -> unit

(** [input_word chan]: input word from channel skipping
    whitespace. Throws exception at end of file. *)
val input_word : in_channel -> string

(** {2 Parallelization} *)

val get_host : unit -> string

(** {2 Logging} *)

val print_confusion : int array array -> unit

val progress : char -> (int -> unit) -> int -> int -> unit

val progress_bar : int -> char -> unit

(** Save an image as PGM and then convert to PNG. Takes a base common
    to both filenames. *)
val save_pgm_png : int Image.t -> string -> unit

(** Print some underscores. *)
val divider : unit -> unit

(** Print some underscores and then a title. *)
val title : string -> unit

(** {2 Miscellaneous} *)

(** Placeholder function for unimplemented functions. Raises
    Not_Implemented. *)
val placeholder : string -> 'a -> unit

(** Make a unique id. *)
val uid : unit -> int

(** Unpack an option. [get (Some x)] yields [x]. [get None] raises an
    exception. *)
val get : 'a option -> 'a
