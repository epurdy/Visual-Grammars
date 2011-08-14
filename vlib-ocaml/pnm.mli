(** PNM image I/O *)

exception Bad_file of string

val load_pgm : string -> int Image.t
  (** [load_pgm name] reads a gray image from a PGM file. *)

val save_pgm : int Image.t -> string -> unit
  (** [save_pgm img name] saves a gray image to a PGM file. *)

val load_ppm : string -> Image.rgb Image.t
  (** [load_ppm name] reads a color image from a PPM file. *)

val save_ppm : Image.rgb Image.t -> string -> unit
  (** [save_ppm img name] saves a color image to a PPM file. *)
