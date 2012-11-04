(** Bundles of parallelized tasks. 

    A bundle that can handle [n] tasks is created with [create n]. The
    user interacts with [bundle] by calling [bundle.go f arg] to
    schedule the execution of [(f arg)], and calling [bundle.finish
    ()] to wait until all scheduled tasks are finished.

    If the user tries to schedule more than [n] tasks, [bundle.go]
    will block until enoughs tasks have been completed that another
    one can be taken on.
*)

(** Type of a bundle. *)
type 'a t = {
  throttle : int;
  waittime : float;
  mutable threads : Thread.t list;
  mutable flags : Mutex.t list;
  mutable go : ('a -> unit) -> 'a -> unit;
  mutable finish : unit -> unit;
}

(** [create throttle] creates a new bundle limited to [throttle]
    simultaneous tasks. *)
val create : int -> 'a t


(** {2 Filesystem} *)

(** [waiton fname] waits until the file named [fname] exists, and then
    returns [fname]. *)
val waiton : string -> string
