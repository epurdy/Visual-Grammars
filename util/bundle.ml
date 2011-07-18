type 'a t = {
  throttle : int;
  waittime : float;
  mutable threads : Thread.t list;
  mutable flags : Mutex.t list;
  mutable go : ('a -> unit) -> 'a -> unit;
  mutable finish : unit -> unit;
}

let waiton fname =
  while not (Sys.file_exists fname) do
    Thread.delay 0.1;
  done;
  fname

let wrap f =
  let wrapped (arg,finished,notstarted) =
    (* Order matters here - only signal we've started AFTER we've
       made it official that we haven't finished. *)
    Mutex.lock finished;   (* not finished *)
    Mutex.lock notstarted; (* Just started - not notstarted *)
    f arg;
    Mutex.unlock finished; (* Just finished *)
  in
    wrapped

let create throttle = 
  let self = {
    throttle = throttle;
    waittime = 0.01;
    go = (fun f x -> ());
    threads = [];
    flags = [];
    finish = (fun () -> ());
  } in

  let go2 f x = 
    let fwrap = wrap f in
    let finished, notstarted = Mutex.create (), Mutex.create () in
    let _ = 
      (* Unlocked so that we can hand them over.  They're presumably
	 unlocked anyway, but docs don't say.  *)
      Mutex.unlock finished;
      Mutex.unlock notstarted;
    in
    let t = Thread.create fwrap (x,finished,notstarted) in
      self.threads <- t :: self.threads;
      self.flags <- finished :: self.flags;

      (* Give t time to get started. *)
      while Mutex.try_lock notstarted do (* while notstarted *)
	(* try_lock grabs the lock when its unlocked (= notstarted),
	   so give it back, since we're waiting until t has
	   started. *)
	Mutex.unlock notstarted; 
	Thread.delay self.waittime;
      done;

      (* If we've reached the thread limit, clean out threads which
	 are finished. If there aren't any, then just wait a bit. *)
      while List.length self.threads >= throttle do 
	let pairs = List.combine self.threads self.flags in
	let pairs = List.filter
	  (* try_lock true <=> wrapped fn unlocked "finished"
	     <=> thread is done! <=> drop it from the list! *)
	  (fun (t,f) -> not (Mutex.try_lock f)) pairs in
	let threads, flags = List.split pairs in
	  self.threads <- threads;
	  self.flags <- flags;
	  Thread.delay self.waittime;
      done;
  in

(*   let go f x =  *)
(*     let t = Thread.create f x in *)
(*       self.threads <- t :: self.threads; *)
(*       if List.length self.threads >= throttle then begin *)
(* 	List.iter Thread.join self.threads; *)
(* 	self.threads <- []; *)
(*       end; *)
(*   in *)

  let finish () =
    List.iter Thread.join self.threads;
    self.threads <- [];
  in

    self.go <- go2;
    self.finish <- finish;
    self
