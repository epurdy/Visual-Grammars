open Printf
open Hashtbl

type ('a,'b) hash = ('a,'b) Hashtbl.t

let keys h = fold 
  begin fun k v ls ->
    k :: ls;
  end
  h []

let mkhash (n : int) : ('a,'b) t = 
  create n

let (!>>) (n : int) : ('a,'b) t = 
  create n

let (>>) (h : ('a,'b) t) (k: 'a) : 'b = 
  find h k

let (>>!) (h : ('a,'b) t) ( (k : 'a), (d : 'b) ) =
  try 
    find h k
  with Not_found -> 
    d    

let (>>>) (h : ('a,'b) t) (k: 'a) : 'b list = 
  find_all h k

let (>>?) (h : ('a,'b) t) (k: 'a) : bool = 
  mem h k

let (<<) (h : ('a,'b) t) (k,v : 'a * 'b) : unit = 
  replace h k v

let (<<+) (h : ('a,'b) t) (k,v : 'a * 'b) : unit =
  add h k v

let (<<-) (h : ('a,'b) t) (k : 'a) : unit =
  remove h k

let (!<<) (h : ('a,'b) t) : unit =
  clear h

let (<<:) (h : ('a,'b) t) (f : 'a -> 'b -> unit) : unit = 
  iter f h

let (~<<>>) (h : ('a,'b) t) (f : 'a -> 'b -> 'c -> 'c) (u : 'c) : 'c =
 fold f h u

(* types below same as above *)
module Make (M : HashedType) = struct
  include Hashtbl.Make (M)

  let (!>>) n = create n

  let (>>) h k = find h k
  let (>>>) h k = find_all h k
  let (>>?) h k = mem h k

  let (<<) h (k,v) = replace h k v
  let (<<+) h (k,v) = add h k v
  let (<<-) h k = remove h k
  let (!<<) h = clear h

  let (<<:) h f = iter f h
  let (~<<>>) h f u = fold f h u

end
