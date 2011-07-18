open Hops
open Printf

module Bundle = Bundle
module Hops = Hops
module Cops = Cops

exception Not_Implemented of string

type ('a,'b) hash = ('a,'b) Hashtbl.t

let fn_of_hash hash = (fun k -> hash >> k)

let hash_of_array arr =
  let h = mkhash (Array.length arr) in
    Array.iteri 
      begin fun i x ->
	h << (i,x);
      end
      arr;
    h

let invert_array arr = 
  let inv = Hashtbl.create (Array.length arr) in
  let _ = Array.iteri 
    begin fun i old ->
      inv << (old, i);
    end
    arr
  in
    inv

let rec take_best n ls =
  match n,ls with
      n, _  when n <= 0 -> []
    | n, [] -> []
    | n, hd :: tl -> hd :: (take_best (n-1) tl)

let square x = x *. x

let round x = int_of_float (x +. 0.5)

let isfinite x =
  let cls = classify_float x in
    if cls = FP_nan || cls = FP_infinite then
      false
    else 
      true

let listmin ls = 
  List.fold_left 
    begin fun a b ->
      min a b
    end
    (List.hd ls)
    ls

let random_choice arr = arr.(Random.int (Array.length arr))

let min_logratio = -. 10.
let invtol = 1000
let finvtol = float_of_int invtol
let logadd_lookup = Array.init (invtol + 1) 
  begin fun i -> 
    let i = float_of_int i in
    let logratio = -. min_logratio *. (i /. finvtol) in
      log (1. +. exp(logratio))
  end

let add_logratio_exact logratio =
  let ratio = exp logratio in (* small/big *)
  let sum = 1. +. ratio in (* 1 + small/big *)
  let logsum = log sum in (* log(1 + small/big) *)
    logsum

let add_logratio_approx logratio = 
  if logratio < min_logratio then
    0.
  else begin
    let i = round ((logratio /. min_logratio) *. finvtol) in
      (* printf "logratio=%f logadd i=%d\n%!" logratio i; *)
      logadd_lookup.(i)
  end

let logadd logx logy = 
  let rv =
    begin match (logx, logy) with
	(l,_) when l = infinity -> infinity 
      | (_, l) when l = infinity -> infinity 
      | (_,l) when l = neg_infinity -> logx
      | (l,_) when l = neg_infinity -> logy
      | (_,_) ->
	  begin
	    let logsmall = min logx logy in
	    let logbig = max logx logy in
	    let logratio = logsmall -. logbig in (* log (small/big) *)
	    let logsum = add_logratio_exact logratio in
	    let rv = logsum +. logbig in (* log(big + small) *)
	      if rv <> rv then begin
		failwith (sprintf "logadd(%f,%f) = nan" logx logy);
	      end;
	      rv
	  end    
    end 
  in
    (* printf "logadd(%f,%f) = %f\n" logx logy rv; *)
    rv

let neglogadd neglogx neglogy = 
  -. (logadd (-. neglogx) (-. neglogy))

(** Strings *)

let string_of_array (a : char array) =
  let len = Array.length a in
  let s = String.make len 'q' in
    for i = 0 to len-1 do
      s.[i] <- a.(i);
    done;
    s

let lower = Array.init 26 (fun x -> (Char.chr ((Char.code 'a') + x)))
let upper = Array.init 26 (fun x -> (Char.chr ((Char.code 'A') + x))) 
let nums = Array.init 10  (fun x -> (Char.chr ((Char.code '0') + x)))
let alphanum = Array.concat [lower; upper; nums]

let randstr len =
  let n = Array.length alphanum in
  let a = Array.init len (fun x -> alphanum.(Random.int n)) in
    string_of_array a

(** Commands and files *)

let doitq s = 
  let code = Sys.command s in
    assert(code = 0)

let doit s = 
  printf "%s\n%!" s;
  doitq s

let doit_ok s = 
  printf "%s\n%!" s;
  let _ = Sys.command s in
    ()

(* input word from channel skipping whitespace *)
let rec input_word (chan : in_channel) = 
  let whitespace c =
    c = '\n' || c = ' ' || c = '\r' || c = '(' || c = ')' || c = ',' in
  let rec iter chan s = 
    let c = input_char chan in 
      if whitespace c then s
      else iter chan (s^(Char.escaped c)) in
  let first = input_char chan in
    if whitespace first then input_word chan
    else iter chan (Char.escaped first)

(** Parallelization *)

let hosts = [| "altair"; "antares"; "arcturus"; "avior"; "bellatrix"; 
	       "betelgeuse"; "capella"; "deneb"; "dubhe"; "gacrux"; "mimosa";
	       "naos"; "polaris"; "procyon"; "rastaban"; "rigel"; "saiph"; 
	       "sirius"; "ursa"; |]
let nhosts = Array.length hosts 
let hfinger = ref 0
let cincr x = 
  x := (!x + 1) mod nhosts

let get_host () = 
  let host = hosts.(!hfinger) in
    cincr hfinger;
    host


(** Logging *)

let print_confusion confusion =
  let nclasses = Array.length confusion in
    printf "   ";
    for d = 0 to nclasses-1 do
      printf "%2d " d;
    done;
    printf "\n";

    for c = 0 to nclasses-1 do
      printf "%2d " c;
      for d = 0 to nclasses-1 do
	let conf = confusion.(c).(d) in
	  if conf = 0 then
	    printf "   "
	  else
	    printf "%2d " conf;
      done;
      printf "%2d " c;
      printf "\n";
    done;

    printf "   ";
    for d = 0 to nclasses-1 do
      printf "%2d " d;
    done;
    printf "\n"


      
let progress_bar n c =
  printf "%s\n%!" (String.make n c)

let progress c (f : 'a -> unit) bg en =
  let dinger () =
    printf "%c%!" c;
  in
    progress_bar (en - bg + 1) c;
    for i = bg to en do
      f i;
      dinger ();
    done;
    printf "\n"

let save_pgm_png img base = 
  Pnm.save_pgm img (sprintf "data/%s.pgm" base);
  let code = Sys.command (sprintf "convert data/%s.pgm data/%s.png" base base) in
    assert(code = 0);
    printf "Wrote to data/%s.pgm & data/%s.png\n%!" base base

let divider () : unit = 
  printf "%s\n%!" (String.make 100 '_')

let title (s : string) : unit = 
  divider ();
  printf "%s\n%!" s

(** Miscellaneous *)

(* unpack an option *)
let get (z : 'a option) : 'a = 
  match z with
      Some x -> x
    | None -> failwith "Got None, expected Some"

(* placeholder for unimplemented functions *)
let placeholder (name : string) (x : 'a) : unit = 
  ignore x; 
  raise (Not_Implemented name)

(* unique id *)
let uid () : int =
  (Oo.id (object end))

