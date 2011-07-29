open Util.Hops
open Util.Misc
open Printf
open Scanf

open Abstract

type sid = int
type cid = int
type index = int

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   It is >>> VERY IMPORTANT <<< that the data inside sdata and cdata
   be immutable. The abstract grammar code is written under this
   assumption, and violating it will cause shared reference bugs.

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  *)

type sdata = {
  first : index;
  last : index;
  len : int;
}
type cdata = {
  bg : index;
  md : index;
  en : index;
}
type sdf_gdata = {n: int}

let is_closed scurve = 
  (scurve.sdata.first = scurve.sdata.last)

let symbol_name scurve = 
  sprintf "[%d, %d]" scurve.first scurve.last
let symbol_name_long scurve = symbol_name scurve

let composition_name comp = 
  sprintf "[%d, %d][%d, %d]" comp.bg comp.md comp.md comp.en

type live_family = {
  gram: (sdata,cdata,sdf_gdata) live_grammar;
  sclookup: (index*index,sdata symbol) hash;
  make_new_scurve: int * int -> int -> sdata symbol;
  make_new_comp: int * int * int -> cdata composition;
  imake_new_scurve: int * int -> int -> unit;
  imake_new_comp: int * int * int -> unit;
}    
type ('a, 'b, 'c) family = ('a, 'b, 'c) Abstract.frozen_grammar

let make_live_family n nsyms =
  let gram = new_live_grammar {n=n}  in
  let sclookup = mkhash nsyms in

  let make_new_scurve (i,k) len =
    let sdata = {
      first = i;
      last = k;
      len = len;
    } in 
    let scurve = make_new_symbol gram sdata (i=k) in
      sclookup << ((i,k), scurve);
      scurve
  in

  let imake_new_scurve (i,k) len =
    ignore (make_new_scurve (i,k) len)
  in

  let make_new_comp (i, j, k) =    
    let top = sclookup >> (i,k) in
    let left = sclookup >> (i,j) in
    let right = sclookup >> (j,k) in
    let cdata = {
      bg = i;
      md = j;
      en = k;
    } in
    let comp = make_new_composition 
      gram top.sid (left.sid, right.sid) cdata in
      comp
  in

  let imake_new_comp (i, j, k) =    
    ignore (make_new_comp (i, j, k))
  in

    {gram = gram;
     sclookup = sclookup;
     make_new_scurve = make_new_scurve;
     make_new_comp = make_new_comp;
     imake_new_scurve = imake_new_scurve;
     imake_new_comp = imake_new_comp;
    }

(***************************************************************************************************)
(* BUILDING FAMILIES                                                                               *)
(***************************************************************************************************)

(** Return an array [arr] of arrays. [arr.(i)] should be an array
    of integers representing indices in the original curve, where
    [arr.(0)] gives the whole curve, and [arr.(i+1)] is
    a subsampling of [arr.(i)]. 

    We enforce the following properties: Every level has at most half
    as many points as the previous level. The top level has at most
    [k] points, so that closed curves are possible.

    Currently the subsampling method is very simple-minded and
    produces some irregular subsampling at the ends.
*)
let make_coarse_curves n k =
  let thiscurve = ref (Array.init n (fun i -> i)) in
  let curves = ref [!thiscurve] in
    while Array.length !thiscurve > k do
      let newcurve = ref [] in
      let finger = ref 0 in
	while !finger < Array.length !thiscurve do
	  newcurve := !thiscurve.(!finger) :: !newcurve;
	  finger := !finger + 2;
	done;
	thiscurve := Array.of_list (List.rev !newcurve);
	curves := !thiscurve :: !curves;
    done;
    Array.of_list (List.rev !curves)

let print_levels curves = 
  Array.iteri
    begin fun i c ->
      printf "Level #%d:\n" i;
      Array.iter (fun i -> printf "%d " i;) c;
      printf "\n---\n";
    end
    curves

let load_family fname = 
  let chan = open_in fname in
  let line = input_line chan in
  let n = 
    let n = ref 0 in
      sscanf line "%d" (fun x -> n := x);
      !n
  in
  let lfamily = make_live_family n (n*n) in

  let thelen i j = 
    let x = j - i + n + 1 in
      (x mod n) - 1
  in

  let seen = mkhash 100 in
  let add (i,j) =
    if not (seen >>? (i,j)) then begin
      seen << ((i,j), ());
      lfamily.imake_new_scurve (i,j) (thelen i j);
    end
  in

    begin try
      let proc ax ay bx by cx cy =
	assert(ax == bx);
	assert(ay == cy);
	assert(by == cx);
	add (ax,ay);
	add (bx,by);
	add (cx,cy);
	lfamily.imake_new_comp (ax, by, ay);
      in
	while true do
	  let line = input_line chan in
	    sscanf line " [%d,%d] -> [%d,%d] [%d,%d]" proc;
	done
    with End_of_file -> ()
    end;

    finalize lfamily.gram


let make_full_family n =
  let lfamily = make_live_family n (n*n) in
    for len = 1 to n do
      for i = 0 to (n-1) do
	let k' = (i + len) in
	  lfamily.imake_new_scurve (i, k' mod n) len;
	  for j' = i+1 to k'-1 do
	    lfamily.imake_new_comp (i, j' mod n, k' mod n);
	  done;
      done;
    done;
    finalize lfamily.gram

let make_restricted_family n scurves comps =
  let lfamily = make_live_family n (List.length scurves) in
    for len = 1 to n do
      for i = 0 to (n-1) do
	let k' = (i + len) in
	let k = k' mod n in
	  if List.mem (i,k) scurves then begin
	    lfamily.imake_new_scurve (i, k) len;
	    for j' = i+1 to k'-1 do
	      let j = j' mod n in
		if List.mem (i,j,k) comps then 
		  lfamily.imake_new_comp (i, j, k);
	    done;
	  end;
      done;
    done;
    finalize lfamily.gram

let make_sparse_family n k =
  let lfamily = make_live_family n n in
  let coarse_curves = make_coarse_curves n k in
    Array.iteri
      begin fun lvl thecurve ->
	let lvl_length = Array.length thecurve in
	let maxlen_lvl = min k lvl_length in
	let minlen_lvl = if lvl = 0 then 1
	else (k/2) + 1 in

	  (* TODO: Make sure the below comment has been resolved. I
	     think it has been. *)
	  (* 0 16 32 in [0,40] with k=2 is a problem, since 16 doesn't make it to the top... *)

	  for len_lvl = minlen_lvl to maxlen_lvl do
	    for i_lvl = 0 to lvl_length-1 do

	      let i = thecurve.(i_lvl) in
	      let k = thecurve.((i_lvl + len_lvl) mod lvl_length) in
	      let len = ((k - i + n + n - 1) mod n) + 1 in

		lfamily.imake_new_scurve (i, k) len;
		for j_lvl' = i_lvl + 1 to i_lvl + len_lvl - 1 do
		  let j = thecurve.(j_lvl' mod lvl_length) in

		    lfamily.imake_new_comp (i, j, k);
		done;

	    done
	  done
      end
      coarse_curves;
    let family = finalize lfamily.gram in
    let family = ensure_reachability family in
      family

