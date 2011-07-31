open Printf
open Util.Misc
open Util.Hops
open Geometry

open Abstract

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   It is >>> VERY IMPORTANT <<< that the data inside sdata and cdata
   be immutable. The abstract grammar code is written under this
   assumption, and violating it will cause shared reference bugs.

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  *)
type sdata = {
  closed: bool;
  straightprob: float;  
  straightcost: float;  
  curve: Curve.t;
}
type ssg_geom_data = Improper | Parzen of Parzen.model 
		     | Watson of Watson.distro
type cdata = {
  prob: float;
  cost: float;
  geom: ssg_geom_data;
  ocurve: Curve.t;
  lcurve: Curve.t;
  rcurve: Curve.t;
}

type gdata = unit

type grammar = (sdata, cdata, gdata) frozen_grammar

let symbol_name_long sym = sprintf "State %d   [->l (p=%f)]" 
  sym.sid sym.sdata.straightprob

let print_grammar gram = Frozen.print symbol_name_long gram

let renormalize gram sym =
  let nltot = List.fold_right
    (fun dcomp nltot -> neglogadd dcomp.cdata.cost nltot) 
    (Frozen.get_decompositions gram sym) sym.sdata.straightcost in

  let straightcost = sym.sdata.straightcost -. nltot in
    sym.sdata <- {
      closed = sym.sdata.closed;
      curve = sym.sdata.curve;
      straightcost = straightcost;
      straightprob = exp (-. straightcost);
    };

    Frozen.iter_decompositions gram sym
      begin fun dcomp -> 
	let cost = dcomp.cdata.cost -. nltot in
	dcomp.cdata <- {
	  cost = cost;
	  prob = exp (-. cost);
	  lcurve = dcomp.cdata.lcurve;
	  rcurve = dcomp.cdata.rcurve;
	  ocurve = dcomp.cdata.ocurve;
	  geom = dcomp.cdata.geom;
	}
      end

let renormalize_binary gram sym =
  let nltot = List.fold_right
    (fun dcomp nltot -> neglogadd dcomp.cdata.cost nltot) 
    (Frozen.get_decompositions gram sym) sym.sdata.straightcost in

    Frozen.iter_decompositions gram sym
      begin fun dcomp -> 
	let cost = dcomp.cdata.cost -. nltot in
	  dcomp.cdata <- {
	    cost = cost;
	    prob = exp (-. cost);
	    lcurve = dcomp.cdata.lcurve;
	    rcurve = dcomp.cdata.rcurve;
	    ocurve = dcomp.cdata.ocurve;
	    geom = dcomp.cdata.geom;
	  }
      end

(* Take in an sdf representing a decomposed curve, and make a grammar
   from it. The user must supply [sdata_maker] and [cdata_maker],
   which map subcurves to symbol metadata and curve compositions to
   composition metadata, respectively. [start_sdata] is the symbol
   metadata to be attached to the start symbol of the grammar.  

   Note that this function does not create L->LL rules.
*)
let grammar_of_family family start_sdata sdata_maker cdata_maker =
  let gram = new_live_grammar () in
  let start = make_new_symbol gram start_sdata true in
  let lookup = mkhash 100 in

    Frozen.iter_symbols family
      begin fun scurve ->
	match (sdata_maker scurve) with 
	    None ->
	      lookup << (scurve.sid, start);
	  | Some sdata ->
	      lookup << (scurve.sid, make_new_symbol gram sdata false);
      end;

    iter_all_decompositions family
      begin fun scurve dcomp ->
	let cdata = cdata_maker scurve dcomp in
	let sym = lookup >> scurve.sid in
	let left = lookup >> dcomp.leftsid in
	let right = lookup >> dcomp.rightsid in
	  imake_new_composition gram
	    sym.sid (left.sid,right.sid) cdata
      end;

    let gram = finalize gram in
      Frozen.iter_symbols gram
	begin fun sym ->
	  renormalize_binary gram sym;
	end;
      gram

(************************************************************)

let merge_symbols_left gram s1 s2 ~renorm =
  let straightcost = neglogadd s1.sdata.straightcost s2.sdata.straightcost in
    printf "TODO: not merging equivalent rules!\n";
    assert(s1.sdata.closed = s2.sdata.closed);
    s2.sdata <- {
      closed = false;
      straightcost = s2.sdata.straightcost;
      straightprob = s2.sdata.straightprob;
      curve = s2.sdata.curve;
    };
    s1.sdata <- {
      closed = s1.sdata.closed;
      straightcost = straightcost;
      straightprob = exp (-. straightcost);      
      curve = s1.sdata.curve;
    };
    s1.dcompids <- s1.dcompids @ s2.dcompids;
    s1.lcompids <- s1.lcompids @ s2.lcompids;
    s1.rcompids <- s1.rcompids @ s2.rcompids;
    Frozen.iter_decompositions gram s2 (fun dcomp -> dcomp.topsid <- s1.sid);
    Frozen.iter_left_compositions gram s2 (fun dcomp -> dcomp.leftsid <- s1.sid);
    Frozen.iter_right_compositions gram s2 (fun dcomp -> dcomp.rightsid <- s1.sid);
    s2.dcompids <- [];
    s2.lcompids <- [];
    s2.rcompids <- [];
    if renorm then
      renormalize gram s1

(* replace every occurence of s2 with s1 *)
let replace_symbol gram s1 s2 =
  assert(s1.sdata.closed = s2.sdata.closed);
  s2.sdata <- {
    closed = false;
    straightcost = s2.sdata.straightcost;
    straightprob = s2.sdata.straightprob;
    curve = s2.sdata.curve;
  };
  s1.lcompids <- s1.lcompids @ s2.lcompids;
  s1.rcompids <- s1.rcompids @ s2.rcompids;
  Frozen.iter_left_compositions gram s2 (fun dcomp -> dcomp.leftsid <- s1.sid);
  Frozen.iter_right_compositions gram s2 (fun dcomp -> dcomp.rightsid <- s1.sid);
  s2.dcompids <- [];
  s2.lcompids <- [];
  s2.rcompids <- []

let merge_leaves gram =
  let leaves = ref [] in
  let _ = Array.iter
    begin fun sym ->
      Frozen.iter_decompositions gram sym
	begin fun comp ->
	  if comp.leftsid = sym.sid then
	    leaves := sym :: !leaves;
	end
    end
    gram.f_symbols in
  let leaves = List.rev !leaves in
  let leaf, leaves = List.hd leaves, List.tl leaves in
    List.iter (fun sym -> replace_symbol gram leaf sym) leaves;
    gram

let merge_tops grams =
  let gram = merge_frozen_grams grams grams.(0).f_gdata in
  let starts = ref [] in
  let _ = Array.iter 
    begin fun sym ->
      if sym.sdata.closed then
	starts := sym :: !starts
    end
    gram.f_symbols in
  let starts = List.rev !starts in
    printf "STARTS\n";
    List.iter (fun s -> printf "%s " (symbol_name s)) starts;
    printf "\n";
  let start, starts = List.hd starts, List.tl starts in
    print_grammar gram;
    List.iter (fun sym -> merge_symbols_left gram start sym ~renorm:false) starts;
    renormalize gram start;
    gram

let prune gram minp =
  filter_compositions gram (fun prod -> prod.cdata.prob >= minp)

(************************************************************)

let sampling_thresh = 0.4 (* 100.0 *)

let sample_prod prod p q = 
  match prod.cdata.geom with 
      Parzen geom -> 
	let shape = Parzen.sample geom in
	let r = Shape.place shape p q in
	  (p,r,q)
    | Improper ->
	(p,q,p)
    | Watson geom ->
	let shape = Watson.sample geom in
	let r = Shape.place shape p q in
	  (p,r,q)

(** sample curve starting at a particular state *)
let rec sample_state gram state p q =
  let d2 = Geometry.dist2 p q in
    assert(d2 = d2); (* nan check *)
    assert(d2 < infinity);
    assert(d2 > 0.);
(*     printf "s%d ->> (%s,%s)\n" state.sid *)
(*       (Geometry.string_of_cpt p) *)
(*       (Geometry.string_of_cpt q); *)
    if (Geometry.dist2 p q < sampling_thresh) ||
      (Random.float 1.0 < state.sdata.straightprob) then
	[p; q]
    else
      let productions = Array.of_list (Frozen.get_decompositions gram state) in
	if Array.length productions = 0 then begin 
	  [p; q]
	end
	else begin
	  let dist = Array.map (fun prod -> prod.cdata.prob) productions in
	  let i = Sample.choose dist 0 (Array.length dist - 1) in
	  let prod = productions.(i) in
	  let left,right = Frozen.get_rhs gram prod in
	  let (p',r',q') = sample_prod prod p q in
	    sample_state gram left p' r' @ sample_state gram right r' q'    
	end

let sample gram p q =
  let rv = sample_state gram (Frozen.start gram) p q in
    Array.of_list rv

let sample_counts gram =
  let therules = ref [] in
  let counts = mkhash 100 in
  let hinc pid =
    let x = counts >>! (pid, 0) in
      counts << (pid, x+1)
  in

  let maxdepth = 10 in
  let ntrials = 100 in
  let ntake = 20 in

  let rec sample_state_counts state depth =
    if (depth < maxdepth) &&
      (Random.float 1.0 >= state.sdata.straightprob) then
	let productions = Array.of_list (Frozen.get_decompositions gram state) in
	let dist = Array.map (fun prod -> prod.cdata.prob) productions in
	let prod = productions.(Sample.choose_any dist) in
	let left, right = Frozen.get_rhs gram prod in
	  hinc prod.cid;
	  sample_state_counts left (depth + 1);
	  sample_state_counts right (depth + 1);
  in	    

    for i = 1 to ntrials do
      sample_state_counts (Frozen.start gram) 0;
    done;

    Hashtbl.iter 
      begin fun cid count ->
	therules := (count, cid) :: !therules;
      end
      counts;
    (* sort in decreasing order *)
    therules := List.sort (fun (c1,_) (c2,_) -> compare c2 c1) !therules;

    take_best ntake !therules
