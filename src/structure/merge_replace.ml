open Util.Misc
open Printf
open Parsing
open Abstract
open Grammar

type merge_replace_build = {
  families: Sdf.family array;
}

type merge_replace_xform = 
    Merge of int * int
  | Replace of int * int

type merge_replace_internal = {
  insides: parse_table array;
  outsides: parse_table array;
  merge_xforms: merge_replace_xform array;
  merge_scores: float array;
  merge_valid: bool array;
  replace_xforms: merge_replace_xform array;
  replace_scores: float array;
  replace_valid: bool array;
}

let print_merge_replace = function
    Merge (i,j) -> printf "Merge(%d,%d)" i j
  | Replace (i,j) -> printf "Replace(%d,%d)" i j


let kl_sum tabs psym qsym =
  let tot = ref 0. in
    Array.iter
      begin fun tab ->
	for scid = 0 to tab.nb-1 do 
	  let nlp = (get tab (psym.sid, scid)) in
	  let nlq = (get tab (qsym.sid, scid)) in
	  let term = 
	    if nlp < infinity then
	      if nlq < infinity then
		(exp (-. nlp)) *. (nlq -. nlp)
	      else
		infinity
	    else
	      0.
	  in
	    tot := !tot +. term;
	done
      end
      tabs;
    !tot

let merge_replace_initialize (gram: Grammar.grammar option) build_data = 
  let gram = Util.Misc.get gram in
  let nsymbols = Frozen.num_symbols gram in
  let nmerges = nsymbols * (nsymbols-1) / 2 in
  let nreplaces = nsymbols * (nsymbols-1) in
  let insides = Array.map (fun fam -> Parsing.inside gram fam Models.EM.strat) build_data.families in
  let outsides = Array.map (fun fam -> Parsing.outside gram fam Models.EM.strat) build_data.families in
  (* let outsides = Array.mapi (fun i fam -> Parsing.outside insides.(i) gram fam) build_data.families in *)

  let xforms = Array.init (nmerges + nreplaces) (fun _ -> (None, infinity)) in

  let i = ref 0 in

  let _ =
    Frozen.iter_symbols gram
      begin fun psym ->
	Frozen.iter_symbols gram
	  begin fun qsym ->
	    if psym.sid != qsym.sid then begin
	      let replace_score = kl_sum insides psym qsym in
		xforms.(!i) <- (Some (Replace (psym.sid, qsym.sid)), replace_score);
		incr i;
	    end;

	    if psym.sid < qsym.sid then begin
	      let merge_score = kl_sum outsides psym qsym in
		xforms.(!i) <- (Some (Merge (psym.sid, qsym.sid)), merge_score);
		incr i;
	    end;

	  end
      end;
  in
  let xforms = Array.map (fun (op,qual) -> (get op, qual)) xforms in
    Array.sort (fun (op1, qual1) (op2, qual2) -> compare qual1 qual2) xforms;

    {
      insides = insides;
      outsides = outsides;

      xforms = Array.map (fun (op,_) -> op) xforms;
      scores = Array.map (fun (_,qual) -> qual) xforms;
      valid = Array.map (fun _ -> true) xforms;
    }

let merge_replace_suggest gram internals =
  (* let gram = get gram in *)
  let i = ref 0 in
    while !i < (Array.length internals.valid) && (not internals.valid(!i)) do 
      incr i;
    done;
    if (!i < Array.length internals.valid) && internals.valid(!i) then 
      Some internals.xforms.(!i)
    else
      None

let merge_replace_apply xform gram =
  let gram = Grammar.copy (get gram) in
  let reachable = check_reachability gram in
    begin match xform with
	Merge (pid,qid) ->
	  if reachable.(pid) && reachable.(qid) then begin
	    printf "MERGE %d with %d\n%!" pid qid;
	    Grammar.merge_symbols_left 
	      gram (gram.get_symbol qid) (gram.get_symbol pid);
	    ensure_reachability gram;
	    
	    gram, true
	  end
	  else begin
	    gram, false
	  end
	  
      | Replace (pid,qid) ->
	  if reachable.(pid) && reachable.(qid) then begin
	    printf "REPLACE %d with %d\n%!" pid qid;
	    Grammar.replace_symbol 
	      gram (gram.get_symbol qid) (gram.get_symbol pid);
	    ensure_reachability gram;
	    
	    gram, true
	  end
	  else begin
	    gram, false
	  end
    end


let merge_replace_print internals =
  for i = 0 to min (Array.length internals.xforms-1) 9 do 
    print_merge_replace internals.xforms.(i); 
  done

let merge_replace_heuristic = {
  initialize = merge_replace_initialize;
  suggest = merge_replace_suggest;
  apply = merge_replace_apply;
  print = merge_replace_print;
}

(* let replacement ?(shrink=0.1) gram families = *)

(*   let insides = List.map (fun fam -> Parsing.inside gram fam) families in *)
(*   let divergence = new_parse_table (gram.num_symbols()) (gram.num_symbols()) "" true in *)
(*   let done_ = ref false in *)
(*   let replace_qualities = *)
(*     let stuff = ref [] in *)
(*       gram.iter_symbols  *)
(* 	begin fun psym -> *)
(* 	  gram.iter_symbols  *)
(* 	    begin fun qsym -> *)
(* 	      if psym.sid != qsym.sid then begin *)
(* 		let tot = ref 0. in *)
(* 		  List.iter *)
(* 		    begin fun inside -> *)
(* 		      for scid = 0 to inside.inside.nb-1 do *)
(* 			let nlp = (get inside.inside (psym.sid, scid)) in *)
(* 			let nlq = (get inside.inside (qsym.sid, scid)) in *)
(* 			let term =  *)
(* 			  if nlp < infinity then *)
(* 			    if nlq < infinity then *)
(* 			      (exp (-. nlp)) *. (nlq -. nlp)  *)
(* 			    else *)
(* 			      infinity *)
(* 			  else  *)
(* 			    0. *)
(* 			in *)
(* 			  (\* printf "%d,%d nlp=%f, nlq=%f, term=%f\n%!" psym.sid qsym.sid nlp nlq term; *\) *)
(* 			  tot := !tot +. term; *)
(* 		      done *)
(* 		    end *)
(* 		    insides; *)
(* 		  stuff := (!tot, (psym.sid, qsym.sid)) :: !stuff; *)
(* 		  set divergence (psym.sid, qsym.sid) !tot; *)
(* (\* 		  printf "%f       psym=%d, qsym=%d, div=%f\n%!" !tot psym.sid qsym.sid !tot; *\) *)
(* 	      end *)
(* 	    end *)
(* 	end; *)
(*       let stuff = List.sort (fun (d1,(p1,q1)) (d2,(p2,q2)) -> compare d1 d2) ! stuff in *)
(* 	ref stuff       *)
(*   in *)
    
    (* while not !done_ do *)
    (*   let reachable = check_reachability gram in *)
    (*   let nreachable = Array.fold_left (fun n b -> if b then n+1 else n) 0 reachable in *)
    (*   let (qual, (pid,qid)) = List.hd !replace_qualities in *)
    (* 	replace_qualities := List.tl !replace_qualities; *)
    (* 	printf "psym=%d, qsym=%d, div=%f\n%!" pid qid qual; *)
	
    (* 	if nreachable < round((1.-.shrink) *. float_of_int(gram.num_symbols())) then *)
    (* 	  done_ := true; *)
    (* 	if qual == infinity then  *)
    (* 	  done_ := true; *)
	
    (* 	if not !done_ then begin *)
    (* 	  if reachable.(pid) && reachable.(qid) then begin *)
    (* 	    printf "REPLACE %d with %d qual=%f\n%!" pid qid qual; *)
    (* 	    Grammar.replace_symbol gram (gram.get_symbol qid) (gram.get_symbol pid); *)

    (* 	  end *)
    (* 	end; *)

    (* 	if List.length !replace_qualities = 0 then *)
    (* 	  done_ := true; *)
    (* done; *)

    (* ensure_reachability gram *)

