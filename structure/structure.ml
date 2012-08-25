open Util.Misc
open Printf
open Parsing
open Abstract
open Grammar


let replacement ?(shrink=0.1) gram families =

  let insides = List.map (fun fam -> Parsing.inside gram fam) families in
  let divergence = new_parse_table (gram.num_symbols()) (gram.num_symbols()) "" true in
  let done_ = ref false in
  let replace_qualities =
    let stuff = ref [] in
      gram.iter_symbols 
	begin fun psym ->
	  gram.iter_symbols 
	    begin fun qsym ->
	      if psym.sid != qsym.sid then begin
		let tot = ref 0. in
		  List.iter
		    begin fun inside ->
		      for scid = 0 to inside.inside.nb-1 do
			let nlp = (get inside.inside (psym.sid, scid)) in
			let nlq = (get inside.inside (qsym.sid, scid)) in
			let term = 
			  if nlp < infinity then
			    if nlq < infinity then
			      (exp (-. nlp)) *. (nlq -. nlp) 
			    else
			      infinity
			  else 
			    0.
			in
			  (* printf "%d,%d nlp=%f, nlq=%f, term=%f\n%!" psym.sid qsym.sid nlp nlq term; *)
			  tot := !tot +. term;
		      done
		    end
		    insides;
		  stuff := (!tot, (psym.sid, qsym.sid)) :: !stuff;
		  set divergence (psym.sid, qsym.sid) !tot;
(* 		  printf "%f       psym=%d, qsym=%d, div=%f\n%!" !tot psym.sid qsym.sid !tot; *)
	      end
	    end
	end;
      let stuff = List.sort (fun (d1,(p1,q1)) (d2,(p2,q2)) -> compare d1 d2) ! stuff in
	ref stuff      
  in
    
    while not !done_ do
      let reachable = check_reachability gram in
      let nreachable = Array.fold_left (fun n b -> if b then n+1 else n) 0 reachable in
      let (qual, (pid,qid)) = List.hd !replace_qualities in
	replace_qualities := List.tl !replace_qualities;
	printf "psym=%d, qsym=%d, div=%f\n%!" pid qid qual;
	
	if nreachable < round((1.-.shrink) *. float_of_int(gram.num_symbols())) then
	  done_ := true;
	if qual == infinity then 
	  done_ := true;
	
	if not !done_ then begin
	  if reachable.(pid) && reachable.(qid) then begin
	    printf "REPLACE %d with %d qual=%f\n%!" pid qid qual;
	    Grammar.replace_symbol gram (gram.get_symbol qid) (gram.get_symbol pid);

	  end
	end;

	if List.length !replace_qualities = 0 then
	  done_ := true;
    done;

    ensure_reachability gram


      
      
