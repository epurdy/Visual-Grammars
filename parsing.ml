open Util.Hops
open Printf
open Util.Misc

module S = Sdf
module G = Grammar
open Abstract

exception Parse_failure

let strict_parsing = false
let inside_verbose = false
let outside_verbose = false
let count_verbose = false
let viterbi_verbose = true

(* a in model domain, b in data domain *)
type parse_table = {
  na: int;
  nb: int;
  data: float array;  
  name: string;
  verbose: bool;
}

type sparse_inside_table = {
  qual : float;
  inside : parse_table;
}

type sparse_outside_table = {
  qual_ : float;
  outside : parse_table;
}

type 'b sparse_counts_table = {
  mutable qual__ : float;
  lexical_counts : float array;
  binary_counts : float array;
  mpchoices: (float * 'b) list array;
  mpmodes: (float * ('b option)) array;
(*   mpcounts: float array; *)
}

let new_parse_table na nb name verbose =
  printf "%d x %d parse table\n%!" na nb;
  {na = na;
   nb = nb;
   name = name;
   verbose = verbose;
   data = Array.init (na*nb) (fun i -> infinity)
  }

let get table (a,b) = table.data.(b*table.na + a)
let set table (a,b) v = table.data.(b*table.na + a) <- v
let linc table (a,b) v = 
  let idx = b*table.na + a in
    table.data.(idx) <- neglogadd v table.data.(idx)

let print_parse_table_entry table aname bname aid bid =
  if table.verbose then 
    printf "%s: %s ->> %s : %f\n" table.name aname bname (get table (aid,bid))

let inside gram family =
  let qual = ref infinity in
    (* chart maps (X,I) -> q, X=nonterminal, I=SDF interval *)
  let table = new_parse_table (gram.num_symbols()) (family.num_symbols()) "sparse inside" inside_verbose in

(*   let print_entry symbol scurve = *)
(*     print_parse_table_entry table  *)
(*       (gram.x.symbol_name symbol) (family.x.symbol_name scurve) *)
(*       symbol.G.sid scurve.S.sid *)
(*   in *)

    family.iter_symbols
      begin fun scurve -> 
	if family.x.lexical_ok scurve then begin
	  gram.iter_symbols
	    begin fun symbol -> 
	      let cost = G.lexical_cost symbol scurve in
		set table (symbol.G.sid, scurve.S.sid) cost;
(* 		print_entry symbol scurve;  *)
	    end
	end;

	if family.x.binary_ok scurve then begin
	  gram.iter_all_decompositions

	    begin fun symbol prod ->
	      if G.compatible symbol scurve then begin
		family.iter_decompositions scurve
		  begin fun dcomp ->
		    let cost = (G.binary_cost prod dcomp) +. 
		      (get table (prod.G.leftsid, dcomp.S.leftsid)) +.
		      (get table (prod.G.rightsid, dcomp.S.rightsid)) 
		    in
		      linc table (symbol.G.sid, scurve.S.sid) cost;
		  end;
(* 		print_entry symbol scurve; *)
	      end
	    end

	end;

	if family.x.goal_ok scurve then begin
	  let cost = S.goal_cost family scurve +. (get table (0, scurve.S.sid)) in
	    (** TODO is that zero a problem? *)
	    qual := neglogadd cost !qual;
	end

      end;

    if strict_parsing && not (isfinite !qual) then
      failwith "Could not parse!";
    
    {qual = !qual;
     inside = table}

let outside gram family inside =
  let itable = inside.inside in
  let table = new_parse_table (gram.num_symbols()) (family.num_symbols())
    "sparse outside" outside_verbose in

(*   let print_entry symbol scurve = *)
(*     print_parse_table_entry table  *)
(*       (gram.x.symbol_name symbol) (family.x.symbol_name scurve) *)
(*       symbol.G.sid scurve.S.sid *)
(*   in *)

    family.iter_symbols_rev
      begin fun scurve ->
	if family.x.goal_ok scurve then begin
	  let thisqual = S.goal_cost family scurve in
	    set table (0,scurve.S.sid) thisqual;
(* 	    print_entry (gram.start()) scurve *)
	end;

	gram.iter_all_decompositions
	  begin fun symbol prod ->
	    if G.compatible symbol scurve then begin

	      let lsym, rsym = gram.get_rhs prod in
		family.iter_decompositions scurve

		  begin fun dcomp ->
		    let lcurve, rcurve = family.get_rhs dcomp in
		    let proc asym acurve bsym bcurve =
		      let cost = (G.binary_cost prod dcomp) +.
			(get itable (asym.G.sid, acurve.S.sid)) +.
			(get table (symbol.G.sid, scurve.S.sid)) in
			
			linc table (bsym.G.sid, bcurve.S.sid) cost;
(* 			print_entry bsym bcurve; *)
		    in
		      proc lsym lcurve rsym rcurve;
		      proc rsym rcurve lsym lcurve;
		  end;
	    end
	  end
      end;

    {qual_ = inside.qual;
     outside = table}

(**
   more straightforward to combine soft counts + retraining?

   total lexical count for each symbol
   total count for each prod
   pruned midpoint list for each prod (more generic? can have pruned scurve list)
*)

let make_soft_counts nsyms ncomps =
  { qual__ = 0.;
    lexical_counts = Array.init nsyms (fun _ -> infinity);
    binary_counts = Array.init ncomps (fun _ -> infinity);
    mpchoices = Array.init ncomps (fun _ -> []);
(*     mpcounts = Array.init ncomps (fun _ -> infinity); *)
    mpmodes = Array.init ncomps (fun _ -> (infinity,None));
  }

let combine_soft_counts c1 c2 =
  let nlex = Array.length c1.lexical_counts in
  let nbin = Array.length c1.binary_counts in
    assert( nlex = Array.length c2.lexical_counts );
    assert( nbin = Array.length c2.binary_counts );
    { qual__ = c1.qual__ +. c2.qual__;
      lexical_counts = Array.init nlex
	(fun i -> neglogadd
	   c1.lexical_counts.(i) c2.lexical_counts.(i));
      binary_counts = Array.init nbin
	(fun i -> neglogadd
	   c1.binary_counts.(i) c2.binary_counts.(i));
      mpchoices = Array.init nbin
	(fun i -> c1.mpchoices.(i) @ c2.mpchoices.(i));
      mpmodes = Array.init nbin
	begin fun i -> 
	  let ct1,m1 = c1.mpmodes.(i) in
	  let ct2,m2 = c2.mpmodes.(i) in
	    if ct1 <= ct2 then
	      (ct1, m1)
	    else
	      (ct2, m2)
	end
    }
	

let soft_counts gram family inside outside sc =
  sc.qual__ <- sc.qual__ +. inside.qual;
  family.iter_symbols
    begin fun scurve ->
      if family.x.lexical_ok scurve then begin
	gram.iter_symbols
	  begin fun state ->
	    let count = 
	      if isfinite inside.qual then
		G.lexical_cost state scurve
		-. inside.qual
		+. (get outside.outside (state.G.sid, scurve.S.sid))
	      else
		infinity
	    in
	      sc.lexical_counts.(state.G.sid) <- neglogadd sc.lexical_counts.(state.G.sid) count;
	      (* 	      if count_verbose then printf "soft_counts: state %d -> scurve %d=(%d,%d) : ~%f~\n%!"  *)
	      (* 		state.G.sid scurve.S.sid scurve.S.first scurve.S.last count; *)
	  end
      end;

      if family.x.binary_ok scurve then begin
	family.iter_decompositions scurve
	  begin fun dcomp ->
	    gram.iter_all_decompositions
	      begin fun state prod ->
		let count =
		  if isfinite inside.qual then
		    (G.binary_cost prod dcomp)
		    -. inside.qual
		    +. (get outside.outside (state.G.sid, scurve.S.sid))
		    +. (get inside.inside  (prod.G.leftsid, dcomp.S.leftsid))
		    +. (get inside.inside  (prod.G.rightsid, dcomp.S.rightsid))
		  else
		    infinity
		in
		  sc.binary_counts.(prod.G.cid) <- neglogadd sc.binary_counts.(prod.G.cid) count;

		  let modecount, _ = sc.mpmodes.(prod.G.cid) in

		    if count < modecount +. 10. then begin
		      sc.mpchoices.(prod.G.cid) <- (count, dcomp.S.cdata) :: sc.mpchoices.(prod.G.cid);
		    end;

		    if count <  modecount then begin
		      sc.mpmodes.(prod.G.cid) <- (count, Some dcomp.S.cdata);
		    end;

	      (* 		  printf "nmps(%d) = %d\n%!" prod.G.cid (List.length sc.mpchoices.(prod.G.cid)); *)
	      end

	  end
      end;
    end;

  printf "QUAL %f\n%!" outside.qual_
  

let sparse_inside_outside gram family sc_table =
  let inside_table = inside gram family in
  let outside_table = outside gram family inside_table in
    soft_counts gram family inside_table outside_table sc_table


let sparse_parse_cost gram family = 
  let table = inside gram family in
    table.qual



let viterbi gram family =
  let qual = ref infinity in
    (* chart maps (X,I) -> q, X=nonterminal, I=SDF interval *)
  let table = new_parse_table (gram.num_symbols()) (family.num_symbols()) 
    "viterbi" viterbi_verbose in
  let toplevel = ref None in
  let foo = mkhash 100 in
  

    (*   let print_entry symbol scurve = *)
    (*     print_parse_table_entry table  *)
    (*       (gram.x.symbol_name symbol) (family.x.symbol_name scurve) *)
    (*       symbol.G.sid scurve.S.sid *)
    (*   in *)

    family.iter_symbols
      begin fun scurve -> 
	if family.x.lexical_ok scurve then begin
	  gram.iter_symbols
	    begin fun symbol -> 
	      let cost = G.lexical_cost symbol scurve in
		set table (symbol.G.sid, scurve.S.sid) cost;
		(* 		print_entry symbol scurve;  *)
	    end
	end;

	if family.x.binary_ok scurve then begin
	  gram.iter_all_decompositions

	    begin fun symbol prod ->
	      if G.compatible symbol scurve then begin
		family.iter_decompositions scurve
		  begin fun dcomp ->
		    let cost = (G.binary_cost prod dcomp) +. 
		      (get table (prod.G.leftsid, dcomp.S.leftsid)) +.
		      (get table (prod.G.rightsid, dcomp.S.rightsid)) 
		    in
		      if cost < (get table (symbol.G.sid, scurve.S.sid)) then begin
			set table (symbol.G.sid, scurve.S.sid) cost;
			foo << ((symbol.G.sid, scurve.S.sid),
				(prod.G.cid, dcomp.S.cid));
		      end
		  end;
		(* 		print_entry symbol scurve; *)
	      end
	    end

	end;

	if family.x.goal_ok scurve then begin
	  let cost = S.goal_cost family scurve +. (get table (0, scurve.S.sid)) in
	    (** TODO is that zero a problem? *)
	    if cost < !qual then begin
	      toplevel := Some (0, scurve.S.sid);
	      qual := cost;
	    end
	end

      end;

    if strict_parsing && not (isfinite !qual) then
      failwith "Could not parse!";

    let pairs = ref [] in
    let rec visit (symbolid, scurveid) = 
      pairs := (symbolid, scurveid) :: !pairs;
      let symbol, scurve = gram.get_symbol symbolid, family.get_symbol scurveid in
	if foo >>? (symbol.G.sid, scurve.S.sid) then begin
	  let prodid, dcompid = foo >> (symbol.G.sid, scurve.S.sid) in
	  let prod, dcomp = gram.get_composition prodid, 
	    family.get_composition dcompid in
	    visit (prod.G.leftsid, dcomp.S.leftsid);
	    visit (prod.G.rightsid, dcomp.S.rightsid);
	end
    in
    let toplevel = 
      match !toplevel with Some x -> x
	| None -> failwith "Could not parse! v2";
    in
      visit toplevel;

      !qual, !pairs
