open Util.Hops
open Printf
open Util.Misc

module S = Sdf
module G = Grammar
open Abstract

exception Parse_failure

let strict_parsing = true
let inside_verbose = false
let outside_verbose = false
let count_verbose = false
let viterbi_verbose = false

type ('mod_sym,'mod_comp,'tgt_sym,'tgt_comp,'tgt_glob) strategy = {
  lexical_ok :  'tgt_sym symbol -> bool;
  lexical_cost : 'mod_sym symbol -> 'tgt_sym symbol -> float;
  binary_ok : 'tgt_sym symbol -> bool;
  binary_cost : 'mod_comp composition -> 'tgt_comp composition -> float;
  goal_ok : 'tgt_sym symbol -> bool;
  goal_cost : 'tgt_glob -> 'tgt_sym symbol -> float;
  compatible : 'mod_sym symbol -> 'tgt_sym symbol -> bool;
  getshape : 'tgt_comp -> Shape.shape;
  fit_midpoint_distro : 'mod_comp composition -> (float * Shape.shape) list -> Shape.shape option ->
				       'mod_comp;

  tgt_sym_namer : 'tgt_sym symbol -> string;
}


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


let inside gram family strat =
  let qual = ref infinity in
    (* chart maps (X,I) -> q, X=nonterminal, I=SDF interval *)
  let table = new_parse_table (Frozen.num_symbols gram)
    (Frozen.num_symbols family) "sparse inside" inside_verbose in

  let print_entry symbol scurve =
    print_parse_table_entry table
      (symbol_name symbol) (strat.tgt_sym_namer scurve)
      symbol.sid scurve.sid
  in

    Frozen.iter_symbols family
      begin fun scurve -> 
	if strat.lexical_ok scurve then begin
	  Frozen.iter_symbols gram
	    begin fun symbol -> 
	      let cost = strat.lexical_cost symbol scurve in
		set table (symbol.sid, scurve.sid) cost;
		print_entry symbol scurve;
	    end
	end;

	if strat.binary_ok scurve then begin
	  iter_all_decompositions gram

	    begin fun symbol prod ->
	      if strat.compatible symbol scurve then begin
		Frozen.iter_decompositions family scurve
		  begin fun dcomp ->
		    let cost = (strat.binary_cost prod dcomp) +. 
		      (get table (prod.leftsid, dcomp.leftsid)) +.
		      (get table (prod.rightsid, dcomp.rightsid)) 
		    in
		      linc table (symbol.sid, scurve.sid) cost;
		  end;
		print_entry symbol scurve;
	      end
	    end

	end;

	if strat.goal_ok scurve then begin
	  let cost = strat.goal_cost family.f_gdata scurve +. (get table (0, scurve.sid)) in
	    (** TODO is that zero a problem? *)
	    qual := neglogadd cost !qual;
	end

      end;

    if strict_parsing && not (isfinite !qual) then
      failwith "Could not parse!";
    
    {qual = !qual;
     inside = table}

let outside gram family inside strat =
  
  let itable = inside.inside in
  let table = new_parse_table
    (Frozen.num_symbols gram) (Frozen.num_symbols family)
    "sparse outside" outside_verbose in

(*   let print_entry symbol scurve = *)
(*     print_parse_table_entry table  *)
(*       (gram.x.symbol_name symbol) (family.x.symbol_name scurve) *)
(*       symbol.G.sid scurve.S.sid *)
(*   in *)

    Frozen.iter_symbols_rev family
      begin fun scurve ->
	if strat.goal_ok scurve then begin
	  let thisqual = strat.goal_cost family.f_gdata scurve in
	    set table (0,scurve.sid) thisqual;
(* 	    print_entry (gram.start()) scurve *)
	end;

	iter_all_decompositions gram
	  begin fun symbol prod ->
	    if strat.compatible symbol scurve then begin

	      let lsym, rsym = Frozen.get_rhs gram prod in
		Frozen.iter_decompositions family scurve

		  begin fun dcomp ->
		    let lcurve, rcurve = Frozen.get_rhs family dcomp in
		    let proc asym acurve bsym bcurve =
		      let cost = (strat.binary_cost prod dcomp) +.
			(get itable (asym.sid, acurve.sid)) +.
			(get table (symbol.sid, scurve.sid)) in
			
			linc table (bsym.sid, bcurve.sid) cost;
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
	

let soft_counts gram family inside outside sc strat =
  sc.qual__ <- sc.qual__ +. inside.qual;
  Frozen.iter_symbols family
    begin fun scurve ->
      if strat.lexical_ok scurve then begin
	Frozen.iter_symbols gram
	  begin fun state ->
	    let count = 
	      if isfinite inside.qual then
		strat.lexical_cost state scurve
		-. inside.qual
		+. (get outside.outside (state.sid, scurve.sid))
	      else
		infinity
	    in
	      sc.lexical_counts.(state.sid) <- neglogadd sc.lexical_counts.(state.sid) count;
	      (* 	      if count_verbose then printf "soft_counts: state %d -> scurve %d=(%d,%d) : ~%f~\n%!"  *)
	      (* 		state.G.sid scurve.S.sid scurve.S.first scurve.S.last count; *)
	  end
      end;

      if strat.binary_ok scurve then begin
	Frozen.iter_decompositions family scurve
	  begin fun dcomp ->
	    iter_all_decompositions gram
	      begin fun state prod ->
		let count =
		  if isfinite inside.qual then
		    (strat.binary_cost prod dcomp)
		    -. inside.qual
		    +. (get outside.outside (state.sid, scurve.sid))
		    +. (get inside.inside  (prod.leftsid, dcomp.leftsid))
		    +. (get inside.inside  (prod.rightsid, dcomp.rightsid))
		  else
		    infinity
		in
		  sc.binary_counts.(prod.cid) <- neglogadd sc.binary_counts.(prod.cid) count;

		  let modecount, _ = sc.mpmodes.(prod.cid) in

		    if count < modecount +. 10. then begin
		      sc.mpchoices.(prod.cid) <- (count, strat.getshape dcomp.cdata) :: sc.mpchoices.(prod.cid);
		    end;

		    if count <  modecount then begin
		      sc.mpmodes.(prod.cid) <- 
			(count, Some (strat.getshape dcomp.cdata));
		    end;

	      (* 		  printf "nmps(%d) = %d\n%!" prod.G.cid (List.length sc.mpchoices.(prod.G.cid)); *)
	      end

	  end
      end;
    end;

  printf "QUAL %f\n%!" outside.qual_
  

let sparse_inside_outside gram family sc_table strat =
  let inside_table = inside gram family strat in
  let outside_table = outside gram family inside_table strat in
    soft_counts gram family inside_table outside_table sc_table strat


let sparse_parse_cost gram family strat = 
  let table = inside gram family strat in
    table.qual


let viterbi gram family strat =
  let qual = ref infinity in
    (* chart maps (X,I) -> q, X=nonterminal, I=SDF interval *)
  let table = new_parse_table (Frozen.num_symbols gram)
    (Frozen.num_symbols family) 
    "viterbi" viterbi_verbose in
  let toplevel = ref None in
  let foo = mkhash 100 in
  

    (*   let print_entry symbol scurve = *)
    (*     print_parse_table_entry table  *)
    (*       (gram.x.symbol_name symbol) (family.x.symbol_name scurve) *)
    (*       symbol.G.sid scurve.S.sid *)
    (*   in *)

    Frozen.iter_symbols family
      begin fun scurve -> 
	if strat.lexical_ok scurve then begin
	  Frozen.iter_symbols gram
	    begin fun symbol -> 
	      let cost = strat.lexical_cost symbol scurve in
		set table (symbol.sid, scurve.sid) cost;
		(* 		print_entry symbol scurve;  *)
	    end
	end;

	if strat.binary_ok scurve then begin
	  iter_all_decompositions gram

	    begin fun symbol prod ->
	      if strat.compatible symbol scurve then begin
		Frozen.iter_decompositions family scurve
		  begin fun dcomp ->
		    let cost = (strat.binary_cost prod dcomp) +. 
		      (get table (prod.leftsid, dcomp.leftsid)) +.
		      (get table (prod.rightsid, dcomp.rightsid)) 
		    in
		      if cost < (get table (symbol.sid, scurve.sid)) then begin
			set table (symbol.sid, scurve.sid) cost;
			foo << ((symbol.sid, scurve.sid),
				(prod.cid, dcomp.cid));
		      end
		  end;
		(* 		print_entry symbol scurve; *)
	      end
	    end

	end;

	if strat.goal_ok scurve then begin
	  let cost = strat.goal_cost family.f_gdata scurve +. (get table (0, scurve.sid)) in
	    (** TODO is that zero a problem? *)
	    if cost < !qual then begin
	      toplevel := Some (0, scurve.sid);
	      qual := cost;
	    end
	end

      end;

    if strict_parsing && not (isfinite !qual) then
      failwith "Could not parse!";

    let pairs = ref [] in
    let rec visit (symbolid, scurveid) = 
      let symbol, scurve = Frozen.get_symbol gram symbolid, 
	Frozen.get_symbol family scurveid in
	if foo >>? (symbol.sid, scurve.sid) then begin
	  let prodid, dcompid = foo >> (symbol.sid, scurve.sid) in
	  let prod, dcomp = Frozen.get_composition gram prodid, 
	    Frozen.get_composition family dcompid in

	    pairs := (symbolid, scurveid, Some prodid, Some dcompid) :: !pairs;

	    (* suppress L->LL pairs, screws up labeling *)
	    if prod.leftsid <> symbolid then
	      visit (prod.leftsid, dcomp.leftsid);
	    if prod.rightsid <> symbolid then
	      visit (prod.rightsid, dcomp.rightsid);
	end
	else begin 
	  pairs := (symbolid, scurveid, None, None) :: !pairs;
	end
    in
    let toplevel = 
      match !toplevel with Some x -> x
	| None -> failwith "Could not parse! v2";
    in
      visit toplevel;

      !qual, !pairs
