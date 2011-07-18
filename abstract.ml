open Util.Misc
open Util.Hops
open Printf

type sid = int
type cid = int

type ('symbol,'composition) friend = {
  __sym__: 'symbol;
  __comp__: 'composition;

  sid_: 'symbol -> int; (* read sid *)
  _sid: 'symbol -> int -> unit; (* set sid *)
  dcompids_: 'symbol -> int list;
  _dcompids: 'symbol -> int list -> unit;
  lcompids_: 'symbol -> int list;
  _lcompids: 'symbol -> int list -> unit;
  rcompids_: 'symbol -> int list;
  _rcompids: 'symbol -> int list -> unit;

  cid_: 'composition -> int;
  _cid: 'composition -> int -> unit;
  topsid_: 'composition -> int;
  _topsid: 'composition -> int -> unit;
  leftsid_: 'composition -> int;
  _leftsid: 'composition -> int -> unit;
  rightsid_: 'composition -> int;
  _rightsid: 'composition -> int -> unit;

  copy_symbol: 'symbol -> 'symbol;
  copy_comp: 'composition -> 'composition;

  symbol_name: 'symbol -> string;
  symbol_name_long: 'symbol -> string;
  composition_name: 'composition -> string;

  lexical_ok: 'symbol -> bool;
  binary_ok: 'symbol -> bool;
  goal_ok: 'symbol -> bool;
}

let change_labels_symbol x symbol sidmap cidmap =
  x._sid symbol (sidmap (x.sid_ symbol));
  x._dcompids symbol (List.map cidmap (x.dcompids_ symbol));
  x._lcompids symbol (List.map cidmap (x.lcompids_ symbol));
  x._rcompids symbol (List.map cidmap (x.rcompids_ symbol))

let change_labels_composition x comp sidmap cidmap =
  x._cid comp (cidmap (x.cid_ comp));
  x._topsid comp (sidmap (x.topsid_ comp));
  x._leftsid comp (sidmap (x.leftsid_ comp));
  x._rightsid comp (sidmap (x.rightsid_ comp))

(* let change_labels_symbol x symbol sidmap cidmap = *)
(*   let apply_cidmap ls = List.map (fun old -> cidmap >> old) ls in *)
(*     x._sid symbol (sidmap >> x.sid_ symbol); *)
(*     x._dcompids symbol (apply_cidmap (x.dcompids_ symbol)); *)
(*     x._lcompids symbol (apply_cidmap (x.lcompids_ symbol)); *)
(*     x._rcompids symbol (apply_cidmap (x.rcompids_ symbol)) *)

(* let change_labels_composition x comp sidmap cidmap = *)
(*   x._cid comp (cidmap >> x.cid_ comp); *)
(*   x._topsid comp (sidmap >> x.topsid_ comp); *)
(*   x._leftsid comp (sidmap >> x.leftsid_ comp); *)
(*   x._rightsid comp (sidmap >> x.rightsid_ comp) *)

type ('symbol,'composition,'c,'self) grammar = {
  mutable self: 'self;
  mutable x: ('symbol,'composition) friend;
  mutable get_gdata: unit -> 'c;
  mutable num_symbols : unit -> int;
  mutable num_compositions : unit -> int;
  mutable get_symbol : sid -> 'symbol;
  mutable get_composition : cid -> 'composition;
  mutable get_lhs : 'composition -> 'symbol;
  mutable get_rhs : 'composition -> 'symbol * 'symbol;
  mutable get_decompositions : 'symbol -> 'composition list;
  mutable iter_symbols : ('symbol -> unit) -> unit;
  mutable iter_symbols_rev : ('symbol -> unit) -> unit;
  mutable iter_decompositions : 'symbol -> ('composition -> unit) -> unit;
  mutable iter_left_compositions : 'symbol -> ('composition -> unit) -> unit;
  mutable iter_right_compositions : 'symbol -> ('composition -> unit) -> unit;
  mutable iter_all_compositions : ('composition -> unit) -> unit;
  mutable iter_all_decompositions : ('symbol -> 'composition -> unit) -> unit;
  mutable start: unit -> 'symbol;
}

type ('symbol,'composition,'c) frozen_grammar_data = {
  f_gdata: 'c;
  f_symbols: 'symbol array;
  f_compositions: 'composition array;
}
type ('symbol,'composition,'c) frozen_grammar = 
    ('symbol,'composition,'c, ('symbol,'composition,'c) frozen_grammar_data) grammar

let bind_frozen_grammar self x =
  let get_gdata () = self.f_gdata in
  let num_symbols () = Array.length self.f_symbols in
  let num_compositions () = Array.length self.f_compositions in
  let get_symbol sid = self.f_symbols.(sid) in
  let get_composition cid = self.f_compositions.(cid) in
  let get_lhs comp = get_symbol (x.topsid_ comp) in
  let get_rhs comp = (get_symbol (x.leftsid_ comp), get_symbol (x.rightsid_ comp)) in
  let get_decompositions sym = List.map get_composition (x.dcompids_ sym) in
  let iter_symbols f = Array.iter f self.f_symbols in
  let iter_symbols_rev f = 
    let nsymbols = num_symbols () in
      for i = (nsymbols-1) downto 0 do
	f self.f_symbols.(i)
      done
  in
  let iter_decompositions sym f = List.iter f (get_decompositions sym) in
  let iter_left_compositions sym f = List.iter (fun i -> f (get_composition i)) (x.lcompids_ sym) in
  let iter_right_compositions sym f = List.iter (fun i -> f (get_composition i)) (x.rcompids_ sym) in
  let iter_all_compositions f = Array.iter f self.f_compositions in
  let iter_all_decompositions f = 
    Array.iter (fun sym -> iter_decompositions sym (f sym)) self.f_symbols in
  let start () = self.f_symbols.(0) in

    {self = self;
     x = x;
     get_gdata = get_gdata;
     num_symbols = num_symbols;
     num_compositions = num_compositions;
     get_symbol = get_symbol;
     get_composition = get_composition;
     get_lhs = get_lhs;
     get_rhs = get_rhs;
     get_decompositions = get_decompositions;
     iter_symbols = iter_symbols; 
     iter_symbols_rev = iter_symbols_rev;
     iter_decompositions = iter_decompositions;
     iter_left_compositions = iter_left_compositions;
     iter_right_compositions = iter_right_compositions;
     iter_all_compositions = iter_all_compositions;
     iter_all_decompositions = iter_all_decompositions;
     start = start
    }

let new_frozen_grammar (x: ('symbol,'composition) friend) gdata 
    (symbols: 'symbol array) (compositions: 'composition array) =
  let self = {
    f_gdata = gdata;
    f_symbols = symbols;
    f_compositions = compositions;
  } in
    bind_frozen_grammar self x

let marshal_frozen_grammar 
    (gram: ('symbol,'composition,'c) frozen_grammar) fname =
  let file = open_out_bin fname in
    Marshal.to_channel file gram.self [];
    close_out file

let unmarshal_frozen_grammar x fname =
  let file = open_in fname in
  let gram = Marshal.from_channel file in
  let gram = (gram: ('symbol,'composition,'c) frozen_grammar_data) in
    close_in file;
    new_frozen_grammar x gram.f_gdata 
      gram.f_symbols gram.f_compositions 

let map_frozen_grammar newx
    (gram: ('symbol,'composition,'c) frozen_grammar) fdata fsym fcomp =
  new_frozen_grammar newx
    (fdata (gram.get_gdata ()))
    (Array.map fsym gram.self.f_symbols)
    (Array.map fcomp gram.self.f_compositions)

type ('symbol,'composition,'c) live_grammar_data = {
  mutable l_gdata: 'c;
  mutable l_symbols: (sid, 'symbol) hash;
  mutable l_compositions: (cid, 'composition) hash;
  mutable next_sid: int;
  mutable next_cid: int;
  mutable insert_symbol : 'symbol -> 'symbol;
  mutable insert_composition : 
    'composition -> 'composition;
}
type ('symbol,'composition,'c) live_grammar = 
    ('symbol,'composition,'c, ('symbol,'composition,'c) live_grammar_data) grammar

let id x = x

let new_live_grammar x gdata ex_a ex_b nsyms nrules =
  let self = {
    l_gdata = gdata;
    l_symbols = mkhash nsyms; 
    l_compositions = mkhash nrules;    
    next_sid = 0;
    next_cid = 0;
    insert_symbol = id;
    insert_composition = id;
  } in

  let get_gdata () = self.l_gdata in
  let num_symbols () = Hashtbl.length self.l_symbols in
  let num_compositions () = Hashtbl.length self.l_compositions in
  let get_symbol sid = self.l_symbols >> sid in
  let get_composition cid = self.l_compositions >> cid in
  let get_lhs comp = get_symbol (x.topsid_ comp) in
  let get_rhs comp = (get_symbol (x.leftsid_ comp), get_symbol (x.rightsid_ comp)) in
  let get_decompositions sym = List.map get_composition (x.dcompids_ sym) in
  let start () = raise (Not_Implemented "live_grammar#start") in
  let iter_symbols f = 
    raise (Not_Implemented "live_grammar#iter_symbols (ordering would be problematic") in
  let iter_symbols_rev f = 
    raise (Not_Implemented "live_grammar#iter_symbols_rev (ordering would be problematic") in
  let iter_decompositions sym f = List.iter f (get_decompositions sym) in
  let iter_left_compositions sym f = List.iter (fun i -> f (get_composition i)) (x.lcompids_ sym) in
  let iter_right_compositions sym f = List.iter (fun i -> f (get_composition i)) (x.rcompids_ sym) in
  let iter_all_decompositions f =
    raise (Not_Implemented "live_grammar#iter_all_decompositions (ordering would be problematic") in 
  let iter_all_compositions f =
    raise (Not_Implemented "live_grammar#iter_all_compositions (ordering would be problematic") in 

  let insert_symbol sym = 
    assert ((x.dcompids_ sym) = []);
    assert ((x.lcompids_ sym) = []);
    assert ((x.rcompids_ sym) = []);
    x._sid sym self.next_sid;
    self.l_symbols << (self.next_sid, sym);
    self.next_sid <- self.next_sid + 1;
    sym
  in

(*   let insert_symbol_squash sym =  *)
(*     x._dcompids sym []; *)
(*     x._lcompids sym []; *)
(*     x._rcompids sym []; *)
(*     x._sid sym self.next_sid; *)
(*     self.l_symbols << (self.next_sid, sym); *)
(*     self.next_sid <- self.next_sid + 1; *)
(*     sym *)
(*   in *)

  let insert_composition comp = 
    let cid = self.next_cid in
    let top = (get_symbol (x.topsid_ comp)) in
    let left = (get_symbol (x.leftsid_ comp)) in
    let right = (get_symbol (x.rightsid_ comp)) in
      x._cid comp cid;
      self.l_compositions << (cid, comp);

      x._dcompids top  (cid :: (x.dcompids_ top));
      x._lcompids left  (cid :: (x.lcompids_ left));
      x._rcompids right  (cid :: (x.rcompids_ right));

      self.next_cid <- self.next_cid + 1;
      comp
  in

    self.insert_symbol <- insert_symbol;
    self.insert_composition <- insert_composition;

    {self = self;
     x = x;
     get_gdata = get_gdata;
     num_symbols = num_symbols;
     num_compositions = num_compositions;
     get_symbol = get_symbol;
     get_composition = get_composition;
     get_lhs = get_lhs;
     get_rhs = get_rhs;
     get_decompositions = get_decompositions;
     iter_symbols = iter_symbols; 
     iter_symbols_rev = iter_symbols_rev;
     iter_decompositions = iter_decompositions;
     iter_left_compositions = iter_left_compositions;
     iter_right_compositions = iter_right_compositions;
     iter_all_compositions = iter_all_compositions;
     iter_all_decompositions = iter_all_decompositions;
     start = start
    }


let sample_path gram =
  let thesymbol = ref (gram.start ()) in
  let thesamples = ref [ !thesymbol ] in
    while (List.length (gram.x.dcompids_ !thesymbol)) > 0 do
      let decompositions = gram.get_decompositions !thesymbol in
      let decompositions = Array.of_list decompositions in
      let thecomp = random_choice decompositions in
	thesymbol := random_choice 
	  [| gram.get_symbol (gram.x.leftsid_ thecomp); 
	     gram.get_symbol (gram.x.rightsid_ thecomp) |];
	thesamples := !thesymbol :: !thesamples;
    done;
    List.rev !thesamples

let print gram =
  gram.iter_symbols_rev
    begin fun sym -> 
      printf "%s:\n" (gram.x.symbol_name_long sym);
      gram.iter_decompositions sym
	begin fun dcomp ->
	  let left,right = gram.get_rhs dcomp in
	    printf "  -> %s %s    [%s]\n"
	      (gram.x.symbol_name left)
	      (gram.x.symbol_name right)
	      (gram.x.composition_name dcomp)	    
	end
    end

let validate_live g = 
  Hashtbl.iter
    begin fun sid sym ->
      g.iter_decompositions sym
	begin fun dcomp ->
	  let cid = g.x.cid_ dcomp in
	  let left, right = g.get_rhs dcomp in
	    assert( (g.x.topsid_ dcomp) == sid);
	    assert( List.mem cid (g.x.lcompids_ left));
	    assert( List.mem cid (g.x.rcompids_ right));
	end
    end
    g.self.l_symbols

let validate_froz g = 
  Array.iteri
    begin fun sid sym ->
      g.iter_decompositions sym
	begin fun dcomp ->
	  let cid = g.x.cid_ dcomp in
	  let left, right = g.get_rhs dcomp in
	    assert( (g.x.topsid_ dcomp) == sid);
	    assert( List.mem cid (g.x.lcompids_ left));
	    assert( List.mem cid (g.x.rcompids_ right));
	end
    end
    g.self.f_symbols
  

let compactify gram =
  validate_live gram;
  let sids = List.sort compare (keys gram.self.l_symbols) in
  let new_old_sids = Array.of_list sids in
  let old_new_sids = fn_of_hash (invert_array new_old_sids) in

  let cids = List.sort compare (keys gram.self.l_compositions) in
  let new_old_cids = Array.of_list cids in
  let old_new_cids = fn_of_hash (invert_array new_old_cids) in

  let newsymbols = Hashtbl.create (Hashtbl.length gram.self.l_symbols) in
  let newcomps = Hashtbl.create (Hashtbl.length gram.self.l_compositions) in

    Hashtbl.iter 
      begin fun oldsid sym ->
	let new_symbol = gram.x.copy_symbol sym in
	  change_labels_symbol gram.x new_symbol old_new_sids old_new_cids;
	  newsymbols << (gram.x.sid_ new_symbol, new_symbol);	  	      
      end
      gram.self.l_symbols;

    Hashtbl.iter
      begin fun oldcid comp ->
	let new_comp = gram.x.copy_comp comp in
	  change_labels_composition gram.x new_comp old_new_sids old_new_cids;
	  newcomps << (gram.x.cid_ new_comp, new_comp);
	  
      end
      gram.self.l_compositions;

    gram.self.l_symbols <- newsymbols;
    gram.self.l_compositions <- newcomps

let finalize (gram: ('symbol,'composition,'c) live_grammar) =
  compactify gram;
  validate_live gram;
  new_frozen_grammar gram.x (gram.get_gdata())
    (Array.init gram.self.next_sid
       (fun i -> gram.self.l_symbols >> i))
    (Array.init gram.self.next_cid
       (fun i -> gram.self.l_compositions >> i))

(* let enliven (gram: ('symbol,'composition,'c) froz_grammar) = *)
(*   validate_froz gram; *)
(*   let live = new_live_grammar gram.x (gram.get_gdata()) *)
(*     gram.self.f_symbols.(0) gram.self.f_compositions.(0) *)
(*     (gram.num_symbols()) (gram.num_compositions()) *)
(*   in *)
(*     gram.iter_symbols *)
(*       begin fun sym -> *)
(* 	let nsym = gram.x.copy_symbol sym in *)
(* 	let nsym = live.insert_symbol_squash nsym in *)
(* 	  gram.iter_decompositions sym *)
(* 	    begin fun dcomp -> *)
(* 	      let ndcomp = gram.x.copy_comp dcomp in *)
(* 		insert_composition ndcomp *)
(* 	    end *)
(*       end *)
(* ... this was never close to done *)
(*       (Array.init gram.self.next_sid *)
(* 	 (fun i -> gram.self.l_symbols >> i)) *)
(*       (Array.init gram.self.next_cid *)
(* 	 (fun i -> gram.self.l_compositions >> i)) *)

let merge_frozen_grams grams =
  printf "TODO: currently modifying grams in place!!!\n";
  Array.iter validate_froz grams;
  let allsymbols, allcomps = ref [], ref [] in
  let sym_offset, comp_offset = ref 0, ref 0 in
    Array.iter
      begin fun g ->
	let sidmap i = i + !sym_offset in
	let cidmap i = i + !comp_offset in
	  Array.iter (fun sym -> change_labels_symbol g.x sym sidmap cidmap) g.self.f_symbols;
	  Array.iter (fun comp -> change_labels_composition g.x comp sidmap cidmap) 
	    g.self.f_compositions;
	  allsymbols := g.self.f_symbols :: !allsymbols;
	  allcomps := g.self.f_compositions :: !allcomps;
	  sym_offset := (g.num_symbols()) + !sym_offset;
	  comp_offset := (g.num_compositions()) + !comp_offset;
      end
      grams;
    new_frozen_grammar grams.(0).x (grams.(0).get_gdata())
      (Array.concat (List.rev !allsymbols))
      (Array.concat (List.rev !allcomps))

let check_reachability frozen =
  let x = frozen.x in
  let reachable = Array.init (frozen.num_symbols()) (fun i -> false) in

  let progress = ref true in
  let check sid = 
    if not reachable.(sid) then begin
      progress := true;
      reachable.(sid) <- true;
    end
  in
    while !progress do
      progress := false;
      (* long first (by convention, really) *)
      frozen.iter_symbols_rev
	begin fun symbol ->
	  if x.goal_ok symbol || reachable.(x.sid_ symbol) then begin
	    check (x.sid_ symbol);
	    frozen.iter_decompositions symbol
	      begin fun dcomp ->
		check (x.leftsid_ dcomp);
		check (x.rightsid_ dcomp);
	      end
	  end
	end;
    done;
    reachable

let ensure_reachability frozen =
  (*   validate_froz frozen; *)
  let x = frozen.x in
  let reachable = Array.init (frozen.num_symbols()) (fun i -> false) in
  let liveid = Array.init (frozen.num_symbols()) (fun i -> None) in
  let live = new_live_grammar
    x
    (frozen.get_gdata())
    frozen.self.f_symbols.(0)
    frozen.self.f_compositions.(0)
    (frozen.num_symbols())
    (frozen.num_compositions()) in

  let reachable = check_reachability frozen in

    frozen.iter_symbols
      begin fun symbol ->
	if reachable.(x.sid_ symbol) then begin
	  let newsym = x.copy_symbol symbol in
	  let _ =
	    x._dcompids newsym [];
	    x._lcompids newsym [];
	    x._rcompids newsym [];
	  in
	  let newsym = live.self.insert_symbol newsym in
	    liveid.(x.sid_ symbol) <- Some (x.sid_ newsym);
	end
      end;

    frozen.iter_symbols
      begin fun symbol ->
	if reachable.(x.sid_ symbol) then begin
	  frozen.iter_decompositions symbol
	    begin fun dcomp ->
	      let new_dcomp = x.copy_comp dcomp in

		x._topsid new_dcomp (get liveid.(x.sid_ symbol));
		x._leftsid new_dcomp (get liveid.(x.leftsid_ dcomp));
		x._rightsid new_dcomp (get liveid.(x.rightsid_ dcomp));
		ignore (live.self.insert_composition new_dcomp);
	    end;
	end
      end;


    validate_live live;
    let froz = finalize live in
      validate_froz froz;
      froz



let filter_compositions frozen pred =
  (* should be fixed -----v *)
  (*   failwith "We're relying on ordering of symbols, which doesn't always hold."; *)
  frozen.iter_symbols 
    begin fun sym ->
      let dcompids = frozen.x.dcompids_ sym in
      let dcompids = List.filter
	begin fun cid ->
	  pred (frozen.get_composition cid)
	end 
	dcompids in
	frozen.x._dcompids sym dcompids;
    end;
  ensure_reachability frozen
