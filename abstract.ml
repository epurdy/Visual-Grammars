open Util.Misc
open Util.Hops
open Printf

type sid = int
(** Unique identifier for a symbol *)

type cid = int
(** Unique identifier for a composition *)

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   It is >>> VERY IMPORTANT <<< that the data inside sdata and cdata
   be immutable. The abstract grammar code is written under this
   assumption, and violating it will cause shared reference bugs.

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  *)

type 'a symbol = {
  mutable sid: sid;
  mutable dcompids: cid list;
  mutable lcompids: cid list;
  mutable rcompids: cid list;
  mutable sdata: 'a;
  mutable startable: bool; (* can we use this symbol as a start symbol? *)
}

type 'b composition = {
  mutable cid: cid;
  mutable topsid: sid;
  mutable leftsid: sid;
  mutable rightsid: sid;
  mutable cdata: 'b;
}

let copy_symbol sym = {
  sid = sym.sid;
  dcompids = sym.dcompids;
  lcompids = sym.lcompids;
  rcompids = sym.rcompids;
  startable = sym.startable;
  sdata = sym.sdata;
}

let copy_composition comp = {
  cid = comp.cid;
  topsid = comp.topsid;
  leftsid = comp.leftsid;
  rightsid = comp.rightsid;
  cdata = comp.cdata;
}

let symbol_name sym = sprintf "State %d" sym.sid
let composition_name comp = sprintf "Composition %d (s%d -> s%d s%d)"
  comp.cid comp.topsid comp.leftsid comp.rightsid

let change_labels_symbol sym sidmap cidmap =
  sym.sid <- sidmap sym.sid;
  sym.dcompids <- List.map cidmap sym.dcompids;
  sym.lcompids <- List.map cidmap sym.lcompids;
  sym.rcompids <- List.map cidmap sym.rcompids

let change_labels_composition comp sidmap cidmap =
  comp.cid <- cidmap comp.cid;
  comp.topsid   <- sidmap comp.topsid;
  comp.leftsid  <- sidmap comp.leftsid;
  comp.rightsid <- sidmap comp.rightsid

let map_labels_symbol sym sidmap cidmap =
  {sdata = sym.sdata;
   sid = sidmap sym.sid;
   dcompids = List.map cidmap sym.dcompids;
   lcompids = List.map cidmap sym.lcompids;
   rcompids = List.map cidmap sym.rcompids;
   startable = sym.startable;
  }

let map_labels_composition comp sidmap cidmap =
  {cdata = comp.cdata;
   cid = cidmap comp.cid;
   topsid   = sidmap comp.topsid;
   leftsid  = sidmap comp.leftsid;
   rightsid = sidmap comp.rightsid
  }

(***************************************************)
(* Code that is common to frozen and live grammars *)
(***************************************************)

(* primitive operations supported by frozen and live grammars *)
module type GRAMMAR = 
sig 
  type ('a,'b,'c) grammar

  val get_gdata : ('a,'b,'c) grammar -> 'c
  val num_symbols : ('a,'b,'c) grammar -> int
  val num_compositions : ('a,'b,'c) grammar -> int
  val get_symbol : ('a,'b,'c) grammar -> sid -> 'a symbol
  val get_composition : ('a,'b,'c) grammar -> cid -> 'b composition
  val start : ('a,'b,'c) grammar -> 'a symbol

  val iter_symbols : ('a,'b,'c) grammar -> ('a symbol -> unit) -> unit
  val iter_symbols_rev : ('a,'b,'c) grammar -> ('a symbol -> unit) -> unit
end

(* secondary operations common to frozen and live grammars *)
module GramCommon = 
  functor (Gram: GRAMMAR) ->
struct 
  type ('a,'b,'c) grammar = ('a,'b,'c) Gram.grammar

  let get_gdata = Gram.get_gdata
  let num_symbols = Gram.num_symbols
  let num_compositions  = Gram.num_compositions
  let get_symbol = Gram.get_symbol
  let get_composition = Gram.get_composition
  let start = Gram.start
  let iter_symbols = Gram.iter_symbols
  let iter_symbols_rev = Gram.iter_symbols_rev

  let get_decompositions gram sym = 
    List.map (fun cid -> get_composition gram cid) sym.dcompids
  let get_left_compositions gram sym = 
    List.map (fun cid -> get_composition gram cid) sym.lcompids
  let get_right_compositions gram sym = 
    List.map (fun cid -> get_composition gram cid) sym.rcompids

  let iter_decompositions gram sym f = 
    List.iter f (get_decompositions gram sym)
  let iter_left_compositions gram sym f = 
    List.iter f (get_left_compositions gram sym)
  let iter_right_compositions gram sym f = 
    List.iter f (get_right_compositions gram sym)

  let get_lhs gram comp = get_symbol gram comp.topsid
  let get_rhs gram comp = 
    get_symbol gram comp.leftsid,
    get_symbol gram comp.rightsid

  let validate_sym gram sym =
    iter_decompositions gram sym
      begin fun dcomp ->
	let left, right = get_rhs gram dcomp in
	  assert(dcomp.topsid == sym.sid);
	  assert(List.mem dcomp.cid sym.dcompids);
	  assert(List.mem dcomp.cid left.lcompids);
	  assert(List.mem dcomp.cid right.rcompids);
      end

  let sample_path gram =
    let thesymbol = ref (Gram.start gram) in
    let thesamples = ref [ !thesymbol ] in
      while (List.length (!thesymbol).dcompids) > 0 do
	let decompositions = get_decompositions gram !thesymbol in
	let decompositions = Array.of_list decompositions in
	let thecomp = random_choice decompositions in
	  thesymbol := random_choice 
	    [| Gram.get_symbol gram thecomp.leftsid;
	       Gram.get_symbol gram thecomp.rightsid; |];
	  thesamples := !thesymbol :: !thesamples;
      done;
      List.rev !thesamples

  let print long_namer gram =
    Gram.iter_symbols_rev gram
      begin fun sym -> 
	printf "%s:\n" (long_namer sym);
	iter_decompositions gram sym
	  begin fun dcomp ->
	    let left,right = get_rhs gram dcomp in
	      printf "  -> %s %s    [%s]\n"
		(symbol_name left)
		(symbol_name right)
		(composition_name dcomp)	    
	  end
      end

end

(*****************)
(* Live grammars *)
(*****************)

type ('a,'b,'c) live_grammar = {
  mutable l_gdata: 'c;
  mutable l_symbols: (int, 'a symbol) hash;
  mutable l_compositions: (int, 'b composition) hash;
  mutable next_sid: int;
  mutable next_cid: int;
}

(* the primitive operations necessary to fulfill the GRAMMAR interface *)
module Live_base = 
struct
  type ('a,'b,'c) grammar = ('a,'b,'c) live_grammar

  let get_gdata gram = gram.l_gdata
  let num_symbols gram = Hashtbl.length gram.l_symbols
  let num_compositions gram = Hashtbl.length gram.l_compositions
  let get_symbol gram sid = gram.l_symbols >> sid
  let get_composition gram cid = gram.l_compositions >> cid
  let start gram = gram.l_symbols >> 0

  let iter_symbols gram f = Hashtbl.iter (fun k v -> f v) gram.l_symbols
  let iter_symbols_rev gram f = 
    fprintf stderr "iter_symbols_rev is not really reversed for live grammars!\n";
    Hashtbl.iter (fun k v -> f v) gram.l_symbols
end

module Live = GramCommon(Live_base)

let new_live_grammar ?(nsyms=100) ?(ncomps=100) gdata = 
  {l_gdata = gdata;
   l_symbols = mkhash nsyms;
   l_compositions = mkhash ncomps;
   next_sid = 0;
   next_cid = 0;}

let validate_live gram = 
  Hashtbl.iter
    begin fun sid sym ->
      Live.validate_sym gram sym;
      assert(sym.sid = sid);
    end  
    gram.l_symbols;
  Hashtbl.iter
    begin fun cid dcomp ->
      assert(dcomp.cid = cid);
    end
    gram.l_compositions

let make_new_symbol gram sdata startable = 
  let sym = {
    sdata = sdata;
    startable = startable;
    sid = gram.next_sid;
    dcompids = [];
    lcompids = [];
    rcompids = [];
  } in    
    gram.l_symbols << (gram.next_sid, sym);
    gram.next_sid <- gram.next_sid + 1;
    sym

let make_new_composition gram topsid (leftsid,rightsid) cdata = 
  let comp = {
    cdata = cdata;
    cid = gram.next_cid;
    topsid = topsid;
    leftsid = leftsid;
    rightsid = rightsid;
  } in
  let top = Live.get_symbol gram topsid in
  let left = Live.get_symbol gram leftsid in
  let right = Live.get_symbol gram rightsid in
    gram.l_compositions << (gram.next_cid, comp);
    top.dcompids   <- comp.cid :: top.dcompids;
    left.lcompids  <- comp.cid :: left.lcompids;
    right.rcompids <- comp.cid :: right.rcompids;
    gram.next_cid <- gram.next_cid + 1;
    comp

let imake_new_symbol gram sdata startable = 
  ignore (make_new_symbol gram sdata startable)

let imake_new_composition gram topsid (leftsid, rightsid) cdata = 
  ignore (make_new_composition gram topsid (leftsid,rightsid) cdata)


(*******************)
(* Frozen grammars *)
(*******************)

type ('a,'b,'c) frozen_grammar = {
  f_gdata: 'c;
  f_symbols: 'a symbol array;
  f_compositions: 'b composition array;
}

(* the primitive operations necessary to fulfill the GRAMMAR interface *)
module Frozen_base = 
struct 
  type ('a,'b,'c) grammar = ('a,'b,'c) frozen_grammar

  let get_gdata gram = gram.f_gdata
  let num_symbols gram = Array.length gram.f_symbols
  let num_compositions gram = Array.length gram.f_compositions
  let get_symbol gram sid = gram.f_symbols.(sid)
  let get_composition gram cid = gram.f_compositions.(cid)
  let start gram = gram.f_symbols.(0)

  let iter_symbols gram f = Array.iter f gram.f_symbols

  let iter_symbols_rev gram f = 
    let nsymbols = num_symbols gram in
      for i = (nsymbols-1) downto 0 do
	f gram.f_symbols.(i)
      done
end

module Frozen = GramCommon(Frozen_base)

let new_frozen_grammar gdata symbols compositions =
  {f_gdata = gdata;
   f_symbols = symbols;
   f_compositions = compositions;} 

let validate_frozen gram = 
  Array.iteri
    begin fun sid sym ->
      Frozen.validate_sym gram sym;
      assert(sym.sid = sid);
    end
    gram.f_symbols;
  Array.iteri
    begin fun cid dcomp ->
      assert(dcomp.cid = cid);
    end
    gram.f_compositions

let iter_all_compositions gram f = Array.iter f gram.f_compositions

let iter_all_decompositions gram f = 
  Array.iter (fun sym -> Frozen.iter_decompositions gram sym (f sym)) 
    gram.f_symbols


(********************************)
(* Useful Functions on Grammars *)
(********************************)

(* works on frozen grams *)
let map_frozen_grammar gram fdata fsym fcomp =
  new_frozen_grammar 
    (fdata gram.f_gdata)
    (Array.map fsym gram.f_symbols)
    (Array.map fcomp gram.f_compositions)

(* works on live grams *)
let compactify gram =
  validate_live gram;
  let sids = List.sort compare (keys gram.l_symbols) in
  let new_old_sids = Array.of_list sids in
  let old_new_sids = fn_of_hash (invert_array new_old_sids) in

  let cids = List.sort compare (keys gram.l_compositions) in
  let new_old_cids = Array.of_list cids in
  let old_new_cids = fn_of_hash (invert_array new_old_cids) in

  let newsyms = Hashtbl.create (Hashtbl.length gram.l_symbols) in
  let newcomps = Hashtbl.create (Hashtbl.length gram.l_compositions) in

    Hashtbl.iter 
      begin fun oldsid sym ->
	let new_sym = copy_symbol sym in
	  change_labels_symbol new_sym old_new_sids old_new_cids;
	  newsyms << (new_sym.sid, new_sym);	  	      
      end
      gram.l_symbols;

    Hashtbl.iter
      begin fun oldcid comp ->
	let new_comp = copy_composition comp in
	  change_labels_composition new_comp old_new_sids old_new_cids;
	  newcomps << (new_comp.cid, new_comp);
	  
      end
      gram.l_compositions;

    gram.l_symbols <- newsyms;
    gram.l_compositions <- newcomps

(* map live to frozen *)
let finalize (gram: ('symbol,'composition,'c) live_grammar) =
  compactify gram;
  validate_live gram;
  new_frozen_grammar gram.l_gdata
    (Array.init gram.next_sid
       (fun i -> gram.l_symbols >> i))
    (Array.init gram.next_cid
       (fun i -> gram.l_compositions >> i))

let enliven froz =
  validate_frozen froz;
  let live = new_live_grammar
    ~nsyms:(Frozen.num_symbols froz)
    ~ncomps:(Frozen.num_compositions froz)
    froz.f_gdata
  in
    Frozen.iter_symbols froz
      begin fun sym ->
	let newsym = make_new_symbol live sym.sdata sym.startable in
	  assert(newsym.sid = sym.sid);
      end;
    iter_all_compositions froz
      begin fun comp ->
	let newcomp = make_new_composition live 
	  comp.topsid (comp.leftsid, comp.rightsid) comp.cdata in
	  assert(newcomp.cid = comp.cid);
      end;
    live

(* only works on frozen grams *)
let merge_frozen_grams grams new_gdata =
  fprintf stderr "TODO: assumes start states are not special!!!\n";
  Array.iter validate_frozen grams;
  let allsyms, allcomps = ref [], ref [] in
  let sym_offset, comp_offset = ref 0, ref 0 in
    Array.iter
      begin fun g ->
	let init_sym_offset, init_comp_offset = !sym_offset, !comp_offset in
	let sidmap i = i + init_sym_offset in
	let cidmap i = i + init_comp_offset in
	let syms = Array.map
	  begin fun sym -> 
	    incr sym_offset;
	    map_labels_symbol sym sidmap cidmap
	  end
	  g.f_symbols 
	in
	let comps = Array.map
	  begin fun comp -> 
	    incr comp_offset;
	    map_labels_composition comp sidmap cidmap
	  end
	  g.f_compositions
	in
	  allsyms := syms :: !allsyms;
	  allcomps := comps :: !allcomps;
      end
      grams;

    new_frozen_grammar new_gdata
      (Array.concat (List.rev !allsyms))
      (Array.concat (List.rev !allcomps))

(* only works on frozen grams *)
let check_reachability gram =
  let reachable = Array.init (Frozen.num_symbols gram ) (fun i -> false) in

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
      Frozen.iter_symbols_rev gram
	begin fun symbol ->
	  if symbol.startable || reachable.(symbol.sid) then begin
	    check symbol.sid;
	    Frozen.iter_decompositions gram symbol
	      begin fun dcomp ->
		check dcomp.leftsid;
		check dcomp.rightsid;
	      end
	  end
	end;
    done;
    reachable

(* only works on frozen *)
let ensure_reachability gram =
  (*   validate_froz gram; *)
  let nsymbols = Frozen.num_symbols gram in
  let liveid = Array.init nsymbols (fun i -> None) in
  let live = new_live_grammar gram.f_gdata in

  let reachable = check_reachability gram in

    Frozen.iter_symbols gram
      begin fun sym ->
	if reachable.(sym.sid) then begin
	  let newsym = make_new_symbol live sym.sdata sym.startable in
	    liveid.(sym.sid) <- Some (newsym.sid);
	end
      end;

    Frozen.iter_symbols gram
      begin fun sym ->
	if reachable.(sym.sid) then begin
	  Frozen.iter_decompositions gram sym
	    begin fun dcomp ->
	      let topsid = get liveid.(sym.sid) in
	      let leftsid = get liveid.(dcomp.leftsid) in
	      let rightsid = get liveid.(dcomp.rightsid) in
		imake_new_composition live topsid (leftsid, rightsid) dcomp.cdata;
	    end;
	end
      end;


    validate_live live;
    let froz = finalize live in
      validate_frozen froz;
      froz

(* works on frozen grams *)
let filter_compositions gram pred =
  Frozen.iter_symbols gram
    begin fun sym ->
      sym.dcompids <-
	List.filter
	begin fun cid ->
	  pred (Frozen.get_composition gram cid)
	end 
	sym.dcompids
    end;

  ensure_reachability gram
