open Printf
open Util.Misc
open Util.Hops
open Geometry

open Abstract

type sid = int
type cid = int

type 'a symbol = {
  mutable sdata: 'a;
  mutable closed: bool;
  mutable straightprob: float;  
  mutable straightcost: float;  
  mutable sid: sid;
  mutable dcompids: cid list;
  mutable lcompids: cid list;
  mutable rcompids: cid list;  
}

type ssg_geom_data = Improper | Parzen of Parzen.model
type 'b composition = {
  mutable cdata: 'b;
  mutable prob: float;
  mutable cost: float;
  mutable geom: ssg_geom_data;
  mutable cid: cid;
  mutable topsid: sid;
  mutable leftsid: sid;
  mutable rightsid: sid;
}

let bssym sdata = {
  sdata = sdata;
  closed=true; straightprob=1.; straightcost=0.; sid = 0; dcompids = []; lcompids = []; rcompids = [];
}

let bscomp cdata = {
  cdata = cdata;
  prob=1.; cost=0.; geom=Improper; cid=0; topsid=0; leftsid=0; rightsid=0;
}


let copy_symbol' sym new_sdata = {
  sdata = new_sdata;
  closed = sym.closed;
  straightprob = sym.straightprob;
  straightcost = sym.straightcost;
  sid = sym.sid;
  dcompids = sym.dcompids;
  lcompids = sym.lcompids;
  rcompids = sym.rcompids;  
}

let copy_comp' comp new_cdata = {
  cdata = new_cdata;
  prob = comp.prob;
  cost = comp.cost;
  geom = comp.geom;
  cid = comp.cid;
  topsid = comp.topsid;
  leftsid = comp.leftsid;
  rightsid = comp.rightsid;
}

let copy_symbol sym = copy_symbol' sym sym.sdata
let copy_comp comp = copy_comp' comp comp.cdata

let symbol_name sym = sprintf "State %d" sym.sid
let symbol_name_long sym = sprintf "State %d   [->l (p=%f)]" sym.sid sym.straightprob
let composition_name comp = sprintf "Composition %d (s%d -> s%ds%d) (p=%f)"
  comp.cid comp.topsid comp.leftsid comp.rightsid comp.prob

let lexical_ok sym = not sym.closed
let binary_ok sym = true
let goal_ok sym = not (lexical_ok sym)


let make_friend bssym bscomp = {
  __sym__ = bssym;
  __comp__ = bscomp;

  sid_ = (fun s -> s.sid);
  _sid = (fun s x -> s.sid <- x);
  dcompids_ = (fun s -> s.dcompids);
  _dcompids = (fun s x -> s.dcompids <- x);
  lcompids_ = (fun s -> s.lcompids);
  _lcompids = (fun s x -> s.lcompids <- x);
  rcompids_ = (fun s -> s.rcompids);
  _rcompids = (fun s x -> s.rcompids <- x);

  cid_ = (fun c -> c.cid);
  _cid = (fun c x -> c.cid <- x);
  topsid_ = (fun c -> c.topsid);
  _topsid = (fun c x -> c.topsid <- x);
  leftsid_ = (fun c -> c.leftsid);
  _leftsid = (fun c x -> c.leftsid <- x);
  rightsid_ = (fun c -> c.rightsid);
  _rightsid = (fun c x -> c.rightsid <- x);

  copy_symbol = copy_symbol;
  copy_comp = copy_comp;

  symbol_name = symbol_name;
  symbol_name_long = symbol_name_long;
  composition_name = composition_name;

  lexical_ok = lexical_ok;
  binary_ok = binary_ok;
  goal_ok = goal_ok;

}
let friend  = make_friend (bssym ()) (bscomp ())


(***********************************************************************************************)

let lexical_cost symbol scurve = -. log symbol.straightprob

let prod_cost comp shape = 
  match comp.geom with 
      Improper ->
	comp.cost
    | Parzen model ->
	comp.cost +. (Parzen.cost model shape)

let binary_cost comp (dcomp: 'a Sdf.composition) = prod_cost comp dcomp.Sdf.cdata.Sdf.dshape

(*  method goal_cost scurve = ... *)

let compatible symbol scurve = 
  if symbol.closed then
    Sdf.is_closed scurve
  else
    not (Sdf.is_closed scurve)

type ('a,'b,'c) live_shape_grammar = {
  gram: ('a symbol,'b composition,'c) live_grammar;
  make_new_state: bool -> float -> 'a -> 'a symbol;
  make_new_prod: sid -> sid * sid -> float -> ssg_geom_data -> 'b -> 'b composition;

  imake_new_state: bool -> float -> 'a -> unit;
  imake_new_prod: sid -> sid * sid -> float -> ssg_geom_data -> 'b -> unit;
  start_: 'a symbol;
}    

type ('a,'b,'c) grammar = ('a symbol, 'b composition, 'c) frozen_grammar
let print_grammar gram = print gram

let make_live_shape_grammar root_sdata ex_a ex_b =
  let bs_a, bs_b = bssym ex_a, bscomp ex_b in
  let friend = make_friend bs_a bs_b in
  let gram = new_live_grammar friend () bs_a bs_b 100 100 in
    
  let make_new_state closed straightcost sdata =
    let state = {
      sdata = sdata;
      closed = closed;
      straightprob = exp (-. straightcost);
      straightcost = straightcost;
      sid = -1;
      dcompids = [];
      lcompids = [];
      rcompids = [];
    }
    in
      gram.self.insert_symbol state
  in

  let imake_new_state closed straightcost sdata =
    ignore (make_new_state closed straightcost sdata)
  in

  let make_new_prod topsid (leftsid,rightsid) cost geom cdata =
    let prod = {
      cdata = cdata;
      prob = exp (-. cost);
      cost = cost;
      geom = geom;
      cid = -1;
      topsid = topsid;
      leftsid = leftsid;
      rightsid = rightsid;
    } in
      gram.self.insert_composition prod
  in

  let imake_new_prod topsid (leftsid,rightsid) cost geom cdata =
    ignore (make_new_prod topsid (leftsid,rightsid) cost geom cdata)
  in

  (* make the start symbol *)
  let start = make_new_state true infinity root_sdata in

    {gram = gram;
     make_new_state = make_new_state;
     make_new_prod = make_new_prod;
     imake_new_state = imake_new_state;
     imake_new_prod = imake_new_prod;
     start_ = start;     
    }


let renormalize gram sym =
  let nltot = List.fold_right
    (fun dcomp nltot -> neglogadd dcomp.cost nltot) 
    (gram.get_decompositions sym) sym.straightcost in

    sym.straightcost <- sym.straightcost -. nltot;
    sym.straightprob <- exp (-. sym.straightcost);
    gram.iter_decompositions sym
      begin fun dcomp -> 
	dcomp.cost <- dcomp.cost -. nltot;
	dcomp.prob <- exp (-. dcomp.cost);
      end

let renormalize_binary gram sym =
  let nltot = List.fold_right
    (fun dcomp nltot -> neglogadd dcomp.cost nltot) 
    (gram.get_decompositions sym) sym.straightcost in

    gram.iter_decompositions sym
      begin fun dcomp -> 
	dcomp.cost <- dcomp.cost -. nltot;
	dcomp.prob <- exp (-. dcomp.cost);
      end


type ('a,'b,'c) get_params = {
  get_scale: 'a Sdf.symbol -> float;
  get_shape: 'b Sdf.composition -> Shape.shape;
  get_sigma: 'a Sdf.symbol -> float;
  get_sdata: 'a Sdf.symbol -> 'a;
  get_cdata: 'b Sdf.composition -> 'b;
  get_null_sdata: ('a,'b,'c) Sdf.family -> 'a;
  get_null_cdata: 'a Sdf.symbol -> 'b;
  get_straightcost: 'a Sdf.symbol -> float;
  get_geom: 'a Sdf.symbol -> 'b Sdf.composition -> ssg_geom_data;
  get_null_geom: 'a Sdf.symbol -> ssg_geom_data;
  ex_a: 'a;
  ex_b: 'b;
}

let get_scale = begin fun scurve -> scurve.Sdf.sdata.Sdf.dscale end
let get_shape = begin fun comp -> comp.Sdf.cdata.Sdf.dshape end
let get_sigma = begin fun 
  scurve -> 
    let sigma = Con.default_baseline_sigma /. (* sqrt *) 
      ( (get_scale scurve) ** 0.5) /. 10. in
      printf "scale=%f, sigma=%f\n%!" (get_scale scurve) sigma;
      sigma
end

let get_params spfactor = {
  get_scale = get_scale;
  get_shape = get_shape;
  get_sigma = get_sigma;
  get_sdata = begin fun scurve -> scurve.Sdf.sdata end;
  get_cdata = begin fun comp -> comp.Sdf.cdata end;
  get_null_sdata = begin fun family ->
    {Sdf.dscale = 1.;
     Sdf.curve = (family.get_gdata ()).Sdf.thecurve
    }
  end;
  get_null_cdata = begin fun scurve ->
    {dshape = Shape.default_shape;
     Sdf.lcurve = scurve.Sdf.sdata.Sdf.curve;
     Sdf.rcurve = scurve.Sdf.sdata.Sdf.curve;
     Sdf.ocurve = [||]}
  end;

  get_straightcost = begin fun scurve ->
    let scale = get_scale scurve in
      spfactor *. scale *. scale 
  end;

  get_geom = begin fun scurve dcomp ->
    if Sdf.is_closed scurve then 
      Improper
    else
      Parzen
	(Parzen.make_model [||] 
	   Con.default_sigma Con.default_granularity 
	   (get_shape dcomp, get_sigma scurve)
	   1.0 false)
  end;

  get_null_geom = begin fun scurve ->
    Parzen
      (Parzen.make_model [||] 
	 Con.default_sigma Con.default_granularity 
	 (Shape.default_shape, get_sigma scurve)
	 1.0 false)
  end;

  ex_a = {Sdf.dscale=0.; Sdf.curve = [||]};
  ex_b = {Sdf.dshape=Shape.default_shape; 
	  Sdf.lcurve = [||];
	  Sdf.rcurve = [||];
	  Sdf.ocurve = [||];};

}

let grammar_of_family ?(spfactor=Con.spfactor) family =
  let get = get_params spfactor in
  let ex_a = get.ex_a in
  let ex_b = get.ex_b in
  let gram = make_live_shape_grammar (get.get_null_sdata family) ex_a ex_b in
  let lookup = mkhash 100 in

    family.iter_symbols
      begin fun scurve ->
	(* 	printf "working on lexical for %s\n%!" (Sdf.symbol_name scurve); *)
	if not (Sdf.is_closed scurve) then begin
	  let state = gram.make_new_state false (get.get_straightcost scurve) (get.get_sdata scurve) in
	    lookup << (scurve.Sdf.sid, state);
	end
	else
	  lookup << (scurve.Sdf.sid, gram.start_);
      end;

    family.iter_symbols
      begin fun scurve ->
	(* 	printf "working on binary for %s\n%!" (Sdf.symbol_name scurve); *)
	let dcomps = family.get_decompositions scurve in
	let ndcomps = List.length dcomps in
	let sym = lookup >> scurve.Sdf.sid in
	  List.iter 
	    begin fun dcomp ->
	      let left, right = family.get_rhs dcomp in
	      let scalediff = left.Sdf.sdata.Sdf.dscale -. right.Sdf.sdata.Sdf.dscale in
	      let scalediff = 0. in
	      let balance_term = 0. in
(* 		if Sdf.is_closed scurve then *)
(* 		  10. *. scalediff *. scalediff +. (Random.float 50.) *)
(* 		else  *)
(* 		  1. *. scalediff *. scalediff   *)
(* 	      in *)
	      let left = lookup >> dcomp.Sdf.leftsid in
	      let right = lookup >> dcomp.Sdf.rightsid in
		ignore (gram.make_new_prod sym.sid (left.sid,right.sid) balance_term
			  (get.get_geom scurve dcomp) (get.get_cdata dcomp))
	    end
	    dcomps;
	  if ndcomps = 0 then begin
	    ignore (gram.make_new_prod sym.sid (sym.sid,sym.sid) 0. 
		      (get.get_null_geom scurve) (get.get_null_cdata scurve))
	  end;
	  renormalize_binary gram.gram sym;
      end;
    finalize gram.gram

let finalize gram = finalize gram.gram

(************************************************************)

let merge_symbols_left gram s1 s2 ~renorm =
  printf "TODO: not merging equivalent rules!\n";
  assert(s1.closed = s2.closed);
  s2.closed <- false;
  s1.straightcost <- neglogadd s1.straightcost s2.straightcost;
  s1.dcompids <- s1.dcompids @ s2.dcompids;
  s1.lcompids <- s1.lcompids @ s2.lcompids;
  s1.rcompids <- s1.rcompids @ s2.rcompids;
  gram.iter_decompositions s2 (fun dcomp -> dcomp.topsid <- s1.sid);
  gram.iter_left_compositions s2 (fun dcomp -> dcomp.leftsid <- s1.sid);
  gram.iter_right_compositions s2 (fun dcomp -> dcomp.rightsid <- s1.sid);
  s2.dcompids <- [];
  s2.lcompids <- [];
  s2.rcompids <- [];
  if renorm then
    renormalize gram s1

(* replace every occurence of s2 with s1 *)
let replace_symbol gram s1 s2 =
  assert(s1.closed = s2.closed);
  s2.closed <- false;
  s1.lcompids <- s1.lcompids @ s2.lcompids;
  s1.rcompids <- s1.rcompids @ s2.rcompids;
  gram.iter_left_compositions s2 (fun dcomp -> dcomp.leftsid <- s1.sid);
  gram.iter_right_compositions s2 (fun dcomp -> dcomp.rightsid <- s1.sid);
  s2.dcompids <- [];
  s2.lcompids <- [];
  s2.rcompids <- []

let merge_leaves gram =
  let leaves = ref [] in
  let _ = Array.iter
    begin fun sym ->
      gram.iter_decompositions sym
	begin fun comp ->
	  if comp.leftsid = sym.sid then
	    leaves := sym :: !leaves;
	end
    end
    gram.self.f_symbols in
  let leaves = List.rev !leaves in
  let leaf, leaves = List.hd leaves, List.tl leaves in
    List.iter (fun sym -> replace_symbol gram leaf sym) leaves;
    gram

let merge_tops grams =
  let gram = merge_frozen_grams grams in
  let starts = ref [] in
  let _ = Array.iter 
    begin fun sym ->
      if sym.closed then
	starts := sym :: !starts
    end
    gram.self.f_symbols in
  let starts = List.rev !starts in
    printf "STARTS\n";
    List.iter (fun s -> printf "%s " (symbol_name s)) starts;
    printf "\n";
  let start, starts = List.hd starts, List.tl starts in
    print_grammar gram;
    List.iter (fun sym -> merge_symbols_left gram start sym ~renorm:false) starts;
    renormalize gram start;
    gram

(************************************************************)

let sampling_thresh = 0.4 (* 100.0 *)

let sample_prod prod p q = 
  match prod.geom with 
      Parzen geom -> 
	let shape = Parzen.sample geom in
	let r = Shape.place shape p q in
	  (p,r,q)
    | Improper ->
	(p,q,p)

(** sample curve starting at a particular state *)
let rec sample_state gram state p q =
  let d2 = Geometry.dist2 p q in
    assert(d2 = d2); (* nan check *)
    assert(d2 < infinity);
    assert(d2 > 0.);
    if (Geometry.dist2 p q < sampling_thresh) ||
      (Random.float 1.0 < state.straightprob) then
	[p; q]
    else
      let productions = Array.of_list (gram.get_decompositions state) in
      let dist = Array.map (fun prod -> prod.prob) productions in
      let i = Sample.choose dist 0 (Array.length dist - 1) in
      let prod = productions.(i) in
      let left,right = gram.get_rhs prod in
      let (p',r',q') = sample_prod prod p q in
	sample_state gram left p' r' @ sample_state gram right r' q'    

let sample gram p q =
  let rv = sample_state gram (gram.start ()) p q in
    Array.of_list rv


(***********************************************)

let marshal gram fname = 
  Abstract.marshal_frozen_grammar gram fname

let unmarshal ex_a ex_b fname =
  let friend = make_friend (bssym ex_a) (bscomp ex_b) in
    Abstract.unmarshal_frozen_grammar friend fname

(***********************************************)
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
      (Random.float 1.0 >= state.straightprob) then
	let productions = Array.of_list (gram.get_decompositions state) in
	let dist = Array.map (fun prod -> prod.prob) productions in
	let prod = productions.(Sample.choose_any dist) in
	let left, right = gram.get_rhs prod in
	  hinc prod.cid;
	  sample_state_counts left (depth + 1);
	  sample_state_counts right (depth + 1);
  in	    

    for i = 1 to ntrials do
      sample_state_counts (gram.start()) 0;
    done;

    Hashtbl.iter 
      begin fun cid count ->
	therules := (count, cid) :: !therules;
      end
      counts;
    (* sort in decreasing order *)
    therules := List.sort (fun (c1,_) (c2,_) -> compare c2 c1) !therules;

    take_best ntake !therules

let draw_prod_curves im bds granularity prod =
  let draw_scurve prod im scurve value =
    for i = 0 to (Array.length scurve) - 2 do
      let p1 = Bounds.bounds_to_array_nice bds granularity scurve.(i) in
      let p2 = Bounds.bounds_to_array_nice bds granularity scurve.(i+1) in
	Draw.draw_line im p1 p2 value;
    done
  in
    draw_scurve prod im prod.cdata.Sdf.lcurve (255,0,0);
    draw_scurve prod im prod.cdata.Sdf.rcurve (0,255,0);
    draw_scurve prod im prod.cdata.Sdf.ocurve (0,255,255)

let log_cutoff = 100.

let draw_production_standard prod =
  let granularity = 100 in
  let size = granularity + 1 in
  let im = Image.create size size 0. in
  let lcurve, rcurve, ocurve = prod.cdata.Sdf.lcurve, prod.cdata.Sdf.rcurve, prod.cdata.Sdf.ocurve in
  let bds = Bounds.nice_curve_bounds (Array.concat [lcurve; rcurve; ocurve]) in
  let p,q = lcurve.(0), rcurve.((Array.length rcurve)-1) in
  let mincost = ref infinity in
  let maxcost = ref neg_infinity in
  let _ = 
    for x = 0 to granularity do
      for y = 0 to granularity do
	let z = Bounds.array_to_bounds bds granularity (x,y) in
	let shape = Shape.shape_of_complex_bme p z q in
	let cost = prod_cost prod shape in
	  mincost := min !mincost cost;
	  maxcost := max !maxcost cost;
	  Image.set im cost (x,y);
      done
    done;
  in
  let im = Image.map
    begin fun cost ->
      let value = (cost -. !mincost) /. (!maxcost -. !mincost) in
      let value = value ** 0.5 in
	(* let value = (cost -. !mincost) /. (log_cutoff -. !mincost) in *)
	(* let value = cost /. log_cutoff in  *)
      let value = min value 1. in
      let pixval = round (255. *. (1. -. value)) in
	Image.color_of_gray pixval
    end
    im
  in
    draw_prod_curves im bds granularity prod;
    im

let draw_grammar_best_rules namer gram =
  let stuff = sample_counts gram in
    Array.iteri
      begin fun rank (count, cid) -> 
	let im = ref None in
	  gram.iter_all_compositions 
	    begin fun prod ->
	      if prod.cid = cid then
		im := Some (draw_production_standard prod);
	    end;
	  (* save it with an appropriate name - best.RANK.COUNT.PID.ppm, eg *)
	  Pnm.save_ppm (get !im) (namer rank count cid);
      end
      (Array.of_list stuff)


let draw_grammar namer gram =
  gram.iter_all_compositions
    begin fun prod ->
      let im = draw_production_standard prod in
	Pnm.save_ppm im (namer prod.cid)
    end


let prune gram minp =
  filter_compositions gram (fun prod -> prod.prob >= minp)
