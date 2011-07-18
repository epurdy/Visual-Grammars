open Util.Hops
open Util.Misc
open Printf

open Abstract

type sid = int
type cid = int
type index = int

type sdf_gdata = {n: int}
type sdf_debug_gdata = {nn: int; thecurve: Curve.t}

type sdf_sdata = {scale: float}
type sdf_cdata = {shape: Shape.shape}
type sdf_debug_sdata = {dscale: float; curve: Curve.t}
type sdf_debug_cdata = {
  dshape: Shape.shape;
  lcurve: Curve.t;
  rcurve: Curve.t;
  ocurve: Curve.t;
}

let ex_debug_sdata = {dscale=0.;curve=[||]} 
let ex_debug_cdata = {dshape=Shape.default_shape;lcurve=[||];rcurve=[||];ocurve=[||];}

type 'a symbol = {
  mutable sdata: 'a;
  first: index;
  last: index;
  len: int;
  mutable sid: sid;
  mutable dcompids: cid list;
  mutable lcompids: cid list;
  mutable rcompids: cid list;
}

type 'b composition = {
  mutable cdata: 'b;
  bg: index;   (* i *)
  md: index;   (* j *)
  en: index;   (* k *)
  mutable cid: cid;
  mutable topsid: sid;
  mutable leftsid: sid;
  mutable rightsid: sid;
}

let bssym sdata = {
  sdata = sdata;
  first = 0; last = 0; len = 0; sid = 0; dcompids = []; lcompids = []; rcompids = [];
}

let bscomp cdata = {
  cdata = cdata;
  bg=0; md=0; en=0; cid=0; topsid=0; leftsid=0; rightsid=0;
}

let copy_symbol' sym new_sdata = {
  sdata = new_sdata;
  first = sym.first;
  last = sym.last;
  len = sym.len;
  sid = sym.sid;
  dcompids = sym.dcompids;
  lcompids = sym.lcompids;
  rcompids = sym.rcompids;
}

let copy_comp' comp new_cdata = {
  cdata = new_cdata;
  bg = comp.bg;
  md = comp.md;
  en = comp.en;
  cid = comp.cid;
  topsid = comp.topsid;
  leftsid = comp.leftsid;
  rightsid = comp.rightsid;
}

let copy_symbol sym = copy_symbol' sym sym.sdata
let copy_comp comp = copy_comp' comp comp.cdata

let is_closed scurve = 
  (scurve.first = scurve.last)

let symbol_name scurve = 
  sprintf "[%d, %d]" scurve.first scurve.last
let symbol_name_long scurve = symbol_name scurve

let composition_name comp = 
  sprintf "[%d, %d][%d, %d]" comp.bg comp.md comp.md comp.en

let lexical_ok scurve = (scurve.len = 1)
let binary_ok scurve = (scurve.len != 1)
let goal_ok scurve = is_closed scurve

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
let friend2 = make_friend (bssym {scale=0.}) (bscomp {shape=Shape.default_shape})
let friend3 = make_friend (bssym ex_debug_sdata) (bscomp ex_debug_cdata)


(***********************************************************************************************)

type ('a,'b,'c) live_family = {
  gram: ('a symbol,'b composition,'c) live_grammar;
  sclookup: (index*index,'a symbol) hash;
  make_new_scurve: int * int -> int -> 'a -> 'a symbol;
  make_new_comp: int * int * int -> 'b -> 'b composition;
  imake_new_scurve: int * int -> int -> 'a -> unit;
  imake_new_comp: int * int * int -> 'b -> unit;
}    
type ('a, 'b, 'c) family = ('a symbol, 'b composition, 'c) Abstract.frozen_grammar
type debug_family = (sdf_debug_sdata, sdf_debug_cdata, sdf_debug_gdata) family
type unit_family = (unit, unit, sdf_gdata) family
type normal_family = (sdf_sdata, sdf_cdata, sdf_gdata) family


let marshal gram fname = 
  Abstract.marshal_frozen_grammar gram fname

let unmarshal ex_a ex_b fname =
  let friend = make_friend (bssym ex_a) (bscomp ex_b) in
    Abstract.unmarshal_frozen_grammar friend fname

let goal_cost gram scurve = log (float_of_int (gram.get_gdata()).nn)

let make_live_family (ex_a: 'a) (ex_b:'b) n nsyms nrules :
    ('a,'b,sdf_gdata) live_family= 
  let bs_a, bs_b = bssym ex_a, bscomp ex_b in
  let gram = new_live_grammar (make_friend bs_a bs_b) {n=n} 
    bs_a bs_b
    nsyms nrules in
  let sclookup = mkhash nsyms in

  let make_new_scurve (i,k) len sdata =
    let scurve = {
      first = i;
      last = k;
      len = len;
      sdata = sdata;
      sid = -1;
      dcompids = [];
      lcompids = [];
      rcompids = [];
    } in
    let scurve = gram.self.insert_symbol scurve in
      sclookup << ((i,k), scurve);
      scurve
  in

  let imake_new_scurve (i,k) len sdata =
    ignore (make_new_scurve (i,k) len sdata)
  in

  let make_new_comp (i, j, k) cdata =    
    let top = sclookup >> (i,k) in
    let left = sclookup >> (i,j) in
    let right = sclookup >> (j,k) in
    let comp = {
      cdata = cdata;
      bg = i;
      md = j;
      en = k;
      cid = -1;
      topsid = top.sid;
      leftsid = left.sid;
      rightsid = right.sid;
    } in
    let comp = gram.self.insert_composition comp in
      comp
  in

  let imake_new_comp (i, j, k) cdata =    
    ignore (make_new_comp (i, j, k) cdata)
  in

    {gram = gram;
     sclookup = sclookup;
     make_new_scurve = make_new_scurve;
     make_new_comp = make_new_comp;
     imake_new_scurve = imake_new_scurve;
     imake_new_comp = imake_new_comp;
    }

(*************************************************************************************************)
(* USING FAMILIES                                                                                *)
(*************************************************************************************************)

let add_curve_data_to_family curve ffamily =
  let n = Array.length curve in
    assert(n = (ffamily.get_gdata ()).n);
    map_frozen_grammar friend2 ffamily
      (fun x -> x)
      begin fun scurve ->
	copy_symbol' scurve 
	  {scale = (float_of_int scurve.len) /. (float_of_int n)}
      end
      begin fun comp ->
	copy_comp' comp
	  {shape = Shape.shape_of_complex_bme 
	      curve.(comp.bg) curve.(comp.md) curve.(comp.en)}
      end

let add_curve_data_to_family_debug curve family =
  let n = Array.length curve in
  let doubled = Array.append curve curve in 
    assert(n = (family.get_gdata()).n);
    map_frozen_grammar friend3 family
      (fun x -> {nn=n; thecurve=curve})
      begin fun scurve ->
	copy_symbol' scurve
	  {dscale = (float_of_int scurve.len) /. (float_of_int n);
	   curve = Array.sub doubled scurve.first (scurve.len+1);
	  }
      end
      begin fun comp ->
	let lcurve, rcurve = family.get_rhs comp in
	let olen = n - (lcurve.len + rcurve.len) in
	  copy_comp' comp
	    {dshape = Shape.shape_of_complex_bme curve.(comp.bg) curve.(comp.md) curve.(comp.en);
	     lcurve = Array.sub doubled comp.bg (lcurve.len+1);
	     rcurve = Array.sub doubled comp.md (rcurve.len+1);
	     ocurve = Array.sub doubled comp.en (olen+1);
	    }
      end


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

let make_full_family n =
  let lfamily = make_live_family () () n (n*n) (n*n*n) in
    for len = 1 to n do
      for i = 0 to (n-1) do
	let k' = (i + len) in
	  lfamily.imake_new_scurve (i, k' mod n) len ();	  
	  for j' = i+1 to k'-1 do
	    lfamily.imake_new_comp (i, j' mod n, k' mod n) ();
	  done;
      done;
    done;
    finalize lfamily.gram

let make_restricted_family n scurves comps =
  let lfamily = make_live_family () () n (List.length scurves) (List.length comps) in
    for len = 1 to n do
      for i = 0 to (n-1) do
	let k' = (i + len) in
	let k = k' mod n in
	  if List.mem (i,k) scurves then begin
	    lfamily.imake_new_scurve (i, k) len ();
	    for j' = i+1 to k'-1 do
	      let j = j' mod n in
		if List.mem (i,j,k) comps then 
		  ignore (lfamily.make_new_comp (i, j, k) ());
	    done;
	  end;
      done;
    done;
    finalize lfamily.gram

let make_sparse_family n k =
  let lfamily = make_live_family () () n n n in
  let coarse_curves = make_coarse_curves n k in
    Array.iteri
      begin fun lvl thecurve ->
	let lvl_length = Array.length thecurve in
	let maxlen_lvl = min k lvl_length in
	let minlen_lvl = if lvl = 0 then 1
	else (k/2) + 1 in

	  (* 0 16 32 in [0,40] with k=2 is a problem, since 16 doesn't make it to the top... *)

	  for len_lvl = minlen_lvl to maxlen_lvl do
	    for i_lvl = 0 to lvl_length-1 do

	      let i = thecurve.(i_lvl) in
	      let k = thecurve.((i_lvl + len_lvl) mod lvl_length) in
	      let len = ((k - i + n + n - 1) mod n) + 1 in

		lfamily.imake_new_scurve (i, k) len ();
		for j_lvl' = i_lvl + 1 to i_lvl + len_lvl - 1 do
		  let j = thecurve.(j_lvl' mod lvl_length) in

		    lfamily.imake_new_comp (i, j, k) ();
		done;

	    done
	  done
      end
      coarse_curves;
    let family = finalize lfamily.gram in
    let family = ensure_reachability family in
      family


let sparse_family_of_curve c k = 
  let fam = make_sparse_family (Array.length c) k in
  let fam = add_curve_data_to_family c fam in
    fam

let sparse_family_of_curve_debug c k = 
  let fam = make_sparse_family (Array.length c) k in
  let fam = add_curve_data_to_family_debug c fam in
    fam

let full_family_of_curve c = 
  let fam = make_full_family (Array.length c) in
  let fam = add_curve_data_to_family c fam in
    fam

let full_family_of_curve_debug c = 
  let fam = make_full_family (Array.length c) in
  let fam = add_curve_data_to_family_debug c fam in
    fam
