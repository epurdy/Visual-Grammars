open Printf
module C = Complex
open Util.Cops
open Util.Misc
open Abstract
open Sdf
open Grammar

module type MODEL = sig 

end

module Simple = 
struct
  type loc_sdata = {
    scale_: float;
    first_: int;
    last_: int;
    len_: int;
  }
  type loc_cdata = {
    shape_: Shape.shape;
    bg_: int;
    md_: int;
    en_: int;
  }

  let make_grammar (c, fam) =
    let doubled = Array.append c c in
    let n = Array.length c in
    let fn = float_of_int n in

    let sdata_maker scurve = 
      let closed = (scurve.sdata.first = scurve.sdata.last) in
	if closed then
	  None
	else begin
	  let scale = float_of_int scurve.sdata.len /. fn in
	  let straightcost = 
	    if scurve.dcompids = [] then 0.
	    else infinity (* 100000. *. scale *. scale *)
	  in
	  let straightprob = exp (-. straightcost) in
	    Some {closed = closed;
		  straightprob = straightprob;
		  straightcost = straightcost;
		  curve = Array.sub doubled scurve.sdata.first (scurve.sdata.len + 1);}
	end
    in

    let cdata_maker scurve comp = 
      let scale = float_of_int scurve.sdata.len /. fn in
      let left, right = Frozen.get_rhs fam comp in
      let shape = Shape.shape_of_complex_bme  
	c.(comp.cdata.bg) c.(comp.cdata.md) c.(comp.cdata.en) in
      let olen = n - (left.sdata.len + right.sdata.len) in
      let geom = 
	if comp.cdata.bg = comp.cdata.en then
	  Improper
	else
	  Watson {
	    Watson.mean = shape; 
	    Watson.conc_ = (* 1000. *) scale *. 1000.;
	  }
      in

	{prob = 1.;
	 cost = 0.;
	 geom = geom;
	 lcurve = Array.sub doubled comp.cdata.bg (left.sdata.len+1);
	 rcurve = Array.sub doubled comp.cdata.md (right.sdata.len+1);
	 ocurve = Array.sub doubled comp.cdata.en (olen+1);
	}
    in

    let gram = Grammar.grammar_of_family fam {closed=true; straightprob=0.; straightcost=infinity; curve=c} sdata_maker cdata_maker in

      gram

  let add_curve_data_to_family curve fam =
    let n = Array.length curve in
      assert(n = fam.f_gdata.n);
      map_frozen_grammar fam
	(fun x -> x)
	begin fun scurve ->
	  {sid = scurve.sid;
	   dcompids = scurve.dcompids;
	   lcompids = scurve.lcompids;
	   rcompids = scurve.rcompids;
	   startable = scurve.startable;
	   sdata = {
	     scale_ = (float_of_int scurve.sdata.len) /. (float_of_int n);
	     first_ = scurve.sdata.first;
	     last_ = scurve.sdata.last;
	     len_ = scurve.sdata.len;
	   }
	  }
	end
	begin fun comp ->
	  {cid = comp.cid;
	   topsid = comp.topsid;
	   leftsid = comp.leftsid;
	   rightsid = comp.rightsid;
	   cdata = {
	     shape_ = Shape.shape_of_complex_bme
	       curve.(comp.cdata.bg) 
	       curve.(comp.cdata.md) 
	       curve.(comp.cdata.en);
	     bg_ = comp.cdata.bg;
	     md_ = comp.cdata.md;
	     en_ = comp.cdata.en;
	   }
	  }
	end

  let prod_cost comp shape =
    match comp.cdata.geom with
	Improper ->
	  comp.cdata.cost
      | Parzen model ->
	  comp.cdata.cost +. (Parzen.cost model shape)
      | Watson model ->
	  begin
	    let watsoncost = Watson.cost model shape in
	      if not (isfinite watsoncost) then
		printf "watsoncost = %f\n" watsoncost;
	      comp.cdata.cost +. watsoncost
	  end

  let sigma = Con.default_sigma
  let baseline_sigma = Con.default_baseline_sigma

  let strat = {
    Parsing.tgt_sym_namer = 
      (fun scurve -> sprintf "[%d, %d]" scurve.sdata.first_ scurve.sdata.last_);

    Parsing.lexical_ok = (fun scurve -> (scurve.sdata.len_ = 1));
    Parsing.binary_ok = (fun scurve -> (scurve.sdata.len_ != 1));
    Parsing.goal_ok = (fun scurve -> scurve.sdata.first_ = scurve.sdata.last_);

    Parsing.lexical_cost = (fun sym scurve -> sym.sdata.straightcost);
    Parsing.binary_cost = (fun comp dcomp -> prod_cost comp dcomp.cdata.shape_);
    Parsing.goal_cost = 
      (fun gdata scurve -> 
(* 	 fprintf stderr "This should probably be log n, not 0\n"; *)
	 0. (*log (float_of_int gdata.n) *)
      );

    Parsing.compatible = (fun sym scurve -> sym.sdata.closed = scurve.startable);
    Parsing.getshape = (fun cdata -> cdata.shape_);

    Parsing.fit_midpoint_distro = begin fun prod samples themode ->
      (*     let geom = Parzen.make_model_neglog_weighted *)
      (*       (Array.of_list samples) *)
      (*       sigma Con.default_granularity *)
      (*       (begin match themode with *)
      (* 	   None -> Shape.default_shape *)
      (* 	 | Some shape -> shape *)
      (*        end, baseline_sigma) Con.default_baseline_weight *)
      (*       true *)
      let geom =  Watson.fit_neglog_weighted samples in
	{
	  geom = Watson geom;
	  prob = prod.cdata.prob;
	  cost = prod.cdata.cost;
	  ocurve = prod.cdata.ocurve;
	  lcurve = prod.cdata.lcurve;
	  rcurve = prod.cdata.rcurve;
	}
    end;

  }


end

module LLL_shorter = 
struct
  let add_curve_data_to_family = Simple.add_curve_data_to_family
  let strat = Simple.strat
  let make_grammar (c, fam) = 
    let fn = float_of_int (Array.length c) in
    let gram = Simple.make_grammar (c, fam) in
      Frozen.iter_symbols gram
	begin fun sym ->
	  let scale = Array.length (sym.sdata.curve) - 1 in
	  let scale = (float_of_int scale) /. fn in
	  let straightcost = 
	    if sym.sdata.closed then
	      infinity
	    else
	      100. *. scale *. scale 
	  in
	    sym.sdata <- {
	      closed = sym.sdata.closed;
	      straightcost = straightcost;
	      straightprob = exp (-. straightcost); (* TODO should renormalize, but I'm lazy *)
	      curve = sym.sdata.curve
	    };
	end;
      gram
	
end

module LLL_longer = 
struct

  let add_curve_data_to_family = Simple.add_curve_data_to_family
  let strat = Simple.strat

  let make_grammar (c, fam) =
    let fn = float_of_int (Array.length c) in
    let gram = Simple.make_grammar (c, fam) in
      (* vvvv Adding L->LL rules vvvv *)
    let gram = enliven gram in
      Live.iter_symbols gram
        begin fun sym ->
      	  if sym.dcompids = [] then begin
      	    let scale = 1. /. fn in
	    let straightcost = 100. *. scale *. scale in
	    let sdata = {
	      closed = sym.sdata.closed;
	      straightcost = straightcost;
	      straightprob = exp (-. straightcost);
	      curve = sym.sdata.curve; (* note the conflict with below *)
	    } 
	    in
      	    let prob = 1. -. sdata.straightprob in
      	    let cdata = {
      	      prob = prob;
      	      cost = -. log prob;
      	      geom = Watson {
      		Watson.mean = Shape.default_shape;
(*       		Watson.conc_ = scale *. 10.; *)
      		Watson.conc_ = scale *. 1000.;
      	      };
	      (* A reasonable enough depiction of L->LL: 
		 o-o-o
		 How does it interact with grammar drawing code? *)
      	      lcurve = [| c0; c1 |];
      	      rcurve = [| c1; c1 +& c1 |];
      	      ocurve = [| |];
       	    } in
	      sym.sdata <- sdata;
      	      imake_new_composition gram sym.sid (sym.sid,sym.sid) cdata;
      	  end
        end;
      finalize gram


end


module LLL_both = 
struct

  let add_curve_data_to_family = Simple.add_curve_data_to_family
  let strat = Simple.strat

  let make_grammar (c, fam) =
    let fn = float_of_int (Array.length c) in
    let gram = LLL_shorter.make_grammar (c, fam) in
      (* vvvv Adding L->LL rules vvvv *)
    let gram = enliven gram in
      Live.iter_symbols gram
        begin fun sym ->
      	  if sym.dcompids = [] then begin
      	    let scale = 1. /. fn in
	    let straightcost = 100. *. scale *. scale in
	    let sdata = {
	      closed = sym.sdata.closed;
	      straightcost = straightcost;
	      straightprob = exp (-. straightcost);
	      curve = sym.sdata.curve; (* note the conflict with below *)
	    } 
	    in
      	    let prob = 1. -. sdata.straightprob in
      	    let cdata = {
      	      prob = prob;
      	      cost = -. log prob;
      	      geom = Watson {
      		Watson.mean = Shape.default_shape;
      		Watson.conc_ = scale *. 1000.;
      	      };
	      (* A reasonable enough depiction of L->LL: 
		 o-o-o
		 How does it interact with grammar drawing code? *)
      	      lcurve = [| c0; c1 |];
      	      rcurve = [| c1; c1 +& c1 |];
      	      ocurve = [| |];
       	    } in
	      sym.sdata <- sdata;
      	      imake_new_composition gram sym.sid (sym.sid,sym.sid) cdata;
      	  end
        end;
      finalize gram


end

module Mpeg7 =
struct
type loc_sdata = {
  dist2_: float;
  scale_: float;
  first_: int;
  last_: int;
  len_: int;
}
type loc_cdata = {
  shape_: Shape.shape;
  bg_: int;
  md_: int;
  en_: int;
}

let thestraightcost scale dist2 = 
(*   100. *. (sqrt dist2) *)

(* 100. *. scale *. scale *)
	  100. *. (scale *. (sqrt dist2))

(* 	  100. *. ((sqrt dist2) *. scale) *)





(* 	  100. *. (scale *. scale) *)

(* (100. *. scale / dist2???) *)

(* 	  100. *. (scale *. scale *. dist2) *)

(* 	  100. *. (scurve.sdata.scale_ *. (sqrt scurve.sdata.dist2_)) *)

(* 	  100. *. (scurve.sdata.dist2_) *)


let add_curve_data_to_family curve fam =
  let dist2 p q =
    C.norm2 (p -& q)
  in
  let n = Array.length curve in

  (*   let maxdist2 =  *)
  (*     let maxdist2 = ref 0. in *)
  (*       Array.iter  *)
  (* 	begin fun p -> *)
  (* 	  Array.iter  *)
  (* 	    begin fun q -> *)
  (* 	      maxdist2 := max !maxdist2 (dist2 p q); *)
  (* 	    end *)
  (* 	    curve *)
  (* 	end *)
  (* 	curve; *)
  (*       !maxdist2 *)
  (*   in *)

  let maxdist2 = 
    let sum = ref 0. in
    let num = ref 0 in
      for pi = 0 to n-1 do 
	for qi = pi-(n/8) to pi+(n/8) do 
	  let qi = (qi + n) mod n in
	  let p,q = curve.(pi), curve.(qi) in
	  let d2 = dist2 p q in	  
	    sum := !sum +. (sqrt d2);
	    incr num;
	done
      done;
      let avg = (!sum /. (float_of_int !num)) in
	printf "avg=%f\n%!" avg;
	avg *. avg
  in

  let n = Array.length curve in
    assert(n = fam.f_gdata.n);
    map_frozen_grammar fam
      (fun x -> x)
      begin fun scurve ->
	{sid = scurve.sid;
	 dcompids = scurve.dcompids;
	 lcompids = scurve.lcompids;
	 rcompids = scurve.rcompids;
	 startable = scurve.startable;
	 sdata = {
	   dist2_ = (dist2 curve.(scurve.sdata.first) 
		       curve.(scurve.sdata.last)) /.
	     maxdist2;
	   scale_ = (float_of_int scurve.sdata.len) /. (float_of_int n);
	   first_ = scurve.sdata.first;
	   last_ = scurve.sdata.last;
	   len_ = scurve.sdata.len;
	 }
	}
      end
      begin fun comp ->
	{cid = comp.cid;
	 topsid = comp.topsid;
	 leftsid = comp.leftsid;
	 rightsid = comp.rightsid;
	 cdata = {
	   shape_ = Shape.shape_of_complex_bme
	     curve.(comp.cdata.bg) 
	     curve.(comp.cdata.md) 
	     curve.(comp.cdata.en);
	   bg_ = comp.cdata.bg;
	   md_ = comp.cdata.md;
	   en_ = comp.cdata.en;
	 }
	}
      end

let make_grammar (c, fam) =
  let fam = add_curve_data_to_family c fam in
  let doubled = Array.append c c in
  let n = Array.length c in

  let sdata_maker scurve = 
    let closed = (scurve.sdata.first_ = scurve.sdata.last_) in
      if closed then
	None
      else begin
	let straightcost = 
	  thestraightcost scurve.sdata.scale_ scurve.sdata.dist2_
	in
	let straightprob = exp (-. straightcost) in
	  Some {closed = closed;
		straightprob = straightprob;
		straightcost = straightcost;
		curve = Array.sub doubled scurve.sdata.first_ (scurve.sdata.len_ + 1);}
      end
  in

  let cdata_maker scurve comp = 
    let scale = scurve.sdata.scale_ in
    let left, right = Frozen.get_rhs fam comp in
    let shape = Shape.shape_of_complex_bme  
      c.(comp.cdata.bg_) c.(comp.cdata.md_) c.(comp.cdata.en_) in
    let olen = n - (left.sdata.len_ + right.sdata.len_) in
    let geom = 
      if comp.cdata.bg_ = comp.cdata.en_ then
	Improper
      else
	Watson {
	  Watson.mean = shape; 
	  Watson.conc_ = (* 1000. *) scale *. 1000.;
	}
    in

      {prob = 1.;
       cost = 0.;
       geom = geom;
       lcurve = Array.sub doubled comp.cdata.bg_ (left.sdata.len_+1);
       rcurve = Array.sub doubled comp.cdata.md_ (right.sdata.len_+1);
       ocurve = Array.sub doubled comp.cdata.en_ (olen+1);
      }
  in

  let gram = Grammar.grammar_of_family fam {closed=true; straightprob=0.; straightcost=infinity; curve=c} sdata_maker cdata_maker in

    gram


let prod_cost comp shape =
  match comp.cdata.geom with
      Improper ->
	comp.cdata.cost
    | Parzen model ->
	comp.cdata.cost +. (Parzen.cost model shape)
    | Watson model ->
	begin
	  let watsoncost = Watson.cost model shape in
	    if not (isfinite watsoncost) then
	      printf "watsoncost = %f\n" watsoncost;
	    comp.cdata.cost +. watsoncost
	end

let strat = {
  Parsing.lexical_ok =
    begin fun scurve ->
(*       scurve.sdata.first_ <> scurve.sdata.last_ *)
      scurve.sdata.scale_ <= 0.125
(*       scurve.sdata.len_ = 1 *)
    end;

(*   Parsing.lexical_ok =  *)
(*     begin fun scurve ->  *)
(*       scurve.sdata.len_ = 1 *)
(*     end; *)

  Parsing.lexical_cost = 
    begin fun sym scurve -> 
      sym.sdata.straightcost +. 
	(thestraightcost scurve.sdata.scale_ scurve.sdata.dist2_) 
+.
	1000. *. scurve.sdata.scale_ *. 
	(log (float_of_int(scurve.sdata.len_))) /. (log 2.)
	(* trying to correct for having fewer midpoint distros *)
    end;

  Parsing.binary_cost = 
    begin fun comp dcomp -> 
      1.0 (* 0.1 *) *. (prod_cost comp dcomp.cdata.shape_)
    end;

  (* rest are same as simple *)
  Parsing.tgt_sym_namer = 
    (fun scurve -> sprintf "[%d, %d]" scurve.sdata.first_ scurve.sdata.last_);

  Parsing.binary_ok = (fun scurve -> (scurve.sdata.len_ <> 1));
  Parsing.goal_ok = (fun scurve -> scurve.sdata.first_ = scurve.sdata.last_);

  Parsing.goal_cost = 
    (fun gdata scurve -> 
       (* 	 fprintf stderr "This should probably be log n, not 0\n"; *)
       0. (*log (float_of_int gdata.n) *)
    );

  Parsing.compatible = (fun sym scurve -> sym.sdata.closed = scurve.startable);
  Parsing.getshape = (fun cdata -> cdata.shape_);

  Parsing.fit_midpoint_distro = begin fun prod samples themode ->
    (*     let geom = Parzen.make_model_neglog_weighted *)
    (*       (Array.of_list samples) *)
    (*       sigma Con.default_granularity *)
    (*       (begin match themode with *)
    (* 	   None -> Shape.default_shape *)
    (* 	 | Some shape -> shape *)
    (*        end, baseline_sigma) Con.default_baseline_weight *)
    (*       true *)
    let geom =  Watson.fit_neglog_weighted samples in
      {
	geom = Watson geom;
	prob = prod.cdata.prob;
	cost = prod.cdata.cost;
	ocurve = prod.cdata.ocurve;
	lcurve = prod.cdata.lcurve;
	rcurve = prod.cdata.rcurve;
      }
  end;

}





(*
(* pedro and josh used a clever occlusion cost: *)

(* inline float occlusion(int startA, int endA, int lengthA, comp_curve *A, *)
(*                        int startB, int endB, int lengthB, comp_curve *B) { *)
(*   float lA = (endA-startA+lengthA) % lengthA; *)
(*   float lB = (endB-startB+lengthB) % lengthB; *)
(*   float dA = (mynorm((\*A)[startA]-(\*A)[endA])); *)
(*   float dB = (mynorm((\*B)[startB]-(\*B)[endB])); *)
(*   return BETA * (dA*lA + dB*lA); *)
(* } *)

(* inline float mynorm(comp a) { *)
(*   return a.real() * a.real() + a.imag() * a.imag(); *)
(* } *)

*)

end
