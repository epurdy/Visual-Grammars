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
	    Watson.conc_ = 1000. (* scale *. 1000. *);
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



      (* vvvv Adding L->LL rules vvvv *)
      (*   let gram = enliven gram in *)
      (*     Live.iter_symbols gram *)
      (*       begin fun sym -> *)
      (* 	if sym.dcompids = [] then begin  *)
      (* 	  let scale = 1. /. fn in *)
      (* 	  let prob = 1. -. sym.sdata.straightprob in *)
      (* 	  let cdata = { *)
      (* 	    prob = prob; *)
      (* 	    cost = -. log prob; *)
      (* 	    geom = Watson { *)
      (* 	      Watson.mean = Shape.default_shape; *)
      (* 	      Watson.conc_ = scale *. 10.; *)
      (* 	    }; *)
      (* 	    lcurve = sym.sdata.curve; *)
      (* 	    rcurve = sym.sdata.curve; *)
      (* 	    ocurve = [| |]; (\* TODO this is wrong, but i'm lazy *\) *)
      (*  	  } in *)
      (* 	    imake_new_composition gram sym.sid (sym.sid,sym.sid) cdata; *)
      (* 	end *)
      (*       end; *)

      (*     finalize gram *)
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
    Parsing.goal_cost = (fun gdata scurve -> 0. (*log (float_of_int gdata.n) *));

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
