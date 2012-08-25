open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Abstract
open Sdf
open Grammar
open Models.Simple

let incorporate_worst_midpoint gram training trainfamilies = 
  let worst, worstthing = ref neg_infinity, ref None in
    Array.iteri
      begin fun ifam fam -> 
	let qual, pairs = Parsing.viterbi gram fam Models.Simple.strat in
	  List.iter
	    begin fun (sid,scid,prodid,dcompid) ->
	      if prodid <> None then begin
		let prodid, dcompid = get prodid, get dcompid in
		let thisqual = Models.Simple.strat.Parsing.binary_cost (Frozen.get_composition gram prodid)
		  (Frozen.get_composition fam dcompid) in
		  if thisqual > !worst then begin 
		    worst := thisqual;
		    worstthing := Some (ifam,sid,scid,prodid,dcompid);
		  end
	      end
	    end
	    pairs
      end
      trainfamilies;

    let lgram = enliven gram in

    (* get the worst production/decomposition pair *)
    let ifam,sid,scid,prodid,dcompid = get !worstthing in
    let fam = trainfamilies.(ifam) in
    let sym = Frozen.get_symbol gram sid in
    let prod = Frozen.get_composition gram prodid in
    let lsym, rsym = Frozen.get_rhs gram prod in
    let dcomp = Frozen.get_composition fam dcompid in
    let lscurve, rscurve = Frozen.get_rhs fam dcomp in

    (* make copies of the three symbols involved in the chosen rule,
       and duplicate other rules involving them *)
    let new_sym = make_new_symbol lgram sym.sdata sym.startable in
    let new_lsym = make_new_symbol lgram lsym.sdata lsym.startable in
    let new_rsym = make_new_symbol lgram rsym.sdata rsym.startable in
    let copy_prod_top newtop prod =  
      imake_new_composition lgram newtop.sid (prod.leftsid, prod.rightsid) prod.cdata;
    in
    let copy_prod_left newleft prod = 
      imake_new_composition lgram prod.topsid (newleft.sid, prod.rightsid) prod.cdata;
    in
    let copy_prod_right newright prod = 
      imake_new_composition lgram prod.topsid (prod.leftsid, newright.sid) prod.cdata;
    in
    let _ = 
      Live.iter_decompositions lgram lsym (copy_prod_top new_lsym);
      Live.iter_decompositions lgram rsym (copy_prod_top new_rsym);
      Live.iter_left_compositions lgram sym (copy_prod_left new_sym);
      Live.iter_right_compositions lgram sym (copy_prod_right new_sym);
    in

    (* make the new production, and split the old production's probability between the two

       actually, don't split it anymore!

       we're not correctly normalizing rules with new_sym on rhs, but
       it probably doesn't matter...
    *)

    let geom = match prod.cdata.geom with
	Watson w -> 
	  Watson { Watson.mean = Models.Simple.strat.Parsing.getshape dcomp.cdata;
		   Watson.conc_ = w.Watson.conc_}
      | Improper -> Improper
    in
    let prob = prod.cdata.prob in
    let cost = -. log prob in
(*     let prob = prod.cdata.prob /. 2. in *)
(*     let cost = -. log prob in *)
    let doubled = Array.append training.(ifam) training.(ifam) in

(*       prod.cdata <- *)
(* 	{prob = prob; *)
(* 	 cost = cost; *)
(* 	 geom = prod.cdata.geom; *)
(* 	 ocurve = prod.cdata.ocurve; *)
(* 	 lcurve = prod.cdata.lcurve; *)
(* 	 rcurve = prod.cdata.rcurve; *)
(* 	}; *)

      let _ = 
	imake_new_composition lgram new_sym.sid (new_lsym.sid, new_rsym.sid)
	  {prob = prob;
	   cost = cost;
	   geom = geom;
	   ocurve = prod.cdata.ocurve;
	   lcurve = Array.sub doubled dcomp.cdata.bg_ (lscurve.sdata.len_ + 1);
	   rcurve = Array.sub doubled dcomp.cdata.md_ (rscurve.sdata.len_ + 1);
	  };
      in

	finalize lgram


let incremental excurve_name sdf_name training_fnames niters =
  let excurve = Curve.load excurve_name in
  let exfamily = excurve, Sdf.load_family sdf_name in
  let gram = Models.Simple.make_grammar exfamily in

  let training = Array.map Curve.load training_fnames in
  let trainfamilies = Array.map
    begin fun c -> 
      let fam = Sdf.make_full_family (Array.length c) in
	Models.Simple.add_curve_data_to_family c fam
    end 
    training 
  in

  let logging i gram = 
    let file = open_out (sprintf "tmp/incremental.%d.gram" i) in
      Marshal.to_channel file gram [];
      close_out file;
      printf "LOGGING %d\n%!" i;
      Grammar.print_grammar gram;
  in

    logging 0 gram;    

    let gram = ref gram in
      for i = 0 to niters-1 do
	for j = 1 to 5 do
	  gram := incorporate_worst_midpoint !gram training trainfamilies;
	  logging (10*i + j) !gram;
	done;

	for j = 6 to 10 do
	  gram := Retrain.retrain
	    (* 	  ~sparsefactor:(float_of_int i) *)
	    ~sparsefactor:(1.0)
	    ~prunethresh:(0.05)
	    !gram (Array.to_list trainfamilies) 
	    Models.Simple.strat;
	  logging (10*i + j) !gram;
	done
      done
	
let _ = 
  let example = ref "NONE.curve" in
  let sdf = ref "NONE.sdf" in
  let curves = ref [] in
  let niters = ref 0 in

  let add_curve fname = (curves := fname :: !curves;) in

  let _ = Arg.parse [
    "-example", Arg.Set_string example, "Example curve.";
    "-niters", Arg.Set_int niters, "Number of EM iterations.";
    "-sdf", Arg.Set_string sdf, "SDF for decomposing example.";]
    add_curve
    "./prog -example example -sdf sdf curve1 curve2 curve3..."
  in

  let curves = Array.of_list (List.rev !curves) in
    
    incremental !example !sdf curves !niters
