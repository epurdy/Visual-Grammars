open Printf
open Util.Hops
open Util.Cops
open Abstract
open Sdf
open Geometry
open Models.Simple
open Parsing

module C = Complex

let optimal_constituents curves outname = 
  let excurve = curves.(0) in
  let n = Array.length excurve in
  let family = Sdf.make_full_family n in
  let family = Models.Simple.add_curve_data_to_family excurve family in

  let strat = Models.Simple.strat in
    
  let tab = mkhash 100 in
  let bestdcomp = mkhash 100 in
  let best = ref 0. in
  let besttop = ref (-1) in

  let wdf = Watson.watson_distro_family in

    Frozen.iter_symbols family
      begin fun scurve ->
	if strat.lexical_ok scurve then begin
	  tab << (scurve.sid, 0.);
	end;

	if strat.binary_ok scurve then begin
	  Frozen.iter_decompositions family scurve
	    begin fun dcomp ->
	      let bg, md, en = dcomp.cdata.bg_, dcomp.cdata.md_, dcomp.cdata.en_ in

	      let triples = Array.map 
		(fun c -> (c.(bg), c.(md), c.(en)))
		curves
	      in
	      let winput = Array.map (fun (p,q,r) -> (1.0, Shape.shape_of_complex_bme p q r)) 
		triples 
	      in 
	      let winput = {
		Watson.samples = winput;
		Watson.gamma_shape_param = 1000.;
		Watson.gamma_mean_param = 100.;
	      }
	      in


	      let woutput, wdata = wdf.Distro.infer winput in
	      let cost = wdata.Watson.cost +.
		(tab >> dcomp.leftsid) +. (tab >> dcomp.rightsid) in

		if cost <= (tab >>! (scurve.sid, infinity)) then begin
		  tab << (scurve.sid, cost);
		  bestdcomp << (scurve.sid, dcomp.cid);
		end
	    end
	end;
	
	if strat.goal_ok scurve then begin
	  if tab >>! (scurve.sid,infinity) < !best then begin 
	    best := tab >> scurve.sid;
	    besttop := scurve.sid;
	  end
	end
      end;

    let lfam = Sdf.make_live_family n 100 in

    let rec visit sid =
      let scurve = Frozen.get_symbol family sid in
	lfam.imake_new_scurve (scurve.sdata.first_, scurve.sdata.last_) 
	  scurve.sdata.len_;

	if not (strat.lexical_ok scurve) then begin 
	  let dcompid = bestdcomp >> sid in
	  let dcomp = Frozen.get_composition family dcompid in
	    visit dcomp.leftsid;
	    visit dcomp.rightsid;
	    lfam.imake_new_comp
	      (dcomp.cdata.bg_, dcomp.cdata.md_, dcomp.cdata.en_);
	end
    in 
      visit !besttop;

      let newfam = finalize lfam.gram in
	save_family newfam outname

let _ = 
  let curves = Array.init 16
    begin fun i -> 
      let idx = i * 10 in
      let fname = sprintf "romer/ann/curve%04d.curve" idx in
	printf "romer/ann/curve%04d.curve\n%!" idx;
	Curve.load fname 
    end
  in
    optimal_constituents curves "tmp/optimal.sdf"



