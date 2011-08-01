open Printf
open Util.Hops
open Util.Cops
open Abstract
open Sdf
open Geometry
open Models.Simple
open Parsing

module C = Complex

let _ = 
  let x = Svg.create Sys.argv.(1) in

  let excurve = Curve.load "romer/ann/curve0000.curve" in


  (* note that excurve is being included below! *)
  let curves = Array.init 16
    begin fun i -> 
      let idx = i * 10 in
      let fname = sprintf "romer/ann/curve%04d.curve" idx in
	printf "romer/ann/curve%04d.curve\n%!" idx;
	Curve.load fname
    end
  in

  let n = Array.length excurve in
  let family = Sdf.make_full_family n in
  let family = Models.Simple.add_curve_data_to_family excurve family in

  let strat = Models.Simple.strat in
    
  let tab = mkhash 100 in
  let bestdcomp = mkhash 100 in
  let best = ref 0. in
  let besttop = ref 0 in

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

	      let wdata = Watson.fit_watson triples in
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

    let excurve = Curve.normalize excurve in
    let doubled = Array.append excurve excurve in

    let stuff = ref [] in

    let rec visit sid = 
      let scurve = Frozen.get_symbol family sid in
	if bestdcomp >>? sid then begin
	  let dcompid = bestdcomp >> sid in
	  let dcomp = Frozen.get_composition family dcompid in
	  let thecurve = Array.sub doubled scurve.sdata.first_ (scurve.sdata.len_ + 1) in
	  let thecurve = Curve.flip_xy thecurve in
	  let themidpoints = [| Geometry.flip_xy excurve.(dcomp.cdata.md_) |] in

	    stuff := (scurve.sdata.len_, thecurve, themidpoints) :: !stuff;
	    printf "[%d,%d] -> [%d,%d] [%d,%d]\n" 
	      dcomp.cdata.bg_ dcomp.cdata.en_ dcomp.cdata.bg_ dcomp.cdata.md_ dcomp.cdata.md_ dcomp.cdata.en_;
	    visit dcomp.leftsid;
	    visit dcomp.rightsid;
	end
	else begin
	  (* 	let scurve = family.get_symbol sid in *)
	  (* 	  printf "[%d,%d] x\n" scurve.first scurve.last *)
	end
    in
      visit !besttop;

      let stuff = List.sort (fun (l1, _, _) (l2, _, _) -> compare l2 l1) !stuff in
      let stuff = Array.of_list stuff in
      let scurves = Array.map (fun (len, thecurve, themidpoints) -> thecurve) stuff in
      let midpoints = Array.map (fun (len, thecurve, themidpoints) -> themidpoints) stuff in


      Viz.show_samples_midpoints x "" 6 (Curve.flip_xy excurve)
	(scurves, midpoints) 1.0;
      Svg.finish x;
;;
