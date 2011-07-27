open Printf
open Util.Hops
open Abstract
open Sdf
open Geometry
open Util.Cops

module C = Complex
module S = Sdf

let _ = 
  let x = Svg.create Sys.argv.(1) in

  let excurve = Curve.load "romer/ann/curve0000.curve" in
  let doubled = Array.append excurve excurve in

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
    
  let tab = mkhash 100 in
  let bestdcomp = mkhash 100 in
  let best = ref 0. in
  let besttop = ref 0 in

    family.iter_symbols
      begin fun scurve ->
	if family.x.lexical_ok scurve then begin
	  tab << (scurve.S.sid, 0.);
	end;

	if family.x.binary_ok scurve then begin
	  family.iter_decompositions scurve
	    begin fun dcomp ->
	      let bg, md, en = dcomp.bg, dcomp.md, dcomp.en in

	      let triples = Array.map 
		(fun c -> (c.(bg), c.(md), c.(en)))
		   curves
	      in

	      let wdata = Watson.fit_watson triples in
	      let cost = wdata.Watson.cost +.
		(tab >> dcomp.leftsid) +. (tab >> dcomp.rightsid) in

		if cost <= (tab >>! (scurve.S.sid, infinity)) then begin
		    tab << (scurve.S.sid, cost);
		    bestdcomp << (scurve.S.sid, dcomp.S.cid);
		  end
	    end
	end;
	
	if family.x.goal_ok scurve then begin
	  if tab >>! (scurve.S.sid,infinity) < !best then begin 
	    best := tab >> scurve.S.sid;
	    besttop := scurve.S.sid;
	  end
	end
      end;

    let excurve = Curve.normalize excurve in
    let doubled = Array.append excurve excurve in

    let scurves = ref [] in
    let midpoints = ref [] in

    let rec visit sid = 
      let scurve = family.get_symbol sid in
	if bestdcomp >>? sid then begin
	  let dcompid = bestdcomp >> sid in
	  let dcomp = family.get_composition dcompid in
	  let thecurve = Array.sub doubled scurve.first (scurve.len + 1) in
	  let thecurve = Curve.flip_xy thecurve in
	    scurves := thecurve :: !scurves;
	    midpoints := [| Geometry.flip_xy excurve.(dcomp.md) |] :: !midpoints;
	    printf "[%d,%d] -> [%d,%d] [%d,%d]\n" 
	      dcomp.bg dcomp.en dcomp.bg dcomp.md dcomp.md dcomp.en;
	    visit dcomp.leftsid;
	    visit dcomp.rightsid;
	end
	else begin
	  (* 	let scurve = family.get_symbol sid in *)
	  (* 	  printf "[%d,%d] x\n" scurve.first scurve.last *)
	end
    in
      visit !besttop;

      Viz.show_samples_midpoints x "" 6 (Curve.flip_xy excurve)
	(Array.of_list (List.rev !scurves), Array.of_list (List.rev !midpoints)) 1.0;
      Svg.finish x;
;;
