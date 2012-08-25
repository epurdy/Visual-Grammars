open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf
open Parsing

(* let product_viterbi gram family strat = *)
(*   let qual = ref infinity in *)
(*     (\* chart maps (X,I) -> q, X=nonterminal, I=SDF interval *\) *)
(*   let table = Parsing.new_parse_table (Frozen.num_symbols gram) *)
(*     (Frozen.num_symbols family)  *)
(*     "viterbi" false in *)
(*   let toplevel = ref None in *)
    
(*     Frozen.iter_symbols family *)
(*       begin fun scurve ->  *)
(* 	if strat.lexical_ok scurve then begin *)
(* 	  Frozen.iter_symbols gram *)
(* 	    begin fun symbol ->  *)
(* 	      let cost = strat.lexical_cost symbol scurve in *)
(* 	      let cost = 0.125 *. cost in *)
(* 		Parsing.set table (symbol.sid, scurve.sid) cost; *)
(* 	    end *)
(* 	end; *)

(* 	if strat.binary_ok scurve then begin *)

(* 	  Frozen.iter_symbols gram *)
(* 	    begin fun symbol -> *)
(* 	      if strat.compatible symbol scurve then begin *)
(* 		let tot = ref 0. in *)
(* 		let totflag = ref false in *)

(* 		  Frozen.iter_decompositions gram symbol *)
(* 		    begin fun  prod -> *)
(* 		      let prodcost = ref infinity in *)

(* 			Frozen.iter_decompositions family scurve *)
(* 			  begin fun dcomp -> *)
(* 			    let cost = (strat.binary_cost prod dcomp) +.  *)
(* 			      (Parsing.get table (prod.leftsid, dcomp.leftsid)) +. *)
(* 			      (Parsing.get table (prod.rightsid, dcomp.rightsid))  *)
(* 			    in *)
(* 			      if cost < !prodcost then begin *)
(* 				prodcost := cost; *)
(* 			      end; *)
(* 			  end; *)

(* 			if not symbol.sdata.closed then begin *)
(* 			  tot := !prodcost +. !tot; *)
(* 			end *)
(* 			else begin  *)
(* 			  if !totflag then *)
(* 			    tot := min !tot !prodcost *)
(* 			  else begin  *)
(* 			    tot := !prodcost; *)
(* 			    totflag := true; *)
(* 			  end *)
(* 			end *)
(* 		    end; *)

(* 		  if !tot < Parsing.get table (symbol.sid, scurve.sid) then *)
(* 		    Parsing.set table (symbol.sid, scurve.sid) !tot; *)
(* 	      end *)
(* 	    end; *)
(* 	end; *)

(* 	if strat.goal_ok scurve then begin *)
(* 	  let cost = strat.goal_cost family.f_gdata scurve +. (Parsing.get table (0, scurve.sid)) in *)
(* 	    (\** TODO is that zero a problem? *\) *)
(* 	    if cost < !qual then begin *)
(* 	      toplevel := Some (0, scurve.sid); *)
(* 	      qual := cost; *)
(* 	    end *)
(* 	end *)

(*       end; *)

(*     if false && not (isfinite !qual) then *)
(*       failwith "Could not parse!"; *)


(*     !qual *)


let parse sdf_name excurve_name curve_name qual_name = 
  let curve1 = Curve.load curve_name in
  let curve2 = Curve.flip curve1 in
  let n = Array.length curve1 in
  let family = Sdf.make_full_family n in
  let family1 = Models.Mpeg7_reverse.add_curve_data_to_family curve1 family in
  let family2 = Models.Mpeg7_reverse.add_curve_data_to_family curve2 family in

  let excurve = Curve.load excurve_name in

  let gfamily = Sdf.make_balanced_family (Array.length excurve) in
(*   let gfamily = Sdf.make_balanced_flexible_family (Array.length excurve) 4 in *)
(*   let gfamily = Sdf.make_balanced_flexible_family2 (Array.length excurve) 8 in *)
(*   let gfamily = Sdf.load_family sdf_name in *)

  let gfamily = Sdf.bottom_out_family gfamily in
  let gram = Models.Mpeg7_reverse.make_grammar (excurve, gfamily) in
  let _ = Abstract.reorder_symbols gram in
  let _ = Models.Mpeg7_reverse.propagate_geometry gram in

  let qual1, pairs = Parsing.viterbi gram family1 Models.Mpeg7_reverse.strat in
  let qual2, pairs = Parsing.viterbi gram family2 Models.Mpeg7_reverse.strat in
  let qual = min qual1 qual2 in
  let qualfile = open_out qual_name in 
    printf "qual=%f\n%!" qual;
    fprintf qualfile "%f\n%!" qual;
    close_out qualfile
      
let _ =
  let sdf_name, excurve_name, curve_name, qual_name = 
    try
      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4)
    with _ ->
      printf "usage: ./%s ex.sdf excurve.curve curve.curve out.qual" Sys.argv.(0);
      "", "", "", ""
  in
    (* flipping them! *)
  let excurve_name, curve_name = curve_name, excurve_name in

    parse sdf_name excurve_name curve_name qual_name
