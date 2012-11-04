open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Cops
open Abstract
open Grammar
open Sdf

type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k) model = {
  add_curve_data_to_family: 
    Curve.t -> ('a,'b,'c) Abstract.frozen_grammar -> 
	       ('d,'e,'f) Abstract.frozen_grammar;
  strat: ('g,'h,'i,'j,'k) Parsing.strategy;
}

(* let display_parse model gram excurve curve x =  *)
(*   let n = Array.length curve in *)
(*   let family = Sdf.make_full_family n in *)
(*   let family = model.add_curve_data_to_family curve family in *)

(*   let qual, pairs = Parsing.viterbi gram family model.strat in *)
(* (\*   let qualfile = open_out qual_name in  *\) *)
(* (\*     printf "qual = %f\n#pairs = %d\n%!" qual (List.length pairs); *\) *)
(* (\*     fprintf qualfile "%f\n%!" qual; *\) *)
(* (\*     close_out qualfile; *\) *)

(*     let find x arr =  *)
(*       let rv = ref None in *)
(* 	Array.iteri *)
(* 	  begin fun i y -> *)
(* 	    if x = y then begin *)
(* 	      rv := Some i *)
(* 	    end *)
(* 	  end arr; *)
(* 	try  *)
(* 	  get !rv *)
(* 	with Failure "Got None, expected Some" -> *)
(* 	  fprintf stderr "longer_curves: x was not in array!\n"; *)
(* 	  get !rv *)
(*     in *)
(*     let thebigcurve = (Frozen.start gram).sdata.curve in *)
(*     let labels = mkhash 100 in *)

(*       List.iter  *)
(* 	begin fun (sid, scid, _, _) -> *)
(* 	  let symbol = Frozen.get_symbol gram sid in *)
(*  	  let scurve = Frozen.get_symbol family scid in  *)
(* 	  let thecurve = symbol.sdata.curve in *)
(* 	  let bg, en =  *)
(* 	    if symbol.startable then  *)
(* 	      thecurve.(0), thecurve.(0) *)
(* 	    else *)
(* 	      thecurve.(0), thecurve.(Array.length thecurve - 1)  *)
(* 	  in *)
(* 	  let ibg, ien = find bg thebigcurve, find en thebigcurve in *)

(* 	    (\* 	    labels <<+ (ibg, scurve.Sdf.first); *\) *)
(* 	    (\* 	    labels <<+ (ien, scurve.Sdf.last); *\) *)
(* 	    labels <<+ (scurve.sdata.Model.first_, ibg); *)
(* 	    labels <<+ (scurve.sdata.Model.last_, ien); *)
(* 	    printf "s%d (%d,%d) -> sc%d (%d,%d)\n" sid *)
(* 	      ibg ien *)
(* 	      scid *)
(* 	      (Frozen.get_symbol family scid).sdata.first_ *)
(* 	      (Frozen.get_symbol family scid).sdata.last_; *)
(* 	    Geometry.print_cpt bg; *)
(* 	    Geometry.print_cpt en; *)
(* 	    printf "---\n%!"; *)
(* 	end *)
(* 	pairs; *)

(*       Hashtbl.iter  *)
(* 	begin fun k v -> *)
(* 	  printf "%d -> %d\n%!" k v; *)
(* 	end *)
(* 	labels; *)

(*       let labels =  *)
(* 	Array.init (Array.length curve) *)
(* 	  begin fun i -> *)
(* 	    if labels >>? i then  *)
(* 	      (sprintf "%d" (labels >> i)) *)
(* 	    else *)
(* 	      "" *)
(* 	  end *)
(*       in *)

(*       let excurve = Curve.flip_xy excurve in *)
(*       let curve = Curve.flip_xy curve in *)
	
(* 	Viz.show_labeled_curve x excurve (Array.init (Array.length excurve) (sprintf "%d")); *)
(* 	Cairo.translate x.Svg.ctx 10. 0.; *)
(* 	Viz.show_labeled_curve x curve labels; *)

(* (\* 	Svg.finish x; *\) *)

(* 	qual *)
(* 	() *)
