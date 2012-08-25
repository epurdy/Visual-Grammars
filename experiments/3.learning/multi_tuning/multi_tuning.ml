open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Abstract
open Sdf
open Grammar

let ncopies = 5;;
let fncopies = float_of_int ncopies;;

(* let orig_prob = 0.8;; *)
let orig_prob = 1. /. fncopies;;

let multi_tuning excurve_name sdf_name training_fnames niters =
  let excurve = Curve.load excurve_name in
  let exfamily = excurve, Sdf.load_family sdf_name in
  let gram = Models.Simple.make_grammar exfamily in

  let gram = 
    let live = new_live_grammar
      ~nsyms:(Frozen.num_symbols gram) ~ncomps:(Frozen.num_compositions gram)
      gram.f_gdata in

      Frozen.iter_symbols gram
	begin fun sym ->
	  imake_new_symbol live sym.sdata sym.startable;
	end;

      iter_all_compositions gram
	begin fun comp ->
	  for i = 1 to ncopies do
	    let newgeom, prob = 
	      if i=1 then begin
		comp.cdata.geom, comp.cdata.prob *. orig_prob
	      end
	      else begin
		(match comp.cdata.geom with
		    Watson watson ->  
		      Watson {
			Watson.mean = Watson.sample watson; 
			Watson.conc_ = watson.Watson.conc_}
		  | Improper -> Improper), 
		comp.cdata.prob *. (1. -. orig_prob) /. (fncopies -. 1.)
	      end
	    in
	    let cdata = 
	      {prob = prob;
	       cost = -. log prob;
	       geom = newgeom;
	       ocurve = comp.cdata.ocurve;
	       lcurve = comp.cdata.lcurve;
	       rcurve = comp.cdata.rcurve;
	      }
	    in
	      imake_new_composition live comp.topsid (comp.leftsid, comp.rightsid) cdata;
	  done;	    
	end;

      finalize live
  in

  let training = Array.map Curve.load training_fnames in
  let trainfamilies = Array.map
    begin fun c -> 
      let fam = Sdf.make_full_family (Array.length c) in
	Models.Simple.add_curve_data_to_family c fam
    end 
    training 
  in

  let logging i gram = 
    let file = open_out (sprintf "tmp/multi_tuning.%d.gram" i) in
      Marshal.to_channel file gram [];
      close_out file;
      printf "LOGGING %d\n%!" i;
      Grammar.print_grammar gram;
  in

    logging 0 gram;

    let gram = ref gram in
      for i = 1 to niters do
	gram := Retrain.retrain
	  ~sparsefactor:0. (* (float_of_int i) *)
	  ~prunethresh:0.01 (* 0.0001 *)
	  !gram (Array.to_list trainfamilies) 
	  Models.Simple.strat;
	(* gram := Structure.replacement ~shrink:0.1 !gram (Array.to_list trainfamilies);  *)
	logging i !gram;
      done

let _ = 
  let example = ref "NONE.curve" in
  let sdf = ref "NONE.sdf" in
  let niters = ref 0 in
  let curves = ref [] in

  let add_curve fname = (curves := fname :: !curves;) in

  let _ = Arg.parse [
    "-example", Arg.Set_string example, "Example curve.";
    "-niters", Arg.Set_int niters, "Number of EM iterations.";
    "-sdf", Arg.Set_string sdf, "SDF for decomposing example.";]
    add_curve
    "./prog -example example -sdf sdf curve1 curve2 curve3..."
  in

  let curves = Array.of_list (List.rev !curves) in
    
    multi_tuning !example !sdf curves !niters
