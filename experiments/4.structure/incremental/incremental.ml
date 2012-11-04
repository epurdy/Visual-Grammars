open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Abstract
open Sdf
open Grammar
open Models.Simple

let incremental excurve_name sdf_name training_fnames niters =
  let training = Array.map Curve.load training_fnames in
  let trainfamilies = Array.map
    begin fun c -> 
      let fam = Sdf.make_full_family (Array.length c) in
	Models.EM.add_curve_data_to_family c fam
    end 
    training 
  in

  let gram = Xform.ctor_pipeline 
    Structure.Init.init_heuristic
    {Structure.Init.excurve = Curve.load excurve_name;
     Structure.Init.family = Sdf.load_family sdf_name;
     Structure.Init.name = sprintf "INIT: %s %s" excurve_name sdf_name;
    }
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
	  gram := Structure.Incremental.incorporate_worst_midpoint !gram training trainfamilies;
	  logging (10*i + j) !gram;
	done;

	for j = 6 to 10 do
	  gram :=
	    Xform.xform_pipeline
	      Retrain.retrain_heuristic
	      !gram
	      {Retrain.sparsefactor =0.; (* (float_of_int i) *)
	       Retrain.watson_prior_shape = 1000. *. 1000.;
	       Retrain.watson_prior_mean = 1000.;
	       Retrain.curves = training;
	       Retrain.families = trainfamilies;
	       Retrain.strat = Models.EM.strat;
	       Retrain.name = "correlated_retrain";
	       Retrain.parallel = true;
	      };
	  gram := 
	    Xform.xform_pipeline
	      Structure.Prune.prune_heuristic
	      !gram
	      {Structure.Prune.prunethresh = 0.01}; (* 0.0001 *)

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
