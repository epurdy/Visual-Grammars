open Util.Misc
open Printf

let _ = 
  let in_gram_name = ref "" in
  let out_gram_name = ref "" in
  let training_curves = ref [] in

  let watson_prior_shape = ref 0.0 in
  let watson_prior_mean = ref 0.0 in

  let add_curve fname = (training_curves := fname :: !training_curves) in

  let _ = Arg.parse [
    "-input", Arg.Set_string in_gram_name, "Input grammar.";
    "-output", Arg.Set_string out_gram_name, "Output grammar.";
    "-prior_shape", Arg.Set_float watson_prior_shape, "Watson prior shape parameter.";
    "-prior_mean", Arg.Set_float watson_prior_mean, "Watson prior mean parameter.";
  ]
    add_curve
    "./prog -input in.gram -output -prior_shape [num] -prior_mean [num] out.gram curve1 curve2 curve3 ..."
  in

  let _ = 
    assert (!watson_prior_shape > 0.0);
    assert (!watson_prior_mean > 0.0);
  in
  let infile = open_in !in_gram_name in
  let outfile = open_out !out_gram_name in

  let training_curves = Array.of_list (List.rev !training_curves) in
  let training_curves = Array.map Curve.load training_curves in
  let trainfamilies = Array.map
    begin fun c -> 
      let fam = Sdf.make_full_family (Array.length c) in
	Models.EM.add_curve_data_to_family c fam
    end 
    training_curves
  in

  let gram = (Marshal.from_channel infile: Grammar.grammar) in
  let gram = 
    Xform.xform_pipeline
      Retrain.retrain_heuristic
      gram
      {Retrain.sparsefactor =0.; (* (float_of_int i) *)
       Retrain.watson_prior_shape = !watson_prior_shape;
       Retrain.watson_prior_mean = !watson_prior_mean;
       Retrain.curves = training_curves;
       Retrain.families = trainfamilies;
       Retrain.strat = Models.EM.strat;
       Retrain.name = "retrain";
       Retrain.parallel = true;
      };
  in
  let gram = 
      Xform.xform_pipeline
	Structure.Prune.prune_heuristic
	gram
	{Structure.Prune.prunethresh = 0.01}; (* 0.0001 *)
  in

    Marshal.to_channel outfile gram [];
    close_in infile;
    close_out outfile
