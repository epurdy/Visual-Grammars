open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Abstract
open Sdf
open Grammar

let simple_tuning excurve_name sdf_name training_fnames niters =
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
    let file = open_out (sprintf "tmp/simple_tuning.%d.gram" i) in
      Marshal.to_channel file gram [];
      close_out file;
      printf "LOGGING %d\n%!" i;
      Grammar.print_grammar gram;
  in

    logging 0 gram;

    let gram = ref gram in
      for i = 1 to niters do
	gram := Retrain.retrain
	  ~sparsefactor:(float_of_int i)
	  !gram (Array.to_list trainfamilies) 
	  Models.Simple.strat;
	(* gram := Structure.replacement ~shrink:0.1 !gram (Array.to_list trainfamilies);  *)
	logging i !gram;
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
    
    simple_tuning !example !sdf curves !niters
