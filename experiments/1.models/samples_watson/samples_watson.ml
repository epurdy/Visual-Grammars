open Printf
open Util.Misc
open Util.Cops
module C = Complex

let scale = 100.
let ntrials = 5

let dir = "output/1.models/samples_watson"

(* let shape_to_triple shape = (c0, Shape.get_pt shape, c1) *)

(* let save_triangle shape fname =  *)
(*   let p = Shape.get_pt shape in *)
(*   let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in *)
(*     Curve.save fname (Curve.flip_xy curve) *)

let _ = 
  let maxwidth, maxheight = 4, 3 in
  let ntrials = 5 in

  let trueconc = 30. in
  let mp = {C.re = 0.5 +. (Sample.gaussian 0.3); 
	    C.im = (Sample.gaussian 1.0);} in
  let shape = Shape.shape_of_complex_bme c0 mp c1 in
  let distro = Watson.watson_distro_family.Distro.build
    {Watson.mean_shape = shape; Watson.concentration = trueconc} 
  in

  let tex = open_out (sprintf "%s/watson.tex" dir) in
  let prefix = sprintf "%s/watson_true" dir in
  let [picstring; concstring] = 
    Watson.watson_distro_family.Distro.show distro prefix 
  in 


  let nsamples = [ 3; 10; 30; 100; 300; 1000] in

    for trial = 1 to ntrials do
      fprintf tex "\\begin{table}\n";
      fprintf tex "\\begin{tabular}{l l l}\n";
      fprintf tex "number of samples & mean & concentration\\\\ \n";
      fprintf tex "\\hline\n";
      fprintf tex "(true) & %s & %s\\\\ \n" picstring concstring;    
      fprintf tex "\\hline\n";

      List.iter 
	begin fun nsamples ->

	  let samples = Array.init nsamples
	    begin fun i -> 
	      let shape = Watson.watson_distro_family.Distro.sample distro in
		(0.0, shape)
	    end
	  in
	  let distro, est_data = 
	    Watson.watson_distro_family.Distro.infer 
	      {Watson.samples = samples;
	       Watson.gamma_shape_param=100.;
	       Watson.gamma_mean_param=100.;
	      }
	  in

	  let prefix = sprintf "%s/watson_est_%d" dir nsamples in
	  let [picstring; concstring] = 
	    Watson.watson_distro_family.Distro.show distro prefix 
	  in 
	    fprintf tex "%d & %s & %s \\\\ \n" nsamples picstring concstring;

	end
	nsamples;

      fprintf tex "\\end{tabular}\n";
      fprintf tex "\caption{Fitting the Watson distribution with different numbers of samples.}\n";
      fprintf tex "\\end{table}\n";
    done;

    close_out tex
