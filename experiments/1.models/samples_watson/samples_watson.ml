open Printf
open Util.Misc
open Util.Cops
module C = Complex

let scale = 100.
let ntrials = 5

let dir = "output/1.models/samples_watson"

let shape_to_triple shape = (c0, Shape.get_pt shape, c1)

let save_triangle shape fname = 
  let p = Shape.get_pt shape in
  let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in
    Curve.save fname (Curve.flip_xy curve)

let _ = 
  let maxwidth, maxheight = 4, 3 in
  let tex = open_out (sprintf "%s/watson.tex" dir) in
  let trueconc = 30. in

  let mp = {C.re = 0.5 +. (Sample.gaussian 0.3); 
	    C.im = (Sample.gaussian 1.0);} in
  let shape = Shape.shape_of_complex_bme c0 mp c1 in
  let _ = 
    save_triangle shape (sprintf "%s/watson_true.curve" dir);
    doit (sprintf "./show_curve.native %s/watson_true.curve %s/watson_true.svg" dir dir);

    fprintf tex "\\begin{table}\n";
    fprintf tex "\\begin{tabular}{l l l}\n";
    fprintf tex "number of samples & mean & concentration\\\\ \n";
    fprintf tex "\\hline\n";
    fprintf tex "(true) & \\includegraphics[width=0.6in]{%s/watson_true.png} & %0.2f\\\\ \n" 
      dir trueconc;    
    fprintf tex "\\hline\n";
  in

  let nsamples = [ 3; 10; 30; 100; 300; 1000] in

    List.iter 
      begin fun nsamples ->

	let distro = {Watson.mean = shape; Watson.conc_ = trueconc} in
	let samples = Array.init nsamples (fun i -> Watson.sample distro) in
	let est_data = Watson.fit_watson (Array.map shape_to_triple samples) in
	let z1,z2,z3 = est_data.Watson.mode in
	let est = Shape.shape_of_complex_bme z1 z2 z3 in

	  save_triangle est (sprintf "%s/watson_est_%d.curve" dir nsamples);
	  doit (sprintf "./show_curve.native %s/watson_est_%d.curve %s/watson_est_%d.svg" 
		  dir nsamples dir nsamples);

	  fprintf tex "%d & \\includegraphics[width=0.6in]{%s/watson_est_%d.png} & %0.2f \\\\ \n" 
	    nsamples dir nsamples est_data.Watson.conc;
      end
      nsamples;

    fprintf tex "\\end{tabular}\n";
    fprintf tex "\caption{Fitting the Watson distribution with different numbers of samples.}\n";
    fprintf tex "\\end{table}\n";

    close_out tex
