open Printf
open Util.Misc
open Util.Cops
module C = Complex

let scale = 100.
let ntrials = 5

let dir = "output/1.models/test_watson"

let shape_to_triple shape = (c0, Shape.get_pt shape, c1)

let save_triangle shape fname = 
  let p = Shape.get_pt shape in
  let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in
    Curve.save fname (Curve.flip_xy curve)

let _ = 
  let maxwidth, maxheight = 4, 3 in
  let tex = open_out "output/1.models/test_watson/watson.tex" in
  let trueconc = 30. in
  let nsamples = 20 in
    (* fprintf tex "\\begin{itemize}\n\n"; *)
    for i = 1 to ntrials do
      let mp = {C.re = 0.5 +. (Sample.gaussian 0.3); 
		C.im = (Sample.gaussian 1.0);} in
      let shape = Shape.shape_of_complex_bme c0 mp c1 in
      let distro = {Watson.mean = shape; Watson.conc_ = trueconc} in
      let samples = Array.init nsamples (fun i -> Watson.sample distro) in
      let est_data = Watson.fit_watson (Array.map shape_to_triple samples) in
      let z1,z2,z3 = est_data.Watson.mode in
      let est = Shape.shape_of_complex_bme z1 z2 z3 in

      let fnames = ref "" in
	Array.iteri
	  begin fun j samp ->
	    let fname = (sprintf "%s/watson_%d_%d.curve" dir i j) in
	      save_triangle samp fname;
	      fnames := !fnames ^ (" " ^ fname);
	  end
	  samples;
	save_triangle shape (sprintf "%s/watson_%d_true.curve" dir i);
	save_triangle est (sprintf "%s/watson_%d_est.curve" dir i);
	doit (sprintf "./show_curve.native %s/watson_%d_true.curve %s/watson_%d_true.svg" dir i dir i);
	doit (sprintf "./show_curve.native %s/watson_%d_est.curve %s/watson_%d_est.svg" dir i dir i);
	(* doit (sprintf "./show_superimposed.native -fname tmp/watson_%d_est.svg tmp/watson_%d_true.curve tmp/watson_%d_est.curve -title '' " i i i); *)
	doit (sprintf "./show_curves.native -fname %s/watson_%d_samples.svg %s -title ''" dir i !fnames);

	fprintf tex "\\begin{figure}\n";
	fprintf tex "\\includegraphics[width=0.6in]{output/1.models/test_watson/watson_%d_true.png}\\\\ \n" i;
	fprintf tex "\\includegraphics[width=4in]{output/1.models/test_watson/watson_%d_samples.png}\\\\ \n" i;
	fprintf tex "\\includegraphics[width=0.6in]{output/1.models/test_watson/watson_%d_est.png}\n" i;
	fprintf tex "\caption{Experimenting with the Watson distribution, part %d. In the first row, the original triangle $T$. Subsequent rows are samples from Watson$(T,%0.2f)$. The final row is the mode of the estimated Watson distribution. The estimated concentration was $%0.2f$.}\n" i trueconc est_data.Watson.conc;
	fprintf tex "\\label{fig-watson-%d}\n" i;
	fprintf tex "\\end{figure}\n\n";


	(* fprintf tex "\\item Round %d\\\\ \n" i; *)
	(* fprintf tex "Here is the initial triangle $T$:\n\n"; *)
	(* fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{experiments/1.grammars/test_watson/output.d/watson_%d_true.png}\n\n" maxwidth maxheight i; *)
	(* fprintf tex "Here are samples from Watson$(T,%f)$:\n\n" trueconc; *)
	(* fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{experiments/1.grammars/test_watson/output.d/watson_%d_samples.png}\n\n" maxwidth maxheight i; *)
	(* fprintf tex "Here is the initial triangle again:\n\n"; *)
	(* fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{experiments/1.grammars/test_watson/output.d/watson_%d_true.png}\n\n" maxwidth maxheight i; *)
	(* fprintf tex "Here are the initial triangle and the estimated triangle, superimposed. The original triangle is blue, the estimated triangle purple. \n\n"; *)
	(* fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{experiments/1.grammars/test_watson/output.d/watson_%d_est.png}\n\n" maxwidth maxheight i; *)
	(* fprintf tex "The true concentration was %f, while the estimated concentration was %f.\n\n" trueconc est_data.Watson.conc; *)
    done;
    (* fprintf tex "\\end{itemize}"; *)
    close_out tex
