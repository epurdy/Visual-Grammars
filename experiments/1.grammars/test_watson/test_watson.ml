open Printf
open Util.Misc
open Util.Cops
module C = Complex

let scale = 100.
let ntrials = 5

let shape_to_triple shape = (c0, Shape.get_pt shape, c1)

let save_triangle shape fname = 
  let p = Shape.get_pt shape in
  let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in
    Curve.save fname (Curve.flip_xy curve)

let _ = 
  let maxwidth, maxheight = 4, 3 in
  let tex = open_out "experiments/1.grammars/test_watson/output.d/watson.tex" in
  let trueconc = 30. in
  let nsamples = 20 in
    fprintf tex "\\begin{itemize}\n\n";
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
	    let fname = (sprintf "tmp/watson.%d.%d.curve" i j) in
	      save_triangle samp fname;
	      fnames := !fnames ^ (" " ^ fname);
	  end
	  samples;
	save_triangle shape (sprintf "tmp/watson.%d.true.curve" i);
	save_triangle est (sprintf "tmp/watson.%d.est.curve" i);
	doit (sprintf "./show_curve.native tmp/watson.%d.true.curve tmp/watson.%d.true.svg" i i);
	doit (sprintf "./show_superimposed.native -fname tmp/watson.%d.est.svg tmp/watson.%d.true.curve tmp/watson.%d.est.curve -title '' " i i i);
	doit (sprintf "./show_curves.native -fname tmp/watson.%d.samples.svg %s -title ''" i !fnames);
	fprintf tex "\\item Round %d\\\\ \n" i;
	fprintf tex "Here is the initial triangle $T$:\n\n";
	fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{./1.grammars/test_watson/output.d/watson.%d.true.eps}\n\n" maxwidth maxheight i;
	fprintf tex "Here are samples from Watson$(T,%f)$:\n\n" trueconc;
	fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{./1.grammars/test_watson/output.d/watson.%d.samples.eps}\n\n" maxwidth maxheight i;
	fprintf tex "Here is the initial triangle again:\n\n";
	fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{./1.grammars/test_watson/output.d/watson.%d.true.eps}\n\n" maxwidth maxheight i;
	fprintf tex "Here are the initial triangle and the estimated triangle, superimposed. The original triangle is blue, the estimated triangle purple. \n\n";
	fprintf tex "\\includegraphics[width=%din,height=%din,keepaspectratio]{./1.grammars/test_watson/output.d/watson.%d.est.eps}\n\n" maxwidth maxheight i;
	fprintf tex "The true concentration was %f, while the estimated concentration was %f.\n\n" trueconc est_data.Watson.conc;
    done;
    fprintf tex "\\end{itemize}";
    close_out tex
