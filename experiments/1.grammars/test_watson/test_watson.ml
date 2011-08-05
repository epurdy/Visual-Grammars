open Printf
open Util.Misc
open Util.Cops
module C = Complex

let scale = 100.
let ntrials = 10

let shape_to_triple shape = (c0, Shape.get_pt shape, c1)

let save_triangle shape fname = 
  let p = Shape.get_pt shape in
  let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in
    Curve.save fname (Curve.flip_xy curve)

let _ = 
  for i = 1 to ntrials do
    let mp = {C.re = 0.5 +. (Sample.gaussian 0.3); 
	      C.im = (Sample.gaussian 1.0);} in
    let shape = Shape.shape_of_complex_bme c0 mp c1 in
    let distro = {Watson.mean = shape; Watson.conc_ = 30.} in
    let samples = Array.init 20 (fun i -> Watson.sample distro) in
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
  done
