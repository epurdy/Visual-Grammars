open Util.Cops
open Util.Misc
open Printf
open Bounds

module C = Complex
module G = Geometry
module N = Parzen

let dir = "output/1.models/test_parzen"

let shape_to_triple shape = (c0, Shape.get_pt shape, c1)

let scale = 100.

let default_baseline_weight = 0.1
let default_baseline_sigma = 0.05
 
let save_triangle shape fname = 
  let p = Shape.get_pt shape in
  let curve =  (Array.map (( *.&) scale) [| c0; p; c1 |]) in
    Curve.save fname (Curve.flip_xy curve)

let numrs = 3;;
let num_new_rs = 30;;
let sigma = 1.0;;
let newsigma = sigma *. 0.5;;
let granularity = 100;;
let fgranularity = float_of_int granularity;;

let _ = 
  let rs = [| {C.re=0.5;C.im=0.3;}; {C.re=0.3;C.im=0.4;}; {C.re=0.2;C.im=0.5;} |] in
  let rs = Array.map (fun z -> Shape.shape_of_complex_bme c0 z c1) rs in
  let bandwidths = [| 0.01; 0.03; 0.1; 0.3; 1.0 |] in

  let fnames = ref "" in
    Array.iteri
      begin fun i s ->
	let fname = sprintf "%s/parzen_true_%d.curve" dir i in
	  save_triangle s fname;
	  fnames := !fnames ^ (" " ^ fname);
      end
      rs;
    doit (sprintf "./show_curves.native -fname %s/parzen_true.svg %s -title ''" dir !fnames);      

    Array.iter
      begin fun bw ->
	let model = N.make_model rs bw granularity 
	  (Shape.default_shape, default_baseline_sigma)  0.0 false
	in
	let samples  = Array.init num_new_rs (fun i -> N.sample model) in
	let fnames = ref "" in
	  Array.iteri
	    begin fun i s ->
	      let fname = sprintf "%s/parzen_%0.2f_%d.curve" dir bw i in
		save_triangle s fname;
		fnames := !fnames ^ (" " ^ fname);
	    end
	    samples;
	  doit (sprintf "./show_curves.native -fname %s/parzen_%0.2f_samples.svg %s -title ''" dir bw !fnames);      
      end
      bandwidths;


    
    ()
