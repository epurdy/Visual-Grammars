open Printf
open Util.Cops

module C = Complex
module I = Image
open Bounds

let bounds = {
  xmin = 0.;
  xmax = 1.;
  ymin = -.0.5;
  ymax = 0.5;}


let granularity = 20
let sigma = 1.0

let _ = 
  let leafnum = 2 in

  let curves = 
    Array.init 10
      begin fun i ->
	Curve.load (sprintf "leaves/leaf%02d/leaf%02d_%04d.curve" leafnum leafnum (i+1))
      end 
  in
  let scales = [| 0.5; 0.4; 0.3; 0.2; 0.1;
		  0.09; 0.08; 0.07; 0.06; 0.05; 0.04; 0.03; 0.02; 0.01;
		  0.009; 0.008; 0.007; 0.006; 0.005; 0.004; 0.003; 0.002; 0.001;|] in
  let find_scale s = 
    let idx, least = ref 0, ref 1.0 in
      Array.iteri
	begin fun i t ->
	  if abs_float (s-.t) < !least then begin
	    least := abs_float (s-.t);
	    idx := i;
	  end
	end
	scales;
      !idx
  in

  let samples = Array.init (Array.length scales) (fun x -> []) in

    Array.iter 
      begin fun curve ->
	let n = Array.length curve in
	let nf = float_of_int n in
	  for len = 1 to n/2 do
	    printf "len=%d\n%!" len;
	    let scale = (float_of_int len) /. nf in
	    let si = find_scale scale in
	      for i = 0 to n-1 do
		let j, k = (i + len/2) mod n , (i + len) mod n in
		let shape = Shape.shape_of_complex_bme curve.(i) curve.(j) curve.(k) in
		let x = Shape.get_pt shape in

		  samples.(si) <- x :: samples.(si);
	      done
	  done;
      end
      curves;

    Array.iteri
      begin fun i ssamples ->
	let maxcount = ref 1 in
	let im = Image.create (granularity+1) (granularity+1) 0 in
	  List.iter
	    begin fun sample ->
	      let x,y = Bounds.bounds_to_array_nice bounds granularity sample in
		I.set im (I.get im (x,y) + 1) (x,y);
	    end
	    ssamples;

	  let wdata = Watson.fit_watson (Array.map (fun x -> (c0,x,c1)) (Array.of_list ssamples)) in
	    

	    for y=0 to granularity do
	      for x=0 to granularity do 
		maxcount := max (I.get im (x,y)) !maxcount;
	      done
	    done;

	    let im = Image.map
	      begin fun count ->
		let count = (255 * count) / !maxcount in
		  (count,count,count);
	      end
	      im
	    in
	      I.set im (255,0,0) (Bounds.bounds_to_array_nice bounds granularity c0);
	      I.set im (0,255,0) (Bounds.bounds_to_array_nice bounds granularity c1);

	      Pnm.save_ppm im (sprintf "density-%0.3f-%0.2f.ppm" scales.(i) wdata.Watson.conc);
      end
      samples
	  
	  
	  
