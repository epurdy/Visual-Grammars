open Util.Misc
open Util.Cops

module C = Complex

type parzen_curve = Parzen.model array

let neglog_prob pc c =
  let totcost = ref 0. in
    Array.iteri
      begin fun i p ->
	let shape = Shape.shape_of_complex_bme c.(0) p c.(1) in
	  totcost := !totcost +. Parzen.cost pc.(i) shape;
      end
      c;
    !totcost

let prob pc c =
  let nlp = neglog_prob pc c in
    exp (-. nlp)

let sample (pc: parzen_curve) =
  Array.map
    begin fun model ->
      let shape = Parzen.sample model in
      let p = Shape.get_pt shape in
	(cxre 1000.) *& p
    end
    (pc : parzen_curve)

let infer curves =
  let n = Array.length curves.(0) in
  let models =
    Array.mapi
      begin fun i p ->
	let shapes =
	  Array.map
	    begin fun c ->
	      let shape = Shape.shape_of_complex_bme c.(0) c.(i) c.(n/2) in
		shape
	    end
	    curves
	in
	  Parzen.make_model shapes 0.0000001 1000 (Shape.default_shape, 0.0001) 0.0 false
      end
      curves.(0)
  in

    models, true

let default_distro () = [| Parzen.new_straight_model 1.0 1.0 100 |]

let parzen_curve_distro_family = {
  Distro.default_distro = default_distro;
  Distro.build = (fun x -> x);
  Distro.unpack = (fun x -> x);
  Distro.sample = sample;
  Distro.infer = infer;
  Distro.prob = prob;
  Distro.neglog_prob = neglog_prob;
  Distro.show = (fun x -> failwith "show parzen curve not implemented");
}
