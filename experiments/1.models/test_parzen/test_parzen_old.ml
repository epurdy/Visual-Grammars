open Util.Cops
open Util.Misc
open Printf
open Bounds

module C = Complex
module G = Geometry
module N = Parzen

let dir = "output/1.models/test_parzen"

let numrs = 3;;
let num_new_rs = 1000;;
let sigma = 1.0;;
let newsigma = sigma *. 0.5;;
let granularity = 100;;
let fgranularity = float_of_int granularity;;

let draw_points pts =
  let im = Image.create granularity granularity 0 in
    Array.iter 
      begin fun pt ->
	let relpt = map_to_unit_square defaultbounds pt in
	  match relpt with
	      None -> ()
	    | Some relpt -> begin
		let x,y = int_of_float (fgranularity *. relpt.C.re), 
		  int_of_float (fgranularity *. relpt.C.im) in
		let x = min (max x 0) (granularity-1) in
		let y = min (max y 0) (granularity-1) in
		  Image.set im 255 (x,y);
	      end
      end
      pts;
    im

(* Make points *)
let rs = Array.init numrs 
  begin fun i -> 
    {C.re = Random.float 1.0;
     C.im = Random.float 1.0}
  end;;
let smallsquare = {
  xmin = -.3.0;
  xmax =   3.0;
  ymin = -.3.0;
  ymax =   3.0;
};;
let rs = Array.map 
  begin fun r -> 
    map_from_unit_square smallsquare r
  end
  rs;;
Array.iter G.print_cpt rs;;
Pnm.save_pgm (draw_points rs)  (sprintf "%s/1.points.pgm" dir);;

(* Make nonparametric model *)
let rs = Array.map (fun z -> Shape.shape_of_complex_bme c0 z c1) rs;;
let model = N.make_model rs sigma granularity 
  (Shape.default_shape, Con.default_baseline_sigma)  0.00001 false;;
N.force model;;
let im = N.draw_model model;;
Pnm.save_ppm im  (sprintf "%s/2.density.pgm" dir);;

(* Get sample points *)
let new_rs = Array.init num_new_rs (fun i -> N.sample_pt model);;

(* let new_rs = Array.map *)
(*   begin fun z -> *)
(*     let x,y = z.C.re, z.C.im in *)
(*       {C.re=x; C.im=y} *)
(*   end *)
(*   new_rs;; *)

let new_rs_10 = Array.sub new_rs 0 10;;
let new_rs_100 = Array.sub new_rs 0 100;;

title "Make points:\n%!";;
Array.iter G.print_cpt new_rs;;
Pnm.save_pgm (draw_points new_rs_10)  (sprintf "%s/3.newpoints.0010.pgm" dir);;
Pnm.save_pgm (draw_points new_rs_100)  (sprintf "%s/5.newpoints.0100.pgm" dir);;
Pnm.save_pgm (draw_points new_rs)  (sprintf "%s/7.newpoints.1000.pgm" dir);;

let new_rs = Array.map (fun z -> Shape.shape_of_complex_bme c0 z c1) new_rs;;
let new_rs_10 = Array.sub new_rs 0 10;;
let new_rs_100 = Array.sub new_rs 0 100;;

(* Make new model *)
let model2_10 = N.make_model new_rs_10 newsigma granularity (Shape.default_shape,1.0) 0.00001 false;;
let model2_100 = N.make_model new_rs_100 newsigma granularity (Shape.default_shape,1.0) 0.00001 false;;
let model2 = N.make_model new_rs newsigma granularity (Shape.default_shape,1.0) 0.00001 false;;
N.force model2;;
N.force model2_10;;
N.force model2_100;;


Pnm.save_ppm (N.draw_model model2_10) (sprintf "%s/4.newdensity.0010.pgm" dir);;
Pnm.save_ppm (N.draw_model model2_100) (sprintf "%s/6.newdensity.0100.pgm" dir);;
Pnm.save_ppm (N.draw_model model2) (sprintf "%s/8.newdensity.1000.pgm" dir);;
