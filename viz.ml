open Svg
module C=Complex
open Util.Misc
open Printf

let show_samples x ?(bottom_buffer=4.5) title ncols samples =
  Svg.show_title x title;
  Svg.draw_in_table x ncols (Array.length samples)
    begin fun x i ->
      Svg.draw_closed_curve_wt x samples.(i) 0.01;
    end;
  Cairo.translate x.Svg.ctx 0. bottom_buffer

let show_samples_midpoints x title ncols basecurve (samples,mpchoices) buffer =
  let radius=0.03 in
    Svg.show_title x title;
    Svg.draw_in_table x ncols (Array.length samples)
      begin fun x i ->

	let n = Array.length samples.(i) in
	  Array.iter
	    begin fun mp ->
	      Svg.draw_circle_filled x mp radius;
	    end
	    mpchoices.(i);
	  Svg.draw_circle x samples.(i).(0) radius;
	  Svg.draw_circle x samples.(i).(n-1) radius;

  	  Svg.draw_curve_wt x samples.(i) 0.01;
  	  Svg.draw_closed_curve_wt x basecurve 0.002;

      end;
    Cairo.translate x.Svg.ctx 0. buffer

let save_samples dir i samples =
  (* let samples = Array.map (fun c -> Curve.align c training.(0)) samples in *)
  let samples = Array.map
    (fun c -> Array.map 
       (fun z -> 
	  {C.re=1000. *. z.C.re;C.im=1000. *. z.C.im})
       c) 
    samples in

    Curve.save_all ((sprintf "%s/sample.%04d.curve") dir) samples;
    begin try
      doit (sprintf "mkdir %s/iter.%d" dir i);
    with 
	_ -> (); 
    end;
    doit (sprintf "mv %s/sample.*.curve %s/iter.%d/" dir dir i)




(* let show_samples_old gram = *)

(*   (\** draw several random samples *\) *)
(*   let samples = Array.init nsamples (fun i -> Grammar.sample gram p q) in *)
(*   let samples = Array.map *)
(*     begin fun samp -> *)
(*       let bds = Bounds.nice_curve_bounds samp in *)
(* 	Array.map (fun pt -> map_to_unit_square_strict bds pt) curve *)
(*     end *)
(*     samples  *)
(*   in *)

(*     (\** represent each sample as a flip book somehow *\) *)
(*     Array.iter *)
(*       begin fun sample -> *)
(*   	Cairo.translate x.Svg.ctx (2. *. w) h; *)
(*   	Svg.draw_closed_curve_wt x sample 1.; *)
(*   	Cairo.translate x.Svg.ctx (-. 2. *. w) h; *)
(*   	(\* Array.iteri *\) *)
(*   	(\*   begin fun i ex -> *\) *)
(*   	(\*     Svg.draw_closed_curve_wt x ex 0.5; *\) *)
(*   	(\*     Svg.draw_closed_curve_wt x (Curve.align sample ex) 1.; *\) *)
(*   	(\*     Cairo.translate x.Svg.ctx w 0.; *\) *)
(*   	(\*     if (i+1) mod ncols = 0 then begin *\) *)
(*   	(\* 	Cairo.translate x.Svg.ctx (-. (float_of_int ncols) *. w) (h); *\) *)
(*   	(\*     end *\) *)
(*   	(\*   end  *\) *)
(*   	(\*   training; *\) *)
(*   	(\* Cairo.translate x.Svg.ctx (-. (float_of_int (ntrain mod ncols)) *. w) (h); *\) *)
(*       end *)
(*       samples *)
