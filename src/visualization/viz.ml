open Svg
module C=Complex
open Util.Misc
open Printf
module X = Cairo

let show_labeled_curve x curve labels =
  X.set_source x.ctx (X.Pattern.create_rgb ~red:0.0 ~green:0.0 ~blue:0.0);
  X.select_font_face x.ctx "Georgia" X.FONT_SLANT_NORMAL X.FONT_WEIGHT_BOLD;
  X.set_font_size x.ctx 0.3;
  let curve = Curve.normalize curve in
  let curve = Array.map (fun {C.re=x;C.im=y;} -> {C.re=x*.10.; C.im=y*.10.}) curve in
  let radius=0.3 in
    Array.iter
      begin fun z ->
	Svg.draw_circle_filled x z radius;
      end
      curve;

    Svg.draw_closed_curve_wt x curve 0.01;

    X.set_source x.ctx (X.Pattern.create_rgb ~red:1.0 ~green:1.0 ~blue:1.0);
    (* X.set_source x.ctx (X.Pattern.create_rgb ~red:1.0 ~green:0.0 ~blue:0.0); *)
    X.translate x.ctx (-. 0.5 *. radius) (0.5 *. radius);
    Array.iteri
      begin fun i l ->
	X.translate x.ctx curve.(i).C.im curve.(i).C.re;	
	X.show_text x.ctx l;
	X.translate x.ctx (-. curve.(i).C.im) (-. curve.(i).C.re);
	X.stroke x.ctx;
      end
      labels;
    X.translate x.ctx (0.5 *. radius) (-. 0.5 *. radius)


let show_samples x ?(bottom_buffer=4.5) title ncols samples =
  Svg.show_title x title;
  Svg.draw_in_table x ncols (Array.length samples)
    begin fun x i ->
      Svg.draw_closed_curve_wt x samples.(i) 0.01;
    end;
  Cairo.translate x.Svg.ctx 0. bottom_buffer

let show_samples_midpoints x title ncols basecurve (samples,mpchoices) buffer =
  let radius=0.03 in
    if String.length title > 0 then
      Svg.show_title x title;
    Svg.draw_in_table x ncols (Array.length samples)
      begin fun x i ->

	let n = Array.length samples.(i) in
	  assert(n >= 1);
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
