open Util.Misc

module C = Complex
module X = Cairo

type t = {
  svg:Cairo_svg.surface;
  ctx:Cairo.t}

let create fname = 
  let svg = Cairo_svg.surface_create_for_channel (open_out fname) 400. 400. in
  let ctx = X.create svg in
    {svg=svg;
     ctx=ctx}

let create_sized fname (w,h) = 
  let svg = Cairo_svg.surface_create_for_channel (open_out fname) w h in
  let ctx = X.create svg in
    {svg=svg;
     ctx=ctx}

let finish x = 
  X.surface_flush x.svg;
  X.surface_finish x.svg;
  X.surface_flush x.svg

let make_the_curve x c =
  let n = Array.length c in
    X.move_to x.ctx c.(0).C.im c.(0).C.re;
    for i=1 to n-1 do
      X.line_to x.ctx c.(i).C.im c.(i).C.re;
    done

let draw_curve x c =
  X.set_line_width x.ctx 1.;
  make_the_curve x c;
  X.stroke x.ctx

let draw_curve_wt x c wt =
  X.set_line_width x.ctx wt;
  make_the_curve x c;
  X.stroke x.ctx

let draw_closed_curve x c =
  X.set_line_width x.ctx 1.;
  make_the_curve x c;
  X.close_path x.ctx;
  X.stroke x.ctx

let draw_closed_curve_wt x c wt =
  X.set_line_width x.ctx wt;
  make_the_curve x c;
  X.close_path x.ctx;
  X.stroke x.ctx

let show_title x title =
  X.translate x.ctx 0. 1.5;    
  X.select_font_face x.ctx "Georgia" X.FONT_SLANT_NORMAL X.FONT_WEIGHT_BOLD;
  X.set_font_size x.ctx 1.0;
  X.show_text x.ctx title;
  X.translate x.ctx 0. 0.5

let draw_circle x center radius =
  Cairo.set_line_width x.ctx 0.01;
  Cairo.arc x.ctx ~xc:center.C.im ~yc:center.C.re
    ~radius:radius ~angle1:0. ~angle2:(2. *. pi);
  Cairo.stroke x.ctx
  
let draw_circle_filled x center radius =
  Cairo.arc x.ctx ~xc:center.C.im ~yc:center.C.re 
    ~radius:radius ~angle1:0. ~angle2:(2. *. pi);
  Cairo.fill x.ctx


let draw_in_table x ncols nents (f : t -> int -> unit) =
  let col = ref 0 in
  let return () = 
    X.translate x.ctx (-. (float_of_int !col) *. 1.5) 1.5;
    col := 0;
  in
    for i=0 to (nents-1) do
      f x i;
      X.translate x.ctx 1.5 0.;

      incr col;
      
      if !col = ncols then return ()
    done;
    return ()
  
