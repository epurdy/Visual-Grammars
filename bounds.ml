open Util.Misc
module C = Complex

type bounds = {
  xmin : float;
  xmax : float;
  ymin : float;
  ymax : float;
}

exception Out_of_bounds of bounds * C.t

let unitsquare = {
  xmin = 0.0;
  xmax = 1.0;
  ymin = 0.0;
  ymax = 1.0;
}

(** A sensible default : [ \[-6.0, 6.0\] x \[-6.0, 6.0\] ]  *)
(* let defaultbounds = { *)
(*   xmin = -.4.0; *)
(*   xmax =   4.0; *)
(*   ymin = -.4.0; *)
(*   ymax =   4.0; *)
(* } *)
let defaultbounds = {
  xmin = -.6.0;
  xmax =   6.0;
  ymin = -.6.0;
  ymax =   6.0;
}


let move_into_bounds bounds pt =
  let x,y = pt.C.re, pt.C.im in
  let x = min (max x bounds.xmin) bounds.xmax in
  let y = min (max y bounds.ymin) bounds.ymax in
    {C.re=x; C.im=y}

let map_to_unit_square_strict bounds pt =
  let x,y = pt.C.re, pt.C.im in
  let interp abs low high =
    if abs < low then raise (Out_of_bounds (bounds, pt));
    if abs > high then raise (Out_of_bounds (bounds, pt));
    (abs -. low) /. (high -. low)
  in
  let xrel = interp x bounds.xmin bounds.xmax in
  let yrel = interp y bounds.ymin bounds.ymax in
    {C.re = xrel; C.im = yrel}

let map_to_unit_square bounds pt =
  try
    Some (map_to_unit_square_strict bounds pt)
  with Out_of_bounds _ ->
    None

let map_to_unit_square_nice bounds pt =
  map_to_unit_square_strict bounds (move_into_bounds bounds pt)

let map_from_unit_square bounds pt =
  let x,y = pt.C.re, pt.C.im in
  let interp rel low high =
    if rel < 0. then raise (Out_of_bounds (unitsquare, pt));
    if rel > 1. then raise (Out_of_bounds (unitsquare, pt));
    (low *. (1. -. rel)) +. (high *. rel)
  in
  let xabs = interp x bounds.xmin bounds.xmax in
  let yabs = interp y bounds.ymin bounds.ymax in
    {C.re = xabs; C.im = yabs}

let unit_square_to_array granularity pt =
  let fgranularity = float_of_int granularity in
  let fn t = round (fgranularity *. t) in
  let x, y = fn pt.C.re, fn pt.C.im in
    (x, y)

let bounds_to_array_strict bounds granularity pt =
  let relpt = map_to_unit_square_strict bounds pt in
    unit_square_to_array granularity relpt

let bounds_to_array_nice bounds granularity pt =
  let relpt = map_to_unit_square_nice bounds pt in
    unit_square_to_array granularity relpt

let array_to_bounds bounds granularity (ix,iy) =
  let fgranularity = float_of_int granularity in
  let ux = (float_of_int ix) /. fgranularity in
  let uy = (float_of_int iy) /. fgranularity in
    map_from_unit_square bounds {C.re=ux; C.im=uy}


let union bds1 bds2 = 
  { xmin = min bds1.xmin bds2.xmin;
    xmax = max bds1.xmax bds2.xmax;
    ymin = min bds1.ymin bds2.ymin;
    ymax = max bds1.ymax bds2.ymax;
  }

let bounds_of_pt z = 
  let x,y = z.C.re, z.C.im in
  { xmin = x;
    xmax = x;
    ymin = y;
    ymax = y;
  }

let null_bounds = 
  {xmin = infinity;
   xmax = neg_infinity;
   ymin = infinity;
   ymax = neg_infinity}

let squarify bds =
  let dx, dy = bds.xmax -. bds.xmin, bds.ymax -. bds.ymin in
    if dx < dy then
      let diff = dy -. dx in
	{ xmin = bds.xmin -. 0.5 *. diff;
	  xmax = bds.xmax +. 0.5 *. diff;
	  ymin = bds.ymin;
	  ymax = bds.ymax;
	}
    else 
      let diff = dx -. dy in
	{ xmin = bds.xmin;
	  xmax = bds.xmax;
	  ymin = bds.ymin -. 0.5 *. diff;
	  ymax = bds.ymax +. 0.5 *. diff;
	}

let marginify bds amt =
  let dx, dy = bds.xmax -. bds.xmin, bds.ymax -. bds.ymin in
  let mx, my = dx *. amt *. 0.5, dy *. amt *. 0.5 in
    { xmin = bds.xmin -. mx;
      xmax = bds.xmax +. mx;
      ymin = bds.ymin -. my;
      ymax = bds.ymax +. my;
    }

let nice_curve_bounds curve = 
  let bds =
    Array.fold_left
      begin fun bds z ->
	union bds (bounds_of_pt z)
      end
      null_bounds
      curve 
  in
  let bds = squarify bds in
  let bds = marginify bds 0.1 in
    bds

let nice_multicurve_bounds curves = 
  let bigcurve = Array.concat (Array.to_list curves) in
    nice_curve_bounds bigcurve
