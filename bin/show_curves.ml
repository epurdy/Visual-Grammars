let ncols = 5
let scale = 100.

let _ = 
  let fname = ref "foo.svg" in
  let title = ref "" in
  let curves = ref [] in

  let add_curve fname = 
    let c = Curve.load fname in
      curves := c :: !curves;
  in

  let _ = Arg.parse ["-fname", Arg.Set_string fname, "File to write to.";
		     "-title", Arg.Set_string title, "Title to write above curves.";]
    add_curve
		     "./prog -fname fname curve1 curve2 curve3..."
  in

  let curves = Array.of_list (List.rev !curves) in 
  let curves = Array.map Curve.normalize curves in
  let ncurves = Array.length curves in
    
  let x = Svg.create_sized !fname
    ((float_of_int ncols) *. 1.5 *. scale, 
     float_of_int (5 + (ncurves/ncols)) *. 4.0 *. scale) in
  let _ = Cairo.scale x.Svg.ctx scale scale in

    Viz.show_samples x !title ncols curves;
    Svg.finish x
