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

  (* let n0 = Array.length curves.(0) in *)
  (* let axis = curves.(0).(n0/2) -& curves.(0).(0) in *)
  (* let curves = Array.map *)
  (*   begin fun c -> *)
  (*     let nc = Array.length c in *)
  (*     let axis2 = c.(nc/2) -& c.(0) in *)
  (*     let factor = axis /& axis2 in *)
  (* 	Array.map (fun z -> z *& factor) c *)
  (*   end *)
  (*   curves  *)
  (* in *)

  let curves = Array.map Curve.normalize curves in
  let ncurves = Array.length curves in
    
  let x = Svg.create_sized !fname
    ((float_of_int ncols) *. 1.5 *. scale, 
     float_of_int (5 + (ncurves/ncols)) *. 4.0 *. scale) in
  let _ = Cairo.scale x.Svg.ctx scale scale in

    Viz.show_samples x !title ncols curves;
    Svg.finish x
