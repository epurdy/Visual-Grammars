
let scale = 100. 


let _ = 
  (* ./show_curve.native in.curve out.svg *)
  let x = Svg.create_sized Sys.argv.(2) (scale, scale) in
  let curve = Curve.load Sys.argv.(1) in
  let curve = Curve.normalize curve in
  let _ = Cairo.scale x.Svg.ctx scale scale in
    Svg.draw_closed_curve_wt x curve 0.01;
    Svg.finish x

