

let _ = 
  (* ./subsample.native in.curve npts out.curve *)
  let curve = Curve.load Sys.argv.(1) in
  let n = int_of_string Sys.argv.(2) in

  let curve2 = Subsampling.subsample_dp curve n 2 in
    Curve.save Sys.argv.(3) curve2
