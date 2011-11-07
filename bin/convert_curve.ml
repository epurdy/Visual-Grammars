

let _ = 
  let fname = Sys.argv.(1) in
  let out_fname = Sys.argv.(2) in
  let curve = Curve.load_from_xml fname in
    Curve.save out_fname curve
