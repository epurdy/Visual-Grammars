
open Annotation

let _ = 
  let annot = load_annotation Sys.argv.(1) in
  let curve = curve_of_annotation annot in
    Curve.save Sys.argv.(2) curve
