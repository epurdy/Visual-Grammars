open Util.Misc
open Printf

let _ = 
  let example_name = ref "" in
  let out_gram_name = ref "" in

  let barf s = (failwith ("extra arg: " ^ s)) in

  let _ = Arg.parse [
    "-example", Arg.Set_string example_name, "Example curve.";
    "-output", Arg.Set_string out_gram_name, "Output grammar.";]
    barf
    "./prog -example in.curve -output out.gram"
  in
    
  let excurve = Curve.load !example_name in
  let gram = Xform.ctor_pipeline 
    Structure.Init.init_heuristic
    {Structure.Init.excurve = excurve;
     Structure.Init.family = Sdf.make_full_family (Array.length excurve);
     Structure.Init.name = sprintf "FULL: %s" !example_name;
    }
  in

  let outfile = open_out !out_gram_name in

    Marshal.to_channel outfile gram [];
    close_out outfile

