open Util.Misc
open Printf

let _ = 
  let example_name = ref "" in
  let sdf_name = ref "" in
  let out_gram_name = ref "" in

  let barf s = (failwith ("extra arg: " ^ s)) in

  let _ = Arg.parse [
    "-example", Arg.Set_string example_name, "Example curve.";
    "-sdf", Arg.Set_string sdf_name, "Sdf name.";
    "-output", Arg.Set_string out_gram_name, "Output grammar.";]
    barf
    "./prog -example in.curve -sdf sdf.sdf -output out.gram"
  in
    
  let gram = Xform.ctor_pipeline 
    Structure.Init.init_heuristic
    {Structure.Init.excurve = Curve.load !example_name;
     Structure.Init.family = Sdf.load_family !sdf_name;
     Structure.Init.name = sprintf "INIT: %s %s" !example_name !sdf_name;
    }
  in

  let outfile = open_out !out_gram_name in

    Marshal.to_channel outfile gram [];
    close_out outfile

