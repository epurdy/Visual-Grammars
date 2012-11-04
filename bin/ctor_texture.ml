open Util.Misc
open Printf

let _ = 
  let n = ref 0 in
  let max_lvl = ref 0 in
  let max_len_lvl = ref 0 in
  let out_gram_name = ref "" in
  let curves = ref [] in

  let add_curve fname = 
    let c = Curve.load fname in
      curves := c :: !curves;
  in

  let _ = Arg.parse [
    "-n", Arg.Set_int n, "Smallest scale is 1/n.";
    "-max_lvl", Arg.Set_int max_lvl, "log_2 n.";
    "-max_len_lvl", Arg.Set_int max_len_lvl, "Max length of curve relative to its level.";
    "-output", Arg.Set_string out_gram_name, "Output grammar.";]
    add_curve
    "./prog -example in.curve -sdf sdf.sdf -output out.gram"
  in

  let _ = 
    assert (!n > 0);
  in
    
  let gram = Xform.ctor_pipeline 
    Structure.Texture.texture_heuristic
    {Structure.Texture.n = !n;
     Structure.Texture.max_lvl = !max_lvl;
     Structure.Texture.max_len_lvl = !max_len_lvl;
     Structure.Texture.curves = Array.of_list (List.rev !curves);
    }
  in

  let outfile = open_out !out_gram_name in

    Marshal.to_channel outfile gram [];
    close_out outfile

