open Util.Misc
open Printf

let _ = 
  let in_gram_name = ref "" in
  let out_gram_name = ref "" in
  let ncopies = ref 0 in

  let barf s = (failwith ("extra arg: " ^ s)) in

  let _ = Arg.parse [
    "-input", Arg.Set_string in_gram_name, "Input grammar.";
    "-output", Arg.Set_string out_gram_name, "Output grammar.";
    "-ncopies", Arg.Set_int ncopies, "";]
    barf
    "./prog -input in.gram -output out.gram"
  in

  let _ =
    assert (!ncopies > 0);
  in
    
  let infile = open_in !in_gram_name in
  let outfile = open_out !out_gram_name in

  let gram = (Marshal.from_channel infile: Grammar.grammar) in
  let gram = Xform.xform_pipeline 
    Structure.Multi.multi_heuristic 
    gram 
    {Structure.Multi.ncopies = !ncopies;
     Structure.Multi.name = "MULTI";
    }
  in

    Marshal.to_channel outfile gram [];
    close_out outfile;
    close_in infile
