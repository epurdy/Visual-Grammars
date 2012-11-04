open Util.Misc
open Printf

let _ = 
  let in_gram_name = ref "" in
  let out_gram_name = ref "" in
  let ncopies = ref 0 in
  let nrulecopies = ref 0 in

  let barf s = (failwith ("extra arg: " ^ s)) in

  let _ = Arg.parse [
    "-input", Arg.Set_string in_gram_name, "Input grammar.";
    "-output", Arg.Set_string out_gram_name, "Output grammar.";
    "-ncopies", Arg.Set_int ncopies, "";
    "-nrulecopies", Arg.Set_int nrulecopies, "";]
    barf
    "./prog -input in.gram -output out.gram"
  in

  let _ =
    assert (!ncopies > 0);
    assert (!nrulecopies > 0);
  in
    
  let infile = open_in !in_gram_name in
  let outfile = open_out !out_gram_name in

  let gram = (Marshal.from_channel infile: Grammar.grammar) in
  let gram = Xform.xform_pipeline 
    Structure.Correlated.correlated_repetition_heuristic 
    gram 
    {Structure.Correlated.ncopies = !ncopies;
     Structure.Correlated.nrulecopies = !nrulecopies;
     Structure.Correlated.name = "CORRELATED REPETITION";
    }
  in

    Marshal.to_channel outfile gram [];
    close_out outfile;
    close_in infile
