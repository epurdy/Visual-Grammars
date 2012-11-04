open Util.Misc
open Util.Cops
open Printf
open Abstract
open Grammar

let _ =
  let gramfile = ref "" in
  let prefix = ref "" in

  let fnames = ref [] in
  let add_curve fname =
    fnames := fname :: !fnames;
  in

  let _ = Arg.parse ["-gramfile", Arg.Set_string gramfile, "File containing grammar.";]
    add_curve
    ("./prog -gramfile GRAMFILE curve1.curve curve2.curve ...")
  in

  let _ = 
    assert (!gramfile != "");
    assert (!prefix != "");
  in

  let fnames = Array.of_list (List.rev !fnames) in
  let nsamples = Array.length fnames in

  let gram = (Marshal.from_channel (open_in !gramfile): Grammar.grammar) in

  let p, q = c0, cxre 1000. in
  let samples = Array.init nsamples (fun i -> Grammar.sample gram p q) in
  let samples = Array.map (Curve.normalize ~scale:1000.) samples in

    for i = 0 to nsamples-1 do 
      Curve.save fnames.(i) samples.(i);
    done

