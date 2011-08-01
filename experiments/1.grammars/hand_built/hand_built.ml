open Printf
open Util.Misc
open Util.Cops
module C = Complex
open Abstract
open Sdf
open Grammar

let dir = "experiments/1.grammars/hand_built/hand_built.d";;
let latexdir = "./1.grammars/hand_built/hand_built.d";;
let nsamples = 20;;

let _ = 
  (*********************)
  (* construct grammar *)
  (*********************)
  let curve = Curve.load "romer/ann/curve0000.curve" in
  let gram = Models.Simple.make_grammar 
    (curve,
     Sdf.load_family "romer/misc/romer1.sdf") in
  let gramfile = open_out "tmp/hand_built.gram" in

    Marshal.to_channel gramfile gram [];
    close_out gramfile;

    Curve.save "tmp/flipped.curve" (Curve.flip_xy curve);
    doit "./show_curve.native tmp/flipped.curve tmp/hand_built_curve.svg";

    doit_ok "mkdir -p tmp/hand_built.d";
    (*    doit (sprintf "./show_grammar.native -gramfile tmp/hand_built.gram -dir %s -title '' -rules" dir);  *)
    doit (sprintf "./show_grammar.native -gramfile tmp/hand_built.gram -dir %s -latexdir %s -title ''" dir latexdir);

    ()
