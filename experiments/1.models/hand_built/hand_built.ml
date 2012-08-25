open Printf
open Util.Misc
open Util.Cops
module C = Complex
open Abstract
open Sdf
open Grammar

let _ = 
  let dir = Sys.argv.(1) in

  let _ = 
    let curve = Curve.load "DATA/romer/ann/curve0000.curve" in
    let gram = Models.Simple.make_grammar 
      (curve,
       Sdf.load_family "DATA/romer/misc/romer1.sdf") in
    let gramfile = open_out (sprintf "%s/romer.gram" dir) in
      Marshal.to_channel gramfile gram [];
      close_out gramfile;
      Curve.save (sprintf "%s/romer_flipped.curve" dir) (Curve.flip_xy curve);
  in

  let _ = 
    let curve1 = Curve.load "DATA/romer/ann/curve0000.curve" in
    let curve2 = Curve.load "DATA/romer/misc/skirt.curve" in
    let gram1 = Models.Simple.make_grammar 
      (curve1,
       Sdf.load_family "DATA/romer/misc/romer1.sdf") in
    let gram2 = Models.Simple.make_grammar 
      (curve2,
       Sdf.load_family "DATA/romer/misc/romer_skirt.sdf") in
    let gram = Grammar.merge_tops [| gram1; gram2 |] in
    let gramfile = open_out (sprintf "%s/romerchoice.gram" dir) in
      Marshal.to_channel gramfile gram [];
      close_out gramfile;
      Curve.save (sprintf "%s/romer_skirt.curve" dir) (Curve.flip_xy curve2);
  in

  let _ = 
    let curve = Examples.Hand.curve in
    let gram = Examples.Hand.gram in
    let gramfile = open_out (sprintf "%s/hand.gram" dir) in
      Marshal.to_channel gramfile gram [];
      close_out gramfile;
      Curve.save (sprintf "%s/hand.curve" dir) (Curve.flip_xy curve);
  in

    ()
