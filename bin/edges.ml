open Printf
open Util.Misc
module I = Image


let _ = 
  let im = Pnm.load_pgm Sys.argv.(1) in
  let sigma = float_of_string Sys.argv.(3) in
  let hi = float_of_string Sys.argv.(4) in
  let lo = float_of_string Sys.argv.(5) in
  let im = Image.map (float_of_int) im in
  let im = Filter.smooth Image.float_ops im sigma in
  let out = Canny.canny im hi lo in
  let out = Image.map (function true -> 0 | false -> 255) out in
    Pnm.save_pgm out Sys.argv.(2)
