open Printf
open Scanf
open Util.Hops
open Util.Cops
open Util.Misc

module C = Complex
module I = Image


let _ = 
  let im = Pnm.load_pgm "romer/clean/IMG0020.PGM" in
  let im = Image.map (float_of_int) im in
  let im = Filter.smooth Image.float_ops im (4.0 *. sigma) in
  let im = subsample im 4 in
  let dx,dy = Canny.gradient im in

  let edges = Canny.canny im lo hi in
  let edgeim = Image.map (function true -> 0 | false -> 255) edges in
  let _ = Pnm.save_pgm edgeim "orient_edges_true.pgm" in

  let edges = Pnm.load_pgm "edges.2.pgm" in
  let edges = Image.map (fun x -> (x=0)) edges in
  let edgeim = Image.map (function true -> 0 | false -> 255) edges in    
  let _ = Pnm.save_pgm edgeim "orient_edges_loaded.pgm" in


    
