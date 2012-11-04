open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph
open Filtration

module C = Complex
module I = Image

(* open Detection.Parsing2d *)
module Edges = Detection.Edges
module Gridparsing = Detection.Gridparsing
module Calc_bounds = Detection.Calc_bounds
module Local = Detection.Local_inference
module Global = Detection.Global_inference

let edge_params = {
  Edges.sigma = 3.0; (* 2.0? *)
  Edges.coarse_sigma = 4.0;
  Edges.hi = 200.;
  Edges.lo = 100.;
  Edges.coarsefactor = 4;
}

let pf = 0.80 (* probability of observing an edge pixel in the foreground *)
let pb = 0.10 (* probability of observing an edge pixel in the background *)

let global_params = {
  Global.maxiter = 5; (* 5 *)


  Global.max_interior_frac = 0.25;
  Global.max_exterior_frac = 0.25;
  (* Global.max_interior_frac = 0.5; *)
  (* Global.max_exterior_frac = 0.5; *)


  Global.min_on_frac = 0.5;
  Global.max_len_auto_edge = 5.;
  Global.backwards_factor = 2.;
  Global.edge_term = Global.edge_term_of_probs pf pb;
  Global.length_term = Global.length_term_of_probs pf pb;
}

let local_params = {
  Local.yield_wt = 1.0;
  Local.logistic_param = 1.0;
  Local.nbr_window = 4;
  Local.orth_dropoff_rate = 1.0;  

  Local.orientation_maxiter = 3000;
  Local.orientation_tempinc = 0.001;
(*   Local.orientation_maxiter = 300; *)
(*   Local.orientation_tempinc = 0.01; *)

  Local.curl_window = 5;
  Local.curl_sigma = 1.0;
  Local.interior_maxiter = 3000;
  Local.interior_tempinc = 0.001;
  Local.vote_wt = 10.0;
}

let reparse gram nlvls net filt edgedata maxreiter dir =
  let bestparse, bestqual = ref None, ref infinity in
  let local = ref (Local.make_fake_local ()) in
    for reiter = 1 to maxreiter do
      printf "reparsing iteration #%d\n%!" reiter;
      let filt = copy_filtration filt in
      let bestfine = Global.do_global_inference global_params
	gram net filt !local edgedata (sprintf "%s/global.x%d" dir reiter) in
      let truecost = Global.true_cost global_params gram bestfine edgedata filt.points in
      let theyield = List.map
	(fun (pid,qid) -> (filt.points.(pid), filt.points.(qid)))
	bestfine.Global.yield
      in
	local := Local.infer_orientations_and_interior 
	  local_params edgedata theyield (sprintf "%s/local.x%d" dir reiter);

	(* if truecost < !bestqual then begin *)
	if true then begin
	  bestparse := Some bestfine;
	  bestqual := truecost;
	end;

    done;
    get !bestparse

let draw_it im ptree points =
  let im = Image.map (fun x -> (x,x,x)) im in
  let curve = List.map (fun (pid,qid) -> points.(pid)) ptree.Global.yield in
  let curve = List.map
    (fun (x,y) -> (x * edge_params.Edges.coarsefactor,
		   y * edge_params.Edges.coarsefactor)) curve 
  in
  let curve = Array.of_list curve in
    (* Global.draw_curve im (Array.of_list curve) true (255,255,255); *)


  let n = Array.length curve in
  for i = 0 to n-1 do
    let color = (Random.int 255, Random.int 255, Random.int 255) in
      Draw.draw_line im curve.(i) curve.((i+1) mod n) color;
      Draw.draw_line im (0,i*5) (0,(i+1)*5) color;
  done;

    im

let _ = 

  let dir = Sys.argv.(1) in
  let _ = doit (sprintf "mkdir -p %s" dir) in

  let im = Pnm.load_pgm Sys.argv.(2) in
  let edgedata = Edges.get_edge_data edge_params im in
  let _ = Edges.save_edge_data edgedata dir in

  let excurve = Curve.load Sys.argv.(3) in
  let sdf = Sdf.load_family Sys.argv.(4) in

  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = merge_leaves gram in
  let _ = reorder_symbols gram in

  let w,h = Image.width im, Image.height im in
  let cfactor = edge_params.Edges.coarsefactor in
  let w,h = w / cfactor, h / cfactor in
  let net = Gridparsing.make_short_edge_graph (w,h) 15 in

  let nlvls = 4 in
  (* let nlvls = 6 in *)
  let filtration = Detection.Gridparsing.make_plane_filtration net.vertices nlvls in
    
  let bestparse = reparse gram nlvls net filtration edgedata global_params.Global.maxiter dir in

  let im = draw_it im bestparse net.vertices in
    Pnm.save_ppm im (sprintf "%s/thefinalparse.ppm" dir);

(*   let im = Global.draw_parse bestparse net.vertices edgedata.Edges.edgemap in *)
(*     Pnm.save_ppm im (sprintf "%s/thefinalparse.ppm" dir); *)
(*     Global.print_parse_tree bestparse; *)
    printf "finished!\n%!"
