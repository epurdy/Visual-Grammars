open Printf
open Scanf
open Util.Misc

module Edges = Detection.Edges
module Local = Detection.Local_inference

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

let load_yield fname = 
  let f = open_in fname in
  let rv = ref [] in
  let get_stuff px py qx qy =
    rv := ((px,py),(qx,qy)) :: !rv;
  in
    begin try
      while true do 
	let s = input_line f in
	  sscanf s "(%d,%d)-(%d,%d)\n" get_stuff;
      done
    with End_of_file ->
      close_in f;
    end;
    !rv

let _ =
  let segs = load_yield "theyield.yield" in
  let segs = List.map (fun ((x1,y1), (x2,y2)) ->
			 ((4*x1,4*y1), (4*x2,4*y2))) segs
  in
 
  let im = Pnm.load_pgm "romer/clean/IMG0200.PGM" in
  let fim = Image.map foi im in
  let smooth = Filter.smooth Image.float_ops fim 3.0 in
  let fine_edges = Canny.canny smooth 50.0 100.0 in
  let dx, dy = Canny.gradient fim in
  let edgedata = {
    Edges.original_w = Image.width im;
    Edges.original_h = Image.height im;
    Edges.w = Image.width im;
    Edges.h = Image.height im;

    Edges.original = im;
    Edges.coarse = fim;
    Edges.edgemap = fine_edges;
    Edges.coarse_dx = dx;
    Edges.coarse_dy = dy;
  }
  in

  let _ = Edges.save_edge_data edgedata "local" in 
    
  let local = Local.infer_orientations_and_interior 
    local_params edgedata segs "local/z" in

    local
