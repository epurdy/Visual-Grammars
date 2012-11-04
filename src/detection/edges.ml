open Printf
open Util.Misc

module I = Image

type edge_params = {
  coarsefactor: int;
  sigma: float;
  coarse_sigma: float;
  hi: float;
  lo: float;
}

type edgedata = {
  original_w: int;
  original_h: int;
  w: int;
  h: int;

(*   coarsefactor: int; *)
(*   sigma: float; *)
(*   lo: float; *)
(*   hi: float; *)

  (* original_w x original_h *)
  original: int Image.t;

  (* w x h *)
  coarse: float Image.t;
  edgemap: bool Image.t; 
  coarse_dx: float Image.t;
  coarse_dy: float Image.t;
}

let coarsen_edge_map edgemap factor = 
  printf "starting coarsening\n%!";
  let w,h = I.width edgemap, I.height edgemap in
  let w2, h2 = w/factor, h/factor in
  let newmap = I.create w2 h2 false in
    for y=0 to (h2-1) * factor do
      for x=0 to (w2-1) * factor do
	if (I.get edgemap (x,y)) then begin
	  I.set newmap true (x/factor, y/factor)
	end;
      done
    done;

    printf "finished coarsening\n%!";
    newmap


let save_edge_data edata dir =
  let edgemap = Image.map (function true -> 0 | false -> 255) edata.edgemap in
  let dx = Image.map (fun x -> round (255. *. logistic (x))) edata.coarse_dx in
  let dy = Image.map (fun x -> round (255. *. logistic (x))) edata.coarse_dy in
    Pnm.save_pgm edata.original (sprintf "%s/original.pgm" dir);
    Pnm.save_pgm edgemap (sprintf "%s/edges.pgm" dir);
    Pnm.save_pgm dx (sprintf "%s/coarse_dx.pgm" dir);
    Pnm.save_pgm dy (sprintf "%s/coarse_dy.pgm" dir)

let subsample im gran = 
  let w, h = (I.width im)/gran, (I.height im)/gran in
  let sim = I.create w h (I.get im (0,0)) in
    for y=0 to h-1 do
      for x=0 to w-1 do
	I.set sim (I.get im (gran*x, gran*y)) (x,y);
      done
    done;
    sim

let remove_border edgemap =
  let w,h = Image.width edgemap, Image.height edgemap in
    Image.iterxy
      begin fun (x,y) b ->
	if x < 2 || y < 2 || x > w-3 || y > h-3 then 
	  I.set edgemap false (x,y)
      end
      edgemap

(* let normalize_gradient dx dy mag s = *)
(*   let ndx, ndy = Image.map (fun x -> 0.) dx, Image.map (fun x -> 0. dy) in *)
(*   let w,h = Image.width dx, Image.height dx in *)
(*     for y=0 to h-1 do *)
(*       for x=0 to w-1 do  *)
(* 	let tot, num = ref 0., ref 0. in *)
(* 	  for dy = -s to s do *)
(* 	    for dx = -s to s do *)
(* 	      let u,v = x+dx, y+dy in *)
(* 		if I.inside (u,v) mag then begin *)
(* 		  let gmag = I.get mag (u,v) in *)
(* 		    tot := !tot +. gmag; *)
(* 		    num := !num +. 1.; *)
(* 		end *)
(* 	    done *)
(* 	  done; *)

(* 	  I.set ndx ((I.get dx (x,y))/. !tot *. !num) (x,y); *)
(* 	  I.set ndy ((I.get dy (x,y))/. !tot *. !num) (x,y); *)
(*       done  *)
(*     done;... *)

let get_edge_data params im =
  let fim = Image.map float_of_int im in

  (* compute edges of fine image and then coarsen *)
  let smooth = Filter.smooth Image.float_ops fim params.sigma in
  let fine_edges = Canny.canny smooth params.lo params.hi in
  let coarse_edges = coarsen_edge_map fine_edges params.coarsefactor in
  let _ = remove_border coarse_edges in

  (* compute gradient of coarse image *)
  let smooth2 = Filter.smooth Image.float_ops fim params.coarse_sigma in
  let coarse_im = subsample smooth2 params.coarsefactor in
  let coarse_dx, coarse_dy = Canny.gradient coarse_im in

    { original_w = Image.width im;
      original_h = Image.height im;
      w = Image.width coarse_im;
      h = Image.height coarse_im;

(*       coarsefactor = coarsefactor; *)
(*       sigma = sigma; *)
(*       lo = lo; *)
(*       hi = hi; *)

      original = im;
      coarse = coarse_im;
      edgemap = coarse_edges;
      coarse_dx = coarse_dx;
      coarse_dy = coarse_dy;
    }
