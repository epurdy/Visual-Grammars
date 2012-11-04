open Printf
open Geometry
open Util.Misc
open Util.Hops
open Util.Cops

module C = Complex
module I = Image

type local_interpretation = {
  fake: bool;
  edges_: bool Image.t;
  inside: bool Image.t;
  orientations: (int*int, C.t) hash;
  rgrad: C.t Image.t;
}

type local_params = {
  yield_wt: float;
  logistic_param: float;
  nbr_window: int;
  orth_dropoff_rate: float;  
  orientation_maxiter: int;
  orientation_tempinc: float;
  curl_window: int;
  curl_sigma: float;
  interior_maxiter: int;
  interior_tempinc: float;
  vote_wt: float;
}

let make_fake_local () = {
  fake = true;
  edges_ = Image.create 1 1 true;
  inside = Image.create 1 1 true;
  orientations = mkhash 1;
  rgrad = Image.create 1 1 c0;
}

let get_edge_pixels edgedata =
  let edges = edgedata.Edges.edgemap in 
  let local = {
    fake = false;
    edges_ = edges;
    inside = Image.map (fun x -> false) edges;
    orientations = mkhash 100;
    rgrad = Image.map (fun x -> c0) edges;
  } in
    for y=2 to edgedata.Edges.h-3 do
      for x=2 to edgedata.Edges.w-3 do 
	if I.get edges (x,y) then begin 
	  local.orientations << ((x,y), c0);
	end
      done 
    done;
    local

let get_rotated_gradient edgedata local =
  for y=0 to edgedata.Edges.h-1 do 
    for x=0 to edgedata.Edges.w-1 do 
      let gx,gy = I.get edgedata.Edges.coarse_dx (x,y), 
	I.get edgedata.Edges.coarse_dy (x,y) in
	I.set local.rgrad {C.re = gy; C.im = -.gx} (x,y);
    done
  done


let round_angle v =
  let a = C.arg v in
  let a = (a +. pi) in
  let a = 8. *. a /. (2. *. pi) in
  let a = (round a) mod 8 in
    a

let save_orientations im epixels = 
  let im = Image.map (fun x -> 255) im in
    Hashtbl.iter 
      begin fun (px,py) v ->
	let a = C.arg v in
	let a = (a +. pi) in
	let a = 8. *. a /. (2. *. pi) in
	let a = (round a) mod 8 in
	  I.set im a (px,py);
      end
      epixels;
    im


let draw_orientations local = 
  let diffs = [| (1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1) |] in
  let sqsize = 3 in
  let colors = [| (255,0,0); (255,0,255); (0,0,255); (0,255,255);
		  (0,255,0); (128,64,0); (255,255,0); (255,128,0) |]
  in
  let im = Image.map (fun x -> (255,255,255)) local.edges_ in
    Hashtbl.iter 
      begin fun (px,py) v ->
	let a = C.arg v in
	let a = (a +. pi) in
	let a = 8. *. a /. (2. *. pi) in
	let a = (round a) mod 8 in
	  I.set im colors.(a) (px,py);
      end
      local.orientations;

    for a = 0 to 7 do 
      for y=0 to sqsize-1 do 
	for x=0 to sqsize-1 do 
	  let dx,dy = diffs.(a) in
	  let px,py = (dx+1)*sqsize+x, (dy+1)*sqsize+y in
	    I.set im colors.(a) (px,py);
	done
      done
    done;
    im


let draw_orientations_grey local = 
  let diffs = [| (1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1) |] in
  let sqsize = 3 in

  let ims = Array.init 4 
    begin fun _ -> 
      Image.map (function true -> 80 | false -> 160) local.edges_ 
    end
  in

    for i = 0 to 3 do 
      Hashtbl.iter 
	begin fun (px,py) v ->
	  let a = C.arg v in
	  let a = (a +. pi) in
	  let a = 8. *. a /. (2. *. pi) in
	  let a = (round a) mod 8 in
	    if a = i then begin
	      I.set ims.(i) 255 (px,py);
	    end;
	    if a = (i+4) then begin
	      I.set ims.(i) 0 (px,py);
	    end;
	end
	local.orientations;

      for a = 0 to 7 do 
	for y=0 to sqsize-1 do 
	  for x=0 to sqsize-1 do 
	    let dx,dy = diffs.(a) in
	    let px,py = (dx+1)*sqsize+x, (dy+1)*sqsize+y in
	      if a = i then begin
		I.set ims.(i) 255 (px,py);
	      end
	      else begin
		if a = (i+4) then begin
		  I.set ims.(i) 0 (px,py);
		end
		else begin 
		  I.set ims.(i) 80 (px,py);
		end
	      end;
	  done
	done
      done;


    done;

    ims


let draw_interior local =
  let interiorim = Image.mapxy
    begin fun (x,y) b ->
      if local.orientations >>? (x,y) then
	(0,0,255)
      else begin
	if b then
	  (0,255,0)
	else
	  (255,0,0)
      end
    end
    local.inside
  in
    interiorim

let get_yield_directions yield im = 
  let yielddir = I.map (fun x -> c0) im in
    List.iter
      begin fun (ip,iq) ->
	let p,q = complex_of_point ip, complex_of_point iq in
	let v = q -& p in
	let v = scale v (1. /. (C.norm v)) in
	  List.iter
	    begin fun ir ->
	      I.set yielddir ((I.get yielddir ir) +& v) ir;
	    end
	    (Draw.line ip iq);
      end
      yield;
    yielddir

let get_neighbor_weighting params local =
  let nbr_wts = mkhash 1000 in
  let window = params.nbr_window in
    Hashtbl.iter
      begin fun (px,py) v ->
	for y=py-window to py+window do
	  for x=px-window to px+window do
	    if local.orientations >>? (x,y) then begin 
	      let diff = complex_of_point (x-px, y-py) in
	      let rgrad = I.get local.rgrad (x,y) in
	      let disp_par = (dot diff rgrad) /. (foi (2*window)) in
	      let disp_orth = (cross diff rgrad) in
	      let wt = (exp (-. disp_par *. disp_par)) *.
		(1. -. params.orth_dropoff_rate *. (abs_float disp_orth))
	      in
		nbr_wts <<+ ((px,py), ((x,y),wt));
	    end
	  done
	done
      end
      local.orientations;
    nbr_wts
      
let infer_orientations params local yielddir nbr_wts =
  let yield_wt = cxre params.yield_wt in
  for iter = 1 to params.orientation_maxiter do 
    let temp = (foi iter) *. params.orientation_tempinc in

      Hashtbl.iter 
	begin fun p _ ->
	  (* get yield vote *)
	  let v = ref (yield_wt *& (I.get yielddir p)) in
	    
	    (* get nbr votes *)
	    List.iter
	      begin fun (q,wt) ->
		v := !v +& (scale (local.orientations >> q) wt);
	      end
	      (nbr_wts >>> p);

	    (* Having tabulated the votes to get v, we now pick +/-
	       rotated gradient, biased towards the one which points
	       the same way as v. *)
	    let rgrad = I.get local.rgrad p in
	    let dotprod = dot rgrad !v in
	    let prob = logistic (dotprod *. params.logistic_param *. temp) in
	    let pick = if Random.float 1.0 < prob then rgrad else c0 -& rgrad in
	      local.orientations << (p, pick)

	end
	local.orientations;
  done

let get_curl params local =
  let curl_window = params.curl_window in
  let wting = mkhash 100 in

    (* get weighting for a window around each point *)
    for dy = -curl_window to curl_window do 
      for dx = -curl_window to curl_window do 
	let norm2 = foi (dx*dx + dy*dy) in
	let gwting = exp (-. params.curl_sigma *. norm2) in
	let norm = sqrt norm2 in
	  wting << ((dx,dy), gwting /. norm);
      done
    done;

    let curl = Image.mapxy
      begin fun (x,y) _ ->
	let tot = ref 0. in
	  for dy = -curl_window to curl_window do 
	    for dx = -curl_window to curl_window do 
	      if (dx,dy) <> (0,0) then begin
		let q = (x+dx, y+dy) in
		let diff = complex_of_point (dx,dy) in
		  if local.orientations >>? q then begin 
		    let vec = local.orientations >> q in
		    let curl_vote = cross vec diff in
		    let curl_vote = curl_vote *. (wting >> (dx,dy)) in
		      assert(classify_float curl_vote <> FP_nan);
		      tot := !tot +. curl_vote;
		  end
	      end
	    done
	  done;
	  !tot
      end
      local.edges_
    in
      
      curl

let infer_interior params local curl =
  let w,h = Image.width local.inside, Image.height local.inside in
    (* Use curl to guess the interior, clean up with Ising model. *)
    Image.iterxy
      begin fun p crl ->
	I.set local.inside (Random.float 1.0 < logistic crl) p
      end
      curl;

    for iter = 1 to params.interior_maxiter do 
      if (iter mod 100) = 0 then printf "iter=%d\n%!" iter;

      let temp = params.interior_tempinc *. (foi iter) in

	Image.iterxy
	  begin fun (x,y) _ ->
	    (* set a border of width 2 to be outside *)
	    if x < 2 || y < 2 || x > w-3 || y > h-3 then begin 
	      I.set local.inside false (x,y)
	    end
	    else begin
	      if not (local.orientations >>? (x,y)) then begin 
		(* curl should be positive inside and negative outside
		   (at least near the curve) *)
		let votes = ref (I.get curl (x,y)) in

		  (* should agree with neighbors *)
		  List.iter
		    begin fun (x2,y2) ->
		      if I.inside local.inside (x2,y2) && 
			not (local.orientations >>? (x2,y2)) then begin 

			  if I.get local.inside (x2,y2) then
			    votes := !votes +. params.vote_wt
			  else
			    votes := !votes -. params.vote_wt;
			end
		    end
		    [ (x-1,y); (x+1,y); (x,y-1); (x,y+1) ];

		  let prob = logistic (!votes *. temp) in
		    if Random.float 1.0 < prob then
		      I.set local.inside true (x,y)
		    else
		      I.set local.inside false (x,y);
	      end
	    end
	  end
	  local.inside;
    done      

let infer_orientations_and_interior params edgedata segments name =
  let local = get_edge_pixels edgedata in
  let _ = get_rotated_gradient edgedata local in
  let yielddir = get_yield_directions segments edgedata.Edges.edgemap in
  let nbr_wts = get_neighbor_weighting params local in

  let _ = printf "inferring orientations\n%!" in
  let _ = infer_orientations params local yielddir nbr_wts in

  let _ = printf "computing curl\n%!" in
  let curl = get_curl params local in
  let _ = printf "inferring interior\n%!" in
  let _ = infer_interior params local curl in

  let _ = printf "drawing pictures\n%!" in

  (* let orim = draw_orientations local in *)
  let orims = draw_orientations_grey local in
  let intim = draw_interior local in
  let curlim = Image.map (fun x -> round (255. *. (logistic x))) curl in
    Array.iteri (fun i im -> Pnm.save_pgm im (sprintf "%s.orientations.%d.pgm" name i)) orims;
    (* Pnm.save_ppm orim (sprintf "%s.orientations.ppm" name); *)
    Pnm.save_pgm curlim (sprintf "%s.curl.pgm" name);
    Pnm.save_ppm intim (sprintf "%s.interior.ppm" name);

    printf "done with local inference!\n%!";

    local
