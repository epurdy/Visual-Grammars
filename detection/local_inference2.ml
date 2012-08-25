open Scanf
open Printf
open Geometry
open Util.Misc
open Util.Hops
open Util.Cops
open Sample

module C = Complex
module I = Image

let seedz = 1000

type local_interpretation = {
  fake: bool;

  w: int;
  h: int;

  grad: C.t Image.t;
  rgrad: C.t Image.t;
  edges_: bool Image.t;

  orientations: (int*int, C.t) hash;
  zindex: float Image.t;
(*   zindex: int Image.t; *)
  phi: float Image.t;

}

type local_params = {
  hh_curl_wt: float;
  hh_div_wt: float;
  hh_orth_sigma: float;
  hh_par_sigma: float;

  yield_wt: float;
  logistic_param: float;
  nbr_window: int;
  orth_dropoff_rate: float;  
  orientation_maxiter: int;
  orientation_tempinc: float;
  curl_window: int;
  curl_sigma: float;

  zindex_maxiter: int;
  zindex_tempinc: float;
  sigma_z: float;
  nbr_wt: float;

  div_thresh: float;
}

let make_fake_local () = {
  fake = true;
  w=1;
  h=1;
  edges_ = Image.create 1 1 true;
  zindex = Image.create 1 1 0.;
(*   zindex = Image.create 1 1 0; *)
  orientations = mkhash 1;
  rgrad = Image.create 1 1 c0;
  grad = Image.create 1 1 c0;
  phi = Image.create 1 1 0.;
}

let make_new_local edgedata =
  let edges = edgedata.Edges.edgemap in 
  let local = {
    fake = false;
    w = edgedata.Edges.w;
    h = edgedata.Edges.h;

    edges_ = edges;
    grad = Image.map (fun x -> c0) edges;
    rgrad = Image.map (fun x -> c0) edges;

    orientations = mkhash 100;
    zindex = Image.map (fun x -> 0.) edges;
(*     zindex = Image.map (fun x -> 0) edges; *)
    phi = Image.map (fun x -> 0.) edges;
  } in
  let cx, cy = local.w/2, local.h/2 in

    for y=0 to edgedata.Edges.h-1 do
      for x=0 to edgedata.Edges.w-1 do 
	if (x < 2) || (y < 2) || (x > edgedata.Edges.w-3) ||
	  (y > edgedata.Edges.h-3) then begin 
	    I.set edges false (x,y);
	end
      done 
    done;

    for y=2 to edgedata.Edges.h-3 do
      for x=2 to edgedata.Edges.w-3 do 
	if I.get edges (x,y) then begin 
	  local.orientations << ((x,y), c0);
	end
      done 
    done;

    for y=0 to edgedata.Edges.h-1 do 
      for x=0 to edgedata.Edges.w-1 do 
	let gx,gy = I.get edgedata.Edges.coarse_dx (x,y), 
	  I.get edgedata.Edges.coarse_dy (x,y) in
	  I.set local.grad {C.re = gx; C.im = gy} (x,y);
	  I.set local.rgrad {C.re = gy; C.im = -.gx} (x,y);
      done
    done;
    
    (* a somewhat reasonable guess for the z indices *)
    for y=0 to local.h-1 do 
      for x=0 to local.w-1 do 
(* 	let vx,vy = cx-x, cy-y in *)
(* 	let vl = sqrt (foi ((cx * cx + cy * cy) - (vx * vx + vy * vy))) in *)
(* 	let mvl = sqrt (foi (cx * cx + cy * cy)) in *)
(* 	let thing = vl /. mvl in *)
(* 	let ithing = logistic (3. *. (thing -. 0.5)) in *)
(* 	  if Random.float 1.0 < ithing then *)
(* 	    I.set local.zindex 1 (x,y) *)
(* 	  else *)
(* 	    I.set local.zindex (-1) (x,y) *)

	if I.get local.edges_ (x,y) then
	  I.set local.zindex 1. (x,y)
	else
	  I.set local.zindex 0. (x,y);
      done
    done;

    local

(** drawing, saving, etc. *)

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

let draw_orientations local vecfield = 
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
      vecfield;
(*       local.orientations; *)

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

(* let draw_interior local = *)
(*   let interiorim = Image.mapxy *)
(*     begin fun (x,y) b -> *)
(*       if local.orientations >>? (x,y) then *)
(* 	(0,0,255) *)
(*       else begin *)
(* 	if b then *)
(* 	  (0,255,0) *)
(* 	else *)
(* 	  (255,0,0) *)
(*       end *)
(*     end *)
(*     local.inside *)
(*   in *)
(*     interiorim *)

(** inference *)

let get_hh_votes params local =
  let hh_votes = mkhash 1000 in
    Hashtbl.iter
      begin fun (x,y) _ ->
	let curl = {
	  C.re = (I.get local.zindex (x,y+1)) -. (I.get local.zindex (x,y-1));
	  C.im = (I.get local.zindex (x-1,y)) -. (I.get local.zindex (x+1,y));
	}
	in
(* 	let curl = { *)
(* 	  C.re = foi ((I.get local.zindex (x,y+1)) - (I.get local.zindex (x,y-1))); *)
(* 	  C.im = foi ((I.get local.zindex (x-1,y)) - (I.get local.zindex (x+1,y))); *)
(* 	} *)
(* 	in *)

	let gradphi = {
	  C.re = (I.get local.phi (x+1,y) -. I.get local.phi (x-1,y));
	  C.im = (I.get local.phi (x,y+1) -. I.get local.phi (x,y-1));
	}
	in

	let vote = (scale curl params.hh_curl_wt) +& 
	  (scale gradphi params.hh_div_wt) 
	in
	  
	  hh_votes << ((x,y), vote);
      end
      local.orientations;

    hh_votes

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

let get_yield_directions_coarsen yield im = 
  printf "(%d,%d)\n%!" (I.width im) (I.height im);
  let cyielddir = I.create (((I.width im)/4)+1) (((I.height im)/4)+1) c0 in
    List.iter
      begin fun (ip,iq) ->
	let p,q = complex_of_point ip, complex_of_point iq in
	let v = q -& p in
	let v = scale v (1. /. (C.norm v)) in
	  List.iter
	    begin fun ir ->
	      I.set cyielddir ((I.get cyielddir ir) +& v) ir;
	    end
	    (Draw.line ip iq);
      end
      yield;

    let yielddir =
      I.mapxy
	begin fun (x,y) _ ->
	  I.get cyielddir (x/4,y/4)
	end
	im
    in

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

let infer_orientations params local yielddir nbr_wts hh_votes =
  for iter = 1 to params.orientation_maxiter do
(*     printf "infer_orientations %d\n%!" iter; *)
    let itemp = (foi iter) *. params.orientation_tempinc in
    let temp = 1.0 /. itemp in

      Hashtbl.iter
	begin fun p _ ->
	  let v = ref (hh_votes >> p) in
	  let grad = I.get local.grad p in
	  let rgrad = I.get local.rgrad p in
	  let sigma_orth = temp *. params.hh_orth_sigma in
	  let sigma_par = temp *. params.hh_par_sigma in

	    v := !v +& (scale (I.get yielddir p) params.yield_wt);

	    List.iter
	      begin fun (q,wt) ->
		v := !v +& (scale (local.orientations >> q) wt);
	      end
	      (nbr_wts >>> p);
	    
(* 	      v := !v +& (scale rgrad (gaussian sigma_par)); *)

(* 	      let dotprod = dot !v rgrad in *)
(* 	      let pick = if dotprod > 0. then rgrad else c0 -& rgrad in *)

	      let dotprod = dot !v rgrad in
	      let x = (params.logistic_param *. itemp *. dotprod) in
	      let pick = if Random.float 1. < (logistic x)
	      then rgrad else c0 -& rgrad in
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

let infer_zindex params local curl smim =
  let w,h = Image.width local.zindex, Image.height local.zindex in

    for iter = 1 to params.zindex_maxiter do 
      if (iter mod 100) = 0 then printf "iter=%d\n%!" iter;
      let itemp = params.zindex_tempinc *. (foi iter) in
      let temp = 1. /. itemp in

	Image.iterxy
	  begin fun (x,y) _ ->
	    (* set a border of width 2 to be outside *)
	    if x < 2 || y < 2 || x > w-3 || y > h-3 then begin 
	      I.set local.zindex (-.1.) (x,y);
	    end
	    else begin
	      (* curl should be positive inside and negative outside
		 (at least near the curve) *)
	      let z = ref 0. in
	      let totwt = ref 0.0000001 in

		(* should agree with neighbors, unless neighbor is
		   edge *)
		List.iter
		  begin fun (x2,y2) ->
		    let wt = exp 
		      (-. 0.1 *. 
			 (abs_float
			    ((I.get smim (x,y)) -. (I.get smim (x2,y2)))))
		    in
		      totwt := !totwt +. wt;
		      z := !z +.
			wt *. (I.get local.zindex (x2,y2));
(* 		    if not (I.get local.edges_ (x2,y2)) then  *)
(* 		      z := !z +.  *)
(* 			params.nbr_wt *. (foi (I.get local.zindex (x2,y2))); *)
		  end
		  [ (x-1,y); (x+1,y); (x,y-1); (x,y+1) ];

		z := !z /. !totwt;
		

		z := !z +. (I.get curl (x,y));

		z := !z +. gaussian (params.sigma_z *. temp);
		I.set local.zindex !z (x,y);

(* 		if not (I.get local.edges_ (x,y)) then begin *)
(* 		  if Random.float 1.0 < logistic (itemp *. !z) then *)
(* 		    I.set local.zindex 1 (x,y) *)
(* 		  else *)
(* 		    I.set local.zindex (-1) (x,y); *)
(* 		end *)
	    end
	  end
	  local.zindex;
    done      

let infer_phi params local =
  for y=2 to local.h-3 do
    for x=2 to local.w-3 do
      let div = ref 0. in

	if local.orientations >>? (x,y+1) then
	  div := !div +. (dot (local.orientations >> (x,y+1)) {C.re=0.; C.im=1.});

	if local.orientations >>? (x,y-1) then
	  div := !div +. (dot (local.orientations >> (x,y-1)) {C.re=0.; C.im= -.1.});

	if local.orientations >>? (x+1,y) then
	  div := !div +. (dot (local.orientations >> (x+1,y)) {C.re=1.; C.im=0.});

	if local.orientations >>? (x-1,y) then
	  div := !div +. (dot (local.orientations >> (x-1,y)) {C.re= -.1.; C.im=0.});

	if local.orientations >>? (x+1,y+1) then
	  div := !div +. (dot (local.orientations >> (x+1,y+1)) {C.re=0.707; C.im=0.707});

	if local.orientations >>? (x+1,y-1) then
	  div := !div +. (dot (local.orientations >> (x+1,y-1)) {C.re=0.707; C.im= -.0.707});

	if local.orientations >>? (x-1,y+1) then
	  div := !div +. (dot (local.orientations >> (x-1,y+1)) {C.re= -.0.707; C.im=0.707});

	if local.orientations >>? (x-1,y-1) then
	  div := !div +. (dot (local.orientations >> (x-1,y-1)) {C.re= -.0.707; C.im= -.0.707});

	if (abs_float !div) >= params.div_thresh then
	  I.set local.phi !div (x,y)
	else
	  I.set local.phi 0. (x,y);
    done
  done

let infer_everything params edgedata smim segments name =
  let local = make_new_local edgedata in
  let yielddir = get_yield_directions_coarsen segments edgedata.Edges.edgemap in
  let nbr_wts = get_neighbor_weighting params local in

    for foo = 1 to 10 do
      let _ = printf "inferring orientations (hh_votes)\n%!" in
      let hh_votes = get_hh_votes params local in
      let _ = printf "inferring orientations\n%!" in
      let _ = infer_orientations params local yielddir nbr_wts hh_votes in

      let _ = printf "inferring zindex (curl)\n%!" in
      let curl = get_curl params local in
      let _ = printf "inferring zindex\n%!" in
      let _ = infer_zindex params local curl smim in

      let _ = printf "inferring critical points\n%!" in
      let _ = infer_phi params local in

      let _ = printf "drawing pictures\n%!" in
      let hhim = draw_orientations local hh_votes in
      let orim = draw_orientations local local.orientations in
      let curlim = Image.map (fun x -> round (255. *. (logistic x))) curl in
      let zim = Image.map (fun x -> round (255. *. (logistic (x*.0.5)))) local.zindex in
(*       let zim = Image.map (fun x -> 127 * (x+1)) local.zindex in *)
      let phim = Image.map (fun x -> round (255. *. (logistic x))) local.phi in

	Pnm.save_ppm hhim (sprintf "%s.%d.hh.ppm" name foo);
	Pnm.save_ppm orim (sprintf "%s.%d.orientations.ppm" name foo);
	Pnm.save_pgm curlim (sprintf "%s.%d.curl.pgm" name foo);
	Pnm.save_pgm zim (sprintf "%s.%d.zindex.pgm" name foo);
	Pnm.save_pgm phim (sprintf "%s.%d.phi.pgm" name foo);
    done;

    printf "done with local inference!\n%!";
    local


let params = {
(*   sigma_z = 0.1; (\* 1.0; *\) *)
  sigma_z = 1.0; (* 1.0; *)
(*   div_thresh = 0.5; *)
  div_thresh = 0.0;

(*   hh_curl_wt = 0.1; *)
  hh_curl_wt = 1.0;
  hh_div_wt = 0.0;
  hh_orth_sigma = 0.0;
  hh_par_sigma = 0.1; (* 1.0; *)

  yield_wt = 10.0;
  logistic_param = 1.0;
(*   nbr_window = 4; *)
  nbr_window = 16;
(*   orth_dropoff_rate = 1.0;   *)
  orth_dropoff_rate = 0.25;  

  orientation_maxiter = 300;
  orientation_tempinc = 0.01;

  curl_window = 5;
  curl_sigma = 1.0;
  zindex_maxiter = 300;
  zindex_tempinc = 0.01;
  nbr_wt = 10.0;
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
(*   let segs = List.map (fun ((x1,y1), (x2,y2)) -> *)
(* 			 ((4*x1,4*y1), (4*x2,4*y2))) segs  *)
(*   in *)
 
  let im = Pnm.load_pgm "small-asl-5.pgm" in
(*   let im = Pnm.load_pgm "romer/clean/IMG0200.PGM" in *)
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

  let _ = Edges.save_edge_data edgedata "helmholtz" in 
    
  let local = infer_everything params edgedata smooth segs "helmholtz/z" in

    local
