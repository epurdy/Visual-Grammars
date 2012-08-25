open Printf
open Scanf
open Util.Hops
open Util.Cops
open Util.Misc

module C = Complex
module I = Image

let lo, hi = 50., 100.
(* let sigma = 2.0 *)
let sigma = 1.0

let yieldwt = {C.re=1.0; C.im=0.}
let logistic_param = 1.0

let window = 4
let pi = acos (-. 1.)
let foi = float_of_int

let logistic x =      
  let ex = exp x in
  let enx = exp (-. x) in
  let prob = ex /. (ex +. enx) in
(*     if ex = infinity then 1.0  *)
(*     else begin *)
(*       if enx = infinity then 0.0 *)
(*       else prob *)
(*     end *)
    prob

let get_edge_pixels im =
  let epixels = mkhash 100 in
  let w,h = I.width im, I.height im in
    for y=2 to h-3 do
      for x=2 to w-3 do 
	if I.get im (x,y) then begin 
	  (* 	  epixels << ((x,y), (0.,0.)); *)
	  epixels << ((x,y), c0);
	end
      done 
    done;
    epixels

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

let rotate_gradient dx dy =
  let w,h = I.width dx, I.height dx in
  let rgrad = Image.create w h c0 in
    for y=0 to h-1 do 
      for x=0 to w-1 do 
	let gx,gy = I.get dx (x,y), I.get dy (x,y) in
	  I.set rgrad {C.re = gy; C.im = -.gx} (x,y);
      done
    done;
    rgrad

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


let draw_orientations im epixels = 
  let diffs = [| (1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1) |] in
  let sqsize = 5 in
  let colors = [| (255,0,0); (255,0,255); (0,0,255); (0,255,255);
		  (0,255,0); (128,64,0); (255,255,0); (255,128,0) |]
  in
  let im = Image.map (fun x -> (255,255,255)) im in
    Hashtbl.iter 
      begin fun (px,py) v ->
	let a = C.arg v in
	let a = (a +. pi) in
	let a = 8. *. a /. (2. *. pi) in
	let a = (round a) mod 8 in
	  I.set im colors.(a) (px,py);
      end
      epixels;

    for a = 0 to 7 do 
      for y=0 to sqsize-1 do 
	for x=0 to sqsize-1 do 
	  let dx,dy = diffs.(a) in
	    I.set im colors.(a) (20+dx*sqsize+x, 20+dy*sqsize+y);
	done
      done
    done;
    im

let subsample im gran = 
  let w, h = (I.width im)/gran, (I.height im)/gran in
  let sim = I.create w h (I.get im (0,0)) in
    for y=0 to h-1 do
      for x=0 to w-1 do
	I.set sim (I.get im (gran*x, gran*y)) (x,y);
      done
    done;
    sim

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


  let epixels = get_edge_pixels edges in (* hash from (x,y) to vector *)
  let segments = load_yield "theyield" in

  let rgrad = rotate_gradient dx dy in
  let yielddir = I.map (fun x -> c0) im in
  let nbr_wts = mkhash 1000 in
    
    List.iter
      begin fun seg ->
	let (ipx,ipy), (iqx,iqy) = seg in
	let px,py,qx,qy = foi ipx, foi ipy, foi iqx, foi iqy in
	let vx, vy = qx -. px, qy -. py in
	let normv = sqrt (vx *. vx +. vy *. vy) in
	let vx, vy = vx /. normv, vy /. normv in
	let v = {C.re=vx; C.im=vy} in
	  List.iter
	    begin fun (rx,ry) ->
	      I.set yielddir ((I.get yielddir (rx,ry)) +& v) (rx,ry);
	    end
	    (Draw.line (ipx,ipy) (iqx,iqy));
      end
      segments;


    Hashtbl.iter
      begin fun (px,py) v ->
	for y=py-window to py+window do
	  for x=px-window to px+window do
	    if I.inside im (x,y) then begin 
	      if epixels >>? (x,y) then begin 
		(* calculate an appropriate weight *)
		let dx,dy = (foi (x-px),foi (y-py)) in
		let rg = I.get rgrad (x,y) in
		let rx,ry = rg.C.re, rg.C.im in
		let disp_par = (dx *. rx +. dy *. ry)/.(foi (2*window)) in
		let disp_orth = dx *. ry -. dy *. rx in
		let wt = (exp (-. disp_par *. disp_par)) *.
		  (1. -. (abs_float disp_orth))
		in
		  nbr_wts <<+ ((px,py), (x,y,wt));
(* 		  printf "wt=%f\n" wt; *)
	      end
	    end
	  done
	done
      end
      epixels;

    printf "inferring orientations\n%!";
    for iter = 1 to 300 do 
      (*     while true do  *)
      (* save an image to look at *)
      let im = draw_orientations im epixels in
	if (iter mod 10) = 0 then begin
	  Pnm.save_ppm im (sprintf "orient.%d.ppm" iter);
	end;
	Hashtbl.iter 
	  begin fun (px,py) v ->
	    (* should v itself get a vote? *)
	    let v = ref c0 in
	      v := !v +& yieldwt *& (I.get yielddir (px,py));

	      (* get nbr votes *)
	      List.iter
		begin fun (qx,qy,wt) ->
		  v := !v +& (cxre wt) *& (epixels >> (qx,qy));
		end
		(nbr_wts >>> (px,py));

	      (* dot vector with rotated gradient *)
	      let rg = I.get rgrad (px,py) in
	      let rx,ry = rg.C.re, rg.C.im in
	      let vx,vy = !v.C.re, !v.C.im in
	      let dot = rx *. vx +. ry *. vy in
	      let dot = dot *. (logistic_param *. (foi iter) /. 100.) in
	      let prob = logistic dot in
		(* for now, print out the dot product to get sense of magnitudes *)
(* 		printf "dot=%f, prob=%f\n%!" dot prob; *)
		(* pick +/- rotated gradient with logistic probability *)
		if Random.float 1.0 < prob then begin 
		  epixels << ((px,py), rg)
		end
		else begin
		  epixels << ((px,py), c0 -& rg)
		end;

	  end
	  epixels;

    done;

    (* infer the interior

       this gives positive for interior and negative for exterior (if
       curve is clockwise). should be close to zero at boundary

       clean this up somehow. use ising model, with curl as data term,
       edge pixels not counting as neighbors

    *)

    let wting = mkhash 100 in
      for dy = -5 to 5 do 
	for dx = -5 to 5 do 
	  let gwting = exp (-. (foi (dx*dx + dy*dy))) in
	  let norm = sqrt (foi (dx*dx + dy*dy)) in
	    wting << ((dx,dy), gwting /. norm);
	done
      done;
    
      let w,h = Image.width im, Image.height im in
      let curl = Image.map (fun x -> 0.) im in
	for y=0 to h-1 do
	  for x=0 to w-1 do
	    for dy = -5 to 5 do 
	      for dx = -5 to 5 do 
		if (dx,dy) <> (0,0) then begin
		  let ex,ey = x+dx, y+dy in
		    if epixels >>? (ex,ey) then begin 
		      let thisone = epixels >> (ex,ey) in
		      let thisone = (foi dy) *. thisone.C.re -. (foi dx) *. thisone.C.im in
		      let thisone = thisone *. (wting >> (dx,dy)) in
			assert(classify_float thisone <> FP_nan);
			I.set curl ((I.get curl (x,y)) +. thisone) (x,y);
		    end
		end
	      done
	    done;

(* 	    printf "curl=%f\n" (I.get curl (x,y)); *)
	  done
	done;

    let inside = Image.map (fun x -> (Random.float 1.0) < logistic (x)) curl in

    printf "inferring interior\n%!";
    let maxiter = 3000 in
      for iter = 1 to maxiter do 
	for y=0 to h-1 do 
	  for x=0 to w-1 do 
	    if not (epixels >>? (x,y)) then begin 
	      let votewt = 10.0 in
	      let v = ref (I.get curl (x,y)) in

		List.iter
		  begin fun (x2,y2) ->
		    if I.inside im (x2,y2) && not (epixels >>? (x2,y2)) then begin 
		      if I.get inside (x2,y2) then
			v := !v +. votewt
		      else
			v := !v -. votewt;
		    end
		  end
		  [ (x-1,y); (x+1,y); (x,y-1); (x,y+1) ];

		let p = logistic (!v *. 0.001 *. (foi iter)) in
(* 		  printf "v=%f, p=%f\n" !v p; *)
		  if Random.float 1.0 < p then
		    I.set inside true (x,y)
		  else
		    I.set inside false (x,y);
	    end
	  done
	done;

	if iter = maxiter || iter mod 100 = 0 then begin
	  let interiorim = Image.mapxy
	    begin fun (x,y) b ->
	      if epixels >>? (x,y) then
		(0,0,255)
	      else begin
		if b then
		  (0,255,0)
		else
		  (255,0,0)
	      end
	    end
	    inside
	  in
	    Pnm.save_ppm interiorim (sprintf "interior.%d.ppm" iter);
	    if iter = maxiter then
	      Pnm.save_ppm interiorim "theinterior.ppm";
	end
      done;

      let curlim = Image.mapxy
	begin fun (x,y) v ->
	  if epixels >>? (x,y) then
	    (0,255,0)
	  else begin 
	    let lv = logistic v in
	      if isfinite lv then begin
		let v = round (lv *. 255.) in
		  (v,v,v)
	      end
	      else
		(255,0,0)
	  end
	end
	curl
      in	
      let _ = Pnm.save_ppm curlim "curlim.ppm" in
	
    let im = save_orientations im epixels in
      Pnm.save_pgm im "orientations.pgm"
