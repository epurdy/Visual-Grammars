open Printf
open Scanf
open Util.Misc
open Util.Hops

module I = Image

let windowsize = 8

let nice_color tlabel = 
  let x = (tlabel * tlabel * tlabel * tlabel) mod 61 in
  let x = (tlabel * tlabel * tlabel * x) mod 61 in
  let r,g,b = x mod 4, (x/4) mod 4, (x/16) in
    (r+1)*64, (g+1)*64, (b+1)*64


let load_annotation fname = 
  let annot = mkhash 100 in
  let chan = open_in fname in
    begin try
      while true do 
	let line = input_line chan in
	  try 
	    sscanf line "%d %d (%d, %d)" 
	      (fun tlabel num x y -> 
		 annot << ((x,y), tlabel))
	  with _ -> 
	    printf "dying now...\n";
	    exit (-1);
      done
    with End_of_file ->
      close_in chan;
    end;
    annot

let _ = 
(*   let cim = Pnm.load_pgm Sys.argv.(1) in *)
  let aim = Pnm.load_ppm Sys.argv.(2) in
  let xim = Image.map 
    begin fun (r,g,b) ->
      r*r + (255-g)*(255-g) + b*b < 2000
    end 
    aim 
  in
  let w,h = Image.width aim, Image.height aim in
  let annotation = load_annotation Sys.argv.(1) in
  let labelmap = mkhash 100 in
  let labels = Image.create w h 0 in
  let nextlabel = ref 1 in

  let rec visit (x,y) label color = 
    let xtot, ytot, nseen = ref x, ref y, ref 1 in
    let nbrs = [(x-1,y); (x+1,y); (x,y-1); (x,y+1)] in
      I.set labels label (x,y);
      I.set aim color (x,y);

      if annotation >>? (x,y) then begin
	let tlabel = annotation >> (x,y) in
	  if tlabel > 0 then begin
	    labelmap << (label, tlabel);
(* 	    printf "Discovered that point #%d is labeled %d in annotation file!\n" *)
(* 	      label tlabel; *)
	  end
      end;

      List.iter 
	begin fun (nx,ny) ->
	  if nx >=0 && nx < w && ny >= 0 && ny < h then begin 
	    if (I.get xim (nx,ny)) && (I.get labels (nx,ny) = 0) then begin 
	      let xsum, ysum, nhere = visit (nx,ny) label color in
		xtot  := !xtot  + xsum;
		ytot  := !ytot  + ysum;
		nseen := !nseen + nhere;
	    end
	  end
	end
	nbrs;
      (!xtot, !ytot, !nseen)
  in
    for y=0 to h-1 do
      for x=0 to w-1 do
	if I.get xim (x,y) && (I.get labels (x,y) = 0) then begin
	  let thelabel = !nextlabel in
	    incr nextlabel;	    
	    let xsum, ysum, nseen = visit (x,y) thelabel 
	      (Random.int 255,Random.int 128,Random.int 255) in
	    let xsum, ysum, nseen = 
	      float_of_int xsum, float_of_int ysum, float_of_int nseen in
	    let mx, my = (xsum /. nseen), (ysum /. nseen) in
	    let mx, my = int_of_float mx, int_of_float my in
	    let tlabel = labelmap >>! (thelabel, 0) in
	    let color = nice_color tlabel in

	      printf "%d %d (%d, %d)\n" tlabel thelabel mx my;

	      for dy= -windowsize to windowsize do
		for dx = -windowsize to windowsize do
		  if (mx+dx >= 0 && mx+dx < w && my+dy >= 0 && my+dy < h) then
		    I.set aim color (mx+dx,my+dy);
		done
	      done

	end
	  
      done
    done;

    let boxsize = windowsize*2 +1 in

    for tlabel = 0 to 60 do 
      let color = nice_color tlabel in
      let ymin = (tlabel / 30) * boxsize in
      let xmin = ((tlabel mod 30) + ((tlabel mod 30) / 10)) * boxsize in
	for y = ymin to ymin + boxsize-1 do
	  for x = xmin to xmin + boxsize-1 do 
	    I.set aim color (x,y);
	  done
	done
    done;


    Pnm.save_ppm aim Sys.argv.(3);

    ()


