open Printf
open Scanf
open Util.Misc
open Util.Hops

module I = Image
module C = Complex

(* let windowsize = 8 *)
let windowsize = 3

let curve_of_annotation annot = 
  let n = ref 0 in
    Hashtbl.iter 
      begin fun (x,y) i ->
	n := max i !n;
      end
      annot;
    let curve = Array.init !n (fun x -> None) in
      Hashtbl.iter
	begin fun (x,y) i ->
	  curve.(i-1) <- Some (Geometry.complex_of_point (x,y));
	end 
	annot;
      Array.map get curve


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

let draw_annotation annot im = 
  let boxsize = windowsize*2 +1 in
  let w, h = Image.width im, Image.height im in

    for tlabel = 0 to 60 do 
      let color = nice_color tlabel in
      let ymin = (tlabel / 30) * boxsize in
      let xmin = ((tlabel mod 30) + ((tlabel mod 30) / 10)) * boxsize in
	for y = ymin to ymin + boxsize-1 do
	  for x = xmin to xmin + boxsize-1 do 
	    I.set im color (x,y);
	  done
	done
    done;
    
    Hashtbl.iter 
      begin fun (x,y) tlabel ->
	let color = nice_color tlabel in

	  for dy= -windowsize to windowsize do
	    for dx = -windowsize to windowsize do
	      if (x+dx >= 0 && x+dx < w && y+dy >= 0 && y+dy < h) then
		I.set im color (x+dx,y+dy);
	    done
	  done
	    
      end
      annot
      
let print_annotation annot =
  Hashtbl.iter
    begin fun (x,y) tlabel ->
      printf "%d %d (%d, %d)\n" tlabel tlabel x y;
    end
    annot
