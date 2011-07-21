open Printf
open Scanf
open Util.Misc
open Util.Hops

module I = Image

open Annotation

let load_annotation_from_image fname imname outname =
  let annotation = load_annotation fname in
  let aim = Pnm.load_ppm imname in
  let labels = Image.map 
    begin fun (r,g,b) ->
      if r*r + (255-g)*(255-g) + b*b < 2000 then
	0
      else
	(-1)
    end 
    aim 
  in
  let w,h = Image.width aim, Image.height aim in
  let labelmap = mkhash 100 in
  let nextlabel = ref 1 in

  let rec visit (x,y) label color = 
    let xtot, ytot, nseen = ref x, ref y, ref 1 in
    let nbrs = [(x-1,y); (x+1,y); (x,y-1); (x,y+1)] in
      I.set labels label (x,y);
      I.set aim color (x,y);

      if annotation >>? (x,y) then begin
	let tlabel = annotation >> (x,y) in
	  if tlabel > 0 then begin
(* 	    point #label is labeled tlabel in annotation file *)
	    labelmap << (label, tlabel);
	  end
      end;

      List.iter 
	begin fun (nx,ny) ->
	  if nx >=0 && nx < w && ny >= 0 && ny < h then begin 
	    if I.get labels (nx,ny) = 0 then begin 
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
	if I.get labels (x,y) = 0 then begin
	  let thelabel = !nextlabel in
	    incr nextlabel;	    

	    let xsum, ysum, nseen = visit (x,y) thelabel 
	      (Random.int 255,Random.int 128,Random.int 255) in
	    let xsum, ysum, nseen = 
	      float_of_int xsum, float_of_int ysum, float_of_int nseen in
	    let mx, my = (xsum /. nseen), (ysum /. nseen) in
	    let mx, my = int_of_float mx, int_of_float my in

	    let tlabel = labelmap >>! (thelabel, 0) in
	      annotation << ((mx,my), tlabel);

	end
      done
    done;

    print_annotation annotation;
    draw_annotation annotation aim;
    Pnm.save_ppm aim outname;

    annotation

let _ = 
  (* ./read_annotation.native x.annot x.ppm out.ppm *)
  load_annotation_from_image Sys.argv.(1) Sys.argv.(2) Sys.argv.(3);

