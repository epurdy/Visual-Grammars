open Printf
open Util.Misc
open Util.Hops

module I = Image

type segment = {
  initial: int * int;
  id: int;
  mutable parent: int option;
  mutable size: int; (* or float? *)
  mutable maxwt: float;
  mutable bounds: int * int * int * int;
  (* minx, miny, maxx, maxy *)
}

let find segments i =
  let rv = ref i in
  let j = ref i in
    while segments.(!rv).parent <> None do 
      rv := get segments.(!rv).parent;
    done;
    while !j <> !rv do 
      let oldp = get segments.(!j).parent in
	segments.(!j).parent <- Some !rv;
	j := oldp;
    done;
    !rv

let get_segmentation smoothed (w,h) curl = 
  printf "initializing\n%!";

  let k = 30. in
  let n = w * h in
  let labelimage = Image.map (fun x -> (-1)) smoothed in
  let segments = Array.init n
    begin fun i ->
      let x,y = i mod w, i / w in
	I.set labelimage i (x,y);
	{initial = (x,y);
	 id = i;
	 parent = None;
	 size = 1;
	 bounds = (x,y,x,y);
	 maxwt = 0.;
	}
    end
  in

    printf "making edges\n%!";
    let smallcurl = 1.0 in
    let edges = Array.init (2*n) (fun x -> None) in
    let enum = ref 0 in
    let _ = 
      for i = 0 to n-1 do
	let pix = segments.(i) in
	let x,y = pix.initial in
	  if x < w-1 then begin 
	    let diff = (I.get smoothed (x,y)) -. (I.get smoothed (x+1,y)) in
	    let wt =
(* 	      if (I.get curl (x,y)) *. (I.get curl (x+1,y)) < -. smallcurl then *)
(* 		infinity *)
(* 	      else *)
		abs_float diff
	    in
	      edges.(!enum) <- Some (i, i+1, wt);
	      incr enum;
	  end;
	  if y < h-1 then begin 
	    let diff = (I.get smoothed (x,y)) -. (I.get smoothed (x,y+1)) in
	    let wt =
(* 	      if (I.get curl (x,y)) *. (I.get curl (x,y+1)) < -. smallcurl then *)
(* 		infinity *)
(* 	      else *)
		abs_float diff
	    in
	      edges.(!enum) <- Some (i, i+w, wt);
	      incr enum;
	  end
      done;
    in
    let edges = Array.init !enum (fun i -> get edges.(i)) in

    let promote_left si sj wt =
      let iminx, iminy, imaxx, imaxy = si.bounds in
      let jminx, jminy, jmaxx, jmaxy = sj.bounds in
	sj.parent <- Some si.id;
	si.size <- si.size + sj.size;
	si.maxwt <- wt;
	si.bounds <- (min iminx jminx, min iminy jminy,
		      max imaxx jmaxx, max imaxy jmaxy);
    in

      printf "sorting\n%!";
      let _ = Array.sort 
	begin fun (_, _, wt1) (_, _, wt2) ->
	  compare wt1 wt2
	end
	edges
      in

	printf "computing segmentation\n%!";
	Array.iteri
	  begin fun eidx (i,j,wt) ->
	    let _ = 
	      if eidx < !enum-1 then begin
		let (i2,j2,wt2) = edges.(eidx+1) in
		  assert(wt <= wt2);
	      end;

	      assert(wt >= 0.);
	    in
	    let i,j = find segments i, find segments j in
	      if i <> j then begin
		let si, sj = segments.(i), segments.(j) in
(* 		let iminx, iminy, imaxx, imaxy = si.bounds in *)
(* 		let jminx, jminy, jmaxx, jmaxy = sj.bounds in *)
(* 		let ispan = min 1 (min (imaxx - iminx) (imaxy - iminy)) in *)
(* 		let jspan = min 1 (min (jmaxx - jminx) (jmaxy - jminy)) in *)

		let thresh = min
		  (si.maxwt +. (k /. (foi si.size)))
		  (sj.maxwt +. (k /. (foi sj.size)))
		in
		  if wt < thresh then begin 
		    if si.size < sj.size then begin 
		      promote_left sj si wt;
		    end
		    else begin 
		      promote_left si sj wt;
		    end
		  end
	      end
	  end
	  edges;

	Array.iter
	  begin fun (i,j,wt) ->
	    let i,j = find segments i, find segments j in
	    let si,sj = segments.(i), segments.(j) in
	      if i <> j then begin 
		let iminx, iminy, imaxx, imaxy = si.bounds in
		let jminx, jminy, jmaxx, jmaxy = sj.bounds in
		let ispan = min (imaxx - iminx) (imaxy - iminy) in
		let jspan = min (jmaxx - jminx) (jmaxy - jminy) in
		  if ispan < 5 || jspan < 5 then begin 
		    if si.size < sj.size then begin 
		      promote_left sj si wt;
		    end
		    else begin 
		      promote_left si sj wt;
		    end
		  end;
	      end
	  end
	  edges;
	

	printf "producing image\n%!";
	for y=0 to h-1 do 
	  for x=0 to w-1 do 
	    I.set labelimage (find segments (I.get labelimage (x,y))) (x,y);
	  done
	done;

	labelimage

let draw_segmentation labelimage = 
  let im = Image.map (fun x -> (255,0,255)) labelimage in
  let w,h = Image.width im, Image.height im in
  let colors = mkhash 100 in
    for y=0 to h-1 do
      for x=0 to w-1 do 
	let i = I.get labelimage (x,y) in
	  if colors >>? i then
	    I.set im (colors >> i) (x,y)
	  else begin 
	    let color = (Random.int 256, Random.int 256, Random.int 256) in
	      colors << (i, color);
	      I.set im color (x,y);
	  end
      done 
    done;

    im
