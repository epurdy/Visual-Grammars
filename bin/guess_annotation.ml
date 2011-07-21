open Printf
open Scanf
open Util.Misc
open Util.Hops

module I = Image

open Annotation

let match_annotations ann1 ann2 = 
  (* it is presumed that ann2 does not have meaningful labels *)
  let rann1 = mkhash 100 in (* map ids to points *)
  let rann2 = mkhash 100 in (* map ids to points *)
  let fann2 = mkhash 100 in (* map points to ids *)
  let themap = mkhash 100 in (* map ids in 1 to ids in 2 *)
  let n = Hashtbl.length ann1 in
  let counter = ref 1 in
  let xs1 = Array.init n (fun x -> (-1,-1)) in (* an array containing (x,id) of ann1 *)
  let ys1 = Array.init n (fun x -> (-1,-1)) in
  let xs2 = Array.init n (fun x -> (-1,-1)) in
  let ys2 = Array.init n (fun x -> (-1,-1)) in
  let xrank1 = mkhash 100 in
  let yrank1 = mkhash 100 in
  let xrank2 = mkhash 100 in
  let yrank2 = mkhash 100 in

    Hashtbl.iter 
      begin fun (x,y) i ->
	assert (not (rann1 >>? i));
	rann1 << (i,(x,y));
	themap << (i, i);
	xs1.(!counter-1) <- (x,i);
	ys1.(!counter-1) <- (y,i);
	incr counter;
      end
      ann1;
    counter := 1;
    Hashtbl.iter
      begin fun (x,y) _ ->
	rann2 << (!counter, (x,y));
	fann2 << ((x,y), !counter);
	xs2.(!counter-1) <- (x,!counter);
	ys2.(!counter-1) <- (y,!counter);
	incr counter;
      end
      ann2;

    Array.sort (fun (z1,_) (z2,_) -> compare z1 z2) xs1;
    Array.sort (fun (z1,_) (z2,_) -> compare z1 z2) ys1;
    Array.sort (fun (z1,_) (z2,_) -> compare z1 z2) xs2;
    Array.sort (fun (z1,_) (z2,_) -> compare z1 z2) ys2;

    Array.iteri (fun rk (_,i) -> xrank1 << (i, rk)) xs1;
    Array.iteri (fun rk (_,i) -> yrank1 << (i, rk)) ys1;
    Array.iteri (fun rk (_,i) -> xrank2 << (i, rk)) xs2;
    Array.iteri (fun rk (_,i) -> yrank2 << (i, rk)) ys2;

(*     for i = 1 to n do  *)
(*       let x1,y1 = rann1 >> i in *)
(*       let x2,y2 = rann2 >> i in *)
(*       printf "In ann1, point #%d (%d,%d) has xrank %d, yrank %d\n" i x1 y1  *)
(* 	(xrank1 >> i) (yrank1 >> i); *)
(*       printf "In ann2, point #%d (%d,%d) has xrank %d, yrank %d\n" i x2 y2  *)
(* 	(xrank2 >> i) (yrank2 >> i); *)
(*     done; *)

    for pass=1 to n*n*n do
(*       printf "pass #%d\n" pass; *)
    for a=1 to n do
      for b=1 to n do
	let c, d = themap >> a, themap >> b in

	let axr, ayr = xrank1 >> a, yrank1 >> a in
	let bxr, byr = xrank1 >> b, yrank1 >> b in
	let cxr, cyr = xrank2 >> c, yrank2 >> c in
	let dxr, dyr = xrank2 >> d, yrank2 >> d in

	let tx1old, ty1old = axr-cxr, ayr-cyr in
	let tx2old, ty2old = bxr-dxr, byr-dyr in
	let tx1new, ty1new = axr-dxr, ayr-dyr in
	let tx2new, ty2new = bxr-cxr, byr-cyr in
(* 	let cost = tx1old*tx1old + ty1old*ty1old + tx2old*tx2old + ty2old*ty2old in *)
(* 	let newcost = tx1new*tx1new + ty1new*ty1new + tx2new*tx2new + ty2new*ty2new in *)
	let cost = (abs tx1old) + (abs ty1old) + (abs tx2old) + (abs ty2old) in
	let newcost = (abs tx1new) + (abs ty1new) + (abs tx2new) + (abs ty2new) in

(* 	  printf "trying %d and %d, %d vs %d\n" c d newcost cost; *)
	  if newcost < cost then begin 
	    themap << (a,d);
	    themap << (b,c);
(* 	    printf "swapped %d and %d, %d < %d\n" c d newcost cost; *)
(* 	    printf "point #%d had ranks %d,%d, which is more like %d,%d\n" c cxr cyr bxr byr; *)
(* 	    printf "point #%d had ranks %d,%d, which is more like %d,%d\n" d dxr dyr axr ayr; *)
(* 	    printf "---\n"; *)
	  end
      done
    done;
    done;

    let foo= mkhash 100 in
    Hashtbl.iter 
      begin fun (x,y) i ->
	let x2, y2 = rann2 >> (themap >> i) in
	  foo << ((x2,y2), i);
      end
      ann1;
    foo

let _ = 
  let ann1 = load_annotation Sys.argv.(1) in
  let ann2 = load_annotation Sys.argv.(2) in
  let ann3 = match_annotations ann1 ann2 in
    print_annotation ann3
