open Printf
open Abstract
open Util.Misc
open Util.Hops

let dist2 (ax,ay) (bx,by) = (ax-bx)*(ax-bx) + (ay-by)*(ay-by)

type sdata = {
  p: int * int;
  q: int * int;
  len2: int;
  lvl: int;
}

type cdata = {
  bg: int * int;
  md: int * int;
  en: int * int;
}

let make_sdf granularity maxlen = 
  let gram = new_live_grammar () in
  let lookup = mkhash 10000 in

  let rec intlog2 x = 
    if x <= 1 then 0 
    else 1 + (intlog2 (x/2))
  in

  let make_scurve (px,py) (qx,qy) = 
    let len2 = dist2 (px,py) (qx,qy) in
    let foo = len2 / (maxlen*maxlen) in
    let lvl = (intlog2 foo) / 2 in
    let step = 1 lsl lvl in
    let (px,py) = (step*(px/step), step*(py/step)) in
    let (qx,qy) = (step*(qx/step), step*(qy/step)) in
    let scurve = {
      p = (px,py);
      q = (qx,qy);
      len2 = len2;
      lvl = lvl;
    }
    in
      if not (lookup >>? scurve) then begin
	make_new_symbol gram scurve false
      end
      else
	lookup >> scurve
  in

  let granularity = ref granularity in
  let size = 256 in

    while !granularity > 2 do 
      let step = size / !granularity in
	for pxi = 0 to !granularity-1 do 
	  for pyi = 0 to !granularity-1 do 
	    for qxi = (max 0 (pxi-maxlen)) to (min (!granularity-1) (pxi+maxlen)) do 
	      for qyi = (max 0 (pyi-maxlen)) to (min (!granularity-1) (pyi+maxlen)) do 

		let len_pq_2 = dist2 (pxi,pyi) (qxi,qyi) in
		  if len_pq_2 <= maxlen*maxlen &&
		    maxlen*maxlen/4 < len_pq_2
		  then begin
		    let px, py = step * pxi, step * pyi in
		    let qx, qy = step * qxi, step * qyi in
		      printf "sym (%d,%d) - (%d,%d)\n" px py qx qy;


		      (* 		      let len_pq = sqrt (float_of_int len_pq_2) in *)

		      (* assume wlog d(p,q) <= d(q,r) <= d(p,r)

			 also assume d(q,r) <= 2*d(p,q)

			 this means we need to think about all 6
			 possible compositions, though *)
		      (* need to think about the bounds on rxi, ryi... *)
		      for rxi = (max 0 (qxi-2*maxlen)) to (min (!granularity-1) (qxi+2*maxlen)) do 
			for ryi = (max 0 (qyi-2*maxlen)) to (min (!granularity-1) (qyi+2*maxlen)) do 
			  let rx, ry = step * rxi, step * ryi in
			  let len_qr_2 = dist2 (qxi,qyi) (rxi,ryi) in
			  let len_pr_2 = dist2 (pxi,pyi) (rxi,ryi) in

			    if len_qr_2 >= len_pq_2 &&
			      len_qr_2 <= 4 * len_pq_2 &&
			      len_pr_2 >= len_qr_2 then begin 
				printf "rule (%d,%d) - (%d,%d) - (%d,%d)\n" 
				  px py 
				  qx qy
				  rx ry;

			      end

			done
		      done
		  end

	      done
	    done
	  done
	done;
	granularity := !granularity / 2;
    done

(*       for px = 0 to granularity-1 do *)
(* 	printf "%d\n%!" px; *)
(* 	for py = 0 to granularity-1 do *)
(* 	  for qx = 0 to granularity-1 do *)
(* 	    for qy = 0 to granularity-1 do *)
(* 	      for rx = 0 to granularity-1 do *)
(* 		for ry = 0 to granularity-1 do *)

(* 		  tot := !tot + px + py + qx + qy + rx + ry; *)
(* 		  let top = make_scurve (px,py) (rx,ry) in *)
(* 		  let left = make_scurve (px,py) (qx,qy) in *)
(* 		  let right = make_scurve (qx,qy) (rx,ry) in *)
(* 		    () *)
(* 		      (\* 		  imake_new_composition gram top.sid (left.sid,right.sid)  *\) *)
(* 		      (\* 		    {bg = top.sdata.p; *\) *)
(* 		      (\* 		     md = left.sdata.q; *\) *)
(* 		      (\* 		     en = top.sdata.q; *\) *)
(* 		      (\* 		    } *\) *)
(* 		done *)
(* 	      done *)
(* 	    done *)
(* 	  done *)
(* 	done *)
(*       done *)


let _ = make_sdf 32 5
