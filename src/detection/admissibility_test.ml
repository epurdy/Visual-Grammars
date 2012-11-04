open Printf
open Util.Misc
open Filtration
open Util.Cops
open Util.Hops
open Graph
module C = Complex

(* the way we've written it now, we only are looking at sets at the
 same level, but we also want to be able to do sets between levels.
 probably too many combinations then, so switch from doing everything
 to random samples? anyway, check this first
*)

let min_dist p r lvl =
  let jump = 2 lsl lvl in
  let px, py = p in
  let rx, ry = r in
  let dx, dy = abs (px-rx), abs (py-ry) in
  let dx = min dx (abs (dx-jump)) in
  let dy = min dy (abs (dy-jump)) in
    sqrt (float_of_int (dx*dx + dy*dy)) 

let max_diff lvl =
  let sidelen = float_of_int (2 lsl lvl) in
    sidelen *. 1.414

(* need to convert back and forth between shape space and bookstein below *)

let bound_shape_cost2 lvl (p,q,r) model verbose =
  let sidelen = (2 lsl lvl) - 1 in
  let (px,py), (qx,qy), (rx,ry) = p,q,r in
  let jig n = n + Random.int sidelen in
  let mincost, maxcost = ref infinity, ref neg_infinity in
    for i = 1 to 10 do
	let px,py,qx,qy,rx,ry = jig px, jig py, jig qx, jig qy, jig rx, jig ry in
	let shape = Shape.shape_of_ints_bme (px,py) (qx,qy) (rx,ry) in
	let cost = Watson.cost model shape in
	  if cost < !mincost then 
	    mincost := cost;
	  if cost > !maxcost then 
	    maxcost := cost;
    done;
    if verbose then begin 
      printf "bsc2: %f %f %f\n%!" !mincost !maxcost (2. *. !mincost -. !maxcost);
    end;
    3. *. !mincost -. 2. *. !maxcost
    

let bound_shape_cost lvl (p,q,r) model verbose =
  let shape = Shape.shape_of_ints_bme p q r in
  let z = Shape.get_pt shape in
  let mu = Shape.get_pt model.Watson.mean in
  let diff = mu -& z in
  let mindist = min_dist p r lvl in
  let max_abs_diff = max_diff lvl in
  let max_book_diff = max_abs_diff /. mindist in
  let dist = C.norm diff in
  let z' =
    if max_book_diff < dist then
      z +& diff *& (cxre (max_book_diff /. dist))
    else
      mu
  in
  let _ = 
    if verbose then begin 
      printf "bsc: ";
      Geometry.print_cpt z';
    end
  in
  let newshape = Shape.shape_of_complex_bme c0 z' c1 in
    Watson.cost model newshape

let _ = 
  let size = 8 in
  let nlvls = 3 in

  let net = Gridparsing.make_net size size in
  let nverts = Array.length net.vertices in
  let filtration = make_filtration net.vertices nlvls in
    
  let tindices = Array.init 20 
    begin fun i -> 
      (Random.int nverts, Random.int nverts, Random.int nverts)
    end
  in
  let tmodels = Array.map 
    begin fun (pid,qid,rid) -> 
      {
	Watson.mean = (Shape.shape_of_ints_bme
			 net.vertices.(pid) net.vertices.(qid) net.vertices.(rid));
	Watson.conc_ = 100.};
    end
    tindices
  in

  let lbounds_est = mkhash 100 in
  let lbounds_true = mkhash 100 in
  let pts = filtration.points in
  let npts = Array.length pts in

    Array.iteri
      begin fun tnum model ->
	for lvl=0 to filtration.depth-1 do 
	  printf "estimating bounds tnum=%d, lvl=%d\n%!" tnum lvl;
	  lbounds_est << ((tnum,lvl), mkhash 1000);
	  lbounds_true << ((tnum,lvl), mkhash 1000);

	  let lbounds_est_this = lbounds_est >> (tnum,lvl) in
	  let topids = top_level_ids filtration.levels.(lvl) in
	  let ntopids = Array.length topids in
	    for ii = 0 to ntopids-1 do 
	      for jj = 0 to ntopids-1 do 
		for kk = 0 to ntopids-1 do 
		  let i,j,k = topids.(ii), topids.(jj), topids.(kk) in
		  let lb = bound_shape_cost2 lvl (pts.(i),pts.(j),pts.(k)) model false in
		    lbounds_est_this << ((i,j,k), lb);
		done
	      done
	    done
	done;

	  let lbounds_true_this = lbounds_true >> (tnum,0) in
	    printf "true bounds tnum=%d, lvl=0\n" tnum;
	    for i = 0 to npts-1 do 
	      for j = 0 to npts-1 do 
		for k = 0 to npts-1 do 
		  let shape = Shape.shape_of_ints_bme pts.(i) pts.(j) pts.(k) in
		  let cost = Watson.cost model shape in
		    lbounds_true_this << ((i,j,k), cost);
		done
	      done
	    done;


	  for lvl = 1 to filtration.depth-1 do 
	    printf "true bounds tnum=%d, lvl=%d\n" tnum lvl;
	    let lbounds_true_this = lbounds_true >> (tnum,lvl) in
	      Hashtbl.iter
		begin fun (i,j,k) cost ->
		  let i2 = filtration.maps.(lvl-1).(i) in
		  let j2 = filtration.maps.(lvl-1).(j) in
		  let k2 = filtration.maps.(lvl-1).(k) in
		    if cost < (lbounds_true_this >>! ((i2,j2,k2),infinity)) then begin 
		      lbounds_true_this << ((i2,j2,k2), cost);
		    end
		end
		(lbounds_true >> (tnum,lvl-1));
	  done;

	  for lvl = 2 to filtration.depth-1 do
	    printf "checking bounds tnum=%d, lvl=%d\n" tnum lvl;
	    let lbounds_est_this = lbounds_est >> (tnum,lvl) in
	    let lbounds_true_this = lbounds_true >> (tnum,lvl) in
	      Hashtbl.iter
		begin fun (i,j,k) estcost ->
		  let truecost = lbounds_true_this >> (i,j,k) in
		    printf "est=%0.3f, true=%0.3f\n%!" estcost truecost;
		    try
		      assert(estcost <= truecost);
		    with Assert_failure _ ->
		      let p,q,r = pts.(i), pts.(j), pts.(k) in
		      let shape = Shape.shape_of_ints_bme p q r in
			printf "model: ";
			Geometry.print_cpt (Shape.get_pt model.Watson.mean);
			printf "shape: ";
			Geometry.print_cpt (Shape.get_pt shape);
			printf "tnum = %d\n%!" tnum;
		      printf "lvl=%d,i=%d,j=%d,k=%d\n%!" lvl i j k;
		      for i=1 to 10 do 
		      bound_shape_cost2 lvl (p,q,r) model true;
		      done;
		      assert(estcost <= truecost);		      
		end
		lbounds_est_this;
	  done;

      end
      tmodels;

    ()
