open Printf
open Util.Hops
open Util.Cops
open Filtration
open Abstract
open Graph
open Bounds
open Models.Simple
open Grammar

module B = Bloom
module C = Complex

let foi x = float_of_int x

let move_pt shape p q r qbounds =
  let q = Shape.place_unsafe shape p r in
  let q = Bounds.move_into_bounds qbounds q in
    q

let outline im pt len color =
  let (x,y) = pt in
    Draw.draw_line im (x,y) (x+len-1,y) color;
    Draw.draw_line im (x,y) (x,y+len-1) color;
    Draw.draw_line im (x+len-1,y) (x+len-1,y+len-1) color;
    Draw.draw_line im (x,y+len-1) (x+len-1,y+len-1) color;
    ()

let fatpt im color (x,y) = 
  I.set im color (x,y);  
  I.set im color (x+1,y);  
  I.set im color (x,y+1);  
  I.set im color (x+1,y+1)

let draw_shape im size shape =
  let x,y = size/2, size/2 in
  let fsize = foi size in
  let fx, fy = foi x, foi y in
  let scale, cscale = 10, cxre 10. in
  let z = Shape.place_unsafe shape {C.re=fx;C.im=fy} {C.re=fx+.10.;C.im=fy} in
  let z = Bounds.move_into_bounds {xmin=0.;xmax=fsize;ymin=0.;ymax=fsize} z in
    fatpt im (255,0,0) (x,y);
    fatpt im (0,255,0) (Geometry.point_of_complex z);
    fatpt im (0,0,255) (x+scale,y);
    ()

let print_pt = Geometry.print_cpt

let verbose = false
let bsim = I.create 1 1 (0,0,0)

let calc_bounds_fake gram filt = 
  None

let calc_bounds gram filt =
  let size = 60 in
  let topids = top_level_ids filt.levels.(filt.depth-1) in  
  let ntopids = Array.length topids in
  let fpoints = Array.map (fun (x,y) -> (foi x, foi y)) filt.points in
  let isidelens = Array.map (fun rk -> 1 lsl rk) filt.levels.(filt.depth-1).ranks in
  let sidelens = Array.map foi isidelens in
  let bounds = Array.mapi 
    begin fun i (x,y) -> 
      {xmin=x; xmax=x+.sidelens.(i);
       ymin=y; ymax=y+.sidelens.(i);}
    end
    fpoints 
  in
  let fpoints = Array.mapi (fun i (x,y) -> (x+.sidelens.(i)/.2., y+.sidelens.(i)/.2.)) fpoints in
  let fpoints = Array.map (fun (x,y) -> {C.re=x;C.im=y}) fpoints in

(*   let best_tri_cost = mkhash *)
(*     (min ((Frozen.num_compositions gram)*ntopids*ntopids*ntopids)  *)
(*        100000) *)
(*   in *)
  let best_tri_cost = Bloom.new_bloom 1000 6 
    (fun (cid,pid,qid,rid) -> (cid+1,pid,qid,rid)) in
    iter_all_compositions gram
      begin fun prod ->
	printf "i=%d/%d\n%!" prod.cid (Frozen.num_compositions gram);
	match prod.cdata.geom with 
	    Improper -> 
	      for i = 0 to ntopids-1 do 
		for j = 0 to ntopids-1 do 
		  for k = 0 to ntopids-1 do 
		    let pid,qid,rid = topids.(i), topids.(j), topids.(k) in
(* 		      best_tri_cost << ((prod.cid,pid,qid,rid), 0.); *)
		      B.insert best_tri_cost (prod.cid,pid,qid,rid) 0.;
		  done
		done
	      done	      

	  | Watson watson ->

	      let pqr_shape = watson.Watson.mean in
	      let q0 = Shape.place_unsafe pqr_shape c0 c1 in
	      let rpq_shape = Shape.shape_of_complex_bme c1 c0 q0 in
	      let qrp_shape = Shape.shape_of_complex_bme q0 c1 c0 in

		for i = 0 to ntopids-1 do 
		  for j = 0 to ntopids-1 do 
		    for k = 0 to ntopids-1 do 
		      let pid,qid,rid = topids.(i), topids.(j), topids.(k) in
		      let p = ref fpoints.(pid) in
		      let q = ref fpoints.(qid) in
		      let r = ref fpoints.(rid) in
		      let pbounds, qbounds, rbounds = bounds.(pid), bounds.(qid), bounds.(rid) in
		      let im = 
			if verbose then Image.create (size+1) (size+1) (0,0,0) 
			else bsim
		      in
			if verbose then begin
			  outline im filt.points.(pid) isidelens.(pid) (255,128,128);
			  outline im filt.points.(qid) isidelens.(qid) (128,255,128);
			  outline im filt.points.(rid) isidelens.(rid) (128,128,255);
			end;
			for iter = 0 to 3 do 
			  if verbose then begin
			    I.set im (100 + 50*i,0,0) (Geometry.point_of_complex !p);
			    I.set im (0,100 + 50*i,0) (Geometry.point_of_complex !q);
			    I.set im (0,0,100 + 50*i) (Geometry.point_of_complex !r);
			  end;
			  p := move_pt rpq_shape !r !p !q pbounds;
			  q := move_pt pqr_shape !p !q !r qbounds;
			  r := move_pt qrp_shape !q !r !p rbounds;
			done;
			if verbose then begin
			  draw_shape im size pqr_shape;
			  Pnm.save_ppm im (sprintf "traj.%d.%d.%d.ppm" i j k);
			  printf "---\n";
			  printf "orig: \n";
			  print_pt fpoints.(pid);
			  print_pt fpoints.(qid);
			  print_pt fpoints.(rid);
			  printf "new: \n";
			  print_pt !p;
			  print_pt !q;
			  print_pt !r;
			  printf "model: ";
			  print_pt q0;
			  printf "orig bk: \n";
			  print_pt (Shape.place_unsafe (Shape.shape_of_complex_bme fpoints.(pid) fpoints.(qid) fpoints.(rid)) c0 c1);
			  printf "new bk: \n";
			  print_pt (Shape.place_unsafe (Shape.shape_of_complex_bme !p !q !r) c0 c1);
			  printf "---\n";
			end;

			let finshape = Shape.shape_of_complex_bme !p !q !r in
			let cost = Models.Simple.prod_cost prod finshape in

(* 			  best_tri_cost << ((prod.cid,pid,qid,rid), cost); *)
			  B.insert best_tri_cost (prod.cid,pid,qid,rid) cost;
		    done
		  done
		done
      end;
    best_tri_cost

(*  
let _ = 
  let excurve = Curve.load "romer/newann/IMG0020.curve" in
  let sdf = Sdf.load_family "romer/misc/romer1.sdf" in
  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = Grammar.merge_leaves gram in
  let _ = reorder_symbols gram in

  let nlvls = 4 in
  let size = 60 in

  let net = Gridparsing.make_net size size in
  let filt = make_filtration net.vertices nlvls in
  let foo = calc_bounds gram filt in

    ()
*)
      
