open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Util.Cops
open Abstract
open Grammar
open Sdf
open Models.Simple

let pick_constituents curve = 
  let nn = Array.length curve in
  let fam = Sdf.make_live_family nn nn in
  let add_triple (p,q,r) = 
    let ip,iq,ir = find p curve, find q curve, find r curve in
    let len = (ir - ip + nn + 1) in
    let len = (len mod nn) - 1 in
      fam.imake_new_scurve (ip,ir) len;
      fam.imake_new_comp (ip,iq,ir)
  in

  let _ = 
    for i=0 to nn-1 do 
      fam.imake_new_scurve (i, (i+1) mod nn) 1;
    done;
  in

  let mycurve = ref curve in
  let iter = ref 0 in
  let fnames = ref "" in
    while Array.length !mycurve > 3 do
      let n = Array.length !mycurve in
      let smallest = ref infinity in
      let besti = ref 0 in
	for i=0 to n-1 do
	  let x,y,z = !mycurve.(i), !mycurve.((i+1)mod n), !mycurve.((i+2)mod n) in
	  let xy, xz = y-&x, z-&y in
	  let crossprod = xy.C.re *. xz.C.im -. xy.C.im *. xz.C.re in
	  let a,b,c = C.norm (y -& x), C.norm (z -& y), C.norm (x -& z) in
	  let s = (a +. b +. c) /. 2. in
	  let area = sqrt (s *. (s-.a) *. (s-.b) *. (s-.c)) in
	    (* 	    printf "area(%d,%d,%d) = %f\n" *)
	    (* 	      i ((i+1) mod n) ((i+2) mod n) area *)
	    (* 	    ; *)
	    if (crossprod <= 0.) && (area < !smallest) then begin
	      smallest := area;
	      besti := i;
	    end
	done;
	let fname = (sprintf "tmp/decay.%d.curve" !iter)  in
	let p,q,r = !mycurve.(!besti),
	  !mycurve.((!besti + 1) mod n),
	  !mycurve.((!besti + 2) mod n)
	in
	  add_triple (p,q,r);

	  printf "saving %s\n" fname;
	  Curve.save fname !mycurve;
	  fnames := !fnames ^ " " ^ fname;

	  mycurve := Array.of_list
	    (List.filter
	       begin fun x -> x <> q
	       end
	       (Array.to_list !mycurve));
	  incr iter;
    done;

    doit("./show_curves.native -fname tmp/decay.svg " ^ !fnames);

    let sdf = finalize fam.gram in
      Sdf.save_family sdf "tmp/decay.sdf";
      doit("./show_sdf.native -sdf tmp/decay.sdf -curve tmp/decay.0.curve -fname tmp/decay_sdf.svg");
;;    

let _ =
  let fname = ref "decay.svg" in
  let curves = ref [] in

  let add_curve fname = 
    let c = Curve.load fname in
      curves := c :: !curves;
  in

  let _ = Arg.parse ["-out", Arg.Set_string fname, "File to write to.";

		    ]
    add_curve
    "./prog -out fname curve1 curve2 curve3..."
  in

    pick_constituents (Array.of_list (List.rev !curves)).(0)
