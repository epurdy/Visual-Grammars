open Util.Misc
open Util.Hops
open Printf
module C = Complex
open Util.Cops
open Abstract
open Grammar
open Sdf
open Models.Simple

let make_sdf curves = 
  let base = curves.(0) in
  let idx_curves = 
    Array.map
      begin fun curve ->
	Array.map
	  begin fun p ->
	    let ip = find p base in
	      ip
	  end
	  curve
      end
      curves
  in

  (* this code is a beginning to the adaptive sdf idea. Should think
     about adding as few midpoints as possible. Instead of adding all
     of them when we have a long segment, add only the ones that live
     inside the long segment. Think about what this does... *)
  let level = ref (Array.length idx_curves - 1) in
  let thecurve = idx_curves.(!level) in
  let n = Array.length base in
  let ncur = Array.length thecurve in
  let seglens = Array.init ncur
    begin fun i -> 
      let len = thecurve.((i+1) mod ncur) - thecurve.(i) in
      let len = ((n + len - 1) mod n) + 1 in
	printf "seglen( [%d,%d] ) = %d\n" thecurve.(i) thecurve.((i+1) mod ncur) len;
	len
    end 
  in

  (* this code is fucking around with the coarsest curve *)
  let mycurve = ref thecurve in
  let iter = ref 0 in
  let fnames = ref "" in
    while Array.length !mycurve > 3 do 
      let n = Array.length !mycurve in
      let smallest = ref infinity in
      let besti = ref 0 in
	for i=0 to n-1 do
	  let x,y,z = !mycurve.(i), !mycurve.((i+1)mod n), !mycurve.((i+2)mod n) in
	  let x,y,z = base.(x), base.(y), base.(z) in
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
	  printf "saving %s\n" fname;
	  Curve.save fname
	    (Array.map (fun i -> base.(i)) !mycurve);
	  fnames := !fnames ^ " " ^ fname;
	  mycurve := Array.of_list
	    (List.filter
	       begin fun p ->
(*		 (p != !mycurve.(!besti)) && *)
		   (p != !mycurve.((!besti + 1) mod n)) (* && *)
(*		   (p != !mycurve.((!besti + 2) mod n)) *)
	       end
	       (Array.to_list !mycurve));
	  incr iter;
    done;

    doit("./show_curves.native -fname tmp/decay.svg " ^ !fnames);

    exit 0;

    (* more adaptive sdf stuff here.*)
    let theta = 0.3 in
      for i = 0 to ncur-1 do
	for len = 1 to ncur do
	  let works = ref true in
	    for j = i to i+len-1 do 
	      if !works then begin
		let truelen = thecurve.((i+len) mod ncur) - thecurve.(i) in
		let truelen = ((n + truelen - 1) mod n) + 1 in
		let rel_len = ( (float_of_int seglens.(j mod ncur)) /. (float_of_int truelen) ) in
		  if rel_len > theta then begin 
		    printf "  [%d,%d] (level length %d) contains [%d,%d], with length %d, relative length %f\n%!" 
		      thecurve.(i) thecurve.((i+len) mod ncur) len 
		      thecurve.(j mod ncur)
		      thecurve.((j+1) mod ncur)
		      seglens.(j mod ncur)
		      rel_len
		    ;
		    printf "  [%d,%d] must be further broken up!\n%!" 
		      thecurve.(i) thecurve.((i+len) mod ncur); 
		    works := false;
		  end
	      end
	    done;
	    if !works then begin
	      printf "[%d,%d] works!\n%!" thecurve.(i) thecurve.((i+len) mod ncur);
	    end
	done
      done;
      
      exit 0
;;    

let _ =
  let fname = ref "foo.svg" in
  let curves = ref [] in

  let add_curve fname = 
    let c = Curve.load fname in
      curves := c :: !curves;
  in

  let _ = Arg.parse ["-out", Arg.Set_string fname, "File to write to.";]
    add_curve
    "./prog -out fname curve1 curve2 curve3..."
  in

    make_sdf (Array.of_list (List.rev !curves))
