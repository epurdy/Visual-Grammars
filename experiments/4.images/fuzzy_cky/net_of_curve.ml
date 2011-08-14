open Printf
open Util.Misc
open Util.Hops
open Graph

let dist2 (x1,y1) (x2,y2) = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)

let net_of_curve c imname
    gran size maxlen
    curve_cost background_cost maxcost =

  let step = size / gran in
  let foo (x,y) = (step*x,step*y) in
  let im = Image.create (size+1) (size+1) 255 in
  let net = Graph.new_live_graph () in
  let c = Curve.normalize ~scale:(float_of_int gran) c in
  let c = Array.map Geometry.point_of_complex c in
  let c = Array.map foo c in
  let c = Curve.uniqify c in
  let n = Array.length c in

  let missing = Array.init n (fun x -> false) in
    (*   let num_missing = ref num_missing in *)
    (*   let num_noise = ref num_noise in *)

  let curve_cost_distro () = 
    let sigma = 1.0 *. (log 10.) in
    let epsilon = Sample.gaussian sigma in
      curve_cost *. (exp epsilon)
  in
  let background_cost_distro () = 
    let sigma = 0.5 *. (log 10.) in
    let epsilon = Sample.gaussian sigma in
      background_cost *. (exp epsilon)
  in

  let color_of_cost x = 
    let maxviscost = 30. in
    let x = max x 0. in
      if x > maxviscost then 
	None
      else
	Some (int_of_float (255. *. ( (x /. maxviscost))))
  in

    (* make grid points *)
    for x = 0 to gran-1 do
      for y = 0 to gran-1 do
	ignore (Graph.add_vertex net (foo (x,y)));
	Image.set im 0 (x*step,y*step);
      done;
    done;

    (*     (\* pick some segments to drop *\) *)
    (*     while !num_missing > 0 do  *)
    (*       let i = Random.int n in *)
    (* 	if missing.(i) = false then begin  *)
    (* 	  missing.(i) <- true; *)
    (* 	  decr num_missing; *)
    (* 	end *)
    (*     done; *)

    (*     (\* add in some random segments with cheap edges *\) *)
    (*     while !num_noise > 0 do  *)
    (*       let p = foo (Random.int gran, Random.int gran) in *)
    (*       let q = foo (Random.int gran, Random.int gran) in *)
    (* 	if dist2 p q < step * step * maxlen_noise * maxlen_noise then begin *)
    (* 	  Graph.add_edge_strict net p q 1.; *)
    (* 	  Draw.draw_line im p q 100; *)
    (* 	  decr num_noise; *)
    (* 	end *)
    (*     done; *)

    (* put cheap edges for undropped segments in graph *)
    for i = 0 to n-1 do 
      if not missing.(i) then begin
	let cost = curve_cost_distro () in
	  assert( (dist2 c.(i) c.((i+1) mod n)) <=
		    step * step * maxlen * maxlen);
	  Graph.add_edge_strict net c.(i) c.((i+1) mod n) cost;
	  let color = color_of_cost cost in 
	    if color != None then 
	      Draw.draw_line im c.(i) c.((i+1) mod n) (get color);
      end
      else begin 
	Draw.draw_line im c.(i) c.((i+1) mod n) 200;
      end
    done;

    (* add in expensive edges everywhere *)
    for x1 = 0 to gran-1 do
      for y1 = 0 to gran-1 do
	for x2 = 0 to gran-1 do
	  for y2 = 0 to gran-1 do
	    if dist2 (x1,y1) (x2,y2) < maxlen * maxlen then begin
	      let p, q = foo (x1,y1), foo (x2,y2) in
	      let cost = background_cost_distro () in
		if (net.ledges >>! ((net.lvertices >> p, 
				     net.lvertices >> q), 
				    maxcost)) > cost
		then begin
		  Graph.add_edge_strict net p q cost;
		  let color = color_of_cost cost in 
		    if color != None then 
		      Draw.draw_line im p q (get color);

		end
	    end
	  done;
	done;
      done;
    done;

    Pnm.save_pgm im imname;
    printf "Finished building network!\n%!";
    Graph.finalize net



let _ = 

  let granularity = ref 32 in
  let size = ref 256 in
  let maxlen = ref 10 in

  let curve_cost = ref 1. in
  let background_cost = ref 1000. in
  let maxcost = ref 100. in

  let curvename = ref "" in
  let imname = ref "" in
  let gfname = ref "" in

  (* parse args *)
  let _ = Arg.parse [
    "-granularity", Arg.Set_int granularity, "Granularity";
    "-size", Arg.Set_int size, "";
    "-maxlen", Arg.Set_int maxlen, "Maximum length of background segments.";

    "-curvecost", Arg.Set_float curve_cost, "";
    "-backgroundcost", Arg.Set_float background_cost, "";
    "-maxcost", Arg.Set_float maxcost, "";

    "-curve", Arg.Set_string curvename, "";
    "-imname", Arg.Set_string imname, "";
    "-gfname", Arg.Set_string gfname, "";
  ]
    (placeholder "Arg.parse")
    ("./prog -gramfile GRAMFILE -dir DIR -title TITLE [-nsamples N] [-rules [-midpoints]]")
  in

  let curve = Curve.load !curvename in    
  let net = 
    net_of_curve curve !imname 
      !granularity !size !maxlen
      !curve_cost !background_cost !maxcost
  in
    
    Graph.save_point_float_graph net !gfname


