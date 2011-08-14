open Printf
open Util.Misc
open Util.Hops
open Graph

module I = Image

let dist2 (x1,y1) (x2,y2) = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)

type stuff = {
gradx: float Image.t;
grady: float Image.t;
mag: float Image.t;
maxmag: float;
}

let cheating_net_of_image curve im netname 
    gran maxlen 
    background_cost maxcost =
  let w,h = Image.width im, Image.height im in
  let size = min w h in
  let im = Image.sub im (0,0) (size,size) in

  let step = size / gran in
  let extra = size - step * gran in
  let im = Image.init (size+gran-extra) (size+gran-extra) 
    begin fun (x,y) ->
      if x >= size && y >= size then
	I.get im (size-1,size-1)
      else if x >= size then
	I.get im (size-1,y)
      else if y >= size then 
	I.get im (x,size-1)
      else
	I.get im (x,y)
    end
  in    
  let size = size+gran-extra in
  let step = size / gran in

  let foo (x,y) = (step*x,step*y) in
  let netim = Image.map Image.color_of_gray im in
  let net = Graph.new_live_graph () in

  let im = Image.map float_of_int im in

  let scalespacing = 2 in
  let scales = Array.init (gran/scalespacing) (fun i -> (i+1) * scalespacing) in
  let find_scale len =
    let besti = ref 0 in
    let bestdiff = ref infinity in
      Array.iteri
	begin fun i s ->
	  let s = float_of_int s in
	    if abs_float (s-.len) < !bestdiff then begin 
	      bestdiff := abs_float (s-.len);
	      besti := i;
	    end
	end
	scales;
      !besti
  in

  let stuff = Array.map
    begin fun scale ->
      printf "scale=%d\n%!" scale;
      let im = Filter.smooth Image.float_ops im (0.5*.(float_of_int scale)) in
      let mag___, gradx, grady = Canny.grad_and_mag im in
      let _ = Canny.nms_in_place mag___ gradx grady in 
      let spreader x = 
	Filter.smooth Image.float_ops x ((float_of_int step) *. 0.5) in
      let gradx, grady = spreader gradx, spreader grady in
      let maxmag = ref 0. in
      let totmag, num = ref 0., ref 0. in
      let mag = Image.init (Image.width gradx) (Image.height gradx)
	begin fun (x,y) ->
	  let gx = I.get gradx (x,y) in
	  let gy = I.get grady (x,y) in
	  let m = sqrt (gx *. gx +. gy *. gy) in
	    maxmag := max !maxmag m;
	    totmag := m +. !totmag;
	    num := !num +. 1.;
	    m
	end in

      let gradx = Image.map (fun x -> 1000. *. x *. !num /. !totmag) gradx in
      let grady = Image.map (fun x -> 1000. *. x *. !num /. !totmag) grady in
      let mag   = Image.map (fun x -> 1000. *. x *. !num /. !totmag) mag in

(*       let gradx = Image.map (fun x -> x /. !maxmag) gradx in *)
(*       let grady = Image.map (fun x -> x /. !maxmag) grady in *)
(*       let mag   = Image.map (fun x -> x /. !maxmag) mag in *)

	Pnm.save_pgm 
	  (Image.map (fun x -> int_of_float (255. *. x)) gradx)
	  (sprintf "experiments/4.images/image_parsing/output.d/gradx.%d.pgm" scale);
	Pnm.save_pgm
	  (Image.map (fun x -> int_of_float (255. *. x)) grady)
	  (sprintf "experiments/4.images/image_parsing/output.d/grady.%d.pgm" scale);
	Pnm.save_pgm
	  (Image.map (fun x -> int_of_float (255. *. x)) mag)
	  (sprintf "experiments/4.images/image_parsing/output.d/mag.%d.pgm" scale);

	{gradx = gradx;
	 grady = grady;
	 mag = mag;
	 maxmag = !maxmag;
	}
    end
    scales
  in

  let alpha_of_cost x = 
    let maxviscost = 1.1 *. maxcost in
    let x = max x 0. in
    let x = min x maxviscost in
    let alpha = 1. -. (x /. maxviscost) in
      alpha (* *. alpha *. alpha  *)
  in

    (* make grid points *)
    for x = 0 to gran-1 do
      for y = 0 to gran-1 do
	ignore (Graph.add_vertex net (foo (x,y)));
	Image.set netim (0,255,0) (x*step,y*step);
      done;
    done;

    let usedup = mkhash 100 in

    let get_gradient p q =
      let (px,py), (qx,qy) = p, q in 
      let dx, dy = float_of_int (qx-px), float_of_int (qy-py) in
      let norm = sqrt (dx *. dx +. dy *. dy) in
      let len = norm /. (float_of_int step) in
      let iscale = find_scale len in
      let dx, dy = dx /. norm, dy /. norm in

      let line = Array.of_list (Draw.line p q) in
      let tot, maskedtot, num = ref 0., ref 0., ref 0. in
	Array.iteri
	  begin fun i (x,y) ->
	    if i+1 < Array.length line then begin
	      let gradx, grady, mag = 
		I.get stuff.(iscale).gradx (x,y), 
		I.get stuff.(iscale).grady (x,y), 
		I.get stuff.(iscale).mag (x,y) in
	      let cross = (dy *. gradx -. dx *. grady) in
	      let cos2 = (cross *. cross) /. (mag *. mag) in
		(* cosine of angle between segment and what
		   gradient wants (which is 90 deg off of
		   gradient) *)
		if cos2 > 0.5 then begin (* |cos| > rt(2)/2, 45 degrees either way. *)
		  (* 		  if cos2 > 0.75 then begin (\* |cos| > rt(3)/2, 30 degrees either way. *\) *)
		  (* 		if cos2 > 0.99 then begin (\* |cos| > 0.1, ~6 degrees either way *\) *)
		  tot := !tot +. cross; (* ...was |cos|^3 * |grad| *)
		  let x,y = step*(x/step), step*(y/step) in
		  let x2,y2 = line.(i+1) in
		  let x2,y2 = step*(x2/step), step*(y2/step) in
		    if not (usedup >>? ((x,y), (x2,y2), iscale)) then begin
		      maskedtot := !maskedtot +. cross;
		    end
		end;

		num := !num +. 1.;
	    end;
	  end
	  line;
	!tot, !maskedtot, !num
    in

    let insert_edge p q cost = 
      Graph.add_edge_strict net p q cost;
      let alpha = alpha_of_cost cost in 
	if alpha > 0.01 then begin 
	  let line = Draw.line p q in
	    List.iter
	      begin fun pt -> 
		let (cr,cg,cb) = I.get netim pt in
		  if (cr,cg,cb) != (0,255,0) then begin
		    let color = 
		      (int_of_float
			 ((float_of_int cr) *. (1.-.alpha) +.
			    255. *. alpha
			 ),
		       int_of_float
			 ((float_of_int cg) *. (1.-.alpha)),
		       int_of_float
			 ((float_of_int cb) *. (1.-.alpha)))
		    in
		      I.set netim color pt;
		  end
	      end
	      line;
	end
    in


    let cost_of_grad grad = 
      (* 	    let cost = background_cost ** (1. -. (10. *. grad)) in *)
      let cost = background_cost *. (1. -. grad) in
      let cost = max cost 1. in
	cost
    in	    

      (* the cheating! *)
    let nc = Array.length curve in 
    let _ = 
      for i=0 to nc-1 do 
	let p,q = curve.(i), curve.((i+1) mod nc) in
	let bestp, bestq, bestpdist, bestqdist = 
	  ref None, ref None, ref infinity, ref infinity in
	  Hashtbl.iter
	    begin fun v i ->
	      if float_of_int (dist2 p v) < !bestpdist then begin
		bestp := Some v;
		bestpdist := float_of_int (dist2 p v);
	      end;
	      if float_of_int (dist2 q v) < !bestqdist then begin
		bestq := Some v;
		bestqdist := float_of_int (dist2 q v);
	      end
	    end
	    net.lvertices;

	  let p,q = get !bestp, get !bestq in

	  let tot,maskedtot,num = get_gradient p q in	  
	    insert_edge p q (cost_of_grad ((abs_float tot) /. num));
	    insert_edge q p (cost_of_grad ((abs_float tot) /. num));
      done;
    in      

    let progress = ref true in
    let quals = mkhash 1000 in
    let leftquals = mkhash 1000 in 
      (* leftquals >> p is an upper bound on the quality of pq for any q *)

      (*       for i=1 to 1000 do  *)
      (* 	if !progress then begin *)
      while !progress do
	progress := false;
	printf ".%!";
	let bestqual, bestgrad, bestpair = ref 0., ref 0., ref None in

	  progress := false;
	  for x1 = 0 to gran-1 do
	    for y1 = 0 to gran-1 do
	      if (leftquals >>! ((x1,y1), infinity)) > !bestqual then begin
		if leftquals >>? (x1,y1) then
		  leftquals <<- (x1,y1);

		for x2 = 0 to gran-1 do
		  for y2 = 0 to gran-1 do
		    if (quals >>! (((x1,y1), (x2,y2)), infinity)) > !bestqual then begin
		      if dist2 (x1,y1) (x2,y2) < maxlen * maxlen then begin

			let p, q = foo (x1,y1), foo (x2,y2) in
			let tot,maskedtot,num = get_gradient p q in
			let qual = ((abs_float maskedtot)) /. (num) in
			  quals << (((x1,y1), (x2,y2)), qual);
			  if qual > !bestqual then begin 
			    bestqual := qual;
			    bestgrad := (abs_float tot) /. num;
			    bestpair := Some (p,q);
			  end;
			  if qual > (leftquals >>! ((x1,y1),-.1.)) then begin
			    leftquals << ((x1,y1), qual);
			  end
		      end
		    end
		  done;
		done;
	      end;
	    done;
	  done;


	  if !bestpair <> None then begin
	    let cost = cost_of_grad !bestgrad in

	    let p,q = get !bestpair in
	    let len = sqrt (float_of_int (dist2 p q)) in
	    let len = len /. (float_of_int step) in
	    let iscale = find_scale len in
	      printf "%f -> %f\n" !bestgrad cost;

	      if (net.ledges >>! ((net.lvertices >> p, 
				   net.lvertices >> q), 
				  0.99 *. maxcost)) > cost
	      then begin
		insert_edge p q cost;
		insert_edge q p cost;
		let line = Array.of_list (Draw.line p q) in
		  Array.iteri
		    begin fun i (x,y) -> 
		      if i+1 < Array.length line then begin
			let x,y = step*(x/step), step*(y/step) in
			let x2,y2 = line.(i+1) in
			let x2,y2 = step*(x2/step), step*(y2/step) in
			  usedup << (((x,y),(x2,y2), iscale), ());
		      end
		    end
		    line;
		  progress := true;
	      end
	      else begin 
		printf "no more desirable edges\n";
	      end
	  end
	    (* end (\* end if progress *\) *)
      done;

      (*       let default_cost = maxcost in *)

      (*       	for x1=0 to gran-1 do *)
      (*       	  for y1=0 to gran-1 do *)
      (*       	    for x2= (max 0 (x1-2)) to (min (gran-1) (x1+2)) do *)
      (*       	      for y2= (max 0 (y1-2)) to (min (gran-1) (y1+2)) do *)
      (*       		if dist2 (x1,y1) (x2,y2) < 2*2 then begin *)
      (*       		  let p = (x1*step, y1*step) in *)
      (*       		  let q = (x2*step, y2*step) in *)
      (*       		    if net.ledges >>! ((net.lvertices >> p, *)
      (*       					net.lvertices >> q), *)
      (*       				       infinity) > default_cost then begin *)
      (*       		      insert_edge p q default_cost; *)
      (*       		    end *)
      (*       		end *)
      (*       	      done *)
      (*       	    done *)
      (*       	  done *)
      (*       	done; *)

      Pnm.save_ppm netim netname;
      printf "Finished building network!\n%!";
      fprintf stderr "Finished building network!\n%!";
      Graph.finalize net

let _ = 

  let granularity = ref 32 in
  let maxlen = ref 10 in

  let background_cost = ref 1000. in
  let maxcost = ref 100. in

  let input = ref "" in
  let netname = ref "" in
  let gfname = ref "" in

  let thecurve = ref "" in

  (* parse args *)
  let _ = Arg.parse [
    "-granularity", Arg.Set_int granularity, "Granularity";
    "-maxlen", Arg.Set_int maxlen, "Maximum length of background segments.";

    "-backgroundcost", Arg.Set_float background_cost, "";
    "-maxcost", Arg.Set_float maxcost, "";

    "-input", Arg.Set_string input, "";
    "-netname", Arg.Set_string netname, "";
    "-gfname", Arg.Set_string gfname, "";

    "-curve", Arg.Set_string thecurve, "";
  ]
    (placeholder "Arg.parse")
    ("./prog -gramfile GRAMFILE -dir DIR -title TITLE [-nsamples N] [-rules [-midpoints]]")
  in

  let im = Pnm.load_pgm !input in
  let curve = Curve.load !thecurve in
  let curve = Array.map Geometry.point_of_complex curve in
  let net = 
    cheating_net_of_image curve im !netname
      !granularity !maxlen
      !background_cost !background_cost (* !maxcost *)
  in
    Graph.save_point_float_graph net !gfname


