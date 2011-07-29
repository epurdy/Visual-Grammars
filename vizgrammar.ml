open Util.Misc
open Abstract
open Grammar

let prod_cost comp shape =
  match comp.cdata.geom with
      Improper ->
	comp.cdata.cost
    | Parzen model ->
	comp.cdata.cost +. (Parzen.cost model shape)


let draw_prod_curves im bds granularity prod =
  let draw_scurve prod im scurve value =
    for i = 0 to (Array.length scurve) - 2 do
      let p1 = Bounds.bounds_to_array_nice bds granularity scurve.(i) in
      let p2 = Bounds.bounds_to_array_nice bds granularity scurve.(i+1) in
	Draw.draw_line im p1 p2 value;
    done
  in
    draw_scurve prod im prod.cdata.lcurve (255,0,0);
    draw_scurve prod im prod.cdata.rcurve (0,255,0);
    draw_scurve prod im prod.cdata.ocurve (0,255,255)

let log_cutoff = 100.

let draw_production_standard prod =
  let granularity = 100 in
  let size = granularity + 1 in
  let im = Image.create size size 0. in
  let lcurve, rcurve, ocurve = prod.cdata.lcurve, prod.cdata.rcurve, prod.cdata.ocurve in
  let bds = Bounds.nice_curve_bounds (Array.concat [lcurve; rcurve; ocurve]) in
  let p,q = lcurve.(0), rcurve.((Array.length rcurve)-1) in
  let mincost = ref infinity in
  let maxcost = ref neg_infinity in
  let _ = 
    for x = 0 to granularity do
      for y = 0 to granularity do
	let z = Bounds.array_to_bounds bds granularity (x,y) in
	let shape = Shape.shape_of_complex_bme p z q in
	let cost = prod_cost prod shape in
	  mincost := min !mincost cost;
	  maxcost := max !maxcost cost;
	  Image.set im cost (x,y);
      done
    done;
  in
  let im = Image.map
    begin fun cost ->
      let value = (cost -. !mincost) /. (!maxcost -. !mincost) in
      let value = value ** 0.5 in
	(* let value = (cost -. !mincost) /. (log_cutoff -. !mincost) in *)
	(* let value = cost /. log_cutoff in  *)
      let value = min value 1. in
      let pixval = round (255. *. (1. -. value)) in
	Image.color_of_gray pixval
    end
    im
  in
    draw_prod_curves im bds granularity prod;
    im

let draw_grammar_best_rules namer gram =
  let stuff = sample_counts gram in
    Array.iteri
      begin fun rank (count, cid) -> 
	let im = ref None in
	  iter_all_compositions gram
	    begin fun prod ->
	      if prod.cid = cid then
		im := Some (draw_production_standard prod);
	    end;
	  (* save it with an appropriate name - best.RANK.COUNT.PID.ppm, eg *)
	  Pnm.save_ppm (get !im) (namer rank count cid);
      end
      (Array.of_list stuff)


let draw_grammar namer gram =
  iter_all_compositions gram
    begin fun prod ->
      let im = draw_production_standard prod in
	Pnm.save_ppm im (namer prod.cid)
    end


