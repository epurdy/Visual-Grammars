(** Making the sigma depend on the position of the point *)

open Util.Misc
open Geometry
open Util.Cops
open Printf
module C = Complex
open Bounds

let log2pi = log (2. *. Con.pi) 

(* let negloggauss2 sigma x mean = *)
(*   let sigma2 = sigma *. sigma in *)
(*     ((dist2 mean x) /. (2. *. sigma2)) *)
(*     +. log2pi +. log sigma2 *)

(* let negloggauss2_variant sigma x mean =  *)
(*   let factor = max (0.1) (abs_float mean.C.im) in *)
(*   let sigma = factor *. sigma in *)
(*   let sigma2 = sigma *. sigma in *)
(*     ((dist2 mean x) /. (2. *. sigma2)) *)
(*     +. log2pi +. log sigma2 *)

let nl_watson_const sigma =
  log 2. +. 2. *. log Con.pi -. log sigma +.
    sigma
(*    log ((exp sigma) -. 1.) *)

let nl_watson sigma x mean =
  let sigma = sigma /. 10. in
  let conc = 1. /. (2. *. sigma *. sigma) in
  let canon a = center_and_scale (c0, a, c1) in
  let z0,z1,z2 = canon x in
  let m0,m1,m2 = canon mean in
  let dprod = (~& z0) *& m0 +& (~& z1) *& m1 +& (~& z2) *& m2 in
    (nl_watson_const conc) -. conc *. (C.norm2 dprod)

let nl_base_distro sigma x mean =
  nl_watson sigma x mean

(** ********************************************************************)
(** DENSITIES                                                          *)
(** ********************************************************************)

type density = float array array

let init_density granularity =
  let size = granularity + 1 in
  let density = 
    Array.init size 
      begin fun i -> 
	Array.init size 
	  (fun j -> neg_infinity)
      end      
  in
    density
      

(** ********************************************************************)
(** MODELS                                                             *)
(** ********************************************************************)

(** density will be of size (granularity + 1) x (granularity + 1) *)
type model = 
    {centers : cpt array;
     nl_weights : float array;
     nl_weights_cdf : float array; (* allows efficient sampling *)
     sigma : float;
     granularity : int;
     bounds : bounds;
     baseline_mean : cpt;
     mutable baseline_sigma : float;
     baseline_weight : float;
     density : density}


let either_lookup_neglog_density model pt laziness =
  try
    let xrel, yrel = bounds_to_array_strict model.bounds model.granularity pt in
    (* let _ = printf "xrel,yrel = %d,%d\n%!" xrel yrel in *)
    let rv = model.density.(xrel).(yrel) in
      if rv = neg_infinity then begin
	if laziness then
	  neg_infinity
	else
	  let tot = ref infinity in
	    for i=0 to (Array.length model.centers)-1 do
	      let nlwt = model.nl_weights.(i) in
	      let center = model.centers.(i) in
	      let gval = nl_base_distro model.sigma pt center in
		tot := neglogadd !tot (nlwt +. gval);
	    done;
	    model.density.(xrel).(yrel) <- !tot;
	    !tot
      end      
      else
	rv
  with _ ->
    infinity

let lazy_lookup_neglog_density model pt = 
  either_lookup_neglog_density model pt false

let truly_lazy_lookup_neglog_density model pt =
  either_lookup_neglog_density model pt true

let force model =
  for i = 0 to model.granularity do
    for j = 0 to model.granularity do
      let pt = array_to_bounds model.bounds model.granularity (i,j) in
	ignore (lazy_lookup_neglog_density model pt)
    done;
  done

let draw_centers (model : model) : int Image.t =
  let fgranularity = float_of_int model.granularity  in
  let size = model.granularity + 1 in
  let im = Image.create size size 0 in
    Array.iter 
      begin fun pt ->
	match (map_to_unit_square model.bounds pt) with
	    Some relpt ->
	      let x,y = int_of_float (fgranularity *. relpt.C.re), 
		int_of_float (fgranularity *. relpt.C.im) in
		Image.set im 255 (x,y);
	  | None -> ()
      end
      model.centers;
    im

let log_cutoff = 100.


let draw_model (model : model) : Image.rgb Image.t =
  let size = model.granularity + 1 in
  let im = Image.create size size (0,0,0) in
  let minval = ref infinity in
    Array.iter 
      begin fun arr -> 
	Array.iter 
	  begin
	    fun x -> minval := min !minval x 
	  end 
	  arr 
      end
      model.density;
    for i = 0 to model.granularity do
      for j = 0 to model.granularity do
	if model.density.(i).(j) = neg_infinity then
	  Image.set im (255,200,200) (i,j)
	else
	  let value = (model.density.(i).(j) -. !minval) /. (log_cutoff -. !minval) in
	  let value = min value 1. in
	  let pixval = round (255. *. (1. -. value)) in
	  let pixval = (pixval,pixval,pixval) in
	    Image.set im pixval (i,j);
      done;
    done;

    (* draw special points *)
    let white = (255,255,255) in
    let mark p =
      let i,j = bounds_to_array_strict model.bounds model.granularity p in
	Image.set_many im white [ i,j; i-1,j; i+1,j; i,j-1; i,j+1 ];
    in
      mark {C.re=0.; C.im=0.};
      mark {C.re=1.; C.im=0.};

      im


let sample_pt model =  
  (* find smallest i such arr.(i) >= u *)
  let binsearch arr u =
    (* answer is in [lower, upper] *)
    let i = ref 0 in
    let lower, upper = ref 0, ref ((Array.length arr)-1) in
    let loop = ref true in
      while !loop do
	i := (!lower + !upper) / 2;
	if !lower = !upper then 
	  loop := false	  
	else begin
	  (* flipped from < because we're using negative log probabilities *)
	  if arr.(!i) > u then 
	    lower := !i+1
	  else  
	    upper := !i;
	end
      done;
      !i
  in
  let baseline_u = Random.float 1.0 in
    if baseline_u < model.baseline_weight then begin
      let epsilon = 
	{ C.re = Sample.gaussian model.baseline_sigma; 
	  C.im = Sample.gaussian model.baseline_sigma } in	
	model.baseline_mean +& epsilon
    end
    else begin
      let i = binsearch model.nl_weights_cdf (-. log (Random.float 1.0)) in
      let epsilon = 
	{ C.re = Sample.gaussian model.sigma; 
	  C.im = Sample.gaussian model.sigma } in
	model.centers.(i) +& epsilon
    end

let sample model =
  let pt = sample_pt model in
    Shape.shape_of_complex_bme c0 pt c1

let cost model shape =
  let pt = Shape.get_pt shape in
  let baseline_nlp = nl_base_distro model.baseline_sigma pt model.baseline_mean  in
  let parzen_nlp =  lazy_lookup_neglog_density model pt in
  let rv = neglogadd ((-. log model.baseline_weight) +. baseline_nlp)
    ((-.log(1. -. model.baseline_weight)) +. parzen_nlp) in

  (* scale correction - can think of it as getting the right
     normalization constant for sigma_scaled = scale sigma_nominal *)
  let scale = Shape.get_scale shape in
    rv (* +. 2. *. (log scale) *)

(* let cost_lazy model shape = *)
(*   let pt = Shape.get_pt shape in *)
(*   let baseline_nlp = negloggauss2 model.baseline_sigma pt model.baseline_mean  in *)
(*   let parzen_nlp =  truly_lazy_lookup_neglog_density model pt in *)
(*     if parzen_nlp = neg_infinity then *)
(*       neg_infinity *)
(*     else *)
(*       let rv = neglogadd ((-. log model.baseline_weight) +. baseline_nlp) *)
(* 	((-.log(1. -. model.baseline_weight)) +. parzen_nlp) in *)

(*       (\* scale correction - can think of it as getting the right *)
(* 	 normalization constant for sigma_scaled = scale sigma_nominal *\) *)
(*       let scale = Shape.get_scale shape in *)
(* 	rv +. 2. *. (log scale) *)


let draw_points (model : model) : int Image.t =
  let size = model.granularity + 1 in
  let fgranularity = float_of_int model.granularity  in
  let im = Image.create size size 0 in
    Array.iter 
      begin fun pt ->
	let relpt = map_to_unit_square defaultbounds pt in
	  begin match relpt with 
	      None -> ()
	    | Some relpt ->
		let x,y = int_of_float (fgranularity *. relpt.C.re), 
		  int_of_float (fgranularity *. relpt.C.im) in
		  Image.set im 255 (x,y);
	  end
      end
      model.centers;
    im

let make_model_neglog_weighted nlwt_centers sigma granularity (baseline_mean, baseline_sigma) baseline_weight truncate =
  let total_mass = 
    if truncate then 0.9 
    else 1.0
  in

  (* sort in increasing order by negative log count to get highest counts *)
  let nlwt_centers = Array.to_list nlwt_centers in
  let nlwt_centers = List.sort (fun (w1, p1) (w2, p2) -> compare w1 w2) nlwt_centers in  
  let nlwt_centers = Array.of_list nlwt_centers in
  let nl_totwt = Array.fold_left (fun tot (w,p) -> neglogadd tot w) infinity nlwt_centers in
  let nl_tgt_totwt = (-. log total_mass) +. nl_totwt in

  let stuff = ref [] in
  let nltot = ref infinity in
  let finger = ref 0 in
    while !nltot > nl_tgt_totwt do (* while total < totwt *)
      let (nlw,p) = nlwt_centers.(!finger) in
	nltot := neglogadd !nltot nlw;
	stuff := (nlw,p,!nltot) :: !stuff;
	incr finger;
    done;
    let stuff = Array.of_list (List.rev !stuff) in

    let nl_weights = Array.map (fun (nlw,_,_) -> nlw -. !nltot) stuff in
    let nl_weights_cdf = Array.map (fun (_,_,nl_cum) -> nl_cum -. !nltot) stuff in
    let centers = Array.map (fun (_,p,_) -> Shape.get_pt p) stuff in

(*       printf "MMNW: ncenters = %d/%d\n%!" (Array.length centers) (Array.length nlwt_centers);    *)

      let density = init_density granularity in
      let rv = {
	centers = centers;
	nl_weights = nl_weights;
	nl_weights_cdf = nl_weights_cdf;
	sigma = sigma;
	granularity = granularity;
	bounds = defaultbounds;
	baseline_mean = Shape.get_pt baseline_mean;
	baseline_sigma = baseline_sigma;
	baseline_weight = baseline_weight;
	density = density;
      } in
	rv

let make_model centers sigma granularity (baseline_mean, baseline_sigma) baseline_weight truncate = 
  let nlwt_centers = Array.map (fun p -> 0.0, p) centers in
    make_model_neglog_weighted nlwt_centers sigma granularity (baseline_mean, baseline_sigma) 
      baseline_weight truncate

let new_straight_model sigma = 
  make_model [||] Con.default_sigma Con.default_granularity 
    ((* Con.default_baseline_mean *) Shape.default_shape , sigma)
    1.0 false

let string_of_model model = sprintf
  "Parzen< %f nonparametric, %f baseline=[%s,%f]>"
  (1. -. model.baseline_weight)
  model.baseline_weight
  (Geometry.string_of_cpt model.baseline_mean)
  model.baseline_sigma

let print_model model =
  printf "%s\n%!" (string_of_model model)
