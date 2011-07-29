open Util.Misc
open Printf
module C = Complex
open Util.Cops

open Abstract
open Sdf
open Grammar

let npoints = 30;;

let ntrain = 20	 (* 278 max for romer *);;
let ngrammars = 3;;
let k = 8;; (* approximation parameter *)
let k2 =8;;

let nsamples = 20;;
let niters = 5;;

let ncols = 5;;
let scale = 100.;;

let dir = "em.d";;

type loc_sdata = {
  scale_: float;
  first_: int;
  last_: int;
  len_: int;
}
type loc_cdata = {
  shape_: Shape.shape;
  bg_: int;
  md_: int;
  en_: int;
}

let choose_regular arr nchoice = 
  let spacing = (Array.length arr) / nchoice in
  let rv = ref [] in      
    for i = 0 to nchoice-1 do
      rv := arr.(i * spacing) :: !rv;
    done;
    Array.of_list (List.rev !rv);;

let choose_spaced arr nchoice = 
  let last = Array.length arr - 1 in
  let factor = (float_of_int last) /. (float_of_int (nchoice-1)) in
  let rv = ref [] in      
    for i = 0 to nchoice-1 do
      rv := arr.(round (float_of_int i *. factor)) :: !rv;
    done;
    Array.of_list (List.rev !rv);;

let make_grammar (c, fam) =
  let doubled = Array.append c c in
  let n = Array.length c in
  let fn = float_of_int n in
  let sdata_maker scurve = 
    let closed = (scurve.sdata.first = scurve.sdata.last) in
      if closed then
	None
      else begin
	let scale = float_of_int scurve.sdata.len /. fn in
	let straightcost = Con.spfactor *. scale *. scale in
	let straightprob = exp (-. straightcost) in
	  Some {closed = closed;
		straightprob = straightprob;
		straightcost = straightcost;
		curve = Array.sub doubled scurve.sdata.first (scurve.sdata.len + 1);}
      end
  in
  let cdata_maker scurve comp = 
    let scale = float_of_int scurve.sdata.len /. fn in
    let left, right = Frozen.get_rhs fam comp in
    let shape = Shape.shape_of_complex_bme  
      c.(comp.cdata.bg) c.(comp.cdata.md) c.(comp.cdata.en) in
    let olen = n - (left.sdata.len + right.sdata.len) in
      {prob = 1.;
       cost = 0.;
       geom = Watson {
	 Watson.mean = shape; 
	 Watson.conc_ = scale *. 10.;
       };
       lcurve = Array.sub doubled comp.cdata.bg (left.sdata.len+1);
       rcurve = Array.sub doubled comp.cdata.md (right.sdata.len+1);
       ocurve = Array.sub doubled comp.cdata.en (olen+1);
      }
  in
  let gram = Grammar.grammar_of_family fam {closed=true; straightprob=0.; straightcost=infinity; curve=c} sdata_maker cdata_maker in

    Frozen.iter_symbols gram
      begin fun sym ->
	if sym.dcompids = [] then begin 
	  printf "need to add LLL or something here\n";
	  exit 0;
	end
      end;

    gram

let add_curve_data_to_family curve fam =
  let n = Array.length curve in
    assert(n = fam.f_gdata.n);
    map_frozen_grammar fam
      (fun x -> x)
      begin fun scurve ->
	{sid = scurve.sid;
	 dcompids = scurve.dcompids;
	 lcompids = scurve.lcompids;
	 rcompids = scurve.rcompids;
	 startable = scurve.startable;
	 sdata = {
	   scale_ = (float_of_int scurve.sdata.len) /. (float_of_int n);
	   first_ = scurve.sdata.first;
	   last_ = scurve.sdata.last;
	   len_ = scurve.sdata.len;
	 }
	}
      end
      begin fun comp ->
	{cid = comp.cid;
	 topsid = comp.topsid;
	 leftsid = comp.leftsid;
	 rightsid = comp.rightsid;
	 cdata = {
	   shape_ = Shape.shape_of_complex_bme
	     curve.(comp.cdata.bg) 
	     curve.(comp.cdata.md) 
	     curve.(comp.cdata.en);
	   bg_ = comp.cdata.bg;
	   md_ = comp.cdata.md;
	   en_ = comp.cdata.en;
	 }
	}
      end

let prod_cost comp shape =
  match comp.cdata.geom with
      Improper ->
	comp.cdata.cost
    | Parzen model ->
	comp.cdata.cost +. (Parzen.cost model shape)

let sigma = Con.default_sigma
let baseline_sigma = Con.default_baseline_sigma

let strat = {
  Parsing.lexical_ok = (fun scurve -> (scurve.sdata.len_ = 1));
  Parsing.binary_ok = (fun scurve -> (scurve.sdata.len_ != 1));
  Parsing.goal_ok = (fun scurve -> scurve.sdata.first_ = scurve.sdata.last_);

  Parsing.lexical_cost = (fun sym scurve -> -. log sym.sdata.straightprob);
  Parsing.binary_cost = (fun comp dcomp -> prod_cost comp dcomp.cdata.shape_);
  Parsing.goal_cost = (fun gdata scurve -> log (float_of_int gdata.n));

  Parsing.compatible = (fun sym scurve -> sym.sdata.closed = scurve.startable);
  Parsing.getshape = (fun cdata -> cdata.shape_);

  Parsing.fit_midpoint_distro = begin fun prod samples themode ->
    let geom = Parzen.make_model_neglog_weighted
      (Array.of_list samples)
      sigma Con.default_granularity
      (begin match themode with
	   None -> Shape.default_shape
	 | Some shape -> shape
       end, baseline_sigma) Con.default_baseline_weight
      true
    in
      {
	geom = Parzen geom;
	prob = prod.cdata.prob;
	cost = prod.cdata.cost;
	ocurve = prod.cdata.ocurve;
	lcurve = prod.cdata.lcurve;
	rcurve = prod.cdata.rcurve;
      }
  end;

}

let _ = 
  (************)
  (* init SVG *)
  (************)
  let x = Svg.create_sized (sprintf "%s/samples.svg" dir)
    ((float_of_int ncols) *. 1.5 *. scale, 
     float_of_int (5 + (nsamples/ncols) * niters) *. 4.0 *. scale) in
  let _ = Cairo.scale x.Svg.ctx scale scale in

  (*********************)
  (* assemble examples *)
  (*********************)
  (*   let training = Datasets.get_hand () in *)
  (*   let examples = [| training.(0); |] in *)
  (*   let training = Array.append (Datasets.get_articulator ~n:10 ()) (Datasets.get_hand ~n:10 ()) in *)
  (*   let examples = [| training.(0); training.(10); training.(12) |] in *)

  let training = Datasets.get_romer ~n:20 () in
  let training = Array.map (fun c -> Curve.subsample_dp c npoints 2) training in
  let examples = [| training.(0); training.(1); training.(10); training.(11); |] in
    (*   let examples = choose_spaced training ngrammars in *)

  (*********************)
  (* construct grammar *)
  (*********************)
  let exfamilies = Array.map 
    begin fun c -> 
      c,
      Sdf.make_sparse_family (Array.length c) k 
	(*       Sdf.make_full_family (Array.length c) *)
    end 
    examples in

  let grams = Array.map make_grammar exfamilies in
  let gram = Grammar.merge_tops grams in

  (**************************)
  (* assemble training data *)
  (**************************)
  let trainfamilies = Array.map
    begin fun c -> 
      let fam = Sdf.make_sparse_family (Array.length c) k2 in
	(* 	let fam = Sdf.make_full_family (Array.length c) in *)
	add_curve_data_to_family c fam
    end 
    training in

  let _ = 
    let examples = Array.map Curve.normalize examples in
    let training = Array.map Curve.normalize training in
      Viz.show_samples x ("examples") ncols examples;
      Viz.show_samples x ("training data") ncols training
  in

  let logging i gram =
    printf "LOGGING %d\n%!" i;
    let p, q = c0, cxre 1000. in
    let samples = Array.init nsamples (fun i -> Grammar.sample gram p q) in

    let samples = Array.map Curve.normalize samples in

      Grammar.print_grammar gram;
      Viz.show_samples x (sprintf "round #%d" i) ncols samples;
      Viz.save_samples (dir) i samples;

      if (* i > 0 *) i = niters then begin
	printf "drawing grammar\n%!";
	Vizgrammar.draw_grammar (sprintf "%s/gram.%04d.ppm" dir) gram;
	Vizgrammar.draw_grammar_best_rules (sprintf "%s/best.rk%03d.ct%d.pid%04d.ppm" dir) gram;
	printf "done drawing grammar\n%!";
      end;

      begin try
	doit (sprintf "mkdir %s/iter.%d" dir i);
      with 
	  _ -> (); 
      end;
      doit_ok (sprintf "mv %s/gram.*.ppm %s/iter.%d" dir dir i);
      doit_ok (sprintf "mv %s/best.*.ppm %s/iter.%d" dir dir i);
  in

    logging 0 gram;

    let gram = ref gram in
      for i = 1 to niters do
	gram := Retrain.retrain
	  ~sparsefactor:(float_of_int i)
	  !gram (Array.to_list trainfamilies) 
	  strat;
	(* gram := Structure.replacement ~shrink:0.1 !gram (Array.to_list trainfamilies);  *)
	logging i !gram;
      done;

      Svg.finish x;
;;
