open Util.Hops
open Util.Misc
module Bundle = Util.Bundle
open Printf


open Abstract
open Parsing
module G = Grammar
module S = Sdf

type ('tgt_sym, 'tgt_comp, 'tgt_glob) retraining_build_data = {
  sparsefactor: float;
  watson_prior_shape: float;
  watson_prior_mean: float;
  curves: Curve.t array;
  families: ('tgt_sym, 'tgt_comp, 'tgt_glob) Abstract.frozen_grammar array;
  strat: (Grammar.sdata, Grammar.cdata, 'tgt_sym, 'tgt_comp, 'tgt_glob) Parsing.strategy;
  name: string;

  parallel: bool;
}

type ('tgt_sym, 'tgt_comp, 'tgt_glob, 'a) retraining_internal_data = {
  soft_counts: 'a sparse_counts_table;
  build_data: ('tgt_sym, 'tgt_comp, 'tgt_glob) retraining_build_data;
  watson_prior_shape_: float;
  watson_prior_mean_: float;
}

let print_counts2 counts =
  printf "counts: ";
  List.iter (fun x -> printf "%f, " x) counts;
  printf "\n%!" 

let print_counts neglogs = 
  let counts = List.map (fun x -> exp (-.x)) neglogs in
    print_counts2 counts

let normalize_neglogs neglogs sparsefactor = 
  if List.length neglogs = 1 then
    [1.0]
  else begin
    print_counts neglogs;

    let counts = List.map (fun x -> exp (-.x)) neglogs in
    let sorted = List.sort (fun x y -> compare y x) counts in

    (* enforce that 80% of the total count is preserved *)
    let frac_preserve = 0.8 in
    let sum = List.fold_left (+.) 0. sorted in
    let thecum = ref 0. in
    let idx = ref 0 in
    let sparsefactor = ref sparsefactor in
    let _ =  List.iter 
      begin fun c ->
	(* If sparsefactor were c, then the total left over is the
	   total mass we've already seen, minus (c * # buckets
	   seen). If that's less than frac_preserve of the original, then move
	   sparsefactor down. *)
	let total_left = !thecum -. (float_of_int !idx) *. c in
	  if total_left < frac_preserve *. sum then
	    sparsefactor := min !sparsefactor c;
	  thecum := c +. !thecum;	  
	  incr idx;
      end
      sorted
    in
    let sparsefactor = !sparsefactor in

(*     let sparsefactor = sparsefactor /. (float_of_int (List.length sorted)) in *)
    let sparsefactor = min sparsefactor (List.nth sorted 1) in
    let sparsefactor = 0.9999 *. sparsefactor in
    let counts = 
      List.map (fun x -> max (x -. sparsefactor) 0.) 
      counts in 

    print_counts2 counts;

    let sum = List.fold_left (+.) 0. counts in
    let probs = List.map (fun x -> x /. sum) counts in
      probs
  end

let get_soft_counts_parallel_worker gram family strat =
  let soft = make_soft_counts (Frozen.num_symbols gram)
    (Frozen.num_compositions gram) in
    sparse_inside_outside gram family soft strat;
    soft

let get_soft_counts_num_workers = 4

let get_soft_counts_parallel gram families =
  let families = Array.of_list families in
  let workers = Bundle.create get_soft_counts_num_workers in
  let magic = randstr 10 in
  let file = open_out (sprintf "/var/tmp/epurdy/%s.gram" magic) in
    Marshal.to_channel file gram [];
    close_out file;
    Array.iteri
      begin fun i fam ->
	let proc_fam fam = 
	  let file = open_out (sprintf "/var/tmp/epurdy/%s.%d.fam" magic i) in
	    Marshal.to_channel file fam [];
	    close_out file;
	  (* 	  let host = get_host () in *)
	  doitq (sprintf "./do_soft_counts.native -gramfile /var/tmp/epurdy/%s.gram -famfile /var/tmp/epurdy/%s.%d.fam -countfile /var/tmp/epurdy/%s.%d.count"
		  magic magic i magic i)
	in
	  workers.Bundle.go proc_fam fam;
      end
      families;
    workers.Bundle.finish ();
    let softs = Array.init (Array.length families) 
      begin fun i -> 
	let file = open_in (sprintf "/var/tmp/epurdy/%s.%d.count" magic i) in
	let soft = (Marshal.from_channel file: Shape.shape (*'b*) sparse_counts_table) in
	  close_in file;
	  soft
      end
    in

    let softs = Array.to_list softs in
    let soft = List.fold_right combine_soft_counts (List.tl softs) (List.hd softs) in
      soft

let get_soft_counts_seq gram families strat =
  let soft = make_soft_counts (Frozen.num_symbols gram)
    (Frozen.num_compositions gram) in
    List.iter 
      begin fun family ->
	sparse_inside_outside gram family soft strat;
      end
      families;
    soft

 (* thin wrapper around get_soft_counts *)
let retrain_initialize gram params = 
  let gram = Util.Misc.get gram in
  let _ = printf "starting retrain!\n%!" in

  let soft = 
    if params.parallel then
      get_soft_counts_parallel gram (Array.to_list params.families)
    else
      get_soft_counts_seq gram (Array.to_list params.families) params.strat
  in

    {soft_counts = soft;
     build_data = params;
     watson_prior_shape_ = params.watson_prior_shape;
     watson_prior_mean_ = params.watson_prior_mean;
    }

let retrain_apply soft_data gram =
  (* let gram = Grammar.copy gram in *)
  let gram = Util.Misc.get gram in
  let soft_data = Util.Misc.get soft_data in
  let soft = soft_data.soft_counts in

    printf "TOTALQUAL %f\n%!" soft.qual__;

    (* use multinomial counts for each state *)
    Frozen.iter_symbols gram
      begin fun state ->
	let scounts =
	  List.map (fun prod -> soft.binary_counts.(prod.cid)) 
	    (Frozen.get_decompositions gram state) in
	let scounts = soft.lexical_counts.(state.sid) :: scounts in
	let probs = normalize_neglogs scounts soft_data.build_data.sparsefactor in
	let prob_lex, probs_bin = List.hd probs, List.tl probs in

	  state.sdata <- {
	    G.straightprob = prob_lex;
	    G.straightcost = -. log prob_lex;
	    G.closed = state.sdata.G.closed;
	    G.curve = state.sdata.G.curve;
	  };
	  List.iter2
	    begin fun prod probability ->
	      prod.cdata <- {
		G.prob = probability;
		G.cost = -. log probability;
		G.geom = prod.cdata.G.geom;
		G.ocurve = prod.cdata.G.ocurve;
		G.lcurve = prod.cdata.G.lcurve;
		G.rcurve = prod.cdata.G.rcurve;
	      };
	    end
	    (Frozen.get_decompositions gram state)
	    probs_bin;
      end;

    iter_all_decompositions gram
      begin fun state prod ->
	if not state.sdata.G.closed then
	  (* 	  let getshape (nlw, cdata) = (nlw, strat.getshape cdata) in *)
	  let samples = (* List.map getshape *) soft.mpchoices.(prod.cid) in
	  let _,themode = soft.mpmodes.(prod.cid) in


	    (* replace midpoint distro *)
	    prod.cdata <- soft_data.build_data.strat.fit_midpoint_distro prod samples themode soft_data.watson_prior_shape_ soft_data.watson_prior_mean_;
	    (* 	    printf "computed (some) midpoints for state #%d\n%!" state.G.sid	   *)
      end;

    printf "done with geometry!\n%!";

    Grammar.print_grammar gram;

    gram, true

let retrain_suggest gram internal = Some internal
      
let retrain_heuristic = {
  Xform.initialize = retrain_initialize;
  Xform.suggest = retrain_suggest;
  Xform.apply = (fun x -> retrain_apply (Some x));
  Xform.print = (fun x -> printf "%s\n%!" x.build_data.name);
}
