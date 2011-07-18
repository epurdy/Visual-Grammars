open Util.Hops
open Util.Misc
module Bundle = Util.Bundle
open Printf

open Abstract
open Parsing
module G = Grammar
module S = Sdf

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

let get_soft_counts_parallel_worker gram family =
  let soft = make_soft_counts (gram.num_symbols()) (gram.num_compositions()) in
    sparse_inside_outside gram family soft;
    soft

let get_soft_counts_num_workers = 8

let get_soft_counts_parallel gram families =
  let families = Array.of_list families in
  let workers = Bundle.create get_soft_counts_num_workers in
  let magic = randstr 10 in
    Grammar.marshal gram (sprintf "/var/tmp/epurdy/%s.gram" magic);
    Array.iteri
      begin fun i fam ->
	let proc_fam fam = 
	  Sdf.marshal fam (sprintf "/var/tmp/epurdy/%s.%d.fam" magic i);
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
	let soft = (Marshal.from_channel file: 'b sparse_counts_table) in
	  close_in file;
	  soft
      end
    in

    let softs = Array.to_list softs in
    let soft = List.fold_right combine_soft_counts (List.tl softs) (List.hd softs) in
      soft

let get_soft_counts_seq gram families =
  let soft = make_soft_counts (gram.num_symbols()) (gram.num_compositions()) in
    List.iter 
      begin fun family ->
	sparse_inside_outside gram family soft;
      end
      families;
    soft

let retrain
    ?(sparsefactor=0.)
    ?(sigma = Con.default_sigma)
    ?(baseline_sigma = Con.default_baseline_sigma)
    gram families   = 
  (*   print gram; *)

  printf "starting retrain!\n%!";

(*   let soft = get_soft_counts_seq gram families in *)
  let soft = get_soft_counts_parallel gram families in

    printf "TOTALQUAL %f\n%!" soft.qual__;

    (* use multinomial counts for each state *)
    gram.iter_symbols 
      begin fun state ->
	let scounts =
	  List.map (fun prod -> soft.binary_counts.(prod.G.cid)) (gram.get_decompositions state) in
	let scounts = soft.lexical_counts.(state.G.sid) :: scounts in
	let probs = normalize_neglogs scounts sparsefactor in
	let prob_lex, probs_bin = List.hd probs, List.tl probs in

	  state.G.straightprob <- prob_lex;
	  state.G.straightcost <- -. log prob_lex;
	  List.iter2
	    begin fun prod probability ->
	      prod.G.prob <- probability;
	      prod.G.cost <- -. log probability;
	    end
	    (gram.get_decompositions state)
	    probs_bin;
      end;

    gram.iter_all_decompositions
      begin fun state prod ->
	if not state.G.closed then
	  let getshape (nlw,cdata) = (nlw,cdata.S.dshape) in
	  let samples = List.map getshape soft.mpchoices.(prod.G.cid) in
	  let _,themode = soft.mpmodes.(prod.G.cid) in

	  (* make new midpoint distro *)
	  let geom = Parzen.make_model_neglog_weighted (Array.of_list samples)
	    sigma Con.default_granularity
	    (begin match themode with
		 None -> Shape.default_shape
	       | Some cdata -> cdata.S.dshape 
	     end, baseline_sigma) Con.default_baseline_weight
	    true
	  in

	    (* replace midpoint distro *)
	    prod.G.geom <- G.Parzen geom;

      (* 	    printf "computed (some) midpoints for state #%d\n%!" state.G.sid	   *)
      end;

    printf "done with geometry!\n%!";

    Grammar.print_grammar gram;

    Grammar.prune gram 0.0001

      
