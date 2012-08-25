open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Util.Hops
open Abstract
open Sdf
open Grammar

(* let ncopies = 5;; *)
(* let nrulecopies = 5;; *)
let ncopies = 1;;
let nrulecopies = 1;;
let fncopies = float_of_int ncopies;;


let correlated_tuning excurve_name training_fnames niters =
  let excurve = Curve.load excurve_name in
  let exfamily = excurve, Sdf.make_full_family (Array.length excurve) in
  let gram = Models.Simple.make_grammar exfamily in

  let gram = 
    let live = new_live_grammar
      ~nsyms:(Frozen.num_symbols gram) ~ncomps:(Frozen.num_compositions gram)
      gram.f_gdata in

    let map = mkhash 100 in

      (* we want to have ncopies copies of each symbol, and give each
	 a rule with a different midpoint that is tied to random
	 copies of the target symbols.

	 might make more sense to give the different midpoints near
	 the top, and let that determine the other midpoints. how do
	 we know if we're near the top? Can say, if lcurve and rcurve
	 together have >= 4 points? 

	 annoying point: with only one start symbol, we only get one
	 rule. with multiple start symbols, only one of them is
	 actually the start symbol, unless we apply a merge.

	 Only need to have one copy of bottom-level symbols.
      *)
      Frozen.iter_symbols gram
	begin fun sym ->
	  (* assert(List.length sym.dcompids <= 1); *)
	  if sym.dcompids = [] then begin 
	    let newsym = make_new_symbol live sym.sdata sym.startable in
	      map <<+ (sym.sid, newsym.sid);
	  end
	  else begin
	    for i = 1 to ncopies do
	      let newsym = make_new_symbol live sym.sdata sym.startable in
		map <<+ (sym.sid, newsym.sid);
	    done
	  end
	end;

      iter_all_decompositions gram
	begin fun sym comp ->
	  List.iter
	    begin fun topsid ->
	      for xxx = 1 to nrulecopies do
		let leftsids = Array.of_list (map >>> comp.leftsid) in
		let leftsid = leftsids.(Random.int (Array.length leftsids)) in

		let rightsids = Array.of_list (map >>> comp.rightsid) in
		let rightsid = rightsids.(Random.int (Array.length rightsids)) in

		let prob = comp.cdata.prob /. fncopies in
		let newgeom = 

		  if (Array.length comp.cdata.lcurve) +
		    (Array.length comp.cdata.rcurve) >= 4 then begin
		      (match comp.cdata.geom with
			   Watson watson ->  
			     Watson {
			       Watson.mean = Watson.sample watson; 
			       Watson.conc_ = watson.Watson.conc_}
			 | Improper -> Improper)
		    end
		  else
		    comp.cdata.geom
		in

		let cdata = 
		  {prob = prob;
		   cost = -. log prob;
		   geom = newgeom;
		   ocurve = comp.cdata.ocurve;
		   lcurve = comp.cdata.lcurve;
		   rcurve = comp.cdata.rcurve;
		  }
		in
		  imake_new_composition live topsid
		    (leftsid, rightsid) cdata;

	      done
	    end
	    (map >>> sym.sid)

	end;

      let gram = finalize live in
      let start = Frozen.start gram in
	Frozen.iter_symbols gram
	  begin fun sym -> 
	    if sym.startable && sym.sid <> start.sid then begin 
	      merge_symbols_left gram start sym ~renorm:false
	    end
	  end;
	let gram = ensure_reachability gram in
	  Frozen.iter_symbols gram (renormalize_binary gram);

	  gram

  in

  let training = Array.map Curve.load training_fnames in
  let trainfamilies = Array.map
    begin fun c -> 
      let fam = Sdf.make_full_family (Array.length c) in
	Models.Simple.add_curve_data_to_family c fam
    end 
    training 
  in

  let logging i gram = 
    let file = open_out (sprintf "tmp/full_tuning.%d.gram" i) in
      Marshal.to_channel file gram [];
      close_out file;
      printf "LOGGING %d\n%!" i;
      Grammar.print_grammar gram;
  in

    logging 0 gram;

    let gram = ref gram in
      for i = 1 to niters do
	gram := Retrain.retrain
	  ~sparsefactor:0. (* (float_of_int i) *)
	  ~prunethresh:0.01 (* 0.0001 *)
	  !gram (Array.to_list trainfamilies) 
	  Models.Simple.strat;
	(* gram := Structure.replacement ~shrink:0.1 !gram (Array.to_list trainfamilies);  *)
	logging i !gram;
      done

let _ = 
  let example = ref "NONE.curve" in
  let niters = ref 0 in
  let curves = ref [] in

  let add_curve fname = (curves := fname :: !curves;) in

  let _ = Arg.parse [
    "-example", Arg.Set_string example, "Example curve.";
    "-niters", Arg.Set_int niters, "Number of EM iterations.";]
    add_curve
    "./prog -example example -sdf sdf curve1 curve2 curve3..."
  in

  let curves = Array.of_list (List.rev !curves) in
    
    correlated_tuning !example curves !niters
