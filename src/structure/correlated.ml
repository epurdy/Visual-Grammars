open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Util.Hops
open Abstract
open Sdf
open Grammar

(** We want to have ncopies copies of each symbol, and give each a
    rule with a different midpoint that is tied to random copies of the
    target symbols.

    It might make more sense to give the different midpoints near the
    top, and let that determine the other midpoints. how do we know if
    we're near the top? Can say, if lcurve and rcurve together have >=
    4 points?

    Annoying point: with only one start symbol, we only get one
    rule. with multiple start symbols, only one of them is
    actually the start symbol, unless we apply a merge.

    Only need to have one copy of bottom-level symbols.
*)

type correlated_repetition = {
  ncopies: int;
  nrulecopies: int;
  name: string;
}



let correlated_repetition_apply internal gram =
  let gram = get gram in
  let fncopies = float_of_int internal.ncopies in

  let gram = 
    let live = new_live_grammar
      ~nsyms:(Frozen.num_symbols gram) ~ncomps:(Frozen.num_compositions gram)
      gram.f_gdata in

    let map = mkhash 100 in

      Frozen.iter_symbols gram
	begin fun sym ->
	  (* assert(List.length sym.dcompids <= 1); *)
	  if sym.dcompids = [] then begin 
	    let newsym = make_new_symbol live sym.sdata sym.startable in
	      map <<+ (sym.sid, newsym.sid);
	  end
	  else begin
	    for i = 1 to internal.ncopies do
	      let newsym = make_new_symbol live sym.sdata sym.startable in
		map <<+ (sym.sid, newsym.sid);
	    done
	  end
	end;

      iter_all_decompositions gram
	begin fun sym comp ->
	  List.iter
	    begin fun topsid ->
	      for xxx = 1 to internal.nrulecopies do
		let leftsids = Array.of_list (map >>> comp.leftsid) in
		let leftsid = leftsids.(Random.int (Array.length leftsids)) in

		let rightsids = Array.of_list (map >>> comp.rightsid) in
		let rightsid = rightsids.(Random.int (Array.length rightsids)) in

		let prob = comp.cdata.prob /. fncopies in

		  
		let unpack = 
		  begin match comp.cdata.geom with
		      Watson watson ->  
			Some (Watson.watson_distro_family.Distro.unpack watson)
		    | Improper -> None
		  end
		in
		let newgeom = 

		  if (Array.length comp.cdata.lcurve) +
		    (Array.length comp.cdata.rcurve) >= 4 then begin
		      (match comp.cdata.geom with
			   Watson watson ->  
			     let unpack = get unpack in
			       Watson 
				 (Watson.watson_distro_family.Distro.build
				    {
				      Watson.mean_shape = Watson.watson_distro_family.Distro.sample watson;
				      Watson.concentration = unpack.Watson.concentration;
				    }
				 )
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

    gram, true

let correlated_repetition_heuristic = {
  Xform.initialize = (fun g x -> x);
  Xform.suggest = (fun g x -> (Some x));
  Xform.apply = (fun x g -> correlated_repetition_apply x g);
  Xform.print = (fun x -> printf "%s\n%!" x.name);
}
