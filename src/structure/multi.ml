open Util.Misc
open Printf
module C = Complex
open Util.Cops
open Util.Hops
open Abstract
open Sdf
open Grammar

type multi_params = {
  ncopies: int;
  name: string;
}

let multi_apply params gram = 
  let gram = get gram in
  let fncopies = float_of_int params.ncopies in
  let orig_prob = 1. /. fncopies in
  let live = new_live_grammar
    ~nsyms:(Frozen.num_symbols gram) ~ncomps:(Frozen.num_compositions gram)
    gram.f_gdata in

    Frozen.iter_symbols gram
      begin fun sym ->
	imake_new_symbol live sym.sdata sym.startable;
      end;

    iter_all_compositions gram
      begin fun comp ->
	for i = 1 to params.ncopies do
	  let unpack, prob = 
	    begin match comp.cdata.geom with
		Watson watson ->  
		  Some (Watson.watson_distro_family.Distro.unpack watson)
	      | Improper -> None
	    end,
	    comp.cdata.prob *. (1. -. orig_prob) /. (fncopies -. 1.)
	  in

	  let newgeom =
	    begin match comp.cdata.geom with
		Watson watson -> 				 
		  let unpack = get unpack in
		    Watson
		      (Watson.watson_distro_family.Distro.build
			 {
			   Watson.mean_shape = Watson.watson_distro_family.Distro.sample watson;
			   Watson.concentration = unpack.Watson.concentration;
			 }
		      )
	      | Improper -> Improper
	    end
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
	    imake_new_composition live comp.topsid (comp.leftsid, comp.rightsid) cdata;
	done;	    
      end;

    finalize live, true



let multi_heuristic = {
  Xform.initialize = (fun g x -> x);
  Xform.suggest = (fun g x -> (Some x));
  Xform.apply = (fun x g -> multi_apply x g);
  Xform.print = (fun x -> printf "%s\n%!" x.name);
}
