open Printf
open Util.Cops
open Util.Hops
open Util.Misc
module C = Complex
module I = Image
open Bounds
open Abstract
open Grammar

let bounds = {
  xmin = 0.;
  xmax = 1.;
  ymin = -.0.5;
  ymax = 0.5;}

type scale_build = {
  n: int;  (* smallest scale is 1/n *)
  max_lvl: int; (* log_2 n *)
  max_len_lvl: int; (* max length of curve relative to its level *)
  curves: Curve.t array;
}

type scale_sym_sdata = {
  scale: float;
  len: int;
}

type scale_comp_sdata = {
  prob_: float;
  scale_: float;
  relmid: float;
  leftlen: int;
  rightlen: int;
  closed_: bool;
}

type scale_sdf = {
  gram: (scale_sym_sdata, scale_comp_sdata, unit) Abstract.frozen_grammar;
  nts: (int, scale_sym_sdata symbol) hash;
  rules: (int*int, scale_comp_sdata composition) hash;
  lens: int array;
  complens: (int*int) array;
  scales: float array;
  curves_: Curve.t array;
}

(* let make_scale_parsing_strategy n =  *)
(*   let fn = float_of_int n in *)
(*     {Parsing.tgt_sym_namer = (fun scurve -> sprintf "[%d, %d]" scurve.sdata.first_ scurve.sdata.last_); *)

(*      Parsing.lexical_ok = (fun scurve -> (scurve.sdata.len_ = 1)); *)
(*      Parsing.binary_ok = (fun scurve -> (scurve.sdata.len_ != 1)); *)
(*      Parsing.goal_ok = (fun scurve -> scurve.sdata.first_ = scurve.sdata.last_); *)

(*      Parsing.lexical_cost = (fun sym scurve -> sym.sdata.straightcost); *)
(*      Parsing.binary_cost = (fun comp dcomp -> prod_cost comp dcomp.cdata.shape_); *)
(*      Parsing.goal_cost = (fun gdata scurve -> 0.); *)

(*      Parsing.compatible =  *)
(* 	begin fun sym scurve -> *)
(* 	  let sym_scale = (Array.length sym.curve) /. fn in *)
(* 	  let scale_ratio = sym_scale /. scurve.scale in *)
(* 	    (sym.sdata.closed = scurve.startable) && *)
(* 	      (scale_ratio > 0.5) && *)
(* 	      (scale_ratio <= 2.0) *)

(* 	end; *)

(*      Parsing.getshape = (fun cdata -> cdata.shape_); *)

(*      Parsing.fit_midpoint_distro = begin fun prod samples themode -> *)
(*        let samples = Array.of_list samples in *)
(*        let geom, inference_data =   *)
(* 	 Watson.watson_distro_family.Distro.infer samples  *)
(*        in *)
(* 	 { *)
(* 	   geom = Watson geom; *)
(* 	   prob = prod.cdata.prob; *)
(* 	   cost = prod.cdata.cost; *)
(* 	   ocurve = prod.cdata.ocurve; *)
(* 	   lcurve = prod.cdata.lcurve; *)
(* 	   rcurve = prod.cdata.rcurve; *)
(* 	 } *)
(*      end; *)

(*     } *)


let granularity = 20
let sigma = 1.0

let binom n k = 
  let n, k = ref (float_of_int n), ref (float_of_int k) in
  let prod = ref 1. in
    while !k >= 1. do 
      prod := !prod *. !n /. !k;
      n := !n -. 1.;
      k := !k -. 1.;
    done;
    !prod

let make_scale_sdf gramopt build = 
  assert (gramopt = None);
  let live = new_live_grammar () in
  let nts = mkhash 100 in 
  let rules = mkhash 100 in 
  let lens = ref [] in
  let breaks = mkhash 100 in

    for lvl = 0 to build.max_lvl do
      let base_len = 1 lsl lvl in
	for a = 1 to build.max_len_lvl do
	  let len = min (a * base_len) build.n in
	    if not (List.mem len !lens) then begin
	      lens := len :: !lens;

	      for b = 1 to a-1 do 
		let leftlen = b * base_len in
		let rightlen = (a-b) * base_len in
		  breaks <<+ (len, (leftlen, rightlen, a, b));
	      done
	    end
	done
    done;

    (* Add L-> LL rules??? *)
    List.iter
      begin fun len ->
	printf "adding break for len=%d?\n%!" len;
    	if not (breaks >>? len) then begin
	  printf "adding break for len=%d!\n%!" len;
    	  breaks <<+ (len, (len, len, 1, 1));
    	end
      end
      !lens;

    List.iter
      begin fun len ->
	if not (nts >>? len) then begin
	  let sym = 
	    make_new_symbol live
	      {scale = (float_of_int len) /.
		  (float_of_int build.n);
	       len = len;
	      } 
	      (len = build.n) (* <- is it startable? *)
	  in
	    nts << (len, sym);
	end
      end
      !lens; (* lens reversed -> large to small -> largest is start *)
    
    List.iter
      begin fun len ->
	let sym = nts >> len in
	  List.iter 
	    begin fun (leftlen,rightlen,a,b) ->
	      let left = nts >> leftlen in
	      let right = nts >> rightlen in
	      let comp = make_new_composition live sym.sid
		(left.sid, right.sid) 
		{(* prob_ = (binom (a+b) b) /. (float_of_int (2 lsl (a+b))); *)
		 prob_ = 1.0;
		 scale_ = sym.sdata.scale;
		 relmid = (float_of_int b) /. (float_of_int a);
		 leftlen = leftlen;
		 rightlen = rightlen;
		 closed_ = (len = build.n);
		}
	      in
		printf "{%d} -> {%d} + {%d}: %b, %f\n" len leftlen rightlen comp.cdata.closed_ comp.cdata.prob_;
		rules <<+ ((leftlen,rightlen), comp);
	    end
	    (breaks >>> len);
      end
      !lens;

    let gram = finalize live in
    let lens = Array.map (fun sym -> sym.sdata.len) gram.f_symbols in
    let complens = Array.map (fun comp -> (comp.cdata.leftlen, comp.cdata.rightlen)) gram.f_compositions in
    let scales = Array.map (fun sym -> sym.sdata.scale) gram.f_symbols in

    let reachable = check_reachability gram in
      Array.iter (fun x -> assert(x)) reachable;


      {
	gram = gram;
	nts = nts;
	rules = rules;
	lens = lens;
	complens = complens;
	scales = scales;
	curves_ = build.curves;
      }

let suggest gramopt internal = Some internal

let apply scale_sdf gramopt =
  assert(gramopt = None);
  
  let find_closest_len_to_scale s = 
    let idx, least = ref 0, ref 1.0 in
      Array.iteri
	begin fun i t ->
	  if abs_float (s-.t) < !least then begin
	    least := abs_float (s-.t);
	    idx := i;
	  end
	end
	scale_sdf.scales;
      scale_sdf.lens.(!idx)
  in

  (* harvest the samples *)
  let samples = Array.init (Hashtbl.length scale_sdf.rules) (fun x -> []) in
  let _ = Array.iter 
    begin fun curve ->
      printf "+%!";
      let n = Array.length curve in
      let nf = float_of_int n in
	for len = 1 to n do
	  let scale = (float_of_int len) /. nf in
	  let len' = find_closest_len_to_scale scale in
	  let nt = scale_sdf.nts >> len' in

	    Frozen.iter_decompositions scale_sdf.gram nt
	      begin fun dcomp -> 
		let midlen = round (dcomp.cdata.relmid *. (float_of_int len)) in

		  for i = 0 to n-1 do
		    let j, k = (i + midlen) mod n , (i + len) mod n in
		    let shape = Shape.shape_of_complex_bme curve.(i) curve.(j) curve.(k) in
		    let x = Shape.get_pt shape in
		      samples.(dcomp.cid) <- x :: samples.(dcomp.cid);
		  done
	      end

	done;
    end
    scale_sdf.curves_;
    printf "\n%!";
  in

  (* fit the distros, one per production *)
  let wdf = Watson.watson_distro_family in
  let distros = 
    Array.map 
      begin fun ssamples ->
	assert (List.length ssamples > 1);
	printf ".%!";
	let winput = (Array.map (fun x -> (c0,x,c1)) (Array.of_list ssamples)) in
	let winput = Array.map (fun (p,q,r) -> (1.0, Shape.shape_of_complex_bme p q r)) winput in
	let winput = {
	  Watson.samples = winput;
	  Watson.gamma_shape_param=1000.;
	  Watson.gamma_mean_param=100.;
	}
	in
	let wdistro, woutput = wdf.Distro.infer winput in
	  (* let z0,z1,z2 = wdata.Watson.mode in *)
	  (*   {Watson.mean = Shape.shape_of_complex_bme z0 z1 z2; *)
	  (*    Watson.conc_ = wdata.Watson.conc; *)
	  (*   } *)
	  wdistro
      end
      samples
  in
  let _ = printf "\n%!" in

  (* build the grammar *)
  let doubled_acurve = Array.append scale_sdf.curves_.(0) scale_sdf.curves_.(0) in
  let flen_acurve = float_of_int (Array.length scale_sdf.curves_.(0)) in
  let gram = map_frozen_grammar scale_sdf.gram
    (fun x -> x)

    begin fun sym -> 
      let symlen = (round (sym.sdata.scale *. flen_acurve)) in
      let straightcost = 100. *. sym.sdata.scale *. sym.sdata.scale in
	printf "straightcost=%f\n%!" straightcost;
	{sdata = {
	   closed = sym.startable;
	   straightprob = exp (-. straightcost);(* if symlen = 1 then 1. else 0.; *)
	   straightcost = straightcost; (* if symlen = 1 then 0. else infinity; *)
	   curve = Array.sub doubled_acurve 0 symlen;
	 };
	 sid = sym.sid;
	 dcompids = sym.dcompids;
	 lcompids = sym.lcompids;
	 rcompids = sym.rcompids;
	 startable = sym.startable;
	}
    end

    begin fun comp ->
      let compmid = round (comp.cdata.scale_ *. 
			     comp.cdata.relmid *. flen_acurve) in
      let compend = round (comp.cdata.scale_ *. flen_acurve) in
	{cdata = {
	   prob = comp.cdata.prob_;
	   cost = -. log comp.cdata.prob_;
	   geom = if comp.cdata.closed_ then 
	     Improper 
	   else
	     Watson distros.(comp.cid);
	   ocurve = [| |];
	   lcurve = Array.sub doubled_acurve 0 compmid;
	   rcurve = Array.sub doubled_acurve compmid (compend-compmid);
	 };
	 cid = comp.cid;
	 topsid = comp.topsid;
	 leftsid = comp.leftsid;
	 rightsid = comp.rightsid;
	}
    end

  in
    gram, true


let texture_heuristic = {
  Xform.initialize = make_scale_sdf;
  Xform.suggest = suggest;
  Xform.apply = apply;
  Xform.print = (fun x -> printf "texture\n%!");
}

(* let _ =  *)
(*   let leafnum = int_of_string Sys.argv.(1) in *)
(*   let dir = (sprintf "scaled_nts.%d.d" leafnum) in *)
(*   let nsamples = 20 in *)
(*   let ntraining = 10 in *)

(*   let training_namer i =  *)
(*      (sprintf "DATA/leaves/leaf%02d/leaf%02d_%04d.curve" leafnum leafnum (i+1)) *)
(*   in *)

(*   let curves = Curve.load_all training_namer 0 ntraining in *)
(*   let gram, nts, rules, lens, complens, scales = make_scale_sdf () in *)
    
(*   let find_closest_len_to_scale s =  *)
(*     let idx, least = ref 0, ref 1.0 in *)
(*       Array.iteri *)
(* 	begin fun i t -> *)
(* 	  if abs_float (s-.t) < !least then begin *)
(* 	    least := abs_float (s-.t); *)
(* 	    idx := i; *)
(* 	  end *)
(* 	end *)
(* 	scales; *)
(*       lens.(!idx) *)
(*   in *)

(*   (\* harvest the samples *\) *)
(*   let samples = Array.init (Hashtbl.length rules) (fun x -> []) in *)
(*   let _ = Array.iter  *)
(*     begin fun curve -> *)
(*       printf "+%!"; *)
(*       let n = Array.length curve in *)
(*       let nf = float_of_int n in *)
(* 	for len = 1 to n do *)
(* 	  let scale = (float_of_int len) /. nf in *)
(* 	  let len' = find_closest_len_to_scale scale in *)
(* 	  let nt = nts >> len' in *)

(* 	    Frozen.iter_decompositions gram nt *)
(* 	      begin fun dcomp ->  *)
(* 		let midlen = round (dcomp.cdata.relmid *. (float_of_int len)) in *)

(* 		  for i = 0 to n-1 do *)
(* 		    let j, k = (i + midlen) mod n , (i + len) mod n in *)
(* 		    let shape = Shape.shape_of_complex_bme curve.(i) curve.(j) curve.(k) in *)
(* 		    let x = Shape.get_pt shape in *)
(* 		      samples.(dcomp.cid) <- x :: samples.(dcomp.cid); *)
(* 		  done *)
(* 	      end *)

(* 	done; *)
(*     end *)
(*     curves; *)
(*     printf "\n%!"; *)
(*   in *)

(*   (\* fit the distros, one per production *\) *)
(*   let distros =  *)
(*     Array.map  *)
(*       begin fun ssamples -> *)
(* 	printf ".%!"; *)
(* 	let wdata = Watson.fit_watson *)
(* 	  (Array.map (fun x -> (c0,x,c1)) (Array.of_list ssamples)) in *)
(* 	let z0,z1,z2 = wdata.Watson.mode in *)
(* 	  {Watson.mean = Shape.shape_of_complex_bme z0 z1 z2; *)
(* 	   Watson.conc_ = wdata.Watson.conc; *)
(* 	  } *)
(*       end *)
(*       samples *)
(*   in *)
(*   let _ = printf "\n%!" in *)

(*   (\* build the grammar *\) *)
(*   let doubled_acurve = Array.append curves.(0) curves.(0) in *)
(*   let flen_acurve = float_of_int (Array.length curves.(0)) in *)
(*   let gram = map_frozen_grammar gram *)
(*     (fun x -> x) *)

(*     begin fun sym ->  *)
(*       let symlen = (round (sym.sdata.scale *. flen_acurve)) in *)
(* 	{sdata = { *)
(* 	   closed = sym.startable; *)
(* 	   straightprob = if sym.dcompids = [] then 1. else 0.; *)
(* 	   straightcost = if sym.dcompids = [] then 0. else infinity; *)
(* 	   curve = Array.sub doubled_acurve 0 symlen; *)
(* 	 }; *)
(* 	 sid = sym.sid; *)
(* 	 dcompids = sym.dcompids; *)
(* 	 lcompids = sym.lcompids; *)
(* 	 rcompids = sym.rcompids; *)
(* 	 startable = sym.startable; *)
(* 	} *)
(*     end *)

(*     begin fun comp -> *)
(*       let compmid = round (comp.cdata.scale_ *. comp.cdata.relmid *. flen_acurve) in *)
(*       let compend = round (comp.cdata.scale_ *. flen_acurve) in *)
(* 	{cdata = { *)
(* 	   prob = comp.cdata.prob_; *)
(* 	   cost = -. log comp.cdata.prob_; *)
(* 	   geom = if comp.cdata.closed_ then  *)
(* 	     Improper  *)
(* 	   else *)
(* 	     Watson distros.(comp.cid); *)
(* 	   ocurve = [| |]; *)
(* 	   lcurve = Array.sub doubled_acurve 0 compmid; *)
(* 	   rcurve = Array.sub doubled_acurve compmid (compend-compmid); *)
(* 	 }; *)
(* 	 cid = comp.cid; *)
(* 	 topsid = comp.topsid; *)
(* 	 leftsid = comp.leftsid; *)
(* 	 rightsid = comp.rightsid; *)
(* 	} *)
(*     end *)

(*   in *)

(*   (\* show samples from the grammar *\) *)
(*   let p, q = c0, cxre 1000. in *)
(*   let curves = Array.init nsamples  *)
(*     begin fun i ->  *)
(*       printf ".%!"; *)
(*       Grammar.sample gram p q  *)
(*     end  *)
(*   in *)
(*   let _ = printf "\n%!" in *)
(*   let curves = Array.map (Curve.normalize ~scale:1000.) curves in *)

(*   let curvenamer = (sprintf "tmp/scaled_nts.%d.%04d.curve" leafnum)  in *)
(*   let fnames = Array.init nsamples curvenamer in *)
(*   let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in *)
(*     Curve.save_all curvenamer curves; *)
(*     doit (sprintf "./show_curves.native -fname tmp/scaled_nts_%d.svg -title '' %s" leafnum fnames); *)

(*   let fnames = Array.init ntraining training_namer in *)
(*   let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in *)
(*     doit (sprintf "./show_curves.native -fname tmp/scaled_nts_training_%d.svg -title '' %s" leafnum fnames); *)
    
    (* () *)
