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

let make_scale_sdf () = 
  let live = new_live_grammar () in
  let n = 512 in (* smallest scale is 1/n *)
  let max_lvl = 9 in (* log_2 n *)
  let max_len_lvl = 4 in (* max length of curve relative to its level *)
  let nts = mkhash 100 in 
  let rules = mkhash 100 in 

  let lens = ref [] in
  let breaks = mkhash 100 in

    for lvl = 0 to max_lvl do
      let base_len = 1 lsl lvl in
	for a = 1 to max_len_lvl do
	  let len = min (a * base_len) n in
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

    List.iter
      begin fun len ->
	if not (nts >>? len) then begin
	  let sym = 
	    make_new_symbol live
	      {scale = (float_of_int len) /.
		  (float_of_int n);
	       len = len;
	      } 
	      (len = n) (* <- is it startable? *)
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
		{prob_ = (binom a b) /. (float_of_int (2 lsl a));
		 scale_ = sym.sdata.scale;
		 relmid = (float_of_int b) /. (float_of_int a);
		 leftlen = leftlen;
		 rightlen = rightlen;
		 closed_ = (len = n);
		}
	      in
(* 		printf "{%d} -> {%d} + {%d}\n" len leftlen rightlen; *)
		rules << ((leftlen,rightlen), comp);
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

      gram, nts, rules, lens, complens, scales



let _ = 
  let leafnum = int_of_string Sys.argv.(1) in
  let dir = (sprintf "scaled_nts.%d.d" leafnum) in
  let nsamples = 20 in
  let ntraining = 10 in

  let training_namer i = 
     (sprintf "leaves/leaf%02d/leaf%02d_%04d.curve" leafnum leafnum (i+1))
  in

  let curves = Curve.load_all training_namer 0 ntraining in
  let gram, nts, rules, lens, complens, scales = make_scale_sdf () in
    
  let find_closest_len_to_scale s = 
    let idx, least = ref 0, ref 1.0 in
      Array.iteri
	begin fun i t ->
	  if abs_float (s-.t) < !least then begin
	    least := abs_float (s-.t);
	    idx := i;
	  end
	end
	scales;
      lens.(!idx)
  in

  (* harvest the samples *)
  let samples = Array.init (Hashtbl.length rules) (fun x -> []) in
  let _ = Array.iter 
    begin fun curve ->
      printf "+%!";
      let n = Array.length curve in
      let nf = float_of_int n in
	for len = 1 to n do
	  let scale = (float_of_int len) /. nf in
	  let len' = find_closest_len_to_scale scale in
	  let nt = nts >> len' in

	    Frozen.iter_decompositions gram nt
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
    curves;
    printf "\n%!";
  in

  (* fit the distros, one per production *)
  let distros = 
    Array.map 
      begin fun ssamples ->
	printf ".%!";
	let wdata = Watson.fit_watson
	  (Array.map (fun x -> (c0,x,c1)) (Array.of_list ssamples)) in
	let z0,z1,z2 = wdata.Watson.mode in
	  {Watson.mean = Shape.shape_of_complex_bme z0 z1 z2;
	   Watson.conc_ = wdata.Watson.conc;
	  }
      end
      samples
  in
  let _ = printf "\n%!" in

  (* build the grammar *)
  let doubled_acurve = Array.append curves.(0) curves.(0) in
  let flen_acurve = float_of_int (Array.length curves.(0)) in
  let gram = map_frozen_grammar gram
    (fun x -> x)

    begin fun sym -> 
      let symlen = (round (sym.sdata.scale *. flen_acurve)) in
	{sdata = {
	   closed = sym.startable;
	   straightprob = if sym.dcompids = [] then 1. else 0.;
	   straightcost = if sym.dcompids = [] then 0. else infinity;
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
      let compmid = round (comp.cdata.scale_ *. comp.cdata.relmid *. flen_acurve) in
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

  (* show samples from the grammar *)
  let p, q = c0, cxre 1000. in
  let curves = Array.init nsamples 
    begin fun i -> 
      printf ".%!";
      Grammar.sample gram p q 
    end 
  in
  let _ = printf "\n%!" in
  let curves = Array.map (Curve.normalize ~scale:1000.) curves in

  let curvenamer = (sprintf "tmp/scaled_nts.%d.%04d.curve" leafnum)  in
  let fnames = Array.init nsamples curvenamer in
  let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in
    Curve.save_all curvenamer curves;
    doit (sprintf "./show_curves.native -fname tmp/scaled_nts_%d.svg -title '' %s" leafnum fnames);

  let fnames = Array.init ntraining training_namer in
  let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in
    doit (sprintf "./show_curves.native -fname tmp/scaled_nts_training_%d.svg -title '' %s" leafnum fnames);

    (* show the grammar *)
    (*     let _ =  *)
    (*       let file = open_out "tmp/scaled_nts.gram" in *)
    (* 	doit (sprintf "mkdir -p %s" dir); *)
    (* 	doit (sprintf "rm -rf %s/*" dir); *)
    (* 	Marshal.to_channel file gram []; *)
    (* 	close_out file; *)
    (* 	(\* no -rules *\) *)
    (* 	doit (sprintf "./show_grammar.native -gramfile tmp/scaled_nts.gram -dir %s -latexdir ../%s -title ''" dir dir);     *)
    (*     in *)


    (* visualize the bastards *)
    (*     let _ =  *)
    (*       Array.iteri *)
    (* 	begin fun i ssamples -> *)
    (* 	  let maxcount = ref 1 in *)
    (* 	  let im = Image.create (granularity+1) (granularity+1) 0 in *)
    (* 	    List.iter *)
    (* 	      begin fun sample -> *)
    (* 		let x,y = Bounds.bounds_to_array_nice bounds granularity sample in *)
    (* 		  I.set im (I.get im (x,y) + 1) (x,y); *)
    (* 	      end *)
    (* 	      ssamples; *)

    (* 	    for y=0 to granularity do *)
    (* 	      for x=0 to granularity do *)
    (* 		maxcount := max (I.get im (x,y)) !maxcount; *)
    (* 	      done *)
    (* 	    done; *)

    (* 	    let im = Image.map *)
    (* 	      begin fun count -> *)
    (* 		let count = (255 * count) / !maxcount in *)
    (* 		  (count,count,count); *)
    (* 	      end *)
    (* 	      im *)
    (* 	    in *)
    (* 	      I.set im (255,0,0) (Bounds.bounds_to_array_nice bounds granularity c0); *)
    (* 	      I.set im (0,255,0) (Bounds.bounds_to_array_nice bounds granularity c1); *)

    (* 	      let leftlen, rightlen = complens.(i) in *)

    (* 		Pnm.save_ppm im (sprintf "%s/density-%03d-%03d-%03d.ppm" dir (leftlen + rightlen) leftlen rightlen); *)
    (* 	end *)
    (* 	samples *)
    (*     in	   *)
    
    
    ()
