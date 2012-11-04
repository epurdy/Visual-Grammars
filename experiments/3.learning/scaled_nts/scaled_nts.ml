open Printf
open Util.Cops
open Util.Hops
open Util.Misc
module C = Complex
module I = Image
open Bounds
open Abstract
open Grammar

(* open Structure.Texture *)

(* let niters = 0;; *)
let niters = 5;;
let ncopies = 5;;
let nrulecopies = 5;;

let _ = 
  let leafnum = int_of_string Sys.argv.(1) in
  let dir = (sprintf "scaled_nts.%d.d" leafnum) in
  let nsamples = 20 in
  let ntraining = 10 in

  let training_namer i = 
    (sprintf "DATA/leaves/leaf%02d/leaf%02d_%04d.curve" leafnum leafnum (i+1))
  in

  let heur = Structure.Texture.texture_heuristic in

  let curves = Curve.load_all training_namer 0 ntraining in
  let scale_sdf = heur.Xform.initialize None 
    {Structure.Texture.n = 512;
     Structure.Texture.max_lvl = 9;
     Structure.Texture.max_len_lvl = 4;
     Structure.Texture.curves = curves;
    }
  in

  let gram, succ = heur.Xform.apply scale_sdf None in
  let _ = assert succ in

  let _ = 
    Frozen.iter_symbols gram
      begin fun sym ->
	assert (sym.sdata.straightcost < 1000.);
      end
  in

  (* apply correlated repetition *)
  let gram = Xform.xform_pipeline
    Structure.Correlated.correlated_repetition_heuristic
    gram
    {Structure.Correlated.ncopies = ncopies;
     Structure.Correlated.nrulecopies = nrulecopies;
     Structure.Correlated.name = "CORRELATED REPETITION";
    }
  in

  (* retrain the grammar *)
  let trainfamilies = Array.map
    begin fun c -> 
      (* let fam = Sdf.make_full_family (Array.length c) in *)
      (* let fam = Sdf.make_balanced_flexible_family (Array.length c) 4 in *)
      let fam = Sdf.make_sparse_family (Array.length c) 8 in
      let fam = Sdf.bottom_out_family fam in
      let reachable = Abstract.check_reachability fam in
      let realizable = Abstract.check_realizability fam 
	begin fun sym ->
	  sym.sdata.Sdf.len = 1
	end  
      in
	Array.iter (fun x -> assert x) reachable;
	Array.iter (fun x -> assert x) realizable;
	Models.EM.add_curve_data_to_family c fam
    end 
    curves 
  in

  let gram =
    let gram = ref gram in
      for i = 1 to niters do
  	printf "x%!";
  	gram :=
  	  Xform.xform_pipeline
  	    Retrain.retrain_heuristic
  	    !gram
  	    {Retrain.sparsefactor =0.01; (* (float_of_int i) *)
	     Retrain.watson_prior_shape = 1000.;
	     Retrain.watson_prior_mean = 100.;
  	     Retrain.curves = curves;
  	     Retrain.families = trainfamilies;
  	     Retrain.strat = Models.EM.strat;
  	     Retrain.name = "correlated_retrain";
	     Retrain.parallel = false;
  	    };
  	gram :=
  	  Xform.xform_pipeline
  	    Structure.Prune.prune_heuristic
  	    !gram
  	    {Structure.Prune.prunethresh = 0.01}; (* 0.0001 *)
      done;

      !gram
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
