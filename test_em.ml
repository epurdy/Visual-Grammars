open Util.Misc
open Printf
module C = Complex
open Util.Cops

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


let _ = 
  let x = Svg.create_sized (sprintf "%s/samples.svg" dir)
    ((float_of_int ncols) *. 1.5 *. scale, 
     float_of_int (5 + (nsamples/ncols) * niters) *. 4.0 *. scale) in
  let _ = Cairo.scale x.Svg.ctx scale scale in

  let training = Datasets.get_hand () in
  let examples = [| training.(0); |] in

  (* (\*   let training = Array.append (Datasets.get_articulator ~n:10 ()) (Datasets.get_hand ~n:10 ()) in *\) *)
  (* (\*   (\\*   let training = Datasets.get_hand () in *\\) *\) *)
  (* (\*   let examples = [| training.(0); training.(10); training.(12) |] in *\) *)

  let training = Datasets.get_romer ~n:20 () in
  let training = Array.map (fun c -> Curve.subsample_dp c npoints 2) training in
  let examples = [| training.(0); training.(1); training.(10); training.(11); |] in
    (*   let examples = choose_spaced training ngrammars in *)
    
  let _ = printf "about to make grammar sdf's\n%!" in

  let exfamilies = Array.map (fun c -> Sdf.sparse_family_of_curve_debug c k) examples in
    (*   let exfamilies = Array.map (fun c -> Sdf.full_family_of_curve_debug c) examples in *)

  let _ = printf "about to make grams!\n%!" in

  let grams = Array.map Grammar.grammar_of_family exfamilies in
  let gram = Grammar.merge_tops grams in

  let _ = printf "grammars built\n%!" in

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
	Grammar.draw_grammar (sprintf "%s/gram.%04d.ppm" dir) gram;
	Grammar.draw_grammar_best_rules (sprintf "%s/best.rk%03d.ct%d.pid%04d.ppm" dir) gram;
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

    let trainfamilies = Array.map (fun c -> Sdf.sparse_family_of_curve_debug c k2) training in
      (*     let trainfamilies = Array.map (fun c -> Sdf.full_family_of_curve_debug c) training in *)

    let gram = ref gram in
      for i = 1 to niters do
	(* retrain the grammar *)
	gram := Retrain.retrain
	  ~sparsefactor:(float_of_int i)
(* 	  ~sigma:(Con.default_sigma /. (1. +. float_of_int i)) *)
(* 	  ~baseline_sigma:(Con.default_sigma /. (1. +. float_of_int i)) *)
	  !gram (Array.to_list trainfamilies) 
	  (*  	  1.0  *)
	  (* 	  (float_of_int (i)) *);
(*  	gram := Structure.replacement ~shrink:0.1 !gram (Array.to_list trainfamilies);  *)
	logging i !gram;
      done;

      Svg.finish x;
;;
