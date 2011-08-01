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

(* let dir = "em.d";; *)
let dir = "experiments/3.em/simple_tuning/simple_tuning.d";;
let latexdir = "./3.em/simple_tuning/simple_tuning.d";;

let choose_spaced arr nchoice = 
  let last = Array.length arr - 1 in
  let factor = (float_of_int last) /. (float_of_int (nchoice-1)) in
  let rv = ref [] in      
    for i = 0 to nchoice-1 do
      rv := arr.(round (float_of_int i *. factor)) :: !rv;
    done;
    Array.of_list (List.rev !rv);;

let _ = 
  (*********************)
  (* assemble examples *)
  (*********************)
  (*   let training = Datasets.get_hand () in *)
  (*   let examples = [| training.(0); |] in *)
  (*   let training = Array.append (Datasets.get_articulator ~n:10 ()) (Datasets.get_hand ~n:10 ()) in *)
  (*   let examples = [| training.(0); training.(10); training.(12) |] in *)

  (* let training = Datasets.get_romer ~n:20 () in *)
  (*   let training = Array.map (fun c -> Curve.subsample_dp c npoints 2) training in *)
  (* let examples = [| training.(0); training.(1); training.(10);
     training.(11); |] in *)
  (* let examples = choose_spaced training ngrammars in *)

  let training = Curve.load_all (sprintf "romer/ann/curve%03d0.curve") 0 16 in
  let examples = [| training.(0) |] in

  (*********************)
  (* construct grammar *)
  (*********************)
  let exfamilies = [| (training.(0), Sdf.load_family "romer/misc/romer1.sdf") |] in
  let grams = Array.map Models.Simple.make_grammar exfamilies in
  let gram = Grammar.merge_tops grams in

  (**************************)
  (* assemble training data *)
  (**************************)
  let trainfamilies = Array.map
    begin fun c -> 
      (*       let fam = Sdf.make_sparse_family (Array.length c) k2 in *)
      let fam = Sdf.make_full_family (Array.length c) in
	Models.Simple.add_curve_data_to_family c fam
    end 
    training in

  let _ = 
    doit (sprintf "mkdir -p %s" dir);
    let show_curves curves title = 
      let curvenamer = (sprintf "tmp/curve.%04d.curve")  in
      let fnames = Array.init (Array.length curves) curvenamer in
      let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in
	Curve.save_all curvenamer curves;
	doit (sprintf "./show_curves.native -fname %s/%s.svg -title '%s' %s"  dir title title fnames);
    in
(*     let examples = Array.map Curve.normalize examples in *)
(*     let training = Array.map Curve.normalize training in *)

      show_curves examples "examples";
      show_curves training "training";
  in

  let logging i gram = 
    let file = open_out (sprintf "tmp/logging.%d.gram" i) in
      Marshal.to_channel file gram [];
      close_out file;
      printf "LOGGING %d\n%!" i;
      Grammar.print_grammar gram;
      doit (sprintf "mkdir -p %s/gram.%d.d" dir i);
      doit (sprintf "rm -rf %s/gram.%d.d/*" dir i);
      if (* i > 0 *) false (* i = niters *) then 
	doit (sprintf "./show_grammar.native -gramfile tmp/logging.%d.gram -dir %s/gram.%d.d -latexdir %s/gram.%d.d -title 'round #%d' -rules" i dir i latexdir i i)
      else
	doit (sprintf "./show_grammar.native -gramfile tmp/logging.%d.gram -dir %s/gram.%d.d -latexdir %s/gram.%d.d -title 'round #%d' " i dir i latexdir i i)
  in

    logging 0 gram;

    let gram = ref gram in
      for i = 1 to niters do
	gram := Retrain.retrain
	  ~sparsefactor:(float_of_int i)
	  !gram (Array.to_list trainfamilies) 
	  Models.Simple.strat;
	(* gram := Structure.replacement ~shrink:0.1 !gram (Array.to_list trainfamilies);  *)
	logging i !gram;
      done
