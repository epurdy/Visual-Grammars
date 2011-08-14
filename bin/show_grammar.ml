open Util.Misc
open Util.Cops
open Printf
open Abstract
open Grammar

let nperpage = 6


let prod_cost comp shape =
  match comp.cdata.geom with
      Improper ->
	comp.cdata.cost
    | Parzen model ->
	comp.cdata.cost +. (Parzen.cost model shape)
    | Watson model ->
	comp.cdata.cost +. Watson.cost model shape

let draw_prod_curves im bds granularity prod =
  let draw_scurve prod im scurve value =
    for i = 0 to (Array.length scurve) - 2 do
      let p1 = Bounds.bounds_to_array_nice bds granularity scurve.(i) in
      let p2 = Bounds.bounds_to_array_nice bds granularity scurve.(i+1) in
	Draw.draw_line im p1 p2 value;
    done
  in
    draw_scurve prod im prod.cdata.lcurve (255,0,0);
    draw_scurve prod im prod.cdata.rcurve (0,255,0);
    draw_scurve prod im prod.cdata.ocurve (0,255,255)

let log_cutoff = 100.

let draw_production_standard prod =
  let granularity = 100 in
  let size = granularity + 1 in
  let im = Image.create size size 0. in
  let lcurve, rcurve, ocurve = prod.cdata.lcurve, prod.cdata.rcurve, prod.cdata.ocurve in
  let bds = Bounds.nice_curve_bounds (Array.concat [lcurve; rcurve; ocurve]) in
  let p,q = lcurve.(0), rcurve.((Array.length rcurve)-1) in
  let mincost = ref infinity in
  let maxcost = ref neg_infinity in
  let _ = 
    for x = 0 to granularity do
      for y = 0 to granularity do
	let z = Bounds.array_to_bounds bds granularity (x,y) in
	let shape = Shape.shape_of_complex_bme p z q in
	let cost = prod_cost prod shape in
	  mincost := min !mincost cost;
	  maxcost := max !maxcost cost;
	  Image.set im cost (x,y);
      done
    done;
  in
  let im = Image.map
    begin fun cost ->
      let value = (cost -. !mincost) /. (!maxcost -. !mincost) in
      let value = value ** 0.5 in
	(* let value = (cost -. !mincost) /. (log_cutoff -. !mincost) in *)
	(* let value = cost /. log_cutoff in  *)
      let value = min value 1. in
      let pixval = round (255. *. (1. -. value)) in
	Image.color_of_gray pixval
    end
    im
  in
    draw_prod_curves im bds granularity prod;
    im

let draw_grammar_best_rules namer gram =
  let stuff = sample_counts gram in
    Array.iteri
      begin fun rank (count, cid) -> 
	let im = ref None in
	  iter_all_compositions gram
	    begin fun prod ->
	      if prod.cid = cid then
		im := Some (draw_production_standard prod);
	    end;
	  (* save it with an appropriate name - best.RANK.COUNT.PID.ppm, eg *)
	  Pnm.save_ppm (get !im) (namer rank count cid);
      end
      (Array.of_list stuff)


let draw_grammar namer gram =
  iter_all_compositions gram
    begin fun prod ->
      let im = draw_production_standard prod in
	Pnm.save_ppm im (namer prod.cid)
    end

let draw_curves outfile title namer curves = 
  let fnames = Array.init (Array.length curves) namer in
  let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in
    doit (sprintf "./show_curves.native -fname %s -title '%s' %s" outfile title fnames)

let draw_distorted_curves gram namer nsamples =
  let fn prod = 
    let lcurve, rcurve =  prod.cdata.lcurve, prod.cdata.rcurve in
    let bg, md, en = lcurve.(0), rcurve.(0), rcurve.(Array.length rcurve - 1) in
    let samples = ref [] in

      if true (* bg != en *) then begin
	for i = 1 to nsamples do
	  let _,new_md,_ = sample_prod prod bg en in
	    
	  let new_lcurve = 
	    Array.map 
	      begin fun oldp ->
		let oldshape = Shape.shape_of_complex_bme bg oldp md in
		let newp = Shape.place_unsafe oldshape bg new_md in
		  newp
	      end
	      lcurve
	  in

	  let new_rcurve = 
	    Array.map 
	      begin fun oldp ->
		let oldshape = Shape.shape_of_complex_bme md oldp en in
		let newp = Shape.place_unsafe oldshape new_md en in
		  newp
	      end
	      rcurve
	  in	    
	    samples := (Array.append new_lcurve new_rcurve) :: !samples;
	done;
      end;
      let samples = Array.of_list !samples in
	Curve.save_all  (sprintf "tmp/prod.%d.curve") samples;
	draw_curves (namer prod.cid) "" (sprintf "tmp/prod.%d.curve") samples;
  in
    iter_all_compositions gram fn

let get_samples gram nsamples = 
  let p, q = c0, cxre 1000. in
  let samples = Array.init nsamples (fun i -> Grammar.sample gram p q) in
  let samples = Array.map (Curve.normalize ~scale:1000.) samples in
    samples

let _ =
  let gramfile = ref "NOGRAMFILE" in
  let dir = ref "NODIR" in
  let latexdir = ref "NODIR" in
  let nsamples = ref 20 in
  let title = ref "NO TITLE SELECTED" in
  let show_rules = ref false in
  let draw_rules = ref false in
  let draw_midpoints = ref false in

  (* parse args *)
  let _ = Arg.parse ["-gramfile", Arg.Set_string gramfile, "File containing grammar.";
		     "-dir", Arg.Set_string dir, "Directory to put files in. Should exist already.";
		     "-latexdir", Arg.Set_string latexdir, 
		     "Directory where latex thinks the files live. Should just be dir from a different root. Leave this out if not needed.";
		     "-title", Arg.Set_string title, "Title to put above grammar samples.";
		     "-nsamples", Arg.Set_int nsamples, "How many samples? (default=20)";
		     "-rules", Arg.Set draw_rules, "Make pictures of rules";
		     "-showrules", Arg.Set show_rules, "Write rules out";
		     "-midpoints", Arg.Set draw_midpoints, "Make pictures of midpoint distributions (need -rules)";
		    ]
    (placeholder "Arg.parse")
		     ("./prog -gramfile GRAMFILE -dir DIR -title TITLE [-nsamples N] [-rules [-midpoints]]")
  in
  let _ = 
    if !latexdir = "NODIR" then
      latexdir := !dir;
  in


  let gram = (Marshal.from_channel (open_in !gramfile): Grammar.grammar) in
  let samples = get_samples gram !nsamples in
  let curvenamer = (sprintf "%s/sample.%04d.curve" !dir)  in

  let tex = open_out (sprintf "%s/foo.tex" !dir) in

    Grammar.print_grammar gram;
    Curve.save_all curvenamer samples;
    let fnames = Array.init !nsamples curvenamer in
    let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in
      doit (sprintf "./show_curves.native -fname %s/samples.svg -title '%s' %s" !dir !title fnames);
      doit (sprintf "inkscape %s/samples.svg -E %s/samples.eps" !dir !dir);
      fprintf tex "Here are some samples from the grammar:\n\n";
      fprintf tex "\\includegraphics[width=6in]{%s/samples.eps}\n\n" !latexdir;


      let comp_namer = sprintf "%s/gram.%04d.ppm" !dir in
      let comp_namer_eps = sprintf "%s/gram.%04d.eps" !dir in
      let comp_namer_eps_latex = sprintf "%s/gram.%04d.eps" !latexdir in
      let comp_sample_namer = sprintf "%s/gram.%04d.sample.svg" !dir in
      let comp_sample_namer_eps = sprintf "%s/gram.%04d.sample.eps" !dir in
      let comp_sample_namer_eps_latex = sprintf "%s/gram.%04d.sample.eps" !latexdir in

	if !show_rules then begin

	  if !draw_midpoints then begin
	    draw_grammar (comp_namer) gram;
	    (*     draw_grammar_best_rules (sprintf "%s/best.rk%03d.ct%d.pid%04d.ppm" !dir) gram; *)
	  end;

	  if !draw_rules then begin
	    draw_distorted_curves gram comp_sample_namer 10;
	  end;

	  fprintf tex "here are the rules\n\n";
	  fprintf tex "\\begin{tabular}{|l|c|c|}\n\\hline\n";

	  iter_all_compositions gram
	    begin fun prod ->
	      fprintf tex "$S_{%d} \\to S_{%d} S_{%d}, P=%f$ & " prod.topsid prod.leftsid prod.rightsid prod.cdata.prob;

	      if !draw_midpoints then begin
		doit (sprintf "convert %s %s" (comp_namer prod.cid) (comp_namer_eps prod.cid));
		fprintf tex "\\includegraphics[height=1in]{%s} " (comp_namer_eps_latex prod.cid);
	      end;
	      fprintf tex " & ";

	      if !draw_rules && (prod.cdata.geom != Improper) then begin
		doit (sprintf "inkscape %s -E %s" (comp_sample_namer prod.cid) (comp_sample_namer_eps prod.cid));
		fprintf tex "\\includegraphics[height=1in]{%s} " (comp_sample_namer_eps_latex prod.cid);
	      end;
	      fprintf tex "\\\\\n\\hline\n";

	      if prod.cid mod nperpage = nperpage - 1 then begin
		fprintf tex "\\end{tabular}\n\n\\begin{tabular}{|l|c|c|}\n\n\\hline\n"
	      end
	    end;
	  fprintf tex "\\end{tabular}\n";
	  fprintf tex "\\begin{tabular}{|l|c|}\n\n\\hline\n";
	  Frozen.iter_symbols gram
	    begin fun sym ->
	      if sym.sdata.straightprob > 0.01 then begin
		fprintf tex "$S_{%d} \\to \\ell, P=%f$ & \\\\\n\\hline\n" sym.sid sym.sdata.straightprob;
	      end
	    end;
	  fprintf tex "\\end{tabular}\n";
	end;

	close_out tex;
	()

