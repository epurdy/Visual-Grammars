open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph
open Filtration
open Sdf

module C = Complex
module I = Image

let get_samples gram nsamples = 
  let p, q = c0, cxre 1000. in
  let samples = Array.init nsamples (fun i -> Grammar.sample gram p q) in
  let samples = Array.map (Curve.normalize ~scale:1000.) samples in

  (* points are doubled for some reason *)
  let samples = Array.map
    (fun arr -> Array.init (Array.length arr / 2) (fun i -> arr.(2*i)))
    samples
  in
    samples


let _ = 
  let excurve = Curve.load "romer/newann/IMG0020.curve" in
  let sdf = Sdf.load_family "romer/misc/romer1.sdf" in
  let gram = Models.Simple.make_grammar (excurve, sdf) in
(*   let gram = merge_leaves gram in *)

  let fold = (11,16) in
  let pid, qid = fold in
  let p,q = excurve.(pid), excurve.(qid) in
  let c2_3, c1_3 = {C.re=2./.3.; C.im=0.}, {C.re=1./.3.; C.im=0.} in
  let p',q' = c2_3*&p +& c1_3*&q, c1_3*& p +& c2_3*&q in

  let thescurve = Array.sub excurve pid (qid-pid+1) in
  let lencurve = Array.length thescurve in
    
  let ntid = 
    let ntid = ref (-1) in
      Frozen.iter_symbols gram
	begin fun sym -> 
	  if sym.sdata.curve = thescurve then
	    ntid := sym.sid;
	end;
      assert(!ntid <> (-1));
      !ntid
  in

  let traverse_root = 
    let tr = ref None in
      Frozen.iter_symbols sdf
	begin fun scurve ->
	  if (scurve.sdata.first,scurve.sdata.last) = (pid,qid) then
	    tr := Some scurve;
	end;
      assert(!tr <> None);
      get !tr
  in

  (* insert p' and q' into c, and flip interior *)
  let newcurve = Array.init lencurve (fun i -> thescurve.(lencurve-1-i)) in
  let newcurve = Array.concat [
    [| p; p'|]; newcurve; [| q'; q |]
  ] in
  let lennewcurve = Array.length newcurve in
    
  (* flip the curve so that geometry is flipped *)
  let newcurve = Array.map C.conj newcurve in

  let _ = printf "start making family\n%!" in

  let newsdf = make_live_family lennewcurve lennewcurve in
  let newfoldroot =
    newsdf.make_new_scurve  (0,lennewcurve-1) (lennewcurve-1); (* p p' rev q' q *)
  in
  let _ = 

    newsdf.imake_new_scurve (0,lennewcurve-2) (lennewcurve-2); (* p p' rev q'   *)
    newsdf.imake_new_scurve (1,lennewcurve-2) (lennewcurve-3); (*   p' rev q'   *)
    newsdf.imake_new_scurve (1,lennewcurve-3) (lennewcurve-4); (*   p' rev      *)
    newsdf.imake_new_scurve (2,lennewcurve-3) (lennewcurve-5); (*      rev      *)

    newsdf.imake_new_scurve (0,1) 1;
    newsdf.imake_new_scurve (1,2) 1;
    newsdf.imake_new_scurve (lennewcurve-3,lennewcurve-2) 1;
    newsdf.imake_new_scurve (lennewcurve-2,lennewcurve-1) 1;

    newsdf.imake_new_comp (0,lennewcurve-2,lennewcurve-1);
    newsdf.imake_new_comp (0,1,lennewcurve-2);
    newsdf.imake_new_comp (1,lennewcurve-3,lennewcurve-2);
    newsdf.imake_new_comp (1,2,lennewcurve-3);
  in
    
  let translate idx =
    2 + qid - idx 
      (* qid -> 2 *)
      (* pid -> 2 + qid - pid = lennewcurve-3 *)
  in

  let _ = printf "start traversal\n%!" in

  let seen = mkhash 100 in
  let rec traverse scid create =
    let oldthing = Frozen.get_symbol sdf scid in
      if create then
	newsdf.imake_new_scurve
	  (translate oldthing.sdata.last, translate oldthing.sdata.first)
	  oldthing.sdata.len;


      seen << (scid, ());

      Frozen.iter_decompositions sdf oldthing
	begin fun dcomp ->
	  if not (seen >>? dcomp.leftsid) then
	    traverse dcomp.leftsid true;
	  if not (seen >>? dcomp.rightsid) then
	    traverse dcomp.rightsid true;

	  newsdf.imake_new_comp
	    (translate dcomp.cdata.en, 
	     translate dcomp.cdata.md, 
	     translate dcomp.cdata.bg);
	end;

      ()
  in
  let _ = traverse traverse_root.sid false in

  let newsdf = Abstract.finalize newsdf.gram in

  let _ = printf "grow the grammar\n%!" in

  let gram = Models.Simple.grow_grammar gram ntid newfoldroot (newcurve, newsdf) in

  let samples = get_samples gram 20 in
  let curvenamer = (sprintf "folding.d/sample.%04d.curve")  in
  let fnames = Array.init 20 curvenamer in
  let fnames = Array.fold_left (fun s fname -> s ^ " " ^ fname) "" fnames in
    Curve.save_all curvenamer samples;
    doit (sprintf "./show_curves.native -fname folding.d/samples.svg -title '' %s" fnames);    

    let chan = (open_out "folding.d/folded.gram") in
      Marshal.to_channel chan gram [];
      close_out chan;
    
    ()
