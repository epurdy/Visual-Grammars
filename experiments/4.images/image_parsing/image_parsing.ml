open Printf
open Util.Misc
open Util.Hops
open Util.Cops
open Abstract
open Grammar
open Graph

module I = Image

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
let canon p q = if p > q then q,p else p,q

type sid = int
type cid = int
type pid = int

type split = Lexical | Binary of cid * pid | Lift of pid * pid

type image_parse_data = {
  costs: (sid*pid*pid, float) hash;
  split: (sid*pid*pid, split) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
}

type image_parse_tree = {
  lvl: int;
  nt: sid;
  bg: pid;
  en: pid;
  yield: (pid*pid) list;
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

let rec harvest_best_tree gram pdatas nt lvl (bg,en) = 
  match pdatas.(lvl).split >> (nt,bg,en) with
      Binary (cid, q) ->
	let prod = Frozen.get_composition gram cid in
	let left = harvest_best_tree gram pdatas prod.leftsid lvl (bg,q) in
	let right = harvest_best_tree gram pdatas prod.rightsid lvl (q,en) in
	  {lvl = lvl;
	   nt=nt; bg=bg; en=en;
	   yield = left.yield @ right.yield;
	   left = Some left;
	   right = Some right;
	   qual = pdatas.(lvl).costs >>! ((nt,bg,en), infinity);
	   thesplit = Some (cid,q);
	  }
    | Lift (bg,en) ->
	harvest_best_tree gram pdatas nt (lvl-1) (bg,en)
    | Lexical ->
	{lvl = (assert (lvl=0); lvl);
	 nt=nt; bg=bg; en=en;
	 yield = [(bg, en)];
	 left = None;
	 right = None;
	 qual = pdatas.(lvl).costs >>! ((nt,bg,en), infinity);
	 thesplit = None;
	}

let harvest_best_tree_overall gram pdatas = 
  let top = Array.length pdatas-1 in
  let start = Frozen.start gram in
    assert ( pdatas.(top).thisbest >>? start.sid );
    let p,_ (* also p *) = pdatas.(top).thisbest >> start.sid in
    let rv = harvest_best_tree gram pdatas start.sid top (p, p) in
      rv

let print_parse_tree t nets step = 
  let rec printer t tabs =
    let px, py = nets.(t.lvl).vertices.(t.bg) in
    let qx, qy = nets.(t.lvl).vertices.(t.en) in
      printf "%s s%d -> (%d,%d) (%d,%d) (lvl=%d, |yield|=%d)\n" (String.make (tabs+1) '*') 
	t.nt (px/step) (py/step) (qx/step) (qy/step)
	t.lvl
	(List.length t.yield);
      if t.left != None then
	printer (get t.left) (tabs+1);
      if t.right != None then
	printer (get t.right) (tabs+1)
  in
    printer t 0

let draw_parse_tree t nets im step =
  let colors = [| 
    (0,0,255);   (* dark blue *)
    (255,255,0); (* yellow *)
    (0,255,255); (* cyan *)
    (255,0,0); (* red *)
    (0,255,0); (* green *)
    (255,0,255); (* purple *)
    (0,0,0); (* black *)
    (0,0,0); (* black *)
    (0,0,0); (* black *)
    (0,0,0); (* black *)
    (0,0,0); (* black *)
    (0,0,0); (* black *)
  |] in
  let rec draw t depth =
    let px, py = nets.(t.lvl).vertices.(t.bg) in
    let qx, qy = nets.(t.lvl).vertices.(t.en) in
      if t.left != None then
	draw (get t.left) (depth+1);
      if t.right != None then
	draw (get t.right) (depth+1);
      Draw.draw_line im (px,py) (qx,qy) colors.(depth);
  in
    printf "about to draw\n%!";
    draw t 0;
    printf "finished drawing\n%!"

let save_parse_tree t nets fname step = 
  let chan = open_out fname in
  let rec printer t tabs =
    let px, py = nets.(t.lvl).vertices.(t.bg) in
    let qx, qy = nets.(t.lvl).vertices.(t.en) in
      fprintf chan "%s s%d -> (%d,%d) (%d,%d) (lvl=%d, |yield|=%d)\n" (String.make (tabs+1) '*') 
	t.nt (px/step) (py/step) (qx/step) (qy/step)
	t.lvl
	(List.length t.yield);
      if t.left != None then
	printer (get t.left) (tabs+1);
      if t.right != None then
	printer (get t.right) (tabs+1)
  in
    printer t 0;
    close_out chan

let draw_changing_segments im segments closed = 
  Array.iteri
    begin fun i (p,q) -> 
      let counter = i mod 6 in
      let color = (counter * 50, 0, 255 - counter * 50) in
	Draw.draw_line im p q color;
    end
    segments

let draw_segments im segments closed = 
  Array.iteri
    begin fun i (p,q) -> 
      Draw.draw_line im p q (0,128,255);
    end
    segments

let new_parse_data () = 
  {costs = mkhash 1000;
   split = mkhash 1000;
   thisbest = mkhash 100;
   thisbestqual = mkhash 100;
  }

let parse_image gram nets pdatas maxlen imname prefix = 
  let net = nets.(Array.length nets-1) in
  let n = Graph.nvertices net in
  let data = pdatas.(Array.length pdatas-1) in

  let nsymbols = Frozen.num_symbols gram in
  let ntchanged = Array.init nsymbols (fun x -> true) in
  let changed = ref true in
  let iter = ref (-1) in

  let rule_cost pdata net gram sym prod (p,q,r) = 
    let leftkey, rightkey = (prod.leftsid, p, q), (prod.rightsid, q, r) in
    let leftcost  = data.costs >>! (leftkey,  infinity) in
    let rightcost = data.costs >>! (rightkey, infinity) in
    let shape = Shape.shape_of_ints_bme 
      net.vertices.(p) net.vertices.(q) net.vertices.(r) in
      leftcost +. rightcost +. (Models.Simple.prod_cost prod shape)
  in
    
  let display sym nt (bg,en) final =
    let im = Pnm.load_ppm imname in
(*     let im = Image.map Image.color_of_gray im in *)
    let ptree = harvest_best_tree gram pdatas nt (Array.length pdatas - 1) (bg,en) in
    let segments = List.map 
      begin fun (p,q) -> 
	nets.(0).vertices.(p), nets.(0).vertices.(q)
      end
      ptree.yield 
    in
    let fname =
      if final then (sprintf "%s.iter%d.nt%d.ppm" prefix !iter nt)
      else (sprintf "%s.final.ppm" prefix)
    in
      draw_segments im (Array.of_list segments) sym.startable;
      Pnm.save_ppm im fname
  in

    fprintf stderr "need to size these hashes better!\n";

    let tryout sid p q qual thesplit = 
      let rv = ref false in
	(*       if qual < (data.costs >>! ((sid, p, q), infinity)) then  *)
	(* 	rv := true; *)
	(* vvvv this was <=, not sure why... *)
	if qual < (data.costs >>! ((sid, p, q), infinity)) then begin
	  rv := true;
	  data.costs << ((sid, p, q), qual);
	  data.split << ((sid, p, q), thesplit);
	  if qual < (data.thisbestqual >>! (sid, infinity)) then begin
	    data.thisbestqual << (sid, qual);
	    data.thisbest << (sid, (p,q));
	  end;
	end;
	!rv
    in
    let tryout_ignore sid p q qual thesplit = ignore (tryout sid p q qual thesplit) in

      printf "Grammar has %d symbols\n%!" nsymbols;
      Frozen.iter_symbols gram
	begin fun sym ->
	  if not sym.startable then begin
	    (* 	  for p = 0 to n-1 do *)
	    (* 	    tryout_ignore sym.sid p p (?) None; *)
	    (* 	  done; *)
	    Hashtbl.iter
	      begin fun (p,q) cost ->
		let cost = cost +. sym.sdata.straightcost in
		  tryout_ignore sym.sid q p cost Lexical;
		  tryout_ignore sym.sid p q cost Lexical;
	      end
	      net.edges;
	  end
	end;

      printf "About to start actual parsing!\n%!";
      while !changed do
	incr iter;
	changed := false;

	Frozen.iter_symbols_rev gram
	  begin fun sym ->
	    if !iter > 0 then begin
	      (* If it doesn't change in the loop to come, then every
		 other symbol has seen all its values already. BUT, if
		 we are in the first iteration, other symbols have not
		 seen this one's single-segment cost. *)
	      ntchanged.(sym.sid) <- false;
	    end;

	    Frozen.iter_decompositions gram sym
	      begin fun prod ->
		printf "%s %d: s%d(/%d) -> s%d s%d (c%d)\n%!" prefix !iter prod.topsid 
		  nsymbols prod.leftsid prod.rightsid prod.cid;
		if ntchanged.(prod.leftsid) || ntchanged.(prod.rightsid) then begin
		  let ntiter, nchanged = ref 0, ref 17 in
		    (* 		  while !nchanged > 0 do *)
		    for foofoofoo = 1 to 1 do
		      nchanged := 0;
		      incr ntiter;

		      for p = 0 to n-1 do
			for q = 0 to n-1 do 
			  
(* 			  printf "d v maxlen: %d < %d? %b\n"  *)
(* 			    (dist2 net.vertices.(p) net.vertices.(q)) *)
(* 			    (maxlen * maxlen) *)
(* 			    (dist2 net.vertices.(p) net.vertices.(q) < maxlen * maxlen); *)
			  if (dist2 net.vertices.(p) net.vertices.(q) < maxlen * maxlen) &&
			    (data.costs >>! ((prod.leftsid, p, q),infinity) < infinity) then begin
			      for r = 0 to n-1 do
				(* may want to add the rule back in that L->LL must have d(p,r) > d(p,q), d(q,r) *)
				if (dist2 net.vertices.(q) net.vertices.(r) < maxlen * maxlen) &&
				  ((not sym.startable) || (p=r))
				  (* 			      (sym.startable = (p=r)) *)
				then begin
				  let cost = rule_cost data net gram sym prod (p,q,r) in
				    (* 				if cost < infinity then *)
				    (* 				  printf "cost = %f\n%!" cost; *)
				    if tryout sym.sid p r cost (Binary (prod.cid, q)) then begin
				      changed := true;
				      incr nchanged;
				    end
				end
			      done
			    end
			done
		      done;

		      printf "%d changed\n" !nchanged;
		      if !nchanged > 0 then 
			ntchanged.(sym.sid) <- true;
		    done; (* done with "while !nchanged > 0 do" *) 
		end;
	      end; (* end iter_decompositions *)

	    if data.thisbest >>? sym.sid then begin
	      let p,q = data.thisbest >> sym.sid in
		printf "\ts%d -> p%d p%d: %f\n%!" sym.sid p q (data.thisbestqual >> sym.sid);
		if sym.sid = 0 &&  !iter > 0 (* true *) then begin
		  printf "displaying s%d\n%!" sym.sid;
		  display sym sym.sid (p,q) false;
		  printf "finished displaying s%d\n%!" sym.sid;
		end
	    end
	    else begin 
	      printf "\ts%d -> ????\n%!" sym.sid;
	    end
	  end; (* end iter_symbols *)
      done; (* end while !nchanged *)

      let start = Frozen.start gram in
	if not (data.thisbest >>? start.sid) then begin
	  printf "\n>>> Failed to parse!\n\n%!";
	  exit (-1);
	end;
	display start start.sid (data.thisbest >> start.sid) true;


	()

let coarsen ptable net step = 
  let idmap = mkhash 1000 in
  let coarse_net = new_live_graph () in
  let coarse_ptable = new_parse_data () in

  let coarsen_point (x,y) = 
    step * (x/step), step * (y/step)
  in
    (*   let coarsen_point (x,y) =  *)
    (*     (step/2) + step * (x/step), (step/2) + step * (y/step) *)
    (*   in *)

    (* coarsen net points, remembering where they are mapped to *)
    Array.iteri
      begin fun i (x,y) ->
	let x, y = coarsen_point (x, y) in
	let i' = add_vertex coarse_net (x, y) in
	  idmap << (i,i');
      end
      net.vertices;

    (* Note that we do not bother lifting the segments! Their values
       are put into the finest parse table, so as long as we lift the
       parse table entries, we're good. *)

    (* lift parse table entries *)
    Hashtbl.iter
      begin fun (sid,p,q) oldcost ->
	let (px,py), (qx,qy) = net.vertices.(p), net.vertices.(q) in
	  if dist2 (px,py) (qx,qy) >= step then begin
	    let p',q' = idmap >> p, idmap >> q in
	    let newkey = (sid, p', q') in
	      if oldcost < (coarse_ptable.costs >>! (newkey, infinity)) then begin 
		coarse_ptable.costs << (newkey, oldcost);
		coarse_ptable.split << (newkey, Lift (p,q) );
	      end
	  end
      end
      ptable.costs;

    coarse_ptable, (Graph.finalize coarse_net)

let _ = 

  let excurve = Curve.load Sys.argv.(2) in
  let sdf = Sdf.load_family Sys.argv.(3) in
  let netname = Sys.argv.(4) in
  let yieldprefix = Sys.argv.(5) in
  let parseprefix = Sys.argv.(6) in
    
  (*   let granularity = 16 in *)
  let granularity = 32 in
  let maxlen = 8 (* 5 *) in

  let net = Graph.load_point_float_graph Sys.argv.(7) in
  let size = 
    let size = ref 0 in
      Array.iter 
	begin fun (x,y) ->
	  size := max !size x;
	  size := max !size y;
	end
	net.vertices;
      printf "size=%d\n%!" (!size + granularity);
      !size + granularity (* hopefully? *)
  in

  let step = size / granularity in

  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = merge_leaves gram in
  let _ = reorder_symbols gram in

  let nets = ref [| net |] in
  let ptables = ref [| new_parse_data () |] in
    (*   let lift_ptables = ref [| None |] in *)

  let thegran = ref granularity in


  let report () = 
    let ptree = harvest_best_tree_overall gram !ptables in
    let im = Pnm.load_ppm Sys.argv.(4) in
      (*     let im = Image.map Image.color_of_gray im in  *)
      save_parse_tree ptree !nets (sprintf "%s.%d.parse" parseprefix !thegran) step;
      draw_parse_tree ptree !nets im step;
      Pnm.save_ppm im (sprintf "%s.%d.ppm" parseprefix !thegran);
      print_parse_tree ptree !nets step
  in

    while 1.414 *. (float_of_int !thegran) > float_of_int maxlen do
      let prefix = yieldprefix ^ (sprintf ".x%d" !thegran) in
      let thestep = size / !thegran in

	printf "Doing scale %d\n%!" !thegran;
	parse_image gram !nets !ptables (* !lift_ptables *)
	  (maxlen * thestep) netname prefix;

	report ();

	thegran := !thegran / 2;

	printf "about to coarsen!\n%!";
	let coarse_ptable, coarse_net = coarsen 
	  !ptables.(Array.length !ptables-1) 
	  !nets.(Array.length !nets-1) 
	  (thestep * 2)
	in      
	  ptables := Array.append !ptables [| coarse_ptable |];
	  (* 	  lift_ptables := Array.append !lift_ptables [| coarse_ptable |]; *)
	  nets := Array.append !nets [| coarse_net |];
	  printf "finished with coarsen!\n%!";
    done;

