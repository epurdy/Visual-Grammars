open Printf
open Util.Misc
open Util.Hops
open Util.Cops
open Abstract
open Grammar
open Curve_network

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

let print_parse_tree t = 
  let rec printer t tabs =
    printf "%s  s%d -> p%d p%d (|yield|=%d)\n" (String.make (2*tabs) ' ') 
      t.nt t.bg t.en 
      (List.length t.yield);
    if t.left != None then
      printer (get t.left) (tabs+1);
    if t.right != None then
      printer (get t.right) (tabs+1)
  in
    printer t 0

let draw_changing_curve im curve closed = 
  let n = Array.length curve in
  let maxi = if closed then n-1 else n-2 in
  for i = 0 to maxi do
    let counter = i mod 6 in
    let color = (counter * 50, 0, 255 - counter * 50) in
      Draw.draw_line im curve.(i) curve.((i+1) mod n) color;
  done

let new_parse_data () = 
  {costs = mkhash 1000;
   split = mkhash 1000;
   thisbest = mkhash 100;
   thisbestqual = mkhash 100;
   }

let parse_image gram finest_net net pdatas maxlen imname prefix = 
  let n = Curve_network.npoints net in
  let data = pdatas.(Array.length pdatas-1) in

  let nsymbols = Frozen.num_symbols gram in
  let ntchanged = Array.init nsymbols (fun x -> true) in
  let changed = ref true in
  let iter = ref (-1) in

  let rule_cost pdata net gram sym prod (p,q,r) = 
    let leftkey, rightkey = (prod.leftsid, p, q), (prod.rightsid, q, r) in
    let leftcost  = data.costs >>! (leftkey,  infinity) in
    let rightcost = data.costs >>! (rightkey, infinity) in
    let shape = Shape.shape_of_ints_bme net.points.(p) net.points.(q) net.points.(r) in
      leftcost +. rightcost +. (Models.Simple.prod_cost prod shape)
  in
    
  let display sym nt (bg,en) =
    let im = Pnm.load_pgm imname in
    let im = Image.map Image.color_of_gray im in
      printf "about to harvest\n%!";
    let ptree = harvest_best_tree gram pdatas nt (Array.length pdatas - 1) (bg,en) in
      printf "about to get yields\n%!";
    let curve = List.map (fun (p,q) -> finest_net.points.(p)) ptree.yield in
      printf "about to draw\n%!";
      draw_changing_curve im (Array.of_list curve) sym.startable;
      Pnm.save_ppm im (sprintf "%s.iter%d.nt%d.ppm" prefix !iter nt)
  in

    drawing these wrong, need to look at both endpoints

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

    printf "About to add segment costs!\n%!";
    printf "Grammar has %d symbols\n%!" nsymbols;
    Frozen.iter_symbols gram
      begin fun sym ->
	if not sym.startable then begin
	  printf "Have to add %d things\n%!" (Hashtbl.length net.segments);
	  (* 	  for p = 0 to n-1 do *)
	  (* 	    tryout_ignore sym.sid p p (?) None; *)
	  (* 	  done; *)
	  Hashtbl.iter
	    begin fun (p,q) cost ->
	      let cost = cost +. sym.sdata.straightcost in
		tryout_ignore sym.sid q p cost Lexical;
		tryout_ignore sym.sid p q cost Lexical;
	    end
	    net.segments;
	end
      end;

    printf "About to start actual parsing!\n%!";
    while !changed do
      incr iter;
      changed := false;

      Frozen.iter_symbols gram
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
	      printf ">>> s%d -> s%d s%d\n%!" prod.topsid prod.leftsid prod.rightsid;
	      if ntchanged.(prod.leftsid) || ntchanged.(prod.rightsid) then begin
		let ntiter, nchanged = ref 0, ref 17 in
		  (* 		  while !nchanged > 0 do *)
		  for foofoofoo = 1 to 1 do
		    nchanged := 0;
		    incr ntiter;
		    printf "%d: %d/%d (%d)\n%!" !iter sym.sid nsymbols prod.cid;

		    for p = 0 to n-1 do
		      for q = 0 to n-1 do 
			
			if (dist2 net.points.(p) net.points.(q) < maxlen * maxlen) &&
			  (data.costs >>! ((prod.leftsid, p, q),infinity) < infinity) then begin
			  for r = 0 to n-1 do
			    (* may want to add the rule back in that L->LL must have d(p,r) > d(p,q), d(q,r) *)
			    if (dist2 net.points.(q) net.points.(r) < maxlen * maxlen) &&
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
		display sym sym.sid (p,q);
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

      ()

let coarsen ptable net step = 
  let idmap = mkhash 1000 in
  let coarse_net = new_network () in
  let coarse_ptable = new_parse_data () in

    (* coarsen net points, remembering where they are mapped to *)
    Array.iteri
      begin fun i (x,y) ->
	let x, y = x/step, y/step in
	let x',y' = x/2, y/2 in
	let i' = add_point coarse_net (step * x', step * y') in
	  idmap << (i,i');
      end
      net.points;

    (* Note that we do not bother lifting the segments! Their values
       are put into the finest parse table, so as long as we lift the
       parse table entries, we're good. *)

    (* lift parse table entries *)
    Hashtbl.iter
      begin fun (sid,p,q) oldcost ->
	let p',q' = idmap >> p, idmap >> q in
	let newkey = (sid, p', q') in
	  if oldcost < (coarse_ptable.costs >>! (newkey, infinity)) then begin 
	    coarse_ptable.costs << (newkey, oldcost);
	    coarse_ptable.split << (newkey, Lift (p,q) );
	  end
      end
      ptable.costs;

    coarse_ptable, (Curve_network.finalize coarse_net)

let net_of_curve c gran size imname = 
  let step = size / gran in
  let foo (x,y) = (step*x,step*y) in
  let im = Image.create (size+1) (size+1) 255 in
  let net = Curve_network.new_network () in
  let c = Curve.normalize ~scale:(float_of_int gran) c in
  let c = Array.map Geometry.point_of_complex c in
  let c = Array.map foo c in
  let c = Curve.uniqify c in
  let n = Array.length c in
  let maxdist = 3 in

    for x = 0 to gran-1 do
      for y = 0 to gran-1 do
	ignore (Curve_network.add_point net (foo (x,y)));
	Image.set im 0 (x*step,y*step);
      done;
    done;

    for i = 0 to n-1 do 
      Curve_network.add_edge net c.(i) c.((i+1) mod n) 1.;
      Draw.draw_line im c.(i) c.((i+1) mod n) 128;
    done;

    for x1 = 0 to gran-1 do
      for y1 = 0 to gran-1 do
	for x2 = 0 to gran-1 do
	  for y2 = 0 to gran-1 do
	    if dist2 (x1,y1) (x2,y2) < maxdist * maxdist then begin
	      let p, q = foo (x1,y1), foo (x2,y2) in
	      if not (Curve_network.is_edge net p q) then
		Curve_network.add_edge net p q 1000.;
	    end
	  done;
	done;
      done;
    done;

    Pnm.save_pgm im imname;
    printf "Finished building network!\n%!";
    Curve_network.finalize net

let _ = 
(*   let granularity = 16 in *)
  let granularity = 32 in
  let size = 256 in
  let step = size / granularity in
  let maxlen = 5 in

  let curve = Curve.load Sys.argv.(1) in
  let excurve = Curve.load Sys.argv.(2) in
  let sdf = Sdf.load_family Sys.argv.(3) in
  let imname = Sys.argv.(4) in
  let prefix = Sys.argv.(5) in
    
  let net = net_of_curve curve granularity size imname in
  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = merge_leaves gram in

  let nets = ref [| net |] in
  let ptables = ref [| new_parse_data () |] in

  let thegran = ref granularity in

    while 1.414 *. (float_of_int !thegran) > float_of_int maxlen do
      let prefix = prefix ^ (sprintf ".x%d" !thegran) in
	printf "Doing scale %d\n%!" !thegran;
	thegran := !thegran / 2;

	parse_image gram !nets.(0) !nets.(Array.length !nets-1) !ptables (maxlen*step) imname prefix;
	let pdata, net = coarsen 
	  !ptables.(Array.length !ptables-1) 
	  !nets.(Array.length !nets-1) step in      
	  ptables := Array.append !ptables [| pdata |];
	  nets := Array.append !nets [| net |];
    done;

    let ptables = Array.sub !ptables 0 (Array.length !ptables - 1) in

    let ptree = harvest_best_tree_overall gram ptables in
      print_parse_tree ptree
