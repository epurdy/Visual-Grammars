open Printf
open Util.Misc
open Util.Hops
open Util.Cops
open Abstract
open Grammar
open Graph

let dataweight = 1000. 

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
let canon p q = if p > q then q,p else p,q

type sid = int
type cid = int
type pid = int

type image_parse_data = {
  costs: (sid*pid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
}

type image_parse_tree = {
  nt: sid;
  bg: pid;
  en: pid;
  yield: (pid*pid) list;
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

let rec harvest_best_tree gram pdata nt (bg,en) = 
(*   printf "harvest %d -> p%d p%d\n%!" nt bg en; *)
  match pdata.split >> (nt,bg,en) with
      Some (cid, q) ->
	let prod = Frozen.get_composition gram cid in
	let left = harvest_best_tree gram pdata prod.leftsid (bg,q) in
	let right = harvest_best_tree gram pdata prod.rightsid (q,en) in
	  {nt=nt; bg=bg; en=en;
	   yield = left.yield @ right.yield;
	   left = Some left;
	   right = Some right;
	   qual = pdata.costs >>! ((nt,bg,en), infinity);
	   thesplit = Some (cid,q);
	  }
    | None ->
	{nt=nt; bg=bg; en=en;
	 yield = [(bg, en)];
	 left = None;
	 right = None;
	 qual = pdata.costs >>! ((nt,bg,en), infinity);
	 thesplit = None;
	}

let harvest_best_tree_overall gram pdata = 
  let start = Frozen.start gram in
  let p,_ (* also p *) = pdata.thisbest >> start.sid in
  let rv = harvest_best_tree gram pdata start.sid (p, p) in
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

let draw_curve im curve closed color = 
  let n = Array.length curve in
  let maxi = if closed then n-1 else n-2 in
  for i = 0 to maxi do
    Draw.draw_line im curve.(i) curve.((i+1) mod n) color;
  done

let new_parse_data () = 
  {costs = mkhash 1000;
   split = mkhash 1000;
   thisbest = mkhash 100;
   thisbestqual = mkhash 100;
   }

let parse_image_cheating gram net imname prefix curve = 
  let n = Graph.nvertices net in
  let data = new_parse_data () in

  let nsymbols = Frozen.num_symbols gram in
  let ntchanged = Array.init nsymbols (fun x -> true) in
  let changed = ref true in
  let iter = ref (-1) in

  let rule_cost pdata net gram sym prod (p,q,r) = 
    let leftkey, rightkey = (prod.leftsid, p, q), (prod.rightsid, q, r) in
    let leftcost  = data.costs >>! (leftkey,  infinity) in
    let rightcost = data.costs >>! (rightkey, infinity) in
    let shape = Shape.shape_of_ints_bme net.vertices.(p) net.vertices.(q) net.vertices.(r) in
      leftcost +. rightcost +. (Models.Simple.prod_cost prod shape)
  in
    
  let display sym nt (bg,en) =
    let im = Pnm.load_ppm imname in
    let ptree = harvest_best_tree gram data nt (bg,en) in
    let curve = List.map (fun (p,q) -> net.vertices.(p)) ptree.yield in
      draw_curve im (Array.of_list curve) sym.startable (0,255,255);
      Pnm.save_ppm im (sprintf "%s.iter%d.nt%d.ppm" prefix !iter nt)
  in

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

  let nc = Array.length curve in 
    for i=0 to nc-1 do 
      let p,q = curve.(i), curve.((i+1) mod nc) in
      let bestp, bestq, bestpdist, bestqdist = 
	ref 0, ref 0, ref infinity, ref infinity in
	Array.iteri
	  begin fun i v ->
	    if float_of_int (dist2 p v) < !bestpdist then begin
	      bestp := i;
	      bestpdist := float_of_int (dist2 p v);
	    end;
	    if float_of_int (dist2 q v) < !bestqdist then begin
	      bestq := i;
	      bestqdist := float_of_int (dist2 q v);
	    end
	  end
	  net.vertices;
	
	Frozen.iter_symbols gram 
	  begin fun sym ->
	    if not sym.startable then begin 
	      tryout_ignore sym.sid !bestp !bestq (sym.sdata.straightcost) None;
	    end 
	  end;


(* 	Hashtbl.iter  *)
(* 	  begin fun (ip',iq') cost -> *)
(* 	    let p',q' = net.vertices.(ip'), net.vertices.(iq') in *)
(* 	    let diff = (dist2 p p') + (dist2 q q') in *)
(* 	    let diff = (float_of_int diff) /. (float_of_int (dist2 p q)) in *)
(* 	    let cost = exp (10. *. diff) in *)
(* 	      if diff < 0.1 then begin *)
(* 		Frozen.iter_symbols gram  *)
(* 		  begin fun sym -> *)
(* 		    if not sym.startable then begin  *)
(* 		      tryout_ignore sym.sid ip' iq' (cost +. sym.sdata.straightcost) None; *)
(* 		      tryout_ignore sym.sid iq' ip' (cost +. sym.sdata.straightcost) None; *)
(* 		    end  *)
(* 		  end *)
(* 	      end *)
(* 	  end *)
(* 	  net.edges; *)



    done;

    printf "About to add segment costs!\n%!";
    printf "Grammar has %d symbols\n%!" nsymbols;
    Frozen.iter_symbols gram
      begin fun sym ->
	if not sym.startable then begin
	  printf "Have to add %d things\n%!" (Hashtbl.length net.edges);
	  (* 	  for p = 0 to n-1 do *)
	  (* 	    tryout_ignore sym.sid p p (?) None; *)
	  (* 	  done; *)
	  Hashtbl.iter
	    begin fun (p,q) cost ->
	      let cost = dataweight *. cost +. sym.sdata.straightcost in
		tryout_ignore sym.sid q p cost None;
		tryout_ignore sym.sid p q cost None;
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
	      printf "\n>>> s%d -> s%d s%d\n%!" prod.topsid prod.leftsid prod.rightsid;
	      if ntchanged.(prod.leftsid) || ntchanged.(prod.rightsid) then begin
		let ntiter, nchanged = ref 0, ref 17 in
		  (* 		  while !nchanged > 0 do *)
		  for foofoofoo = 1 to 1 do
		    nchanged := 0;
		    incr ntiter;
		    printf "%d: %d/%d (%d)\n%!" !iter sym.sid nsymbols prod.cid;

		    for p = 0 to n-1 do
		      for q = 0 to n-1 do 
			if data.costs >>! ((prod.leftsid, p, q),infinity) < infinity then begin
			  for r = 0 to n-1 do
			    (* 			    if data.costs >>! ((prod.rightsid, q, r),infinity) < infinity then begin *)
			    (* 			      printf "SHOULD HAVE HIT NOW\n%!"; *)
			    (* 			    end; *)
			    (* may want to add the rule back in that L->LL must have d(p,r) > d(p,q), d(q,r) *)
			    if sym.startable = (p=r) then begin
			      let cost = rule_cost data net gram sym prod (p,q,r) in
				(* 				if cost < infinity then *)
				(* 				  printf "cost = %f\n%!" cost; *)
				if tryout sym.sid p r cost (Some (prod.cid, q)) then begin
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
		printf "displaying s%d\n" sym.sid;
		display sym sym.sid (p,q);
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

      data


let _ = 
  let excurve = Curve.load Sys.argv.(1) in
  let sdf = Sdf.load_family Sys.argv.(2) in
  let imname = Sys.argv.(3) in
  let prefix = Sys.argv.(4) in
    
  let net = Graph.load_point_float_graph Sys.argv.(5) in
  let curve = Curve.load Sys.argv.(6) in
  let curve = Array.map Geometry.point_of_complex curve in

  let gram = Models.Simple.make_grammar (excurve, sdf) in
  let gram = merge_leaves gram in
  let _ = reorder_symbols gram in

  let pdata = parse_image_cheating gram net imname prefix curve in
  let ptree = harvest_best_tree_overall gram pdata in
    printf "Finished parsing!\n%!";
    print_parse_tree ptree;

    let im = Pnm.load_ppm imname in
    let curve = List.map (fun (p,q) -> net.vertices.(p)) ptree.yield in
      draw_curve im (Array.map Geometry.point_of_complex excurve) true (0,255,0);
      draw_curve im (Array.of_list curve) true (0, 255, 255);
      Pnm.save_ppm im (sprintf "%s.final.ppm" prefix)


