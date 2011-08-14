open Printf
open Util.Misc
module I = Image


let _ = 
  let im = Pnm.load_pgm Sys.argv.(1) in
  let im = Image.init ((Image.width im)+2) ((Image.height im)+2) 
    begin fun (x,y) ->
      if I.inside im (x-1,y-1) then
	I.get im (x-1,y-1)
      else
	0
    end
  in
  let w,h = Image.width im, Image.height im in

  let out = Image.map (fun x -> (0,0,0)) im in
  let color = Image.map (fun x -> 0) im in 
    (* colors: 

       0 unvisited+white or black
       1 visited, white, not edge
       2 visited, white, edge
    *)

  let nbrs8 (x,y) = (* in ccw order *)
    [ x+1,y; x+1,y+1; x,y+1; x-1,y+1; x-1,y; x-1,y-1; x,y-1; x+1,y-1 ]
  in
  let nbrs4 (x,y) =
    [ x+1,y; x,y+1; x-1,y; x,y-1; ]
  in

  let rec visit (x,y) = 
    let tot = ref 1 in
      I.set color 1 (x,y);
      I.set out (255,255,255) (x,y);

      List.iter 
	begin fun (x',y') ->
	  if I.inside im (x',y') && I.get im (x',y') = 0 then begin
	    I.set color 2 (x,y);
	    I.set out (255,0,0) (x,y);
	  end
	end
	(nbrs4 (x,y));

      List.iter
	begin fun (x',y') ->
	  if I.inside im (x',y') && 
	    I.get im (x',y') != 0 && 
	    I.get color (x',y') = 0 then 
	      tot := !tot + visit (x',y');
	end
	(nbrs4 (x,y));

      !tot
  in

  let rec walk head cur prev =
    let nbrs = nbrs8 cur in
    let nbrs = List.filter 
      (fun (x,y) -> 
	 I.inside color (x,y) && 
	   I.get color (x,y) = 2) nbrs in

      (* first time, head=cur=prev, pick any neighbor *)
      if head = cur && cur = prev then begin
	walk head (List.hd nbrs) cur
      end

      else begin 
	(* finished going around the boundary *)
	if head = cur then begin
	  [cur]
	end

	else begin
	  (* rotate list until we see prev, and remove prev *)
	  let nbrs = 
	    let front, back = ref [], ref nbrs in
	      while (List.hd !back) <> prev do 
		front := !front @ [List.hd !back];
		back := List.tl !back; 
	      done;

	      (List.tl !back) @ !front 
	  in

	    if nbrs = [] then begin
	      (* we're on a peninsula, go back *)
	      cur :: (walk head prev cur)
	    end
	    else
	      (* go to point that comes first going ccw from prev *)
	      cur :: (walk head (List.hd nbrs) cur)
	end
      end
  in

  let largest = ref 0 in
  let bestseed = ref None in

    for y=0 to h-1 do
      for x=0 to w-1 do 
	if (I.get im (x,y) != 0) && (I.get color (x,y) = 0) then begin  
	  let num = visit (x,y) in
	    printf "%d pixels\n%!" num;
	    if num > !largest then begin
	      largest := num;
	      bestseed := Some (x,y);
	    end
	end
      done 
    done;

    let x,y = get !bestseed in
    let boundary = walk (x,y) (x,y) (x,y) in
    let boundary = Array.of_list boundary in 
      
      Array.iter 
	begin fun (x,y) -> 
	  I.set out (0,255,0) (x,y);
	end
	boundary;


      let boundary = Array.map Geometry.complex_of_point boundary in

	Curve.save Sys.argv.(2) boundary;
	Pnm.save_ppm out Sys.argv.(3)
