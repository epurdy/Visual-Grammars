
let draw_orientations_grey local = 
  let diffs = [| (1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1) |] in
  let sqsize = 3 in

  let ims = Array.init 4 
    begin fun _ -> 
      Image.map (function true -> 80 | false -> 160) local.edges_ 
    end
  in

    for i = 0 to 3 do 
      Hashtbl.iter 
	begin fun (px,py) v ->
	  let a = C.arg v in
	  let a = (a +. pi) in
	  let a = 8. *. a /. (2. *. pi) in
	  let a = (round a) mod 8 in
	    if a = i then begin
	      I.set ims.(i) 255 (px,py);
	    end;
	    if a = (i+4) then begin
	      I.set ims.(i) 0 (px,py);
	    end;
	end
	local.orientations;

      for a = 0 to 7 do 
	for y=0 to sqsize-1 do 
	  for x=0 to sqsize-1 do 
	    let dx,dy = diffs.(a) in
	    let px,py = (dx+1)*sqsize+x, (dy+1)*sqsize+y in
	      if a = i then begin
		I.set ims.(i) 255 (px,py);
	      end
	      else begin
		if a = (i+4) then begin
		  I.set ims.(i) 0 (px,py);
		end
		else begin 
		  I.set ims.(i) 80 (px,py);
		end
	      end;
	  done
	done
      done;


    done;

    ims
