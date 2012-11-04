open Util.Misc
open Printf 
open Util.Cops
module C = Complex

type t = Geometry.cpt array

let width c =
  Array.fold_left
    (fun x y -> max x y.C.im)
    c.(0).C.im c

let height c =
  Array.fold_left
    (fun x y -> max x y.C.re)
    c.(0).C.re c

let print_curve (c : t) = Array.iter 
  (fun x -> printf "%s\n" (Geometry.string_of_cpt x)) c

(* make open curve from closed curve *)
let open_curve (curve : 'a array) (skip : int) : 'a array =
  let n = Array.length curve in
    Array.sub curve skip (n-skip*2)

(* split into two open subcurves *)
let open_of_closed (curve : 'a array) : 
    'a array * 'a array = 
  let n = Array.length curve in
  let m = n lsr 1 in
  let a = Array.sub curve 0 (m+1) in
  let b = Array.append (Array.sub curve m (n-m)) 
    (Array.sub curve 0 1) in
    (a, b)

let rotate_indices (curve : 'a array) (k : int) : 'a array =
  let n = Array.length curve in
  let k = k mod n in
    Array.append (Array.sub curve k (n-k)) (Array.sub curve 0 k)

let reverse (curve : 'a array) : 'a array =
  let n = Array.length curve in
  let rev = Array.init n (fun i -> curve.(n-1-i)) in
    rev

let flip (curve : t) : t =
  let curve = reverse curve in
    Array.map Geometry.flip_x curve

let flip_xy (curve : t) : t =
  Array.map Geometry.flip_xy curve

  


(* remove duplicates *)
let uniqify (curve : 'a array) : 'a array =
  let n = Array.length curve in
  let ls = Array.to_list curve in 
  let tab = Hashtbl.create n in
  let ls2 = List.filter
      (fun p ->
	 let rv = not (Hashtbl.mem tab p) in
	   Hashtbl.replace tab p ();
	   rv
      )
      ls in
    Array.of_list ls2

(* let subsample_dp_levels (curve : t) (nump : int) k l =  *)
(*   let n = Array.length curve in *)
(*   let deslen = float_of_int n /. float_of_int nump in *)
(*   let curve2 = Array.append curve curve in *)
(*   let curve = ref curve in *)

(*   let linecost =  *)
(*     let linecost = mkhash ((maxlen-minlen)*Array.length curve) in *)
(*       for i=0 to n-1 do *)
(* 	for len=1 to n/2 do *)
(* 	  linecost << ( (i,i+len mod n), Geometry.linecostfn curve2 i (i+len) ); *)
(* 	done *)
(*       done; *)
(*       linecost *)
(*   in *)

(*   let levels = ref [] in *)
(*   let indices = ref (Array.init n (fun i -> i)) in *)
(*   let alpha = ref (1. /. (float_of_int l)) in *)
(*   let beta = ref k in *)
(*     while true do *)

(*       let dptab = Array.make_matrix maxlen (n+maxlen+1) infinity in *)
(*       let pred = Array.make_matrix maxlen (n+maxlen+1) None in *)
(*       let bestsplit = ref None in *)
(*       let bestval = ref infinity in *)

(*       let nind = Array.length indices in *)

(* 	for ii=0 to nind - 1 do *)
(* 	  let i = indices.(ii) in *)
(* 	    if i - indices.(0) < maxlen then begin *)
(* 	      dptab << ( (ii,ii), 0.0 ); *)
(* 	      for jj=ii+1 to nind + ii - 1 do *)
(* 		for kk=jj+1 to nind + ii - 1 do *)
(* 		  let j = indices.(jj mod nind) in *)
(* 		  let k = indices.(kk mod nind) in		 *)
(* 		  let len = (k - j + n mod n) in *)
(* 		    if len <= maxlen then begin *)

(* 		      let deltalen = (float_of_int len) -. deslen in *)
(*       		      let reg = Con.regweight *. deltalen *. deltalen in *)
(* 		      let cost = reg +. dptab >>! ((ii, jj), infinity) +.  *)
(* 			linecost >>! ((jj,kk), infinity) in *)
(* 			if i != k then begin *)
(* 			  if cost < dptab >> (i,k) then begin *)
(* 			    dptab << ((ii,kk), cost); *)
(* 			    pred << ((i,k), j); *)
(* 			  end; *)
(* 			end *)
(* 			else begin *)
(* 			  if cost < !bestval then begin *)
(* 			    bestsplit := Some i; *)
(* 			    bestval := cost; *)
(* 			  end *)
(* 			end *)
(* 		    end *)
(* 		done *)
(* 	      done *)
(* 	    end *)
(* 	done; *)

(* 	assert(!bestsplit != None); *)

(* 	let bestsplit = get !bestsplit in *)
(* 	let finger = ref bestsplit in *)
(* 	let rv = ref [] in *)
(* 	  while !finger != bestsplit || (List.length rv = 0) do *)
(* 	    let newfinger = pred >> (bestsplit, !finger) in *)
(* 	      printf "finger=%d\n" newfinger; *)
(* 	      finger := newfinger; *)
(* 	      rv := !finger :: !rv; *)
(* 	  done; *)
	  
(* 	  indices := Array.of_list !rv; *)
(* 	  levels := !indices :: !levels; *)
(* 	  alpha := 2. *. !alpha; *)
	  
(*     done *)
(* ;;	   *)




(* load curve from file *)
let load (name : string) : t = 
  let chan = open_in name in
  let input_point () = 
    let x = int_of_string (input_word chan) in
    let y = int_of_string (input_word chan) in
      (x, y) in
  let rec read pts = 
    try read (input_point() :: pts)
    with End_of_file -> (close_in chan; pts)
  in
    Array.map Geometry.complex_of_point
      (Array.of_list (List.rev (read [])))

let load_from_xml name =
  let chan = open_in name in
  let points = ref [] in

  let read_tag () =
    let tagname = ref "" in
    let intagname = ref true in
      while !intagname do 
	let d = input_char chan in
	  if d = '>' then begin 
	    intagname := false;
	  end
	  else begin
	    tagname := !tagname ^ (Char.escaped d);
	  end
      done;
      !tagname
  in

  let find_next_tag () =
    let found = ref false in
    let tagname = ref "" in
      while not !found do 
	let c = input_char chan in
	  if c = '<' then begin 
	    found := true;
	    tagname := read_tag ();
	  end
      done;
      !tagname
  in

  let read_int () =
    let finished = ref false in
    let intstring = ref "" in
      while not !finished do 
	let c = input_char chan in
	  if c = '<' then begin 
	    finished := true;
	    ignore (read_tag ()); (* burn </x>, </y> *)
	  end
	  else begin
	    intstring := !intstring ^ (Char.escaped c);
	  end
      done;
      int_of_string !intstring
  in
    
    begin try
      while true do 
	let tagname = find_next_tag () in
	  if tagname = "pt" then begin 
	    let tagname_x = find_next_tag () in
	    let x = read_int () in
	    let tagname_y = find_next_tag () in
	    let y = read_int () in
	      assert(tagname_x = "x");
	      assert(tagname_y = "y");
	      points := (x,y) :: !points;
	  end
      done
    with End_of_file ->
      close_in chan;
    end;

    let curve = List.rev !points in
    let curve = Array.of_list curve in
    let curve = Array.map Geometry.complex_of_point curve in
      curve
      

(* save curve to file *)
let save (name : string) (curve : t) : unit =
  let curve = Array.map Geometry.point_of_complex curve in
  let chan = open_out_bin name in
  let out (x,y) = Printf.fprintf chan "%d %d\n" x y in    
    Array.iter out curve;
    close_out chan

(* save curve to file *)
let save_all namer curves =
  Array.iteri
    begin fun i curve ->
      save (namer i) curve;
    end 
    curves

(* x/y coordinates of curve *)
let x_coords (pts : (int * int) array) : int array =
    Array.map (fun (x,y) -> x) pts
let y_coords (pts : (int * int) array) : int array =
  Array.map (fun (x,y) -> y) pts

let draw (curve : t) (off : 'a) (on : 'a) : 'a Image.t =
  let curve = Array.map Geometry.point_of_complex curve in
  let border = 20 in
  let x = x_coords curve in
  let y = y_coords curve in
  let xmin = Array.fold_left min max_int x in
  let xmax = Array.fold_left max min_int x in
  let ymin = Array.fold_left min max_int y in
  let ymax = Array.fold_left max min_int y in
  let width = xmax - xmin + border * 2 + 1 in
  let height = ymax - ymin + border * 2 + 1 in
  let im = Image.create width height off in
  let n = Array.length curve in
    for i = 0 to n-2 do
      let x1,y1 = curve.(i) in
      let x2,y2 = curve.(i+1) in
	Draw.draw_line im 
	  (x1-xmin+border, y1-ymin+border)
	  (x2-xmin+border, y2-ymin+border)
	  on
    done;
    im

let draw_closed (curve : t) (off : 'a) (on : 'a) : 
    'a Image.t = 
  let first = Array.sub curve 0 1 in
    draw (Array.append curve first) off on

(* input a set of curves *)
let load_all namer (first : int) (num : int) : t array =
  let load i = load (namer (i+first)) in
    Array.init num load

(* output a set of curves *)
let draw_all (curves : t array) namer : unit =
  let num = Array.length curves in
    for i = 0 to num-1 do
      let im = draw curves.(i) 255 0 in
      let name = namer i in
	Pnm.save_pgm im name
    done

let flip_best curve =
  let bounds = Bounds.nice_curve_bounds curve in
  let width = bounds.Bounds.xmax -. bounds.Bounds.xmin in
  let height = bounds.Bounds.ymax -. bounds.Bounds.ymin in
    if width < height then
      curve
    else
      flip_xy curve

let normalize ?(scale=1.0) c =
  let bounds = Bounds.nice_curve_bounds c in
  let c = Array.map (fun z -> Bounds.map_to_unit_square_strict bounds z) c in
  let c = Array.map (fun z -> (cxre scale) *& z) c in
    c

(* align two curves *)
let align a b =
  let mean c = 
    let sm = Array.fold_right (+&) c {C.re=0.; C.im=0.} in
      sm /& {C.re = float_of_int (Array.length c); C.im = 0.}
  in
    
  let norm (x,y) = C.norm {C.re=x; C.im=y} in
  let lanczos (a,b,c,d) =
    let x, y = 1. +. (Random.float 1.0), 1. +. (Random.float 1.0) in
    let x, y = ref x , ref y in
      for i = 0 to 10 do
	let newx = a *. !x +. b *. !y in
	let newy = c *. !x +. d *. !y in
	let newnorm = norm (newx, newy) in
	  x := newx /. newnorm;
	  y := newy /. newnorm;
      done;
      !x, !y
  in

  let var_dir_sign c = 
    let n = float_of_int (Array.length c) in
    let mu = mean c in
    let ex, ey = mu.C.re, mu.C.im in
    let x2 = Array.map (fun z -> z.C.re *. z.C.re) c in
    let y2 = Array.map (fun z -> z.C.im *. z.C.im) c in
    let xy = Array.map (fun z -> z.C.re *. z.C.im) c in
    let ex2 = (Array.fold_right (+.) x2 0.) /. n in
    let exy = (Array.fold_right (+.) xy 0.) /. n in
    let ey2 = (Array.fold_right (+.) y2 0.) /. n in
    let cov_11 = ex2 -. ex *. ex in
    let cov_12 = exy -. ex *. ey in
    let cov_22 = ey2 -. ey *. ey in
    let eigx, eigy = lanczos (cov_11, cov_12, cov_12, cov_22) in
    let det = (cov_11 *. cov_22 -. cov_12 *. cov_12) in
    let trace = cov_11 +. cov_22 in
    let eval1 = (trace +. sqrt (trace *. trace -. 4. *. det)) /. 2. in
    let eval2 = (trace -. sqrt (trace *. trace -. 4. *. det)) /. 2. in

    let scale = max (abs_float eval1) (abs_float eval2) in

    let x3 = Array.map (fun z -> (z.C.re -. ex) ** 3.) c in
    let y3 = Array.map (fun z -> (z.C.im -. ey) ** 3.) c in
    let skewx = (Array.fold_right (+.) x3 0.) /. n in
    let skewy = (Array.fold_right (+.) y3 0.) /. n in
    let flip = 
      if (abs_float skewx) > (abs_float skewy) then
	(skewx /. (abs_float skewx))
      else
	(skewy /. (abs_float skewy))
    in

      (scale, {C.re = eigx; C.im = eigy}, flip)
  in

  let amu, bmu = mean a, mean b in
  let (avar, adir, asign), (bvar, bdir, bsign) = var_dir_sign a, var_dir_sign b in
  let scale = {C.re=sqrt (bvar /. avar); C.im=0.} in
  let rot = bdir /& adir in
  let a = Array.map 
    begin fun p ->
      {C.re=asign *. bsign; C.im=0.} *& rot *& scale *& (p -& amu) +& bmu
    end
    a in
    a
;;
