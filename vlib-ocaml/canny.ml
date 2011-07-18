open Image

let pi = 3.14159265

type 'a image = 'a Image.t

let deriv (img : float image) x y =
  let dx = (get img (x+1, y)) -. (get img (x-1, y))
  and dy = (get img (x, y+1)) -. (get img (x, y-1))
  in 
    (dx, dy)

(* returns a pair of images, the first is the dx image
   the second the dy image. *)
let gradient (img : float image) : (float image) * (float image) =   
  let imgdx = create (width img) (height img) 0.0
  and imgdy = create (width img) (height img) 0.0 in
    
  let grad_aux img x y =
    let (dx, dy) = deriv img x y in
      set imgdx dx (x, y);
      set imgdy dy (x, y)
  in
    iterc grad_aux img (1, 1) (width img-2, height img-2);
    (imgdx, imgdy)

let gradient_magnitude (img : float image) =
  let img' = create (width img) (height img) 0.0 in
  
  let aux img x y =
    let (dx, dy) = deriv img x y in
    let mag = (dx *. dx) +. (dy *. dy) in
     set img' mag (x, y)
  in
    iterc aux img (1, 1) (width img-2, height img-2);
    img'

(* returns a triple of mag image, dx image, dy image *)
let grad_and_mag (img : float image) =
  let imgmag = create (width img) (height img) 0.0 
  and imgdx = create (width img) (height img) 0.0 
  and imgdy = create (width img) (height img) 0.0 in
    
  let aux img x y =
    let (dx, dy) = deriv img x y in
    let mag = (dx *. dx) +. (dy *. dy) in
      set imgmag mag (x, y);
      set imgdx dx (x, y);
      set imgdy dy (x, y)
  in    
    iterc aux img (1, 1) (width img-2, height img-2);
    (imgmag, imgdx, imgdy)

(* offset table for nms*)
let offset_tbl = 
  [| (1,0); (1,1); (0,1); (-1,1); 
     (-1,0); (-1,-1); (0,-1); (1,-1) |]

(* non-maximum suppression *)
let nms (imgmag : float image) (imgdx : float image) (imgdy : float image) = 
  let img' = create (width imgmag) (height imgmag) false in

  let aux img x y = 
    let p = get imgmag (x, y)
    and (dx, dy) = ( get imgdx (x, y), get imgdy (x, y)) in
    let theta = atan2 dy dx in 
    let theta' = ( (int_of_float (theta *. 8.0 /. (2.0 *. pi)) ) + 8 ) mod 8 in
    let theta'' = ( theta' + 4 ) mod 8 in
    let p' = 
      (let (x', y') = offset_tbl.(theta') in
       let x' = min (max (x+x') 0) (width imgmag-1)
       and y' = min (max (y+y') 0) (height imgmag-1) in
	 get imgmag (x', y'))
    and p'' = 
      (let (x', y') = offset_tbl.(theta'') in
       let x' = min (max (x+x') 0) (width imgmag-1)
       and y' = min (max (y+y') 0) (height imgmag-1) in
	 get imgmag (x', y'))
    in
    let edge = (p > p' && p >= p'') || (p > p'' && p >= p') in
      set img' edge (x, y)

  in
    iterc aux imgmag (1, 1) (width img'-2, height img'-2);
    img'

(* hysteresis *)
let hysteresis (img : bool image) (imgmag : float image) (hi : float) (lo : float) = 
  let img' = create (width img) (height img) false in

  (* breadth-first-search for hysteresis *)
  let rec bfs x y =
    List.iter
      (fun (x,y) ->
	 (* only make the pixel and do bfs if 
	    1) the pixel is in bounds
	    2) the pixel is a peak,
	    3) the pixel's magnitude is greater than lo 
	    4) the pixel has not yet been marked *)
	 if (x < (width img) && y < (height img))
	   && (get img (x, y) == true) 
	   && (get imgmag (x, y) > lo) 
	   && (get img' (x, y) == false) then
	   (set img' true (x, y);
	    bfs x y))
      [ ((x+1), y); ((x-1), y); (x, y+1); (x, y-1) ]
  in
    
  let aux img x y =
    if (get img (x, y) == true) 
      && (get imgmag (x, y) > hi)
      (* only do bfs if the pixel has not yet been marked *)
      && (get img' (x, y) == false) then 
      (set img' true (x, y);
       bfs x y)
  in
    iterc aux img (1, 1) (width img-2, height img-2);
    img'

let canny (img : float image) (hi : float) (lo : float) =
  let (imgmag, imgdx, imgdy) = grad_and_mag img in
  let img_edges = nms imgmag imgdx imgdy in
  let img_edges' = hysteresis img_edges imgmag hi lo in
    img_edges'

(* for now do not return the img border pixels *)
let edge_list (img : bool image) = 
  let edges = ref [] in
    for y=3 to (height img)-3 do
      let offset = y*(width img) in
	for x=3 to (width img)-3 do
	  if (get img (x,y)) == true then
	    edges := (x,y)::(!edges);
	done
    done;
    !edges
