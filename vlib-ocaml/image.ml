open Printf

exception Image_Too_Large

type 'a t = 
    { width : int;
      height : int;
      data : 'a array }

let create width height value = 
  let data = 
    try
      Array.create (width * height) value 
    with Invalid_argument("Array.make") ->
      raise Image_Too_Large
  in
    { width = width; height = height; data = data }


let init width height f =
  let img = create width height (f (0, 0)) in
  let rec iter x y =
    if x < width then 
      (img.data.(y * width + x) <- f (x, y);
       iter (x+1) y)
    else 
      if y < height-1 then iter 0 (y+1) else ()
  in
    iter 1 0;
    img

let copy img =
  { width = img.width; 
    height = img.height;
    data = Array.copy img.data }

let width img = 
  img.width

let height img =
  img.height
    
let get img (x, y) =
  img.data.(y * img.width + x)

let set img value (x, y) = 
  img.data.(y * img.width + x) <- value

let get_many img pts = 
  List.map (get img) pts

let set_many img value pts = 
  List.iter (set img value) pts

let iter f img =  
  Array.iter f img.data

let map f img = 
  let data = Array.map f img.data in
    { width = img.width; height = img.height; data = data }

let iterxy f img =
  for y = 0 to img.height-1 do
    for x = 0 to img.width-1 do
      f (x, y) img.data.(y * img.width + x)
    done
  done

let iterc f img (x1,y1) (x2,y2) =
  for y = y1 to y2 do
    for x = x1 to x2 do
      f img x y
    done
  done

let mapxy f img =
  let dst = create img.width img.height (f (0, 0) img.data.(0)) in
  let rec iter x y =
    if x < img.width then 
      (dst.data.(y * img.width + x) <- f (x, y) img.data.(y * img.width + x);
       iter (x+1) y)
    else 
      if y < img.height-1 then iter 0 (y+1) else ()
  in
    iter 1 0; 
    dst

let fold f v img =
  Array.fold_left f v img.data

let blit src dst (x, y) =
  for yp = 0 to src.height-1 do
    for xp = 0 to src.width-1 do
      set dst (get src (xp, yp)) (x+xp, y+yp)
    done
  done


let sub img (x, y) (w, h) =
  let f (xp, yp) = get img (x+xp, y+yp) in
    init w h f

let inside img (x, y) =
  x >= 0 && x < img.width && y >= 0 && y < img.height

let combine a b f =
  let width = min a.width b.width in
  let height = min a.height b.height in
  let g p = f (get a p) (get b p) in
    init width height g

type rgb = int * int * int

type rgbfloat = float * float * float

let color_of_gray v = (v, v, v)

let rgbfloat_of_rgb (r, g, b) =
  (float_of_int r, float_of_int g, float_of_int b)

let rgb_of_rgbfloat (r, g, b) =
  (int_of_float r, int_of_float g, int_of_float b)

type 'a ops =
    { zero : 'a;
      mul : float -> 'a -> 'a;
      add : 'a -> 'a -> 'a;
      sub : 'a -> 'a -> 'a;
      dot : 'a -> 'a -> float }

let float_ops = 
  { zero = 0.0;
    mul = (fun s x -> s *. x);
    add = (fun x y -> x +. y);
    sub = (fun x y -> x -. y);
    dot = (fun x y -> x *. y) }

let rgbfloat_ops = 
  { zero = (0.0, 0.0, 0.0);
    mul = (fun s (r, g, b) -> (s *. r, s *. g, s *. b));
    add = (fun (r1, g1, b1) (r2, g2, b2) -> (r1 +. r2, g1 +. g2, b1 +. b2));
    sub = (fun (r1, g1, b1) (r2, g2, b2) -> (r1 -. r2, g1 -. g2, b1 -. b2));
    dot = (fun (r1, g1, b1) (r2, g2, b2) -> r1 *. r2 +. g1 *. g2 +. b1 *. b2) }
