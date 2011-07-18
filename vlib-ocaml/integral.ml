type t = int * int -> float

let create img = 
  let width = Image.width img in
  let height = Image.height img in
  let out = Image.create (width+1) (height+1) 0.0 in     
    for y = 1 to height do
      for x = 1 to width do
	let a = Image.get out (x-1, y) in
	let b = Image.get out (x, y-1) in
	let c = Image.get out (x-1, y-1) in
	  Image.set out ((Image.get img (x-1, y-1)) +. a +. b -. c) (x, y)
      done
    done;
    Image.get out

let box vals (x,y) w h =
  let a = vals (x+w+1, y+h+1) in
  let b = vals (x, y+h+1) in
  let c = vals (x+w+1, y) in
  let d = vals (x, y) in
    ((a +. d) -. (b +. c)) /. (float (w * h))


