let reduce_helper ops img a =
  let w0 = a in
  let w1 = 0.25 in
  let w2 = 0.25 -. a /. 2.0 in
  let zero = ops.Image.zero in
  let mul = ops.Image.mul in
  let add = ops.Image.add in
  let width = Image.width img in
  let height = Image.height img in 
  let value (x, y) = 
    let xp =
      if x >= 0 then
	if x < width then x
	else width-1
      else 0 in
    let yp =
      if y >= 0 then
	if y < height then y
	else height-1
      else 0 in
      Image.get img (xp, yp)
  in
  let new_width = ((width-1) lsr 1) + 1 in
  let out = Image.create height new_width zero in
    for y = 0 to height-1 do
      for x = 0 to new_width-1 do
	let xp = x lsl 1 in
	let v0 = mul w0 (value (xp, y)) in
	let v1 = mul w1 (add (value (xp-1, y)) (value (xp+1, y))) in
	let v2 = mul w2 (add (value (xp-2, y)) (value (xp+2, y))) in
	  Image.set out (add (add v0 v1) v2) (y, x)
      done
    done;
    out

let reduce ops img a =
  let tmp = reduce_helper ops img a in
  let out = reduce_helper ops tmp a in
    out

let create ops img levels = 
  let p = Array.make levels (Image.copy img) in
    for i = 1 to levels-1 do
      p.(i) <- reduce ops p.(i-1) 0.5
    done;
    p

let output p empty =
  let levels = Array.length p in
  let height = Image.height p.(0) in
  let width = Array.fold_left (fun x i -> x + (Image.width i)) 0 p in
  let out = Image.create width height empty in
  let pos = ref 0 in
    for i = 0 to levels-1 do
      Image.blit p.(i) out (!pos, 0);
      pos := !pos + (Image.width p.(i))
    done;
    out

