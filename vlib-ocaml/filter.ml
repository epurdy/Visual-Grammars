
let convolve_even ops img mask =
  let zero = ops.Image.zero in
  let mul = ops.Image.mul in
  let add = ops.Image.add in
  let width = Image.width img in
  let height = Image.height img in 
  let v = Image.get img in
  let out = Image.create height width zero in
  let mask_last = (Array.length mask) - 1 in 
  let new_pix = ref zero in
    for y = 0 to height-1 do
      for x = 0 to width-1 do
	new_pix := mul mask.(0) (v (x,y));
	for i = 1 to min mask_last x do
	  new_pix := add !new_pix (mul mask.(i) (v (x-i,y)))
	done;
	for i = 1 to min mask_last ((width-1)-x) do
	  new_pix := add !new_pix (mul mask.(i) (v (x+i,y)))
	done;
	Image.set out !new_pix (y,x)
      done
    done; 
    out

let normalize mask = 
  let mask_last = (Array.length mask) - 1 in
  let sum = ref (abs_float mask.(0)) in
    for i = 1 to mask_last do
      sum := !sum +. 2.0 *. (abs_float mask.(i))
    done;
    for i = 0 to mask_last do
      mask.(i) <- mask.(i) /. !sum
    done

let gmask sigma = 
  let len = (truncate (sigma *. 4.0)) + 1 in
  let g x = 
    let v = (float x) /. sigma in
      exp (-0.5 *. v *. v) 
  in
  let mask = Array.init len g in
    (normalize mask; mask)

let smooth ops img sigma = 
  let mask = gmask sigma in
  let tmp = convolve_even ops img mask in
  let out = convolve_even ops tmp mask in
    out

type grad = 
    { mag : float;
      theta : float }

let grad_float img = 
  let width = Image.width img in
  let height = Image.height img in 
  let v = Image.get img in
  let out = Image.create width height { mag = 0.0; theta = 0.0 } in
    for y = 1 to height-2 do
      for x = 1 to width-2 do
        let dx = (v (x+1, y)) -. (v (x-1, y)) in
	let dy = (v (x, y-1)) -. (v (x, y+1)) in
	let mag = sqrt (dx *. dx +. dy *. dy) in
	let theta = atan2 dy dx in
	  Image.set out { mag = mag; theta = theta } (x,y) 
      done
    done;
    out

let grad_rgbfloat img = 
  let width = Image.width img in
  let height = Image.height img in 
  let v = Image.get img in
  let out = Image.create width height { mag = 0.0; theta = 0.0 } in
    for y = 1 to height-2 do
      for x = 1 to width-2 do
	let (rx, gx, bx) = 
	  Image.rgbfloat_ops.Image.sub (v (x+1, y)) (v (x-1, y)) in
	let (ry, gy, by) = 
	  Image.rgbfloat_ops.Image.sub (v (x, y-1)) (v (x, y+1)) in
	let dx2 = (rx *. rx, gx *. gx, bx *. bx) in
	let dy2 = (ry *. ry, gy *. gy, by *. by) in
	let (r, g, b) = Image.rgbfloat_ops.Image.add dx2 dy2 in
	  if (r >= g) && (r >= b) then
	    Image.set out { mag = sqrt r; theta = atan2 ry rx } (x, y)
	  else if (g >= b) then
	    Image.set out { mag = sqrt g; theta = atan2 gy gx } (x, y)
	  else
	    Image.set out { mag = sqrt b; theta = atan2 by bx } (x, y)
      done
    done;
    out
