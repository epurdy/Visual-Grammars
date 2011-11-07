let line (x1,y1) (x2,y2) = 
  let vx = x2-x1 in
  let vy = y2-y1 in
  let ax = abs(vx) lsl 1 in
  let ay = abs(vy) lsl 1 in
  let sx = if vx >= 0 then 1 else -1 in
  let sy = if vy >= 0 then 1 else -1 in
    if ax > ay then
      let rec iter x1 y1 d pts =
	let pts = (x1,y1) :: pts in 
	if x1 == x2 then pts else
	  if (d > 0)
	  then iter (x1 + sx) (y1 + sy) (d - ax + ay) pts
	  else iter (x1 + sx) y1 (d + ay) pts 
      in
	iter x1 y1 (ay - (ax lsr 1)) []
    else
      let rec iter x1 y1 d pts =
	let pts = (x1,y1) :: pts in 
	if y1 == y2 then pts else
	  if (d > 0)
	  then iter (x1 + sx) (y1 + sy) (d + ax - ay) pts
	  else iter x1 (y1 + sy) (d + ax) pts 
      in
	iter x1 y1 (ax - (ay lsr 1)) []

let box (x,y) r =
  let pixels = ref [] in
    for xp = -r to r do
      for yp = -r to r do
	pixels := (x-xp,y-yp) :: !pixels
      done
    done; 
    !pixels


let draw_pts img points value = 
  List.iter
    begin fun (x,y) -> 
      if Image.inside img (x,y) then
	Image.set img value (x,y) 
    end
    points

let draw_line img p1 p2 value = 
  draw_pts img (line p1 p2) value

let draw_box img p r value = 
  draw_pts img (box p r) value
