let helper src dst n =
  let rec iter s1 s2 d1 d2 =
    if d2 >= d1 then
      let d = (d1 + d2) lsr 1 in
      let s = ref s1 in
	for p = s1 to s2 do
	  if (src !s) +. float_of_int ((!s-d)*(!s-d)) > 
	      (src p) +. float_of_int ((p-d)*(p-d)) then
	    s := p
	done;
	dst d ((src !s) +. float_of_int ((!s-d)*(!s-d)));
	iter s1 !s d1 (d-1);
	iter !s s2 (d+1) d2
  in
    iter 0 (n-1) 0 (n-1)
      
let gdt img =
  let width = Image.width img in
  let height = Image.height img in
  let tmp = Image.create width height 0.0 in
  let out = Image.create width height 0.0 in
  let src1 x y = Image.get img (x,y) in
  let dst1 x y v = Image.set tmp v (x,y) in
  let src2 y x = Image.get tmp (x,y) in
  let dst2 y x v = Image.set out v (x,y) in
    for x = 0 to width-1 do
      helper (src1 x) (dst1 x) height
    done;
    for y = 0 to height-1 do
      helper (src2 y) (dst2 y) width
    done;
    out

let dt img =
  let width = Image.width img in
  let height = Image.height img in
  let tmp = Image.create width height 0.0 in
  let out = Image.create width height 0.0 in
  let src1 x y = if Image.get img (x,y) then 0.0 else infinity in
  let dst1 x y v = Image.set tmp v (x,y) in
  let src2 y x = Image.get tmp (x,y) in
  let dst2 y x v = Image.set out v (x,y) in
    for x = 0 to width-1 do
      helper (src1 x) (dst1 x) height
    done;
    for y = 0 to height-1 do
      helper (src2 y) (dst2 y) width
    done;
    out
