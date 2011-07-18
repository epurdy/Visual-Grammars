let uniform a b =
  Random.float (b-.a) +. a

let rec gaussian sigma = 
  let x1 = (Random.float 2.0) -. 1.0 in
  let x2 = (Random.float 2.0) -. 1.0 in 
  let x = x1 *. x1 +. x2 *. x2 in
    if x >= 1.0 then gaussian sigma
    else sigma *. x1 *. sqrt (-.2.0 *. (log x) /. x)
      
let rec choose dist i j =
  if i = j then i 
  else
    let k = (i+j) lsr 1 in
    let a = ref 0.0 in
    let b = ref 0.0 in
    let _ = 
      for pos = i to k do
	a := !a +. dist.(pos)
      done;
      for pos = k+1 to j do
	b := !b +. dist.(pos)
      done in
    let r = Random.float (!a +. !b) in
      if r <= !a then choose dist i k else choose dist (k+1) j

let choose_any dist = choose dist 0 (Array.length dist - 1)
