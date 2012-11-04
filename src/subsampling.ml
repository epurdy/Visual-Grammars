open Util.Misc

let regweight = 1.0

let subsample_dp (curve : Curve.t) (nump : int) ?maxlen (minlen : int) =
  (*   let minlen = 1 in *)
  let n = Array.length curve in
  let deslen = float_of_int n /. float_of_int nump in
  let curve2 = Array.append curve curve in
  let maxlen = 
    match maxlen with 
	None -> int_of_float (deslen *. 1.5) 
      | Some l ->l
  in
  let linecost = Array.make_matrix n (maxlen+1) infinity in
  let dptab = Array.make_matrix maxlen (n+maxlen+1) infinity in
  let pred = Array.make_matrix maxlen (n+maxlen+1) None in
  let bestsplit = ref None in
  let bestval = ref infinity in

    (* linecost.(i).(len) is cost of matching points i through i+len 
       (inclusive) to a straight line. 
       linecost.(i).(j) is meaningless for j < minlen.
    *)
    for i = 0 to n-1 do
      for len = minlen to maxlen do
	let deltalen = (float_of_int len) -. deslen in
	let reg = regweight *. deltalen *. deltalen in
	let deviation = Geometry.linecostfn curve2 i (i+len) in
	  linecost.(i).(len) <- deviation +. reg;
	  (* 	  printf "%d %d %f\n" i len linecost.(i).(len); *)
      done;
    done;


    (* dptab.(i).(j) is minimum cost of subsampling points i through j
       (inclusive). Here i runs only from 0 to maxlen-1, since we only
       care about constructing a full subsample of the entire curve,
       but we want to allow one segment to wrap around the end. *)
    for i = 0 to maxlen-1 do
      dptab.(i).(i) <- 0.0;
      (*       printf "i=%d, n=%d, i+n=%d\n" i n (i+n); *)
      for j = i+1 to n + maxlen do
	(* 	printf "i=%d, j=%d minlen=%d maxlen=%d n=%d\n%!"  *)
	(* 	  i j minlen (min maxlen (j-i)) n; *)
	for len = minlen to min maxlen (j-i) do  
	  let cost = dptab.(i).(j-len) +. linecost.((j-len) mod n).(len) in
	    if cost < dptab.(i).(j) then begin
	      (* 	      printf "[%d,%d] = [%d,%d] + [%d,%d]\n%f = %f + %f\n%!" *)
	      (* 		i j i (j-len) (j-len) j *)
	      (* 		cost dptab.(i).(j-len) linecost.((j-len) mod n).(len); *)
	      dptab.(i).(j) <- cost;
	      pred.(i).(j) <- Some (j-len);
	    end;
	done;
      done;
    done;

    (* pick the best place for the first segment to start *)
    for i = 0 to maxlen-1 do
(*       printf "i=%d, n=%d, dptab.(%d).(%d) = %f\n" i n i (i+n) dptab.(i).(i+n); *)
      if dptab.(i).(i+n) < !bestval then begin
	bestsplit := Some i;
	bestval := dptab.(i).(i+n);
      end;
    done;
    assert( !bestsplit != None);

    (* reconstruct the best subsampling *)
    let bestsplit = get !bestsplit in
    let finger = ref (n + bestsplit) in
    let rv = ref [] in
      while !finger != bestsplit do
	let newfinger = get pred.(bestsplit).(!finger) in
	  (* 	  printf "[%d,%d] -> [%d,%d] (maxlen is %d, deslen is %f, len is %d)\n%!" *)
	  (* 	    bestsplit !finger *)
	  (* 	    bestsplit newfinger *)
	  (* 	    maxlen deslen (!finger - newfinger); *)
	  finger := newfinger;
	  rv := !finger :: !rv;
      done;

      Array.map (fun i -> curve2.(i)) (Array.of_list !rv)

(* subsample curve using midpoint rule *)
let subsample_trivial (curve : 'a array) (depth : int) : 
    'a array =
  let mid n i j =
    if j >= i then
      (i+j) lsr 1
    else
      ((i+j+n) lsr 1) mod n
  in
  let n = Array.length curve in
  let m = n lsr 1 in
  let rec iter d i j =
    let k = mid n i j in
      if d >= depth then []
      else iter (d+1) i k @ [curve.(k)] @ iter (d+1) k j
  in
  let first = curve.(0) in
  let mid = curve.(m) in
    Array.of_list ([first] @ iter 0 0 m @ [mid] @ iter 0 m 0)

let subsample_ultracrude (curve : 'a array) (len : int) : 'a array =
  let n = Array.length curve in
    Array.init len
      begin fun i ->
	curve.((i*n) / len)
      end
