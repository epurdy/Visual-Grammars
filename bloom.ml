open Printf
module H = Hashtbl

(* A Bloom filter that gives a lower bound on H[x]. Note that it is
   impossible to increase the value of H[x], but it is possible to
   decrease it. *)
type 'a t = {
  size: int;
  nfuncs: int;
  buckets: float array;
  hash1: 'a -> int;
  hash2: 'a -> int;
}

let new_bloom n k mutate =
  {size = n;
   nfuncs = k;
   buckets = Array.init n (fun x -> infinity);
   hash1 = H.hash;
   hash2 = (fun x -> H.hash (mutate x));
  }

let insert t k v =
  let h1 = t.hash1 k in
  let h2 = t.hash2 k in
(*     if h1 <> h2 then printf "h1<>h2\n%!"; *)
    for i = 0 to t.nfuncs-1 do 
      let idx = (h1 + i * h2) mod t.size in
	t.buckets.(idx) <- min t.buckets.(idx) v;
    done;
    ()

let query t k =
  let h1 = t.hash1 k in  
  let h2 = t.hash2 k in
  let rv = ref neg_infinity in
    for i=0 to t.nfuncs-1 do 
      let idx = (h1 + i * h2) mod t.size in
	rv := max !rv t.buckets.(idx);
    done;
    !rv

let test_lt t k v =
  let h1 = t.hash1 k in
  let h2 = t.hash2 k in
  let rv = ref true in
    for i = 0 to t.nfuncs-1 do
      if (not !rv) then	begin 
	let idx = (h1 + i * h2) mod t.size in
	  if t.buckets.(idx) >= v then
	    rv := false;
      end
    done;
    !rv

let test_gt t k v =
  not (test_lt t k v)
