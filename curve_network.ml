open Printf
open Scanf
open Util.Hops
open Util.Misc

type point = int * int

type 'a network = {
  points: point array;
  segments: (int*int, 'a) Hashtbl.t;
  nbrs: (int,int) hash; (* multiple bindings to be like aal *)
}

type 'a live_network = {
  lpoints: (point, int) Hashtbl.t;
  mutable nextid: int;
  lsegments: (int*int, 'a) Hashtbl.t;
}

let new_network () =
  {lpoints = mkhash 1000;
   lsegments = mkhash 1000;
   nextid = 0;
  }

let npoints net = 
  Array.length net.points

let canonical p1 p2 =
  if p1 > p2 then p2, p1
  else p1, p2

let add_point net (x,y) =
  if not (net.lpoints >>? (x,y)) then begin
    net.lpoints << ((x,y), net.nextid);
    net.nextid <- net.nextid + 1;
  end;
  net.lpoints >> (x,y)

let add_edge net p1 p2 data =
  let p1, p2 = canonical p1 p2 in
  let id1 = add_point net p1 in
  let id2 = add_point net p2 in
    net.lsegments <<+ ((id1,id2), data)

let finalize net = 
  let n = net.nextid in
  let points = Array.init n (fun i -> (0,0)) in
  let nbrs = mkhash (2* (Hashtbl.length net.lsegments)) in
    Hashtbl.iter 
      begin fun (x,y) i ->
	points.(i) <- (x,y);
      end
      net.lpoints;
    Hashtbl.iter
      begin fun (id1,id2) data ->
	nbrs <<+ (id1, id2);
	nbrs <<+ (id2, id1);
      end
      net.lsegments;
    {points = points;
     segments = net.lsegments;
     nbrs = nbrs;
    }

let load_network_float fname = 
  let net = new_network () in
  let add_edge2 x1 y1 x2 y2 w = 
    add_edge net (x1,y1) (x2,y2) w;
  in
  let chan = open_in fname in
    begin try
      while true do
	let line = input_line chan in
	try
	  sscanf line "%d,%d %d,%d [%f]" add_edge2;
	with _ -> 
	  sscanf line "%d,%d %d,%d [inf]" (fun x1 y1 x2 y2 -> add_edge2 x1 y1 x2 y2 infinity);
      done
    with End_of_file ->
      close_in chan;
    end;
    finalize net

let load_network_unit fname = 
  let net = new_network () in
  let add_edge x1 y1 x2 y2 = 
    add_edge net (x1,y1) (x2,y2) ();
  in
  let chan = open_in fname in
    begin try
      while true do
	let line = input_line chan in
	  sscanf line "%d,%d %d,%d" add_edge;
      done
    with End_of_file ->
      close_in chan;
    end;
    finalize net

let save_unit net fname = 
  let chan = open_out fname in
    Hashtbl.iter 
      begin fun (i,j) data ->
	let px,py = net.points.(i) in
	let qx,qy = net.points.(j) in
	  fprintf chan "%d,%d %d,%d\n" px py qx qy;
      end
      net.segments;
    close_out chan
  
let network_of_curve c vl = 
  let c = Array.map Geometry.point_of_complex c in
  let net = new_network () in
    let n = Array.length c in
      for i = 0 to n-1 do
  	let p,q = c.(i), c.((i+1)mod n) in
	  add_edge net p q vl;
      done;
      finalize net

let live_network_of_curve c vl = 
  let c = Array.map Geometry.point_of_complex c in
  let net = new_network () in
    let n = Array.length c in
      for i = 0 to n-1 do
  	let p,q = c.(i), c.((i+1)mod n) in
	  add_edge net p q vl;
      done;
      net

let cdt_network net = 
  save_unit net "/var/tmp/epurdy/tmp.txt";
  doit "./cdt/constrained /var/tmp/epurdy/tmp.txt > /var/tmp/epurdy/tmp2.txt";  
  let net = load_network_unit "/var/tmp/epurdy/tmp2.txt" in
    net

let cdt_network_float net vl = 
  save_unit net "/var/tmp/epurdy/tmp.txt";
  doit "./cdt/constrained /var/tmp/epurdy/tmp.txt > /var/tmp/epurdy/tmp2.txt";  
  let net = load_network_unit "/var/tmp/epurdy/tmp2.txt" in
  let h = mkhash 1000 in
  let nbrs = mkhash 1000 in
    Hashtbl.iter
      begin fun (id1,id2) () ->
	h <<+ ((id1,id2), vl);
	nbrs <<+ (id1,id2);
	nbrs <<+ (id2,id1);
      end
      net.segments;
    {points = net.points;
     nbrs = nbrs;
     segments = h}

let draw_network im net = 
  Hashtbl.iter
    begin fun (i,j) _ ->
      let p, q = net.points.(i), net.points.(j) in
	Draw.draw_line im p q (255,0,0);
    end
    net.segments
