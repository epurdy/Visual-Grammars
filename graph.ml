open Printf
open Scanf
open Util.Hops
open Util.Misc

type point = int * int

type vid = int (* Unique identifier of a vertex *)

(* An undirected graph with vertices of type ['v] and edges of type
   ['e]. It is assumed that the vertex labels are unique, i.e., if
   v1 and v2 have the same label, they are the same vertex.

 *)
type ('v,'e) graph = {
  (* "Maps" vids to vertex labels. *)
  vertices: 'v array;

  (* Maps vid pair to the associated edge data. *)
  edges: (vid*vid, 'e) Hashtbl.t; 

  (* Maps the id of a vertex to the ids of its neighbors. It has
     multiple bindings to be like an array of adjacency lists. *)
  nbrs: (vid,vid) hash; 
}

(* an undirected graph, in the process of construction *)
type ('v,'e) live_graph = {
  (* Maps vertex labels to vids. *)
  lvertices: ('v, vid) Hashtbl.t;
  mutable next_vid: vid;

  (* maps u.id,v.id -> edge data *)
  ledges: (vid*vid, 'e) Hashtbl.t;

}

let new_live_graph () =
  {lvertices = mkhash 1000;
   ledges = mkhash 1000;
   next_vid = 0;
  }

let nvertices gf = 
  Array.length gf.vertices

let nedges gf = 
  Hashtbl.length gf.edges

(* order vertex labels canonically to create undirected pairs *)
let canonical v1 v2 =
  if v1 > v2 then v2, v1
  else v1, v2

let add_vertex gf v =
  if not (gf.lvertices >>? v) then begin
    gf.lvertices << (v, gf.next_vid);
    gf.next_vid <- gf.next_vid + 1;
  end;
  gf.lvertices >> v

let is_edge gf v1 v2 =
  let v1, v2 = canonical v1 v2 in
    gf.ledges >>? (gf.lvertices >> v1, gf.lvertices >> v2)

let add_edge_strict gf v1 v2 edata =
  let v1, v2 = canonical v1 v2 in
  let id1 = gf.lvertices >> v1 in
  let id2 = gf.lvertices >> v2 in
    gf.ledges <<+ ((id1,id2), edata)

let add_edge_and_vertices gf v1 v2 edata =
  let v1, v2 = canonical v1 v2 in
  let id1 = add_vertex gf v1 in
  let id2 = add_vertex gf v2 in
    gf.ledges <<+ ((id1,id2), edata)

let finalize gf = 
  let n = gf.next_vid in
  let vertices = Array.init n (fun i -> None) in
  let nbrs = mkhash (2 * (Hashtbl.length gf.ledges)) in
  let edges = mkhash (Hashtbl.length gf.ledges) in
  let _ = 
    Hashtbl.iter 
      begin fun v i ->
	vertices.(i) <- Some v;
      end
      gf.lvertices;
  in
  let vertices = Array.map get vertices in
    Hashtbl.iter
      begin fun (id1,id2) edata ->
	edges <<+ ((id1,id2), edata);
	nbrs <<+ (id1, id2);
	nbrs <<+ (id2, id1);
      end
      gf.ledges;
    {vertices = vertices;
     edges = edges;
     nbrs = nbrs;
    }

type ('v,'e) iface = {
  loader: string -> 'v * 'v * 'e;
  saver: 'v -> 'v -> 'e -> string;
}       

let load_graph fname iface = 
  let gf = new_live_graph () in
  let chan = open_in fname in
    begin try
      while true do
	let line = input_line chan in
	let v1, v2, edata = iface.loader line in
	  add_edge_and_vertices gf v1 v2 edata;
      done
    with End_of_file ->
      close_in chan;
    end;
    finalize gf

let point_float_iface = {
  loader = 
    begin fun s -> 
      let v1, v2, edata = ref None, ref None, ref None in
      let get_stuff x1 y1 x2 y2 w = 
	v1 := Some (x1,y1);
	v2 := Some (x2,y2);
	edata := Some w;
      in
	begin try
	  sscanf s "%d,%d %d,%d [%f]" get_stuff;
	with _ -> 
	  sscanf s "%d,%d %d,%d [inf]" 
	    (fun x1 y1 x2 y2 -> get_stuff x1 y1 x2 y2 infinity);
	end;
	(get !v1, get !v2, get !edata)
    end;

  saver = begin fun (px,py) (qx,qy) edata -> 
    sprintf "%d,%d %d,%d [%f]" px py qx qy edata;
  end;
}

let point_unit_iface = {
  loader = begin fun s -> 
    let v1, v2 = ref None, ref None in
    let get_stuff x1 y1 x2 y2 = 
      v1 := Some (x1,y1);
      v2 := Some (x2,y2);
    in
      sscanf s "%d,%d %d,%d" get_stuff;
      (get !v1, get !v2, ())
  end;

  saver = begin fun (px,py) (qx,qy) () -> 
    sprintf "%d,%d %d,%d" px py qx qy;
  end;

}

let load_point_float_graph fname = load_graph fname point_float_iface

let load_point_unit_graph fname = load_graph fname point_unit_iface

let save_graph gf fname iface = 
  let chan = open_out fname in
    Hashtbl.iter 
      begin fun (i,j) edata ->
	let v1 = gf.vertices.(i) in
	let v2 = gf.vertices.(j) in
	  fprintf chan "%s\n" (iface.saver v1 v2 edata);
      end
      gf.edges;
    close_out chan

let save_point_unit_graph gf fname = save_graph gf fname point_unit_iface

let save_point_float_graph gf fname = save_graph gf fname point_float_iface

let live_graph_of_curve c vl =
  let c = Array.map Geometry.point_of_complex c in
  let gf = new_live_graph () in
  let n = Array.length c in
    for i = 0 to n-1 do
      add_edge_and_vertices gf c.(i) c.((i+1) mod n) vl;
    done;
    gf

let graph_of_curve c vl = 
  finalize (live_graph_of_curve c vl)

let triangulate_point_unit_graph gf = 
  save_point_unit_graph gf "/var/tmp/epurdy/tmp.txt";
  doit "./cdt/constrained /var/tmp/epurdy/tmp.txt > /var/tmp/epurdy/tmp2.txt";  
  let gf = load_point_unit_graph "/var/tmp/epurdy/tmp2.txt" in
    gf

let cdt_network_float gf vl = 
  let gf' = {
    vertices = gf.vertices;
    edges = mkhash (Hashtbl.length gf.edges);
    nbrs = gf.nbrs;
  } in
  let _ = 
    Hashtbl.iter
      begin fun (i,j) edata ->
	gf'.edges <<+ ((i,j), ());
      end
      gf.edges;
  in
  let gf' = triangulate_point_unit_graph gf' in
  let edges' = mkhash (Hashtbl.length gf.edges) in

  let _ = 
    Hashtbl.iter
      begin fun (i,j) () ->
	edges' <<+ ((i,j), vl);
      end
      gf'.edges;
  in
    {
      vertices = gf'.vertices;
      edges = edges';
      nbrs = gf'.nbrs;
    }

let draw_point_graph im gf = 
  Hashtbl.iter
    begin fun (i,j) _ ->
      let p, q = gf.vertices.(i), gf.vertices.(j) in
	Draw.draw_line im p q (255,0,0);
    end
    gf.edges
