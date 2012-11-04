open Util.Misc

(* vertex identifier *)
type vid = int

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

type ('v,'e) iface = {
  loader: string -> 'v * 'v * 'e;
  saver: 'v -> 'v -> 'e -> string;
}       

val new_live_graph : unit -> ('v,'e) live_graph

val nvertices : ('v,'e) graph -> int

val nedges : ('v,'e) graph -> int

val add_vertex : ('v,'e) live_graph -> 'v -> vid

val add_vertex_strict : ('v,'e) live_graph -> 'v -> vid

val is_edge : ('v,'e) live_graph -> 'v -> 'v -> bool

val add_edge_strict : ('v, 'e) live_graph -> 'v -> 'v -> 'e -> unit

val add_edge_and_vertices : ('v, 'e) live_graph -> 'v -> 'v -> 'e -> unit

val finalize : ('v,'e) live_graph -> ('v,'e) graph

val load_graph : string -> ('v,'e) iface -> ('v,'e) graph

val point_float_iface : (int * int, float) iface

val point_unit_iface : (int * int, unit) iface

val load_point_float_graph : string -> (int * int, float) graph

val load_point_unit_graph : string -> (int * int, unit) graph

val save_graph : ('v,'e) graph -> string -> ('v,'e) iface -> unit

val save_point_unit_graph : (int * int, unit) graph -> string -> unit

val save_point_float_graph : (int * int, float) graph -> string -> unit

(**********************)

(*
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
*)
