open Printf

let get_example () =
  Curve.load (sprintf "%s/data/example0000.curve" Config.basedir)

let get_romer ?(n = 278) () = 
  Curve.input (sprintf "data/romer/curves1/diff%04d.curve") 0 n

let get_maple_leaves n =
  Curve.input (sprintf "data/swedish_leaves/leaf02/leaf02_%04d.curve") 1 n

let get_articulator ?(n=20) () = 
  Curve.input (sprintf "data/new_articulator/sample.%04d.curve") 0 n

let get_hand ?(n=20) () = 
  Curve.input (sprintf "data/hand/sample.%04d.curve") 0 n


(* type ('params, 'data, 'internals) dataset = { *)
(*   internals: 'internals; *)
(*   get: 'params -> 'data; *)
(*   init_classification: unit -> unit; *)
  
(* } *)
