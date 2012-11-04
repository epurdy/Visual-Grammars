open Util.Misc
open Printf

(** correlated_repetition gram ncopies nrulecopies *)
type init_build = {
  excurve: Curve.t;
  family: Sdf.family;
  name: string;
}

(* let init_heuristic_initialize excurve family name = *)
(*   {excurve = excurve; *)
(*    family = family; *)
(*    name = name; *)
(*   } *)
let init_heuristic_initialize gram build = 
  assert (gram = None);
  build

let init_heuristic_suggest gram internal =
  Some internal

let init_heuristic_apply params gram =
  assert (gram = None);
  Models.EM.make_grammar (params.excurve, params.family),
  true

let init_heuristic = 
  {Xform.initialize = init_heuristic_initialize;
   Xform.suggest = init_heuristic_suggest;
   Xform.apply = init_heuristic_apply;
   Xform.print = (fun x -> printf "%s\n%!" x.name);
  }

  
