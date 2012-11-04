open Util.Misc

type ('build_data, 'internal_data, 'xform_params) xform_heuristic = {
  initialize: Grammar.grammar option -> 'build_data -> 'internal_data;
  suggest: Grammar.grammar option -> 'internal_data -> 'xform_params option;
  apply: 'xform_params -> Grammar.grammar option -> Grammar.grammar * bool;   (* success/failure flag *)
  print: 'internal_data -> unit;
}


let xform_pipeline heur (gram: Grammar.grammar) build =
  let internal = heur.initialize (Some gram) build in
  let xform = heur.suggest (Some gram) internal in
  let newgram, succ = heur.apply (get xform) (Some gram) in
    heur.print internal;
    assert succ;
    newgram

let ctor_pipeline heur build = 
  let internal = heur.initialize None build in
  let xform = heur.suggest None internal in
  let gram, succ = heur.apply (get xform) None in
    heur.print internal;
    assert succ;
    gram
