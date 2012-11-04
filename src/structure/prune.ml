open Util.Misc
open Printf

type pruning = {
  prunethresh: float;
}

let prune_apply internal gram =
  let gram = get gram in
  let gram = Grammar.prune gram internal.prunethresh in
    gram, true


let prune_heuristic = {
  Xform.initialize = (fun g x -> x);
  Xform.suggest = (fun g x -> (Some x));
  Xform.apply = (fun x g -> prune_apply x g);
  Xform.print = (fun x -> printf "prune(%0.2f)" x.prunethresh);
}
