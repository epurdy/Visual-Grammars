open Util.Misc
open Printf

type pruning = {
  prunethresh: float;
}



val prune_heuristic : (pruning,pruning,pruning) Xform.xform_heuristic
