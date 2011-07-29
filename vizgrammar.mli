open Abstract

val draw_prod_curves :
  (int * int * int) Image.t ->
  Bounds.bounds -> int -> Grammar.cdata Abstract.composition -> unit
val log_cutoff : float
val draw_production_standard :
  Grammar.cdata Abstract.composition -> (int * int * int) Image.t
val draw_grammar_best_rules :
  (int -> int -> Abstract.cid -> string) ->
  Grammar.grammar -> unit
val draw_grammar :
  (Abstract.cid -> string) -> ('a, Grammar.cdata, 'b) Abstract.frozen_grammar -> unit
