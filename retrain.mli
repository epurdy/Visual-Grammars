
open Abstract

val retrain :
  ?sparsefactor:float ->
  ?prunethresh:float ->
  Grammar.grammar -> 
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar list ->
  (Grammar.sdata, Grammar.cdata, 'tgt_sym, 'tgt_comp, 'tgt_glob) Parsing.strategy ->
  Grammar.grammar


val get_soft_counts_parallel_worker : 
  (Grammar.sdata, Grammar.cdata, 'mod_glob) frozen_grammar ->
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar ->
  (Grammar.sdata, Grammar.cdata, 'tgt_sym, 'tgt_comp, 'tgt_glob) Parsing.strategy ->
  Shape.shape Parsing.sparse_counts_table

