open Xform
open Abstract

type ('tgt_sym, 'tgt_comp, 'tgt_glob) retraining_build_data = {
  sparsefactor: float;
  watson_prior_shape: float;
  watson_prior_mean: float;
  curves: Curve.t array;
  families: ('tgt_sym, 'tgt_comp, 'tgt_glob) Abstract.frozen_grammar array;
  strat: (Grammar.sdata, Grammar.cdata, 'tgt_sym, 'tgt_comp, 'tgt_glob) 
    Parsing.strategy;
  name: string;
  parallel: bool;
}

type ('tgt_sym, 'tgt_comp, 'tgt_glob, 'a) retraining_internal_data = {
  soft_counts: 'a Parsing.sparse_counts_table;
  build_data: ('tgt_sym, 'tgt_comp, 'tgt_glob) retraining_build_data;
}

val retrain_heuristic:  
  (('tgt_sym,'tgt_comp,'tgt_glob) retraining_build_data,
   ('tgt_sym,'tgt_comp,'tgt_glob,Shape.shape) retraining_internal_data,
   ('tgt_sym,'tgt_comp,'tgt_glob,Shape.shape) retraining_internal_data)
  Xform.xform_heuristic

val get_soft_counts_parallel_worker : 
  (Grammar.sdata, Grammar.cdata, 'mod_glob) frozen_grammar ->
  ('tgt_sym, 'tgt_comp, 'tgt_glob) frozen_grammar ->
  (Grammar.sdata, Grammar.cdata, 'tgt_sym, 'tgt_comp, 'tgt_glob) Parsing.strategy ->
  Shape.shape Parsing.sparse_counts_table
