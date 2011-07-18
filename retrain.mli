



val retrain :
  ?sparsefactor:float ->
  ?sigma:float ->
  ?baseline_sigma:float ->
 ('a,'b,'c) Grammar.grammar -> Sdf.debug_family list -> 
  ('a,'b,'c) Grammar.grammar (* unit *)

val get_soft_counts_parallel_worker : ('a,'b,'c) Grammar.grammar -> Sdf.debug_family -> 
  Sdf.sdf_debug_cdata Parsing.sparse_counts_table
