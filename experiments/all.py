#!/usr/bin/python

import os

def doit(cmd):
  print cmd
  assert(os.system(cmd) == 0)

exes = [
  'bin/show_curve.native',
  'bin/show_curves.native',
  'bin/show_grammar.native',
   'experiments/1.grammars/hand_built/hand_built.native',  
   'experiments/2.parsing/one_to_one/one_to_one.native',
   'experiments/3.em/simple_tuning/simple_tuning.native',
   'experiments/3.em/simple_tuning/do_soft_counts.native',
   'experiments/3.em/multi_tuning/multi_tuning.native',
   'experiments/6.structure/constituents/constituents.native',
#   'experiments/7.texture/scaled_nts/scaled_nts.native',
]

doit('ocamlbuild ' + ' '.join(exes))

# doit('./experiments/1.grammars/hand_built/hand_built.py')
# doit('./experiments/2.parsing/one_to_one/one_to_one.py')
# doit('./experiments/3.em/simple_tuning/simple_tuning.py')
doit('./experiments/3.em/multi_tuning/multi_tuning.py')
# doit('./experiments/6.structure/constituents/constituents.py')

#doit('./experiments/7.texture/scaled_nts/scaled_nts.py')


doit('cd experiments/ && latex experiments.tex && dvips experiments.dvi')
