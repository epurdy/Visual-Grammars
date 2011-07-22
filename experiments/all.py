#!/usr/bin/python

import os

def doit(cmd):
  print cmd
  assert(os.system(cmd) == 0)

doit('ocamlbuild experiments/2.parsing/one_to_one/one_to_one.native experiments/6.structure/constituents/constituents.native')

doit('./experiments/2.parsing/one_to_one/one_to_one.py')
doit('./experiments/6.structure/constituents/constituents.py')


doit('cd experiments/ && latex experiments.tex && dvips experiments.dvi')
