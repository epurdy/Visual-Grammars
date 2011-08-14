#!/usr/bin/python

import os

dir = 'experiments/6.structure/constituency_heuristics/output.d'

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

doit('./constituency_heuristics.native romer/ann/curve0000.curve')

doit('./show_curve.native romer/ann/curve0000.curve tmp/curve.svg')

doit('inkscape tmp/curve.svg -E %s/curve.eps' % dir)
doit('inkscape tmp/decay.svg -E %s/decay.eps' % dir)
doit('inkscape tmp/decay_sdf.svg -E %s/decay_sdf.eps' % dir)
