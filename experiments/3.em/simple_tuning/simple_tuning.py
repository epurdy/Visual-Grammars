#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

doit('./simple_tuning.native')

cmd = 'inkscape experiments/3.em/simple_tuning/simple_tuning.d/examples.svg -E experiments/3.em/simple_tuning/examples.eps'
doit(cmd)

cmd = 'inkscape experiments/3.em/simple_tuning/simple_tuning.d/training.svg -E experiments/3.em/simple_tuning/training.eps'
doit(cmd)

