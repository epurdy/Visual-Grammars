#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

cmd = './subsample.native romer/clean/diff0080.curve 50 tmp/diff0080.50.curve'
doit(cmd)

cmd = './longer_curves.native tmp/diff0080.50.curve romer/ann/curve0000.curve romer/misc/romer1.sdf tmp/parse.svg'
doit(cmd)

cmd = 'inkscape tmp/parse.svg -E experiments/2.parsing/longer_curves/output.d/parse.eps'
doit(cmd)
