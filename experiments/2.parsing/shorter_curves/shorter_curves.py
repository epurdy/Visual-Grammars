#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

cmd = './subsample.native romer/clean/diff0080.curve 50 tmp/diff0080.50.curve'
doit(cmd)

cmd = './shorter_curves.native romer/ann/curve0000.curve tmp/diff0080.50.curve tmp/parse_00.svg'
doit(cmd)
cmd = './shorter_curves.native romer/ann/curve0080.curve tmp/diff0080.50.curve tmp/parse_80.svg'
doit(cmd)

cmd = 'inkscape tmp/parse_00.svg -E experiments/2.parsing/shorter_curves/output.d/parse_00.eps'
doit(cmd)
cmd = 'inkscape tmp/parse_80.svg -E experiments/2.parsing/shorter_curves/output.d/parse_80.eps'
doit(cmd)
