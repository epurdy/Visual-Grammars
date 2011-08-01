#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

cmd = 'mkdir -p experiments/1.grammars/hand_built/hand_built.d/'
doit(cmd)
cmd = 'rm -rf experiments/1.grammars/hand_built/hand_built.d/*'
doit(cmd)

doit('./hand_built.native')

cmd = 'inkscape tmp/hand_built_curve.svg -E experiments/1.grammars/hand_built/hand_built_curve.eps'
doit(cmd)


