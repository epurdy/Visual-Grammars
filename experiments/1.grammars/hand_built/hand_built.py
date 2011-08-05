#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

doit('./hand_built.native')

cmd = ('inkscape tmp/hand_built_curve.svg -E ' +
       'experiments/1.grammars/hand_built/output.d/hand_built_curve.eps')
doit(cmd)


