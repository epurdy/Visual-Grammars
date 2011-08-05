#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

for leafnum in xrange(1,15+1):
  cmd = ('./scaled_nts.native %d' % leafnum)
  doit(cmd)

  cmd = ('inkscape tmp/scaled_nts.%d.svg -E experiments/7.texture/scaled_nts/output.d/scaled_nts.%d.eps' % (leafnum,leafnum))
  doit(cmd)
  cmd = ('inkscape tmp/scaled_nts_training.%d.svg -E experiments/7.texture/scaled_nts/output.d/scaled_nts_training.%d.eps' % (leafnum,leafnum))
  doit(cmd)





