#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

cmd = './test_watson.native'
doit(cmd)

dir = 'experiments/1.grammars/test_watson/output.d'

for i in xrange(1,5+1):
  cmd = ('inkscape tmp/watson.%d.true.svg -E %s/watson.%d.true.eps' %
         (i,dir,i))
  doit(cmd)
  cmd = ('inkscape tmp/watson.%d.samples.svg -E %s/watson.%d.samples.eps' %
         (i,dir,i))
  doit(cmd)
  cmd = ('inkscape tmp/watson.%d.est.svg -E %s/watson.%d.est.eps' %
         (i,dir,i))
  doit(cmd)





