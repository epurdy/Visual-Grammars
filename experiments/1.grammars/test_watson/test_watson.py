#!/usr/bin/python

from experiments.common import *

cmd = './test_watson.native'
doit(cmd)

dir = 'experiments/1.grammars/test_watson'

for i in xrange(1,5+1):
  svg_to_png(dir, 'watson_%d_true.svg' % i, 'watson_%d_true.png' % i)
  svg_to_png(dir, 'watson_%d_samples.svg' % i, 'watson_%d_samples.png' % i)
  svg_to_png(dir, 'watson_%d_est.svg' % i, 'watson_%d_est.png' % i)

#   cmd = ('inkscape tmp/watson.%d.true.svg -E %s/watson.%d.true.eps' %
#          (i,dir,i))
#   doit(cmd)
#   cmd = ('inkscape tmp/watson.%d.samples.svg -E %s/watson.%d.samples.eps' %
#          (i,dir,i))
#   doit(cmd)
#   cmd = ('inkscape tmp/watson.%d.est.svg -E %s/watson.%d.est.eps' %
#          (i,dir,i))
#   doit(cmd)





