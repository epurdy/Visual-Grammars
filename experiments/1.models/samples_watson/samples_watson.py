#!/usr/bin/python

from experiments.common import *

cmd = './samples_watson.native'
doit(cmd)

dir = 'output/1.models/samples_watson'
svg_to_png(dir, 'watson_true.svg', 'watson_true.png')

for i in [ 3, 10, 30, 100, 300, 1000 ]: 
  svg_to_png(dir, 'watson_est_%d.svg' % i, 'watson_est_%d.png' % i)

#   cmd = ('inkscape tmp/watson.%d.true.svg -E %s/watson.%d.true.eps' %
#          (i,dir,i))
#   doit(cmd)
#   cmd = ('inkscape tmp/watson.%d.samples.svg -E %s/watson.%d.samples.eps' %
#          (i,dir,i))
#   doit(cmd)
#   cmd = ('inkscape tmp/watson.%d.est.svg -E %s/watson.%d.est.eps' %
#          (i,dir,i))
#   doit(cmd)





