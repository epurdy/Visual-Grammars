#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

dir = 'experiments/4.images/fuzzy_cky/output.d/'
example = 'romer/ann/curve0000.curve'
sdf = 'romer/misc/romer1.sdf'

for i in xrange(16): 
  curve = 'romer/ann/curve%03d0.curve' % i
  imname = dir + 'network.%03d0.pgm' % i
  gfname = dir + 'network.%03d0.gf' % i
  yieldprefix = dir + '%03d0.yield' % i
  parseprefix = dir + '%03d0.parse' % i

  doit('./net_of_curve.native -curve %s -imname %s -gfname %s -granularity %d -size %d -maxlen %d -curvecost %f -backgroundcost %f -maxcost %f'  % 
      (curve, imname, gfname, 32, 256, 10, 1.0, 1000.0, 100.0))

  doit('./fuzzy_cky.native %s %s %s %s %s %s %s' % (
      curve, example, sdf, imname, yieldprefix, parseprefix, gfname))

