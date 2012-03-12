#!/usr/bin/python

from experiments.common import *

dir = 'experiments/4.images/standard_cky'

example = 'romer/ann/curve0000.curve'
sdf = 'romer/misc/romer1.sdf'

show_single_curve(dir, example, 'examples.svg', 'examples.png')

curvenames = []

for i in xrange(2):
  curve = 'romer/ann/curve%03d0.curve' % i
  curvenames.append(curve)
  imname = dir + '/output.d/network.im%03d0.pgm' % i
  gfname = dir + '/output.d/network.%03d0.gf' % i
  prefix = dir + '/output.d/cky.im%03d0' % i

  doit('./net_of_curve.native -curve %s -imname %s -gfname %s -granularity %d -size %d -maxlen %d -curvecost %f -backgroundcost %f -maxcost %f'  % 
      (curve, imname, gfname, 32, 256, 10, 1.0, 1000.0, 100.0))

  doit('./standard_cky.native %s %s %s %s %s %s' % (
      curve, example, sdf, imname, prefix, gfname))

  doit('convert %s/output.d/network.im%03d0.pgm %s/output.d/network.im%03d0.png' % (dir, i, dir, i))
  doit('convert %s.final.ppm %s.final.png' % (prefix, prefix))

show_curves(dir, curvenames, 'targets.svg', 'targets.png')



