#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

dir = 'experiments/4.images/standard_cky/output.d'

example = 'romer/ann/curve0000.curve'
sdf = 'romer/misc/romer1.sdf'

doit('./show_curve.native %s %s/examples.svg' % (example, dir))
doit('inkscape %s/examples.svg -E %s/examples.eps' % (dir, dir))

curvenames = []

for i in xrange(2):
  curve = 'romer/ann/curve%03d0.curve' % i
  curvenames.append(curve)
  imname = dir + '/network.im%03d0.pgm' % i
  prefix = dir + '/cky.im%03d0' % i

  doit('./standard_cky.native %s %s %s %s %s' % (
      curve, example, sdf, imname, prefix))

  doit('convert %s/network.im%03d0.pgm %s/network.im%03d0.eps' % (dir, i, dir, i))
  doit('convert %s.final.ppm %s.final.eps' % (prefix, prefix))

doit('./show_curves.native -fname %s/targets.svg %s' % (dir, ' '.join(curvenames), ))
doit('inkscape %s/targets.svg -E %s/targets.eps' % (dir, dir))



