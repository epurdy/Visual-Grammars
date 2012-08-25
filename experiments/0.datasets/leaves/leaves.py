#!/usr/bin/python
from experiments.common import *

dir = 'experiments/0.datasets/leaves'

for i in xrange(1,16):
  filenames = [ "leaves/leaf%02d/leaf%02d_%04d.curve" % (i,i,x) for x in xrange(1,11) ]
  show_curves(dir, filenames, 'leaves_%02d.svg' % i, 'leaves_%02d.png' % i)

