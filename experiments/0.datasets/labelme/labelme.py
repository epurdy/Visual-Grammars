#!/usr/bin/python
from experiments.common import *

min_examples = 100
dir = 'output/0.datasets/labelme'

dirs = [ d.rstrip() for d in os.popen("ls DATA/labelme_polygons") ]

classes = {}

for d in dirs:
#  print d
  curves = [ c.rstrip() for c in os.popen("ls DATA/labelme_polygons/%s" % d) ]
  if len(curves) >= min_examples:
    classes[d] = curves

for d in classes:
  print d, len(classes[d]), classes[d][0]
  filenames = ["DATA/labelme_polygons/%s/%s" % (d,x) for x in classes[d] ]
  show_curves(dir, filenames, '%s.svg' % d, '%s.png' % d)

print len(classes)

# filenames = [ "romer/newann/IMG%03d0.curve" % x for x in xrange(num) ]
# show_curves(dir, filenames, 'romerI.svg', 'romerI.png')

# filenames = [ "romer/clean/diff%03d0.curve" % x for x in xrange(num) ]
# show_curves(dir, filenames, 'romerII.svg', 'romerII.png')
