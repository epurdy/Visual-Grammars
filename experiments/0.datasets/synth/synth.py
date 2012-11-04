#!/usr/bin/python
from experiments.common import *

dir = 'output/0.datasets/synth'

filenames = [ "DATA/synth/polygons/polygon.%02d.curve" % x for x in xrange(3,11) ]
show_curves(dir, filenames, 'polygons.svg', 'polygons.png')

filenames = [ "DATA/synth/stars/star.%02d.curve" % x for x in xrange(4,11) ]
show_curves(dir, filenames, 'stars.svg', 'stars.png')

filenames = [ "DATA/synth/articulator/articulator.%02d.curve" % x for x in xrange(9) ]
show_curves(dir, filenames, 'articulator.svg', 'articulator.png')

filenames = [ "DATA/synth/narms/narms.%d.curve" % x for x in xrange(9) ]
show_curves(dir, filenames, 'narms.svg', 'narms.png')

