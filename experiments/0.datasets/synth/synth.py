#!/usr/bin/python
from experiments.common import *

dir = 'experiments/0.datasets/synth'

filenames = [ "synth/polygons/polygon.%02d.curve" % x for x in xrange(3,11) ]
show_curves(dir, filenames, 'polygons.svg', 'polygons.png')

filenames = [ "synth/stars/star.%02d.curve" % x for x in xrange(4,11) ]
show_curves(dir, filenames, 'stars.svg', 'stars.png')

filenames = [ "synth/articulator/articulator.%02d.curve" % x for x in xrange(9) ]
show_curves(dir, filenames, 'articulator.svg', 'articulator.png')

filenames = [ "synth/narms/narms.%d.curve" % x for x in xrange(9) ]
show_curves(dir, filenames, 'narms.svg', 'narms.png')

