#!/usr/bin/python

from experiments.common import *

dir = 'experiments/6.structure/constituency_heuristics'

doit('./constituency_heuristics.native romer/ann/curve0000.curve')

doit('./show_curve.native romer/ann/curve0000.curve tmp/curve.svg')

svg_to_png(dir, 'curve.svg', 'curve.png')
svg_to_png(dir, 'decay.svg', 'decay.png')
svg_to_png(dir, 'decay_sdf.svg', 'decay_sdf.png')

# doit('inkscape tmp/curve.svg -E %s/curve.eps' % dir)
# doit('inkscape tmp/decay.svg -E %s/decay.eps' % dir)
# doit('inkscape tmp/decay_sdf.svg -E %s/decay_sdf.eps' % dir)
