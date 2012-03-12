#!/usr/bin/python

from experiments.common import *

doit('./hand_built.native')

dir = 'experiments/1.grammars/hand_built'

svg_to_png(dir, 'hand_built_curve.svg', 'hand_built_curve.png')
svg_to_png(dir, 'hand_built_sdf.svg', 'hand_built_sdf.png')



