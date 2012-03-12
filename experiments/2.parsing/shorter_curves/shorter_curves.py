#!/usr/bin/python
from experiments.common import *

dir = 'experiments/2.parsing/shorter_curves'

cmd = './subsample.native romer/clean/diff0080.curve 50 tmp/diff0080.50.curve'
doit(cmd)

cmd = './shorter_curves.native romer/ann/curve0000.curve tmp/diff0080.50.curve tmp/parse_00.svg tmp/sdf_0.svg'
doit(cmd)
cmd = './shorter_curves.native romer/ann/curve0080.curve tmp/diff0080.50.curve tmp/parse_80.svg tmp/sdf_8.svg'
doit(cmd)

svg_to_png(dir, 'parse_00.svg', 'parse_00.png')
svg_to_png(dir, 'parse_80.svg', 'parse_80.png')
svg_to_png(dir, 'sdf_0.svg', 'sdf_0.png')
svg_to_png(dir, 'sdf_8.svg', 'sdf_8.png')
