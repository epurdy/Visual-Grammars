#!/usr/bin/python
from experiments.common import *

dir = 'experiments/2.parsing/longer_curves'

cmd = './subsample.native romer/clean/diff0080.curve 50 tmp/diff0080.50.curve'
doit(cmd)

cmd = './longer_curves.native tmp/diff0080.50.curve romer/ann/curve0000.curve romer/misc/romer1.sdf tmp/parse.svg'
doit(cmd)

svg_to_png(dir, 'parse.svg', 'parse.png')
