#!/usr/bin/python
from experiments.common import *

dir = 'output/1.models/longer_curves'

cmd = './subsample.native DATA/romer/clean/diff0080.curve 50 %s/diff0080.50.curve' % dir
doit(cmd)

cmd = ('./longer_curves.native %s/diff0080.50.curve DATA/romer/ann/curve0000.curve DATA/romer/misc/romer1.sdf %s/parse.svg' %
       (dir,dir))
doit(cmd)

svg_to_png(dir, 'parse.svg', 'parse.png')
