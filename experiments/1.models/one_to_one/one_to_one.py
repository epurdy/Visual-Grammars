#!/usr/bin/python
from experiments.common import *

dir = 'output/1.models/one_to_one'

cmd = './one_to_one.native DATA/romer/ann/curve0020.curve DATA/romer/ann/curve0000.curve DATA/romer/misc/romer1.sdf %s/parse.svg' % dir
doit(cmd)

svg_to_png(dir, 'parse.svg', 'parse.png')
