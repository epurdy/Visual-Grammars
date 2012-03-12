#!/usr/bin/python
from experiments.common import *

dir = 'experiments/2.parsing/one_to_one'

cmd = './one_to_one.native romer/ann/curve0020.curve romer/ann/curve0000.curve romer/misc/romer1.sdf tmp/parse.svg'
doit(cmd)

svg_to_png(dir, 'parse.svg', 'parse.png')
