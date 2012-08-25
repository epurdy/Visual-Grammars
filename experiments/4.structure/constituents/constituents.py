#!/usr/bin/python

from experiments.common import *

dir = 'experiments/6.structure/constituents'

cmd = './constituents.native'
doit(cmd)

cmd = './show_sdf.native -sdf tmp/optimal.sdf -curve romer/ann/curve0000.curve -fname tmp/optimal.svg'
doit(cmd)

cmd = './show_sdf.native -sdf romer/misc/romer1.sdf -curve romer/ann/curve0000.curve -fname tmp/handpicked.svg'
doit(cmd)

svg_to_png(dir, 'optimal.svg', 'optimal.png')
svg_to_png(dir, 'handpicked.svg', 'handpicked.png')
# cmd = 'inkscape tmp/optimal.svg -E experiments/6.structure/constituents/output.d/optimal.eps'
# doit(cmd)
# cmd = 'inkscape tmp/handpicked.svg -E experiments/6.structure/constituents/output.d/handpicked.eps'
# doit(cmd)





