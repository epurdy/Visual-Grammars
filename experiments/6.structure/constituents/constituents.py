#!/usr/bin/python

import os

def doit(cmd): assert(os.system(cmd) == 0)

cmd = './constituents.native'
doit(cmd)

cmd = './show_sdf.native -sdf tmp/optimal.sdf -curve romer/ann/curve0000.curve -fname tmp/optimal.svg'
doit(cmd)

cmd = './show_sdf.native -sdf romer/misc/romer1.sdf -curve romer/ann/curve0000.curve -fname tmp/handpicked.svg'
doit(cmd)

cmd = 'inkscape tmp/optimal.svg -E experiments/6.structure/constituents/output.d/optimal.eps'
doit(cmd)
cmd = 'inkscape tmp/handpicked.svg -E experiments/6.structure/constituents/output.d/handpicked.eps'
doit(cmd)





