#!/usr/bin/python

import os

def doit(cmd): assert(os.system(cmd) == 0)

cmd = './constituents.native tmp/constituents.svg'
doit(cmd)

cmd = 'inkscape tmp/constituents.svg -E experiments/6.structure/constituents/output.d/constituents.eps'
doit(cmd)





