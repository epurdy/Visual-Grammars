#!/usr/bin/python

import os

def doit(cmd): assert(os.system(cmd) == 0)

cmd = './one_to_one.native romer/ann/curve0020.curve romer/ann/curve0000.curve romer/misc/romer1.sdf tmp/parse.svg'
doit(cmd)

cmd = 'inkscape tmp/parse.svg -E experiments/2.parsing/one_to_one/parse.eps'
doit(cmd)



# need to turn svg into eps
# need to include eps in tex fragment


