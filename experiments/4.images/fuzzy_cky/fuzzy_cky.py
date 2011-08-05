#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

dir = 'experiments/4.images/fuzzy_cky/output.d/'

curve = 'romer/ann/curve0020.curve'
example = 'romer/ann/curve0000.curve'
sdf = 'romer/misc/romer1.sdf'
imname = dir + 'network.pgm'
prefix = dir + 'cky'

doit('./fuzzy_cky.native %s %s %s %s %s' % (
    curve, example, sdf, imname, prefix))

