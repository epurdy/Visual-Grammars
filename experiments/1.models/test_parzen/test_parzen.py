#!/usr/bin/python

from experiments.common import *

cmd = './test_parzen.native'
doit(cmd)

dir = 'output/1.models/test_parzen'

svg_to_png(dir, 'parzen_true.svg', 'parzen_true.png')

for x in [0.01,0.03,0.1,0.3,1.0]:
  svg_to_png(dir, 'parzen_%0.2f_samples.svg' % x, 'parzen_%0.2f_samples.png' % x)

