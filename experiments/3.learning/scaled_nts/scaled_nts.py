#!/usr/bin/python

from experiments.common import *

dir = 'output/3.learning/scaled_nts'

def svg_to_png(dir, svg_name, png_name):
  dispatch('inkscape tmp/%s' % svg_name, D='', e='%s/%s' % (dir,png_name))

for leafnum in xrange(12,15+1):
#for leafnum in xrange(5,15+1):
  if leafnum == 5:
    continue

  cmd = ('./scaled_nts.native %d' % leafnum)
  dispatch(cmd)

  svg_to_png(dir, 'scaled_nts_%d.svg' % leafnum, 'scaled_nts_%d.png' % leafnum)
  svg_to_png(dir, 'scaled_nts_training_%d.svg' % leafnum, 'scaled_nts_training_%d.png' % leafnum)
#   cmd = ('inkscape tmp/scaled_nts.%d.svg -E experiments/7.texture/scaled_nts/output.d/scaled_nts.%d.eps' % (leafnum,leafnum))
#   doit(cmd)
#   cmd = ('inkscape tmp/scaled_nts_training.%d.svg -E experiments/7.texture/scaled_nts/output.d/scaled_nts_training.%d.eps' % (leafnum,leafnum))
#   doit(cmd)





