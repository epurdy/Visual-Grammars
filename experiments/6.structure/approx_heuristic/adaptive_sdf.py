#!/usr/bin/python

import os, sys

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

cmd = './subsample.native romer/clean/diff0080.curve 200 tmp/diff0080.200.curve'
doit(cmd)
cmd = './subsample.native tmp/diff0080.200.curve 50 tmp/diff0080.50.curve'
doit(cmd)
cmd = './subsample.native tmp/diff0080.50.curve 10 tmp/diff0080.10.curve'
doit(cmd)

fnames = (['romer/clean/diff0080.curve'] +  
          ['tmp/diff0080.%d.curve' % l for l in [200, 50, 10] ])

cmd = './show_curves.native -fname tmp/adaptive_sdf_curves.svg %s' % ' '.join(fnames)
doit(cmd)

cmd = './adaptive_sdf.native %s -out tmp/adaptive_sdf.svg' % (' ' .join(fnames))
doit(cmd)

sys.exit()

cmd = 'inkscape tmp/adaptive_sdf.svg -E experiments/2.parsing/adaptive_sdf/output.d/adaptive_sdf.eps'
doit(cmd)
