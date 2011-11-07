#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

niters = 2

dir = 'experiments/3.em/simple_tuning/output.d'
latexdir = './3.em/simple_tuning/output.d'

#example = 'romer/ann/curve0000.curve'
example = 'romer/newann/IMG0000.curve'
sdf = 'romer/misc/romer1.sdf'
training = [ 'romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]

doit('./show_curves.native -fname %s/examples.svg -title \'\' %s' % (
    dir, example))

doit('./show_curves.native -fname %s/training.svg -title \'\' %s' % (
    dir, ' '.join(training)))

doit('./simple_tuning.native -example %s -sdf %s -niters %d %s' % (
      example, sdf, niters, ' '.join(training)))

doit('inkscape %s/examples.svg -E %s/examples.eps' % (
    dir, dir))

doit('inkscape %s/training.svg -E %s/training.eps' % (
    dir, dir))

for i in xrange(niters+1):
  doit('mkdir -p %s/gram.%d.d' % (dir,i))
  doit('rm -rf %s/gram.%d.d/*' % (dir,i))
  doit(('./show_grammar.native -gramfile tmp/simple_tuning.%d.gram ' +
        '-dir %s/gram.%d.d -latexdir %s/gram.%d.d -title \'\' ') % (
      i, dir, i, latexdir, i))
# add -rules above?
