#!/usr/bin/python

from experiments.common import *

niters = 2

dir = 'experiments/3.em/simple_tuning'

#example = 'romer/ann/curve0000.curve'
example = 'romer/newann/IMG0000.curve'
sdf = 'romer/misc/romer1.sdf'
training = [ 'romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]

show_curves(dir, [example], 'examples.svg', 'examples.png')
show_curves(dir, training, 'training.svg', 'training.png')

doit('./simple_tuning.native -example %s -sdf %s -niters %d %s' % (
      example, sdf, niters, ' '.join(training)))

for i in xrange(niters+1):
  show_grammar(dir, 'gram.%d.d' % i, 'simple_tuning.%d.gram' % i)
#   doit('mkdir -p %s/gram.%d.d' % (dir,i))
#   doit('rm -rf %s/gram.%d.d/*' % (dir,i))
#   doit(('./show_grammar.native -gramfile tmp/simple_tuning.%d.gram ' +
#         '-dir %s/gram.%d.d -latexdir %s/gram.%d.d -title \'\' ') % (
#       i, dir, i, latexdir, i))
# # add -rules above?
