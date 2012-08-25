#!/usr/bin/python

from experiments.common import *

niters = 3
# niters = 1

dir = 'experiments/3.em/incremental'

#example = 'romer/ann/curve0000.curve'
example = 'romer/newann/IMG0000.curve'
sdf = 'romer/misc/romer1.sdf'
# training = [ 'romer/ann/curve%03d0.curve' % i for i in xrange(16) ]
training = [ 'romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]

# it's messed up, see the notes
# training.remove('romer/ann/curve%03d0.curve' % 13)

show_curves(dir, [example], 'examples.svg', 'examples.png')
show_curves(dir, training, 'training.svg', 'training.png')

doit('./incremental.native -example %s -sdf %s -niters %d %s' % (
      example, sdf, niters, ' '.join(training)))

for i in xrange(10*niters + 1):
  show_grammar(dir, 'gram.%d.d' % i, 'incremental.%d.gram' % i)
#   doit('mkdir -p %s/gram.%d.d' % (dir,i))
#   doit('rm -rf %s/gram.%d.d/*' % (dir,i))
#   doit(('./show_grammar.native -gramfile tmp/incremental.%d.gram ' +
#         '-dir %s/gram.%d.d -latexdir %s/gram.%d.d -title \'\' ') % (
#       i, dir, i, latexdir, i))
# # add -rules above?
