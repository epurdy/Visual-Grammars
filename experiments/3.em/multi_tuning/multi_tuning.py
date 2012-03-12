#!/usr/bin/python

from experiments.common import *

niters = 10

dir = 'experiments/3.em/multi_tuning'

#example = 'romer/ann/curve0000.curve'
example = 'romer/newann/IMG0000.curve'
sdf = 'romer/misc/romer1.sdf'
#training = [ 'romer/ann/curve%03d0.curve' % i for i in xrange(16) ]
training = [ 'romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]

# it's messed up, see the notes
# training.remove('romer/ann/curve%03d0.curve' % 13)

show_curves(dir, [example], 'examples.svg', 'examples.png')
show_curves(dir, training, 'training.svg', 'training.png')

doit('./multi_tuning.native -example %s -sdf %s -niters %d %s' % (
      example, sdf, niters, ' '.join(training)))

for i in xrange(niters + 1):
  show_grammar(dir, 'gram.%d.d' % i, 'multi_tuning.%d.gram' % i)

