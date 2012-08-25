#!/usr/bin/python

from experiments.common import *

niters = 2
dir = 'experiments/3.em/simple_tuning'
example = 'romer/newann/IMG0000.curve'
sdf = 'romer/misc/romer1.sdf'
training = load_dataset('romer/training1.list')

show_curves(dir, [example], 'examples.svg', 'examples.png')
show_curves(dir, training, 'training.svg', 'training.png')

doit('./simple_tuning.native -example %s -sdf %s -niters %d %s' % (
      example, sdf, niters, ' '.join(training)))

for i in xrange(niters+1):
  show_grammar(dir, 'gram.%d.d' % i, 'simple_tuning.%d.gram' % i)
  
