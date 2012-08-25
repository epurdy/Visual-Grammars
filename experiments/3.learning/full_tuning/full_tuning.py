#!/usr/bin/python

from experiments.common import *

niters = 10
# niters = 1

dir = 'output/3.learning/full_tuning'

#example = 'romer/ann/curve0000.curve'
example = 'DATA/romer/newann/IMG0000.curve'

training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]
# training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(3) ]

# it's messed up, see the notes
# training.remove('romer/ann/curve%03d0.curve' % 13)

show_curves(dir, [example], 'examples.svg', 'examples.png')
show_curves(dir, training, 'training.svg', 'training.png')

doit('./full_tuning.native -example %s -niters %d %s' % (
      example, niters, ' '.join(training)))

for i in xrange(niters + 1):
#  show_grammar(dir, 'gram.%d.d' % i, 'full_tuning.%d.gram' % i)

  doit('mkdir -p %s/gram.%d.d' % (dir,i))
  doit('rm -rf %s/gram.%d.d/*' % (dir,i))
  doit(('./show_grammar.native -gramfile tmp/full_tuning.%d.gram ' +
        '-dir %s/gram.%d.d -latexdir %s/gram.%d.d -title \'\' ') % (
      i, dir, i, dir, i))
# add -rules above?
