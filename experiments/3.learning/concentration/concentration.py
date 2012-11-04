#!/usr/bin/python

from experiments.common import *
import experiments.em as em

niters = 30
nsamples = 20

name = 'concentration'
title_name = 'Tuning with Different Priors on Concentration'
dir = 'output/3.learning/concentration'
experiment = Experiment('concentration', dir, 'tmp')

example = 'DATA/romer/newann/IMG0000.curve'
sdf = 'DATA/romer/misc/romer2.sdf'
training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]
validation = [ x.rstrip() for x in os.popen('ls DATA/romer/validation/*.curve') ]


prior_shape_weights = [ 1, 10, 100, 1000 ]
iters_to_draw = range(0,niters+1,10)

# make the initial grammars
for psw in prior_shape_weights:
  init_gram = experiment.tmp_file_namer('init', 'gram')
  xform_gram = experiment.tmp_file_namer('foo', 'gram', ('psw',psw), ('iter',0))

  dispatch('./ctor_init.native', example=example, sdf=sdf, output=init_gram)
  dispatch('cp %s %s' % (init_gram, xform_gram))

  for i in xrange(niters):
    gram_file = experiment.tmp_file_namer('foo', 'gram', ('psw',psw), ('iter',i))
    next_gram_file = experiment.tmp_file_namer('foo', 'gram', ('psw',psw), ('iter',i+1))
    dispatch('./do_retrain.native', input=gram_file, output=next_gram_file, anon=training,
             prior_shape=psw, prior_mean=100.0)

experiment.start_table('xent', '|l|l|l|l|', '''
Cross-entropy scores across iterations. Each column represents a different weight given to the prior over concentrations.''')

for psw in prior_shape_weights:
  experiment.table_cell_from_string('xent', str(psw))
experiment.table_end_row('xent')
experiment.table_hrule('xent')

for i in xrange(niters):
  for ipsw, psw in enumerate(prior_shape_weights):
    gram_file = experiment.tmp_file_namer('foo', 'gram', ('psw',psw), ('iter',i))
    qual_file = experiment.tmp_file_namer('foo', 'qual', ('psw',psw), ('iter',i))

    dispatch('./xent.native', gram=gram_file, qual=qual_file, anon=validation)
    experiment.table_cell_from_file('xent', qual_file)

    if ipsw < len(prior_shape_weights)-1:
      experiment.table_end_cell('xent')

  experiment.table_end_row('xent')

experiment.end_table('xent')

    
################################
# boilerplate from here on out #
################################
#em.draw_grammars(experiment, sdfs, title_name, nsamples, range(0,niters+1,10))

for psw in prior_shape_weights:
  experiment.subsection('%s with $\sigma=%d$' % (title_name, psw))
  for i in iters_to_draw:
    gram_file = experiment.tmp_file_namer('foo', 'gram', ('psw',psw), ('iter',i))
    experiment.subsubsection('After %d iterations' % i)
    experiment.show_grammar_samples(gram_file=gram_file, name='psw%d_iter%d' % (psw,i), nsamples=nsamples, width='6in')

