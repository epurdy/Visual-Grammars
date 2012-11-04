#!/usr/bin/python

from experiments.common import *
import experiments.em as em

niters = 30
nsamples = 20

name = 'full_tuning'
title_name = 'Tuning with full grammar'
dir = 'output/3.learning/full_tuning'
experiment = Experiment(name, dir, 'tmp')

example = 'DATA/romer/newann/IMG0000.curve'
sdfs = [ 'DATA/romer/misc/romer1.sdf' ]
training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]
validation = [ x.rstrip() for x in os.popen('ls DATA/romer/validation/*.curve') ]

# make the initial grammars
for isdf, sdf in enumerate(sdfs):
  init_gram = experiment.tmp_file_namer('init', 'gram', ('sdf',isdf))
  xform_gram = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',0))

  dispatch('./ctor_full.native', example=example, output=init_gram)
  dispatch('./xform_correlated.native', input=init_gram, output=xform_gram, ncopies=1, nrulecopies=1)

# do the retraining and xent measures
for isdf, sdf in enumerate(sdfs):
  for i in xrange(niters):
    gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i))
    next_gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i+1))
    dispatch('./do_retrain.native', input=gram_file, output=next_gram_file, anon=training,
             prior_shape=1000.0, prior_mean=100.0)
    
################################
# boilerplate from here on out #
################################
em.measure_cross_entropy(experiment, sdfs, niters, validation)
em.draw_grammars(experiment, sdfs, title_name, nsamples, range(0,niters+1,10))

for isdf, sdf in enumerate(sdfs):
  for i in xrange(0,niters+1,10):
    gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i))
    thedir = '%s/sdf%d_iter%d' % (dir,isdf,i)
    dispatch('mkdir %s' % thedir)
    dispatch('./show_grammar.native', gramfile=gram_file, dir=thedir, title='\'\'', rules='')
