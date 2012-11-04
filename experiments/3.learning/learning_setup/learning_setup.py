#!/usr/bin/python

from experiments.common import *
import experiments.em as em

niters = 30
nsamples = 20

name = 'learning_setup'
title_name = 'Learning Setup'
dir = 'output/3.learning/learning_setup'
experiment = Experiment('learning_setup', dir, 'tmp')

example = 'DATA/romer/newann/IMG0000.curve'
sdfs = [ 'DATA/romer/misc/romer%d.sdf' % i for i in [1,2,3] ]
training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]
validation = [ x.rstrip() for x in os.popen('ls DATA/romer/validation/*.curve') ]


em.show_training_etc(experiment, example, training, validation)

for isdf, sdf in enumerate(sdfs):
  # init_gram = experiment.tmp_file_namer('init', 'gram', ('sdf',isdf))
  # xform_gram = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',0))

  # dispatch('./ctor_init.native', example=example, sdf=sdf, output=init_gram)
  # dispatch('cp %s %s' % (init_gram, xform_gram))

  prefix = 'sdf%d' % isdf

  dispatch('./show_sdf.native', sdf=sdf, curve=example, fname=experiment.tmp_file_namer(prefix,'svg'))

  experiment.subsection('Structure \#%d' % (isdf+1))
  experiment.show_svg_image(prefix, '6in')
