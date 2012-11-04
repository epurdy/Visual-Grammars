#!/usr/bin/python

from experiments.common import *
import experiments.em as em

niters = 30
nsamples = 20

name = 'comparison_gaussians'
dir = 'output/1.models/comparison_gaussians'
experiment = Experiment(name, dir, 'tmp')

example = 'DATA/romer/newann/IMG0000.curve'
sdfs = [ 'DATA/romer/misc/romer%d.sdf' % i for i in [1,2,3] ]
training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]
validation = training

dispatch('./comparison_gaussians.native', prefix='tmp/gaussians', nsamples=nsamples, anon=training)

experiment.subsection('Maximum likelihood')
fnames = [ 'tmp/gaussians.%04d.curve' % i for i in xrange(nsamples) ]
experiment.show_curves(fnames, 'gaussians', '6in')

experiment.subsection('Variance decreased')
fnames = [ 'tmp/gaussians.%04d.decvar.curve' % i for i in xrange(nsamples) ]
experiment.show_curves(fnames, 'gaussians_decvar', '6in')

experiment.subsection('Variance set to zero')
fnames = [ 'tmp/gaussians.%04d.novar.curve' % i for i in xrange(nsamples) ]
experiment.show_curves(fnames, 'gaussians_novar', '6in')



