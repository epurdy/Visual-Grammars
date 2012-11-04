#!/usr/bin/python

from experiments.common import *
import experiments.em as em

niters = 30
nsamples = 20

name = 'comparison_parzen'
dir = 'output/1.models/comparison_parzen'
experiment = Experiment(name, dir, 'tmp')

example = 'DATA/romer/newann/IMG0000.curve'
sdfs = [ 'DATA/romer/misc/romer%d.sdf' % i for i in [1,2,3] ]
training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]
validation = training

dispatch('./comparison_parzen.native', prefix='tmp/parzen', nsamples=nsamples, anon=training)
fnames = [ 'tmp/parzen.%04d.curve' % i for i in xrange(nsamples) ]
experiment.show_curves(fnames, 'parzen', '6in')
