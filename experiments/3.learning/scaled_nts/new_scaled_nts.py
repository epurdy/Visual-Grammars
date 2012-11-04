#!/usr/bin/python

from experiments.common import *

niters = 30

dir = 'output/3.learning/scaled_nts'
example = 'DATA/romer/newann/IMG0000.curve'
sdfs = [ 'DATA/romer/misc/romer%d.sdf' % i for i in [1,2,3] ]
training = [ 'DATA/romer/newann/IMG%03d0.curve' % i for i in xrange(28) ]

validation = training

experiment = Experiment('scaled_nts', dir, 'tmp')

for i in xrange(1,16):
  dispatch('./scaled_nts.native', anon=[str(i)])

# # make the initial grammars
# for isdf, sdf in enumerate(sdfs):
#   init_gram = experiment.tmp_file_namer('texture', 'gram', ('sdf',isdf))
#   xform_gram = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',0))

#   dispatch('./ctor_init.native', example=example, sdf=sdf, output=init_gram)
#   dispatch('./xform_correlated.native', input=init_gram, output=xform_gram, ncopies=5, nrulecopies=5)

# # experiment.start_table('xent', '|l|l|l|', '''
# # (Fake) cross-entropy scores across iterations. Each column represents a different starting structure.''')

# # # do the retraining and xent measures
# # for i in xrange(niters + 1):
# #   for isdf, sdf in enumerate(sdfs):
# #     gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i))
# #     next_gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i+1))
# #     qual_file = experiment.tmp_file_namer('foo', 'qual', ('sdf',isdf), ('iter',i))

# #     dispatch('./xent.native', gram=gram_file, qual=qual_file, anon=validation)
    
# #     if i < niters:
# #       dispatch('./do_retrain.native', input=gram_file, output=next_gram_file, anon=training)
    
# #     experiment.table_cell_from_file('xent', qual_file)
# #     if isdf < len(sdfs)-1:
# #       experiment.table_end_cell('xent')
# #   experiment.table_end_row('xent')

# # experiment.end_table('xent')

# # make pictures of grammars
# for isdf, sdf in enumerate(sdfs):
#   experiment.subsection('Correlated tuning with SDF %d' % (isdf+1))
#   for i in xrange(0,niters+1,10):
#     gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i))
#     experiment.subsubsection('After %d iterations' % i)
#     experiment.show_grammar_samples(gram_file=gram_file, name='iter%d' % i, nsamples=20, width='6in')
