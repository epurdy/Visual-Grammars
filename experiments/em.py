
from experiments.common import dispatch


def show_training_etc(experiment, example, training, validation):
  experiment.subsection('Example Curve')
  experiment.show_curves([example], 'examples', '1in')
  experiment.subsection('Training Curves')
  experiment.show_curves(training, 'training', '6in')
  experiment.subsection('Validation Curves')
  experiment.show_curves(validation, 'validation', '6in')


def measure_cross_entropy(experiment, sdfs, niters, validation):
  fmt = ('|l' * len(sdfs)) + '|'
  experiment.start_table('xent', '|l|l|l|', '''
  Cross-entropy scores across iterations. Each column represents a different starting structure.''')
  for i in xrange(niters + 1):
    for isdf, sdf in enumerate(sdfs):
      gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i))
      qual_file = experiment.tmp_file_namer('foo', 'qual', ('sdf',isdf), ('iter',i))

      dispatch('./xent.native', gram=gram_file, qual=qual_file, anon=validation)
      experiment.table_cell_from_file('xent', qual_file)

      if isdf < len(sdfs)-1:
        experiment.table_end_cell('xent')

    experiment.table_end_row('xent')
  experiment.end_table('xent')
  

def draw_grammars(experiment, sdfs, title_name, nsamples, iters_to_draw):
  for isdf, sdf in enumerate(sdfs):
    experiment.subsection('%s with SDF %d' % (title_name,isdf+1))
    for i in iters_to_draw:
      gram_file = experiment.tmp_file_namer('foo', 'gram', ('sdf',isdf), ('iter',i))
      experiment.subsubsection('After %d iterations' % i)
      experiment.show_grammar_samples(gram_file=gram_file, name='iter%d' % i, nsamples=nsamples, width='6in')

