import os

def dispatch(cmd, **kwargs):
  cmdline_parts = [cmd] + [ '-%s %s' % (key, kwargs[key]) for key in kwargs if key is not 'anon' ]
  if 'anon' in kwargs:
    cmdline_parts = cmdline_parts + kwargs['anon']
  cmdline = ' '.join(cmdline_parts)
  code = os.system(cmdline)
  assert(code == 0)

def clean_dir(d):
  dispatch('mkdir -p', anon=[d])
  dispatch('rm -rf', anon=['%s/*' % d])

class Experiment:
  '''Class that allows experimental code to generate figures, tables and text'''
  def __init__(self, name, outdir, tmpdir):
    self.experiment_name = name
    self.outdir = outdir
    self.tmpdir = tmpdir
    clean_dir(outdir)
    clean_dir(tmpdir)

    self.f = open('%s/out.tex' % outdir , 'w')

    self.tables = {}
    self.captions = {}

  def finalize(self):
    self.f.close()

  def tmp_file_namer(self, name, ext, *params):
    param_string = '_'.join('%s%s' % (k,v) for (k,v) in params)
    return '%s/%s_%s_%s.%s' % (self.tmpdir, self.experiment_name, name, param_string, ext)

  def out_file_namer(self, name, ext, *params):
    param_string = '_'.join('%s%s' % (k,v) for (k,v) in params)
    return '%s/%s_%s_%s.%s' % (self.outdir, self.experiment_name, name, param_string, ext)

  def show_svg_image(self, fname_prefix, width):
    svg_name = self.tmp_file_namer(fname_prefix, 'svg')
    png_name = self.out_file_namer(fname_prefix, 'png')
    dispatch('inkscape %s' % svg_name, D='', e=png_name)
    self.f.write('\\includegraphics[width=%s]{%s}\n' % (width, png_name))
    self.f.flush()

  def show_curves(self, curve_files, fname_prefix, width):
    svg_name = self.tmp_file_namer(fname_prefix, 'svg')
    dispatch('./show_curves.native', fname=svg_name, title="''", anon=curve_files)
    self.show_svg_image(fname_prefix, width)

  def show_grammar_samples(self, gram_file, name, nsamples, width):    
    fnames = [ self.tmp_file_namer(name, 'curve', ('n',i)) for i in xrange(nsamples) ]
    dispatch('./sample_from_grammar.native', gramfile=gram_file, anon=fnames)
    self.show_curves(fnames, fname_prefix=name, width=width)

  # def show_grammar_rules(self, gramdir, gramname):
  #   doit('mkdir -p %s/%s' % (dir,gramdir))
  #   doit('rm -rf %s/%s/*' % (dir,gramdir))
  #   doit(('./show_grammar.native -gramfile tmp/%s ' +
  #         '-dir %s/%s -latexdir %s/%s -title \'\' ') % (
  #       gramname, dir, gramdir, dir, gramdir))

  def subsection(self, title):
    self.f.write('\\subsection{%s}\n\n' % title)

  def subsubsection(self, title):
    self.f.write('\\subsubsection{%s}\n\n' % title)

  def start_table(self, key, fmt, caption):
    assert(key not in self.tables)    
    self.tables[key] = '\\begin{table}\\begin{tabular}{%s}\n\\hline\n' % fmt
    self.captions[key] = caption

  def table_cell_from_file(self, key, fname):
    f = open(fname, 'r')
    lines = [ line.rstrip() for line in f ]
    lines = ' '.join(lines)
    f.close()
    self.tables[key] = self.tables[key] + lines

  def table_cell_from_string(self, key, s):
    self.tables[key] = self.tables[key] + s

  def table_hrule(self, key):
    self.tables[key] = self.tables[key] + '\hline\\\\\n'

  def table_end_cell(self, key):
    self.tables[key] = self.tables[key] + '&'

  def table_end_row(self, key):
    self.tables[key] = self.tables[key] + '\\\\\n'

  def end_table(self, key):
    self.tables[key] = self.tables[key] + ('\\hline\n\\end{tabular}\\caption{%s}\\end{table}\n\n' % 
                                           self.captions[key])
    self.f.write(self.tables[key])
    self.f.flush()
    del self.tables[key]
    del self.captions[key]

# def load_dataset(fname):
#   f = open(fname)
#   data = [ name.strip() for name in f ]
#   f.close()
#   return data

