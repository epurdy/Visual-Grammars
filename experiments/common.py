import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

def load_dataset(fname):
  f = open(fname)
  data = [ name.strip() for name in f ]
  f.close()
  return data

def svg_to_png(dir, svgname, pngname):
  cmd = ('inkscape %s/%s -D -e %s/%s' %
         (dir, svgname, dir, pngname))
  doit(cmd)

def show_curves(dir, filenames, svgname, pngname):
  cmd = ('./show_curves.native -fname %s/%s -title \'\' %s' %
         (dir, svgname, ' '.join(filenames)))
  doit(cmd)
  svg_to_png(dir, svgname, pngname)

def show_single_curve(dir, filename, svgname, pngname):
  cmd = ('./show_curve.native %s %s/%s' % (filename, dir, svgname))
  doit(cmd)
  svg_to_png(dir, svgname, pngname)

# def show_grammar(dir, gramdir, gramname):
#   doit('mkdir -p %s/output.d/%s' % (dir,gramdir))
#   doit('rm -rf %s/output.d/%s/*' % (dir,gramdir))
#   doit(('./show_grammar.native -gramfile tmp/%s ' +
#         '-dir %s/output.d/%s -latexdir %s/output.d/%s -title \'\' ') % (
#       gramname, dir, gramdir, dir, gramdir))
# # add -rules above?

# def show_curves_eps(dir, filenames, svgname, epsname):
#   cmd = ('inkscape tmp/%s -E %s/output.d/%s' %
#          (svgname, dir, epsname))
#   doit(cmd)
  
