import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

def svg_to_png(dir, svgname, pngname):
  cmd = ('inkscape tmp/%s -D -e %s/output.d/%s' %
         (svgname, dir, pngname))
  doit(cmd)

def show_curves(dir, filenames, svgname, pngname):
  cmd = ('./show_curves.native -fname tmp/%s -title \'\' %s' %
         (svgname, ' '.join(filenames)))
  doit(cmd)

  svg_to_png(dir, svgname, pngname)
#   cmd = ('inkscape tmp/%s -D -e %s/output.d/%s' %
#          (svgname, dir, pngname))
#   doit(cmd)

def show_single_curve(dir, filename, svgname, pngname):
  cmd = ('./show_curve.native %s tmp/%s' % (filename, svgname))
  doit(cmd)

  svg_to_png(dir, svgname, pngname)

def show_grammar(dir, gramdir, gramname):
  doit('mkdir -p %s/output.d/%s' % (dir,gramdir))
  doit('rm -rf %s/output.d/%s/*' % (dir,gramdir))
  doit(('./show_grammar.native -gramfile tmp/%s ' +
        '-dir %s/output.d/%s -latexdir %s/output.d/%s -title \'\' ') % (
      gramname, dir, gramdir, dir, gramdir))
# add -rules above?

def show_curves_eps(dir, filenames, svgname, epsname):
  cmd = ('inkscape tmp/%s -E %s/output.d/%s' %
         (svgname, dir, epsname))
  doit(cmd)
  
