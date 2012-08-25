

class Curve:
  def __init__(self, fname):
    self.fname = fname

class CurveSet:
  def __init__(self, fnames):
    self.fnames = fnames

class Sdf:
  def __init__(self, fname):
    self.fname = fname

class Grammar:
  def __init__(self, fname):
    self.fname = fname


class Task:
  def __init__(self, cmdline):
    self.cmdline = cmdline

  def do(self, **kwargs):
    doit(self.cmdline % kwargs)

class Fn:
  def __init__(self, f):
    self.f = f
  def do(self, **kwargs):
    self.f(**kwargs)... figure this one out

showcurves = Task(
'./show_curves.native -fname %(svgname)s -title \'\' %(curvenames)s')

convert = Task('inkscape %(basename)s.svg -E %(basename)s.eps')

romer_example = Curve('romer/newann/IMG0000.curve')
romer_training = CurveSet(
[ 'romer/newann/IMG%03d0.curve' % i for i in xrange(28) ])



