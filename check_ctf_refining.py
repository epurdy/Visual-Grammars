
from experiments.common import *

for i in xrange(0,280,10):
  doit ('./ctf_refining6.native romer/clean/IMG%04d.PGM detex/%04d.d' % (i,i))
