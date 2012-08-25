#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

dir = 'experiments/6.structure/shock_graph/output.d'

# files = [ line.rstrip() for line in os.popen('ls /var/tmp/epurdy_mpeg7 | grep gif | grep 10') ]
files = [ line.rstrip() for line in os.popen('ls /var/tmp/epurdy_mpeg7 | grep gif') ]

for f in files:
  assert(f.endswith('.gif'))
  basename = f[:-4]
  assert(f == basename + '.gif')
  pgmname = basename + '.pgm'
#   doit('convert /var/tmp/epurdy_mpeg7/%s /var/tmp/epurdy_mpeg7/%s' % (
#       f, pgmname))
#   doit('./shock_graph.native /var/tmp/epurdy_mpeg7/%s /var/tmp/epurdy_mpeg7/shock.%s' %  (
#       pgmname, basename))

#   doit('inkscape /var/tmp/epurdy_mpeg7/shock.%s/sdf.svg -E experiments/6.structure/shock_graph/output.d/%s.eps' % (
#       basename, basename))

  doit('./make_grammar.native /var/tmp/epurdy_mpeg7/shock.%s/boundary.curve /var/tmp/epurdy_mpeg7/shock.%s/shock.sdf /var/tmp/epurdy_mpeg7/shock.%s/shock.gram' % (
      basename, basename, basename))


    
