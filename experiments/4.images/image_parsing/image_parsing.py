#!/usr/bin/python

import os

def doit(cmd): 
  print cmd
  assert(os.system(cmd) == 0)

dir = 'experiments/4.images/image_parsing/output.d/'
# example = 'romer/ann/curve0000.curve'
example = 'romer/ann/curve0020.curve'
sdf = 'romer/misc/romer1.sdf'

for i in xrange(2,16):
  im = 'romer/clean/IMG%03d0.PGM' % i
  network = dir + 'network.%03d0.pgm' % i
  gfname = dir + 'network.%03d0.gf' % i
  yieldprefix = dir + '%03d0.yield' % i
  parseprefix = dir + '%03d0.parse' % i

#   doit('./net_of_image.native -input %s -netname %s -gfname %s -granularity %d -maxlen %d -backgroundcost %f -maxcost %f'  % 
#       (im, network, gfname, 32, 10, 1000.0, 100.0))

#   doit('./cheating_image_parsing.native %s %s %s %s %s %s' % (
#       'romer/ann/curve%03d0.curve' % i,
#       sdf, network, yieldprefix, gfname, 'romer/ann/curve%03d0.curve' % i))


  doit('./cheating_net_of_image.native -curve %s -input %s -netname %s -gfname %s -granularity %d -maxlen %d -backgroundcost %f -maxcost %f'  % 
      ('romer/ann/curve%03d0.curve' % i, im, network, gfname, 32, 10, 1000.0, 100.0))


  doit('./standard_image_parsing.native %s %s %s %s %s' % (
      example, sdf, network, yieldprefix, gfname))
