#!/usr/bin/python

from experiments.common import *

dir = 'output/2.detection/local_inference'
example = 'DATA/romer/newann/IMG0020.curve'
sdf = 'DATA/romer/misc/romer1.sdf'

def parse_romer(seq, num):
  im = 'DATA/romer/SEQ%d/IMG%04d.PGM' % (seq,num)
  outdir = '%s/out.s%d.%04d.d' % (dir,seq,num)

  doit('./ctf_refining6.native %s %s %s %s' % (
      outdir, im, example, sdf))

# for s in xrange(1,4):
for s in [1]:
  for i in xrange(10,280,10):
#  for i in [20,30,40,50]:
    parse_romer(s, i)
