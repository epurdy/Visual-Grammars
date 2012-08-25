#!/usr/bin/python

from experiments.common import *

dir = 'output/2.detection/image_parsing'
example = 'DATA/romer/newann/IMG0020.curve'
sdf = 'DATA/romer/misc/romer1.sdf'

def parse_romer(seq, num):
  im = 'DATA/romer/SEQ%d/IMG%03d0.PGM' % (seq,num)
  outdir = '%s/out.s%d.%s.d' % (dir,seq,num)

  doit('./ctf_refining6.native %s %s %s %s' % (
      outdir, im, example, sdf))

# for i in xrange(2,16):
#   parse_romer(1, i)

for i in xrange(0,30):
  parse_romer(2, i)
