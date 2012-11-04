#!/usr/bin/python

import os

def doit(cmd):
  print cmd
  assert(os.system(cmd) == 0)

hintsls = [ x.rstrip() for x in open('romer/hints') ]
hintsls = [ z.split() for z in hintsls if len(z.split()) == 2 ]
print hintsls
hints = {}
for (x,y) in hintsls:
  hints[x] = y

ims = [ x.rstrip() for x in os.popen('ls romer/ann/IMG*') ]

d = 'romer/ann'

for im in ims: #[:5]:
  im = im.replace('romer/ann/', '')
  number = im.replace('IMG','').replace('.PPM','')
  annot = 'auto_annot%s.annotation' % number
  curve = 'auto_curve%s.curve' % number
  labeled = 'auto_labeled%s.ppm' % number

  if os.system('ls %s/annot%s.annotation' % (d,number)) == 0:
    print 'Annotation for %s found already' % im
    doit('cp %s/annot%s.annotation %s/%s' % (d,number,d,annot))
    cmd = ('./read_annotation.native %s/%s %s/%s %s/labeled%s.ppm > /dev/null' 
           % (d, annot, d, im, d, number))
    doit(cmd)
    doit('cp %s/labeled%s.ppm %s/%s' % (d,number,d,labeled))
  else:
    oldannot = 'auto_annot%s.annotation' % hints[number]

    print 'im = %s' % im
    print 'annot = %s' % annot
    print 'oldannot = %s' % oldannot
    print 'curve = %s' % curve
    print 'labeled = %s' % labeled

    cmd = 'touch %s/%s' % (d, annot)
    doit(cmd)

    cmd = ('./read_annotation.native %s/%s %s/%s %s/%s > romer/tmp/annot.annotation' 
           % (d, annot, d, im, d, labeled))
    doit(cmd)

    cmd = ('./guess_annotation.native %s/%s romer/tmp/annot.annotation > romer/tmp/annot2.annotation' 
           % (d, oldannot))
    doit(cmd)

    cmd = ('./read_annotation.native romer/tmp/annot2.annotation %s/%s %s/%s > %s/%s' 
           % (d, im, d, labeled, d, annot))
    doit(cmd)


