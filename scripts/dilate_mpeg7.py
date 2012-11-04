#!/usr/bin/python
import os

fnames = [ x for x in os.popen("ls xstage/mpeg7/*.gif") ]

for fname in fnames:
  # os.system("./dilate.native %s")
  fname = fname.rstrip()
  fname = fname[:-4]
  dir, _ , rest = fname.rpartition('/')
  sdir = "%s/shock.%s" % (dir, rest)

  print fname

  cmd = "convert %s.gif %s/im.pgm" % (fname, sdir)
  print cmd
  assert(os.system(cmd) == 0)

  cmd = "./dilate.native %s/im.pgm %s/dilated.curve" % (sdir, sdir)
  print cmd
  assert(os.system(cmd) == 0)

