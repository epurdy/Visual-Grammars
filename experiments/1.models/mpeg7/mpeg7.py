#!/usr/bin/python
import os, sys, random
os.nice(10)

random.seed()

k = int(sys.argv[1])
ncores = int(sys.argv[2])
deadflag, raceflag = sys.argv[3], sys.argv[4]

# nstragglers = 10
# nstragglers2 = 10

nstragglers = 1
nstragglers2 = 1

def doit(cmd): 
  assert(os.system(cmd) == 0)

figdir = 'experiments/6.structure/mpeg7/output.d'

datadir = '/stage/epurdy/mpeg7'
resultdir = '/stage/epurdy/mpeg7/classification'

ntest = 20
# ntest = 1
ntestclass = 70
nmodel = 20
nmodelclass = 70

neval = 40

lookup = {}

def curvenamer(name, i):
  fmt = '%s/shock.%s/boundary.curve'
  return fmt % (datadir, lookup[name, i]) 

def dilcurvenamer(name, i):
  fmt = '%s/shock.%s/dilated.curve'
  return fmt % (datadir, lookup[name, i]) 

def gramnamer(name, i):
  return '%s/shock.%s/shock.gram' % (datadir, lookup[name, i])

def sdfnamer(name, i):
  return '%s/shock.%s/shock.sdf' % (datadir, lookup[name, i])

def qualnamer(modelname, modeli, testname, testi):
  return '%s/test_%s/%d/%s-%d-%s-%d.qual' % (
    resultdir, testname, testi, 
    modelname, modeli, testname, testi)

def dilqualnamer(modelname, modeli, testname, testi):
  return '%s/test_%s/%d/%s-%d-%s-%d.dil.qual' % (
    resultdir, testname, testi, 
    modelname, modeli, testname, testi)

def allqualnamer(testname, testi):
  return '%s/test_%s/%d/*.qual' % (
    resultdir, testname, testi)

def ranknamer(testname, testi):
  return '%s/test_%s/%d/%s-%d.rank' % (
    resultdir, testname, testi, testname, testi)

def scorenamer(testname, testi):
  return '%s/test_%s/%d/%s-%d.score' % (
    resultdir, testname, testi, testname, testi)

os.system('mkdir -p %s' % resultdir)
os.system('mkdir -p /tmp/thrasher-epurdy')

files = [ line.rstrip() for line in os.popen('ls %s | grep gif' % datadir) ]

classnamehash = {}
classfmts = {}

for f in files:
  assert(f.endswith('.gif'))
  basename = f[:-4]
  assert(f == basename + '.gif')
  pgmname = basename + '.pgm'

  classname, _, number = basename.partition('-')
  lookup[classname, int(number)] = basename

  if classname not in classnamehash:
    classnamehash[classname] = len(classnamehash)

classnames = ['' for x in xrange(70) ]
for name in classnamehash: 
  classnames[classnamehash[name]] = name
for name in classnames:
  assert(name != '')



counter = 0
nparses = 30

def dothething(modelname, modeli, testname, testi):
  global nparses

  if not (os.path.isfile(qualnamer(modelname,modeli,testname,testi)) and
          os.path.isfile(dilqualnamer(modelname,modeli,testname,testi))):
    os.system('rm -f %s %s' % (deadflag, raceflag))
    os.system('mkdir -p %s/test_%s/%d' % (resultdir,testname,testi))
    os.system('touch %s' % (qualnamer(modelname,modeli,testname,testi)))
    os.system('touch %s' % (dilqualnamer(modelname,modeli,testname,testi)))
    cmd = ('/home/epurdy/viz/parse_mpeg_reverse.native %s %s %s %s >/dev/null' % (
        sdfnamer(modelname, modeli), 
        curvenamer(modelname, modeli),
        curvenamer(testname, testi),
        qualnamer(modelname, modeli, testname, testi)))
    doit(cmd)
    os.system('rm -f %s %s' % (deadflag, raceflag))
    cmd = ('/home/epurdy/viz/parse_mpeg_reverse.native %s %s %s %s >/dev/null' % (
        sdfnamer(modelname, modeli), 
        dilcurvenamer(modelname, modeli),
        dilcurvenamer(testname, testi),
        dilqualnamer(modelname, modeli, testname, testi)))
    doit(cmd)
    nparses = nparses - 1
    if nparses == 0:
      sys.exit(0)

def compute_scores(testi, testname):
  if not os.path.isfile(ranknamer(testname,testi)):
    quals = []
    for modelname in classnames[:nmodelclass]:
      for modeli in xrange(1,nmodel+1):
        done = False
        while not done:
          dothething(modelname, modeli, testname, testi)
          qual1 = [ line for line in open(qualnamer(modelname,modeli,testname,testi)) ]
          qual2 = [ line for line in open(dilqualnamer(modelname,modeli,testname,testi)) ]
          if len(qual1) != 1 or len(qual2) != 1:
            if random.randint(0,10) > 8:
              os.system('rm %s' % qualnamer(modelname,modeli,testname,testi))
              os.system('rm %s' % dilqualnamer(modelname,modeli,testname,testi))
              print "qual problem:\n%s\n%s" % (
              qualnamer(modelname,modeli,testname,testi),
              dilqualnamer(modelname,modeli,testname,testi))
              sys.exit()
          else:
            done = True            
        qual1, qual2 = float(qual1[0]), float(qual2[0])
        # penalize dilated version
        qual2 = qual2 + 40.0
        quals.append( (min(qual1,qual2), modelname, modeli) )


    rankfile = open(ranknamer(testname,testi), 'w')

    quals.sort()
    for qual, modelname, modeli in quals:
      rankfile.write('%s %d\t %f\n' % (modelname, modeli, qual))
    rankfile.close()

  elif os.path.isfile(scorenamer(testname,testi)):
    os.system('rm -f %s' % allqualnamer(testname,testi))

  else:
    quals = [ line for line in open(ranknamer(testname,testi)) ]
    quals = [ x.split() for x in quals ]
    quals = [ (float(qual), modelname, modeli) for (modelname,modeli,qual) in quals]

    scorefile = open(scorenamer(testname,testi), 'w')
    nwin = 0

#     confusion = {}
#     for modelname in classnames:
#       confusion[testname,modelname] = 0

    quals = quals[:neval]
    classes = [ modelname for (qual,modelname,modeli) in quals ]
    for modelname in classes:
#       confusion[testname,modelname] = confusion[testname,modelname] + 1
      if modelname == testname:
        nwin = nwin + 1

    print 'nwin', nwin
    scorefile.write('%d\n' % nwin)    
    scorefile.close()
#    return confusion

for testi in xrange(ntest,0,-1):
# for testi in xrange(1,ntest+1):
  for testname in classnames[:ntestclass]:

    if not os.path.isfile(ranknamer(testname,testi)):
      for modelname in classnames[:nmodelclass]:
        for modeli in xrange(1,nmodel+1):
          counter = counter + 1

          if counter % ncores == k or random.randint(0,ncores-1) < nstragglers2:
            dothething(modelname, modeli, testname, testi)


    if counter % ncores == k or random.randint(0,ncores-1) < nstragglers:
      compute_scores(testi, testname)

  print 'testi', testi

# if k == 0:
#   confusion = {}
#   for modelname in classnames:
#     for testname in classnames:
#       confusion[testname,modelname] = 0

#   for testname in classnames:
#     for i in xrange(1,testi+1):
#       conf = compute_scores(i, testname)
#       for tname, mname in conf:
#         confusion[tname, mname] = confusion[tname,mname] + conf[tname,mname]

#   print
#   for i, modelname in enumerate(classnames):
#     print '%3d' % i, modelname


#   print
#   print '     ',
#   for i, modelname in enumerate(classnames):
#     print '%3d' % i,
#   print      

#   for i, testname in enumerate(classnames):
#     print '%3d  ' % i,
#     for modelname in classnames:
#       if confusion[testname, modelname] > 0:
#         print '%3d' % confusion[testname,modelname],
#       else:
#         print '   ',
# #        print '%3d' % 0,
#     print


      
