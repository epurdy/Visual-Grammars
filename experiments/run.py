#!/usr/bin/python
'''Usage:
run.py list
  List all experiments. Completed experiments are marked +.

run.py all
  Run all experiments, skipping completed ones.

run.py regen all
  Run all experiments, regenerating completed ones.

run.py EXNAME1 [EXNAME2]
  Run experiment matching the given names, skipping completed ones.

run.py regen EXNAME1 [EXNAME2]
  Run experiment matching the given names, regenerating completed ones.

run.py export
  Compile latex files into experiments.ps.
'''
import os,sys,string

compiled = 0
lazy = 1

def doit(cmd):
  print cmd
  assert(os.system(cmd) == 0)

def get_subdirs(d, root):
  subdirs = [ line.rstrip()[:-1] for line in os.popen('ls -F %s | grep /' % d) ]
  subdirs = [ '%s/%s' % (root, sd) for sd in subdirs ]
  return subdirs

def get_exes(d):
  exes = [ line.rstrip() for line in os.popen('ls %s | grep .ml' % d) ]
  exes = [ ex.replace('.ml','.native') for ex in exes if ex.endswith('.ml') ]
  exes = [ '%s/%s' % (d,ex) for ex in exes ]
  return exes

def check_cleanness(d):
  suffixes = ['output\.d', '~', '\.ml', '\.py', '\.tex', '\.aux']
  cmd = ('ls %s ' % d) + ' '.join(' | grep -v %s' % sx for sx in suffixes)
  print 'Checking that directory is clean:\n ', cmd
  if os.system(cmd) == 0:
    print '  Get rid of files listed above in directory %s' % d
    sys.exit()
  print 'Directory is clean!'

def run_ex(name, d, outputd, exes):
  global compiled
  doit('rm -rf tmp/*')
  doit('mkdir -p %s' % outputd)
  check_cleanness(d)
  if not lazy:
    doit('rm -f %s/generated.flag' % outputd)

  if os.system('ls %s/generated.flag' % outputd) != 0:

    if not compiled:
      doit('ocamlbuild -cflags \'-warn-error -Ayxp\' ' + ' '.join(exes))
      compiled = 1

    doit('rm -rf %s/*' % outputd)
    doit('./%s/%s.py' % (d, name))
    doit('touch %s/generated.flag' % outputd)
    check_cleanness(d)

  else:
    print '\n>>> Found %s/generated.flag, skipping\n\n' % outputd

subdirs = get_subdirs('experiments', 'experiments')
outputsubdirs = get_subdirs('experiments', 'output')
subsubdirs = sum(( get_subdirs(d,d) for d in subdirs ), [])
outputsubsubdirs = [ string.replace(s,'experiments','output') for s in subsubdirs ]
exes = sum(( get_exes(d) for d in (subsubdirs + ['bin'])), [])

exlookup = {}
outlookup = {}
for sd, osd in zip(subsubdirs, outputsubsubdirs):
  name = sd.split('/')[-1]
  exlookup[name] = sd
  outlookup[name] = osd

for request in sys.argv[1:]:
  if request == 'regen':
    lazy = 0
    continue

  if request == 'list':
    keys = sorted(exlookup.keys(), (lambda x,y: cmp(exlookup[x],exlookup[y])))
    for name in keys:
      if os.system('ls %s | grep generated.flag >/dev/null' % outlookup[name]) == 0:
        print '+ %s  %s  %s' % (name, ' ' * max(15 - len(name),0), exlookup[name])
      else:
        print '  %s  %s  %s' % (name, ' ' * max(15 - len(name),0), exlookup[name])
    continue

  if request == 'export':
    doit('cd experiments/ && latex experiments.tex && latex experiments.tex && latex experiments.tex && dvips experiments.dvi')
    continue

  if request == 'export2':
    doit('cd experiments/ && latex experiments2.tex && latex experiments2.tex && latex experiments2.tex && dvips experiments2.dvi')
    continue

  if request == 'all':
    print "\n>>> Running all experiments\n"
    for sd in subsubdirs:
      print ' ', sd
    print 
    
    keys = sorted(exlookup.keys(), (lambda x,y: cmp(exlookup[x],exlookup[y])))
    for name in keys:
      print "\n>>> Running experiment '%s'\n" % name
      run_ex(name, exlookup[name], outlookup[name], exes)
    continue

  else:
    found = [ name for name in exlookup if name.find(request) >= 0 ]

    if len(found) == 0:
      print "Couldn't find experiment matching '%s'" % request
      print "Available experiments:"
      for sd in subsubdirs:
        print ' ', sd
      for name in exlookup:
        print ' ', name
      sys.exit()
    if len(found) > 1:
      print "Found multiple experiments matching '%s'" % request
      for name in found:
        print "  Could be:", exlookup[name]
      sys.exit()

    name = found[0]

    print "\n>>> Running experiment '%s'\n" % name

    run_ex(name, exlookup[name], outlookup[name], exes)
    continue



