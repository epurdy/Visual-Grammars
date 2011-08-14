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
import os,sys

compiled = 0
lazy = 1

def doit(cmd):
  print cmd
  assert(os.system(cmd) == 0)

def get_subdirs(d):
  subdirs = [ line.rstrip()[:-1] for line in os.popen('ls -F %s | grep /' % d) ]
  subdirs = [ '%s/%s' % (d, sd) for sd in subdirs ]
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

def run_ex(name, d, exes):
  global compiled
  doit('rm -rf tmp/*')
  doit('mkdir -p %s/output.d' % d)
  check_cleanness(d)
  if not lazy:
    doit('rm -f %s/output.d/generated.flag' % d)

  if os.system('ls %s/output.d/generated.flag' % d) != 0:

    if not compiled:
      doit('ocamlbuild -cflags \'-warn-error -Ayxp\' ' + ' '.join(exes))
      compiled = 1

    doit('rm -rf %s/output.d/*' % d)
    doit('./%s/%s.py' % (d, name))
    doit('touch %s/output.d/generated.flag' % d)
    check_cleanness(d)

  else:
    print '\n>>> Found %s/output.d/generated.flag, skipping\n\n' % d

subdirs = get_subdirs('experiments')
subsubdirs = sum(( get_subdirs(d) for d in subdirs ), [])
exes = sum(( get_exes(d) for d in (subsubdirs + ['bin'])), [])

lookup = {}
for sd in subsubdirs:
  name = sd.split('/')[-1]
  lookup[name] = sd

for request in sys.argv[1:]:
  if request == 'regen':
    lazy = 0
    continue

  if request == 'list':
    keys = sorted(lookup.keys(), (lambda x,y: cmp(lookup[x],lookup[y])))
    for name in keys:
      if os.system('ls %s/output.d | grep generated.flag >/dev/null' % lookup[name]) == 0:
        print '+ %s  %s  %s' % (name, ' ' * max(15 - len(name),0), lookup[name])
      else:
        print '  %s  %s  %s' % (name, ' ' * max(15 - len(name),0), lookup[name])
    continue

  if request == 'export':
    doit('cd experiments/ && latex experiments.tex && latex experiments.tex && latex experiments.tex && dvips experiments.dvi')
    continue

  if request == 'all':
    print "\n>>> Running all experiments\n"
    for sd in subsubdirs:
      print ' ', sd
    print 
    
    keys = sorted(lookup.keys(), (lambda x,y: cmp(lookup[x],lookup[y])))
    for name in keys:
      print "\n>>> Running experiment '%s'\n" % name
      run_ex(name, lookup[name], exes)
    continue

  else:
    found = [ name for name in lookup if name.find(request) >= 0 ]

    if len(found) == 0:
      print "Couldn't find experiment matching '%s'" % request
      print "Available experiments:"
      for sd in subsubdirs:
        print ' ', sd
      sys.exit()
    if len(found) > 1:
      print "Found multiple experiments matching '%s'" % request
      for name in found:
        print "  Could be:", lookup[name]
      sys.exit()

    name = found[0]

    print "\n>>> Running experiment '%s'\n" % name

    run_ex(name, lookup[name], exes)
    continue



