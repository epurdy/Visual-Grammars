import os, time, sys, re

seenscores = {}

# oldscoresf = open('flipscores')
# oldscores = [ int(x) for x in oldscoresf ]
# oldscoresf.close()

def parse_dirname(dirname):
  dirname = dirname.strip()  
  matchobj = re.match('xstage/(|mpeg7/)classification(|[a-zA-Z0-9_\.]*)/test_([a-zA-Z0-9_\.]*)/([0-9]*)', dirname)
  items = [ x for x in matchobj.groups() ]
  name, num = items[-2], int(items[-1])
  return dirname, name, num

oldscoresls = os.popen('find xstage/classification_FULL_dilboth_17.37/ -maxdepth 2 -mindepth 2')
oldscores = {}
for x in oldscoresls:
  dirname, name, num = parse_dirname(x)
#  f = open(x)
  f = open('%s/%s-%s.score' % (dirname,name,num))
  score = [ i for i in f ]
  f.close()
  assert(len(score) == 1)
  score = int(score[0])
  oldscores[(name,num)] = score


while True:
  scores = []
  oscores = []

  print 'infected nodes:'
  os.system('ls xstage/comm/thrash.here.* | wc')
  
  for x in os.popen('find xstage/mpeg7/classification/ -maxdepth 2 -mindepth 2 | sort'):
    dirname, name, num = parse_dirname(x)

    if (name,num) in seenscores:
      scores.append(seenscores[(name,num)])
      oscores.append(oldscores[(name,num)])
      continue

    if not os.system('ls %s/*.score 2>/dev/null >/dev/null' % dirname):
      fnames = [ x for x in os.popen('ls %s/*.score' % dirname) ]
      assert(len(fnames) == 1)
      fname = fnames[0]
      fname = fname.rstrip()
      f = open(fname)
      score = [ x for x in f ]
      assert(len(score) == 1)
      score = int(score[0])
      scores.append(score)
      oscores.append(oldscores[(name,num)])
      print '+', score, fname
      seenscores[(name,num)] = score
    else:
      num = [ x for x in os.popen('ls %s/ | grep qual | wc -l' % dirname) ]
      assert(len(num) == 1)
      num = int(num[0])
      print '-', num, dirname

#   print 'new:', ' '.join("%2d" % x for x in scores)
#   print 'old:', ' '.join("%2d" % x for x in oldscores[:len(scores)+1])
  print 'num:', len(scores)
  if len(scores) > 0:
    print 'avg:', sum(scores) / float(len(scores))

  if len(scores) > 0:
    assert(len(scores) == len(oscores))
    par = sum(scores) - sum(oscores)
    print 'par:', par, (float(par) / len(scores))

  print
  time.sleep(10)
