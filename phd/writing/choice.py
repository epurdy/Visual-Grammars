#!/usr/bin/python

import numpy as N
import sys

if len(sys.argv) not in [3,4]:
  print 'Usage: %s N t [-v|--verbose]' % (sys.argv[0])
  sys.exit()

# n = 128
# t = 2

verbose = 0
args = sys.argv[1:]
if '--verbose' in args:
  verbose = 1
  args.remove('--verbose')
if '-v' in args:
  verbose = 1
  args.remove('-v')

n = int(args[0])
t = int(args[1])

K = int(N.ceil(N.log2(n)))

print 'log n=%s, t=%s\n' % (K,t)

max_level = K/t
assert( max_level == int(float(K)/t) )

def cleanup(ls):
  rv = list(set(ls))
  rv.sort()
  return rv

def make_naivelevel(n,k):
  grain = 2 ** (t*k)
  bound = grain * (2 ** (2*t))

  naivelevel = []
  for x in xrange(0, n+1, grain):
    for y in xrange(x+grain, n+1, grain):
      if y-x <= bound:
        naivelevel.append( (x,y) )

  return cleanup(naivelevel)

def whatlevel_one(x):
  if x == 0:
    return max_level
  rv = 0
  ratio = 2 ** t
  while ratio*(x/ratio) == x:
    x /= ratio
    rv += 1
  return rv

def whatlevel( (x,y) ):
  return min( whatlevel_one(x), whatlevel_one(y))

def allowable(c, k):
  ln = c[1] - c[0]
  if ln <= 2 ** (t*k + 2*t):
    return 1
  return 0

def all_levels(c):
  k = whatlevel(c)
  rv = []
  while k >= 0 and allowable(c, k):
    rv.append(k)
    k -= 1
  return rv

counts = []

levels = [ [] for x in xrange(max_level + 1) ]
for lvl in all_levels( (0,n) ):
  levels[lvl].append( (0,n) )

nrules = 0

for k in xrange(max_level,0,-1):  
  grain = 2 ** (t*k)
  thresh = grain * (2 ** t) # small / large cutoff
  bound = grain * (2 ** (2*t)) # largest allowable

  active = levels[k][:]
  seen = {}
  for c in active: 
    seen[c] = 1

  while len(active) > 0:
    (x,y) = active.pop()

    if verbose:
      print '-' * 10
      print '    popped (%d,%d)' % (x,y)

    if (y-x) <= thresh:
      if verbose:
        print '    SMALL: (%d) <= %d < %d' % (y-x, thresh, bound)
      subgrain = grain / (2 ** t)
    else:
      if verbose:
        print '    LARGE: %d < (%d) <= %d' % (thresh, y-x, bound)
      subgrain = grain

    children = []
    for z in xrange(x+subgrain, y, subgrain):
      # (x,y) -> (x,z) + (z,y)
      nrules += 1
      children.append( (x,z) )
      children.append( (z,y) )
    children.sort()
#    print '    children: ', children, '\n'
    counts.append( (x,y,len(children)/2) )

    for c in children:
      lvls = all_levels(c)
#      print '    LEVELS: %s, %s' % (c,lvls)
      for lvl in lvls:
        levels[lvl].append(c)
      
      if k in lvls:
        if c not in seen:
          active.append(c)
          seen[c] = 1

  levels[k-1] = cleanup(levels[k-1])
  levels[k] = cleanup(levels[k])
  naivelevel = make_naivelevel(n,k)

  if verbose:
    print '\n    Level %d: %s    \n\n' % (k, levels[k])
#    print '\n    Next Level (%d): %s    \n\n' % (k-1, levels[k-1])
    print '\n    Naive Level %d: %s    \n\n' % (k, naivelevel)
    print '     diff = %s' % [ x for x in naivelevel if x not in levels[k] ]
    print 'len(level)=%d,   len(naivelevel)=%d' % (len(levels[k]), len(naivelevel))
    print 'nrules = %d\n' % nrules
    print 'nrules / n = %s\n\n\n' % (nrules/float(n))
    print '2^(4t) = %d' % (2 ** (4*t))
    print '=' * 40, '\n\n'

if verbose:
  print counts

  for i,level in enumerate(levels):
    print i, len(level), (2 ** (2*t)) * (2 ** (K - t * i))

  print '\n%s\n' % len(make_naivelevel(n,0))

print '>>>>  N=%s, nrules / n = %s' % (n, nrules / float(n))

