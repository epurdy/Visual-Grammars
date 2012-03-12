#!/usr/bin/python

import re, sys

base = sys.argv[1]
f=open(base + '.svg')
lines = f.readlines()
txt = ''.join(lines)

thepaths = []

path_pattern = re.compile('\<path\s*[^>]*>')
matches = path_pattern.findall(txt)

points_pattern = re.compile('\Wd="([^"]*)"')
for x in matches:
  pathdata = points_pattern.search(x).group(1)
#  print 'PATHDATA', pathdata
  path = re.split('([MmLl]\s|[\-\d\.]*[,\s]*[\-\d\.]*\s)', pathdata)
  cmd = 'L'
  points = []
  cur = (0,0)

  for thing in path:
#    print 'THING', thing
    thing = thing.strip()
    if not thing:
      continue
#    print thing, cmd, cur, points
    if thing in ['m','l']:
      cmd = 'l'
      continue
    if thing in ['M','L']:
      cmd = 'L'
      continue
    if thing in ['Z','z']:
      continue

    x,y = thing.split(',')
    x,y = float(x), float(y)
    if cmd == 'l':
      x,y = cur[0]+x, cur[1]+y
    cur = x,y
    points.append( (x,y) )

  thepaths.append(points)

for i,path in enumerate(thepaths):
  f = open('%s.%02d.curve' % (base,i), 'w')
  f.write('\n'.join(('%d %d' % (int(x),int(y))) for x,y in path))
  f.close()
