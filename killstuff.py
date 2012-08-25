#!/usr/bin/python
import os

f = open('newmachines')

skiphosts =['be'] # ['alebrije']

for line in f:
  host = line.split()[0].strip()

#   if host < 'clover':
#     continue

  if host[0] == '#':
    continue

  if host in skiphosts:
    continue

#   cmd = "ssh %s 'pkill -9 -f python'" % host
#   print cmd
#   os.system(cmd)

  cmd = "ssh %s 'pkill -9 -f thrasher'" % host
  print cmd
  os.system(cmd)

  cmd = "ssh %s 'pkill -9 -f mpeg'" % host
  print cmd
  os.system(cmd)

  cmd = "ssh %s 'rm -rf /tmp/master-epurdy*'" % host
  print cmd
  os.system(cmd)

  cmd = "ssh %s 'ps aux | grep epurdy'" % host
  print cmd
  os.system(cmd)

