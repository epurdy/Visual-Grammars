#!/usr/bin/python
import os, sys, random, thrasher, time
os.nice(10)

k = int(sys.argv[1])
runid = sys.argv[2]

# def payload(manager, runid, k, n):
#   os.system('hostname | xargs printf "I\'m #%d on %%s!\n"' % k)
  
#   time.sleep(3)
#   if 0: #thrasher.testfile('/home/epurdy/foo'):
#     manager.nuke()

# def payload(manager, runid, k, n):
#   os.system('hostname | xargs printf "I\'m #%d on %%s!\n"' % k)
#   time.sleep(3)

def payload(manager, runid, k, n):
  os.system('hostname | xargs printf "I\'m #%d on %%s!\n"' % k)
  os.system('cd /home/epurdy/viz && ./experiments/6.structure/mpeg7/mpeg7.py %d %d %s %s' % (
      k,n, manager.deadname(), manager.raceflag()))
  os.system('hostname | xargs printf "Finished #%d on %%s!\n"' % k)


# def payload(manager, runid, k, n):
#   if os.system('python /home/epurdy/viz/getdata.py %s' % runid) != 0:
#     for i in xrange(100):
#       print 'about to nuke, nonzero return code'
#     manager.nuke(level=2)

manager = thrasher.Manager(
  machinefile = '/home/epurdy/viz/newmachines',
  payload = payload,
  cmdline = '~/viz/thrasher4.py %d %s',
  k = k,
  runid = runid,
  files = ['~/viz/thrasher.py','~/viz/thrasher4.py', 
           '~/viz/experiments/6.structure/mpeg7/mpeg7.py',
           '~/viz/parse_mpeg_reverse.ml',
           '~/viz/parse_mpeg_reverse.native'
           ],
  pgrep_pattern = 'parse_mpeg',
  delay = 30
  )





