# Borrowing heavily from daemonize.py, which is Copyright 2007 Jerry Seutter
# yello (*a*t*) thegeeks.net
import fcntl, os, sys, time, random, string


#throttles should be ints, for an annoying reason

create_throttle = 5 # minimum number of seconds that must have elapsed
                    # since the instance we're about to spawn was last
                    # seen alive
restart_throttle = 10
ssh_throttle = 10 # minimum number of seconds between ssh commands
max_cores_per_host = 3

user = 'epurdy'
tmpdir = '/tmp/thrasher-epurdy'
homedir = '/home/epurdy'
commdir = '/stage/epurdy/comm'
storedir = '/stage/epurdy'


class Host:
  def __init__(self, name, num):
    self.name = name
    self.num = num

  def __repr__(self):
    return '<%s:%s:%d>' % (self.name, self.ip, self.num)

def testfile(fname):
  fname = fname.replace('~', homedir)
  return os.path.isfile(fname)

def touch(fname):
  os.system('touch %s' % fname)

def mkdir(dirname):
  try:
    os.mkdir(dirname)
  except OSError:
    pass

def get_lock_or_die(name, dir=tmpdir):
  fname = '%s/%s.lock' % (dir, name)
#  touch(fname)
  lockfile = open(fname, 'w')
  # Try to get an exclusive lock on the file.  This will fail
  # if another process has the file locked.
  try:
    fcntl.lockf(lockfile, fcntl.LOCK_EX|fcntl.LOCK_NB)
  except IOError:
    sys.exit(0)

  # Record the process id to the lockfile.  This is standard
  # practice for daemons.
  lockfile.write('%s' %(os.getpid()))
  lockfile.flush()

def get_lock_or_give_up(name, dir=tmpdir):
  fname = '%s/%s.lock' % (dir, name)
  lockfile = open(fname, 'w')
  # Try to get an exclusive lock on the file.  This will fail
  # if another process has the file locked.
  try:
    fcntl.lockf(lockfile, fcntl.LOCK_EX|fcntl.LOCK_NB)
    return True
  except IOError:
    return False

def get_age(name):
  agefile = '%s/%s.age' % (tmpdir, name)
  if testfile(agefile):
    mtime = os.path.getmtime(agefile)
    return time.time() - mtime
  else:
    touch(agefile)
    return 0

def fork_daemon(fun_to_start, uniqid, loglevel):
  # Fork, creating a new process for the child.
  process_id = os.fork()
  if process_id < 0:
    # Fork error.  Exit badly.
    sys.exit(1)
  elif process_id != 0:
    # This is the parent process.  Return.
    return
  # This is the child process.  Continue.

  # Stop listening for signals that the parent process receives.
  # This is done by getting a new process id.
  # setpgrp() is an alternative to setsid().
  # setsid puts the process in a new parent group and detaches its
  # controlling terminal.
  process_id = os.setsid()
  if process_id == -1:
    # Uh oh, there was a problem.
    sys.exit(1)

  # Close file descriptors
  devnull = '/dev/null'
  if hasattr(os, "devnull"):
    # Python has set os.devnull on this system, use it instead 
    # as it might be different than /dev/null.
    devnull = os.devnull
  null_descriptor = open(devnull, 'rw')
  def closeit(descriptor):
    descriptor.close()
    descriptor = null_descriptor

  closeit(sys.stdin)
#  closeit(sys.stdout)
#  closeit(sys.stderr)

  # Set umask to default to safe file permissions when running
  # as a root daemon. 027 is an octal number.
  os.umask(027)

  # Change to a known directory.  If this isn't done, starting
  # a daemon in a subdirectory that needs to be deleted results
  # in "directory busy" errors.
  # On some systems, running with chdir("/") is not allowed,
  # so this should be settable by the user of this library.
  os.chdir(homedir)

  # Create a lockfile so that only one instance of this daemon
  # is running at any time.  Again, this should be user settable.
  get_lock_or_die('thrash.daemon.%s' % uniqid)

  fun_to_start()


def cleanup(exclude):
  files = [ f for f in os.popen('find %s %s -type f -user %s -not -name *%s* 2>/dev/null' % (tmpdir, commdir, user, exclude)) ]
  for f in files:
    f = f.rstrip()
    os.system('rm %s' % f)

class Manager:
  def __init__(self, machinefile, payload, cmdline, k, runid, 
               files,
               #prime=197, 
               prime=383, 
               delay=None, loglevel=4, cleanup=False):

    self.k = k
    self.runid = runid
    self.files = files
    self.id = '%s.%d' % (runid, k)

    mkdir(tmpdir)

    self.check_kill()
    self.check_checksums()

    get_lock_or_die('thrash.manager.%s' % self.id, dir=commdir)

    self.check_race()

    self.iamalive()
    self.monitor()

    self.delay = delay
    self.cmdline = cmdline
    self.loglevel = loglevel
    self.p = prime
    self.hosts = []
    self.uniqhosts = []
    for line in open(machinefile):
      line = line.rstrip()
      line = line.split()
      hostname, hostcores = line[0], int(line[-1])
      hostcores = min(hostcores, max_cores_per_host)
      for i in xrange(hostcores):
        host = Host(hostname, i)
        self.hosts.append(host)
        if i==0:
          self.uniqhosts.append(host)
    self.ncores = len(self.hosts)

    self.iamalive()
    if self.check_delay() and self.check_load() and self.check_mem():
      payload(self, self.runid, self.k, self.ncores)
    self.iamalive()

    if cleanup:
      cleanup(runid)

    self.infect()


    # restart self
    cmdline = self.cmdline % (self.k, self.runid)    
    for _ in xrange(restart_throttle):
      self.iamalive()
      time.sleep(1)    
    fork_daemon(lambda: os.system(cmdline), self.id, loglevel=self.loglevel)

  def iamalive(self):
    touch(self.flagname())
    os.system('rm -f %s' % self.deadname())
    os.system('rm -f %s' % self.raceflag())

  def deadname(self, k=None):
    if k is None:
      k = self.k
    return '%s/dead.%s.%d' % (commdir, self.runid, k)

  def flagname(self, k=None):
    if k is None:
      k = self.k
    return '%s/thrash.here.%s.%d' % (commdir, self.runid, k)

  def raceflag(self):
    return '%s/raceflag.%s' % (tmpdir, self.id)

  def monitor(self):
    os.system('uptime > %s/load.`hostname`.%s' % (commdir, self.runid))
    os.system('free > %s/free.`hostname`.%s' % (commdir, self.runid))
    os.system('ps `pgrep -u %s` > %s/ps.`hostname`.%s' % (user, commdir, self.runid))

  def check_mem(self):
    freeoutput = [ x for x in os.popen('free') ]
    _, total, used, free, shared, buffers, cache = freeoutput[1].split()
    total, free, buffers, cache = int(total), int(free), int(buffers), int(cache)
    ratio = float(free+buffers+cache) / float(total)
    memok = (ratio > 0.25)
    print 'mem', self.id, ratio, free, buffers, cache, total, memok
    return memok

  def check_load(self):
    avg1, avg5, avg15 = os.getloadavg()
    loadok = ((avg1 <= 2.0) and
              (avg5 <= 2.0) and 
              (avg15 <= 2.0))
    print 'load', self.id, avg1, avg5, avg15, loadok
    return loadok

  def check_checksums(self):
    files = ' '.join(self.files)
    checksumfile = '%s/checksum.%s' % (commdir, self.runid)
    checksumcmd = 'md5sum %s | md5sum' % files
    if not testfile(checksumfile):
      os.system('%s > %s' % (checksumcmd, checksumfile))
    if os.system('%s | diff %s - >/dev/null 2>/dev/null' % (checksumcmd, checksumfile)) != 0:
      print 'OLD CODE', self.id
      sys.exit(0)

  def check_delay(self):
    if self.delay is None:
      return True
    else:
      age = get_age('thrash.age.%s' % self.id)
      if age > self.delay:
        return True
      else:
#        print 'WAITING', self.k
        return False

  def check_kill(self):
    if testfile('%s/kill.%s' % (commdir,self.runid)):
      print 'KILL', self.id
      sys.exit(0)

    if testfile('%s/kill' % (commdir)):
      print 'GLOBKILL', self.id
      sys.exit(0)

  def check_race(self):
    raceflag = self.raceflag()
    touch(raceflag)
    random.seed(os.urandom(2))
    sleeptime = 3 * random.random()
#    os.system('echo %f > %s/sleep.%s.%d' % (sleeptime, commdir, self.id, os.getpid()))
    
    time.sleep(sleeptime) # break ties, hopefully
    if not testfile(raceflag):
      sys.exit(0)
    os.system('rm %s' % raceflag)

  def nuke(self, level=0):
    if level == 0: 
      touch('%s/kill.%s' % (commdir,self.runid))
    elif level == 1:
      touch('%s/kill' % (commdir))
    elif level == 2:
      touch('%s/kill' % (commdir))
      os.system('pkill python')
    elif level >= 3:
      touch('%s/kill' % (commdir))
      os.system('kill -9 -1') # kill everything possible 
    else:
      print 'BAD NUKE LEVEL', self.id

    sys.exit(0)

  def nbrs(self):
    assert(self.p >= self.ncores and self.k < self.ncores)

    kplus = (self.k + 1) % self.p

    k3 = (self.k * self.k * self.k) % self.p # k^3 is good because gcd(3, 383-1)=1
    assert( (self.p - 1) % 3 != 0 );

    kinv = 0
    for i in xrange(self.p):
      if (self.k * i) % self.p == 1:
        kinv = i
        break

    return (kinv, k3, kplus)

  def infect(self):
    def spread(i):
      cmdline = self.cmdline % (i, self.runid)
      nbrfile = '%s/thrash.nbr.%s.%d' % (tmpdir, self.id, i)
      cmd = ("ssh -f %s 'mkdir -p %s && touch %s && %s'" % (
          self.hosts[i].name, tmpdir, nbrfile, cmdline))
      os.system(cmd)

    for i in self.nbrs():
      if i != self.k:
        i = i % self.ncores
        self.check_kill()
        self.check_checksums()
        touch(self.deadname(i))

        for _ in xrange(ssh_throttle):
          self.iamalive()
          time.sleep(1)

        if testfile(self.deadname(i)):
          fork_daemon(lambda: spread(i), self.id, loglevel=self.loglevel)


class NiceManager(Manager):
  def nbrs(self):
    kplus = self.k + 1
    if kplus < self.p:
      return (kplus,)
    else:
      return ()

