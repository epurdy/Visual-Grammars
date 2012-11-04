#!/usr/bin/python

import os, sys

recomputing = False
if 'recompute' in sys.argv:
  recomputing = True

def doit(cmd, die=True):
  print cmd
  code = os.system(cmd)
  if die:
    assert(code == 0)

def hash_of_dir(d):
#  doit('find %s  -not -type d | xargs md5sum | awk \'{ print $1 }\'' % d)
  md5sum = [
    line.rstrip() for line in 
    os.popen('find %s  -not -type d | xargs md5sum | awk \'{ print $1 }\' | md5sum' % d) ][0]

  md5sum = md5sum.split()[0]
  return md5sum

output = open('/home/epurdy/html/results.html', 'w')
output.write('''<html><head>
<style>
td{
border: 1px solid black;
padding: 10px;
}
td.r0{ background-color:#ffffff; }
td.r1{ background-color:#ff9999; }
td.r2{ background-color:#99ff99; }
td.r3{ background-color:#00ff00; font-weight:bold;}
</style>
</head><body><table>''')

''
experiment_subdirs = [ line.rstrip() for line in os.popen('find experiments -maxdepth 1 -mindepth 1 -type d | grep \'\.\' ') ]
experiment_subdirs = [ e.split('/')[-1] for e in experiment_subdirs ]
output_subdirs = [ line.rstrip() for line in os.popen('ls output | grep \'\.\'') ]
subdirs = experiment_subdirs
print experiment_subdirs
print subdirs

for s in subdirs:
  output.write('<tr>\n')

#  exes = [ line.rstrip() for line in os.popen('ls output/%s' % s) ]
  exes = [ line.rstrip() for line in os.popen('ls experiments/%s' % s) ]
  print 'SUBDIR', s, exes
  print
  for e in exes:
    print '%s/%s' % (s,e)
    output_dir = 'output/%s/%s' % (s,e)
    results_dir = 'results/%s/%s' % (s,e)
    results_output_dir = '%s/output' % results_dir

    # print 'output_dir', output_dir
    # print 'results_dir', results_dir
    # print 'results_output_dir', results_output_dir

    if recomputing:

      doit('mkdir -p %s' % results_output_dir)
      md5sum_new = hash_of_dir(output_dir)
      md5sum_old = hash_of_dir(results_output_dir)
      if md5sum_old != md5sum_new:
        print 'OLD MD5', md5sum_new
        print 'NEW MD5', md5sum_old
        print '>>>>>> CHANGED <<<<<<'
        print
        keepnew = raw_input('Store new data? (y/n) ')
        print '~%s~' % keepnew
        if keepnew == 'y':
          doit('mv %s %s.%s' % (results_output_dir, 
                                results_output_dir, md5sum_old))
          doit('cp -r %s %s' % (output_dir, results_output_dir))

          print '''ratings:
  0: missing
  1: too crappy
  2: good enough
  3: really good'''
          rating = raw_input('Rating (0-3): ')
          print int(rating)
          doit('mv %s/%s.rating %s/%s.%s.rating' % (
              results_dir, e, results_dir, e, md5sum_old), die=False)
          f = open('%s/%s.rating' % (results_dir, e), 'w')
          f.write(str(int(rating)))
          f.close()

          notes = raw_input('Notes: ')
          print 'NOTES', notes
          doit('mv %s/%s.notes %s/%s.%s.notes' % (
              results_dir, e, results_dir, e, md5sum_old), die=False)
          f = open('%s/%s.notes' % (results_dir, e), 'w')
          f.write(notes)
          f.close()

          doit('mv %s/%s.timestamp %s/%s.%s.timestamp' % (
              results_dir, e, results_dir, e, md5sum_old), die=False)
          doit('date > %s/%s.timestamp' % (results_dir, e))
    # end if recomputing:        

    rating = 0
    try:
      rating = [ line.rstrip() for line in open('%s/%s.rating' % (results_dir, e)) ]
      rating = int(rating[0])
    except IOError:
      pass


    output.write('''<td class='r%d'>\n''' % rating)
    output.write('<p><a href="results/%s/%s/output">%s/%s</a></p>\n' % (s,e,s,e))
    print 'RATING', rating
    output.write('<p>Rating:%s</p>\n' % (rating))

    try:
      timestamp = [ line.rstrip() for line in 
                    open('%s/%s.timestamp' % (results_dir, e)) ]
      timestamp = timestamp[0]
      print 'TIMESTAMP', timestamp
      output.write('<p>Timestamp:%s</p>\n' % (timestamp))
    except IOError:
      print 'NO TIMESTAMP'
      output.write('<p>No Timestamp</p>\n')

    try:
      notes = '\n'.join( line for line in open('%s/%s.notes' % (results_dir, e)) )
      print 'NOTES', notes
      output.write('<p>Notes:%s</p>\n' % (notes))
    except IOError:
      print 'NO NOTES'
      output.write('<p>No Notes</p>\n')

    output.write('</td>\n')


    print
  output.write('</tr>')

output.write('''</table></body></html>''')
output.close()
