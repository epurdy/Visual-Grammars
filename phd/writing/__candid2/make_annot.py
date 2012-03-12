#!/usr/bin/python
import os, sys

header='''
\\documentclass{article}
\\usepackage{leonine,amsmath,amssymb,amsthm,graphicx}%%xy, setspace, amscd (commutative diagram)
\\title{Annotated Bibliography}
\\author{Eric Purdy \\footnote{Department of Computer Science, University of Chicago. Email: epurdy@uchicago.edu}}

%%\\doublespace

\\begin{document}
\\maketitle

'''

footer = '''

\\bibliographystyle{IEEEtranS.bst}
\\bibliography{candid}

\\end{document}
'''

types = ['article', 'phdthesis', 'book', 'inproceedings', 'misc', 
         'proceedings', 'techreport', 'unpublished', 'electronic',
         'incollection', 'inbook']
keys = ['title', 'author', 'year']
bibfile = sys.argv[1]
url = 'http://www.citeulike.org/bibtex/user/forefinger?key_type=1'

# if 'up' in sys.argv:
#   cmd = 'wget %s -O %s' % (url, bibfile)
#   print cmd
#   os.system(cmd)

f = open(bibfile)

class Work: pass

inref = False
names = {}

for line in f:
  if inref:
    if line[0] == '}': 
      inref = False
    else:
      chunks = line.split('{', 1)
      try:
        key, val = chunks[:2]
      except:
        print 'not k,v:', chunks
        continue
      key = sorted(key.split(' '), lambda a,b: len(a) - len(b))[-1]
      if key not in keys:
        continue

      val = sorted(val.rsplit('}',1), lambda a,b: len(a) - len(b))[-1]
#      print curname, key, val
      names[curname].__dict__[key] = val
      continue

  if line[0] == '@':
    inref = True
    thetype, name = line[1:].split('{')
    curname = name[:-2]
    try:
      assert(thetype in types)
    except:
      print thetype, 'not in', types
      assert(thetype in types)
    names[curname] = Work()
    names[curname].thetype = thetype
    
assert(not inref)
f.close()

# Process Annotation

f = open('candid.annot')
tex = open('candid.tex', 'w')
section = ['section', 'subsection', 'subsubsection']
section = [ '\\' + s + '{%s}' for s in section ]
itemizing = False
used = []
print >> tex, header


for line in f:
  if line[0] == '!':
    if itemizing:
      print >> tex, '\\eitem\n' 
      itemizing = False

    lvl = 0
    while line[lvl] == '!':
      lvl += 1
    print >> tex, section[lvl-1] % str.rstrip(line[lvl:]),

  elif line[0] == '^':
    name = str.rstrip(line[1:])
    del names[name]

  elif line[0] == '@':
    name = str.rstrip(line[1:])
    used.append(name)
    try:
      assert(name in names)
    except:
      print name, 'not in', sorted(names.keys())
      assert(name in names)

    if not itemizing:
      print >> tex, '\n\\bitem'
      itemizing = True

    print >> tex, '\\item %s. %s. \\cite{%s}' % (
      names[name].author, names[name].title, name),

  else:
    print >> tex, line,
    
if itemizing:
  print >> tex, '\\eitem'

used = list(set(used))

def authorcmp(n1, n2):
  a1 = str.lower(names[n1].author)
  a2 = str.lower(names[n2].author)
  if a1 < a2: return -1
  if a1 > a2: return 1
  return 0

print >> tex, '\\section{Uncategorized}'
print >> tex, '\\bitem'
for name in sorted(names.keys(), authorcmp):
  if name not in used:
    print >> tex, '\\item %s. %s. \\cite{%s}' % (
      names[name].author, names[name].title, name)
print >> tex, '\\eitem'

print >> tex, footer
