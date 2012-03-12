import re, sys

f = open(sys.argv[1])
out = open(sys.argv[2], 'w')

note = 'note'
tags = ['part', 'section', 'subsection', 'subsubsection', 'note']
regex = re.compile(r'\\(%s){([^}]*)' % '|'.join(tags))


out.write(r'''
\documentclass{article}
\usepackage{leonine,amsmath,amssymb,amsthm,graphicx, setspace}%%xy, setspace, amscd (commutative diagram)
\title{Issues}
\begin{document}
\maketitle
\tableofcontents
''')

def outside(line):
  foo = re.split(regex, line, maxsplit=1)
  if len(foo) == 1:
    return

  assert(len(foo) == 4)

  if foo[1] != note:
    out.write(r'\%s{' % (foo[1]))
  inside(''.join(foo[2:]), foo[1])

def inside(line, tag):
  while True:
    foo2 = str.split(line, '}', 1)
    if len(foo2) == 1:
      out.write(line)
      line = f.readline()
      continue

    out.write('%s' % foo2[0])
    if tag == note:
      out.write('\n\n')
    else:
      out.write('}\n')
    line = foo2[1]
    break
  outside(line)

while True:
  line = f.readline()
  if not line: 
    break
  outside(line)

out.write(r'\end{document}')
