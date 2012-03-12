#!/usr/bin/python
import os, sys

bibfile = sys.argv[1]
url = 'http://www.citeulike.org/bibtex/user/forefinger?key_type=4'

cmd = 'wget %s -O %s' % (url, bibfile)
print cmd
os.system(cmd)

