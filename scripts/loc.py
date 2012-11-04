#!/usr/bin/python

import os

os.system('ls *.ml *.mli bin/*.ml util/*.ml util/*.mli detection/*.ml detection/*.mli > tmp/codefiles')

os.system('wc *.ml *.mli bin/*.ml util/*.ml util/*.mli detection/*.ml detection/*.mli')

