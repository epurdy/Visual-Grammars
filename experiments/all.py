#!/usr/bin/python

import os

print 'Starting Experiment 2.parsing/one_to_one'
os.system('./experiments/2.parsing/one_to_one/one_to_one.py')


os.system('cd experiments/ && latex experiments.tex && dvips experiments.dvi')
