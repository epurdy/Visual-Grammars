#!/usr/bin/python
from experiments.common import *

dir = 'output/2.parsing/shorter_curves'

cmd = './subsample.native DATA/romer/clean/diff0080.curve 50 %s/diff0080.50.curve' % dir
doit(cmd)

cmd = ('./shorter_curves.native DATA/romer/ann/curve0000.curve %s/diff0080.50.curve x %s/parse_0.svg %s/sdf.svg' %
       (dir,dir,dir))
doit(cmd)
cmd = ('./shorter_curves.native DATA/romer/ann/curve0080.curve %s/diff0080.50.curve x %s/parse_8.svg %s/sdf.svg' % 
       (dir,dir,dir))
doit(cmd)
cmd = ('./shorter_curves.native DATA/romer/ann/curve0000.curve %s/diff0080.50.curve DATA/romer/misc/romer_shorter.sdf %s/parse_0_2.svg %s/sdf.svg' % 
       (dir,dir,dir))
doit(cmd)
cmd = ('./shorter_curves.native DATA/romer/ann/curve0080.curve %s/diff0080.50.curve DATA/romer/misc/romer_shorter.sdf %s/parse_8_2.svg %s/sdf.svg' % 
       (dir,dir,dir))
doit(cmd)

svg_to_png(dir, 'parse_0.svg', 'parse_0.png')
svg_to_png(dir, 'parse_8.svg', 'parse_8.png')
svg_to_png(dir, 'parse_0_2.svg', 'parse_0_2.png')
svg_to_png(dir, 'parse_8_2.svg', 'parse_8_2.png')

# svg_to_png(dir, 'sdf_0.svg', 'sdf_0.png')
# svg_to_png(dir, 'sdf_8.svg', 'sdf_8.png')
