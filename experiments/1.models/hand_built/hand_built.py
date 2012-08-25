#!/usr/bin/python


from experiments.common import *

dir = 'output/1.grammars/hand_built'

# build the grammars and save them
doit('./hand_built.native %s' % dir)

doit('./show_sdf.native -sdf %s -curve %s -fname %s/%s' % 
     ("DATA/romer/misc/romer1.sdf", "DATA/romer/ann/curve0000.curve", dir, 
      "romer_sdf.svg"))
doit('./show_curve.native %s/romer_flipped.curve %s/romer_curve.svg' %
     (dir, dir))
doit('./show_curve.native %s/romer_skirt.curve %s/romer_skirt.svg' %
     (dir, dir))

doit('./show_curve.native %s/hand.curve %s/hand_curve.svg' %
     (dir, dir))

svg_to_png(dir, 'romer_curve.svg', 'romer_curve.png')
svg_to_png(dir, 'romer_skirt.svg', 'romer_skirt.png')
svg_to_png(dir, 'romer_sdf.svg', 'romer_sdf.png')
svg_to_png(dir, 'hand_curve.svg', 'hand_curve.png')

doit('mkdir -p %s/romer' % dir)
doit('mkdir -p %s/romerchoice' % dir)
doit('mkdir -p %s/hand' % dir)
doit("./show_grammar.native -gramfile %s/romer.gram -dir %s/romer -showrules -rules" % (dir,dir))
doit("./show_grammar.native -gramfile %s/romerchoice.gram -dir %s/romerchoice -showrules -rules" % (dir,dir))
doit("./show_grammar.native -gramfile %s/hand.gram -dir %s/hand -showrules -rules" % (dir,dir))

