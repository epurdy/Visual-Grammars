import os

idxs = range(1,21)

def test_class(test, cls):
  for i in idxs:
#    os.system('./parse_mpeg.native xstage/mpeg7/shock.%s-%d/shock.sdf xstage/mpeg7/shock.%s-%d/boundary.curve xstage/mpeg7/shock.%s/boundary.curve %s_%s.%d.qual' % (cls,i,cls,i,test,cls,test,i))
    os.system('./show_parse.native xstage/mpeg7/shock.%s/boundary.curve xstage/mpeg7/shock.%s-%d/boundary.curve xstage/mpeg7/shock.%s-%d/shock.sdf %s_%s.%d.svg %s_%s.%d.qual' % (test,cls,i,cls,i,cls,test,i,cls,test,i))
  files = [ open('%s_%s.%d.qual' % (cls,test,i)) for i in idxs ]
  scores = [ float(''.join(line for line in file)) for file in files ]
  scores = [ (score, cls) for score in scores ]
  return scores

# scores = (
#   test_class('device6-1', 'octopus') +
#   test_class('device6-1', 'device6')
# )

scores = (
  test_class('apple-1', 'apple') +
  test_class('apple-1', 'spring')
)

scores.sort()
for (i,(score,cls)) in enumerate(scores):
  print '#%2d:  %s\t%f' % (i, cls, score)

#  os.system('./parse.native xstage/mpeg7/shock.device6-%d/shock.gram xstage/mpeg7/shock.device6-1/boundary.curve dev6_dev6.%d.qual' % (i,i))
