#!/usr/bin/env python

# ---------------------------------------------------------------------------
# test python modules

import sys
import module_test_lib as MT

genlibs  = ['os', 'sys', 'string', 'glob', 'copy', 'gc']
mathlibs = ['math', 'random', 'numpy', 'scipy', 'R']
guilibs  = ['wx', 'matplotlib']

alllibs  = []
alllibs.extend(genlibs)
alllibs.extend(mathlibs)
alllibs.extend(guilibs)


if __name__ == '__main__':
   print ""
   print "\nnumber of python import failures = %d\n" % \
         MT.num_import_failures(verb=1)

