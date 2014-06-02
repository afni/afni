#!/usr/bin/env python

import os, sys
import lib_afni1D as LAD

# run as PROG niters sizefile

help_str = """
quick.alpha.vals.py   - make an alpha table from slow_surf_clustsim.py results

   Run this on each z.max.area file output by slow_surf_clustsim.py.  In some
   cases the z.max.area might not have as many lines as iterations, for which
   the -niter option can be applied.

   usage: quick.alpha.vals.py [-niter N] max_file

       -niter: number of iterations that should be in the z file

            ** Note: -niter should match that from slow_surf_clustsim.py.

   This pathetic program will surely be enhanced.  Someday.

   R Reynolds
"""

usage_str = '\n** usage %s [-niter N] max_file\n'%os.path.basename(sys.argv[0])

narg = len(sys.argv)
if narg < 2 or '-help' in sys.argv:
   print help_str
   sys.exit(0)

an = 1
niter = 0
if sys.argv[an] == '-niter':
   if an+2 >= narg:
      print usage_str
      sys.exit(1)
   niter = int(sys.argv[an+1])
   print '-- using niter = %d' % niter
   an += 2
   
infile = sys.argv[an]

mdata = LAD.Afni1D(infile, verb=0)
if not mdata.ready:
   print '** failed to read maxdata file %s' % infile
   sys.exit(1)
try: mdata = [int(round(m)) for m in mdata.mat[0]]
except:
   print '** failed to process integers from maxdata file %s' % infile
   sys.exit(1)

mdata.sort()
cmax = mdata[-1]
mlen = len(mdata)

if niter > 0 and niter > mlen:
   print '** have input niter (%d) > mlen(%d)' % (niter, mlen)

print '-- have %d values with max %d' % (mlen, cmax)

mcount = [0 for v in range(cmax+1)]
for val in mdata: mcount[val] += 1

alpha  = [0.0 for v in range(cmax+1)]
tot = 0
if niter > 0: fac = 1.0/niter
else:         fac = 1.0/mlen
for ind in range(cmax,0,-1):
   tot += mcount[ind]
   alpha[ind] = 1.0*tot*fac     # fac is alpha factor, e.g. 1/niter

print "   area (mm^2)   alpha (corrected p)\n" \
      "   -----------   -------------------"
for ind in range(cmax+1):
   print "      %4d            %.5f" % (ind, alpha[ind])
