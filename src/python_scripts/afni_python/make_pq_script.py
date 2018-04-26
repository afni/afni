#!/usr/bin/env python

import sys, os

import afni_base as B
import afni_util as U
import lib_afni1D as LD
import lib_textdata as LT

# init main vars, just to list them
dset = None
mask = None
sub  = -1
dof = -1
outfile = ''

# first process inputs: dataset, mask, index, script

prog = os.path.basename(sys.argv[0])
helpstr =                                                               \
        '\n'                                                            \
        '%s   - make a script to compute p-value and q-value curves\n'  \
        '\n'                                                            \
        '  This is just a quick program for now...\n\n'                 \
        '     usage: %s dataset brick_index mask out.script\n\n'        \
        '  dataset:      input dataset (no sub-brick selectors)\n'      \
        '  brick_index:  volume sub-brick for specific t-stat\n'        \
        '  mask:         mask volume dataset\n'                         \
        '  out.script:   name for output script to write\n'             \
        '\n'                                                            \
        'R Reynolds  July, 2010\n' % (prog, prog)

narg = 0
if len(sys.argv) != 5:
   if len(sys.argv) == 1 or '-help' in sys.argv:
      print helpstr
      sys.exit(0)
   else:
      print '\n   usage: %s dataset brick_index mask out.script\n' % prog
      sys.exit(1)
narg += 1

dset = B.afni_name(sys.argv[narg])
if not dset.exist():
   print '** dataset %s not found' % dset.rpv()
   sys.exit(1)
narg += 1

try: sub = int(sys.argv[narg])
except:
   print '** invalid sub-brick index %s' % sys.argv[narg]
   sys.exit(1)
narg += 1

mask = B.afni_name(sys.argv[narg])
if not mask.exist():
   print '** dataset %s not found' % mask.rpv()
   sys.exit(1)
narg += 1

outfile = sys.argv[narg]


dof = U.get_3d_statpar(dset.rpv(), sub, statcode='fitt')
if dof < 0:
   print '** failed to get statpar from %s[%d]' % (dset.rpv(), sub)
   if U.get_3d_statpar(dset.rpv(), sub, verb=0) >= 0:
      print '   (statcode is not fitt?)'
   sys.exit(1)

print '-- t-stat %s[%d] has %d dof' % (dset.rpv(), sub, dof)
print '-- using mask dataset %s' % mask.rpv()
print '-- writing output script to %s' % outfile


ss = '#!/bin/tcsh -ef\n\n'                              \
     '# -----------------------------------------------------------------\n'\
     '# This script creates a list of p and q-value pairs, where the \n'    \
     '# minimum is not applied (to see whether the q-values bottom out).\n' \
     '#\n'                                              \
     '# ==> the p/q list will be output to the file %s\n\n\n' \
     '# main user vars\n'                               \
     'set dset = %s\n'                                  \
     'set sub  = %d\n'                                  \
     'set mask = %s\n'                                  \
     'set dof  = %d\n' % ('both.pq.1D', dset.rpv(), sub, mask.rpv(), dof)


static_script = """
# set main output file and number of q-values to get
set output = both.pq.1D
set nv = 200

# set prefix for temporary output files
set pre = rm.me


# note bottom and and step for percentiles
set step = `ccalc 100.0/$nv`
set bot = `ccalc $step/2.0`


# -----------------------------------------------------------------
# do the work: abs(t) => (positive) percentile t-stats => p-values

3dcalc -a $dset"[$sub]" -expr 'abs(a)' -overwrite -prefix $pre.abs
3dBrickStat -mask $mask -percentile $bot $step 100 -non-zero \\
            $pre.abs+tlrc > $pre.abs.1D

# have percentile p-values, remove unwanted percentile rankings
1dcat $pre.abs.1D'[1..$(2)]' > $pre.at.1D

# transpose and convert t to p
1deval -a $pre.at.1D\\' -expr "fitt_t2p(a,$dof)" > $pre.p.1D

# convert p to q    (** note: the minimum is not applied, we do not want it)
1deval -a $pre.p.1D -expr "a*$nv/($nv-t)" > $pre.q.1D

# put both together in output file
1dcat $pre.p.1D $pre.q.1D > $output


# ----------------------------------------
# nuke temporary files (comment out to keep)
rm -f $pre.*

"""

ss += static_script

# and write output
U.write_text_to_file(outfile, ss)
try:
    os.chmod(outfile, 0755)
except OSError, e:
    print e
