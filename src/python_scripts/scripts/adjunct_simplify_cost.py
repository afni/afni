#!/usr/bin/env python

# auth = PA Taylor (NIMH, NIH)
# ver  = 1.0     ; date = April 8, 2020
# 
# ver  = 1.1     ; date = Aug 7, 2021
# [PT] simplify and generalize The Hard Work
#
# --------------------------------------------------------------------

import sys

hlist = [ "", "-h", "-hview", "-help" ]
hstr  = ''' 
Subsidiary of @SSwarper, not really a program deserving a
help message.

Just takes one argument: a cost function name.

This program will remove the '+' and anything following it from a cost
function's name.  For example:
    lpa+ZZ  ->  lpa
    lpc+    ->  lpc
    lpa+hel:1.0+crA:0.4+nmi:0.2+ov:0.4+ZZ -> lpa

'''

# ------------- read in input(s) -------------------------

Narg = len(sys.argv)

if Narg == 2 :
    cost_in = sys.argv[1]
elif Narg > 2 :
    print("** ERROR: too many inputs, only use one!")
    sys.exit(1)
else:
    cost_in = ''

if hlist.__contains__(cost_in) :
    print(hstr)
    sys.exit(0)

# --------------- the hard work --------------------------

print(cost_in.split('+')[0])

''' OLD
if cost_in[-2:] == 'ZZ' or cost_in[-2:] == 'zz' :
    cost_in = cost_in[:-2]

if cost_in[-1] == '+' :
    print(cost_in[:-1])
else:
    print(cost_in)
'''

sys.exit(0)
