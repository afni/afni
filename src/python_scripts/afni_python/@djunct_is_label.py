#!/usr/bin/env python

# auth = PA Taylor (NIMH, NIH)
# ver  = 1.0
# date = April 30, 2019
# 
#
# --------------------------------------------------------------------

import sys

hlist = [ "", "-h", "-hview", "-help" ]
hstr  = ''' 
Subsidiary of @chauffeur_afni, not really a program deserving a
help message.

Just a little, *tiny* wafer...
'''

# ------------- read in input(s) -------------------------

Narg = len(sys.argv)

if Narg == 2 :
    label_in = sys.argv[1]
elif Narg > 2 :
    print("** ERROR: too many inputs, only use one!")
    sys.exit(1)
else:
    label_in = ''

if hlist.__contains__(label_in) :
    print(hstr)
    sys.exit(0)

# --------------- the hard work --------------------------

try:
    int(label_in)
    print("0")
except:
    print("1")

sys.exit(0)
