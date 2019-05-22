#!/usr/bin/env python

# auth = PA Taylor (NIMH, NIH)
# ver  = 1.0
# date = April 30, 2019
# 
#
# --------------------------------------------------------------------

import sys
import json

hlist = [ "", "-h", "-hview", "-help" ]
hstr  = ''' 
Subsidiary of apqc*py, not really a program deserving a
help message.

Just a little, *tiny* wafer...
'''

# ------------- read in input(s) -------------------------

Narg = len(sys.argv)

if Narg == 3 :
    ijson    = sys.argv[1]
    ifield   = sys.argv[2]
elif Narg < 3 :
    print(hstr)
    sys.exit(0)
elif Narg > 3 :
    print("** ERROR: too many inputs, use 2!")
    sys.exit(1)

# --------------- the hard work --------------------------

try:
    with open(ijson, 'r') as fff:
        idict = json.load(fff)    
except:
    print("** ERROR: bad json file input: {}".format(ijson))
    sys.exit("3")


try:
    oval = idict[ifield]
    print(oval)
except:
    print("** ERROR: bad json field input: {}".format(ifield))
    sys.exit("4")


sys.exit(0)
