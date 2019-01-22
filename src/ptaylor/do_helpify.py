#!/usr/bin/env python

# Uuuuultra basic thing to wrap around some simple text files in order
# to prep them for being part of a C program's help file.

# Just takes a single argument:  the file name holding the text file.

# ----------------------------------------------------------------
# written so hastily by PA Taylor
#
# v0.5, Jan 9, 2018
# + birth
#
# v0.6, May 10, 2018
# + minor tweaks
#
# ----------------------------------------------------------------

import numpy as np
import sys as sys

topbot = "# " + (72*"-") + "\n"
emptyl = "\n"

def get_arg(aa):
    Narg = len(aa)
    if Narg < 2:
        print help_string
        sys.exit(0)
    elif Narg > 2:
        sys.exit("** ERROR: too many args! Need exactly one filename.")
    else:
        afile = aa[1]
        print "++ OK, have my integer argument:", afile

    return afile

def make_out_name(A):
    N = len(A)
    ll = -1
    for i in range(N):
        if A[N-1-i] == '.':
            ll = i
            break
    if ll == -1:
        return A+"_helpy.txt"
    else:
        out = A[:N-1-ll]+"_helpy" + A[N-1-ll:]
        return out

def wrapify_string(x, endchar = -1):
    if x.__contains__('"'):
        print("")
        print("** ERROR! there is a double-quote char present!:")
        print("   -> "+x)
        sys.exit(3)
    if x.__contains__('%'):
        print("")
        print("** ERROR! there is a percent symbol char present!:")
        print("   -> "+x)
        sys.exit(3)
    #x.replace("\\", "\\\\")
    #if x.__contains__("\\"):
    #    print x[:endchar]
    y = x[:endchar]  # do this to get right of trailing whitespace at end
    z = "\" "+y.rstrip()+"\\n\"\n"
    lz = len(z)
    if lz > 80:
        print "Warning! The following line has %d (>80) characters!" % lz
        print "   -> "+z
        print "      "+"|<"+("-"*36)+" 80 "+("-"*36)+">|"
        print ""
    return z

# ========================================================================

if __name__=="__main__":

    print "++ Command line:", sys.argv

    (afile) = get_arg(sys.argv)

    f = open(afile, 'r')
    atext = f.readlines()
    f.close()

    btext = []

    btext.append(wrapify_string(topbot))
#    btext.append(wrapify_string(emptyl))
    for x in atext:
        # take whole line, remove \n,
        newtext = wrapify_string(x)
        btext.append(newtext)
    btext.append(wrapify_string(emptyl))
    btext.append(wrapify_string(topbot))

    fout = make_out_name(afile)

    f = open(fout, 'w')
    for x in btext:
        f.write(x)
    f.close()
    print "++ Done!"
    print "   more", fout
    print "   emacs", fout
