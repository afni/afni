#!/usr/bin/env python

# simple program for combining string selectors

AUTHOR    = "PA Taylor (NIMH, NIH)"
#VERSION   = "1.0"
#VER_DATE  = "Feb 18, 2018"
# + birth
#
#VERSION   = "1.1"
#VER_DATE  = "May 22, 2018"
# + change "union" -> intersection of lists
#
VERSION   = "1.2"
VER_DATE  = "July 17, 2018"
# + [PT] CONVERTED__python__2to3
#
# =================================================================

import sys       as sys
import numpy     as np
import afni_util as au     # they are gold, indeed!

THIS_PROG = '@djunct_combine_str.py'
MIN_NUM_ARGS  = 3                          # three input args needed
OUT_FNAME = ''

help_string = '''

--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Just a simple helper function for the fat_proc* scripts.  

Takes >= %d arguments: 
   1) an output file name;
   2) an int that is the upper index for the selector (-1 means
      just use the max number in the input strings)
   3) 1 or more string selector strings of *goods* to keep

Converts those string selectors to a list of ints, then
intersectionizes all the lists, and then spits out a new string
selector (with ',' and '..'  notation) to the output file.

--------------------------------------------------------------------

'''  % (THIS_PROG, VERSION, VER_DATE, AUTHOR, MIN_NUM_ARGS)

# ================================================================

def get_arg(aa):
    Narg = len(aa)
    
    if Narg == 0:
        print(help_string)
        sys.exit(0)
    elif Narg < MIN_NUM_ARGS:
        sys.exit("** ERROR: too few args!\n"
                 "   Need:\n"
                 "     one output file name;\n"
                 "     one int (max index or -1 for use max given);\n"
                 "     and a list of one or more string selectors.")
    else:
        ofile  = aa[0]         # fname
        maxind = int(aa[1])    # max index or -1 flag

        lstr = []
        print("++ string selector list:", ofile)
        for i in range(2, Narg):
            lstr.append(aa[i])
            print("   %s" % ( aa[i] ))

        print("++ Out file:", ofile)

    return ofile, maxind, lstr

# =================================================================

if __name__=="__main__":

    # --------------------- get input ------------------------

    print("++ Command line:\n   ", ' '.join(sys.argv))
    (ofile, maxind, lstr)  =  get_arg(sys.argv[1:])

    # [PT: May 22, 2018] want INTERSECTION, not union...
    Nlstr = len(lstr)
    if Nlstr < 1:
        sys.exit("Couldn't find any number strings??")
    # put all selectors together
    sss = set(au.decode_1D_ints(lstr[0], imax=maxind))
    for i in range(1,Nlstr):
        ll = lstr[i]
        sss = set.intersection(sss, set(au.decode_1D_ints(ll, imax=maxind)))

    # listify and sort
    list_final = list(sss)
    list_final.sort()

    good_encoded   = au.encode_1D_ints( list_final )

    # ------------------- write out -------------------------

    print("++ OK, the list of good indices in AFNI selector format is:")
    print("\n    %s\n" % good_encoded)

    f = open(ofile, 'w')
    f.write(good_encoded)
    f.close()

    print("   ... which has been written to file: %s" % ofile)
    print("++ Done.")

    sys.exit(0)
