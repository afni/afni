#!/usr/bin/env python

# simple program for inverting a list of unwanted subbrick indices
# into a list of *wanted* indices!

AUTHOR    = "PA Taylor (NIMH, NIH)"
#VERSION   = "1.0"
#VER_DATE  = "March 30, 2017"
# + [PT] birth, in current form
#
#VERSION   = "1.1"
#VER_DATE  = "June 26, 2018"
# + [PT] fixed issue when NO bads were selected
#
VERSION   = "1.2"
VER_DATE  = "July 17, 2018"
# + [PT] CONVERTED__python__2to3
#
# --------------------------------------------------------------------

import sys       as sys
import numpy     as np
import afni_util as au     # they are gold, indeed!

THIS_PROG = '@djunct_select_str.py'
NUM_ARGS  = 3                          # three input args needed
OUT_FNAME = ''

help_string = '''

--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Just a simple helper function for the fat_proc* scripts.  

Takes %d arguments: 
   1) file containing a list of integers;
   2) the number N of bricks in the dset (so max index is N-1);
   3) and an output file name.
--------------------------------------------------------------------

'''  % (THIS_PROG, VERSION, VER_DATE, AUTHOR, NUM_ARGS)

# ================================================================

def get_arg(aa):
    Narg = len(aa)
    
    if Narg == 0:
        print(help_string)
        sys.exit(0)
    elif Narg < NUM_ARGS:
        sys.exit("** ERROR: too few args!\n"
                 "   Need one input file name (contains list of integers);\n"
                 "   and an upper number index ofsubbricks;\n"
                 "   and an output fname.")
    elif Narg > NUM_ARGS:
        sys.exit("** ERROR: too many args! See help.")
    else:
        ifile = aa[0]         # fname
        inumb = int(aa[1])    # number of bricks in dset
        ofile = aa[2]         # output file name

        print("++ Input file    :", ifile)
        print("++ Input Nbricks :", inumb)
        print("++ Out file      :", ofile)

    return ifile, inumb, ofile

def Convert_StrList_to_NumArr(LL):

    MM = []
    for x in LL:
        y = x.strip()
        if y:                   # was it just a space?
            if y[0] != "#":     # was it just a comment?
                MM.append( int(y) )
    # doesn't even need to be sorted?
    return MM

def Invert_BadInd_List(LL, NMAX=-1):

    # [PT: June 26, 2018] fix to a silly error when LL was empty
    if len(LL):
        Lmax = max(LL)
    else:
        Lmax = 0
    
    if Lmax > NMAX:
        sys.exit("** ERROR: `bad list` max (%d) is greater than\n"
                 "   the apparent max subrick index (%d).\n" %
                 (Lmax, NMAX))
    GG = list(np.arange(NMAX))
    for ll in LL:
        try:
            GG.remove(ll)
        except:
            pass

    return GG


# =================================================================

if __name__=="__main__":

    # --------------------- get input ------------------------

    print("++ Command line:\n   ", ' '.join(sys.argv))
    (ifile, inumb, ofile)  =  get_arg(sys.argv[1:])

    # --------------------- proc it ------------------------

    bad_list_init  = au.read_text_file( ifile )
    bad_list_proc  = Convert_StrList_to_NumArr( bad_list_init )
    good_list      = Invert_BadInd_List( bad_list_proc, inumb )

    good_encoded   = au.encode_1D_ints( good_list )

    # ------------------- write out -------------------------

    print("++ OK, the list of good indices in AFNI selector format is:")
    print("\n    %s\n" % good_encoded)

    f = open(ofile, 'w')
    f.write(good_encoded)
    f.close()

    print("   ... which has been written to file: %s" % ofile)
    print("++ Done.")

    sys.exit(0)
