#!/usr/bin/env python

import numpy as np
import sys as sys
import pandas as pd

# in the original file input, "++" and "::" are special-- doooon't use
# them except as separators/markers!

# Each "GROUP" name line *must* start with "++" (no spaces beforehand)

import afni_util as au

THIS_PROG = 'convert_list_to_fields_pandas.py'
NUM_ARGS  = 2                          # two input args needed
OUT_FNAME = ''


final_cols = [
    "GROUP",
    "GRANK",
    "PROG",
    "RANK",
    "DESC"
]


# ===================================================================

VERSION   = "1.0"
VER_DATE  = "Feb 27, 2018"
AUTHOR    = "PA Taylor (NIMH, NIH)"

help_string = '''
--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Takes %d arguments: 
   1) file containing partially-classified doc;
   2) and an output file name.
--------------------------------------------------------------------

'''  % (THIS_PROG, VERSION, VER_DATE, AUTHOR, NUM_ARGS)

# ===================================================================

def get_arg(aa):
    Narg = len(aa)
    
    if Narg == 0:
        print help_string
        sys.exit(0)
    elif Narg < NUM_ARGS:
        sys.exit("** ERROR: too few args!\n"
                 "   Need one input file name (contains list of integers),\n"
                 "   and an output fname.")
    elif Narg > NUM_ARGS:
        sys.exit("** ERROR: too many args! See help.")
    else:
        ifile = aa[0]         # fname
        ofile = aa[1]         # output file name

        print "++ Input file    :", ifile
        print "++ Out file      :", ofile

    return ifile, ofile

# ----------------------------------------------------------------

def parse_lines(LL):

    '''
    final_cols = [ 
    "GROUP",
    "GRANK",
    "PROG",
    "RANK",
    "DESC"
    ]
    
    '''

    N = len(LL)
    list_group = [final_cols[0]]
    list_grank = [final_cols[1]]
    list_prog  = [final_cols[2]]
    list_rank  = [final_cols[3]]
    list_desc  = [final_cols[4]]

    for i in range(N):
        ltype = "empty"
        this_desc = ""
        x = LL[i]
        if len(x.strip()) :
            if x[:2] == "++":
                ltype = "group"
                y = x[2:].split("::")
                this_grank = int(y[0].strip())
                this_group = y[1].strip()
            else:
                ltype = "prog"
                y = x.split("::")

                this_rank = int(y[0].strip())
                this_prog = y[1].strip()
                
                if len(y) > 2:
                    if len(y[2]):
                        this_desc = y[2].strip()
                if len(this_prog):
                    list_group.append(this_group)
                    list_grank.append(this_grank)
                    list_prog.append(this_prog)
                    list_rank.append(this_rank)
                    list_desc.append(this_desc)

        #print "%10s  %5d  %s   %s" % (ltype, len(x.strip()), x, this_desc)

    lll = [list_group, list_grank, list_prog, list_rank, list_desc]
    mmm = map(list, zip(*lll))

    return mmm


# =================================================================

if __name__=="__main__":

    # --------------------- get input ------------------------

    print "++ Command line:\n   ", ' '.join(sys.argv)
    (ifile, ofile)  =  get_arg(sys.argv[1:])

    all_lines  = au.read_text_file( ifile )
    lll        = parse_lines( all_lines )
    lll_hhh    = lll.pop(0)

    # convert to data frame
    ddd        = pd.DataFrame(lll, columns=lll_hhh)

    # sort first by group, then by rank
    sss        = ddd.sort_values(by=['GRANK', 'GROUP', 'RANK'], ascending=False) 

    # ----------------- write it out -------------------------
    fff = open(ofile, 'w')
    
    Nr, Nc = sss.shape

    for i in range(Nr):
        fff.write("%-60s :: %2d :: %-35s :: %2d :: %-30s\n" % 
                  ( sss.iloc[i][final_cols[0]],
                    sss.iloc[i][final_cols[1]],
                    sss.iloc[i][final_cols[2]],
                    sss.iloc[i][final_cols[3]],
                    sss.iloc[i][final_cols[4]]
                )
        )
    fff.close()

    print("++ Done!")
