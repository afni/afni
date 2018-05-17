#!/usr/bin/env python

import numpy as np
import sys as sys
import pandas as pd

# in the original file input, "++" and "::" are special-- doooon't use
# them except as separators/markers!

# Each "GROUP" name line *must* start with "++" (no spaces beforehand)

import afni_util as au

import convert_list_to_fields_pandas as clfp


THIS_PROG = 'convert_fields_to_rst.py'
NUM_ARGS  = 2                          # two input args needed
OUT_FNAME = ''

final_cols = [
    "GROUP",
    "GRANK",
    "PROG",
    "RANK",
    "DESC"
]

# ======================================================================
# ======================================================================

text_label = ".. _edu_class_prog:"

text_title_desc = \
'''

***************************
**Classified program list**
***************************

All AFNI programs, great and small, are listed here and classified
based on functionality.  That is, they are grouped into some general
categories that we made up and given short bios.

The ranking of each program is to highlight ones that we think are
particularly useful in general processing ('5' being the most directly
useful, and '1' being something that might just be a low-level,
supplementary tool).  Note that a given program may appear in more
than one group.

This page might be most useful by using your browser to search through
the text for keywords of interest, such as "ROI", "mask", "diffusion",
"align", "model", etc.  Clicking on the name of the program will bring
you its online help documentation, referenced from :ref:`this page of
all AFNI "helps"<programs_main>`.

.. contents:: :local:

|

'''

table_head = \
'''

**%s**
==============

.. list-table:: 
   :header-rows: 0
   :widths: 5 20 70

'''

# ======================================================================
# ======================================================================

VERSION   = "1.0"
VER_DATE  = "Mar 22, 2018"
AUTHOR    = "PA Taylor (NIMH, NIH)"

help_string = '''
--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Takes %d arguments: 
   1) file containing fields of groups/ranks/progs/descriptions;
   2) and an output file name (-> rst).
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

def parse_field_lines(LL):

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
    list_group = []
    list_grank = []
    list_prog  = []
    list_rank  = []
    list_desc  = []

    for i in range(N):
        x = LL[i]
        if x:
            if len(x.strip()) :
                y = x.split("::")
                list_group.append(y[0].strip())
                list_grank.append(y[1].strip())
                list_prog.append(y[2].strip())
                list_rank.append(y[3].strip())
                list_desc.append(y[4].strip())

    lll = [list_group, list_grank, list_prog, list_rank, list_desc]
    mmm = map(list, zip(*lll))

    return mmm

def write_out_edu_rst(ofile, lll):
    fff = open(ofile, 'w')

    # Top level header stuff and description
    fff.write(text_label)
    fff.write(text_title_desc)

    grp = ""
    for x in lll:
        # make new table if grp
        if x[0] != grp:
            grp = x[0]
            fff.write(table_head % (grp))
        fff.write("   * - %s\n" % (x[3]))
        fff.write("     - :ref:`%s <ahelp_%s>`\n" % (x[2], x[2]))
        fff.write("     - %s\n" % (x[4]))



    fff.close()


# =================================================================

if __name__=="__main__":

    # --------------------- get input ------------------------

    print "++ Command line:\n   ", ' '.join(sys.argv)
    (ifile, ofile)  =  get_arg(sys.argv[1:])

    all_lines  = au.read_text_file( ifile )
    lll        = parse_field_lines( all_lines )

    write_out_edu_rst(ofile, lll)

    print("++ Done writing educational classification rst!")
