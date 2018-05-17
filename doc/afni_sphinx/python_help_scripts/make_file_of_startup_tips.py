#!/usr/bin/env python

import numpy as np
import sys as sys
import os

import afni_util as au

THIS_PROG = 'make_file_of_startup_tips.py'
NUM_ARGS  = 2                          # two input args needed

# ------------------------------------------------------------------

text_label = ".. _edu_startup_tips:"

text_title_desc = \
'''

****************************
**List of all startup tips**
****************************

This is a list of *possibly* useful tips that the AFNI gurus have
thought up, which may inform you, Dear User, of cool features within
AFNI. By default, one gets displayed every time you start up the AFNI
GUI (unless you have set an environment variable in "~/.afnirc" to
turn off this delightful feature).  Hence the name of this list.

We might/should add more over time.

|

'''

# ===================================================================

VERSION   = "1.0"
VER_DATE  = "Mar 29, 2018"
AUTHOR    = "PA Taylor (NIMH, NIH)"

help_string = '''
--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Takes %d arguments: 
   1) file containing all startup tips;
   2) and an output file name.
--------------------------------------------------------------------

'''  % (THIS_PROG, VERSION, VER_DATE, AUTHOR, NUM_ARGS)

# =================================================================

def get_arg(aa):
    Narg = len(aa)
    
    if Narg == 0:
        print help_string
        sys.exit(0)
    elif Narg < NUM_ARGS:
        sys.exit("** ERROR: too few args!\n"
                 "   Need one input file name (contains all tips),\n"
                 "   and an output fname.")
    elif Narg > NUM_ARGS:
        sys.exit("** ERROR: too many args! See help.")
    else:
        ifile = aa[0]         # fname
        ofile = aa[1]         # output file name

        print "++ Input file    :", ifile
        print "++ Out file      :", ofile

    return ifile, ofile

def parse_all_lines(LL):

    N = len(LL)
    tipcount = 0
    START = 0
    alltips=list_tips("the list")
    aa = []
    for i in range(N):
        x = LL[i]
        if not(x.strip('-')): # includes both empty and all-hyphen lines
            START = 0
        else:
            if START:
                #print i, ":", x
                aa.append(x)
            elif x.__contains__("-------- AFNI Startup"):
                START = 1
                aa = []
            else:
                START = 0
                continue

        if aa and not(START): # then we must have just finished one
            #print aa
            tipcount+=1
            newtip = tip("", aa, tipcount)
            alltips.add_tip(newtip)
            aa = [] # reset
           
    print("++ Found %d tips!" % (tipcount))
    return alltips

def write_out_startup_tip_rst(ofile, ttt):
    fff = open(ofile, 'w')

    # Top level header stuff and description
    fff.write(text_label)
    fff.write(text_title_desc)

    Nt = ttt.count

    for i in range(Nt):
        
        fff.write("**Tip "+str(alltips.tips[i].num)+"**\n\n")
        fff.write("    .. code-block:: none\n\n")
        #fff.write("**Tip "+str(alltips.tips[i].num)+"**\n")
        x = alltips.tips[i].tip
        Nrow = len(x)
        for j in range(Nrow):
            fff.write("        "+x[j]+"\n")
            #fff.write("   | "+check_for_webaddr(x[j])+"\n")
        fff.write("\n")
    fff.close()

def check_for_webaddr(sss):

    olds = []
    news = []
    ttt = sss.split()
    for x in ttt:
        if x[:4] == "http":
            uuu = "`"+x+" <"+x+">`_"
            olds.append(x)
            news.append(uuu)

    if olds:
        ppp = sss[:]
        for i in range(len(olds)):
            print("Replacing:", olds[i])
            ppp = ppp.replace(olds[i], news[i])
        return ppp
    else:
        return sss
        

# ====================================================================

# AFNI startup tip class
class list_tips:

    def __init__(self, name):
        self.name  = name
        self.tips  = [] 
        self.count = 0  

    def add_tip(self, newtip):
        self.tips.append(newtip)
        self.count+=1

class tip:

    def __init__(self, name, tip, num):
        self.name = name
        self.tip  = tip
        self.num  = num

# ====================================================================

if __name__=="__main__":

    # --------------------- get input ------------------------

    print "++ Command line:\n   ", ' '.join(sys.argv)
    (ifile, ofile)  =  get_arg(sys.argv[1:])

    # put all recent helps into a file
    os.system("afni -startup ALL > %s" % (ifile))

    all_lines  = au.read_text_file( ifile )

    alltips = parse_all_lines(all_lines)

    write_out_startup_tip_rst(ofile, alltips)

    print("++ Done writing startup tips rst!")
