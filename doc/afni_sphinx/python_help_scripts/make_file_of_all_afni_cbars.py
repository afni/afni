#!/usr/bin/env python

import numpy as np
import sys as sys
import os
import subprocess

import afni_util as au

THIS_PROG = 'make_file_of_all_afni_cbars.py'
NUM_ARGS  = 2                          # two input args needed

# ------------------------------------------------------------------

text_label = ".. _edu_afni_cbars:"

text_title_desc = \
'''

**************************
List of all AFNI colorbars
**************************

This is a list of all AFNI colorbars at present, showing both the
colorbar itself and an image that has a full overlay spectrum
displaying the colorbar.  (The image isn't meant to look meaningful
for every colorbar, it's just an example.)  Some of these apply as
SUMA colorbars, as well.

You can set your default overlay colorbar in with the
AFNI_COLORSCALE_DEFAULT environment variable in your "~/.afnirc" file.

We might/should add more over time.

|

'''

# ===================================================================

VERSION   = "1.0"
VER_DATE  = "April 17, 2018"
AUTHOR    = "PA Taylor (NIMH, NIH)"

help_string = '''
--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Takes %d arguments: 
   1) file containing all colorbar PNGs (and each one is assumed to  
      have a related image in LOCATION/IMGS/);
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

def parse_stdout(SS):

    allfiles = SS.split()
    allpngs = []
    for x in allfiles:
        if x[-4:] == ".png":
            allpngs.append(x)

    print "Found %d PNG files!" % len(allpngs)

    return allpngs

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
        self.tips  = []    # creates a new empty list for each dog
        self.count = 0    # creates a new empty list for each dog

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
    (rdir, ofile)  =  get_arg(sys.argv[1:])

    # put all recent helps into a file
    #all_png = subprocess.Popen(["ls", rdir], stdout=subprocess.PIPE)
    process = subprocess.Popen(['ls', rdir], stdout=subprocess.PIPE)
    stdout, stderr = process.communicate()
    file_list = parse_stdout(stdout)

    #os.system("ls %s" % (ifile))


    sys.exit("byeeeee")

    all_lines  = au.read_text_file( ifile )

    alltips = parse_all_lines(all_lines)

    write_out_startup_tip_rst(ofile, alltips)

    print("++ Done writing startup tips rst!")
