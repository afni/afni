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
SUMA colorbars, as well.  Order is reverse alphabetical.


We might/should add more over time.

|

.. list-table:: Reference images: the dsets viewed as overlay/underlays
   :header-rows: 1
   :widths: 15 35 35

   * - Name
     - anatomical dset
     - ROI dset
   * - Viridis
     - .. image:: media/cbars/Viridis.png
          :height: 3in
          :align: center
     - .. image:: media/cbar_refs/FIG_ref_anat.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbar_refs/FIG_ref_rois.axi.png
          :height: 3in
          :align: center

|

'''

table_head = \
'''

**%s**
==============

.. list-table:: 
   :header-rows: 1
   :widths: 15 10 35 35

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

# better sort key
def MakeLowerCase(s):
    return s.lower()

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

def write_out_edu_rst(ofile, lll, relpath=""):

    fff = open(ofile, 'w')
    lll.sort(key=MakeLowerCase, reverse=True)
    # Top level header stuff and description
    fff.write(text_label)
    fff.write(text_title_desc)

    grp = "Colorbars and example images"
    p1 = "media/cbars/"
    p2 = "media/cbars/IMGS_tt_cbar_"

    fff.write(table_head % (grp))
    fff.write("   * - Name\n" )
    fff.write("     - Cbar\n" )
    fff.write("     - opacity=9, anatomical [1,256]\n" )
    fff.write("     - opacity=4, ROIs [1,256]\n" )

    for x in lll:
        
        my_name   = x[:-4]
        my_cbar   = "media/cbars/"+x
        my_brain1 = "media/cbars/IMGS/tt_cbar_"+my_name+".axi.png"
        my_brain2 = "media/cbars/IMGS_MULTI/mm_cbar_"+my_name+".axi.png"

        fff.write("   * - %s\n" % (x[:-4]))
        fff.write("     - .. image:: %s\n" % (my_cbar))
        fff.write("          :height: 3in\n")
        fff.write("          :align: center\n")
        fff.write("     - .. image:: %s\n" % (my_brain1))
        fff.write("          :height: 3in\n")
        fff.write("          :align: center\n")
        fff.write("     - .. image:: %s\n" % (my_brain2))
        fff.write("          :height: 3in\n")
        fff.write("          :align: center\n")

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

    # need relative paths, internally
    rel_rdir = rdir.replace("../educational/", "")

    process = subprocess.Popen(['ls', rdir], stdout=subprocess.PIPE)
    stdout, stderr = process.communicate()

    file_list = parse_stdout(stdout)

    write_out_edu_rst(ofile, file_list, rel_rdir)

    print("++ Done writing all colorbars rst!")
