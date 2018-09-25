#!/usr/bin/env python
#
# Version 1.0, Jan, 2015.
# written:  PA Taylor (UCT, AIMS).
#
#
# 
#
#  # for help on commandline running and usage:
#  $  fat_mvm_review.py -h
#
#
###########################################################################
###########################################################################


import lib_fat_funcs as GR
import getopt, sys 
from glob import glob
import os.path as op
import numpy as np

def main(argv):
    '''Basic reading in of commandline options.'''

    help_line = '''\
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

** JUST BETA VERSION AT THE MOMENT **

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'''
    comm_str = 'fat_mvm_review.py '
    file_in = ''
    pref_out = ''
    thr_out = 10**6
    TRUNC = 0

    # allow status 0 on -help   24 Sep 2018 [rickr]
    if "-help" in argv:
        print help_line
        sys.exit()

    try:
        opts, args = getopt.getopt(argv,"hTf:p:t:",["help",
                                                    "Trunc_labs",
                                                    "file_in=",
                                                    "prefix=",
                                                    "thr_out="
                                                ])

    except getopt.GetoptError:
        print "** Error reading options. Try looking at the helpfile:"
        print "\t $  fat_mvm_review.py -h\n"
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print help_line
            sys.exit()
        elif opt in ("-f", "--file_in"):
            file_in = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-p", "--prefix"):
            pref_out = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-t", "--thr_out"):
            thr_out = float(arg)
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-T", "--Trunc_labs"):
            TRUNC = 1
            comm_str = GR.RecapAttach(comm_str, opt, arg)




    if not(file_in):
        print "** ERROR: missing an input (*MVM.txt) file."
        print "\t Need to use either '-f' or '--file_in'."
        sys.exit()

    # not requisite to have output prefix
    #if not(pref_out):
    #    print "** ERROR: missing an output prefix."
    #    print "\t Need to use either '-p' or '--pref_out'."
    #    sys.exit()





    return comm_str, file_in, pref_out, thr_out, TRUNC


########################################################################

if __name__=="__main__":
    np.set_printoptions(linewidth=200)
    print "\n"

    comm_str, FI, PO, TO, TRUNC = main(sys.argv[1:])

    if FI[0] == '~' :
        FI = op.expanduser(FI)

    FI_glob = glob(FI)
    FI_glob.sort()

    LIST_FOR_OUT = []
    LIST_FOR_PHOUT = []

    for FI_indi in FI_glob:

        print "\nFOUND:   %s" % (FI_indi)

        FI_list = GR.ReadInGridOrNetcc(FI_indi)

        FI_words = GR.BreakLineList(FI_list)

        FI_mat, FI_par, FI_var = GR.Find_ANOVAE(FI_words)
        FI_phmat, FI_phpar, FI_phvar = GR.Find_POSTHOC(FI_words)

        nvar = len(FI_var)
        npar = len(FI_par)

        if TRUNC:
            for j in range(nvar):
                if len(FI_var[j]) > 10:
                    FI_var[j] = FI_var[j][:10]
            for j in range(len(FI_phvar)):
                if len(FI_phvar[j]) > 10:
                    FI_phvar[j] = FI_phvar[j][:10]

        #  - - - - - - - - - - - -  -  OUTPUT  - - - -  - - - - -- -- -- -

        LIST_FOR_OUT = GR.ScreenAndFileOutput( PO, 
                                               TO, 
                                               FI_mat, 
                                               FI_par, 
                                               FI_var,
                                               FI_indi,
                                               LIST_FOR_OUT,
                                               FI_mat)
        if FI_phvar :
            LIST_FOR_PHOUT = GR.ScreenAndFileOutput( PO, 
                                                     TO, 
                                                     FI_phmat, 
                                                     FI_phpar, 
                                                     FI_phvar,
                                                     FI_indi,
                                                     LIST_FOR_PHOUT,
                                                     FI_mat)



    if PO: 
        outname = PO + '_ANOVAE.txt'
        f = open(outname, 'w')
        for ll in LIST_FOR_OUT:
            f.write("%s" % (ll))
        f.close()

        print "\nWrote out file: ", outname

        if FI_phvar :
            for phvar in FI_phvar:
                outname = PO + '_PH_'+phvar+'.txt'
                f = open(outname, 'w')
                for ll in LIST_FOR_PHOUT:
                    f.write("%s" % (ll))
                f.close()

                print "\nWrote out file: ", outname
