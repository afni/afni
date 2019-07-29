#!/usr/bin/env python

#ver='1.0' ; date='July 18, 2019'
# + [PT] translating Vinai's original afniB0() function from here:
#   https://github.com/nih-fmrif/bids-b0-tools/blob/master/distortionFix.py
#
#ver='1.1' ; date='July 22, 2019'
# + [PT] updated I/O, help, defaults, dictionaries
#
#
#ver='1.2' ; date='July 22, 2019'
# + [PT] redone shell exec
# + [PT] use diff opts/params for distortion direction and scale
#
#ver='1.3' ; date='July 24, 2019'
# + [PT] add in 3dinfo info to use
# + [PT] expand 3dROIstats options
# + [PT] write out *_cmds.tcsh file, recapitulate utilized param info at top
#
#ver='1.4' ; date='July 25, 2019'
# + [PT] change where scaling is applied-- now separate from 'polarity' issue
# + [PT] updated help (included examples); put beta warning messages!
#
ver='1.41' ; date='July 26, 2019'
# + [PT] update help; include JSON description
#
##########################################################################

import sys, os

import afni_base   as ab
import lib_b0_corr as lb0

# =============================================================================

if __name__ == "__main__" : 

    print("\n +* This is a: BETA VERSION! \n")

    iopts = lb0.parse_args_b0_corr(sys.argv)

    print("\n++ ================== Start B0 correction ================== \n"
          "   Ver  : {DEF_ver}\n"
          "   Date : {DEF_date}\n"
          "".format( **lb0.ddefs ))


    # Make a mask from a magn dset, if need be
    did_copy_inps = iopts.copy_inps_to_wdir()

    # Make a mask from a magn dset, if need be
    if not(iopts.dset_mask) :
        did_mask_B0 = iopts.mask_B0()

    # Do the main work
    did_B0_corr = iopts.B0_corr()

    iopts.write_history()

    print("\n +* This is a: BETA VERSION! \n")

    sys.exit(0)



