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
#ver='1.41' ; date='July 26, 2019'
# + [PT] update help; include JSON description
#
#ver='1.5' ; date='July 31, 2019'
# + [PT] rename several variables and opts, to undo my misunderstanding...
# + [PT] EPI back to being required
#
#ver='1.6' ; date='Aug 2, 2019'
# + [PT] added in obliquity checks: should be able to deal with relative 
#        obl diffs between EPI and freq dset (if they exist)
# + [PT] final WARP dset will now be in EPI grid
# + [PT] *still need to check on scaling&recentering of Siemens data*
#
#ver='1.6' ; date='Aug 8, 2019'
# + [PT] update/correct help about Siemens scaling, post-discussion-with-Vinai
#
#ver='1.7' ; date='Aug 12, 2019'
# + [PT] *really* correct help @ Siemens scaling
# + [PT] change internal scaling: *really* demand units of ang freq (rad/s)
# + [PT] py23 compatability of help file-- single dictionary usage!
#
#ver='2.0' ; date='Aug 15, 2019'
# + [PT] new parameter scaling of freq dset from Vinai-- better params
# + [PT] apply obliquity info to output
# + [PT] fixed ocmds_fname, if opref contains path
# + [PT] output a useful params file
# + [PT] another py23 compatability fix
#
#ver='2.1' ; date='Aug 16, 2019'
# + [PT] change default number of erodes: 3 -> 1.  Vinai concurs!
#
#ver='2.2' ; date='Aug 23, 2019'
# + [PT] fix examples (use correct/newer opt names)
# + [PT] fix 'eff echo sp' -> 'bwpp' calculation ('matr len', not 'vox dim')
#
#ver='2.21' ; date='Aug 27, 2019'
# + [PT] update help file and descriptions (param text, for example)
# + [PT] add in more fields to param text output
#
#ver='2.22' ; date='Aug 29, 2019'
# + [PT] update help file
#
#ver='2.3' ; date='Aug 30, 2019'
# + [PT] add this set_blur_sigma() method, which had been
#        forgotten... Thanks, L. Dowdle!
#
#ver='2.31' ; date='Sept 9, 2019'
# + [PT] Fixed help file descripts-- thanks again, L. Dowdle.
#
#ver='2.32' ; date='Sept 10, 2019'
# + [PT] "hview"ify---thanks, RCR!
#
#ver='2.4' ; date='Sept 10, 2019'
# + [PT] now output mask from mask_B0() into the main odir, if that
#   func gets used;  useful for scripting+qc
#
#ver='2.5' ; date='Sept 12, 2019'
# + [PT] QC images output:
#        + images use magn vol as ulay, if entered; otherwise, ulay is EPIs
#
#ver='2.6' ; date='Sept 25, 2019'
# + [PT] major change: update/reverse polarity
#      + that is, the direction of (un)warping will be opposite for a given
#        PE direction
# + [PT] add in '-in_anat ..' opt, for maybe nicer QC (load in anat to be ulay) 
# + [PT] add in '-qc_box_focus_ulay' opt, for maybe nicer QC (focus on ulay)
#
#ver='2.61' ; date='Oct 2, 2019'
# + [PT] 3dmask_tool now to do dilate/erosion
#
ver='2.62' ; date='Oct 2, 2019'
# + [PT] Move to use '3dWarp ...' rather than 'cat_matvec ...' for
#        changing between EPI-freq dsets, which might have relative
#        obliquity difference; should be minisculy better for rounding
#        error considerations
#
##########################################################################

import sys, os

import afni_base   as ab
import lib_b0_corr as lb0

# =============================================================================

if __name__ == "__main__" : 

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

    iopts.write_params()
    iopts.write_history()

    self_vars = vars( iopts ) 
    print("\n------------")
    print("++ epi_b0_correct.py finishes.")
    print("++ Text of commands :  {ocmds_fname}"
          "".format( **self_vars ))
    print("++ Text of params   :  {opars_fname}\n"
          "".format( **self_vars ))
    if iopts.do_qc_image :
        print("++ QC images        :  {outdir}/{outdir_qc}/*.png\n"
              "".format( **self_vars ))
    print("++ MASK dset output :  {outdir}/{odset_mask}{dext}"
          "".format( **self_vars ))
    print("++ WARP dset output :  {outdir}/{odset_warp}{dext}"
          "".format( **self_vars ))
    print("++ EPI  dset output :  {outdir}/{odset_epi}{dext}\n"
          "".format( **self_vars ))

    sys.exit(0)



