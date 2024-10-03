#!/usr/bin/env python
#

auth = 'PA Taylor'
# ver : 1.33 || date: Oct 15, 2018 
# + add in new uvar vars (nt_orig, etc.)
#
# ver : 1.34 || date: Oct 16, 2018 
# + all the new template checks and considerations, for new @Find*
# + define "top level"
# + put in $templ_vol checks
#
# ver : 1.34 || date: Oct 17, 2018
# + add in more images
# + introduce WARN
# + add header text/descrip
#
#ver = '1.35' ; date = 'Oct 17, 2018'
# + add @ss_review_basic output
# + rename dependencies
#
#ver = '1.4' ; date = 'Oct 18, 2018' 
# + io by argv
#
#ver = '1.5' ; date = 'Oct 18, 2018' ; auth = 'PA Taylor'
# + new I/O, renamed files, newer way of putting title
# + will fail if template was used but can't be found
#
#ver = '1.51' ; date = 'Oct 19, 2018' 
# + 'exit 0' added
#
#ver = '1.6' ; date = 'Nov 20, 2018' 
# + [PT] RUN_STYLE now formalized through input name; default 'basic'
# + [PT] end with @ss_review_basic echoed to terminal
#
#ver = '1.7' ; date = 'Nov 23, 2018' 
# + [PT] Now each section outputs a JSON file of info.
#        This replaces the *txt files for each image-- more versatile
#        with info handling
# + [PT] Each output includes link-ID and hovering text for each
# + [PT] title info is now handled with a JSON, too
#
# ver = '1.8' ; date = 'Nov 27, 2018' 
# + [RCR] fixed py 2/3 compatibility
#
#ver = '1.9' ; date = 'Nov 27, 2018' 
# + [PT] changed the conditions for 1dplotting programs
#        -> now can still have enorm and outlier plots even without 
#           censor_dset being in uvar json
#        -> text and subtext strings change depending on what sets are
#           available for those
# + [PT] also, in 'basic' html mode, 1dplot in 'VR6' is now just
#        showing the 6 volreg params without enorm and outlier frac
# + [PT] output %age of vols censored with the enorm and outlier plots
#
#ver = '1.91' ; date = 'Dec 23, 2018' 
# + [PT] rename "run_mode" -> "run_style" (more specific/correct)
#
#ver = '2.0' ; date = 'Jan 2, 2019' 
# + [PT] changed order of enorm and outlier (have former just after VR6)
# + [PT] label on enorm, "~mm"
# + [PT] fix name of per stimulus regressors file: X.stim.xmat.1D
# + [PT] add in EPI in orig space volume, using volreg base vol
#
#ver = '2.1' ; date = 'Feb 26, 2019' 
# + [PT] new plot in regr: grayplot
#
#ver = '2.2' ; date = 'May 14, 2019' 
# + [PT] radcor plots
#
#ver = '2.3' ; date = 'May 19, 2019' 
# + [PT] start aea_checkflip stuff
#
#ver = '2.4' ; date = 'May 22, 2019' 
# + [PT] more details of aea_checkflip
# + [PT] radcor to own QC block
#
#ver = '2.5' ; date = 'May 23, 2019' 
# + [PT] switched to using afni_base functions for executing on
#        commandline
#
#ver = '2.51' ; date = 'June 14, 2019' 
# + [PT] tiiiiny change, updating variable name to match 'omsg'
#
#ver = '2.6' ; date = 'June 18, 2019' 
# + [PT] ephemeral change: when surf and mask blocks are *both* used,
#        don't output grayplot; this will later be revisited when
#        surface things are considered more carefully (i.e., at all)
#
#ver = '2.7' ; date = 'June 26, 2019' 
# + [PT] elif for vstat: no anat or templ, use volreg as ulay; West
#        Coast usage for S Torrisi.
#
#ver = '2.9' ; date = 'July 3, 2019' 
# + [PT] vorig block now starting to be used
# + [PT] add in more stats to be viewed
# + [PT] add in QC block ID to QC block titles
#
#ver = '2.95' ; date = 'July 16, 2019' 
# + [PT] include obliquity in vorig QC block
# + [PT] simplify radcor text; decrease repetition
#
#ver = '3.0' ; date = 'July 18, 2019' 
# + [PT] include obliquity in vorig QC block
# + [PT] simplify radcor text; decrease repetition
# + [PT] -> merge in changed opts for radcor
#
#ver = '3.1' ; date = 'Sep 6, 2019' 
# [PT] put a montgap (1 line, black) into QC montages: sep imgs a bit
#    + put in censoring to the 1dplot.py command when showing VR6 -
#      also known as the 'Molfese approach'
#
#ver = '3.11' ; date = 'Sep 9, 2019' 
# [PT] spacing fix in VR6 with censoring
#
#ver = '3.12' ; date = 'Dec 26, 2019' 
# [PT] for regr QC block, indiv stim plotting: don't need 'xmat_uncensored'
#      as a dependency, so remove it from the list
#
#ver = '3.2' ; date = 'Jan 9, 2019' 
# [PT] new warning block: censor fraction
#    + seed point plotting introduced (vstat section for resting state)
#
#ver = '3.3' ; date = 'Feb 15, 2020' 
# [PT] new funcs for 'widely used' params
#    + for censor and sundry info.  
#
#ver = '3.31' ; date = 'Feb 17, 2020' 
# [PT] further cleaned up (simplified?) a lot of the censoring info
#
#ver = '3.32' ; date = 'Feb 21, 2020' 
# [PT] fix minor bug in case of: 'basic' html with no outlier-based censoring
#
#ver = '3.33' ; date = 'Feb 26, 2020' 
# [PT] fix minor bug in case of: 'pythonic' html with no censoring at all. Sigh.
#
#ver = '3.4' ; date = 'March 11, 2020' 
# [PT] change way template/final_anat dsets are proc'ed/used.
#    + new top level section to get template/anat_final properties
#    + va2t: now underlay anat, and use template for edges
#    + vstat: now underlay template (if there), instead of anat_final
#    + regr: use template as ulay (if there), instead of anat_final
#
#ver = '3.41' ; date = 'March 12, 2020' 
# [PT] no vstat if 'surf' block was used in AP (-> stats dset is
#      *.niml.dset)
#
#ver = '3.5' ; date = 'March 27, 2020' 
# [PT] remove dependency on lib_apqc_html_helps.py
#
#ver = '3.6' ; date = 'May 26, 2020' 
# [PT] ve2a and LR-flipcheck now show EPI under anat edges
#
#ver = '3.61' ; date = 'May 28, 2020' 
# [PT] in vstat maps, report DF value(s)
#
#ver = '3.62' ; date = 'May 31, 2020' 
# [PT] EPI ulay ranges in ve2a and LR-flipcheck now: NZ 2-98%
#
#ver = '3.63' ; date = 'May 31, 2020' 
# [PT] vstat seedbased corr seed thr from 0.3 -> 0.2
#
#ver = '3.7' ; date = 'Feb 24, 2021' 
# [PT] Have been adding TSNR plotting, more added.
#
#ver = '3.73' ; date = 'Mar 5, 2021'
# [PT] cp review basic text to QC_*/ dir
#
#ver = '3.74' ; date = 'Apr 6, 2021'
# [PT] update TSNR-vreg checks
#    + give sep names for TSNR images: tsnr_vreg and tsnr_fin
#
#ver = '3.75' ; date = 'Apr 6, 2021'
# [PT] now use adjunct*tsnr*general prog (just added, only need 1 prog)
#
#ver = '3.76' ; date = 'Sep 21, 2021'
# [PT] use '-no_cor' to not make coronal plane images
#    + save nearly 33% of space in QC_${subj} dir
#
#ver = '3.77' ; date = 'Sep 21, 2021'
# [PT] adjunct*tsnr: '-no_cor' to not make coronal plane images
#    + keep applying new opt
#
#ver = '3.78' ; date = 'Sep 27, 2021'
# [PT] Due to recent changes (from ~Aug 23) in label_size defaults
#      in imseq.c, adjust the default labelsize from 3 -> 4.
#    + this should restore labels to their longrunning size (since Aug
#      23 they have been one size smaller by default); but the new font
#      will be bolder than previously, due to those imseq.c changes.
#
#ver = '3.8' ; date = 'Jan 18, 2022'
# [PT] Add 'mecho' QC block
#    + pretty much just for combine_method=m_tedana for starters
#
#ver = '3.9' ; date = 'Jan 20, 2022'
# [PT] major update to vstat block, for task-based FMRI data
#    + larger selection of stats data automatically imagized, with new
#      internal logic to handle this.  See new library lib_apqc_stats_dset.py
#
#ver = '3.91' ; date = 'Jan 20, 2022'
# [PT] add in comment at top of @ss_review_html script, echoing the
#      command used to create the script; also put text there if
#      'pythonic' mode was downgraded to 'basic' 
#
#ver = '3.92' ; date = 'Jan 25, 2022'
# [PT] vorig has new image: copy_anat dset
#
#ver = '3.93' ; date = 'Jan 26, 2022'
# [PT] epi-anat overlap in vorig QC block 
#
#ver = '3.94' ; date = 'Feb 8, 2022'
# [PT] AP can now pass opts here via '-html_review_opts ..'
# - first one is '-mot_grayplot_off', for S Torrisi.
#
#ver = '4.01' ; date = 'June 6, 2022' 
# [PT] new ve2a entry, if EPI is unifized (via uvar=final_epi_unif_dset)
#    + also better control of brightness scaling for edgy EPI/anat images
#
#ver = '4.02' ; date = 'June 10, 2022' 
# [PT] ... and just like that, no longer make second ve2a image anymore,
#      that would be based on final_epi_unif_dset. Was extraneous/unnec.
#      An ex-parrot.
#
#ver = '4.03' ; date = 'Aug 18, 2022'
# [PT] add warns: 3dDeconvolve *.err text file
#
#ver = '4.04' ; date = 'Aug 18, 2022'
# [PT] add mask_dset images: overlays final dset, whether in 
#      va2t, ve2a or vorig QC block
#
#ver = '4.05' ; date = 'Aug 18, 2022'
# [PT] put already-calc'ed Dice info below ve2a and va2t olay imgs
#      ---> but just as quickly have removed it; might distract from the
#           important sulcal/gyral overlap
#
#ver = '4.1' ; date = 'Oct 5, 2022'
# [PT] add in run_instacorr_errts.tcsh script
#
#ver = '4.2' ; date = 'Nov 15, 2022'
# [PT] add in run_instacorr_tcat.tcsh script
#
#ver = '4.3' ; date = 'Jan 6, 2023'
# [PT] new opt: -vstat_list, to add a user-defined list of labels
#      that would appear in the vstat section (default is still to have
#      5 chosen by the program)
#
ver = '5.0' ; date = 'Mar 05, 2023'
# [PT] move toward Python-only implementation, rather than generating
#      a script intermediately, to simplify flexibility, additions and
#      apqc2/NiiVue functionality
#
#########################################################################

import sys
import os
import json
import glob

from afnipy import afni_base           as BASE
from afnipy import afni_util           as UTIL
from afnipy import lib_apqc_tcsh       as lat
from afnipy import lib_apqc_stats_dset as lasd
from afnipy import lib_ss_review       as lssr
from afnipy import lib_apqc_io         as laio
from afnipy import lib_apqc_run_icgv   as lari     # make run_*tcsh scripts
from afnipy import lib_format_cmd_str  as lfcs

# all possible uvars
all_uvars = []
for x in lssr.g_ss_uvar_fields: 
    all_uvars.append(x[0])

### !!!! need to put something here about checking about
### !!!! knowing template, and getting a list of seed
### !!!! locations (maybe someday user can enter seed
### !!!! locations); also have a label
# in MNI space here:
#seed_locs = [['PCC', 5, 49, 40], ['vis', -1, 77, 11]]
# ['vis', 18, 99, 0]
#seed_locs = [['lh-precuneus',6, 54, 50],
#             ['rh-vis-cortex',-4, 91, -3],
#             ['rh-Mot',-21, 24, 71],
#             ['rh-DAtt',51, -5, 26],
#             ['lhInsSal',-42, -12, -9],
#             ['PCC', 5, 49, 40],
#             ['RetSplCor', -3, 42, 27], 
#             ['PreCVA', -6, 50, 42], 
#             ['vis', 22, 103, 0],
#             ['rhMotor', -32, 25, 54]
#]
# list of seed locs; RAI DICOM coords
# prob need a rad depend on vox size


# ===================================================================
# ===================================================================

if __name__ == "__main__":

    iopts = laio.parse_tcsh_args(sys.argv[1:])

    # note the original location, to which to return at the end
    pwd_orig = os.getcwd()

    # work from subjdir
    if os.path.isdir(iopts.subjdir) :
        os.chdir(iopts.subjdir)
    else:
        print("** ERROR: subjdir '{}' does not exist".format(iopts.subjdir))
        sys.exit(1)
    #### NO LONGER USE otcsh
    # define output tcsh script name
    #otcsh = iopts.subjdir + '/' + lat.scriptname

    # determines what functions/images are used.
    RUN_STYLE = iopts.revstyle

    # get dictionary form of json
    with open(iopts.json, 'r') as fff:
        ap_ssdict = json.load(fff)    

    # also read in ss_rev file to dict
    ssrev_dict = lat.read_in_txt_to_dict(ap_ssdict['ss_review_dset'])

    # add dirs to be made: odir_qc, odir_img, odir_info
    ap_ssdict = lat.set_apqc_dirs(ap_ssdict)

    # add len of EPI run(s): pats
    ap_ssdict = lat.set_apqc_sundry(ap_ssdict, ssrev_dict)

    # add main dset name for ulays: main_dset
    ap_ssdict = lat.set_apqc_main_dset(ap_ssdict)

    # add censoring info: numbers ranges and text blocks
    # Q: what about RUN_STYLE=='none'?
    if RUN_STYLE == 'basic' :
        ap_ssdict = lat.set_apqc_censor_info_BASIC( ap_ssdict )
    elif RUN_STYLE == 'pythonic' :
        ap_ssdict = lat.set_apqc_censor_info_PYTHONIC( ap_ssdict )
    else:
        print("** ERROR: unknown review style: {}".format(RUN_STYLE))
        sys.exit(2)

    # ----------------------- InstaCorr scripts ----------------------

    # [PT: Oct 5, 2022] make an instacorr run script in the main
    # results directory
    # [PT: June 25 2023] now both IC and GV (for pbrun and errts) from
    # single library

    ldep     = ['errts_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        if not(ap_ssdict['errts_dset'].__contains__('.niml.dset')) :
            stat_ic_errts = lari.write_apqc_icgv_script('IC', 'errts', 
                                                        ap_ssdict)
            stat_gv_errts = lari.write_apqc_icgv_script('GV', 'errts', 
                                                        ap_ssdict)

    # pbrun instacorr and graphview scripts
    if 1 :
        stat_ic_pbrun = lari.write_apqc_icgv_script('IC', 'pbrun', ap_ssdict)
        stat_gv_pbrun = lari.write_apqc_icgv_script('GV', 'pbrun', ap_ssdict)

    # ----------------- initialize some params/switches ----------------

    DO_REGR_CORR_ERRTS = 0
    HAVE_MASK          = lat.check_dep(ap_ssdict, ['mask_dset'])

    # -------------------------------------------------------------------
    # -------------------- start + header -------------------------------

####### TO BE KEPT AND USED IN A LOG SOMEWHERE
#    # Add in script used to write this script
#    full_argv = ' '.join(['apqc_make_tcsh.py'] + sys.argv[1:])
#    cmd_log = '''# This script was created with this command:\n'''
#    cmd_log+= lfcs.afni_niceify_cmd_str(full_argv, 
#                                        comment_start = '#    ',
#                                        max_lw = 74)[1]
#
#
#    if iopts.pythonic2basic :
#        comm_p2b = ''' 
#        +* WARNING: The user asked for 'pythonic' APQC, but there are
#        missing dependencies.  This APQC run was therefore downgraded
#        to 'basic'.
#        || 
#        -> To fix: Please check the warnings in out.review_html, but
#        likely Matplotlib is missing or has version<2.2.  You can add
#        this dependency (verify with 'afni_system_check.py
#        -check_all') and redo the APQC pythonically.'''
#        comm_p2b     = lat.commentize(comm_p2b, padpre=1, padpost=1)

    # --------------------------------------------------------------------
    # create output dir (can backup any old one)

    tmp1 = lat.make_apqc_dirs( ap_ssdict, ow_mode=iopts.ow_mode, 
                               bup_dir=iopts.bup_dir )

    tmp2 = lat.copy_apqc_logos(ap_ssdict)
    tmp3 = lat.copy_apqc_fonts(ap_ssdict)

    # --------------------------------------------------------------------
    # pagetop info

    ldep  = ['subj']
    if lat.check_dep(ap_ssdict, ldep) :
        opref = lat.page_title_json  # here, just the title page name
        lat.make_apqc_Top_pagetop( ap_ssdict, opref, "HOME", "pagetop" )

    # --------------------------------------------------------------------
    # --------------------------------------------------------------------
    # --------------------------------------------------------------------

    # --------------------------------------------------------------------
    #
    # The following is a list of images that MAY be made as part of
    # the QC.  These are now organized as QC blocks (see
    # lah.qc_blocks).  The order that these are listed here are
    # basically the order in which they might occur there, and their
    # file names will also reflect their QC block.  
    # 
    # Conventions for naming things:
    #
    # QC block names are short-ish (<= 5 chars) in order to keep small
    # but constantly sized label sections at the top of the APQC HTML.
    # 
    # "obase" format for file names:
    #   [prefix 'qc'] _ [index]
    #
    # final "opref" format for file names:
    #   [prefix 'qc'] _ [index] _ [block abbrev] _ [specific file in block]
    #
    # functions in lib_apqc_tcsh:
    #   [prefix 'apqc'] _ [block abbrev] _ [specific file in block]
    # 
    # To add a new QC thing to an existing QC block, just follow the
    # above conventions in both this file and lib_apqc_tcsh.  These
    # names are also used in the JSON files created for each
    # image. There are examples of programs dealing with: vols, 1D
    # files, text warnings, and other 'data'.
    #
    # If adding a new QC block, probably just do so by following the
    # above conventions once one decides on the QC block name and
    # specific part sub-name, and in the JSON files.
    #
    # --------------------------------------------------------------------

    idx  = 0

    # --------------------------------------------------------------------

    # QC block: "vorig"
    # item    : EPI in orig

    ldep  = ['vr_base_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_all( ap_ssdict, obase, "vorig", "EPI", 
                                       ulay=ap_ssdict[ldep[0]] )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vorig"
    # item    : anat in orig

    ldep  = ['copy_anat']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_all( ap_ssdict, obase, "vorig", "anat", 
                                       ulay=ap_ssdict[ldep[0]] )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vorig"
    # item    : init EPI anat overlap

    ldep  = ['vr_base_dset', 'copy_anat']
    if lat.check_dep(ap_ssdict, ldep) :
        # no focus_box necessary here
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_olap( ap_ssdict, obase, "vorig", "olap" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vorig" (*but could be others; see elsewhere for this func)
    # item    : EPI mask on final dset (template, anat_final, *vr_base*)

    ldep       = ['mask_dset', 'vr_base_dset']
    ldep_anti1 = ['template']    # check this does NOT exist
    ldep_anti2 = ['final_anat']  # check this does NOT exist
    if lat.check_dep(ap_ssdict, ldep)              and \
       not( lat.check_dep(ap_ssdict, ldep_anti1) ) and \
       not( lat.check_dep(ap_ssdict, ldep_anti2) ) :
        focusbox = ap_ssdict['main_dset']
        ulay     = ap_ssdict['main_dset']
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_gen_mask2final( ap_ssdict, obase, "vorig", 
                                            "mask2final", ulay, focusbox )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "ve2a"
    # item    : EPI to anat align

    ldep  = ['final_anat', 'final_epi_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        focusbox  = ap_ssdict['main_dset']
        dice_file = None
        if lat.check_dep(ap_ssdict, ['mask_corr_dset']) :
            dice_file = ap_ssdict['mask_corr_dset']

        ban      = lat.bannerize('EPI and anatomical alignment')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_ve2a_epi2anat( ap_ssdict, obase, "ve2a", 
                                           "epi2anat", focusbox, 
                                           dice_file )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "ve2a" (*but could be others; see elsewhere for this func)
    # item    : EPI mask on final dset (template, *anat_final*, vr_base)

    ldep      = ['mask_dset', 'final_anat']
    ldep_anti = ['template']  # check this does NOT exist
    if lat.check_dep(ap_ssdict, ldep) and \
       not( lat.check_dep(ap_ssdict, ldep_anti) ) :
        focusbox = ap_ssdict['main_dset']
        ulay     = ap_ssdict['main_dset']
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_gen_mask2final( ap_ssdict, obase, "ve2a", 
                                            "mask2final", ulay, focusbox )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "va2t"
    # item    : anat to template align

    ldep = ['final_anat', 'template']
    if lat.check_dep(ap_ssdict, ldep) :
        focusbox  = ap_ssdict['main_dset']
        dice_file = None
        if lat.check_dep(ap_ssdict, ['mask_anat_templ_corr_dset']):
            dice_file = ap_ssdict['mask_anat_templ_corr_dset']

        ban      = lat.bannerize('anatomical and template alignment')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_va2t_anat2temp( ap_ssdict, obase, "va2t", 
                                            "anat2temp", focusbox, 
                                            dice_file )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "va2t" (*but could be others; see elsewhere for this func)
    # item    : EPI mask on final dset (*template*, anat_final, vr_base)

    ldep = ['mask_dset', 'template']
    if lat.check_dep(ap_ssdict, ldep) :
        focusbox = ap_ssdict['main_dset']
        ulay     = ap_ssdict['main_dset']
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_gen_mask2final( ap_ssdict, obase, "va2t", 
                                            "mask2final", ulay, focusbox )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vstat"
    # item    : stats in vol (task FMRI): F-stat (def) and other stim/contrasts
    DO_VSTAT_TASK   = 0

    ldep     = ['stats_dset', 'final_anat']
    ldep2    = ['template']                                # 2ary consid
    alt_ldep = ['stats_dset', 'vr_base_dset']              # elif to ldep
    ldep3    = ['user_stats']                              # 3ary consid
    ldep4    = ['mask_dset']                               # 4ary consid

    if lat.check_dep(ap_ssdict, ldep) :
        DO_VSTAT_TASK = 1
        ulay     = ap_ssdict['main_dset']
        focusbox = ap_ssdict['main_dset']

    elif lat.check_dep(ap_ssdict, alt_ldep) :
        DO_VSTAT_TASK = 1
        ulay     = ap_ssdict['vr_base_dset']
        focusbox = 'AMASK_FOCUS_ULAY' 

    if DO_VSTAT_TASK :
        ### [PT: Jan 20, 2022: changing the way this works now.  There
        ### is a larger set of data chosen for default display
        ### now---later will also add more control for user to specify
        ### items.
        all_vstat_obj = lasd.parse_stats_dset_labels( ap_ssdict['stats_dset'],
                                           user_plabs=iopts.vstat_label_list )
        Nobj = len(all_vstat_obj)

        for ii in range(Nobj):
            # the object to use, and a cleaner version of name
            vso      = all_vstat_obj[ii]
            vsname   = UTIL.rename_label_safely( vso.olay_label, 
                                                 only_hash=True )

            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_vstat_stvol( ap_ssdict, obase, "vstat", 
                                             vsname, ulay, focusbox, vso )
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vstat"
    # item    : corr maps (rest/non-task FMRI): seedbased corr in vol dset
    if not(DO_VSTAT_TASK) and \
       not(lat.check_if_niml_dset(ap_ssdict, 'errts_dset')) :
        # mirror same logic as task (above) for deciding ulay/olay
        DO_VSTAT_SEED_REST = 0

        ldep     = ['errts_dset', 'final_anat']
        ldep2    = ['template']                                # 2ary consid
        alt_ldep = ['errts_dset', 'vr_base_dset']              # elif to ldep
        ldep3    = ['user_stats']                              # 3ary consid
        ldep4    = ['mask_dset']                               # 4ary consid

        if lat.check_dep(ap_ssdict, ldep) :
            DO_VSTAT_SEED_REST = 1
            ulay     = ap_ssdict['main_dset']
            focusbox = ap_ssdict['main_dset']

        elif lat.check_dep(ap_ssdict, alt_ldep) :
            DO_VSTAT_SEED_REST = 1
            ulay     = ap_ssdict['vr_base_dset']
            focusbox = 'AMASK_FOCUS_ULAY' 

        if DO_VSTAT_SEED_REST :
            SEED_FILE = ap_ssdict['abin_dir'] + '/' + 'afni_seeds_per_space.txt'

            if 0 :
                print("This branch will be for a user-entered file. Someday.")
            elif os.path.isfile(SEED_FILE) :
                if lat.check_dep(ap_ssdict, ldep2) :
                    tspace    = lat.get_space_from_dset(ap_ssdict['template'])
                    seed_list = UTIL.read_afni_seed_file(SEED_FILE, 
                                                         only_from_space=tspace)
                    Nseed = len(seed_list)
                else:
                    Nseed = 0
            else:
                Nseed = 0

            # now try a new way to have seed maps, likely when final
            # space is ORIG or a TLRC without predetermined seeds
            if Nseed == 0 :
                # if we haven't found any seeds yet, make 2 (or 1)
                # from within final dset, constrained by mask_dset,
                # if present
                seed_list = lat.set_alternate_seed_locs(ap_ssdict)
                Nseed = len(seed_list)

            # we want to keep seedcorr vols, so make a dir for them in
            # the AP results dir
            if Nseed :
                ap_ssdict['vstat_dir'] = 'vstat_seedcorr'
                vdir = ap_ssdict['vstat_dir']
                if os.path.isdir(vdir) :
                    print("+* Removing and remaking vstat-QC dir:", vdir)
                    cmd    = '''\\rm -rf {}'''.format(vdir)
                    com    = BASE.shell_com(cmd, capture=True)
                    stat   = com.run()
                cmd    = '''\\mkdir -p {}'''.format(vdir)
                com    = BASE.shell_com(cmd, capture=True)
                stat   = com.run()

            for ii in range(Nseed):
                seed  = seed_list[ii]               # obj with nec info
                sname = 'seed_' + seed.roi_label
                obase = 'qc_{:02d}'.format(idx)
                # in this case, we also specify the indices of the ulay
                # and thr volumes in the stats dset-- we intend that this
                # will generalize to viewing not just the F-stat (the
                # default)
                cmd      = lat.apqc_vstat_seedcorr( ap_ssdict, obase, "vstat", 
                                                    sname, ulay, focusbox,
                                                    seed )
                idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : motion (enorm) + outlier fraction

    # first, check if BOTH enorm and outlier dsets are present-- if
    # so, make a combined plot (for space considerations); otherwise,
    # check for the others. An additional constraint-- think only
    # 1dplot.py can do this, so run_style must be 'pythonic'.
    BOTH_ENORM_OUTLIER = 0
    ldep = ['enorm_dset', 'outlier_dset', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) and RUN_STYLE == 'pythonic' :
        BOTH_ENORM_OUTLIER = 1
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_enormoutlr( ap_ssdict,
                                            obase, "mot", "enormoutlr", 
                                            RUN_STYLE )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : motion (enorm), only

    ldep = ['enorm_dset', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) and not(BOTH_ENORM_OUTLIER) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_enorm( ap_ssdict, obase, "mot", "enorm", 
                                       RUN_STYLE, 1600 )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : outlier frac, only

    ldep = ['outlier_dset', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) and not(BOTH_ENORM_OUTLIER) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_outlr( ap_ssdict, obase, "mot", "outlr", 
                                       RUN_STYLE, 1600 )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : motion (VR6)

    # [PT: Nov 1, 2018] update list in ldep var-- much shorter now
    # [PT: Sep 6, 2019] now also include censor bars, if available
    ldep = ['motion_dset', 'nt_orig']  
    if lat.check_dep(ap_ssdict, ldep) :
        HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
        ban      = lat.bannerize(' volreg motion pars, and censoring')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_VR6( ap_ssdict, obase, "mot", "VR6", 
                                     RUN_STYLE, 1600 )

        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : grayplot of errts (task, rest, naturalistic, etc.)

    # [PT: Feb 25, 2019] 
    ldep  = ['errts_dset', 'mask_dset']
    ldep2 = ['enorm_dset', 'nt_orig']    # [PT: June 27, 2019]
    if not(iopts.do_mot_grayplot) :
        print('++ *turned off* making grayplot of residuals')
        idx     += 1
    elif lat.check_dep(ap_ssdict, ldep) :
        if not(ap_ssdict['errts_dset'].__contains__('.niml.dset')) :
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_mot_grayplot( ap_ssdict, obase, "mot", 
                                              "grayplot", RUN_STYLE )
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mecho"
    # item    : multi-echo processing

    ldep = ['combine_method']
    if lat.check_dep(ap_ssdict, ldep) :
        # ***For now*** just m_tedana checks available
        if ap_ssdict['combine_method'] == 'm_tedana':
            comb_meth = ap_ssdict['combine_method']
            obase     = 'qc_{:02d}'.format(idx)
            cmd       = lat.apqc_mecho_mtedana( ap_ssdict, obase, 
                                                "mecho", "mtedana", 
                                                comb_meth )
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : sum of stims (regressors of interest)

    # [PT: Jan 14, 2019] Stats dset now included in ldep here, because
    # if there is no stats dset or its value is NO_STATS, then there
    # *are no* regressors of interest, so we won't try to plot them.
    # At some point, we will add in different info to put here, though.
    ldep = ['sum_ideal', 'stats_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_ideal( ap_ssdict, obase, "regr", "ideal", 
                                        RUN_STYLE, 1600 )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : indiv stims (regressors of interest)

    # [PT: Jan 14, 2019] Stats dset now included in ldep here, because
    # if there is no stats dset or its value is NO_STATS, then there
    # *are no* regressors of interest, so we won't try to plot them.
    # At some point, we will add in different info to put here, though.
    ldep = ['xmat_stim', 'stats_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('plot X-matrix, but without '
                                 'baseline and motion')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_stims( ap_ssdict, obase, "regr", "stims", 
                                        RUN_STYLE, 1600 )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : degrees of freedom (DF) check-- simple text output

    ldep  = ['xmat_regress']
    if lat.check_dep(ap_ssdict, ldep) :

        ban      = lat.bannerize('check degrees of freedom')
        obase    = 'qc_{:02d}'.format(idx)
        # in this case, we also specify the indices of the ulay and
        # thr volumes in the stats dset-- we intend that this will
        # generalize to viewing not just the F-stat (the default)
        cmd      = lat.apqc_regr_df( ap_ssdict, obase, "regr", "df" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : corr brain:  corr of errts WB mask ave with each voxel

    # Q: make uvar for this?

    ldep     = ['errts_dset', 'final_anat']
    ldep2    = ['template']                                # 2ary consid
    alt_ldep = ['errts_dset', 'vr_base_dset']              # elif to ldep
    ldep3    = ['user_stats']                              # 3ary consid
    ldep4    = ['mask_dset']                               # 4ary consid

    if lat.check_dep(ap_ssdict, ldep) :
        DO_REGR_CORR_ERRTS = 1
        ulay     = ap_ssdict['main_dset']
        focusbox = ap_ssdict['main_dset']
    elif lat.check_dep(ap_ssdict, alt_ldep) :
        DO_REGR_CORR_ERRTS = 1
        ulay     = ap_ssdict['vr_base_dset']
        focusbox = 'AMASK_FOCUS_ULAY' 

    list_corr_brain = glob.glob('corr_brain+*.HEAD')
    if len(list_corr_brain) == 1 and DO_REGR_CORR_ERRTS :
        corr_brain = list_corr_brain[0]

        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_corr_errts( ap_ssdict, obase, "regr", 
                                             "corr_errts",
                                             ulay, focusbox, corr_brain )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr" 
    # item    : TSNR of volreg (r01) dset.  
    # --> NB: This will get UVAR of its own some day!!!
        
    ldep     = ['final_anat']
    alt_ldep = ['vr_base_dset']  # elif to ldep

    HAVE_ULAY = 0
    if lat.check_dep(ap_ssdict, ldep) :
        HAVE_ULAY = 1
        ulay      = ap_ssdict['main_dset']
        focusbox  = ap_ssdict['main_dset']
    elif lat.check_dep(ap_ssdict, alt_ldep) :
        HAVE_ULAY = 1
        ulay      = ap_ssdict['vr_base_dset']
        focusbox  = 'AMASK_FOCUS_ULAY' 

    DO_TSNR_VREG = 0
    tsnr_vreg = glob.glob( 'TSNR*vreg*HEAD' )
    if len(tsnr_vreg) == 1 :
        DO_TSNR_VREG = 1

    if HAVE_ULAY and DO_TSNR_VREG :
        olay     = tsnr_vreg[0]
        descrip  = '(TSNR, from r01 dset after volreg)'
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_tsnr( ap_ssdict, obase, "regr", "tsnr_vreg",
                                       ulay, focusbox, olay,
                                       descrip=descrip )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr" 
    # item    : TSNR of *final* dset

    ldep     = ['tsnr_dset', 'final_anat']
    alt_ldep = ['tsnr_dset', 'vr_base_dset']  # elif to ldep

    DO_TSNR = 0
    if lat.check_dep(ap_ssdict, ldep) :
        DO_TSNR = 1
        ulay      = ap_ssdict['main_dset']
        focusbox  = ap_ssdict['main_dset']
    elif lat.check_dep(ap_ssdict, alt_ldep) :
        DO_TSNR = 1
        ulay      = ap_ssdict['vr_base_dset']
        focusbox  = 'AMASK_FOCUS_ULAY' 

    # currently, tsnr_dset must be volumetric (need to check here, bc
    # sometimes mask+surf blocks are both used)
    if DO_TSNR and lat.is_volumetric(ap_ssdict['tsnr_dset']) :
        olay     = ap_ssdict['tsnr_dset']
        descrip  = '(final TSNR dset)'
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_tsnr( ap_ssdict, obase, "regr", "tsnr_fin",
                                       ulay, focusbox, olay,
                                       descrip=descrip )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : ROI stats for TSNR (final) via compute_ROI_stats.tcsh

    # not currently a uvar, check for known dir name in AP results
    # dir; check for certain 'automatic' stats files that do not have
    # HTML encoding already (or brain in suffix)
    all_fname = glob.glob('tsnr_stats_regress/stats_auto_*.txt')
    for fname in all_fname :
        if fname.endswith('_brain.txt') or fname.endswith('eval_html.txt') :
            _tmp = all_fname.remove(fname)
    all_fname.sort()

    if len(all_fname) :
        ban      = lat.bannerize('check TSNR ROI stats')
        for fname in all_fname:
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_regr_roi_stats( ap_ssdict, obase, fname,
                                                "regr", "roi_tsnr_fin" )
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "rcorr"
    # item    : flag to make radial_correlate images
    # [PT: Feb 23, 2021] moved here, seemed more logical place, 
    # above warns

    ldep = ['have_radcor_dirs']
    if lat.check_dep(ap_ssdict, ldep) :
        all_dir_radcor = sorted(glob.glob("radcor.pb*")) # can have many
        for ii in range(len(all_dir_radcor)):
            rcdir    = all_dir_radcor[ii]
            aaa      = rcdir.split(".")
            rcname   = "rc_" + aaa[2] # to be the label
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_radcor_rcvol( ap_ssdict, obase, "radcor", 
                                              rcname, rcdir, ith_run=ii )
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : correlation warnings in X.xmat.1D

    ldep = ['xmat_regress']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('correlation warnings')
        obase    = 'qc_{:02d}'.format(idx)
        txtfile  = ''
        if ap_ssdict.__contains__('cormat_warn_dset') :
            txtfile = ap_ssdict['cormat_warn_dset']
        cmd      = lat.apqc_warns_xmat( ap_ssdict, obase, "warns", "xmat",
                                        fname = txtfile )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : censor fraction, general

    # apply if there is censoring used
    ldep = ['df_info_dset', 'censor_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_cen_total( ap_ssdict, obase, "warns", 
                                             "cen_total" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : censor fraction, per stimulus

    # use if: there are stim files present, and censoring
    ldep = ['stats_dset', 'censor_dset', 'ss_review_dset', 'xmat_stim']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_cen_stim( ap_ssdict, obase, "warns", 
                                            "cen_stim" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : 3dDeconvolve warnings

    ldep = ['decon_err_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_decon( ap_ssdict, obase, "warns", "decon" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : pre-steady state warnings

    ldep = ['pre_ss_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('pre-steady state warnings')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_press( ap_ssdict, obase, "warns", "press" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : 4095 saturation warnings

    ldep = ['max_4095_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('4095 saturation warnings')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_sat_4095( ap_ssdict, obase, "warns", 
                                            "sat_4095" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : TENT warnings from timing tool

    ldep = ['tent_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_TENT( ap_ssdict, obase, "warns", "TENT" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : flip check from aea

    ldep = ['flip_check_dset', 'flip_guess']
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_flip( ap_ssdict, obase, "warns", "flip" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : variance lines

    ldep = ['vlines_tcat_dir']  
    if lat.check_dep(ap_ssdict, ldep) :
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_vlines( ap_ssdict, obase, "warns", 
                                          "vlines" )
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : ROI stats for TSNR (final) via compute_ROI_stats.tcsh

    # not currently a uvar, check for known dir name in AP results
    # dir; check for certain 'user' stats files that do not have
    # HTML encoding already 
    all_fname = glob.glob('tsnr_stats_regress/stats_user_*.txt')
    for fname in all_fname :
        if fname.endswith('eval_html.txt') :
            _tmp = all_fname.remove(fname)
    all_fname.sort()

    if len(all_fname) :
        ban      = lat.bannerize('check TSNR ROI stats (for warns block)')
        for fname in all_fname:
            # make string tag for this; we know that it starts with
            # 'tsnr_stats_regress/stats_user_' and ends with '.txt',
            # by definition (and not ('eval_html.txt'); so, remove
            # those known prefix and suffix
            flabel   = "roi_tsnr_warns_" 
            if len(fname) > 34 :    flabel+= fname[30:-4]
            else:                   flabel+= str(idx)

            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_regr_roi_stats( ap_ssdict, obase, fname,
                                                "warns", flabel )
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "qsumm"
    # item    : quant output of @ss_review_basic

    # ------- out review basic info
    if 1 :
        ban      = lat.bannerize('ss review basic info')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_qsumm_ssrev( ap_ssdict, obase, "qsumm", 
                                         "ssrev" )
        idx     += 1

    # --------------------------------------------------------------------
    # --------------------------------------------------------------------

    # FINAL STEPS
    ## Not generating a QC block, but other supplementary+useful things

    # cp JSON(s) over to QC_* subdir
    if 1:
        all_json = [iopts.json] # only one at the moment...
        cmd      = lat.apqc_DO_cp_subj_jsons( ap_ssdict, all_json )
        idx     += 1

    # cp @ss_review_basic text file to QC dir, and also make a JSON
    # version of it there; should always be true
    ldep = ['ss_review_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        cmd      = lat.apqc_DO_cp_subj_rev_basic( ap_ssdict )
        idx     += 1

    # echo @ss_review_basic *to terminal*, and then exit with 0
    if 1:
        cmd      = lat.apqc_DO_term_ss_review_basic( ap_ssdict )
        idx     += 1

    # ======================================================================
    # ======================================================================
    # finishing text

    # write out log/history of what has been done (not done by default, to
    # save some time, bc this takes a mini-while)
    if iopts.do_log :
        olog = 'log_apqc_tcsh.txt'
        UTIL.write_afni_com_log(olog)

    # note where we are in the AP results dir
    pwd_res   = os.getcwd()
    qcdir_abs = pwd_res + '/' + ap_ssdict['odir_qc']

    bye_msg = '''
++ Done setting up QC dir: {qcdir_loc}
   To create the APQC HTML, run either this (from any location):      
     
       apqc_make_html.py -qc_dir {qcdir_abs}

   ... or this (from the afni_proc.py results directory):

       apqc_make_html.py -qc_dir {qcdir_loc}

'''.format(qcdir_abs=qcdir_abs, qcdir_loc=ap_ssdict['odir_qc'])

    print( bye_msg )

    # And the end of all our exploring will be to arrive where we started
    os.chdir(pwd_orig)

    sys.exit(0)
