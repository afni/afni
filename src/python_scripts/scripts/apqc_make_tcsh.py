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
# + [RCR] fixed py 2/3 compatability
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
ver = '3.41' ; date = 'March 12, 2020' 
# [PT] no vstat if 'surf' block was used in AP (-> stats dset is
#      *.niml.dset)
#
#########################################################################

# !!! UPDATE TO HAVE THE no_scan STUFF INPUT!
#  uvars to add in officially still:
#    "anat_orig": "copy_af_anat_w_skull+orig.HEAD",


import sys
import os
import json
import glob
from afnipy import afni_base      as BASE
from afnipy import afni_util      as UTIL
from afnipy import lib_apqc_tcsh  as lat
from afnipy import lib_ss_review  as lssr
from afnipy import lib_apqc_io    as laio
from afnipy import apqc_make_html as amh
from afnipy import apqc_make_html as amh

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

    # define output tcsh script name
    otcsh = iopts.subjdir + '/' + lat.scriptname

    # determines what functions/images are used.
    RUN_STYLE = iopts.revstyle

    # get dictionary form of json
    with open(iopts.json, 'r') as fff:
        ap_ssdict = json.load(fff)    

    # -------------------------------------------------------------------
    # -------------------- start + header -------------------------------

    # start and accummulate from here
    str_FULL = ''

    # probably add some info about: afni ver, etc. here.
    str_header = '''#!/bin/tcsh\n\n'''

    str_FULL+= str_header

    comm = ''' This script is meant to help review single subject results. 
    || ||
    This script should be run from a '.results' directory produced by an
    afni_proc.py processing script.
    || ||
    It generates an HTML file of useful QC information (images + text) of
    the data throughout its processing.  Results for this subject are
    stored in '{0}_{1}' and may be viewed using an standard browser, e.g.: 
    || ||
    ~~~~firefox {0}_{1}/{2}
    || ||
    The script can be re-run.  Variables are defined in the 'Top level'
    section and used throughout, but otherwise the script is modular.
    individual sections can be run with or without modification (e.g.,
    copying them into a new file), as long as the 'Top level' sections are
    all present.

    '''.format( lat.qcbase, ap_ssdict['subj'], amh.ohtml )

    comm     = lat.commentize(comm, padpost=2)
    str_FULL+= comm

    # ------------------------------------------------------------------

    # Top level: definitions from json/dictionary of 'uvars'

    ban_apqc_topvar = lat.bannerize( 'Top level: file names and global vars',
                                     padpost=1 )
    str_apqc_topvar = lat.make_apqc_top_vars( ap_ssdict, all_uvars )

    str_FULL+= ban_apqc_topvar
    str_FULL+= str_apqc_topvar

    # --------------------------------------------------------------------

    # Top level: commands to make dirs for output 

    ban   = lat.bannerize('Top level: make output directory structure')
    cmd   = lat.make_apqc_dirs()

    str_FULL+= ban
    str_FULL+= cmd

    # ------------------------------------------------------------------

    # Top level: find main dset, from descending order of template,
    # anat_final, vr_dset

    ban = lat.bannerize( 'Top level: find main dset',
                                     padpost=1 )
    cmd = lat.apqc_find_main_dset( ap_ssdict, all_uvars )

    str_FULL+= ban
    str_FULL+= cmd

    # --------------------------------------------------------------------

    # Top level: see if template can be found, *if* a template was used.
    # We always have to look, because even the EPI-anat alignment check
    # will be influenced by this-- if there *is* a template, then use its
    # box to define view slices; else, just try to "box in" the final anat.

#    ldep = ['template']
#    if lat.check_dep(ap_ssdict, ldep) :
#        ban      = lat.bannerize('Top level: find a template')
#        cmd      = lat.apqc_find_template( )

#        str_FULL+= ban
#        str_FULL+= cmd

    # --------------------------------------------------------------------

    # [PT: Feb 14, 2020]  
    # Top level: see if there is censoring used in this dset

    ban      = lat.bannerize('Top level: censor info')
    cmd      = lat.apqc_censor_info( ap_ssdict, RUN_STYLE )

    str_FULL+= ban
    str_FULL+= cmd

    # --------------------------------------------------------------------

    # [PT: Feb 14, 2020]  
    # Top level: sundry

    ban      = lat.bannerize('Top level: sundry pieces for multi-use')
    cmd      = lat.apqc_sundry_info( ap_ssdict )

    str_FULL+= ban
    str_FULL+= cmd


    # --------------------------------------------------------------------

    ldep  = ['subj']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('title of html page: subj')
        opref    = lat.page_title_json  # here, just the title page name
        cmd      = lat.apqc_Top_pagetop( opref, "Top", "pagetop" )

        str_FULL+= ban
        str_FULL+= cmd

    # --------------------------------------------------------------------
    # --------------------------------------------------------------------
    # --------------------------------------------------------------------

    # --------------------------------------------------------------------
    #
    # The following is a list of images that MAY be made as part of
    # the QC.  These are now organized as QC blocks (see
    # lahh.qc_blocks).  The order that these are listed here are
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

    ### [PT: Dec 21, 2018] will come back to this with a uvars for the
    ### vr_base set
    ldep  = ['vr_base_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        volitem  = "EPI" # because we use same func to plot for both
                         # EPI and anat vols
         # no focus_box necessary here
        ban      = lat.bannerize('{} in orig space'.format(volitem))
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_all( obase, "vorig", volitem, 
                                       ulay_name=ldep[0] )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vorig"
    # item    : anat in orig

    ### [PT: Dec 21, 2018] will come back to this with a uvars for the
    ### vr_base set
    ldep  = ['anat_orig']
    if lat.check_dep(ap_ssdict, ldep) :
        volitem  = "anat" # because we use same func to plot for both
                         # EPI and anat vols
        # no focus_box necessary here
        ban      = lat.bannerize('{} in orig space'.format(volitem))
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_all( obase, "vorig", volitem, 
                                       ulay_name=ldep[0] )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    # --------------------------------------------------------------------

    # QC block: "ve2a"
    # item    : EPI to anat align

    ldep  = ['final_anat', 'final_epi_dset']
    ldep2 = ['template'] # secondary consideration
    if lat.check_dep(ap_ssdict, ldep) :
        focusbox = '${main_dset}'

        ban      = lat.bannerize('EPI and anatomical alignment')
        obase    = 'qc_{:02d}'.format(idx) # will get appended to
        cmd      = lat.apqc_ve2a_epi2anat( obase, "ve2a", "epi2anat", focusbox )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "va2t"
    # item    : anat to template align

    ldep = ['final_anat', 'template']
    if lat.check_dep(ap_ssdict, ldep) :
        focusbox = '${main_dset}'

        ban      = lat.bannerize('anatomical and template alignment')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_va2t_anat2temp( obase, "va2t", "anat2temp", 
                                            focusbox )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vstat"
    # item    : stats in vol (task FMRI): F-stat (def) and other stim/contrasts
    DO_VSTAT_TASK   = 0
    VSTAT_HAVE_MASK = 0

    ldep     = ['stats_dset', 'final_anat']
    ldep2    = ['template']                                # 2ary consid
    alt_ldep = ['stats_dset', 'vr_base_dset']              # elif to ldep
    ldep3    = ['user_stats']                              # 3ary consid
    ldep4    = ['mask_dset']                               # 4ary consid

    if lat.check_dep(ap_ssdict, ldep) :
        DO_VSTAT_TASK = 1
        #ulay = '${final_anat}'
        # [PT: Mar 11, 2020] if the template exists, use *that* as ulay;
        # otherwise, use anat final
        focusbox = '${main_dset}'
        ulay     = '${main_dset}'

    elif lat.check_dep(ap_ssdict, alt_ldep) :
        DO_VSTAT_TASK = 1
        ulay     = '${vr_base_dset}'
        focusbox = 'AMASK_FOCUS_ULAY' # '${vr_base_dset}'

    if DO_VSTAT_TASK :
        all_vstat = ["Full_Fstat"]
        if lat.check_dep(ap_ssdict, ldep3) :
            all_vstat.extend(ap_ssdict[ldep3[0]])
        all_vstat_obj = lat.parse_stats_dset_labels( ap_ssdict['stats_dset'], 
                                                     all_vstat )
        Nobj = len(all_vstat_obj)

        if lat.check_dep(ap_ssdict, ldep4) :
            VSTAT_HAVE_MASK = 1

        for ii in range(Nobj):

            # the object to use, and a cleaner version of name
            vso      = all_vstat_obj[ii]
            vsname   = vso.olay_label.replace('#', '_')

            #vso.disp_olay_all()
            #vso.disp_thr_all()

            ban   = lat.bannerize('view stats (+ eff est): ' + vsname)
            obase = 'qc_{:02d}'.format(idx)
            # in this case, we also specify the indices of the ulay
            # and thr volumes in the stats dset-- we intend that this
            # will generalize to viewing not just the F-stat (the
            # default)
            cmd      = lat.apqc_vstat_stvol( obase, "vstat", vsname, 
                                             ulay, focusbox, vso, ii,
                                             HAVE_MASK=VSTAT_HAVE_MASK )

            str_FULL+= ban
            str_FULL+= cmd
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vstat"
    # item    : corr maps (rest/non-task FMRI): seedbased corr 
    if not(DO_VSTAT_TASK) :               # only done in resting/non-task cases
        # mirror same logic as task (above) for deciding ulay/olay
        DO_VSTAT_SEED_REST = 0
        VSTAT_HAVE_MASK    = 0

        ldep     = ['errts_dset', 'final_anat']
        ldep2    = ['template']                                # 2ary consid
        alt_ldep = ['errts_dset', 'vr_base_dset']              # elif to ldep
        ldep3    = ['user_stats']                              # 3ary consid
        ldep4    = ['mask_dset']                               # 4ary consid

        if lat.check_dep(ap_ssdict, ldep) :
            DO_VSTAT_SEED_REST = 1
            ulay = '${main_dset}'
            focusbox = '${main_dset}'
            #if lat.check_dep(ap_ssdict, ldep2) :
            #    focusbox = '${templ_vol}'
            #else:
            #    focusbox = '${final_anat}'

        elif lat.check_dep(ap_ssdict, alt_ldep) :
            DO_VSTAT_SEED_REST = 1
            ulay     = '${vr_base_dset}'
            focusbox = 'AMASK_FOCUS_ULAY' 

        if DO_VSTAT_SEED_REST :

            abin_dir = lat.get_path_abin()

            SPECIAL_FILE = abin_dir + '/' + 'afni_seeds_per_space.txt'

            if lat.check_dep(ap_ssdict, ldep4) :
                VSTAT_HAVE_MASK = 1

            if 0 :
                print("This branch will be for a user-entered file. Someday.")
            elif os.path.isfile(SPECIAL_FILE) :
                if lat.check_dep(ap_ssdict, ldep2) :
                    tspace    = lat.get_space_from_dset(ap_ssdict['template'])
                    seed_list = UTIL.read_afni_seed_file(SPECIAL_FILE, 
                                                         only_from_space=tspace)
                    Nseed = len(seed_list)
                else:
                    Nseed = 0
            else:
                Nseed = 0

            for ii in range(Nseed):

                seed  = seed_list[ii]               # obj with nec info
                sname = 'seed_' + seed.roi_label

                ban   = lat.bannerize('view seedbased corr: ' + seed.roi_label)
                obase = 'qc_{:02d}'.format(idx)
                # in this case, we also specify the indices of the ulay
                # and thr volumes in the stats dset-- we intend that this
                # will generalize to viewing not just the F-stat (the
                # default)
                cmd      = lat.apqc_vstat_seedcorr( obase, "vstat", sname, 
                                                    ulay, focusbox, seed, 
                                                    ii,
                                                    HAVE_MASK=VSTAT_HAVE_MASK )

                str_FULL+= ban
                str_FULL+= cmd
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
        # additional checks here for possible other uvars to use
        BOTH_ENORM_OUTLIER = 1
        HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
        HAS_mot_limit   = lat.check_dep(ap_ssdict, ['mot_limit'])
        HAS_out_limit   = lat.check_dep(ap_ssdict, ['out_limit'])
        ban      = lat.bannerize('mot enorm plus outlier frac, and censoring')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_enormoutlr( obase, "mot", "enormoutlr", 
                                            RUN_STYLE, 
                                            1600, 
                                            has_cen_dset=HAS_censor_dset,
                                            has_lim_mot=HAS_mot_limit,
                                            has_lim_out=HAS_out_limit )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : motion (enorm), only

    ldep = ['enorm_dset', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) and not(BOTH_ENORM_OUTLIER) :
        # additional checks here for possible other uvars to use
        HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
        HAS_mot_limit   = lat.check_dep(ap_ssdict, ['mot_limit'])
        ban      = lat.bannerize('mot enorm and censoring')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_enorm( obase, "mot", "enorm", RUN_STYLE, 
                                       1600, 
                                       has_cen_dset=HAS_censor_dset,
                                       has_lim=HAS_mot_limit )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "mot"
    # item    : outlier frac, only

    # [PT] no longer checks for 'censor_dset' or 'out_limit' *here*,
    #just later
    ldep = ['outlier_dset', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) and not(BOTH_ENORM_OUTLIER) :
        # additional checks here for possible other uvars to use
        HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
        HAS_out_limit   = lat.check_dep(ap_ssdict, ['out_limit'])
        ban      = lat.bannerize('outlier fraction and censoring')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_mot_outlr( obase, "mot", "outlr", RUN_STYLE, 
                                       1600, 
                                       has_cen_dset=HAS_censor_dset,
                                       has_lim=HAS_out_limit )

        str_FULL+= ban
        str_FULL+= cmd
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
        cmd      = lat.apqc_mot_VR6( obase, "mot", "VR6", RUN_STYLE, 
                                     1600,
                                     has_cen_dset=HAS_censor_dset )

        str_FULL+= ban
        str_FULL+= cmd
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
        HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
        ban      = lat.bannerize('sum of regressors of interest in X-matrix')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_ideal( obase, "regr", "ideal", RUN_STYLE, 
                                        1600, 
                                        has_cen_dset=HAS_censor_dset )

        str_FULL+= ban
        str_FULL+= cmd
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
        # additional checks here for possible other uvars to use
        HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
        ban      = lat.bannerize('plot X-matrix, but without '
                                 'baseline and motion')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_stims( obase, "regr", "stims", RUN_STYLE, 
                                        1600, 
                                        has_cen_dset=HAS_censor_dset )

        str_FULL+= ban
        str_FULL+= cmd
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
        cmd      = lat.apqc_regr_df( obase, "regr", "df" )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : corr brain:  corr of errts WB mask ave with each voxel

    # !!! make uvar for this??

    ldep     = ['errts_dset', 'final_anat']
    ldep2    = ['template']                                # 2ary consid
    alt_ldep = ['errts_dset', 'vr_base_dset']              # elif to ldep
    ldep3    = ['user_stats']                              # 3ary consid
    ldep4    = ['mask_dset']                               # 4ary consid

    if lat.check_dep(ap_ssdict, ldep) :
        DO_REGR_CORR_ERRTS = 1
        ulay     = '${main_dset}' #'${final_anat}'
        focusbox = '${main_dset}'
        #if lat.check_dep(ap_ssdict, ldep2) :
        #    focusbox = '${templ_vol}'
        #else:
        #    focusbox = '${final_anat}'
    elif lat.check_dep(ap_ssdict, alt_ldep) :
        DO_REGR_CORR_ERRTS = 1
        ulay     = '${vr_base_dset}'
        focusbox = 'AMASK_FOCUS_ULAY' 

    #ldep  = ['xmat_regress']
    #if lat.check_dep(ap_ssdict, ldep) :

    list_corr_brain = glob.glob('corr_brain+*.HEAD')
    if len(list_corr_brain) == 1 and DO_REGR_CORR_ERRTS :

        corr_brain = list_corr_brain[0]

        ban      = lat.bannerize('check ave errts corr through brain')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_corr_errts( obase, "regr", "corr_errts",
                                             ulay, focusbox, corr_brain )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr"
    # item    : grayplot of errts (task, rest, naturalistic, etc.)

    # [PT: June 27, 2019] expanding to include enorm, if available and
    # in Pythonic mode

    # [PT: Feb 25, 2019] 
    ldep = ['errts_dset', 'mask_dset']
    ldep2 = ['enorm_dset', 'nt_orig']    # [PT: June 27, 2019]
    if lat.check_dep(ap_ssdict, ldep) :
        # [PT: Jun 18, 2019] special case check-- 
        if not(ap_ssdict['errts_dset'].__contains__('.niml.dset')) :
            HAS_mot_dset  = lat.check_dep(ap_ssdict, ldep2)
            HAS_out_dset    = lat.check_dep(ap_ssdict, ['outlier_dset'])
            HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
            HAS_mot_limit   = lat.check_dep(ap_ssdict, ['mot_limit'])
            HAS_out_limit   = lat.check_dep(ap_ssdict, ['out_limit'])

            ban      = lat.bannerize('make grayplot of residuals')
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_regr_grayplot( obase, "regr", "grayplot",
                                               RUN_STYLE,  
                                               has_mot_dset=HAS_mot_dset,
                                               has_out_dset=HAS_out_dset,
                                               has_mot_lim=HAS_mot_limit,
                                               has_out_lim=HAS_out_limit,
                                               has_cen_dset=HAS_censor_dset )
            str_FULL+= ban
            str_FULL+= cmd
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
        cmd      = lat.apqc_warns_xmat( obase, "warns", "xmat",
                                        fname = txtfile )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : censor fraction, general

    # apply if there is censoring used
    ldep = ['df_info_dset', 'censor_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        # ---------- get df info as json

        df_dict = lat.read_in_txt_to_dict( ap_ssdict['df_info_dset'],
                                           tmp_name='__tmp_df_info.json' )

        ban      = lat.bannerize('Censor fraction (total)')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_cen_total( obase, "warns", "cen_total",
                                             df_dict=df_dict )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : censor fraction, per stimulus

    # use if: there are stim files present, and censoring
    ldep = ['stats_dset', 'censor_dset', 'ss_review_dset', 'xmat_stim']
    if lat.check_dep(ap_ssdict, ldep) :

        # get the ss_review_basic info as a dict
        rev_dict = lat.read_in_txt_to_dict( ap_ssdict['ss_review_dset'],
                                            tmp_name='__tmp_ss_rev.json' )

        cmd = '''1d_tool.py -verb 0 -infile {xmat_stim} -show_labels
        '''.format( **ap_ssdict )
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        all_labels = com.so[0].split() # list of all labels

        ban      = lat.bannerize('Censor fraction (per stim)')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_warns_cen_stim( obase, "warns", "cen_stim",
                                            rev_dict=rev_dict,
                                            label_list=all_labels)

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : pre-steady state warnings

    ldep = ['pre_ss_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('pre-steady state warnings')
        obase    = 'qc_{:02d}'.format(idx)
        txtfile  = ap_ssdict['pre_ss_warn_dset']
        cmd      = lat.apqc_warns_press( obase, "warns", "press",
                                         fname = txtfile)

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : TENT warnings from timing tool

    ldep = ['tent_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('TENT warnings')
        obase    = 'qc_{:02d}'.format(idx)
        txtfile  = ap_ssdict['tent_warn_dset']
        cmd      = lat.apqc_warns_TENT( obase, "warns", "TENT",
                                        fname = txtfile )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "warns"
    # item    : flip check from aea

    ldep = ['flip_check_dset', 'flip_guess']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('check_flip warnings')
        obase    = 'qc_{:02d}'.format(idx)
        txtfile  = ap_ssdict['flip_check_dset']
        cmd      = lat.apqc_warns_flip( obase, "warns", "flip",
                                        fname = txtfile )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "rcorr"
    # item    : flag to make radial_correlate images

    ldep = ['have_radcor_dirs'] # binary flag
    if lat.check_dep(ap_ssdict, ldep) :
        all_dir_radcor = sorted(glob.glob("radcor.pb*")) # can have many
        for ii in range(len(all_dir_radcor)):

            rcdir  = all_dir_radcor[ii]
            aaa    = rcdir.split(".")
            rcname = "rc_" + aaa[2] # to be the label
        
            ban      = lat.bannerize('@radial_correlate '
                                     'images: {}'.format(rcname))
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_radcor_rcvol( obase, "radcor", rcname,
                                              rcdir, ith_run=ii )

            str_FULL+= ban
            str_FULL+= cmd
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "qsumm"
    # item    : quant output of @ss_review_basic

    # ------- out review basic info
    if 1 :
        ban      = lat.bannerize('ss review basic info')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_qsumm_ssrev( obase, "qsumm", "ssrev" )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------
    # --------------------------------------------------------------------

    # FINAL STEPS
    ## Not generating a QC block, but other supplementary+useful things

    # cp JSON(s) over to QC_* subdir
    if 1:
        ban      = lat.bannerize('copy JSONs over to QC dir')
        all_json = [iopts.json] # only one at the moment...
        cmd      = lat.apqc_DO_cp_subj_jsons( all_json )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # echo @ss_review_basic *to terminal*, and then exit with 0
    if 1:
        ban      = lat.bannerize('ss review basic info *to terminal*')
        cmd      = lat.apqc_DO_term_ss_review_basic( )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    if 1:
        ban      = lat.bannerize('Finish gracefully, if possible')
        cmd      = lat.commandize('''exit 0''')

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # ======================================================================
    # ======================================================================

    # write, chmod and finish

    fff = open(otcsh, 'w')
    fff.write(str_FULL)
    fff.close()

    # deal with python 2/3   27 Nov 2018 [rickr]
    try: code = eval('0o755')
    except: code = eval('0755')
    try:
        os.chmod(otcsh, code)
    except:
        omsg = "failed: chmod {} {}".format(code, otcsh)
        print(omsg)

    bye_msg = '''
    ++ Done making (executable) script to generate HTML QC:
    {}
    '''.format(otcsh)

    bye_msg = lat.commandize(bye_msg, ALLEOL=False)
    print( bye_msg )

    sys.exit(0)
