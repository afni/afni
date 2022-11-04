#!/usr/bin/env python

# Make a QC HTML for TORTOISE+AFNI combined processing output
# ... based heavily on apqc_make_tcsh.py

auth = 'PA Taylor'
ver = 1.00 ; date = 'June 28, 2022'
# + start this program
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
from afnipy import lib_atortqc_review  as latr
from afnipy import lib_atortqc_tcsh    as latt
from afnipy import lib_atortqc_io      as latio
from afnipy import lib_format_cmd_str  as lfcs

# all possible tvars
all_tvars = []
for x in latr.g_ss_tvar_fields: 
    all_tvars.append(x[0])

# ===================================================================
# ===================================================================

if __name__ == "__main__":

    iopts = latio.parse_atortqc_tcsh_args(sys.argv[1:])

    # define output tcsh script name
    otcsh = iopts.subjdir + '/' + latt.scriptname

    # determines what functions/images are used.
    RUN_STYLE = iopts.revstyle

    # get dictionary form of json
    with open(iopts.json, 'r') as fff:
        atort_ssdict = json.load(fff)    
    
    # ----------------- initialize some params/switches ----------------

    #DO_REGR_CORR_ERRTS = 0
    #DO_TSNR            = 0
    #HAVE_MASK          = lat.check_dep(ap_ssdict, ['mask_dset'])

    # -------------------------------------------------------------------
    # -------------------- start + header -------------------------------

    # start and accummulate from here
    str_FULL = ''

    # probably add some info about: afni ver, etc. here.
    str_header = '''#!/bin/tcsh\n\n'''

    str_FULL+= str_header

    comm = ''' This script is meant to help review single subject results. 
    || ||
    It generates an HTML file of useful QC information (images + text) of
    the data throughout its processing.  Results for this subject are
    stored in '{0}_{1}' and may be viewed using an standard browser, e.g.: 
    || ||
    ~~~~afni_open -b {0}_{1}/{2}
    || ||
    ~~~~firefox {0}_{1}/{2}
    || ||
    The script can be re-run.  Variables are defined in the 'Top level'
    section and used throughout, but otherwise the script is modular.
    individual sections can be run with or without modification (e.g.,
    copying them into a new file), as long as the 'Top level' sections are
    all present.

    '''.format( latt.qcbase, atort_ssdict['subj'], latt.ohtml)

    comm     = lat.commentize(comm, padpost=2)
    str_FULL+= comm

    # Add in script used to write this script
    #full_argv = ' '.join(['atort_make_tcsh.py'] + sys.argv[1:])
    full_argv = ' '.join([os.path.basename(sys.argv[0])] + sys.argv[1:])
    cmd_log = '''# This script was created with this command:\n'''
    cmd_log+= lfcs.afni_niceify_cmd_str(full_argv, 
                                        comment_start = '#    ',
                                        max_lw = 74)[1]
    str_FULL+= cmd_log + "\n"

    if iopts.pythonic2basic :
        comm_p2b = ''' 
        +* WARNING: The user asked for 'pythonic' ATORTQC, but there are
        missing dependencies.  This ATORTQC run will therefore run a bit
        unhappily.
        || 
        -> To fix: Please check the warnings in out.review_html, but
        likely Matplotlib is missing or has version<2.2.  You can add
        this dependency (verify with 'afni_system_check.py
        -check_all') and redo the ATORTQC pythonically.'''
        comm_p2b     = lat.commentize(comm_p2b, padpre=1, padpost=1)
        str_FULL+= comm_p2b


    # ------------------------------------------------------------------

    # Top level: definitions from json/dictionary of 'tvars'
    # copies what happens in APQC (for now)

    ban_apqc_topvar = lat.bannerize( 'Top level: file names and global vars',
                                     padpost=1 )
    str_apqc_topvar = lat.make_apqc_top_vars( atort_ssdict, all_tvars )

    str_FULL+= ban_apqc_topvar
    str_FULL+= str_apqc_topvar

    # --------------------------------------------------------------------

    # Top level: commands to make dirs for output 
    # copies what happens in APQC (for now)

    ban   = lat.bannerize('Top level: make output directory structure')
    cmd   = lat.make_apqc_dirs()

    str_FULL+= ban
    str_FULL+= cmd

    # ------------------------------------------------------------------

    # --------------------------------------------------------------------

    ldep  = ['subj']
    if lat.check_dep(atort_ssdict, ldep) :
        ban      = lat.bannerize('title of html page: subj')
        opref    = lat.page_title_json  # here, just the title page name
        cmd      = latt.atortqc_Top_pagetop( opref, "Top", "pagetop" )

        str_FULL+= ban
        str_FULL+= cmd

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
    # item    : blip up in orig

    ### 
    ldep  = ['orig_data_up']
    if lat.check_dep(atort_ssdict, ldep)  :
        volitem  = "Up_DWI" # because we use same func to plot for both
                         # EPI and anat vols
         # no focus_box necessary here
        ban      = lat.bannerize('{} in orig space'.format(volitem))
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = latt.apqc_vorig_all_4D( obase, "vorig", volitem, 
                                           ulay_name=ldep[0] )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vorig"
    # item    : blip down in orig

    ldep  = ['orig_data_down']
    if lat.check_dep(atort_ssdict, ldep) :
        volitem  = "Down_DWI" # because we use same func to plot for both
                         # EPI and anat vols
         # no focus_box necessary here
        ban      = lat.bannerize('{} in orig space'.format(volitem))
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = latt.apqc_vorig_all_4D( obase, "vorig", volitem, 
                                           ulay_name=ldep[0] )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vorig"
    # item    : anat in orig

    ldep  = ['orig_structural']
    if lat.check_dep(atort_ssdict, ldep) :
        volitem  = "Structural" # because we use same func to plot for both
                                # DWI and anat vols
         # no focus_box necessary here
        ban      = lat.bannerize('{} in orig space'.format(volitem))
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_all( obase, "vorig", volitem, 
                                       ulay_name=ldep[0] )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    """

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

    ldep  = ['copy_anat']
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

    # QC block: "vorig"
    # item    : init EPI anat overlap

    ldep  = ['vr_base_dset', 'copy_anat']
    if lat.check_dep(ap_ssdict, ldep) :

        # no focus_box necessary here
        ban      = lat.bannerize('initial EPI-anatomical overlap')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_vorig_olap( obase, "vorig", "olap" )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    # --------------------------------------------------------------------

    # QC block: "ve2a"
    # item    : EPI to anat align

    ldep  = ['final_anat', 'final_epi_dset']
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
        ### [PT: Jan 20, 2022: changing the way this works now.  There
        ### is a larger set of data chosen for default display
        ### now---later will also add more control for user to specify
        ### items.
        #all_vstat = ["Full_Fstat"]
        #if lat.check_dep(ap_ssdict, ldep3) :
        #    all_vstat.extend(ap_ssdict[ldep3[0]])
        all_vstat_obj = lasd.parse_stats_dset_labels( ap_ssdict['stats_dset'] )
        Nobj = len(all_vstat_obj)

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
                                             HAVE_MASK=HAVE_MASK )

            str_FULL+= ban
            str_FULL+= cmd
            idx     += 1

    # --------------------------------------------------------------------

    # QC block: "vstat"
    # item    : corr maps (rest/non-task FMRI): seedbased corr 
    if not(DO_VSTAT_TASK) :               # only done in resting/non-task cases
        # mirror same logic as task (above) for deciding ulay/olay
        DO_VSTAT_SEED_REST = 0

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
                                                    HAVE_MASK=HAVE_MASK )

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

    # QC block: "mot"
    # item    : grayplot of errts (task, rest, naturalistic, etc.)

    # [PT: June 27, 2019] expanding to include enorm, if available and
    # in Pythonic mode
    # [PT: Feb 23, 2021] moved here to 'mot' from 'regr'

    # [PT: Feb 25, 2019] 
    ldep  = ['errts_dset', 'mask_dset']
    ldep2 = ['enorm_dset', 'nt_orig']    # [PT: June 27, 2019]
    if not(iopts.do_mot_grayplot) :
        ban = lat.bannerize('*turned off*: make grayplot of residuals')
        str_FULL+= ban
        idx     += 1
    elif lat.check_dep(ap_ssdict, ldep) :
        # [PT: Jun 18, 2019] special case check-- 
        if not(ap_ssdict['errts_dset'].__contains__('.niml.dset')) :
            HAS_mot_dset  = lat.check_dep(ap_ssdict, ldep2)
            HAS_out_dset    = lat.check_dep(ap_ssdict, ['outlier_dset'])
            HAS_censor_dset = lat.check_dep(ap_ssdict, ['censor_dset'])
            HAS_mot_limit   = lat.check_dep(ap_ssdict, ['mot_limit'])
            HAS_out_limit   = lat.check_dep(ap_ssdict, ['out_limit'])

            ban      = lat.bannerize('make grayplot of residuals')
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_mot_grayplot( obase, "mot", "grayplot",
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

    # QC block: "mecho"
    # item    : multi-echo processing

    ldep = ['combine_method']
    if lat.check_dep(ap_ssdict, ldep) :

        # ***For now*** just m_tedana checks available
        if ap_ssdict['combine_method'] == 'm_tedana':
            comb_meth = ap_ssdict['combine_method']

            ban      = lat.bannerize('multi-echo, via m_tedana')
            obase    = 'qc_{:02d}'.format(idx)
            cmd      = lat.apqc_mecho_mtedana( obase, "mecho", "mtedana",
                                               comb_meth )

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
    # item    : TSNR of volreg (r01) dset.  
    # --> NB: This will get UVAR of its own some day!!!
        
    ldep     = ['final_anat']
    alt_ldep = ['vr_base_dset']  # elif to ldep

    HAVE_ULAY = 0
    if lat.check_dep(ap_ssdict, ldep) :
        HAVE_ULAY = 1
        ulay      = '${main_dset}' 
        focusbox  = '${main_dset}'
    elif lat.check_dep(ap_ssdict, alt_ldep) :
        HAVE_ULAY = 1
        ulay      = '${vr_base_dset}'
        focusbox  = 'AMASK_FOCUS_ULAY' 

    DO_TSNR_VREG = 0
    tsnr_vreg = glob.glob( iopts.subjdir + '/' + 'TSNR*vreg*HEAD' )
    if len(tsnr_vreg) == 1 :
        DO_TSNR_VREG = 1

    if HAVE_ULAY and DO_TSNR_VREG :

        print("++ Will calc vreg TSNR.")
        olay     = '( TSNR*vreg*HEAD )'
        descrip  = '(TSNR, from r01 dset after volreg)'

        ban      = lat.bannerize('check vreg (r01) TSNR')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_tsnr( obase, "regr", "tsnr_vreg",
                                       ulay, focusbox, olay,
                                       descrip=descrip,
                                       HAVE_MASK=HAVE_MASK )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "regr" 
    # item    : TSNR of *final* dset

    ldep     = ['tsnr_dset', 'final_anat']
    alt_ldep = ['tsnr_dset', 'vr_base_dset']  # elif to ldep

    if lat.check_dep(ap_ssdict, ldep) :
        DO_TSNR = 1
        ulay     = '${main_dset}' 
        focusbox = '${main_dset}'
    elif lat.check_dep(ap_ssdict, alt_ldep) :
        DO_TSNR = 1
        ulay     = '${vr_base_dset}'
        focusbox = 'AMASK_FOCUS_ULAY' 

    if DO_TSNR :
        olay     = '${tsnr_dset}' 
        descrip  = '(final TSNR dset)'

        ban      = lat.bannerize('check final TSNR')
        obase    = 'qc_{:02d}'.format(idx)
        cmd      = lat.apqc_regr_tsnr( obase, "regr", "tsnr_fin",
                                       ulay, focusbox, olay,
                                       descrip=descrip,
                                       HAVE_MASK=HAVE_MASK )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # --------------------------------------------------------------------

    # QC block: "rcorr"
    # item    : flag to make radial_correlate images
    # [PT: Feb 23, 2021] moved here, seemed more logical place, 
    # above warns

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

    # cp @ss_review_basic text file to QC dir; should always be true
    ldep = ['ss_review_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('copy review basic text file to QC dir')
        cmd      = lat.apqc_DO_cp_subj_rev_basic()

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
"""
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
