#!/usr/bin/env python
#
# ver : 1.33 || date: Oct 15, 2018 || auth: PA Taylor
# + add in new uvar vars (nt_orig, etc.)
#
# ver : 1.34 || date: Oct 16, 2018 || auth: PA Taylor
# + all the new template checks and considerations, for new @Find*
# + define "top level"
# + put in $templ_vol checks
#
# ver : 1.34 || date: Oct 17, 2018 || auth: PA Taylor
# + add in more images
# + introduce WARN
# + add header text/descrip
#
#ver = '1.35' ; date = 'Oct 17, 2018' ; auth = 'PA Taylor'
# + add @ss_review_basic output
# + rename dependencies
#
#ver = '1.4' ; date = 'Oct 18, 2018' ; auth = 'PA Taylor'
# + io by argv
#
#ver = '1.5' ; date = 'Oct 18, 2018' ; auth = 'PA Taylor'
# + new I/O, renamed files, newer way of putting title
# + will fail if template was used but can't be found
#
#ver = '1.51' ; date = 'Oct 19, 2018' ; auth = 'PA Taylor'
# + 'exit 0' added
#
ver = '1.6' ; date = 'Nov , 2018' ; auth = 'PA Taylor'
# + [PT] RUN_MODE now formalized through input name; default 'basic'
# + [PT] end with @ss_review_basic echoed to terminal
#
#########################################################################

import sys
import os
import json
import lib_apqc_tcsh  as lat
import lib_ss_review  as lssr
import lib_apqc_io    as laio
import apqc_make_html as amh

# all possible uvars
all_uvars = []
for x in lssr.g_ss_uvar_fields: 
    all_uvars.append(x[0])

# ===================================================================
# ===================================================================

if __name__ == "__main__":

    iopts = laio.parse_tcsh_args(sys.argv[1:])

    # define output tcsh script name
    otcsh = iopts.subjdir + '/' + lat.scriptname

    # determines what functions/images are used.
    RUN_MODE = iopts.revstyle

    # get dictionary form of json
    with open(iopts.json, 'r') as fff:
        ap_ssdict = json.load(fff)

    # -------------------------------------------------------------------
    # -------------------- start + header -------------------------------

    # start and accummulate from here
    str_FULL = ''

    # probably add some info about: afni ver, etc. here.
    str_header = '''#!/bin/tcsh

    '''

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

    # Top level: see if template can be found, *if* a template was used.
    # We always have to look, because even the EPI-anat alignment check
    # will be influenced by this-- if there *is* a template, then use its
    # box to define view slices; else, just try to "box in" the final anat.
    ldep = ['template']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('Top level: find a template')
        cmd      = lat.apqc_find_template( )

        str_FULL+= ban
        str_FULL+= cmd

    # --------------------------------------------------------------------

    ldep  = ['subj']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('title of html page: subj')
        opref    = '_page_title'
        cmd      = lat.apqc_dat_html_title( opref )

        str_FULL+= ban
        str_FULL+= cmd

    # --------------------------------------------------------------------
    # --------------------------------------------------------------------

    idx  = 0     # will see if this is desirable, in reality, for ordering...

    ldep  = ['final_anat', 'final_epi_dset']
    ldep2 = ['template'] # secondary consideration
    if lat.check_dep(ap_ssdict, ldep) :
        if lat.check_dep(ap_ssdict, ldep2) :
            focusbox = '${templ_vol}'
        else:
            focusbox = '${final_anat}'

        ban      = lat.bannerize('EPI and anatomical alignment')
        opref    = 'IMG_{:02d}_vol_align_epi_anat'.format(idx)
        cmd      = lat.apqc_vol_align_epi_anat( opref, focusbox )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['final_anat', 'template']
    if lat.check_dep(ap_ssdict, ldep) :
        focusbox = '${templ_vol}'

        ban      = lat.bannerize('anatomical and template alignment')
        opref    = 'IMG_{:02d}_vol_align_anat_tlrc'.format(idx)
        cmd      = lat.apqc_vol_align_anat_tlrc( opref, focusbox )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep  = ['stats_dset', 'mask_dset', 'final_anat']
    ldep2 = ['template'] # secondary consideration
    if lat.check_dep(ap_ssdict, ldep) :
        if lat.check_dep(ap_ssdict, ldep2) :
            focusbox = '${templ_vol}'
        else:
            focusbox = '${final_anat}'

        ban      = lat.bannerize('view some stats results')
        opref    = 'IMG_{:02d}_vol_check_stats_anat'.format(idx)
        # in this case, we also specify the indices of the ulay and
        # thr volumes in the stats dset-- we intend that this will
        # generalize to viewing not just the F-stat (the default)
        cmd      = lat.apqc_vol_check_stats_anat( opref, focusbox, 0, 0 )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    # [PT: Nov 1, 2018] update list in ldep var-- much shorter now
    ldep = ['motion_dset', 'nt_orig']  
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('outliers and motion')
        opref    = 'IMG_{:02d}_1D_volreg'.format(idx)
        cmd      = lat.apqc_1D_volreg( 1600, opref, RUN_MODE )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['censor_dset', 'outlier_dset', 'out_limit', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('outlier fraction and censoring')
        opref    = 'IMG_{:02d}_1D_cen_out'.format(idx)
        cmd      = lat.apqc_1D_cen_out( 1600, opref, RUN_MODE )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['censor_dset', 'enorm_dset', 'mot_limit', 'nt_orig']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('mot enorm and censoring')
        opref    = 'IMG_{:02d}_1D_enorm_mot'.format(idx)
        cmd      = lat.apqc_1D_motenorm_cen( 1600, opref, RUN_MODE )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['xmat_stim']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('plot X-matrix, but without '
                                 'baseline and motion')
        opref    = 'IMG_{:02d}_1D_xmat_stim'.format(idx)
        cmd      = lat.apqc_1D_xmat_stim(1600, opref, RUN_MODE)

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['sum_ideal']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('sum of non-baseline regressors in X-matrix')
        opref    = 'IMG_{:02d}_1D_sum_ideal'.format(idx)
        cmd      = lat.apqc_1D_sum_ideal(1600, opref, RUN_MODE)

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    # ------- out review basic info
    if 1 :
        ban      = lat.bannerize('ss review basic info')
        opref    = 'TXT_{:02d}_dat_ss_review_basic'.format(idx)
        cmd      = lat.apqc_dat_ss_review_basic( opref )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    # ------- string warning: keep last in HTML converter because of
    # ------- expected variable length per subject

    ldep = ['xmat_regress']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('correlation warnings')
        opref    = 'WARN_{:02d}_dat_cormat_warn'.format(idx)
        cmd      = lat.apqc_dat_cormat_warn( opref )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['pre_ss_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('pre-steady state warnings')
        opref    = 'WARN_{:02d}_dat_pre_ss_warn'.format(idx)
        cmd      = lat.apqc_dat_pre_ss_warn( opref )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    ldep = ['tent_warn_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        ban      = lat.bannerize('TENT warnings')
        opref    = 'WARN_{:02d}_dat_tent_warn'.format(idx)
        cmd      = lat.apqc_dat_tent_warn( opref )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    # -------------------- final steps -------------

    # echo @ss_review_basic *to terminal*, and then exit with 0
    if 1:
        ban      = lat.bannerize('ss review basic info *to terminal*')
        opref    = 'TXT_{:02d}_dat_ss_review_basic'.format(idx)
        cmd      = lat.apqc_term_ss_review_basic( opref )

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1


    if 1:
        ban      = lat.bannerize('Finish gracefully, if possible')
        cmd      = lat.commandize('''exit 0''')

        str_FULL+= ban
        str_FULL+= cmd
        idx     += 1

    # ======================================================================

    # write, chmod and finish

    fff = open(otcsh, 'w')
    fff.write(str_FULL)
    fff.close()

    try:
        os.chmod(otcsh, 0755)
    except OSError, e:
        print e

    bye_msg = '''
    ++ Done making (executable) script to generate HTML QC:
    {}
    '''.format(otcsh)

    print( lat.commandize(bye_msg, ALLEOL=False) )

    sys.exit(0)
