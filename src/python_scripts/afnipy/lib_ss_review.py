#!/usr/bin/env python

# python3 status: compatible


# initialize some main variables for use in other code


# single subject user variable fields
# (for g_uvar_dict and g_eg_uvar in gen_ss_review_scripts.py)
# (and for something in Paul's gen ss html program)
#
# when adding a variable:
#   - possibly add guess_ function and corresponding call to it
#      - probably want to set it in self.uvars
#      - possibly want to set an afni_data element in self.dsets
#   - if it is used in a script, check and apply
#
# to pass to gen_ss_html via json file, it is enough to:
#   - add to this list
#   - pass via gen_ss_review_scripts.py option
#
g_ss_uvar_fields = [ \
   # field_name,        hint/description, 
   #                    example
   ['subj',            'set subject ID', 
                       'FT'],
   ['afni_ver',        'set AFNI version', 
                       'AFNI_18.2.11'],
   ['afni_package',    'set AFNI package', 
                       'macos_10.12_local'],
   ['nt_orig',         'orig num time points (all runs)', 
                       300],
   ['nt_applied',      'num time points after censoring', 
                       280],
   ['tr',              'time between EPI volumes', 
                       2],
   ['rm_trs',          'set number of TRs removed per run', 
                       2],
   ['num_stim',        'set number of main stimulus classes', 
                       2],
   ['tcat_dset',       'set first tcat dataset',
                       'pb00.FT.r01.tcat+orig.HEAD'],
   ['censor_dset',     'set motion_censor file', 
                       'motion_FT_censor.1D'],
   ['enorm_dset',      'set motion_enorm file', 
                       'motion_FT_enorm.1D'],
   ['motion_dset',     'set motion parameter file', 
                       'dfile_rall.1D'],
   ['volreg_dset',     'set first volreg dataset', 
                       'pb02.FT.r01.volreg+tlrc.HEAD'],
   ['outlier_dset',    'set outcount_rall file', 
                       'outcount_rall.1D'],
   ['gcor_dset',       'set gcor_dset file', 
                       'out.gcor.1D'],
   ['mask_corr_dset',  'set anat/EPI correlation file', 
                       'out.mask_ae_dice.txt'],
   ['mask_anat_templ_corr_dset', 'set anat/template correlation file',
                       'out.mask_at_dice.txt'],
   ['combine_method',  'set method for combining multiple echoes', 
                       'OC'],
   ['mot_limit',       'set motion limit (maybe for censoring)', 
                       0.3],
   ['out_limit',       'set outlier limit (maybe for censoring)', 
                       0.1],
   ['xmat_regress',    'set X-matrix file used in regression', 
                       'X.xmat.1D'],
   ['xmat_uncensored', 'if censoring, set un-censored X-matrix',
                       'X.nocensor.xmat.1D'],
   ['xmat_stim',       'stim-only X-matrix', 
                       'X.stim.xmat.1D'],
   ['stats_dset',      'set main output from 3dDeconvolve', 
                       'stats.FT+tlrc.HEAD'],
   ['sum_ideal',       'set 1D file for ideal sum', 
                       'sum_ideal.1D'],
   ['copy_anat',       'original -copy_anat dataset', 
                       'FT_anat+orig.HEAD'],
   ['align_anat',      'anat aligned with orig EPI', 
                       'FT_anat_al_junk+orig.HEAD'],
   ['final_anat',      'anat aligned with stats dataset',
                       'anat_final.FT+tlrc.HEAD'],
   ['vr_base_dset',    'set EPI volreg base dataset',
                       'vr_base_min_outlier+orig.HEAD'],
   ['final_epi_dset',  'set final EPI base dataset',
                       'final_epi_vr_base_min_outlier+tlrc.HEAD'],
   ['final_view',      'set final view of data (orig/tlrc)', 
                       'tlrc'],
   ['template',        'anatomical template', 
                       'TT_N27+tlrc'],
   ['template_warp',   'affine or nonlinear', 
                       'affine'],
   ['have_radcor_dirs','are there any (yes/no)', 
                       'yes'],
   ['flip_check_dset', '-check_flip result dset', 
                       'aea_checkflip_results.txt'],
   ['flip_guess',      'guessed dset flip status', 
                       'NO_FLIP'],
   ['mask_dset',       'set EPI mask', 
                       'full_mask.FT+tlrc.HEAD'],
   ['tsnr_dset',       'set temporal signal to noise dataset', 
                       'TSNR.FT+tlrc.HEAD'],
   ['errts_dset',      'set residual dataset',
                       'errts.FT.fanaticor+tlrc.HEAD'],
   ['ss_review_dset',  'ss_review_basic output file', 
                       'out.ss_review.FT.txt'],
   ['df_info_dset',    'degree of freedom info', 
                       'out.df_info.txt'],
   ['cormat_warn_dset','correlation warns in Xmat', 
                       'out.cormat_warn.txt'],
   ['pre_ss_warn_dset','pre-steady state warnings', 
                       'out.pre_ss_warn.txt'],
   ['tent_warn_dset',  'tent truncation warnings', 
                       'out.tent_warn.txt'],
   ['decon_err_dset',  '3dDeconvolve warnings', 
                       '3dDeconvolve.err'],
   ['dir_suma_spec',   'directory containing surface spec file', 
                       '/home/me/AFNI_data6/FT_analysis/FT/SUMA'],
   ['suma_specs',      'actual spec file names', 
                       ['std.141.FT_lh.spec']],
   ['surf_vol',        'surface volume dset', 
                       'FT.surf_SurfVol_Alnd_Exp+orig.HEAD']
   ]


def def_ss_uvar_names():
   return [u[0] for u in g_ss_uvar_fields]

if __name__ == '__main__':
   print('** this is not a main program')

