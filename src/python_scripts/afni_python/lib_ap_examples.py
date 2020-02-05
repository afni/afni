#!/usr/bin/env python

# ----------------------------------------------------------------------
# This is a library for storing basic infomation regarding options and
# examples for afni_proc.py.
#
#    - examples are stored as individual dictionaries
#    - examples from afni_proc.py -help are created by:
#       - set noglob
#       - add '-optlist_show_argv_array dict' to command
#
#    - for a given example, want:
#       - name (e.g. Example 11) - want case insensitive checking
#       - source (from help, from AFNI_data6, etc.)
#       - keywords?  too hard to match
#       - description ()
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# main array of APExample instances
ap_examples = []

# ----------------------------------------------------------------------
# class definition for instances in ap_examples array
class APExample:
   def __init__(self, name, odict, aphelp=0, source='', descrip=''):
      self.name     = name          # used to refernce example
      self.aphelp   = aphelp        # flag: shown as part of afni_proc.py -help
      self.source   = source        # from AP help, AD6, etc.
      self.descrip  = descrip       
      # self.keywords = keywords

      self.odict    = odict         # dict of options {opt:[params]}

def populate_examples():
   """only populate the examples array if someone wants it
   """
   global ap_examples

   ap_examples.append( APExample( 'Example 1', aphelp=1,
     source='afni_proc.py -help',
     descrip='',
     odict = {
       '-dsets'                    : ['epiRT*.HEAD'],
       '-regress_stim_files'       : ['stims.1D'],
       }))

   ap_examples.append( APExample('Example 2', aphelp=1,
     source='afni_proc.py -help',
     descrip='',
     odict = {
       '-subj_id'                  : ['sb23.e2.simple'],
       '-dsets'                    : ['sb23/epi_r??+orig.HEAD'],
       '-tcat_remove_first_trs'    : ['3'],
       '-regress_stim_times'       : ['sb23/stim_files/blk_times.*.1D'],
       '-regress_basis'            : ['BLOCK(30,1)'],
       }))

   ap_examples.append( APExample( 'Example 11', aphelp=1,
     source='afni_proc.py -help',
     descrip='',
     odict = {
       '-subj_id'                  : ['FT.11.rest'],
       '-blocks'                   : ['despike', 'tshift', 'align', 'tlrc',
                                      'volreg', 'blur', 'mask', 'scale',
                                      'regress'],
       '-copy_anat'                : ['anatSS.FT.nii'],
       '-anat_has_skull'           : ['no'],
       '-anat_follower'            : ['anat_w_skull', 'anat', 'anatU.FT.nii'],
       '-anat_follower_ROI'        : ['aaseg','anat','aparc.a2009s+aseg.nii'],
       '-anat_follower_ROI'        : ['aeseg','epi', 'aparc.a2009s+aseg.nii'],
       '-anat_follower_ROI'        : ['FSvent','epi', 'fs_ap_latvent.nii.gz'],
       '-anat_follower_ROI'        : ['FSWe', 'epi', 'fs_ap_wm.nii.gz'],
       '-anat_follower_erode'      : ['FSvent', 'FSWe'],
       '-dsets'                    : ['FT_epi_r?+orig.HEAD'],
       '-tcat_remove_first_trs'    : ['2'],
       '-align_opts_aea'           : ['-cost', 'lpc+ZZ', '-giant_move',
                                      '-check_flip'],
       '-tlrc_base'                : ['MNI152_2009_template_SSW.nii.gz'],
       '-tlrc_NL_warp'             : [],
       '-tlrc_NL_warped_dsets'     : ['anatQQ.FT.nii', 'anatQQ.FT.aff12.1D',
                                      'anatQQ.FT_WARP.nii'],
       '-volreg_align_to'          : ['MIN_OUTLIER'],
       '-volreg_align_e2a'         : [],
       '-volreg_tlrc_warp'         : [],
       '-blur_size'                : ['4'],
       '-mask_epi_anat'            : ['yes'],
       '-regress_motion_per_run'   : [],
       '-regress_ROI_PC'           : ['FSvent', '3'],
       '-regress_ROI_PC_per_run'   : ['FSvent'],
       '-regress_make_corr_vols'   : ['aeseg', 'FSvent'],
       '-regress_anaticor_fast'    : [],
       '-regress_anaticor_label'   : ['FSWe'],
       '-regress_censor_motion'    : ['0.2'],
       '-regress_censor_outliers'  : ['0.05'],
       '-regress_apply_mot_types'  : ['demean', 'deriv'],
       '-regress_est_blur_epits'   : [],
       '-regress_est_blur_errts'   : [],
       '-html_review_style'        : ['pythonic'],
       }))

   ap_examples.append( APExample('s03.ap.surface', aphelp=0,
     source='FT_analysis',
     descrip='',
     odict = {
       '-subj_id'                  : ['FT.surf'],
       '-blocks'                   : ['tshift', 'align', 'volreg', 'surf',
                                      'blur', 'scale', 'regress'],
       '-copy_anat'                : ['FT/FT_anat+orig'],
       '-dsets'                    : ['FT/FT_epi_r?+orig.HEAD'],
       '-surf_anat'                : ['FT/SUMA/FT_SurfVol.nii'],
       '-surf_spec'                : ['FT/SUMA/std.60.FT_?h.spec'],
       '-tcat_remove_first_trs'    : ['2'],
       '-volreg_align_to'          : ['MIN_OUTLIER'],
       '-volreg_align_e2a'         : [],
       '-blur_size'                : ['6'],
       '-regress_stim_times'       : ['FT/AV1_vis.txt', 'FT/AV2_aud.txt'],
       '-regress_stim_labels'      : ['vis', 'aud'],
       '-regress_basis'            : ['BLOCK(20,1)'],
       '-regress_motion_per_run'   : [],
       '-regress_censor_motion'    : ['0.3'],
       '-regress_opts_3dD'         : ['-jobs', '2', '-gltsym', 'SYM: vis -aud',
                                      '-glt_label', '1', 'V-A'],
       }))

   return

if __name__ == '__main__':
   print('** this is not a main module')
   sys.exit(1)

