#!/usr/bin/env python

import afni_util as UTIL

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
   def __init__(self, name, odict, aphelp=0, source='', descrip='',
                header='', trailer=''):
      self.name     = name          # used to refernce example
      self.aphelp   = aphelp        # flag: shown as part of afni_proc.py -help
      self.source   = source        # from AP help, AD6, etc.
      self.descrip  = descrip       # very short description
      self.header   = header        # shown before example (in -help)
      self.trailer  = trailer       # shown after example (in -help)
      # self.keywords = keywords

      self.odict    = odict         # dict of options {opt:[params]}

def populate_examples():
   """only populate the examples array if someone wants it
   """
   global ap_examples

   if len(ap_examples) > 0:
      return

   ap_examples.append( APExample( 'Example 1', aphelp=1,
     source='afni_proc.py -help',
     descrip='Minimum use.',
     header="""
           Provide datasets and stim files (or stim_times files).  Note that a
           dataset suffix (e.g. HEAD) must be used with wildcards, so that
           datasets are not applied twice.  In this case, a stim_file with many
           columns is given, where the script to changes it to stim_times files.
           """,
     trailer='',
     odict = {
       '-dsets'                    : ['epiRT*.HEAD'],
       '-regress_stim_files'       : ['stims.1D'],
       }))

   ap_examples.append( APExample('Example 2', aphelp=1,
     source='afni_proc.py -help',
     descrip='Very simple.',
     header="""
            """,
     trailer='',
     odict = {
       '-subj_id'                  : ['sb23.e2.simple'],
       '-dsets'                    : ["'sb23/epi_r??+orig.HEAD'"],
       '-tcat_remove_first_trs'    : ['3'],
       '-regress_stim_times'       : ["'sb23/stim_files/blk_times.*.1D'"],
       '-regress_basis'            : ["'BLOCK(30,1)'"],
       }))
                                      

   ap_examples.append( APExample( 'Example 11', aphelp=1,
     source='afni_proc.py -help',
     descrip='Resting state analysis (now even more modern :).',
     header="""
         o Yes, censor (outliers and motion) and despike.
         o Align the anatomy and EPI using the lpc+ZZ cost function, rather
           than the default lpc one.  Apply -giant_move, in case the datasets
           do not start off well-aligned.  Include -check_flip for good measure.
         o Register EPI volumes to the one which has the minimum outlier
              fraction (so hopefully the least motion).
         o Use non-linear registration to MNI template (non-linear 2009c).
           * NOTE: prepare for FreeSurfer before running @SSwarper, so that the
                   FS output will stay aligned with the input.
           * This adds a lot of processing time.
           * Let @SSwarper align to template MNI152_2009_template_SSW.nii.gz.
             Then use the resulting datasets in the afni_proc.py command below
             via -tlrc_NL_warped_dsets.
                  @SSwarper -input FT_anat_FSprep.nii  \
                            -subid FT                  \
                            -odir  FT_anat_warped      \
                            -base  MNI152_2009_template_SSW.nii.gz

            - The SS (skull-stripped) can be given via -copy_anat, and the 
              with-skull unifized anatU can be given as a follower.
         o No bandpassing.
         o Use fast ANATICOR method (slightly different from default ANATICOR).
         o Use FreeSurfer segmentation for:
             - regression of first 3 principal components of lateral ventricles
             - ANATICOR white matter mask (for local white matter regression)
           * For details on how these masks were created, see "FREESURFER NOTE"
             in the help, as it refers to this "Example 11".
         o Input anat was prepared for and given to FreeSurfer, so it should be
           aligned with the FS results and masks.
             - output from FS is usually not quite aligned with input
             - run "3dZeropad -pad2evens" and 3dAllineate before FreeSurfer
             - check anat input to FS using check_dset_for_fs.py
         o Erode FS white matter and ventricle masks before application.
         o Bring along FreeSurfer parcellation datasets:
             - aaseg : NN interpolated onto the anatomical grid
             - aeseg : NN interpolated onto the EPI        grid
           * These 'aseg' follower datasets are just for visualization,
             they are not actually required for the analysis.
         o Compute average correlation volumes of the errts against the
           the gray matter (aeseg) and ventricle (FSVent) masks.

           Note: it might be reasonable to use either set of blur estimates
                 here (from epits or errts).  The epits (uncleaned) dataset
                 has all of the noise (though what should be considered noise
                 in this context is not clear), while the errts is motion
                 censored.  For consistency in resting state, it would be
                 reasonable to stick with epits.  They will likely be almost
                 identical.
            """,
     trailer='',
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
       '-dsets'                    : ["'FT_epi_r?+orig.HEAD'"],
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

   ap_examples.append( APExample( 'Example 13', aphelp=1,
     source='afni_proc.py -help',
     descrip='Complicated ME, surface-based resting state example.',
     header="""
         Key aspects of this example:

            - multi-echo data, using "optimally combined" echoes
            - resting state analysis (without band passing)
            - surface analysis
            - blip up/blip down distortion correction
            - slice-wise regression of physiological parameters (RETROICOR)
            - ventricle principal component regression (3 PCs)
            - EPI volreg to per-run MIN_OUTLIER, with across-runs allineate
            - QC: @radial_correlate on tcat and volreg block results
            - QC: pythonic html report

            * since this is a surface-based example, the are no tlrc options

         Minor aspects:

            - a FWHM=6mm blur is applied, since blur on surface is TO is size

         Note: lacking good sample data for this example, it is simply faked
               for demonstration (echoes are identical, fake ricor parameters
               are not part of this data tree).

              # use data_dir variable for tracking inputs
              set data_dir = FT
            """,
     trailer='',
     odict = {
       '-subj_id'                  : ['FT.complicated'],
       '-blocks'                   : ['despike', 'ricor', 'tshift', 'align',
                                      'volreg', 'mask', 'combine', 'surf',
                                      'blur', 'scale', 'regress'],
       '-radial_correlate_blocks'  : ['tcat', 'volreg'],
       '-blip_forward_dset'        : ["'FT/FT_epi_r1+orig.HEAD[0]'"],
       '-blip_reverse_dset'        : ["'FT/FT_epi_r1+orig.HEAD[0]'"],
       '-copy_anat'                : ['FT/FT_anat+orig'],
       '-anat_follower_ROI'        : ['FSvent', 'epi', 'FT/SUMA/FT_vent.nii'],
       '-anat_follower_erode'      : ['FSvent'],
       '-regress_ROI_PC'           : ['FSvent', '3'],
       '-regress_ROI_PC_per_run'   : ['FSvent'],
       '-regress_make_corr_vols'   : ['FSvent'],
       '-dsets_me_echo'            : ["'FT/FT_epi_r?+orig.HEAD'"],
       '-dsets_me_echo'            : ["'FT/FT_epi_r?+orig.HEAD'"],
       '-dsets_me_echo'            : ["'FT/FT_epi_r?+orig.HEAD'"],
       '-echo_times'               : ['11', '22.72', '34.44'],
       '-combine_method'           : ['OC'],
       '-tcat_remove_first_trs'    : ['2'],
       '-tshift_interp'            : ['-wsinc9'],
       '-mask_epi_anat'            : ['yes'],
       '-ricor_regs_nfirst'        : ['2'],
       '-ricor_regs'               : ["'FT/fake.slibase.FT.r?.1D'"],
       '-ricor_regress_method'     : ['per-run'],
       '-align_opts_aea'           : ['-cost', 'lpc+ZZ', '-giant_move'],
       '-volreg_align_to'          : ['MIN_OUTLIER'],
       '-volreg_align_e2a'         : [],
       '-volreg_post_vr_allin'     : ['yes'],
       '-volreg_pvra_base_index'   : ['MIN_OUTLIER'],
       '-volreg_warp_final_interp' : ['wsinc5'],
       '-surf_anat'                : ['FT/SUMA/FT_SurfVol.nii'],
       '-surf_spec'                : ["'FT/SUMA/std.141.FT_?h.spec'"],
       '-blur_size'                : ['6'],
       '-regress_censor_motion'    : ['0.2'],
       '-regress_censor_outliers'  : ['0.05'],
       '-regress_motion_per_run'   : [],
       '-regress_apply_mot_types'  : ['demean', 'deriv'],
       '-html_review_style'        : ['pythonic'],
       }))

   ap_examples.append( APExample('s03.ap.surface', aphelp=0,
     source='FT_analysis',
     descrip='',
     header="""
            """,
     trailer='',
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

def display_eg_all(aphelp=1, source='', verb=0):
   """display the examples array if someone wants it
      if aphelp is set, limit the list to those
      if source is set, restrict to matching
      verb: verbosity level: pass on to display_eg_one
   """
   global ap_examples
   for eg in ap_examples:
      # skip what we do not want to show
      if aphelp != eg.aphelp:
         continue
      if source != '' and source != eg.source:
         continue
      display_eg_one(eg, verb=verb)

def display_eg_one(eg, verb=0):
   """display a single example
      verb: verbosity level
        0: only show example
        1: include name, source, descrip
        2: full verbosity: as ap -help: include descrip, header, trailer
           - terminate descrip with ~2~, for sphinxificaiton

      ponder indentation
   """
   nindent = 10
   istr = ' '*nindent

   # in any case, get the actual command from odict
   keys = list(eg.odict.keys())
   clist = ['afni_proc.py']
   for key in keys:
      clist.append('%s %s' % (key, ' '.join(eg.odict[key])))
   cjstr = '%s \\\n    ' % istr
   cmd = istr + cjstr.join(clist)
   cmd = cmd.replace('\n ', '\n %s' % istr)
   print("%s" % UTIL.add_line_wrappers(cmd, maxlen=(78-nindent)))
   print("\n")

if __name__ == '__main__':
   print('** this is not a main module')
   sys.exit(1)

