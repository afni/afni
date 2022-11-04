#!/usr/bin/env python

# python3 status: compatible

#  1.00, 2022-09-03 : adding in Okan's initial tvar listing
#
# ==========================================================================

# initialize some main variables for use in other code
# ... for TORTOISE processing of DWI data, to generate TORTQC HTML

# this file is heavily based on lib_ss_review.py, which is used for
# afni_proc.py's APQC HTML (and more)

g_ss_tvar_fields = [ \
   # field_name,        hint/description,
   #                    example
   ['subj',                          'set subject ID',
                                     'sub-001'],
   ['TORTOISE_ver',                  'set TORTOISE version',
                                     'TORTOISE_V4.0.0'],
   ['TORTOISE_package',              'set TORTOISE package',
                                     'linux5_local'],
   ['settings_file',                 'settings used for subject ',
                                     'settings.dmc'],
   ['log_file',                      'log file',
                                     'log_main.txt'],
   ['nvols',                         'number of volumes',
                                     100],
   ['nshells',                       'number of shells',
                                     3],
   ['tr',                            'time between DWI volumes in ms',
                                     5000],
   ['te',                            'echo time',
                                     80],
   ['orig_data_up',                  'Original up data',
                                     'up.nii'],
   ['orig_data_down',                'Original down data',
                                     'down.nii'],
   ['orig_structural',               'Original structural image',
                                     'structural.nii'],
   ['den_gibbs_data_up',             'Denoised, gibbs ringing corrected up data',
                                     'up_proc.nii'],
   ['den_gibbs_data_down',           'Denoised, gibbs ringing corrected down data',
                                     'down_proc.nii'],
   ['mot_eddy_data_up',              'Inter- and intra- volume motion, eddy-currents corrected and outlier replaced up data',
                                     'up_moteddy.nii'],
   ['mot_eddy_data_down',            'Inter- and intra- volume motion, eddy-currents corrected and outlier replaced down data',
                                     'down_moteddy.nii'],
   ['inter-volume_motion_dset_up',   'set inter volume motion&eddy-currents parameter file for up data',
                                     'up_moteddy_transformations.txt'],
   ['inter-volume_motion_dset_down', 'set inter volume motion&eddy-currents parameter file for down data',
                                     'down_moteddy_transformations.txt'],
   ['intra-volume_motion_dset_up',   'set intra volume motion parameter file for up data',
                                     'up_s2v_transformations.txt'],
   ['intra-volume_motion_dset_down', 'set intra volume motion parameter file for down data',
                                     'down_s2v_transformations.txt'],
   ['slice_residuals_up',            'Slice residuals file between predicted and actual for up data',
                                     'up_slice_resids.txt'],
   ['slice_residuals_down',          'Slice residuals file between predicted and actual for down data',
                                     'down_slice_resids.txt'],
   ['slice_residuals_Z_up',          'Z-scores (normalized with median and MAD) of the log of slice RMS for up data',
                                     'up_slice_resids_Z.txt'],
   ['slice_residuals_Z_down',        'Z-scores (normalized with median and MAD) of the log of slice RMS for up data',
                                     'down_slice_resids_Z.txt'],
   ['native_inclusion_map_up',       'Image indicating the voxelwise inliers and outliers for up data',
                                     'up_native_inclusion.nii'],
   ['native_inclusion_map_down',     'Image indicating the voxelwise inliers and outliers for down data',
                                     'down_native_inclusion.nii'],
   ['signal_drift_up',               'Signal drift file for up data',
                                     'up_moteddy_drift.txt'],
   ['signal_drift_down',             'Signal drift file for up data',
                                     'down_moteddy_drift.txt'],
   ['b0_epi_up',                     'up b=0 image used for epi distortion correction',
                                     'blip_up_b0_quad.nii'],
   ['b0_epi_down',                   'down b=0 image used for epi distortion correction',
                                     'blip_down_b0_quad.nii'],
   ['b0_epi_corrected',              'epi distortion corrected b=0 image',
                                     'b0_corrected_final.nii'],
   ['gradient_nonlinearity_field',   'gradient_nonlinearity_field',
                                     'field_inv.nii'],
   ['epi_field_up',                  'susceptibility distortion correction field for up data',
                                     'deformation_FINV.nii.gz'],
   ['epi_field_down',                'susceptibility distortion correction field for down data',
                                     'deformation_MINV.nii.gz'],
   ['final_data_up',                 'Final up data',
                                     'up_TORTOISE_final.nii'],
   ['final_data_down',               'Final down data',
                                     'down_TORTOISE_final.nii'],
   ['final_data',                    'Final data',
                                     'up_TORTOISE_final.nii'],
   ['final_structural',              'Final anatomical image',
                                     'structural.nii'],
   ['final_inclusion_map_up',        'Image indicating the voxelwise inliers and outliers for up data in the final space',
                                     'up_final_temp_inc.nii'],
   ['final_inclusion_map_down',      'Image indicating the voxelwise inliers and outliers for down data in the final space',
                                     'down_final_temp_inc.nii']
   ]


def def_ss_tvar_names():
   return [t[0] for t in g_ss_tvar_fields]

if __name__ == '__main__':
   print('** this is not a main program')

