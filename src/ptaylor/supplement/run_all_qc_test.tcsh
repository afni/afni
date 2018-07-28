#!/bin/tcsh

# ------- alignment checks --------

tcsh -ef do_align_check.tcsh                      \
     anat_final.*+tlrc.HEAD                       \
     final_epi_vr_base_min_outlier+tlrc.HEAD      \
     anatf_epif_edge

tcsh -ef do_align_check.tcsh                      \
     ~/REF_TEMPLATES/TT_N27+tlrc.                 \
     anat_final.*+tlrc.HEAD                       \
     TTN27_anatf_edge

# ------- masking checks --------

tcsh -ef do_mask_check.tcsh                       \
     final_epi_vr_base_min_outlier+tlrc.          \
     full_mask.*+tlrc.HEAD                        \
     epif_fullmask

tcsh -ef do_mask_check.tcsh                       \
     anat_final.*+tlrc.HEAD                       \
     full_mask.*+tlrc.HEAD                        \
     anatf_fullmask

# ------- stat results checks --------

tcsh -ef do_stats_view.tcsh                       \
     anat_final.*+tlrc.HEAD                       \
     stats.*+tlrc.HEAD                            \
     full_mask.*+tlrc.HEAD                        \
     anatf_stats

