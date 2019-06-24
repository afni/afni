#!/bin/tcsh

#:TITLE: Examples of things with ROIs: resampling and clusterizing

cat <<TEXTINTRO

blah

TEXTINTRO

# AFNI demo: part of afni11*pdf talk about ROIs
#
# + last update: May 19, 2019
#
##########################################################################

# This script is meant to be run in the following directory of the
# AFNI Bootcamp demo data:
#     AFNI_data6/roi_demo

#:SECTION: Resampling (regridding) ROIs

cat <<TEXTBLOCK

    We start with an ROI in original anatomical (=high res) space.

    We then resample the data, using the ``NN`` resampling mode to
    preserve integer values.

TEXTBLOCK

# Change grid/resolution of data set from (high res) anat to 
# (low res) EPI
3dresample                   \
    -overwrite \
    -master rall_vr+orig     \
    -prefix anat_roi_resam   \
    -inset anat_roi+orig     \
    -rmode NN

#:HIDE_ON:

# image before resampling
set cent_of_mass = `3dCM anat_roi+orig`
 
@chauffeur_afni                             \
    -ulay  anat+orig                        \
    -olay  anat_roi+orig                    \
    -ulay_range 0% 120%                     \
    -cbar ROI_i64                           \
    -set_dicom_xyz ${cent_of_mass}          \
    -opacity 9                              \
    -prefix   TEST                          \
    -montx 1 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

@chauffeur_afni                             \
    -ulay  rall_vr+orig                     \
    -olay  anat_roi_resam+orig              \
    -ulay_range 0% 110%                     \
    -cbar ROI_i64                           \
    -set_dicom_xyz ${cent_of_mass}          \
    -set_subbricks 0 0 0                    \
    -opacity 9                              \
    -blowup 4                               \
    -prefix   TEST2                         \
    -montx 1 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

#:HIDE_OFF:

cat <<TEXTBLOCK

    #:IMAGE: Original ROI ||  Resampled ROI
        TEST.sag.png TEST2.sag.png
    #:IMCAPTION: sagittal view of ROI on anat

TEXTBLOCK

#:SECTION: Using the ROIs to calculate quantities

# Calculate average value of the voxels in the ROI, per time point,
# and dump the result to a text file (via the redirect '>' operator;
# otherwise, results are just shown in the terminal).
3dmaskave                      \
    -mask anat_roi_resam+orig  \
    -quiet                     \
    rall_vr+orig > epi_avg.1D

cat <<TEXTBLOCK

Ways to view the output time series, as numbers and/or plots:

TEXTBLOCK

# ... and view dumped text file results, using either of these
cat epi_avg.1D
#1dplot -yaxis 1000:1200:2:1 epi_avg.1D
1dplot -jpg TEST3 -yaxis 1000:1200:2:1 epi_avg.1D
# ... and, if you have Python + matplotlib
1dplot.py -prefix TEST3b.jpg -yaxis 1000:1200   -infiles epi_avg.1D

cat <<TEXTBLOCK

    #:INCLUDE: epi_avg.1D
        :end-line: 10

    #:IMAGE: 1dplot version ||  1dplot.py version
        TEST3.jpg TEST3b.jpg
    #:IMCAPTION: viewing the 3dmaskave time series

TEXTBLOCK


# Dump *values* of a dset that are within a mask region into a 
# text file; no coordinate locations saved in this case, but could 
# save the coordinates of each voxel, too.
3dmaskdump                                \
    -noijk                                \
    -mask anat_roi_resam+orig             \
    func_slim+orig'[2]' > Vrel-tstats.txt

# ... and view dumped text file results
cat Vrel-tstats.txt

cat <<TEXTBLOCK

    #:INCLUDE: Vrel-tstats.txt
        :end-line: 10

TEXTBLOCK

# Compute separate statistics for each ROI in a volume
3dROIstats                  \
    -mask 3rois+orig        \
    func_slim+orig'[0]'     \
    > stats_3rois.txt

# ... and view dumped text file results
cat stats_3rois.txt

cat <<TEXTBLOCK

    #:INCLUDE: stats_3rois.txt

TEXTBLOCK

#:SECTION: Clusterizing: viewing and getting ROI info

# Clusterizing example, using labels to specify stat and effect
# estimate volumes, and using p-value to specify voxelwise threshold.
3dClusterize                   \
    -overwrite                 \
    -1Dformat                  \
    -inset func_slim+orig      \
    -idat 'Vrel#0_Coef'          \
    -ithr 'Vrel#0_Tstat'         \
    -NN 2                      \
    -clust_nvox 200            \
    -bisided p=0.0001          \
    -pref_map Clust_map.nii.gz \
    -pref_dat Clust_dat.nii.gz \
    > Clust_report.1D

#:HIDE_ON:

@chauffeur_afni                             \
    -ulay  anat+orig                        \
    -olay  Clust_dat.nii.gz                 \
    -box_focus_slices AMASK_FOCUS_OLAY      \
    -cbar Reds_and_Blues_Inv                \
    -ulay_range 0% 110%                     \
    -func_range 3                           \
    -opacity 6                              \
    -prefix   TEST4                         \
    -montx 7 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

@chauffeur_afni                             \
    -ulay  anat+orig                        \
    -olay  Clust_map.nii.gz                 \
    -box_focus_slices AMASK_FOCUS_OLAY      \
    -cbar ROI_glasbey_256                   \
    -ulay_range 0% 110%                     \
    -pbar_posonly                           \
    -opacity 7                              \
    -prefix   TEST5                         \
    -montx 7 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

#:HIDE_OFF
    
cat <<TEXTBLOCK


    #:IMAGE: Betas and ROI maps after clusterizing
        TEST4.axi.png 
        TEST5.axi.png
    #:IMCAPTION: 


    #:INCLUDE: Clust_report.1D

TEXTBLOCK


# ... and use the center of mass (CM) locations of clusters for
# identification in loaded atlases with 'whereami'
whereami                                  \
   -tab                                   \
   -coord_file Clust_report.1D'[1,2,3]'   \
   > Clust_whereami_CM.1D

# ... AND we could use the map of ROIs to find out locations, based on
# overlaps in loaded atlases, also with whereami
whereami                                  \
   -omask Clust_map.nii.gz                  \
   > Clust_whereami_olap.1D

# Find suprathreshold voxels, and make into clusters (kinda like GUI's
# Clusterize functionality and/or 3dclust)
3dmerge                       \
    -overwrite \
    -prefix func_roi          \
    -dxyz=1                   \
    -1clip 99.0               \
    -1clust_order 1 200       \
    func_slim+orig'[0]'
