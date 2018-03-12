**************
3dZipperZapper
**************

.. _3dZipperZapper:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

     # ------------------------------------------------------------------------
     
     This is a basic program to help highlight problematic volumes in data
     sets, specifically in EPI/DWI data sets with interleaved acquisition.
     
     Intra-volume subject motion can be quite problematic, potentially
     bad-ifying the data values in the volume so much that it is basically
     useless for analysis.  In FMRI analysis, outlier counts might be
     useful to find ensuing badness (e.g., via 3dToutcount). However, with
     DWI data, we might want to find it without aligning the volumes
     (esp. due to the necessarily differing contrasts) and without tensor
     fitting.
     
     *Therefore*, this program will look through axial slices of a data set
     for brightness fluctuations and/or dropout slices.  It will build a
     list of volumes indices that it identifies as bad, and the user can
     then use something like the 'fat_proc_filter_dwis' program after to
     apply the filtration to the volumetric dset *as well as* to any
     accompanying b-value, gradient vector, b-matrix, etc., text files.
     
     The program works by looking for alternating brightness patterns in
     the data (again, specifically in axial slices, so if your data was
     acquired differently, this program ain't for you! (weeellll, some
     tricks with changing header info miiiight be able to work then)).  It
     should be run *before* any processing, particularly alignments or
     unwarping things, because those could change the slice locations.
     Additionally, it has mainly been tested on 3T data of humans; it is
     possible that it will work equally well on 7T or non-humans, but be
     sure to check results carefully in the latter cases (well, *always*
     check your data carefully!).
     
     Note that there is also 'fat_proc_select_vols' program for
     interactively selecting out bad volumes, by looking at a sheet of
     sagittal images from the DWI set.  That might be useful for amending
     or altering the output from this program, if necessary.
     
     written by PA Taylor (started Jan, 2018)
     
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     
     USAGE:
     
         Input: + a 3D+time data set of DWI or EPI volumes,
                + a mask of the brain-ish region.
        
        Output: + a mask of potentially bad slices across the input dset,
                + a 1D (text) file containing a list of the bad volumes,
                + a 1D file of the per-volume parameters used to detect
                  badness,
                + a 1D file of the slices within which calculations were made,
                + a text file with the selector string of *good* volumes
                  in the dset (for easy use with fat_proc_filter_dwis, 
                  for example).
     
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     
     COMMAND: 
      
      3dZipperZapper                                            \
          -input FFF  {-mask MMM}                               \
          -prefix PPP                                           \
          {-min_slice_nvox N}                                   \
          {-min_streak_len L}                                   \
          {-do_out_slice_param}                                 \
          {-no_out_bad_mask}                                    \
          {-no_out_text_vals}                                   \
     
        where:
     
        -input FFF   :input the 3D+time file of DWIs or EPIs.
        -mask MMM    :optional input of a single volume mask file, which 
                      gets applied to the each volume in FFF.  Otherwise,
                      the dataset is assumed to be masked already.
     
        -prefix PPP  :prefix for output file name.  Any volumetric file
                      extension included here (e.g., '.nii.gz') is
                      propagated to any output volumetric dsets.
     
        -min_slice_nvox N
                     :set the minimum number of voxels to be in the mask
                      for a given slice to be included in the calcs. 
                      N must be >0 (and likely much more so, to be useful).
                      Default: use 10 percent of the axial slice's size.
        -min_streak_len L
                     :set the minimum number of slices in a row to look for
                      fluctuations within (def: L=4).  That is, if 'large
                      enough' fluctuations are found in L consecutive slices,
                      then the volume is flagged for motion.  A larger L means
                      that more slices need to vary for a volume to be flagged
                      for 'brightness fluctuations'.  NB: this does parameter
                      setting does not affect the search for dropout slices.
     
        -do_out_slice_param
                     :output the map of slice parameters (not done by
                      default).  Might be of interest for investigating
                      data.  Output file name base will be: PPP_param.
        -no_out_bad_mask
                     :do *not* output the mask of 'bad' slices that shows
                      which volumes are considered bad (is output by
                      default). Output file name base will be: PPP_badmask.
        -no_out_text_vals
                     :do *not* output the 1D files of the slice parameter
                      values (are output by default). The list of slices
                      in the mask (file name: PPP_sli.1D) and the list of
                      values per slice per volume (file name: PPP_param.1D)
                      are output.
     
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     
     EXAMPLE:
     
         1) All types of outputs:
         3dZipperZapper                                    \
             -input AP.nii.gz                              \
             -mask  AP_mask.nii.gz                         \
             -prefix ZZZ.nii.gz                            \
             -do_out_slice_param
     
         2) No volumetric outputs (only if speed/write time is super
            important?):
         3dZipperZapper                                    \
             -input AP.nii.gz                              \
             -mask  AP_mask.nii.gz                         \
             -prefix ZZZ.nii.gz                            \
             -no_out_bad_mask
     
     
     
     # ------------------------------------------------------------------
     
