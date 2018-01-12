.. contents:: 
    :depth: 4 

********************
fat_proc_filter_dwis
********************

.. code-block:: none

    # -----------------------------------------------------------------------
    
        The purpose of this function is to help filter out user-found and
        user-defined bad volumes from DWI data sets.  
    
        If a bad volume is found, then it should be removed from the 4D
        dset, and it also has to be removed from the gradient list and
        the bvalue list.  In addition, if the user is processing DWI data
        that was acquired with two sets of phase encodings for EPI
        distortion correction, then one wants to remove the same volume
        *from both sets*.  This script is designed to help facilitate this
        process in a scriptable manner (the script still has to be run twice,
        but hopefully with easy enough syntax to avoid confusion/bugs).
    
        The user has to input 
    
            1) a 4D volumetric data sets of N DWIs (NAME.nii.gz),
    
            2) and accompanying bvalue/bmatrix/bvector values that they
               want to be parsed; this could be a unit-magn bvec file + a
               file of bvalues, or it could be a single file of scaled
               vector|matrix values.
    
        The output will be in similar format to what was input (i.e., the
        type of bvector|bmatrix files matching what was input), but with a
        different prefix name and/or directory, and everything filtered in
        a consistent manner *hopefully*.
    
        Check out the function "fat_proc_select_vols" for a nice, GUI way
        to select the bad DWIs you want to get rid of and to build a
        selector nicely (courtesy of J. Rajendra).
    
        REQUIRES: AFNI.
    
        Ver. 3.1 (PA Taylor, Sep 04, 2017)
    
    # -----------------------------------------------------------------------
    
      RUNNING: 
    
        fat_proc_filter_dwis  \
            -in_dwi    DDD                     \
            -select   'SSS'                    \
            -prefix   PPP                      \
            {-in_col_matA|-in_col_matT|        \
             -in_col_vec|-in_row_vec} FFF      \
            {-in_bvals BBB}                    \
            {-unit_mag_out}                    \
            {-qc_prefix  QCPREF}               \
            {-no_cmd_out}                      \
            {-no_qc_view}                      \
            {-do_movie AGIF|MPEG}
    
      where:
      -in_dwi  DDD      :name of a 4D file of DWIs (required).
    
      -in_col_matA |
      -in_col_matT |
      -in_col_vec  |
      -in_row_vec  FFF  :one of these options must be used to input 
                         a bvec/bmat file from the gradients. Required.
                         Same type of output file is returned.
    
      -in_bvals BBB     :if the bvec/bmat is a file of unit-magnitude values,
                         then the bvalues can be input, as well (optional).
    
      -select 'SSS'     :a string of indices and index ranges for
                         selecting which volumes/grads/bvals to *keep*.
                         This is done in a generic form of the typical
                         AFNI format, and index counting starts at 0 and
                         the 'last' brick could be specified as '$'.  An
                         example for skipping the index-4 and index-6
                         volumes in a data set: 
                            '0..3,5,7..$' 
                         This string gets applied to the volume, bval|bvec|bmat
                         files for an input set. Required.
                         NB: there are neither square nor curly brackets used
                         here!
                         NB2: Always use the single quotes around the
                         selector expression. 
    
      -prefix    PPP    :output prefix for all the volumes and text files.
                         Required.
    
      -unit_mag_out     :if one wants to prevent an input bvalue file being
                         applied to unit-magnitude gradients|vecs|matrices,
                         or if one just wants to ensure that the output grad
                         information is unit magnitude, use this option.  If
                         this is used with just a vec/matrix file input, then
                         a b-value file will also be output (so b-value info
                         wouldn't be lost at this moment).  Optional.
    
      -qc_prefix QCPREF :can set the prefix of the QC image files separately
                         (default is '').
       -no_qc_view      :can turn off generating QC image files (why?)
       -no_cmd_out      :don't save the command line call of this program
                         and the location where it was run (otherwise, it is
                         saved by default in the ODIR/).
    
        -do_movie AGIF | MPEG
                        :one can use this option with either of the given
                         arguments to output a movie of the newly created
                         dset.  Only those arguments can be used at
                         present.
    
    # -----------------------------------------------------------------------
    
      EXAMPLE (again, note the single apostrophes around the selector!):
    
        fat_proc_filter_dwis  \
            -in_dwi       UNFILT_AP/AP.nii.gz       \
            -in_col_matT  UNFILT_AP/AP_bmatT.dat    \
            -select       '0..5,8,20..$'           \
            -prefix       FILT_AP/AP 
    
    # -----------------------------------------------------------------------
