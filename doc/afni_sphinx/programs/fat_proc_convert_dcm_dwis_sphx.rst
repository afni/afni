.. _ahelp_fat_proc_convert_dcm_dwis:

*************************
fat_proc_convert_dcm_dwis
*************************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
    
        The purpose of this function is to help convert one or more sets
        of DWIs in DICOM format into 'nicer' volume+grad format. If
        multiple directories of DICOMS exist for a given AP or PA set,
        then those can be combined into a single volume+grad file with a
        single call of this function. Data sets are also converted to have
        a single orientation. For data acquired with dual phase encoding
        (such as AP/PA, or what is often known as 'blip up'/'blip down'),
        separate calls of this function need to be made for each encode
        set; however, one can pass origin information to have the same
        origin across all volumes during recentering.
    
        This program can be used to: convert dicoms to NIFTI (with bvals
        and bvecs); reorient volumetric data; and glue together multiple
        sessions/directories of data (may the user choose what is
        appropriate to combine!).  More functionality could be demanded by
        demanding users.
    
        REQUIRES: AFNI (which should now contain dcm2niix_afni, the
        version of dcm2niix [by C. Rorden] distributed in AFNI).
    
        Ver. 3.3 (PA Taylor, Feb 20, 2018)
    
    -------------------------------------------------------------------------
    
      RUNNING: 
    
        fat_proc_convert_dcm_dwis  \
          -indir   DIR                             \
          -prefix PPP                              \
          {-workdir   WWW}                         \
          {-orient    ORIENT}                      \
          {-origin_xyz X0 Y0 Z0}                   \
          {-flip_x | -flip_y | -flip_z | -no_flip} \
          {-reorig_reorient_off}                   \
          {-no_clean}                              \
          {-qc_prefix    QCPREF}                   \
          {-no_cmd_out}                            \
          {-no_qc_view}                            \
          {-do_movie AGIF|MPEG}
    
      where:
      -indir  DIR       :directory name(s) of (only) DICOM files of the
                         DWI data,designated as having 'AP' phase
                         encoding.  DIR can actually contain a wildcard
                         expression for several directories, if, for
                         example, multiple DWI sets should be glued
                         together into one set.  NOTE: if specifying more
                         than one directory, put double quotes around your
                         entry, like: "file*".
    
        -prefix   PPP   :set prefix (and path) for output data; will be prefix
                         for the *.nii.gz, *.bvec and *.bval files. Required.
    
        -orient  ORIENT :optional chance to reset orientation of the volume
                         files (default is currently 'RAI').
    
        -origin_xyz X0 Y0 Z0
                        :by default, dset will be given an origin value
                         such that the center of mass of the dset is located
                         at (x, y, z) = (0, 0, 0), for niceness's sake.  
                         However, an explicit origin can also be given with 
                         this option (NB: this depends on the orientation of
                         the data, which can various be: explicitly chosen or 
                         set to a default value, or not changed at all).  
                         Three numbers are required to be input (i.e., the 
                         xyz origin values), and this might be useful if, for 
                         example, you've already processed one set of a dual-
                         phase encoded acquisition, so that you can get the 
                         origin from the first and ensure that the second
                         has the same values afterwards, making it easier
                         to overlay the data, should you wish.
      -reorig_reorient_off
                        :switch to turn of the nicety of putting (0, 0, 0)
                         at brain's center of mass (-> 'reorigin' calc) and to
                         not reorient data (-> 'reorient' calc).  Could lead
                         to weirdness later on down the road, depending on the
                         data and headers (ergo, not recommended.)
    
        -flip_x  |
        -flip_y  |
        -flip_z  |
        -no_flip        :use any one of these for the ability to flip grads 
                         while processing with 1dDW_Grad_o_Mat++. (Default is 
                         to not flip; it is not necessary to provide that 
                         "-no_flip" option for no flipping to occur.)
    
       -no_clean        :switch to not remove working directory of intermediate,
                         temporary files (default is to delete it).
    
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
    
    -------------------------------------------------------------------------
    
      OUTPUTS:
    
        For a given phase encoding set, the output files are:
    
            PREFIX.nii.gz     # a NIFTI file with N volumes;
            PREFIX.rvec       # a row-wise (3xN) bvec file of 
                                the (unit-magnitude) gradient orientations;
            PREFIX.bval       # a row-wise (1xN) bval file of the
                                gradient magnitudes;
            PREFIX_matA.dat   # a column-wise (Nx6) AFNI-style matrix file of
                                the (scaled) b-matrix values;
            PREFIX_matT.dat   # a column-wise (Nx6) TORTOISE-style matrix file 
                                of the (scaled) b-matrix values;
            PREFIX_cvec.dat   # a column-wise (Nx3) bvec file of 
                                the (b-magn scaled) gradient orientations;
    
        with the first three meant to mimic the trio of files output by
        dcm2niix_afni, and the rest there for convenience.  
    
    -------------------------------------------------------------------------
    
      EXAMPLE:
        fat_proc_convert_dcm_dwis  \
            -indir  "DWI_DICOMS"                 \
            -prefix  DWI_UNFILT/dwi
            
        or
    
        fat_proc_convert_dcm_dwis  \
            -indir    dwi_ap                 \
            -prefix   DWI_UNFILT/dwi
            -do_movie AGIF
    
    -------------------------------------------------------------------------
