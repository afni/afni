*************************
fat_proc_convert_dcm_anat
*************************

.. _ahelp_fat_proc_convert_dcm_anat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
    
        The purpose of this function is to help convert an anatomical data
        set from DICOM files into a volume.  Ummm, yep, that's about it.
    
        (But it will be done in a way to fit in line with other
        processing, particularly with DTI analysis, so it might not be
        *totally* useless; more options while converting might be added
        over time, as well.)
    
        REQUIRES: AFNI (which should now contain dcm2niix_afni, the
        version of dcm2niix [by C. Rorden] distributed in AFNI).
    
        Ver. 3.3 (PA Taylor, Feb 20, 2018)
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      fat_proc_convert_dcm_anat  \
            -indir    DIR_IN                       \
            -prefix  PPP                           \
            {-workdir WWW}                         \
            {-orient  ORIENT}                      \
            {-no_clean}                            \
            {-reorig_reorient_off}                 \
            {-qc_prefix    QCPREF}                 \
            {-no_cmd_out}                          \
            {-no_qc_view} 
    
      where:
        -indir  DIR_IN  :required input directory; DIR_IN should contain
                         only DICOM files; all will be selected.
    
        -prefix   PPP   :set prefix (and path) for output data; required.
    
        -workdir  WWW   :specify a working directory, which can be removed;
                         (default name = '__WORKING_convert_dcm_anat').
    
        -orient  ORIENT :optional chance to reset orientation of the volume
                         files (default is currently 'RAI').
      -reorig_reorient_off
                        :switch to turn of the nicety of putting (0, 0, 0)
                         at brain's center of mass (-> 'reorigin' calc) and to
                         not reorient data (-> 'reorient' calc).  Could lead
                         to weirdness later on down the road, depending on the
                         data and headers (ergo, not recommended.)
    
      -qc_prefix QCPREF :can set the prefix of the QC image files separately
                         (default is '').
       -no_qc_view      :can turn off generating QC image files (why?)
       -no_cmd_out      :don't save the command line call of this program
                         and the location where it was run (otherwise, it is
                         saved by default in the ODIR/).
    
    -------------------------------------------------------------------------
    
      OUTPUTS: a single anatomical volume in the DIR_OUT.  
               In some cases of anatomical volume acquisition, the DICOMS
               get converted to more than one format of volumetric output
               (one total acquired volume, one centered around the head,
               etc.); these usually have different formats of file name,
               starting with '2*', 'co*' and 'o*'.  Basically, the '2*' is
               chosen for outputting, and the others are stored in a
               subdirectory called DIR_OUT/WWW/.
    
    -------------------------------------------------------------------------
    
      EXAMPLE:
    
        fat_proc_convert_dcm_anat  \
           -indir  "ANAT_DICOMS"                 \
           -orient RAI
        
    -------------------------------------------------------------------------
