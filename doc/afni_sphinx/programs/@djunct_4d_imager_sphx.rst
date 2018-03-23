.. _ahelp_@djunct_4d_imager:

*****************
@djunct_4d_imager
*****************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    # ------------------------------------------------------------------------
    
    The program is useful for viewing the same slice across the 'time'
    dimension of a 4D data set.  It is used in several of FATCAT's
    fat_proc* functions (e.g., viewing DWIs).  But because it seemed so
    useful, it now has some basic help for basic usage by *expert* AFNI
    users!
    
    The program outputs the following in sets of three, one for each spatial
    axis:
    
        + an image of the same central slice across volumes along the time
          axis, with the brightness range constant across volume
          ("*onescl*" images); that is, the same grayscale in each panel
          corresponds to the same numerical value.  
        + an image of the same central slice across volumes along the time
          axis, with the brightness range possibly *varying* for each
          panel across volume ("*sepscl*" images); that is, the grayscale
          value in each panel can (and likely will) correspond to *a
          different* numerical value.  Useful, for example, for checking
          details in DWIs, where the expected scale of values can change
          dramatically across volumes.
        + (with option flag) a movie version of the "onescl" images,
          showing one slice at a time.
        + (with option flag) a movie version of the "sepscl" images,
          showing one slice at a time.
    
    The panel dimensionality in each of the above montage images is
    calculated to be approximately golden ratio-ish.  (with blank panels
    appended as deemed desirable; blank slices are *not* appended to the
    dset, they are just added for the montage visualization).  Slice
    numbers are shown in both the image panels and the movie panels.
    
    This program is a very basic wrapper around @chauffeur_afni.  It is
    useful as a quality control (QC) generator, driving AFNI functionality
    to make images *and* movies of a data set while processing (working in
    a virtual X11 environment using xvfb, so that this generates
    images/movies even on a remote terminal).
    
    written by PA Taylor (NIMH, NIH, USA).
    
    # ========================================================================
    
    -help, -h          :see helpfile (here!)
    -ver               :see version number
    
    -inset     UUU     :ulay dset (required).  Probably 4D (hence the name of 
                        this program...).
    
    -prefix    PPP     :prefix for output files (required).
    
    -do_movie  MTYPE   :specify type of movie file.  Basically, one of two 
                        options:
                          MPEG   AGIF
                        This is optional; by default, a montage of PNG images
                        is created: the same slice across the 4D set.
    
    -no_clean          :by default, the temporary directory made by this
                        program is deleted when finishing.  Use this option
                        to keep the final intermediate files.
    
    # ========================================================================
    
    EXAMPLE:
    
        # 1) Um, just view all the volumes in a DWI acquisition, both as a
        #    montage saved as a PNG file and as an mpeg file.
    
        @djunct_4d_imager                 \
            -inset  MY_DWIs.nii.gz        \
            -prefix PRETTY_DWIS           \
            -do_movie AGIF
    
