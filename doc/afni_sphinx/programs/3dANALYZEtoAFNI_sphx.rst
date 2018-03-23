.. _ahelp_3dANALYZEtoAFNI:

***************
3dANALYZEtoAFNI
***************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ** DON'T USE THIS PROGRAM!  REALLY!
    USE 3dcopy OR to3d INSTEAD.
    
    IF YOU CHOOSE TO USE IT ANYWAY, PERHAPS
    BECAUSE IT WORKS BETTER ON YOUR 12th
    CENTURY PLANTAGENET ANALYZE FILES,
    ADD THE OPTION -OK TO YOUR COMMAND
    LINE.
    
    Usage: 3dANALYZEtoAFNI [options] file1.hdr file2.hdr ...
    This program constructs a 'volumes' stored AFNI dataset
    from the ANALYZE-75 files file1.img file2.img ....
    In this type of dataset, there is only a .HEAD file; the
    .BRIK file is replaced by the collection of .img files.
    - Other AFNI programs can read (but not write) this type
      of dataset.
    - The advantage of using this type of dataset vs. one created
       with to3d is that you don't have to duplicate the image data
       into a .BRIK file, thus saving disk space.
    - The disadvantage of using 'volumes' for a multi-brick dataset
       is that all the .img files must be kept with the .HEAD file
       if you move the dataset around.
    - The .img files must be in the same directory as the .HEAD file.
    - Note that you put the .hdr files on the command line, but it is
       the .img files that will be named in the .HEAD file.
    - After this program is run, you must keep the .img files with
       the output .HEAD file.  AFNI doesn't need the .hdr files, but
       other programs (e.g., FSL, SPM) will want them as well.
    
    Options:
     -prefix ppp   = Save the dataset with the prefix name 'ppp'.
                      [default='a2a']
     -view vvv     = Save the dataset in the 'vvv' view, where
                      'vvv' is one of 'orig', 'acpc', or 'tlrc'.
                      [default='orig']
    
     -TR ttt       = For multi-volume datasets, create it as a
                      3D+time dataset with TR set to 'ttt'.
     -fbuc         = For multi-volume datasets, create it as a
                      functional bucket dataset.
     -abuc         = For multi-volume datasets, create it as an
                      anatomical bucket dataset.
       ** If more than one ANALYZE file is input, and none of the
           above options is given, the default is as if '-TR 1s'
           was used.
       ** For single volume datasets (1 ANALYZE file input), the
           default is '-abuc'.
    
     -geomparent g = Use the .HEAD file from dataset 'g' to set
                      the geometry of this dataset.
       ** If you don't use -geomparent, then the following options
           can be used to specify the geometry of this dataset:
     -orient code  = Tells the orientation of the 3D volumes.  The code
                      must be 3 letters, one each from the pairs {R,L}
                      {A,P} {I,S}.  The first letter gives the orientation
                      of the x-axis, the second the orientation of the
                      y-axis, the third the z-axis:
                       R = right-to-left         L = left-to-right
                       A = anterior-to-posterior P = posterior-to-anterior
                       I = inferior-to-superior  S = superior-to-inferior
     -zorigin dz   = Puts the center of the 1st slice off at the
                      given distance ('dz' in mm).  This distance
                      is in the direction given by the corresponding
                      letter in the -orient code.  For example,
                        -orient RAI -zorigin 30
                      would set the center of the first slice at
                      30 mm Inferior.
       ** If the above options are NOT used to specify the geometry
           of the dataset, then the default is '-orient RAI', and the
           z origin is set to center the slices about z=0.
    
     It is likely that you will want to patch up the .HEAD file using
     program 3drefit.
    
     -- RWCox - June 2002.
    
    
    ** DON'T USE THIS PROGRAM!  REALLY!
    USE 3dcopy OR to3d INSTEAD.
    
    IF YOU CHOOSE TO USE IT ANYWAY, PERHAPS
    BECAUSE IT WORKS BETTER ON YOUR 12th
    CENTURY PLANTAGENET ANALYZE FILES,
    ADD THE OPTION -OK TO YOUR COMMAND
    LINE.-- KRH - April 2005.
