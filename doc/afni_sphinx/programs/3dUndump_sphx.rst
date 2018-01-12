.. contents:: 
    :depth: 4 

********
3dUndump
********

.. code-block:: none

    Usage: 3dUndump [options] infile ...
    Assembles a 3D dataset from an ASCII list of coordinates and
    (optionally) values.
    
    Options:
    --------
      -prefix ppp  = 'ppp' is the prefix for the output dataset
                       [default = undump].
    
      -master mmm  = 'mmm' is the master dataset, whose geometry
        *OR*           will determine the geometry of the output.
      -dimen I J K = Sets the dimensions of the output dataset to
                       be I by J by K voxels.  (Each I, J, and K
                       must be >= 1.)  This option can be used to
                       create a dataset of a specific size for test
                       purposes, when no suitable master exists.
              ** N.B.: Exactly one of -master or -dimen must be given.
                  ==>> Unless -ROImask is used!
    
      -mask MMM    = This option specifies a mask dataset 'MMM', which
                       will control which voxels are allowed to get
                       values set.  If the mask is present, only
                       voxels that are nonzero in the mask can be
                       set in the new dataset.
                       * A mask can be created from a pre-existing dataset
                         with program 3dAutomask.
                       * Combining a mask with sphere insertion makes
                         a lot of sense (to me, at least).
    
      -datum type  = 'type' determines the voxel data type of the
                       output, which may be byte, short, or float
                       [default = short].
    
      -dval vvv    = 'vvv' is the default value stored in each
                       input voxel that does not have a value
                       supplied in the input file [default = 1].
                     *** For example: '-dval 7' makes all the voxels
                         whose locations (via '-ijk' or '-xyz') are
                         given without a value get the value 7.
    
      -fval fff    = 'fff' is the fill value, used for each voxel
                       in the output dataset that is NOT listed
                       in the input file [default = 0].
                     *** For example: '-fval 1' makes all the completely
                         unspecified voxels get the value 1.
    
      -ijk         = Coordinates in the input file are (i,j,k) index
           *OR*        triples, as might be output by 3dmaskdump.
      -xyz         = Coordinates in the input file are (x,y,z)
                       spatial coordinates, in mm.  If neither
                       -ijk or -xyz is given, the default is -ijk.
              ** N.B.: -xyz can only be used with -master. If -dimen
                       is used to specify the size of the output dataset,
                       (x,y,z) coordinates are not defined (until you
                       use 3drefit to define the spatial structure).
              ** N.B.: Also see the -ROImask option (infra) for another
                       way to specify what voxels in the output dataset
                       get what values:
                       -- with -ROImask, neither -xyz nor -ijk is used.
    
      -srad rrr    = Specifies that a sphere of radius 'rrr' will be
                       filled about each input (x,y,z) or (i,j,k) voxel.
                       If the radius is not given, or is 0, then each
                       input data line sets the value in only one voxel.
                       * If '-master' is used, then 'rrr' is in mm.
                       * If '-dimen' is used, then 'rrr' is in voxels.
                     *** For example: '-srad 7' means put a sphere of
                         radius 7 mm about each input point.
    
      -orient code = Specifies the coordinate order used by -xyz.
                       The code must be 3 letters, one each from the pairs
                       {R,L} {A,P} {I,S}.  The first letter gives the
                       orientation of the x-axis, the second the orientation
                       of the y-axis, the third the z-axis:
                         R = right-to-left         L = left-to-right
                         A = anterior-to-posterior P = posterior-to-anterior
                         I = inferior-to-superior  S = superior-to-inferior
                       If -orient isn't used, then the coordinate order of the
                       -master dataset is used to interpret (x,y,z) inputs.
              ** N.B.: If -dimen is used (which implies -ijk), then the
                       only use of -orient is to specify the axes ordering
                       of the output dataset.  If -master is used instead,
                       the output dataset's axes ordering is the same as the
                       -master dataset's, regardless of -orient.
                    ** You probably don't need this option, and it is kept
                       here only for historical purposes.
    
      -head_only   =  A 'secret' option for creating only the .HEAD file which
                      gets exploited by the AFNI matlab library function
                      New_HEAD.m
    
     -ROImask rrr  =  This option that lets you specify which voxels get what
                      numbers by using a dataset 'rrr', instead of coordinates.
               ==>>** With this method, the input file should have just
                      one number per line (trailing numbers will be ignored).
                   ** Due to the special way that 3dUndump reads input files, you
                      CANNOT specify an input file using the 1D '[subscript]'
                      notation to pick out a single column of a multicolumn
                      file.  Instead, you can do something like
                        1dcat file.1D'[3]' | 3dUndump -ROImask rmask+orig -prefix ppp -
                      where the last '-' says to read from standard input.
                   ** A more complicated example, using an ROI mask dataset 'mmm+orig'
                      to extract average values from a functional dataset, then create
                      a dataset where the values stored are the ROI averages:
                        3dROIstats -mask mmm+orig -1Dformat func+orig'[1]' | \
                         | 1dcat stdin: | 3dUndump -prefix uuu -datum float -ROImask mmm+orig -
                      Again, the final '-' tells 3dUndump to read the values to be
                      stored from standard input (the pipe).
                   ** If the numbers in the input file are fractional (e.g., '1.372'),
                      be sure to use the '-datum float' option -- otherwise, the
                      default output is '-datum short', which will truncate values!
                    * The 'rrr' dataset must be of integer type -- that is,
                      the values inside must be bytes or shorts.  If you don't
                      know, use program 3dinfo to check.
                    * All voxels with value 1 in dataset 'rrr' get the number in
                      the first row of the input file.
                    * All voxels with value 2 in dataset 'rrr' get the number in
                      the second row of the input file.
                    * Et cetera -- all voxels with value 'n' in dataset 'rrr' get
                      the number in the n-th row of the input file.
                    * Zero or negative values in 'rrr' are ignored completely.
                    * The output dataset has the same spatial grid as 'rrr'
                      (i.e., as if '-master rrr' were used).
                    * The following options cannot be used with -ROImask:
                        -dimen  -master  -mask  -dval
                        -ijk    -xyz     -srad  -orient  -head_only
                    * This option was added 09 Nov 2011:
                      -- Happy 280th Birthday to Benjamin Banneker!
                      -- http://en.wikipedia.org/wiki/Benjamin_Banneker
    
    Input File Format:
    ------------------
     The input file(s) are ASCII files, with one voxel specification per
     line.  A voxel specification is 3 numbers (-ijk or -xyz coordinates),
     with an optional 4th number giving the voxel value.  For example:
    
       1 2 3 
       3 2 1 5
       5.3 6.2 3.7
       // this line illustrates a comment
    
     The first line puts a voxel (with value given by '-dval') at point
     (1,2,3).  The second line puts a voxel (with value 5) at point (3,2,1).
     The third line puts a voxel (with value given by '-dval') at point
     (5.3,6.2,3.7).  If -ijk is in effect, and fractional coordinates
     are given, they will be rounded to the nearest integers; for example,
     the third line would be equivalent to (i,j,k) = (5,6,4).
    
    Notes:
    ------
    * This program creates a 1 sub-brick file.  You can 'glue' multiple
       files together using 3dbucket or 3dTcat to make multi-brick datasets.
      *** At this time, 3dUndump cannot create a multi-brick dataset :-(
    
    * If one input filename is '-', then stdin will be used for input.
       This feature is for the intrepid Unix user who wants to pipe the
       input into 3dUndump from another program.
    
    * If no input files are given, an 'empty' dataset is created.
       For example, to create an all zero 3D dataset with 1 million voxels:
            3dUndump -dimen 100 100 100 -prefix AllZeroAFNI
            3dUndump -dimen 100 100 100 -prefix AllZeroNIFTI.nii
      *** This is probably the simplest way to create an all zero dataset
          with given grid dimensions in AFNI, without any pre-existing
          'master' dataset to start with.  If you want to further change
          the voxel sizes (in mm), you can use 3drefit to alter such
          parameters after the initial act of creation ab nihilo.
      *** You can combine 3dUndump with 3dcalc to create an all zero
          3D+time dataset from 'thin air', as in
            3dUndump -dimen 128 128 32 -prefix AllZero_A -datum float
            3dcalc -a AllZero_A+orig -b '1D: 100@' -expr 0 -prefix AllZero_B
          If you replace the '0' expression with 'gran(0,1)', you'd get a
          random 3D+time dataset, which might be useful for testing purposes.
    
    * By default, the output dataset is of type '-fim', unless the -master
       dataset is an anat type. You can change the output type later using 3drefit.
    
    * You could use program 1dcat to extract specific columns from a
       multi-column rectangular file (e.g., to get a specific sub-brick
       from the output of 3dmaskdump), and use the output of 1dcat as input
       to this program.  If you know what you are doing, that is.
    
    * [19 Feb 2004] The -mask and -srad options were added this day.
       Also, a fifth value on an input line, if present, is taken as a
       sphere radius to be used for that input point only.  Thus, input
          3.3 4.4 5.5 6.6 7.7
       means to put the value 6.6 into a sphere of radius 7.7 mm centered
       about (x,y,z)=(3.3,4.4,5.5).
    
    * [10 Nov 2008] Commas (',') inside an input line are converted to
       spaces (' ') before the line is interpreted.  This feature is for
       convenience for people writing files in CSV (Comma Separated Values)
       format.
       ++ [14 Feb 2010] Semicolons (';') and colons (':') are now changed
            to blanks, as well.  In addition, any line that starts with
            an alphabetic character, or with '#' or '/' will be skipped
            (presumably it is some kind of comment).
    
    * [31 Dec 2008] Inputs of 'NaN' are explicitly converted to zero, and
      a warning message is printed.  AFNI programs do not like with NaN
      floating point values!
    
    -- RWCox -- October 2000
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
