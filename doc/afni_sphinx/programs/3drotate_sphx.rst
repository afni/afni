********
3drotate
********

.. _ahelp_3drotate:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3drotate [options] dataset
    Rotates and/or translates all bricks from an AFNI dataset.
    'dataset' may contain a sub-brick selector list.
    
    GENERIC OPTIONS:
      -prefix fname    = Sets the output dataset prefix name to be 'fname'
      -verbose         = Prints out progress reports (to stderr)
    
    OPTIONS TO SPECIFY THE ROTATION/TRANSLATION:
    -------------------------------------------
    *** METHOD 1 = direct specification:
    At most one of these shift options can be used:
      -ashift dx dy dz = Shifts the dataset 'dx' mm in the x-direction, etc.,
                           AFTER rotation.
      -bshift dx dy dz = Shifts the dataset 'dx' mm in the x-direction, etc.,
                           BEFORE rotation.
        The shift distances by default are along the (x,y,z) axes of the dataset
        storage directions (see the output of '3dinfo dataset').  To specify them
        anatomically, you can suffix a distance with one of the symbols
        'R', 'L', 'A', 'P', 'I', and 'S', meaning 'Right', 'Left', 'Anterior',
        'Posterior', 'Inferior', and 'Superior', respectively.
    
      -rotate th1 th2 th3
        Specifies the 3D rotation to be composed of 3 planar rotations:
           1) 'th1' degrees about the 1st axis,           followed by
           2) 'th2' degrees about the (rotated) 2nd axis, followed by
           3) 'th3' degrees about the (doubly rotated) 3rd axis.
        Which axes are used for these rotations is specified by placing
        one of the symbols 'R', 'L', 'A', 'P', 'I', and 'S' at the end
        of each angle (e.g., '10.7A').  These symbols denote rotation
        about the 'Right-to-Left', 'Left-to-Right', 'Anterior-to-Posterior',
        'Posterior-to-Anterior', 'Inferior-to-Superior', and
        'Superior-to-Inferior' axes, respectively.  A positive rotation is
        defined by the right-hand rule.
    
    *** METHOD 2 = copy from output of 3dvolreg:
      -rotparent rset
        Specifies that the rotation and translation should be taken from the
        first 3dvolreg transformation found in the header of dataset 'rset'.
      -gridparent gset
        Specifies that the output dataset of 3drotate should be shifted to
        match the grid of dataset 'gset'.  Can only be used with -rotparent.
        This dataset should be one this is properly aligned with 'rset' when
        overlaid in AFNI.
      * If -rotparent is used, then don't use -matvec, -rotate, or -[ab]shift.
      * If 'gset' has a different number of slices than the input dataset,
        then the output dataset will be zero-padded in the slice direction
        to match 'gset'.
      * These options are intended to be used to align datasets between sessions:
         S1 = SPGR from session 1    E1 = EPI from session 1
         S2 = SPGR from session 2    E2 = EPI from session 2
     3dvolreg -twopass -twodup -base S1+orig -prefix S2reg S2+orig
     3drotate -rotparent S2reg+orig -gridparent E1+orig -prefix E2reg E2+orig
         The result will have E2reg rotated from E2 in the same way that S2reg
         was from S2, and also shifted/padded (as needed) to overlap with E1.
    
    *** METHOD 3 = give the transformation matrix/vector directly:
      -matvec_dicom mfile
      -matvec_order mfile
        Specifies that the rotation and translation should be read from file
        'mfile', which should be in the format
               u11 u12 u13 v1
               u21 u22 u23 v2
               u31 u32 u33 u3
        where each 'uij' and 'vi' is a number.  The 3x3 matrix [uij] is the
        orthogonal matrix of the rotation, and the 3-vector [vi] is the -ashift
        vector of the translation.
    
    *** METHOD 4 = copy the transformation from 3dTagalign:
      -matvec_dset mset
        Specifies that the rotation and translation should be read from
        the .HEAD file of dataset 'mset', which was created by program
        3dTagalign.
      * If -matvec_dicom is used, the matrix and vector are given in Dicom
         coordinate order (+x=L, +y=P, +z=S).  This is the option to use
         if mfile is generated using 3dTagalign -matvec mfile.
      * If -matvec_order is used, the the matrix and vector are given in the
         coordinate order of the dataset axes, whatever they may be.
      * You can't mix -matvec_* options with -rotate and -*shift.
    
    *** METHOD 5 = input rotation+shift parameters from an ASCII file:
      -dfile dname  *OR*  -1Dfile dname
        With these methods, the movement parameters for each sub-brick
        of the input dataset are read from the file 'dname'.  This file
        should consist of columns of numbers in ASCII format.  Six (6)
        numbers are read from each line of the input file.  If the
        '-dfile' option is used, each line of the input should be at
        least 7 numbers, and be of the form
          ignored roll pitch yaw dS dL dP
        If the '-1Dfile' option is used, then each line of the input
        should be at least 6 numbers, and be of the form
          roll pitch yaw dS dL dP
              (These are the forms output by the '-dfile' and
               '-1Dfile' options of program 3dvolreg; see that
               program's -help output for the hideous details.)
        The n-th sub-brick of the input dataset will be transformed
        using the parameters from the n-th line of the dname file.
        If the dname file doesn't contain as many lines as the
        input dataset has sub-bricks, then the last dname line will
        be used for all subsequent sub-bricks.  Excess columns or
        rows will be ignored.
      N.B.: Rotation is always about the center of the volume.
              If the parameters are derived from a 3dvolreg run
              on a dataset with a different center in xyz-space,
              the results may not be what you want!
      N.B.: You can't use -dfile/-1Dfile with -points (infra).
    
    POINTS OPTIONS (instead of datasets):
    ------------------------------------
     -points
     -origin xo yo zo
       These options specify that instead of rotating a dataset, you will
       be rotating a set of (x,y,z) points.  The points are read from stdin.
       * If -origin is given, the point (xo,yo,zo) is used as the center for
         the rotation.
       * If -origin is NOT given, and a dataset is given at the end of the
         command line, then the center of the dataset brick is used as
         (xo,yo,zo).  The dataset will NOT be rotated if -points is given.
       * If -origin is NOT given, and NO dataset is given at the end of the
         command line, then xo=yo=zo=0 is assumed.  You probably don't
         want this.
       * (x,y,z) points are read from stdin as 3 ASCII-formatted numbers per
         line, as in 3dUndump.  Any succeeding numbers on input lines will
         be copied to the output, which will be written to stdout.
       * The input (x,y,z) coordinates are taken in the same order as the
         axes of the input dataset.  If there is no input dataset, then
           negative x = R  positive x = L  }
           negative y = A  positive y = P  } e.g., the DICOM order
           negative z = I  positive z = S  }
         One way to dump some (x,y,z) coordinates from a dataset is:
    
          3dmaskdump -mask something+tlrc -o xyzfilename -noijk
                     '3dcalc( -a dset+tlrc -expr x -datum float )'
                     '3dcalc( -a dset+tlrc -expr y -datum float )'
                     '3dcalc( -a dset+tlrc -expr z -datum float )'
    
         (All of this should be on one command line.)
    ============================================================================
    
    Example: 3drotate -prefix Elvis -bshift 10S 0 0 -rotate 30R 0 0 Sinatra+orig
    
    This will shift the input 10 mm in the superior direction, followed by a 30
    degree rotation about the Right-to-Left axis (i.e., nod the head forward).
    
    ============================================================================
    Algorithm: The rotation+shift is decomposed into 4 1D shearing operations
               (a 3D generalization of Paeth's algorithm).  The interpolation
               (i.e., resampling) method used for these shears can be controlled
               by the following options:
    
     -Fourier = Use a Fourier method (the default: most accurate; slowest).
     -NN      = Use the nearest neighbor method.
     -linear  = Use linear (1st order polynomial) interpolation (least accurate).
     -cubic   = Use the cubic (3rd order) Lagrange polynomial method.
     -quintic = Use the quintic (5th order) Lagrange polynomial method.
     -heptic  = Use the heptic (7th order) Lagrange polynomial method.
    
     -Fourier_nopad = Use the Fourier method WITHOUT padding
                    * If you don't mind - or even want - the wraparound effect
                    * Works best if dataset grid size is a power of 2, possibly
                      times powers of 3 and 5, in all directions being altered.
                    * The main use would seem to be to un-wraparound poorly
                      reconstructed images, by using a shift; for example:
                       3drotate -ashift 30A 0 0 -Fourier_nopad -prefix Anew A+orig
                    * This option is also available in the Nudge Dataset plugin.
    
     -clipit  = Clip results to input brick range [now the default].
     -noclip  = Don't clip results to input brick range.
    
     -zpad n  = Zeropad around the edges by 'n' voxels during rotations
                  (these edge values will be stripped off in the output)
            N.B.: Unlike to3d, in this program '-zpad' adds zeros in
                   all directions.
            N.B.: The environment variable AFNI_ROTA_ZPAD can be used
                   to set a nonzero default value for this parameter.
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
