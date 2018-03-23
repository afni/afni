.. _ahelp_3dvolreg:

********
3dvolreg
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dvolreg [options] dataset
    Registers each 3D sub-brick from the input dataset to the base brick.
    'dataset' may contain a sub-brick selector list.
    
    -->> Also see the script align_epi_anat.py for a more general
         alignment procedure, which does not require that the base
         and source datasets be defined on the same 3D grid.
    -->> Program 3dAllineate can do nonlinear (polynomial) warping in 3D
         to align 2 datasets.  Script @2dwarper.Allin can do nonlinear
         warping in 2D to align 2 datasets on a slice-wise basis
         (no out-of-slice movements; each slice registered separately).
    
    OPTIONS:
      -verbose        Print progress reports.  Use twice for LOTS of output.
      -Fourier        Perform the alignments using Fourier interpolation.
      -heptic         Use heptic polynomial interpolation.
      -quintic        Use quintic polynomical interpolation.
      -cubic          Use cubic polynomial interpolation.
      -linear         Use linear interpolation.
                 -->>   OLD Default = Fourier [slowest and most accurate interpolator]
                 -->>   NEW Default = Heptic [7th order polynomials]
      -clipit         Clips the values in each output sub-brick to be in the same
                        range as the corresponding input volume.
                        The interpolation schemes can produce values outside
                        the input range, which is sometimes annoying.
                        [16 Apr 2002: -clipit is now the default]
      -noclip         Turns off -clipit
      -zpad n         Zeropad around the edges by 'n' voxels during rotations
                        (these edge values will be stripped off in the output)
                  N.B.: Unlike to3d, in this program '-zpad' adds zeros in
                         all directions.
                  N.B.: The environment variable AFNI_ROTA_ZPAD can be used
                         to set a nonzero default value for this parameter.
      -prefix fname   Use 'fname' for the output dataset prefix.
                        The program tries not to overwrite an existing dataset.
                        Default = 'volreg'.
                  N.B.: If the prefix is 'NULL', no output dataset will be written.
    
      -float          Force output dataset to be written in floating point format.
                  N.B.: If the input dataset has scale factors attached to ANY
                        sub-bricks, then the output will always be written in
                        float format!
    
      -base n         Sets the base brick to be the 'n'th sub-brick
                        from the input dataset (indexing starts at 0).
                        Default = 0 (first sub-brick).
      -base 'bset[n]' Sets the base brick to be the 'n'th sub-brick
                        from the dataset specified by 'bset', as in
                           -base 'elvis+orig[4]'
                        The quotes are needed because the '[]' characters
                        are special to the shell.
    
      -dfile dname    Save the motion parameters in file 'dname'.
                        The output is in 9 ASCII formatted columns:
    
                        n  roll  pitch  yaw  dS  dL  dP  rmsold rmsnew
    
               where:   n     = sub-brick index
                        roll  = rotation about the I-S axis }
                        pitch = rotation about the R-L axis } degrees CCW
                        yaw   = rotation about the A-P axis }
                          dS  = displacement in the Superior direction  }
                          dL  = displacement in the Left direction      } mm
                          dP  = displacement in the Posterior direction }
                       rmsold = RMS difference between input brick and base brick
                       rmsnew = RMS difference between output brick and base brick
           N.B.: If the '-dfile' option is not given, the parameters aren't saved.
           N.B.: The motion parameters are those needed to bring the sub-brick
                 back into alignment with the base.  In 3drotate, it is as if
                 the following options were applied to each input sub-brick:
                  -rotate 'roll'I 'pitch'R 'yaw'A  -ashift 'dS'S 'dL'L 'dP'P
    
      -1Dfile ename   Save the motion parameters ONLY in file 'ename'.
                        The output is in 6 ASCII formatted columns:
    
                        roll pitch yaw dS  dL  dP
    
                      This file can be used in FIM as an 'ort', to detrend
                      the data against correlation with the movements.
                      This type of analysis can be useful in removing
                      errors made in the interpolation.
    
      -1Dmatrix_save ff = Save the matrix transformation from base to input
                          coordinates in file 'ff' (1 row per sub-brick in
                          the input dataset).  If 'ff' does NOT end in '.1D',
                          then the program will append '.aff12.1D' to 'ff' to
                          make the output filename.
                   *N.B.: This matrix is the coordinate transformation from base
                          to input DICOM coordinates.  To get the inverse matrix
                          (input to base), use the cat_matvec program, as in
                            cat_matvec fred.aff12.1D -I
                   *N.B.: This matrix is the inverse of the matrix stored in
                          the output dataset VOLREG_MATVEC_* attributes.
                          The base-to-input convention followed with this
                          option corresponds to the convention in 3dAllineate.
                   *N.B.: 3dvolreg does not have a '-1Dmatrix_apply' option.
                          See 3dAllineate for this function.  Also confer with
                          program cat_matvec.
    
      -rotcom         Write the fragmentary 3drotate commands needed to
                      perform the realignments to stdout; for example:
                        3drotate -rotate 7.2I 3.2R -5.7A -ashift 2.7S -3.8L 4.9P
                      The purpose of this is to make it easier to shift other
                      datasets using exactly the same parameters.
    
      -maxdisp      = Print the maximum displacement (in mm) for brain voxels.
                        ('Brain' here is defined by the same algorithm as used
                        in the command '3dAutomask -clfrac 0.33'; the displacement
                        for each non-interior point in this mask is calculated.)
                        If '-verbose' is given, the max displacement will be
                        printed to the screen for each sub-brick; otherwise,
                        just the overall maximum displacement will get output.
                     ** This displacement is relative to the base volume.
                        [-maxdisp is now turned on by default]
      -nomaxdisp    = Do NOT calculate and print the maximum displacement.
                        [maybe it offends you in some theological sense?]
                        [maybe you have some real 'need for speed'?]
      -maxdisp1D mm = Do '-maxdisp' and also write the max displacement for each
                        sub-brick into file 'mm' in 1D (columnar) format.
                        You may find that graphing this file (cf. 1dplot)
                        is a useful diagnostic tool for your FMRI datasets.
                        [the 'mm' filename can be '-', which means stdout]
                     ** The program also outputs the maximum change (delta) in
                        displacement between 2 successive time points, into the
                        file with name 'mm_delt'.  This output can let you see
                        when there is a sudden head jerk, for example. [22 Jun 2015]
    
      -savedisp sss = Save 3 3D+time datasets with the x,y,z displacments at each
                      voxel at each time point.  The prefix for the x displacement
                      dataset will be the string 'sss' with '_DX' appended, etc.
                      This option is intended for use with various processing
                      scripts now under construction, and is probably otherwise
                      completely useless.
    
      -tshift ii      If the input dataset is 3D+time and has slice-dependent
                      time-offsets (cf. the output of 3dinfo -v), then this
                      option tells 3dvolreg to time shift it to the average
                      slice time-offset prior to doing the spatial registration.
                      The integer 'ii' is the number of time points at the
                      beginning to ignore in the time shifting.  The results
                      should like running program 3dTshift first, then running
                      3dvolreg -- this is primarily a convenience option.
                N.B.: If the base brick is taken from this dataset, as in
                      '-base 4', then it will be the time shifted brick.
                      If for some bizarre reason this is undesirable, you
                      could use '-base this+orig[4]' instead.
    
      -rotparent rset
        Specifies that AFTER the registration algorithm finds the best
        transformation for each sub-brick of the input, an additional
        rotation+translation should be performed before computing the
        final output dataset; this extra transformation is taken from
        the first 3dvolreg transformation found in dataset 'rset'.
      -gridparent gset
        Specifies that the output dataset of 3dvolreg should be shifted to
        match the grid of dataset 'gset'.  Can only be used with -rotparent.
        This dataset should be one this is properly aligned with 'rset' when
        overlaid in AFNI.
      * If 'gset' has a different number of slices than the input dataset,
        then the output dataset will be zero-padded in the slice direction
        to match 'gset'.
      * These options are intended to be used to align datasets between sessions:
         S1 = SPGR from session 1    E1 = EPI from session 1
         S2 = SPGR from session 2    E2 = EPI from session 2
     3dvolreg -twopass -twodup -base S1+orig -prefix S2reg S2+orig
     3dvolreg -rotparent S2reg+orig -gridparent E1+orig -prefix E2reg \
              -base 4 E2+orig
         Each sub-brick in E2 is registered to sub-brick E2+orig[4], then the
         rotation from S2 to S2reg is also applied, which shifting+padding
         applied to properly overlap with E1.
      * A similar effect could be done by using commands
     3dvolreg -twopass -twodup -base S1+orig -prefix S2reg S2+orig
     3dvolreg -prefix E2tmp -base 4 E2+orig
     3drotate -rotparent S2reg+orig -gridparent E1+orig -prefix E2reg E2tmp+orig
        The principal difference is that the latter method results in E2
        being interpolated twice to make E2reg: once in the 3dvolreg run to
        produce E2tmp, then again when E2tmp is rotated to make E2reg.  Using
        3dvolreg with the -rotparent and -gridparent options simply skips the
        intermediate interpolation.
    
              *** Please read file README.registration for more   ***
              *** information on the use of 3dvolreg and 3drotate ***
    
     Algorithm: Iterated linearized weighted least squares to make each
                  sub-brick as like as possible to the base brick.
                  This method is useful for finding SMALL MOTIONS ONLY.
                  See program 3drotate for the volume shift/rotate algorithm.
                  The following options can be used to control the iterations:
                    -maxite     m = Allow up to 'm' iterations for convergence
                                      [default = 19].
                    -x_thresh   x = Iterations converge when maximum movement
                                      is less than 'x' voxels [default=0.020000],
                    -rot_thresh r = And when maximum rotation is less than
                                      'r' degrees [default=0.030000].
                    -delta      d = Distance, in voxel size, used to compute
                                      image derivatives using finite differences
                                      [default=0.700000].
                    -final   mode = Do the final interpolation using the method
                                      defined by 'mode', which is one of the
                                      strings 'NN', 'cubic', 'quintic', 'heptic',
                                      or 'Fourier' or 'linear'
                                      [default=mode used to estimate parameters].
                -weight 'wset[n]' = Set the weighting applied to each voxel
                                      proportional to the brick specified here
                                      [default=smoothed base brick].
                                    N.B.: if no weight is given, and -twopass is
                                      engaged, then the first pass weight is the
                                      blurred sum of the base brick and the first
                                      data brick to be registered.
                       -edging ee = Set the size of the region around the edges of
                                      the base volume where the default weight will
                                      be set to zero.  If 'ee' is a plain number,
                                      then it is a voxel count, giving the thickness
                                      along each face of the 3D brick.  If 'ee' is
                                      of the form '5%', then it is a fraction of
                                      of each brick size.  For example, '5%' of
                                      a 256x256x124 volume means that 13 voxels
                                      on each side of the xy-axes will get zero
                                      weight, and 6 along the z-axis.  If this
                                      option is not used, then 'ee' is read from
                                      the environment variable AFNI_VOLREG_EDGING.
                                      If that variable is not set, then 5% is used.
                                    N.B.: This option has NO effect if the -weight
                                      option is used.
                                    N.B.: The largest % value allowed is 25%.
                         -twopass = Do two passes of the registration algorithm:
                                     (1) with smoothed base and data bricks, with
                                         linear interpolation, to get a crude
                                         alignment, then
                                     (2) with the input base and data bricks, to
                                         get a fine alignment.
                                      This method is useful when aligning high-
                                      resolution datasets that may need to be
                                      moved more than a few voxels to be aligned.
                      -twoblur bb = 'bb' is the blurring factor for pass 1 of
                                      the -twopass registration.  This should be
                                      a number >= 2.0 (which is the default).
                                      Larger values would be reasonable if pass 1
                                      has to move the input dataset a long ways.
                                      Use '-verbose -verbose' to check on the
                                      iterative progress of the passes.
                                    N.B.: when using -twopass, and you expect the
                                      data bricks to move a long ways, you might
                                      want to use '-heptic' rather than
                                      the default '-Fourier', since you can get
                                      wraparound from Fourier interpolation.
                          -twodup = If this option is set, along with -twopass,
                                      then the output dataset will have its
                                      xyz-axes origins reset to those of the
                                      base dataset.  This is equivalent to using
                                      '3drefit -duporigin' on the output dataset.
                           -sinit = When using -twopass registration on volumes
                                      whose magnitude differs significantly, the
                                      least squares fitting procedure is started
                                      by doing a zero-th pass estimate of the
                                      scale difference between the bricks.
                                      Use this option to turn this feature OFF.
                  -coarse del num = When doing the first pass, the first step is
                                      to do a number of coarse shifts in order to
                                      find a starting point for the iterations.
                                      'del' is the size of these steps, in voxels;
                                      'num' is the number of these steps along
                                      each direction (+x,-x,+y,-y,+z,-z).  The
                                      default values are del=10 and num=2.  If
                                      you don't want this step performed, set
                                      num=0.  Note that the amount of computation
                                      grows as num**3, so don't increase num
                                      past 4, or the program will run forever!
                                 N.B.: The 'del' parameter cannot be larger than
                                       10% of the smallest dimension of the input
                                       dataset.
                  -coarserot        Also do a coarse search in angle for the
                                      starting point of the first pass.
                  -nocoarserot      Don't search angles coarsely.
                                      [-coarserot is now the default - RWCox]
                  -wtinp          = Use sub-brick[0] of the input dataset as the
                                      weight brick in the final registration pass.
    
     N.B.: * This program can consume VERY large quantities of memory.
              (Rule of thumb: 40 bytes per input voxel.)
              Use of '-verbose -verbose' will show the amount of workspace,
              and the steps used in each iteration.
           * ALWAYS check the results visually to make sure that the program
              wasn't trapped in a 'false optimum'.
           * The default rotation threshold is reasonable for 64x64 images.
              You may want to decrease it proportionally for larger datasets.
           * -twopass resets the -maxite parameter to 66; if you want to use
              a different value, use -maxite AFTER the -twopass option.
           * The -twopass option can be slow; several CPU minutes for a
              256x256x124 volume is a typical run time.
           * After registering high-resolution anatomicals, you may need to
              set their origins in 3D space to match.  This can be done using
              the '-duporigin' option to program 3drefit, or by using the
              '-twodup' option to this program.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
