************
3dNwarpApply
************

.. _3dNwarpApply:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dNwarpApply [options]
    
    Program to apply a nonlinear 3D warp saved from 3dQwarp (or 3dNwarpCat, etc.)
    to a 3D dataset, to produce a warped version of the source dataset.
    
    The '-nwarp' and '-source' options are MANDATORY.  For both of these options,
    as well as '-prefix', the input arguments after the option name are applied up
    until an argument starts with the '-' character, or until the arguments run out.
    
    This program has been heavily modified [01 Dec 2014], including the following
    major improvements:
    (1) Allow catenation of warps with different grid spacings -- the functions
        that deal with the '-nwarp' option will automatically deal with the grids.
    (2) Allow input of affine warps with multiple time points, so that 3D+time
        datasets can be warped with a time dependent '-nwarp' list.
    (3) Allow input of multiple source datasets, so that several datasets can be
        warped the same way at once.  This operation is more efficient than running
        3dNwarpApply several times, since the auto-regridding and auto-catenation
        in '-nwarp' will only have to be done once.
    (3a) Specification of the output dataset names can be done via multiple
         arguments to the '-prefix' option, or via the new '-suffix' option.
    
    OPTIONS:
    --------
     -nwarp  www  = 'www' is the name of the 3D warp dataset
                    (this is a mandatory option!)
                   ++ Multiple warps can be catenated here.
                 -->> Please see the lengthier discussion below on this feature!
        ++ NOTE WELL: The interpretation of this option has changed,
                      as of 01 Dec 2014.  In particular, this option is
                      generalized from the version in other programs, including
                      3dNwarpCat, 3dNwarpFuncs, and 3dNwarpXYZ.  The major
                      change is that multi-line matrix files are allowed to
                      be included in the 'www' mixture, so that the nonlinear
                      warp being calculated can be time-dependent.
                      In addition, the warps supplied need not all be on the
                      same 3D grid -- this ability lets you catenate a warp
                      defined on the EPI data grid with a warp defined on the
                      structural data grid (e.g.).
    
     -iwarp       = After the warp specified in '-nwarp' is computed,
                    invert it.  If the input warp would take a dataset
                    from space A to B, then the inverted warp will do
                    the reverse.
                    ++ The combination "-iwarp -nwarp 'A B C'" is equivalent
                       to "-nwarp 'INV(C) INV(B) INV(A)'" -- that is, inverting
                       each warp/matrix in the list *and* reversing their order.
                    ++ The '-iwarp' option is provided for convenience, and
                       may prove to be very slow for time-dependent '-nwarp' inputs.
    
     -affter aaa  = *** THIS OPTION IS NO LONGER AVAILABLE ***
                      See the discussion of the new '-nwarp' option above to see
                      how to do include time-dependent matrix transformations
                      in this program.
    
     -source sss  = 'sss' is the name of the source dataset.
                    ++ That is, the dataset to be warped.
                    ++ Multiple datasets can be supplied here; they MUST
                       all be defined over the same 3D grid.
                -->>** You can no longer simply supply the source
                       dataset as the last argument on the command line.
    
     -master mmm  = 'mmm  is the name of the master dataset.
                    ++ Which defines the output grid.
                    ++ If '-master' is not used, then output
                       grid is the same as the source dataset grid.
                    ++ It is often the case that it makes more sense to
                       use the '-nwarp' dataset as the master, since
                       that is the grid on which the transformation is
                       defined, and is (usually) the grid to which the
                       transformation 'pulls' the source data.
                    ++ You can use '-master WARP' or '-master NWARP'
                       for this purpose -- but ONLY if all the warps listed
                       in the '-nwarp' option have the same 3D grid structure.
                    ++ In particular, if the transformation includes a
                       long-distance translation, then the source dataset
                       grid may not have a lot of overlap with the source
                       dataset after it is transformed!
    
     -newgrid dd  = 'dd' is the new grid spacing (cubical voxels, in mm)
       *OR        = ++ This lets you resize the master dataset grid spacing.
     -dxyz dd     =    for example, to bring EPI data to a 1 mm template, but at
                       a coarser resolution, use '-dxyz 2'.
    
     -interp iii  = 'iii' is the interpolation mode
                    ++ Default interpolation mode is 'wsinc5' (slowest, bestest)
                    ++ Available modes are the same as in 3dAllineate:
                         NN  linear  cubic  quintic  wsinc5
                    ++ The same interpolation mode is used for the warp
                       itself (if needed) and then for the data being warped.
                    ++ The warp will be interpolated if the output dataset is
                       not on the same 3D grid as the warp itself, or if a warp
                       expression is used in the '-nwarp' option.  Otherwise,
                       it won't need to be interpolated.
    
     -ainterp jjj = This option lets you specify a different interpolation mode
                    for the data than might be used for the warp.  In particular,
                    '-ainterp NN' would be most logical for atlas datasets, where
                    the data values being mapped are labels.
    
     -prefix ppp  = 'ppp' is the name of the new output dataset
                    ++ If more than 1 source dataset is supplied, then you
                       should supply more than one prefix.  Otherwise, the
                       program will invent prefixes for each output, by
                       attaching the suffix '_Nwarp' to each source
                       dataset's prefix.
    
     -suffix sss  = If the program generates prefixes, you can change the
                    default '_Nwarp' suffix to whatever you want (within
                    reason) by this option.
                    ++ His Holiness Emperor Zhark defines 'within reason', of course.
                    ++ By using '-suffix' and NOT using '-prefix', the program
                       will generate prefix names for all output datasets in
                       a systematic way -- this might be useful for some people.
                    ++ Note that only ONE suffix can be supplied even if many source
                       datasets are input -- unlike the case with '-prefix'.
    
     -short       = Write output dataset using 16-bit short integers, rather than
                    the usual 32-bit floats.
                    ++ Intermediate values are rounded to the nearest integer.
                       No scaling is performed.
                    ++ This option is intended for use with '-ainterp' and for
                       source datasets that contain integral values.
    
     -quiet       = Don't be verbose :-(
     -verb        = Be extra verbose :-)
    
    SPECIFYING THE NONLINEAR WARP IN '-nwarp'
    [If you are catenating warps, read this carefully!]
    ---------------------------------------------------
    A single nonlinear warp (usually created by 3dQwarp) is an AFNI or NIfTI-1
    dataset with 3 sub-bricks, holding the 3D displacements of each voxel.
    (All coordinates and displacements are expressed in DICOM order.)
    
    The '-nwarp' option is used to specify the nonlinear transformation used
    to create the output dataset from the source dataset.  For many purposes,
    the only input needed here is the name of a single dataset holding the
    warp to be used.
    
    However, the '-nwarp' option also allows the catenation of a sequence of
    spatial transformations (in short, 'warps') that will be combined before
    being applied to the source dataset.  Each warp is either a nonlinear
    warp dataset or a matrix warp (a linear transformation of space).
    
    A single affine (or linear) warp is a set of 12 numbers, defining a 3x4 matrix
       a11 a12 a13 a14
       a21 a22 a23 a24
       a31 a32 a33 a34
    A matrix is stored on a single line, in a file with the extension
    '.1D' or '.txt', in this order
       a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34
    For example, the identity matrix is given by
       1 0 0 0 0 1 0 0 0 0 1 0
    This format is output by the '-1Dmatrix_save' options in 3dvolreg and
    3dAllineate, for example.
    
    If the argument 'www' following '-nwarp' is made up of more than one warp
    filename, separated by blanks, then the nonlinear warp to be used is
    composed on the fly as needed to transform the source dataset.  For
    example,
       -nwarp 'AA_WARP.nii BB.aff12.1D CC_WARP.nii'
    specifies 3 spatial transformations, call them A(x), B(x), and C(x) --
    where B(x) is just the 3-vector x multipled into the matrix in the
    BB.aff12.1D file.  The resulting nonlinear warp function N(x) is
    obtained by applying these transformations in the order given, A(x) first:
       N(x) = C( B( A(x) ) )
    That is, the first warp A is applied to the output grid coordinate x,
    then the second warp B to that results, then the third warp C.  The output
    coordinate y = C(B(A(x))) is the coordinate in the source dataset at which
    the output value will be interpolated (for the voxel at coordinate x).
    
    The Proper Order of Catenated Warps:
    ....................................
    To determine the correct order in which to input the warps, it is necessary
    to understand what a warp of the source dataset actually computes.  Call the
    source image S(x) = (scalar) value of source image at voxel location x.
    For each x in the output grid, the warped result is S(N(x)) -- that is,
    N(x) tells where each output location x must be warped to in order to
    find the corresponding value of the source S.
    
    N(x) does *NOT* tell to where an x in the source image must be moved to in
    the output space -- which is what you might think if you mentally prioritize
    the idea of 'warping the source image'.  It is better to think of N(x) as
    reaching out from x in the output space to a location in the source space,
    and then the program will interpolate from the discrete source space grid
    at that location -- which is unlikely to be exactly on a grid node.
    
    Now suppose the sequence of operations on an EPI dataset is
     (1) Nonlinearly unwarp the dataset via warp AA_WARP.nii (perhaps
         from 3dQwarp -plusminus).
     (2) Perform linear volume registration on the result from (1) (with
         program 3dvolreg) to get affine matrix file BB.aff12.1D -- which
         will have 1 line per time point in the EPI dataset.
     (3) Linearly register the structural volume to the EPI dataset
         (via script align_epi_anat.py).  Note that this step transforms
         the structural volume to match the EPI, not the EPI to match the
         structural volume, so this step does not affect the chain of
         transformations being applied to the EPI dataset.
     (4) Nonlinearly warp the structural image from (3) to MNI space via
         warp CC_WARP.nii (generated by 3dQwarp).
    Finally, the goal is to take the original EPI time series dataset, and
    warp it directly to MNI space, including the time series registration for
    each sub-brick in the dataset, with only one interplation being used --
    rather than the 3 interpolations that would come by serially implementing
    steps (1), (2), and (4).  This one-big-step transformation can be done
    with 3dNwarpApply using the '-nwarp' option:
       -nwarp 'CC_WARP.nii BB.aff12.1D AA_WARP.nii'
    that is, N(x) = A( B( C(x) ) ) -- the opposite order to the sample above,
    and with the transformations occuring in the opposite order to the sequence
    in which they were calculated.  The reason for this apparent backwardness
    is that the 'x' being transformed is on the output grid -- in this case, in
    MNI-template space.  So the warp C(x) transforms such an output grid 'x' to
    the EPI-aligned structural space.  The warp B(x) then transforms THAT
    coordinate from aligned spaced back to the rotated head position of the subject.
    And the warp A(x) transforms THAT coordinate back to the original grid that had
    to be unwarped (e.g., from susceptibility and/or eddy current artifacts).
    
    Also note that in step (2), the matrix file BB.aff12.1D has one line for
    each time point.  When transforming a source dataset, the i-th time point
    will be transformed by the warp computed using the i-th line from any
    multi-line matrix file in the '-nwarp' specification.  (If there are more
    dataset time points than matrix lines, then the last line will be re-used.)
    
    In this way, 3dNwarpApply can be used to carry out time-dependent warping
    of time-dependent datasets, provided that the time-dependence in the warp
    only occurs in the affine (matrix) parts of the transformation.
    
    Note that the now-obsolete option '-affter' is subsumed into the new way
    that '-nwarp' works.  Formerly, the only time-dependent matrix had to
    be specified as being at the end of the warp chain, and was given via
    the '-affter' option.  Now, a time-dependent matrix (or more than one)
    can appear anywhere in the warp chain, so there is no need for a special
    option.  If you DID use '-affter', you will have to alter your script
    simply by putting the final matrix filename at the end of the '-nwarp'
    chain.  (If this seems too hard, please consider another line of work.)
    
    The other 3dNwarp* programs that take the '-nwarp' option operate similarly,
    but do NOT allow time-dependent matrix files.  Those programs are built to
    operate with one nonlinear warp, so allowing a time-dependent warp doesn't
    make sense for them.
    
    NOTE: If a matrix is NOT time-dependent (just a single set of 12 numbers),
          it can be input in the .Xat.1D format of 3 rows, each with 4 values:
             a11 a12 a13 a14  }                        1 0 0 0
             a21 a22 a23 a24  } e.g, identity matrix = 0 1 0 0
             a31 a32 a33 a34  }                        0 0 1 0
          This option is just for convenience.  Remember that the coordinates
          are DICOM order, and if your matrix comes from Some other PrograM
          or from a Fine Software Library, you probably have to change some
          signs in the matrix to get things to work correctly.
    
    RANDOM NOTES:
    -------------
    * At present, this program doesn't work with 2D warps, only with 3D.
      (That is, each warp dataset must have 3 sub-bricks.)
    
    * At present, the output dataset is stored in float format, no matter what
      absurd data format the input dataset uses (but cf. the '-short' option).
    
    * As described above, 3dNwarpApply allows you to catenate warps directly on
      the command line, as if you used 3dNwarpCat before running 3dNwarpApply.
      For example:
    
      ++ You have aligned dataset Fred+orig to MNI-affine space using @auto_tlrc,
         giving matrix file Fred.Xaff12.1D
    
      ++ Then you further aligned from MNI-affine to MNI-qwarp via 3dQwarp,
         giving warp dataset Fred_WARP+tlrc
    
      ++ You can combine the transformations and interpolate Fred+orig directly
         to MNI-qwarp space using a command like
            3dNwarpApply -prefix Fred_final    \
                         -source Fred+orig     \
                         -master NWARP         \
                         -nwarp 'Fred_WARP+tlrc Fred.Xaff12.1D'
         Note the warps to be catenated are enclosed in quotes to make a single
         input argument passed to the program.  The processing used for this
         purpose is the same as in 3dNwarpCat -- see the help output for that
         program for a little more information.
    
      ++ When you specify a nonlinear warp dataset, you can use the 'SQRT()' and
         'INV()' and 'INVSQRT()' operators, as well as the various 1D-to-3D
         displacement prefixes ('AP:' 'RL:' 'IS:' 'VEC:' 'FAC:') -- 
         for example, the following is a legal (and even useful) definition of a
         warp herein:
            'SQRT(AP:epi_BU_yWARP+orig)'
         where the 'AP:' transforms the y-displacements in epi_BU_ywarp+orig to a
         full 3D warp (with x- and z-displacments set to zero), then calculates the
         square root of that warp, then applies the result to some input dataset.
        + This is a real example, where the y-displacement-only warp is computed between
          blip-up and blip-down EPI datasets, and then the SQRT warp is applied to
          warp them into the 'intermediate location' which should be better aligned
          with the subject's anatomical datasets.
     -->+ However: see also the '-plusminus' option for 3dQwarp for another way to
          reach the same goal.
    
      ++ You can also use 'IDENT(dataset)' to define a "nonlinear" 3D warp whose
         grid is defined by the dataset header -- nothing else from the dataset will
         be used.  This warp will be filled with all zero displacements, which represents
         the identity warp.  The purpose of such an object is to let you apply a pure
         affine warp -- since this program requires a '-nwarp' option, you can use
         -nwarp 'IDENT(dataset)' to define the 3D grid for the 'nonlinear' 3D warp and
         then catenate the affine warp.
    
    * PLEASE note that if you use the '-allineate' option in 3dQwarp, then the affine
      warp is already included in the output nonlinear warp from 3dQwarp, and so it
      does NOT need to be applied again in 3dNwarpApply!  This mistake has been made
      in the past, and the results were not good.
    
     =========================================================================
    * This binary version of 3dNwarpApply is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system.  You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful?  That varies with the program, and how well
       it was coded.  You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 8.
     =========================================================================
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
