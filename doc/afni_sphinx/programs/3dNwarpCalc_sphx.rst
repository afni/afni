***********
3dNwarpCalc
***********

.. _3dNwarpCalc:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dNwarpCalc [options] expression
    ------
     * This program performs calculations on 3D warps defined on a grid.
    
     * The fundamental idea is a 'stack' of warps, with operators being
        applied to the top element(s) of the stack.
       ++ If you don't know what a computer science 'stack' is, see
          http://en.wikipedia.org/wiki/Stack_(data_structure)
       ++ Also see 1dmatcalc for a similar implementation of a stack
          of matrix operations.
       ++ In the explanations below, the stack will be denoted as
            [ A B C ... ]
          where A is the top element, B the next element, etc.
          Operations take place using the top one or two elements.
    
     * The expression is a single string enclosed in quotes ' or ",
        with operators separated by spaces.  See EXAMPLES below.
       ++ The expression is the last thing on the command line!
       ++ For scripting convenience, you can actually break the
          expression into multiple strings (after all the '-' options),
          and they will be re-assembled into one big string for
          parsing and processing.
    
    ** Note that to get any output, you will have to use the '&write'
        operator at least once.  Otherwise, the program computes stuff
        and then just throws it away.  (Fun perhaps, but useless.)
    
     * To actually use a 3D warp to transform a dataset, you must run the
        program 3dNwarpApply:
       ++ Note that the warp used in 3dNwarpApply does not need to be on the
          same grid as the dataset being transformed.  You can define a warp
          on a high-resolution anatomical grid and apply it to a low-resolution
          functional dataset, for example -- 3dNwarpApply will figure it out.
       ++ On the other hand, all the warps in 3dNwarpCalc must be defined on
          the same spatial grid!
       ++ (LATER) You can use the &apply command to transform a 3D dataset
          from within 3dNwarpCalc, if you don't need the special capabilities
          of 3dNwarpApply.
    
     * Operations such as &invert and &sqrt may produce artifacts and be
        inaccurate near the edges of the 3D grid, since they might require
        extrapolating the warp to the outside of the grid in places, where
        there is no information.
    
     * For convenience, you can break the expression up into multiple strings
        (after all the options), and they will be re-assembled into the single
        expression string the controlling C function needs to work.
       ++ But note that since the '&' and '()' characters are special to the shell
          you have to put the expression string(s) inside single quote ' or double
          quote " pairs, or else ugly things will happen.
          (Not as ugly as having a hippopotamus step on your head, but almost.)
    
    OPTIONS
    -------
     -interp iii   == 'iii' is the interpolation mode:
                      ++ Modes allowed are a subset of those in 3dAllineate:
                           linear  quintic  wsinc5
                      ++ The default interpolation mode is 'quintic'.
                      ++ 'linear' is much faster but less accurate.
                      ++ 'wsinc5' is much slower but more accurate.
    
     -ainterp jjj  == 'jjj' is the interpolation mode for the '&apply' operation.
                      ++ Modes allowed here are
                           NN linear cubic quintic wsinc5
                      ++ If this option isn't given, then the value from '-interp'
                         is used (which should be good enough for government work).
    
     -verb         == print (to stderr) various fun messages along the road
                      ++ A second '-verb' gives you even more fun!
    
    BUT WHERE DO WARPS COME FROM, MOMMY?
    ------------------------------------
     * The program 3dAllineate with the -nwarp_save option will save a
        displacement representation of a nonlinear warp to a 3D dataset
        with 3 sub-bricks (1 for each of x, y, and z).
    
     * The contents of these sub-bricks are the displacments of each voxel in mm.
       ++ The identity warp would be all zero, for example.
    
     * An input warp dataset can contain extra sub-bricks -- only the first 3
        are used.
    
     * If you want the volume distortion at each voxel, use the program
       3dNwarpFuncs.
    
    OPERATORS
    ---------
     * In the explanations below, the single character 'x' represents a 3D
        coordinate vector, and a capital letter such as 'A' represents a
        whole 3D warp function, whose output at a particular location is 'A(x)'.
     * You can replace the '&' character that starts a command with '%' or '@',
        if that is more convenient for you.
     * Operator names are not case sensitive: &INVERT is the same as &invert.
    
    &readnwarp(FF) == Read a 3D warp from a file and place it on top of the stack.
      *OR*             The input file should be a 3D dataset with 3 sub-bricks
    &readwarp(FF)      (volumes) storing the xyz displacments of each grid point.
    
    &identwarp(FF) == Create an identity warp (all displacements 0) on the grid
                       of a 3D dataset specified by the filename 'FF'.
                      ++ This operation is to be used to create a starting point
                         for calculations that otherwise do not involve a warp
                         defined on a grid, such a polynomial warps.
                      ++ The actual data in 'FF' is ignored by '&identwarp'; only the
                         3D grid definition in the header is actually needed.
           ----**==>> ++ Either '&identwarp' or '&readnwarp' should be the first
                         operation, in order to define the grid for all subsequent
                         calculations.
    
    &readpoly(FF)  == The input is a text file with one line of numbers
                       specifying a warp as a polynomial, as output from
                       '3dAllineate -1Dparam_save'.
                      ++ The count of values determines the type of warp:
                             12 ==> affine (shifts+angles+scales+shears)
                             64 ==> cubic (3rd order) polyomial
                            172 ==> quintic (5th order) polynomial
                            364 ==> heptic (7th order) polynomial
                            664 ==> nonic (9th order) polynomial
                      ++ Any other count of values on the single input line is
                         illegal, unconstitutional, against the laws of God,
                         fattening, and will make you get red pimples on your nose.
                      ++ The parameters could come, most probably, from using
                         3dAllineate with the '-1Dparam_save' and '-nwarp' options.
    
    &read4x4(FF)   == Read an affine 4x4 transform matrix directly; the input
                       file should contain 12 numbers in the order:
                         r11 r12 r13 r14 r21 r22 r23 r24 r31 r32 r33 r34
                       which will be organized into the 3D transformation matrix:
                          r11   r12   r13   r14
                          r21   r22   r23   r24
                          r31   r32   r33   r34
                          0.0   0.0   0.0   1.0
                     ++ This matrix defines the transformation from input spatial
                        DICOM coordinates (x,y,z) to output coordinates, in mm.
                     ++ One way to get this matrix is via '3dAllineate -1Dmatrix_save'.
                     ++ This matrix should have non-zero determinant!
    
    &write(FF)     == Write the 3D warp on the top of the stack to a file.
                       The output file is always in a 3D nwarp (dataset) format
                       -- NEVER a matrix or polynomial.
    
    &dup           == Push the duplicate of the top of the stack onto the stack:
                       [ A B C ... ] goes to [ A A B C ... ]  after &dup.
    
    &swap          == Interchange the top two elements of the stack:
                       [ A B C ... ] goes to [ B A C ... ]    after &swap
                      ++ You can swap other elements of the stack by using
                         indexes in the form '&swap(p,q)' where 'p' and 'q'
                         are distinct non-negative integers indicating depth
                         into the stack; '&swap' is equivalent to '&swap(0,1)'.
    
    &pop           == Remove (and delete) the top element from the stack:
                       [ A B C ... ] goes to [ B C ... ]      after &pop
    
    &compose       == If the stack is [ A(x) B(x) C(x) ... ], compute the warp
      *OR*             B(A(x)) and replace these top 2 elements with the result:
    &mult              [ A B C ... ] goes to [ B(A(x)) C(x) ... ] after &compose
                      ++ If you wanted to compute A(B(x)), then you would use the
                         operator combination '&swap &compose'.
    
    &invert        == Replace top element of the stack with its inverse:
                       the warp J(x) such that A(J(x)) = x.
                      ++ Inversion is done via a functional iteration:
                           Jnew(x) = Jold( 2*x - A(Jold(x)) )
                         which requires 1 warp composition and 1 warp interpolation
                         for each step.
                      ++ &invert and &invsqrt (and thus &sqrt) are slow operations
                         due to the iterative nature of the calculations.
                      ++ Multiple CPUS (via OpenMP) are used to help speed up
                         these functions.
                      ++ The '-verb' option to 3dNwarpCalc will show you the
                         progress of the iterations for &invert and &invsqrt.
    
    &sqrt          == Replace top element of the stack with its 'square root':
                       the warp Q(x) such that Q(Q(x)) = A(x).
                      ++ NOTE: not all warps have square roots, so this operation
                         is not guaranteed to work.  Be careful out there.
                      ++ Nor is the square root of a nonlinear operator guaranteed
                         to be unique!
    
    &invsqrt       == Replace the top element of the stack with the inverse of
                       its square root: the warp R(x) such that A(R(R(x)) = x.
                      ++ '&sqrtinv' is a synonym for this operation, since I always
                         have trouble remembering which one is correct-imundo-ific.
                      ++ This operation is based on a functional iteration
                         adapted from the Denman-Beavers method for computing
                         the square root of a matrix:
                           initialize Y(x) = A(x) and Z(x) = x; then iterate
                             Ynew(x) = 0.5*(Yold(x)+inv(Zold(x)))
                             Znew(x) = 0.5*(Zold(x)+inv(Yold(x)))
                           which converges to Y=sqrt(A) and Z=invsqrt(A).
                      ++ For speed, these square root iterations are always done
                         with linear interpolation, no matter what '-interp' is.
    
    &sqrtpair      == Compute both &sqrtinv and &sqrt, and leave both of them
                      on the stack -- &sqrt on top, &sqrtinv 'below' it.
    
    &sqr           == Replace the top element of the stack with its 'square':
                       the warp S(x) = A(A(x)).  Equivalent to '&dup &compose'.
                      ++ To compute the fourth power of a warp: '&sqr &sqr'
                      ++ To compute the third power of a warp:  '&dup &sqr &compose'
                      ++ '&square' is a synonym for this operation.
    
    &scale(a)      == Scale the top-of-stack warp displacements by numerical
                       factor 'a' in all 3 dimensions.
                      ++ NOTE: this might make the warp non-invertible, (e.g., give
                         negative results in 'hexvol') for large enough 'a'.
                         Proceed at your own risk!
                      ++ If a=0, then the result is the identity warp, since
                         all the displacements are now 0.
                      ++ The case a=-1 is NOT the inverse warp!
    
    &sum           == Add the displacements of the two warps on the stack,
                       then replace BOTH of them with the result.
                      ++ You can do something like '&sum(0.5,0.5)' to average
                         the displacements, or '&sum(1,-1)' to difference them.
                         In this case, the first value scales the displacements
                         of the stack's top warp, and the second value scales the
                         displacements of the stack's second warp.
                      ++ NOTE: you can produce a non-invertible warp this way!
    
    &apply(DD,PP)  == Apply the 3D warp at the top of the stack to a dataset
                       whose name is given by the 'DD' argument, to produce
                       a dataset whose prefix is given by the 'PP' argument.
                     ++ This operation does not affect the stack of warps.
                     ++ &apply is provided to make your life simpler and happier :-)
                     ++ &apply is like 3dNwarpApply with the output dataset PP
                        always being on the same grid as the input dataset DD.
                     ++ The grid of dataset DD does NOT have to be the same as the
                        grid defining the warp defined on the stack.  If needed,
                        the warp will be interpolated to be used with DD.
                     ++ Program 3dNwarpApply provides more options to control
                        the way that a warp is applied to a dataset; for example,
                        to control the output grid spacing.
    
    EXAMPLES
    --------
    ** Read a warp from a dataset, invert it, save the inverse.
    
     3dNwarpCalc '&readnwarp(Warp+tlrc.HEAD) &invert &write(WarpInv)'
    
    ** Do the same, but also compute the composition of the warp with the inverse,
       and save that -- ideally, the output warp displacements would be identically
       zero (i.e., the identity warp), and the 'hexvol' entries would be constant
       and equal to the voxel volume.
    
     3dNwarpCalc -verb '&readnwarp(Warp+tlrc.HEAD) &dup &invert' \
                 '&write(WarpInv) &compose &write(WarpOut)'
    
    ** Read in a warp, compute its inverse square root, then square that, and compose
       the result with the original warp -- the result should be the identity warp
       (i.e., all zero displacments) -- except for numerical errors, of course.
    
     3dNwarpCalc '&readnwarp(Warp+tlrc.HEAD) &dup &invsqrt &sqr &compose &write(WarpOut)'
    
    AUTHOR -- RWCox -- August 2011
    
     =========================================================================
    * This binary version of 3dNwarpCalc is compiled using OpenMP, a semi-
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
