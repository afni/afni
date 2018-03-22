**********
3dNwarpCat
**********

.. _3dNwarpCat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dNwarpCat [options] warp1 warp2 ...
    ------
     * This program catenates (composes) 3D warps defined on a grid,
       OR via a matrix.
     ++ All transformations are from DICOM xyz (in mm) to DICOM xyz.
    
     * Matrix warps are in files that end in '.1D' or in '.txt'.  A matrix
       warp file should have 12 numbers in it, as output (for example), by
       '3dAllineate -1Dmatrix_save'.
      ++ The matrix (affine) warp can have either 12 numbers on one row,
         or be in the 3x4 format.
    
     * Nonlinear warps are in dataset files (AFNI .HEAD/.BRIK or NIfTI .nii)
       with 3 sub-bricks giving the DICOM order xyz grid displacements in mm.
    
     * If all the input warps are matrices, then the output is a matrix
       and will be written to the file 'prefix.aff12.1D'.
     ++ Unless the prefix already contains the string '.1D', in which case
        the filename is just the prefix.
     ++ If 'prefix' is just 'stdout', then the output matrix is written
        to standard output.
     ++ In any of these cases, the output format is 12 numbers in one row.
    
     * If any of the input warps are datasets, they must all be defined on
       the same 3D grid!
     ++ And of course, then the output will be a dataset on the same grid.
     ++ However, you can expand the grid using the '-expad' option.
    
     * The order of operations in the final (output) warp is, for the
       case of 3 input warps:
    
         OUTPUT(x) = warp3( warp2( warp1(x) ) )
    
       That is, warp1 is applied first, then warp2, et cetera.
       The 3D x coordinates are taken from each grid location in the
       first dataset defined on a grid.
    
     * For example, if you aligned a dataset to a template with @auto_tlrc,
       then further refined the alignment with 3dQwarp, you would do something
       like this:
           warp1 is the output of 3dQwarp
           warp2 is the matrix from @auto_tlrc
           This is the proper order, since the desired warp takes template xyz
           to original dataset xyz, and we have
             3dQwarp warp:      takes template xyz to affinely aligned xyz, and
             @auto_tlrc matrix: takes affinely aligned xyz to original xyz
    
       3dNwarpCat -prefix Fred_total_WARP -warp1 Fred_WARP+tlrc.HEAD -warp2 Fred.Xat.1D 
    
       The dataset Fred_total_WARP+tlrc.HEAD could then be used to transform original
       datasets directly to the final template space, as in
    
       3dNwarpApply -prefix Wilma_warped        \
                    -nwarp Fred_total_WARP+tlrc \
                    -source Wilma+orig          \
                    -master Fred_total_WARP+tlrc
    
     * If you wish to invert a warp before it is used here, supply its
       input name in the form of
         INV(warpfilename)
       To produce the inverse of the warp in the example above:
    
       3dNwarpCat -prefix Fred_total_WARPINV        \
                  -warp2 'INV(Fred_WARP+tlrc.HEAD)' \
                  -warp1 'INV(Fred.Xat.1D)' 
    
       Note the order of the warps is reversed, in addition to the use of 'INV()'.
    
     * The final warp may also be inverted simply by adding the '-iwarp' option, as in
    
       3dNwarpCat -prefix Fred_total_WARPINV -iwarp -warp1 Fred_WARP+tlrc.HEAD -warp2 Fred.Xat.1D 
    
     * Other functions you can apply to modify a 3D dataset warp are:
        SQRT(datasetname) to get the square root of a warp
        SQRTINV(datasetname) to get the inverse square root of a warp
       However, you can't do more complex expressions, such as 'SQRT(SQRT(warp))'.
       If you think you need something so rococo, use 3dNwarpCalc.  Or think again.
    
     * You can also manufacture a 3D warp from a 1-brick dataset with displacments
       in a single direction.  For example:
          AP:0.44:disp+tlrc.HEAD  (note there are no blanks here!)
       means to take the 1-brick dataset disp+tlrc.HEAD, scale the values inside
       by 0.44, then load them into the y-direction displacements of a 3-brick 3D
       warp, and fill the other 2 directions with zeros.  The prefixes you can use
       here for the 1-brick to 3-brick displacment trick are
         RL: for x-displacements (Right-to-Left)
         AP: for y-displacements (Anterior-to-Posterior)
         IS: for z-displacements (Inferior-to-Superior)
         VEC:a,b,c: for displacements in the vector direction (a,b,c),
                    which vector will be scaled to be unit length.
         Following the prefix's colon, you can put in a scale factor followed
         by another colon (as in '0.44:' in the example above).  Then the name
         of the dataset with the 1D displacments follows.
     * You might reasonably ask of what possible value is this peculiar format?
       This was implemented to use Bz fieldmaps for correction of EPI datasets,
       which are distorted only along the phase-encoding direction.  This format
       for specifying the input dataset (the fieldmap) is built to make the
       scripting a little easier.  Its principal use is in the program 3dNwarpApply.
    
     * Finally, you can input a warp catenation string directly as in the '-nwarp'
       option of 3dNwarpApply, as in
    
       3dNwarpCat -prefix Fred_total_WARP 'Fred_WARP+tlrc.HEAD Fred.Xat.1D' 
    
    
    OPTIONS
    -------
     -interp iii == 'iii' is the interpolation mode:
                    ++ Modes allowed are a subset of those in 3dAllineate:
                         linear  quintic  wsinc5
                    ++ The default interpolation mode is 'wsinc5'.
                    ++ 'linear' is much faster but less accurate.
                    ++ 'quintic' is between 'linear' and 'wsinc5',
                       in both accuracy and speed.
    
     -verb       == print (to stderr) various fun messages along the road.
    
     -prefix ppp == prefix name for the output dataset that holds the warp.
     -space sss  == attach string 'sss' to the output dataset as its atlas
                    space marker.
    
     -warp1 ww1  == alternative way to specify warp#1
     -warp2 ww2  == alternative way to specify warp#2 (etc.)
                    ++ If you use any '-warpX' option for X=1..99, then
                       any addition warps specified after all command
                       line options appear AFTER these enumerated warps.
                       That is, '-warp1 A+tlrc -warp2 B+tlrc C+tlrc'
                       is like using '-warp3 C+tlrc'.
                    ++ At most 99 warps can be used.  If you need more,
                       PLEASE back away from the computer slowly, and
                       get professional counseling.
    
     -iwarp      == Invert the final warp before output.
    
     -expad PP   == Pad the nonlinear warps by 'PP' voxels in all directions.
                    The warp displacements are extended by linear extrapolation
                    from the faces of the input grid.
    
    AUTHOR -- RWCox -- March 2013
    
     =========================================================================
    * This binary version of 3dNwarpCat is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system. You can control this value by setting environment variable
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
    * How many threads are useful? That varies with the program, and how well
       it was coded. You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 12.
     =========================================================================
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
