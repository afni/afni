********
3dinfill
********

.. _ahelp_3dinfill:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    A program to fill holes in a volumes.
    
         3dinfill    <-input DSET> 
    
    Options:
       -input  DSET: Fill volume DSET
       -prefix PREF: Use PREF for output prefix.
       -Niter NITER: Do not allow the fill function to do more than NITER
                     passes. A -1 (default) lets the function go to a maximum
                     of 500 iterations. You will be warned if you run our of 
                     iterations and holes persist.
       -blend METH: Sets method for assigning a value to a hole.
                    MODE: Fill with most frequent neighbor value. Use MODE when
                          filling integral valued data such as ROIs or atlases.
                    AVG: Fill with average of neighboring values.
                    AUTO: Use MODE if DSET is integral, AVG otherwise.
                    SOLID: No blending, brutish fill. See also -minhits
                    SOLID_CLEAN: SOLID, followed by removal of dangling chunks
                                 Dangling chunks are defined as non-zero regions
                                 that surround lesser holes, i.e. holes that have
                                 less than MH. The cleanup step is not iterative
                                 though, and you are most likely better off using
                                 option -ed to do the cleanup.
       -minhits MH: Crietrion for considering a zero voxel to be a hole
                    MH refers to the total number of directions alogn which a
                    zero voxel is considered surrounded by non zero values.
                    a value of 1 is the least strict criterion, and a value of 3
                    is the strictest. 
                    This parameter can only be used with -blend SOLID
       -ed N V: Erode N times then dialate N times to get rid of hanging chunks.
                Values filled in by this process get value V.
       -mask MSET: Provide mask dataset to select subset of input.
       -mask_range BOT TOP: Specify the range of values to consider from MSET.
                            Default is anything non-zero.
       -mrange BOT TOP: Same as option -mask_range
       -cmask CMASK: Provide cmask expression. Voxels where expression is 0
                     are excluded from computations. For example:
                -cmask '-a T1.div.r+orig -b T1.uni.r+orig -expr step(a/b-10)'
       NOTE: For the moment, masking is only implemented for the SOLID* fill
             method.
    
    Example 1:
    Starting from a whole head mask that has some big holes in it where CSF and 
    cavities are. Fill the inside of the mask and remove dangling chunks in the
    end with -ed 
            3dinfill -blend SOLID -ed 3 1 -prefix filledmask \
                     -minhits 2 -input holymask+orig.  
    
    This program will be slow for high res datasets with large holes.
    If you are trying to fill holes in masks, consider also:
      3dmask_tool -fill_holes 
