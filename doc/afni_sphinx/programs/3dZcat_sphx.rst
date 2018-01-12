.. contents:: 
    :depth: 4 

******
3dZcat
******

.. code-block:: none

    Usage: 3dZcat [options] dataset dataset ...
    Concatenates datasets in the slice (z) direction.  Each input
    dataset must have the same number of voxels in each slice, and
    must have the same number of sub-bricks.
    
    Options:
      -prefix pname = Use 'pname' for the output dataset prefix name.
                        [default='zcat']
      -datum type   = Coerce the output data to be stored as the given
                        type, which may be byte, short, or float.
      -fscale       = Force scaling of the output to the maximum integer
                        range.  This only has effect if the output datum
                        is byte or short (either forced or defaulted).
                        This option is sometimes necessary to eliminate
                        unpleasant truncation artifacts.
      -nscale       = Don't do any scaling on output to byte or short datasets.
                        This may be especially useful when operating on mask
                        datasets whose output values are only 0's and 1's.
      -verb         = Print out some verbositiness as the program proceeds.
      -frugal       = Be 'frugal' in the use of memory, at the cost of I/O time.
                        Only needed if the program runs out of memory.
                        Note frugality cannot be combined with NIFTI output
    
    Command line arguments after the above are taken as input datasets.
    
    Notes:
    * You can use the '3dinfo' program to see how many slices a
        dataset comprises.
    * There must be at least two datasets input (otherwise, the
        program doesn't make much sense, does it?).
    * Each input dataset must have the same number of voxels in each
        slice, and must have the same number of sub-bricks.
    * This program does not deal with complex-valued datasets.
    * See the output of '3dZcutup -help' for a C shell script that
        can be used to take a dataset apart into single slice datasets,
        analyze them separately, and then assemble the results into
        new 3D datasets.
    * Also see program 3dXYZcat for a version that can catenate along
        the x and y axes as well (with some limitations).
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
