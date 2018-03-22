*****************
@thickness_master
*****************

.. _ahelp_@thickness_master:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    @thickness_master
    usage:
    @thickness_master -maskset maskset -surfset surfacedset.gii -outdir basethickdir
    
    where maskset is the dataset to find thickness
     using the largest non-zero value in the mask.
     If dataset has values -2,-1 and 1 for different regions, this script
     calculates the thickness only for voxels with a value of 1
    surfset is a surface to use to find normals into the volume
    outdirbase is in directory thickdirbase_.... If not specified, the default is thick
    
    This script calls the three types of thickness scripts
      @measure_bb_thick - ball and box method
      @measure_erosion_thick - erosion method
      @measure_in2out_thick - in2out method
    
    Main options:
      -maskset mydset      mask dataset for input
      -surfset mydset.gii  surface dataset onto which to map thickness
                           (probably a pial/gray matter surface)
      -outdir thick_base output directory basename. The output will be placed
                     in a directory with thick_base in its name: 
                        mmmm_bb, mmmm_erode, mmmm_in2out
    
    Other options:
    
      takes all options from the three @measure_... scripts
    
    Output:
       see Output section of help for each of the method scripts
       This script produces a quick visualization script to see
       thickness maps in suma for all three methods
    See related scripts and programs for computing thickness:
