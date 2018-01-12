.. contents:: 
    :depth: 4 

********
@Reorder
********

.. code-block:: none

    
        @Reorder    - like the Reorder plugin (only averages presently)
    
            Please see the Help from the Reorder plugin for details.
    
          * Note that labels are processed alphabetically.  So using labels
            such as A1,...,A4, B1,...B4 works as one would expect.
    
            The number of each label should be the same (according to the plugin).
            If not, this script will just issue WARNINGs.
    
            Labels at indices outside the valid sub-brick range will be ignored
            though whined about (SKIPPING).
    
            This script does its work in a temporary directory, which will be
            deleted unless the user supplies -save_work.
    
        examples:
    
            1. basic usage
    
               @Reorder -input EPI+tlrc -mapfile events.txt -prefix EPI.reorder
    
            2. shift all TRs by 3 (like adding 3 '-' lines to top of map file)
    
               @Reorder -input EPI+tlrc -mapfile events.txt -prefix EPI.reorder \
                        -offset 3
    
        options:
    
            -help           : show this help
    
            -input INSET    : (REQUIRED) input dataset to reorder
            -mapfile MFILE  : (REQUIRED) TR to event mapping
                              - see Reorder plugin Help for example
            -prefix PREFIX  : (REQUIRED) prefix for output dataset
            -offset OFFSET  : offset mapfile TR indices by OFFSET (in TRs)
            -save_work      : do not delete work directory (reorder.work.dir) at end
            -test           : just report sub-bricks, do not create datasets
    
        R Reynolds (for J Bjork) Sep 2009
