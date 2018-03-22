***********
@DiceMetric
***********

.. _ahelp_@DiceMetric:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 
    @DiceMetric <-base BASE> <-dsets DSET1 [DSET2 ...]> 
                  [max_N_roi MAX_ROI]  
                  [-keep_tmp]
    
    Computes Dice Metric between BASE, and each of DSET volumes
    
    Mandatory parameters:
    <-base BASE>: Name of base (reference) segmentation 
    <-dsets DSET1 [DSET2 ...]>: Data sets for which the Dice Metric with 
                                BASE is computed.
                                This option is to be the last on the command 
                                line.
    
      NOTE: A lazy usage would be:
      @DiceMetric BASE DSET
    
    Optional parameters:
       [-save_match] : Save volume showing BASE*equals(BASE,DSET)
       [-save_diff ] : Save volume showing BASE*(1-equals(BASE,DSET))
              These two options are off by default. The output filenames
              are formed this way:
                 PATH_DSET/PREFIX_BASE.EQ.PREFIX_DSET
                 PATH_DSET/PREFIX_BASE.DF.PREFIX_DSET
       [-max_N_roi MAX_ROI]: The maximum possible roi index. Default is 12
                             or based on LTFILE if specified
       [-labeltable LTFILE]: If given, the labeltable is used to set the 
                             default MAX_ROI parameter
                             Also, this option forces an output for each
                             key in the LTFILE
       [-forceoutput LTFILE]: If given force output for each class in LTFILE
       [-do_not_mask_by_base]: Do not mask dset by step(base) before computing
                               Dice coefficient. This is the default behaviour
                               for backward compatibility. 
       [-mask_by_base]: Mask dset by the step(base) before computing
                               Dice coefficient. With this option,
                               Voxels that are 0 in the base dataset are not
                               considered in the computations.
    
       [-prefix PREFIX]: Use PREFIX for the output table. 
                         Default is separate results for each dset to stdout
       [-ignore_bad]: Warn if encountering bad scenarios, but do not create
                      a zero entry. You should check for the cause of the
                      warnings to be sure they are OK to ignore
       [-keep_tmp]: Keep temporary files for debugging. Note that you should
                    delete temporary files before rerunning the script.
       [-echo]    : set echo 
    
    Ziad Saad (saadz@mail.nih.gov)
    SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
