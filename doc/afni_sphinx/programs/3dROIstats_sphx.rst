**********
3dROIstats
**********

.. _ahelp_3dROIstats:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dROIstats -mask[n] mset [options] datasets
    
       Display statistics over masked regions.  The default statistic
       is the mean.
    
       There will be one line of output for every sub-brick of every
       input dataset.  Across each line will be every statistic for
       every mask value.  For instance, if there 3 mask values (1,2,3),
       then the columns Mean_1, Mean_2 and Mean_3 will refer to the
       means across each mask value, respectively.  If 4 statistics are
       requested, then there will be 12 stats displayed on each line
       (4 for each mask region), besides the file and sub-brick number.
    
    Examples:
    
       3dROIstats -mask mask+orig. 'func_slim+orig[1,3,5]'
    
       3dROIstats -minmax -sigma -mask mask+orig. 'func_slim+orig[1,3,5]'
    
    Options:
      -mask[n] mset Means to use the dataset 'mset' as a mask:
                     If n is present, it specifies which sub-brick
                     in mset to use a la 3dcalc.  Note: do not include
                     the brackets if specifying a sub-brick, they are
                     there to indicate that they are optional.  If not
                     present, 0 is assumed
                     Voxels with the same nonzero values in 'mset'
                     will be statisticized from 'dataset'.  This will
                     be repeated for all the different values in mset.
                     I.e. all of the 1s in mset are one ROI, as are all
                     of the 2s, etc.
                     Note that the mask dataset and the input dataset
                     must have the same number of voxels and that mset
                     must be BYTE or SHORT (i.e., float masks won't work
                     without the -mask_f2short option).
                     
      -mask_f2short  Tells the program to convert a float mask to short
                     integers, by simple rounding.  This option is needed
                     when the mask dataset is a 1D file, for instance
                     (since 1D files are read as floats).
    
                     Be careful with this, it may not be appropriate to do!
    
      -numROI n     Forces the assumption that the mask dataset's ROIs are
                     denoted by 1 to n inclusive.  Normally, the program
                     figures out the ROIs on its own.  This option is 
                     useful if a) you are certain that the mask dataset
                     has no values outside the range [0 n], b) there may 
                     be some ROIs missing between [1 n] in the mask data-
                     set and c) you want those columns in the output any-
                     way so the output lines up with the output from other
                     invocations of 3dROIstats.  Confused?  Then don't use
                     this option!
      -zerofill ZF   For ROI labels not found, use 'ZF' instead of a blank
                     in the output file. This option is useless without -numROI.
                     The option -zerofill defaults to '0'.
    
      -roisel SEL.1D Only considers ROIs denoted by values found in SEL.1D
                     Note that the order of the ROIs as specified in SEL.1D
                     is not preserved. So an SEL.1D of '2 8 20' produces the
                     same output as '8 20 2'
    
      -debug        Print out debugging information
      -quiet        Do not print out labels for columns or rows
      -nomeanout    Do not print out the mean column. Default is 
                    to always start with the mean value.
                    This option cannot be used with -summary
      -nobriklab    Do not print the sub-brick label next to its index
      -1Dformat     Output results in a 1D format that includes 
                    commented labels
      -1DRformat    Output results in a 1D format that includes 
                    uncommented labels. This format does not work well with 
                    typical 1D programs, but it is useful for R functions.
    
    The following options specify what stats are computed.  By default
    the mean is always computed.
    
      -nzmean       Compute the mean using only non_zero voxels.  Implies
                     the opposite for the normal mean computed
      -nzsum        Compute the sum using only non_zero voxels.  
      -nzvoxels     Compute the number of non_zero voxels
      -minmax       Compute the min/max of all voxels
      -nzminmax     Compute the min/max of non_zero voxels
      -sigma        Compute the standard deviation of all voxels
      -nzsigma      Compute the standard deviation of all non_zero voxels
      -median       Compute the median of all voxels.
      -nzmedian     Compute the median of non_zero voxels.
      -summary      Only output a summary line with the grand mean 
                    across all briks in the input dataset. 
                    This option cannot be used with -nomeanout.
      -mode       Compute the mode of all voxels. (integral valued sets only)
      -nzmode     Compute the mode of non_zero voxels.
      -pcxyz      Compute the principal direction of the voxels in the ROI 
                  including the three eigen values. You'll get 12 values out
                  per ROI, per sub-brick, with this option.
                     pc0x pc0y pc0z pc1x pc1y pc1z pc2x pc2y pc2z eig0 eig1 eig2
      -nzpcxyz    Same as -pcxyz, but exclude zero valued voxels.
      -pcxyz+     Same as -pcxyz, but also with FA, MD, Cl, Cp, and Cs computed 
                  from the three eigen values.
                  You will get 17 values out per ROI, per sub-brick, beginning
                  with all the values from -pcxyz and -nzpcxyz then followed by
                     FA MD Cl Cp Cs
      -nzpcxyz+   Same as -nzpcxyz, but also with FA, MD, Cl, Cp, and Cs.
      -key        Output the integer key for the ROI in question
    
    The output is printed to stdout (the terminal), and can be
    saved to a file using the usual redirection operation '>'.
    
    N.B.: The input datasets and the mask dataset can use sub-brick
          selectors, as detailed in the output of 3dcalc -help.
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
