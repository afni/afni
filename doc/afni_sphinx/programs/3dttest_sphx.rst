*******
3dttest
*******

.. _ahelp_3dttest:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Gosset (Student) t-test sets of 3D datasets
    
       * Also see the newer program 3dttest++, which lets you *
      ** include covariates to be regressed out of the data.  **
     *** For most purposes, 3dttest++ is to be preferred over ***
    **** this program -- 3dttest will no longer be upgraded.  ****
    ****------------------------------------------------------****
     *** Also consider program 3dMEMA, which can carry out a  ***
      ** more sophisticated type of 't-test' that also takes  **
       * into account the variance map of each input dataset. *
    
    -----------------------------------------------------------
    *********** In short: DO NOT USE THIS PROGRAM! ************
    -----------------------------------------------------------
    
    Usage 1: 3dttest [options] -set1 datasets ... -set2 datasets ...
       for comparing the means of 2 sets of datasets (voxel by voxel).
    
    Usage 2: 3dttest [options] -base1 bval -set2 datasets ...
       for comparing the mean of 1 set of datasets against a constant.
    
       ** or use -base1_dset
    
    OUTPUTS:
     A single dataset is created that is the voxel-by-voxel difference
     of the mean of set2 minus the mean of set1 (or minus 'bval').
     The output dataset will be of the intensity+Ttest ('fitt') type.
     The t-statistic at each voxel can be used as an interactive
     thresholding tool in AFNI.
    
    t-TESTING OPTIONS:
      -set1 datasets ... = Specifies the collection of datasets to put into
                             the first set. The mean of set1 will be tested
                             with a 2-sample t-test against the mean of set2.
                       N.B.: -set1 and -base1 are mutually exclusive!
      -base1 bval        = 'bval' is a numerical value that the mean of set2
                             will be tested against with a 1-sample t-test.
      -base1_dset DSET   = Similar to -base1, but input a dataset where bval
                             can vary over voxels.
      -sdn1  sd n1       = If this option is given along with '-base1', then
                             'bval' is taken to have standard deviation 'sd'
                             computed from 'n1' samples.  In this case, each
                             voxel in set2 is compared to bval using a
                             pooled-variance unpaired 2-sample t-test.
                             [This is for Tom Johnstone; hope we meet someday.]
      -set2 datasets ... = Specifies the collection of datasets to put into
                             the second set.  There must be at least 2 datasets
                             in each of set1 (if used) and set2.
      -paired            = Specifies the use of a paired-sample t-test to
                             compare set1 and set2.  If this option is used,
                             set1 and set2 must have the same cardinality.
                       N.B.: A paired test is intended for use when the set1 and set2
                             dataset function values may be pairwise correlated.
                             If they are in fact uncorrelated, this test has less
                             statistical 'power' than the unpaired (default) t-test.
                             This loss of power is the price that is paid for
                             insurance against pairwise correlations.
      -unpooled          = Specifies that the variance estimates for set1 and
                             set2 be computed separately (not pooled together).
                             This only makes sense if -paired is NOT given.
                       N.B.: If this option is used, the number of degrees
                             of freedom per voxel is a variable, rather
                             than a constant.
      -dof_prefix ddd    = If '-unpooled' is also used, then a dataset with
                             prefix 'ddd' will be created that contains the
                             degrees of freedom (DOF) in each voxel.
                             You can convert the t-value in the -prefix
                             dataset to a z-score using the -dof_prefix dataset
                             using commands like so:
               3dcalc -a 'pname+orig[1]' -b ddd+orig \
                      -datum float -prefix ddd_zz -expr 'fitt_t2z(a,b)'
               3drefit -substatpar 0 fizt ddd_zz+orig
                             At present, AFNI is incapable of directly dealing
                             with datasets whose DOF parameter varies between
                             voxels.  Converting to a z-score (with no parameters)
                             is one way of getting around this difficulty.
    
      -voxel voxel       = like 3dANOVA, get screen output for a given voxel.
                             This is 1-based, as with 3dANOVA.
    
    The -base1 or -set1 command line switches must follow all other options
    (including those described below) except for the -set2 switch.
    
    INPUT EDITING OPTIONS: The same as are available in 3dmerge.
    
    OUTPUT OPTIONS: these options control the output files.
      -session  dirname  = Write output into given directory (default=./)
      -prefix   pname    = Use 'pname' for the output directory prefix
                           (default=tdif)
      -datum    type     = Use 'type' to store the output difference
                           in the means; 'type' may be short or float.
                           How the default is determined is described
                           in the notes below.
    
    NOTES:
     ** The input datasets are specified by their .HEAD files,
          but their .BRIK files must exist also! This program cannot
          'warp-on-demand' from other datasets.
     ** This program cannot deal with time-dependent or complex-valued datasets!
     ** By default, the output dataset function values will be shorts if the
          first input dataset is byte- or short-valued; otherwise they will be
          floats.  This behavior may be overridden using the -datum option.
     ** In the -set1/-set2 input list, you can specify a collection of
          sub-bricks from a single dataset using a notation like
            datasetname+orig'[5-9]'
          (the single quotes are necessary).  If you want to use ALL the
          sub-bricks from a multi-volume dataset, you can't just give the
          dataset filename -- you have to use
            datasetname+orig'[0-$]' or datasetname'[0..$]'
          Otherwise, the program will reject the dataset as being too
          complicated for its pitiful understanding.  [New in July 2007]
    
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
