.. contents:: 
    :depth: 4 

*******
3dANOVA
*******

.. code-block:: none

    This program performs single factor Analysis of Variance (ANOVA)
    on 3D datasets
    
    ---------------------------------------------------------------
    
    Usage:
    -----
    
    3dANOVA
       -levels r                   : r = number of factor levels
    
       -dset 1 filename            : data set for factor level 1
             . . .. . .
       -dset 1 filename              data set for factor level 1
             . . .. . .
       -dset r filename              data set for factor level r
             . . .. . .
       -dset r filename              data set for factor level r
    
      [-voxel num]                 : screen output for voxel # num
    
      [-diskspace]                 : print out disk space required for
                                     program execution
    
      [-mask mset]                 : use sub-brick #0 of dataset 'mset'
                                     to define which voxels to process
    
      [-debug level]               : request extra output
    
    The following commands generate individual AFNI 2-sub-brick datasets:
      (In each case, output is written to the file with the specified
       prefix file name.)
    
      [-ftr prefix]                : F-statistic for treatment effect
    
      [-mean i prefix]             : estimate of factor level i mean
    
      [-diff i j prefix]           : difference between factor levels
    
      [-contr c1...cr prefix]      : contrast in factor levels
    
    Modified ANOVA computation options:    (December, 2005)
    
         ** For details, see https://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html
    
    [-old_method]       request to perform ANOVA using the previous
                        functionality (requires -OK, also)
    
    [-OK]               confirm you understand that contrasts that
                        do not sum to zero have inflated t-stats, and
                        contrasts that do sum to zero assume sphericity
                        (to be used with -old_method)
    
    [-assume_sph]       assume sphericity (zero-sum contrasts, only)
    
                        This allows use of the old_method for
                        computing contrasts which sum to zero (this
                        includes diffs, for instance).  Any contrast
                        that does not sum to zero is invalid, and
                        cannot be used with this option (such as
                        ameans).
    
    The following command generates one AFNI 'bucket' type dataset:
    
      [-bucket prefix]             : create one AFNI 'bucket' dataset whose
                                     sub-bricks are obtained by
                                     concatenating the above output files;
                                     the output 'bucket' is written to file
                                     with prefix file name
    
    N.B.: For this program, the user must specify 1 and only 1 sub-brick
          with each -dset command. That is, if an input dataset contains
          more than 1 sub-brick, a sub-brick selector must be used,
          e.g., -dset 2 'fred+orig[3]'
    
    Example of 3dANOVA:
    ------------------
    
     Example is based on a study with one factor (independent variable)
     called 'Pictures', with 3 levels:
            (1) Faces, (2) Houses, and (3) Donuts
    
     The ANOVA is being conducted on the data of subjects Fred and Ethel:
    
     3dANOVA -levels 3                     \
             -dset 1 fred_Faces+tlrc       \
             -dset 1 ethel_Faces+tlrc      \
                                           \
             -dset 2 fred_Houses+tlrc      \
             -dset 2 ethel_Houses+tlrc     \
                                           \
             -dset 3 fred_Donuts+tlrc      \
             -dset 3 ethel_Donuts+tlrc     \
                                           \
             -ftr Pictures                 \
             -mean 1 Faces                 \
             -mean 2 Houses                \
             -mean 3 Donuts                \
             -diff 1 2 FvsH                \
             -diff 2 3 HvsD                \
             -diff 1 3 FvsD                \
             -contr  1  1 -1 FHvsD         \
             -contr -1  1  1 FvsHD         \
             -contr  1 -1  1 FDvsH         \
             -bucket fred_n_ethel_ANOVA
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    ---------------------------------------------------
    Also see HowTo#5 - Group Analysis on the AFNI website:
    https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht05_group/html/index.shtml
    
    -------------------------------------------------------------------------
    STORAGE FORMAT:
    ---------------
    The default output format is to store the results as scaled short
    (16-bit) integers.  This truncantion might cause significant errors.
    If you receive warnings that look like this:
      *+ WARNING: TvsF[0] scale to shorts misfit = 8.09% -- *** Beware
    then you can force the results to be saved in float format by
    defining the environment variable AFNI_FLOATIZE to be YES
    before running the program.  For convenience, you can do this
    on the command line, as in
      3dANOVA -DAFNI_FLOATIZE=YES ... other options ... 
    Also see the following links:
     https://afni.nimh.nih.gov/pub/dist/doc/program_help/common_options.html
     https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
