********
3dANOVA2
********

.. _3dANOVA2:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program performs a two-factor Analysis of Variance (ANOVA)
    on 3D datasets
    
    -----------------------------------------------------------
    
    Usage:
    
       3dANOVA2
          -type k              : type of ANOVA model to be used:
                                  k=1  fixed effects model  (A and B fixed)    
                                  k=2  random effects model (A and B random)   
                                  k=3  mixed effects model  (A fixed, B random)
    
          -alevels a           : a = number of levels of factor A
    
          -blevels b           : b = number of levels of factor B
    
          -dset 1 1 filename   : data set for level 1 of factor A
                                          and level 1 of factor B
                . . .                           . . .
          -dset i j filename   : data set for level i of factor A
                                          and level j of factor B
                . . .                           . . .
          -dset a b filename   : data set for level a of factor A
                                          and level b of factor B
    
         [-voxel num]          : screen output for voxel # num
    
         [-diskspace]          : print out disk space required for
                                 program execution
    
         [-mask mset]          : use sub-brick #0 of dataset 'mset'
                                 to define which voxels to process
    
    
       The following commands generate individual AFNI 2-sub-brick datasets:
      (In each case, output is written to the file with the specified
       prefix file name.)
    
         [-ftr prefix]         : F-statistic for treatment effect
    
         [-fa prefix]          : F-statistic for factor A effect
    
         [-fb prefix]          : F-statistic for factor B effect
    
         [-fab prefix]         : F-statistic for interaction
    
         [-amean i prefix]     : estimate mean of factor A level i
    
         [-bmean j prefix]     : estimate mean of factor B level j
    
         [-xmean i j prefix]   : estimate mean of cell at level i of factor A,
                                                          level j of factor B
    
         [-adiff i j prefix]   : difference between levels i and j of factor A
    
         [-bdiff i j prefix]   : difference between levels i and j of factor B
    
         [-xdiff i j k l prefix]     : difference between cell mean at A=i,B=j
                                                      and cell mean at A=k,B=l
    
         [-acontr c1 ... ca prefix]  : contrast in factor A levels
    
         [-bcontr c1 ... cb prefix]  : contrast in factor B levels
    
         [-xcontr c11 ... c1b c21 ... c2b  ...  ca1 ... cab  prefix]
                                     : contrast in cell means
    
    
    The following command generates one AFNI 'bucket' type dataset:
    
         [-bucket prefix]      : create one AFNI 'bucket' dataset whose
                                 sub-bricks are obtained by concatenating
                                 the above output files; the output 'bucket'
                                 is written to file with prefix file name
    
    Modified ANOVA computation options:    (December, 2005)
    
         ** These options apply to model type 3, only.
            For details, see https://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html
    
         [-old_method]        : request to perform ANOVA using the previous
                                functionality (requires -OK, also)
    
         [-OK]                : confirm you understand that contrasts that
                                do not sum to zero have inflated t-stats, and
                                contrasts that do sum to zero assume sphericity
                                (to be used with -old_method)
    
         [-assume_sph]        : assume sphericity (zero-sum contrasts, only)
    
                                This allows use of the old_method for
                                computing contrasts which sum to zero (this
                                includes diffs, for instance).  Any contrast
                                that does not sum to zero is invalid, and
                                cannot be used with this option (such as
                                ameans).
    
    ----------------------------------------------------------
    
     Example of 3dANOVA2:
    
          Example is based on a study with a 3 x 4 mixed factorial design:
    
                  Factor 1 - DONUTS has 3 levels:
                             (1) chocolate, (2) glazed, (3) sugar
    
                  Factor 2 - SUBJECTS, of which there are 4 in this analysis:
                             (1) fred, (2) ethel, (3) lucy, (4) ricky
    
     3dANOVA2 -type 3 -alevels 3 -blevels 4   \
              -dset 1 1 fred_choc+tlrc        \
              -dset 2 1 fred_glaz+tlrc        \
              -dset 3 1 fred_sugr+tlrc        \
              -dset 1 2 ethel_choc+tlrc       \
              -dset 2 2 ethel_glaz+tlrc       \
              -dset 3 2 ethel_sugr+tlrc       \
              -dset 1 3 lucy_choc+tlrc        \
              -dset 2 3 lucy_glaz+tlrc        \
              -dset 3 3 lucy_sugr+tlrc        \
              -dset 1 3 ricky_choc+tlrc       \
              -dset 2 3 ricky_glaz+tlrc       \
              -dset 3 3 ricky_sugr+tlrc       \
              -amean 1 Chocolate              \
              -amean 2 Glazed                 \
              -amean 3 Sugar                  \
              -adiff 1 2 CvsG                 \
              -adiff 2 3 GvsS                 \
              -adiff 1 3 CvsS                 \
              -acontr 1 1 -2 CGvsS            \
              -acontr -2 1 1 CvsGS            \
              -acontr 1 -2 1 CSvsG            \
              -fa Donuts                      \
              -bucket ANOVA_results
    
     The -bucket option will place all of the 3dANOVA2 results (i.e., main
     effect of DONUTS, means for each of the 3 levels of DONUTS, and
     contrasts between the 3 levels of DONUTS) into one big dataset with
     multiple sub-bricks called ANOVA_results+tlrc.
    
    -----------------------------------------------------------
    
    
    N.B.: For this program, the user must specify 1 and only 1 sub-brick
          with each -dset command. That is, if an input dataset contains
          more than 1 sub-brick, a sub-brick selector must be used, e.g.:
          -dset 2 4 'fred+orig[3]'
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    Also see HowTo #5: Group Analysis on the AFNI website:
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
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
