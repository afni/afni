********
3dANOVA3
********

.. _ahelp_3dANOVA3:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program performs three-factor ANOVA on 3D data sets.           
    
    Usage: 
    3dANOVA3 
    -type  k          type of ANOVA model to be used:
                             k = 1   A,B,C fixed;          AxBxC
                             k = 2   A,B,C random;         AxBxC
                             k = 3   A fixed; B,C random;  AxBxC
                             k = 4   A,B fixed; C random;  AxBxC
                             k = 5   A,B fixed; C random;  AxB,BxC,C(A)
    
    -alevels a                     a = number of levels of factor A
    -blevels b                     b = number of levels of factor B
    -clevels c                     c = number of levels of factor C
    -dset 1 1 1 filename           data set for level 1 of factor A
                                            and level 1 of factor B
                                            and level 1 of factor C
     . . .                           . . .
    
    -dset i j k filename           data set for level i of factor A
                                            and level j of factor B
                                            and level k of factor C
     . . .                           . . .
    
    -dset a b c filename           data set for level a of factor A
                                            and level b of factor B
                                            and level c of factor C
    
    [-voxel num]                   screen output for voxel # num
    [-diskspace]                   print out disk space required for
                                      program execution
    
    [-mask mset]                   use sub-brick #0 of dataset 'mset'
                                   to define which voxels to process
    
    
    The following commands generate individual AFNI 2 sub-brick datasets:
      (In each case, output is written to the file with the specified
       prefix file name.)
    
    [-fa prefix]                F-statistic for factor A effect
    [-fb prefix]                F-statistic for factor B effect
    [-fc prefix]                F-statistic for factor C effect
    [-fab prefix]               F-statistic for A*B interaction
    [-fac prefix]               F-statistic for A*C interaction
    [-fbc prefix]               F-statistic for B*C interaction
    [-fabc prefix]              F-statistic for A*B*C interaction
    
    [-amean i prefix]           estimate of factor A level i mean
    [-bmean i prefix]           estimate of factor B level i mean
    [-cmean i prefix]           estimate of factor C level i mean
    [-xmean i j k prefix]       estimate mean of cell at factor A level i,
                                   factor B level j, factor C level k
    
    [-adiff i j prefix]         difference between factor A levels i and j
                                   (with factors B and C collapsed)
    [-bdiff i j prefix]         difference between factor B levels i and j
                                   (with factors A and C collapsed)
    [-cdiff i j prefix]         difference between factor C levels i and j
                                   (with factors A and B collapsed)
    [-xdiff i j k l m n prefix] difference between cell mean at A=i,B=j,
                                   C=k, and cell mean at A=l,B=m,C=n
    
    [-acontr c1...ca prefix]    contrast in factor A levels
                                   (with factors B and C collapsed)
    [-bcontr c1...cb prefix]    contrast in factor B levels
                                   (with factors A and C collapsed)
    [-ccontr c1...cc prefix]    contrast in factor C levels
                                   (with factors A and B collapsed)
    
    [-aBcontr c1 ... ca : j prefix]   2nd order contrast in A, at fixed
                                         B level j (collapsed across C)
    [-Abcontr i : c1 ... cb prefix]   2nd order contrast in B, at fixed
                                         A level i (collapsed across C)
    
    [-aBdiff i_1 i_2 : j prefix] difference between levels i_1 and i_2 of
                                   factor A, with factor B fixed at level j
    
    [-Abdiff i : j_1 j_2 prefix] difference between levels j_1 and j_2 of
                                   factor B, with factor A fixed at level i
    
    [-abmean i j prefix]         mean effect at factor A level i and
                                   factor B level j
    
    The following command generates one AFNI 'bucket' type dataset:
    
    [-bucket prefix]         create one AFNI 'bucket' dataset whose
                               sub-bricks are obtained by concatenating
                               the above output files; the output 'bucket'
                               is written to file with prefix file name
    
    Modified ANOVA computation options:    (December, 2005)
    
         ** These options apply to model types 4 and 5, only.
            For details, see https://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html
    
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
    
    -----------------------------------------------------------------
    example: "classic" houses/faces/donuts for 4 subjects (2 genders)
             (level sets are gender (M/W), image (H/F/D), and subject)
    
        Note: factor C is really subject within gender (since it is
              nested).  There are 4 subjects in this example, and 2
              subjects per gender.  So clevels is 2.
    
        3dANOVA3 -type 5                            \
            -alevels 2                              \
            -blevels 3                              \
            -clevels 2                              \
            -dset 1 1 1 man1_houses+tlrc            \
            -dset 1 2 1 man1_faces+tlrc             \
            -dset 1 3 1 man1_donuts+tlrc            \
            -dset 1 1 2 man2_houses+tlrc            \
            -dset 1 2 2 man2_faces+tlrc             \
            -dset 1 3 2 man2_donuts+tlrc            \
            -dset 2 1 1 woman1_houses+tlrc          \
            -dset 2 2 1 woman1_faces+tlrc           \
            -dset 2 3 1 woman1_donuts+tlrc          \
            -dset 2 1 2 woman2_houses+tlrc          \
            -dset 2 2 2 woman2_faces+tlrc           \
            -dset 2 3 2 woman2_donuts+tlrc          \
            -adiff   1 2           MvsW             \
            -bdiff   2 3           FvsD             \
            -bcontr -0.5 1 -0.5    FvsHD            \
            -aBcontr 1 -1 : 1      MHvsWH           \
            -aBdiff  1  2 : 1      same_as_MHvsWH   \
            -Abcontr 2 : 0 1 -1    WFvsWD           \
            -Abdiff  2 : 2 3       same_as_WFvsWD   \
            -Abcontr 2 : 1 7 -4.2  goofy_example    \
            -bucket donut_anova
    
    
    N.B.: For this program, the user must specify 1 and only 1 sub-brick
          with each -dset command. That is, if an input dataset contains
          more than 1 sub-brick, a sub-brick selector must be used, e.g.:
          -dset 2 4 5 'fred+orig[3]'
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
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
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
