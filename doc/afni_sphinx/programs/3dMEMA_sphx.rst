******
3dMEMA
******

.. _ahelp_3dMEMA:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:
    ------ 
     3dMEMA is a program for performing Mixed Effects Meta Analysis at group level 
     that models both within- and across- subjects variability, thereby requiring
     both regression coefficients, or general linear contrasts among them, and the 
     corresponding t-statistics from each subject as input. It's required to install 
     R (https://www.r-project.org/), plus 'snow' package if parallel computing is
     desirable. Version 1.0.1, Dec 21, 2016. If you want to cite the analysis
     approach, use the following at this moment:
    
     Chen et al., 2012. FMRI Group Analysis Combining Effect Estimates
     and Their Variances. NeuroImage. NeuroImage 60: 747-765.
     
     The basic usage of 3dMEMA is to derive group effects of a condition, contrast,
     or linear combination (GLT) of multiple conditions. It can be used to analyze
     data from one, two, or multiple groups. However, if there are more than two
     groups or more than one subject-grouping variables (e.g., sex, adolescent/adults,
     genotypes, etc.) involved in the analysis, dummy coding (zeros and ones) the 
     variables as covariates is required, and extremely caution should be exercised 
     in doing so because different coding strategy may lead to different 
     interpretation. In addition, covariates (quantiative variables) can be 
     incorporated in the model, but centering and potential interactions with other 
     effects in the model should be considered. 
     
     Basically, 3dMEMA can run one-sample, two-sample, and all types of BETWEEN-SUBJECTS
     ANOVA and ANCOVA. Within-subject variables mostly cannot be modeled, but there are 
     a few exceptions. For instance, paired-test can be performed through feeding the 
     contrast of the two conditons as input. Multi-way ANOVA can be analyzed under the
     following two scnearios: 1) all factors have only two levels (e.g., 2 X 2 repeated-
     measures ANOVA) can be analyzed; or 1) there is only one within-subject (or 
     repeated-measures) factor and it contains two levels only. See more details at
     
     https://afni.nimh.nih.gov/sscc/gangc/MEMA.html
    
     Notice:  When comparing two groups, option "-groups groupA groupB" has to be
     present, and the output includes the difference of groupB - groupA, which is
     consistent with most AFNI convention except for 3dttest++ where groupA - groupB is
     rendered.
    
    Example 1 --- One-sample type (one regression coefficient or general linear 
    contrast from each subject in a group):
    --------------------------------
          3dMEMA   -prefix ex1  \
                   -jobs 4      \
                   -set  happy  \
                      ac   ac+tlrc'[14]'   ac+tlrc'[15]'  \
                      ejk  ejk+tlrc'[14]'  ejk+tlrc'[15]' \
                      ...
                      ss   ss+tlrc'[14]'   ss+tlrc'[15]' \
                   -max_zeros 4    \
                   -model_outliers \        
                   -residual_Z        
    
          3dMEMA   -prefix ex1  \
                   -jobs 4      \
                   -set  happy  \
                      ac   ac+tlrc'[happy#0_Coef]'   ac+tlrc'[happy#0_Tstat]'  \
                      ejk  ejk+tlrc'[happy#0_Coef]'  ejk+tlrc'[happy#0_Tstat]' \
                      ...
                      ss   ss+tlrc'[happy#0_Coef]'   ss+tlrc'[happy#0_Tstat]' \
                   -missing_data 0  \
                   -HKtest         \        
                   -model_outliers \        
                   -residual_Z     
    
    Example 2 --- Two-sample type (one regression coefficient or general linear
    contrast from each subject in two groups with the constrast being the 2nd group 
    subtracing the 1st one), heteroskedasticity (different cross-subjects variability 
    between the two groups), outlier modeling, covariates centering, no payment no 
    interest till Memorial Day next year. Notice that option -groups has to be
    present in this case, and the output includes the difference of the second group
    versus the first one.
    -------------------------------------------------------------------------
       3dMEMA   -prefix ex3  \
                -jobs 4      \
                -groups horses goats  \
                -set   healthy_horses \
                    ac   ac_sad_B+tlrc.BRIK   ac_sad_T+tlrc.BRIK  \
                    ejk  ejk_sad_B+tlrc.BRIK  ejk_sad_T+tlrc.BRIK \
                    ...
                    ss   ss_sad_B+tlrc.BRIK   ss_sad_T+tlrc.BRIK  \
                -set   healthy_goats \
                    jp   jp_sad_B+tlrc.BRIK   jp_sad_T+tlrc.BRIK  \
                    mb   mb_sad_B+tlrc.BRIK   mb_sad_T+tlrc.BRIK  \
                    ...
                    trr  trr_sad_B+tlrc.BRIK  trr_sad_T+tlrc.BRIK \
                -n_nonzero 18   \
                -HKtest         \
                -model_outliers \
                -unequal_variance \
                -residual_Z     \
                -covariates CovFile.txt \
                -covariates_center age = 25 13 weight = 100 150  \
                -covariates_model center=different slope=same   
       
       where file CovFile.txt looks something like this:  
       
          name  age  weight
          ejk   93    117
          jcp   3     34
          ss    12    200   
          ac    12    130
          jp    65    130
          mb    25    630
          trr   18    187
          delb  9     67
          tony  12    4000
    
    
    Example 3 --- Paired type (difference of two regression coefficients or 
    general linear contrasts from each subject in a group). One scenario of 
    general linear combinations is to test linear or higher order trend at 
    individual level, and then take the trend information to group level.
    ---------------------------------
       3dMEMA   -prefix ex2  \
                -jobs 4      \
                -missing_data happyMiss+tlrc sadMiss+tlrc \
                -set happy-sad \
                    ac   ac_hap-sad_B+tlrc   ac_hap-sad_T+tlrc   \
                    ejk  ejk_hap-sad_B+tlrc  ejk_hap-sad_T+tlrc  \
                    ...
                    ss   ss_hap-sad_B+tlrc   ss_hap-sad_T+tlrc   \
                
    
    Options in alphabetical order:
    ------------------------------
    
       -cio: Use AFNI's C io functions
    
       -contrast_name: (no help available)
    
       -covariates COVAR_FILE: Specify the name of a text file containing
                             a table for the covariate(s). Each column in the
                             file is treated as a separate covariate, and each
                             row contains the values of these covariates for
                             each subject. Option -unequal_variance may not be
                             used in the presence of covariates with two groups.
          To avoid confusion, it is best you format COVAR_FILE in this manner
          with BOTH row and column names: 
             subj  age   weight
             Jane   25   300
             Joe    22   313
             ...    ..   ...
          This way, there is no amiguity as to which values are attributed to
          which subject, nor to the label of the covariate(s). The word 'subj'
          must be the first word of the first row. You can still get at the  
          values of the columns of such a file with AFNI's 1dcat -ok_text, 
          which will treat the first row, and first column, as all 0s.
          Alternate, but less recommended ways to specify the covariates:
          (column names only)
             age   weight
             25   300
             22   313
             ..   ...
          or
          (no row and column names)
             25   300
             22   313
             ..   ...
    
       -covariates_center COV_1=CEN_1 [COV_2=CEN_2 ... ]: (for 1 group) 
       -covariates_center COV_1=CEN_1.A CEN_1.B [COV_2=CEN_2.A CEN_2.B ... ]: 
                                                         (for 2 groups) 
         where COV_K is the name assigned to the K-th covariate, 
         either from the header of the covariates file, or from the option
         -covariates_name. This makes clear which center belongs to which
         covariate. When two groups are used, you need to specify a center for
         each of the groups (CEN_K.A, CEN_K.B).
         Example: If you had covariates age, and weight, you would use:
                -covariates_center age = 78 55 weight = 165 198
         If you want all covariates centered about their own mean, 
         just use -covariates_center mean. Be alert: Default is mean centering!
         If no centering is desired (e.g.,the covariate values have been
         pre-centered), set the center value as 0 with -covariates_center.
    
       -covariates_model center=different/same slope=different/same:
              Specify whether to use the same or different intercepts
              for each of the covariates. Similarly for the slope.
    
       -covariates_name COV_1 [... COV_N]: Specify the name of each of the N
                  covariates. This is only needed if the covariates' file 
                  has no header. The default is to name the covariates
                  cov1, cov2, ... 
    
       -dbgArgs: This option will enable R to save the parameters in a
             file called .3dMEMA.dbg.AFNI.args in the current directory
              so that debugging can be performed.
    
       -equal_variance: Assume same cross-subjects variability between GROUP1
                      and GROUP2 (homoskedasticity). (Default)
    
       -groups GROUP1 [GROUP2]: Name of 1 or 2 groups. This option must be used
                              when comparing two groups. Default is one group
                              named 'G1'. The labels here are used to name
                              the sub-bricks in the output. When there are
                              two groups, the 1st and 2nd labels here are
                              associated with the 1st and 2nd datasets
                              specified respectively through option -set,
                              and their group difference is the second group
                              minus the first one, similar to 3dttest but
                              different from 3dttest++.
    
       -help: this help message
    
       -HKtest: Perform Hartung-Knapp adjustment for the output t-statistic. 
              This approach is more robust when the number of subjects
              is small, and is generally preferred. -KHtest is the default 
              with t-statistic output.
    
       -jobs NJOBS: On a multi-processor machine, parallel computing will speed 
                 up the program significantly.
                 Choose 1 for a single-processor computer.
    
       -mask MASK: Process voxels inside this mask only.
                 Default is no masking.
    
       -max_zeros MM: Do not compute statistics at any voxel that has 
                    more than MM zero beta coefficients or GLTs. Voxels around
                    the edges of the group brain will not have data from
                    some of the subjects. Therefore, some of their beta's or
                    GLTs and t-stats are masked with 0. 3dMEMA can handle
                    missing data at those voxels but obviously too much
                    missing data is not good. Setting -max_zeros to 0.25
                    means process data only at voxels where no more than 1/4
                    of the data is missing. The default value is 0 (no
                    missing values allowed). MM can be a positive integer
                    less than the number of subjects, or a fraction 
                    between 0 and 1. Alternatively option -missing_data
                    can be used to handle missing data.
    
       -missing_data: This option corrects for inflated statistics for the voxels where
                   some subjects do not have any data available due to imperfect
                   spatial alignment or other reasons. The absence of this option
                   means no missing data will be assumed. Two formats of option
                   setting exist as shown below.
       -missing_data 0: With this format the zero value at a voxel of each subject
                     will be interpreted as missing data.
       -missing_data File1 [File2]: Information about missing data is specified
                                   with file of 1 or 2 groups (the number 1 or 2
                                   and file order should be consistent with those
                                   in option -groups). The voxel value of each file
                                   indicates the number of sujects with missing data
                                   in that group. 
    
       -model_outliers: Model outlier betas with a Laplace distribution of
                      of subject-specific error.
                      Default is -no_model_outliers
    
       -n_nonzero NN: Do not compute statistics at any voxel that has 
                    less than NN non-zero beta values. This options is
                    complimentary to -max_zeroes, and matches an option in
                    the interactive 3dMEMA mode. NN is basically (number of
                    unique subjects - MM). Alternatively option -missing_data
                    can be used to handle missing data.
    
       -no_HKtest: Do not make the Hartung-Knapp adjustment. -KHtest is 
              the default with t-statistic output.
    
       -no_model_outliers: No modeling of outlier betas/GLTs (Default).
    
       -no_residual_Z: Do not output residuals and their  Z values (Default).
    
       -prefix PREFIX: Output prefix (just prefix, no view+suffix needed)
    
       -residual_Z: Output residuals and their Z values used in identifying
                  outliers at voxel level.
                  Default is -no_residual_Z
    
       -Rio: Use R's io functions
    
       -set SETNAME                         \
                   SUBJ_1 BETA_DSET T_DSET \
                   SUBJ_2 BETA_DSET T_DSET \
                   ...   ...       ...     \
                   SUBJ_N BETA_DSET T_DSET \
          Specify the data for one of two test variables (either group,
                  contrast/GLTs) A & B. 
          SETNAME is the name assigned to the set, which is only for the
                  user's information, and not used by the program. When
                  there are two groups, the 1st and 2nd datasets are
                  associated with the 1st and 2nd labels specified
                  through option -set, and the group difference is
                  the second group minus the first one, similar to
                  3dttest but different from 3dttest++.
          SUBJ_K is the label for the subject K whose datasets will be 
                 listed next
          BETA_DSET is the name of the dataset of the beta coefficient or GLT.
          T_DSET is the name of the dataset containing the Tstat 
                 corresponding to BETA_DSET. 
             To specify BETA_DSET, and T_DSET, you can use the standard AFNI 
             notation, which, in addition to sub-brick indices, now allows for
             the use of sub-brick labels as selectors
          e.g: -set Placebo Jane pb05.Jane.Regression+tlrc'[face#0_Beta]'  \
                                 pb05.Jane.Regression+tlrc'[face#0_Tstat]' \
    
       -show_allowed_options: list of allowed options
    
       -unequal_variance: Model cross-subjects variability difference between
                        GROUP1 and GROUP2 (heteroskedasticity). This option
                        may NOT be invoked when covariate is present in the
                        model. Default is -equal_variance (homoskedasticity).
                        This option may not be useded when covariates are
                        involved in the model.
    
       -verb VERB: VERB is an integer specifying verbosity level.
                 0 for quiet (Default). 1 or more: talkative.
    
    #######################################################################
    Please consider citing the following if this program is useful for you:
    
       Chen et al., 2012. FMRI Group Analysis Combining Effect Estimates
       and Their Variances. NeuroImage. NeuroImage 60: 747-765.
       
       https://afni.nimh.nih.gov/sscc/gangc/MEMA.html
       
