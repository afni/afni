.. contents:: 
    :depth: 4 

*****
3dLME
*****

.. code-block:: none

    
              ================== Welcome to 3dLME ==================          
        AFNI Group Analysis Program with Multi-Variate Modeling Approach
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Version 1.9.6, May 19, 2017
    Author: Gang Chen (gangchen@mail.nih.gov)
    Website - https://afni.nimh.nih.gov/sscc/gangc/lme.html
    SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    Usage:
    ------ 
     3dLME is a group-analysis program that performs linear mixed-effects (LME) 
     modeling analysis. One simple criterion to decide whether 3dLME is appropriate
     is that each subject has to have two or more measurements at each spatial 
     location (except for a small portion of subjects with missing data). In other
     words, at least one within-subject (or repeated-measures) factor serves as
     explanatory variable.
     
     F-statistics for main effects and interactions are automatically included in 
     the output for all variables. In addition, Student t-tests for quantitative 
     variables are also in the output. In addition, general linear tests (GLTs) can 
     be requested via symbolic coding.
     
     If you want to cite the analysis approach, use the following:
     
     Chen, G., Saad, Z.S., Britton, J.C., Pine, D.S., Cox, R.W. (2013). Linear
     Mixed-Effects Modeling Approach to FMRI Group Analysis. NeuroImage 73:176-190.
     http://dx.doi.org/10.1016/j.neuroimage.2013.01.047
     
     Input files for 3dLME can be in AFNI, NIfTI, or surface (niml.dset) format.
     
     In addition to R installtion, the following two R packages need to be acquired
     in R first before running 3dLME: "nlme", "lme4" and "phia". In addition, the "snow"
     package is also needed if one wants to take advantage of parallel computing.
     To install these packages, run the following command at the terminal:
    
     rPkgsInstall -pkgs ALL
    
     Alternatively you may install them in R:
     
     install.packages("nlme")
     install.packages("lme4")
     install.packages("phia")
     install.packages("snow")
     
     More details about 3dLME can be found at 
     https://afni.nimh.nih.gov/sscc/gangc/LME.html
    
     Once the 3dLME command script is constructed, it can be run by copying and
     pasting to the terminal. Alternatively (and probably better) you save the 
     script as a text file, for example, called LME.txt, and execute it with the 
     following (assuming on tc shell),
     
     tcsh -x LME.txt &
     
     or,
     
     tcsh -x LME.txt > diary.txt &
     tcsh -x LME.txt |& tee diary.txt &
    
     The advantage of the latter command is that the progression is saved into
     the text file diary.txt and, if anything goes awry, can be examined later.
     
     Thanks to the R community, Henrik Singmann and Helios de Rosario for the strong
     technical support.
    
    Example 1 --- one condition modeled with 8 basis functions (e.g., TENT or TENTzero)
    for one group of 13 subjects:
    --------------------------------
       3dLME -prefix myOutput -jobs   4               \
             -model '0+Time'                        \
             -qVars order                           \
             -qVarCenters 0                         \
             -ranEff '~1'                              \
             -corStr 'order : AR1'                  \
             -SS_type 3                             \
             -num_glf 1                             \
             -glfLabel 1 4TimePoints -glfCode 1 'Time : 1*Diff2 & 1*Diff3 & 1*Diff4 & 1*Diff5' \
             -dataTable                             \
             Subj   Time  order InputFile           \
             c101   Diff0 0 testData/c101time0+tlrc \
             c101   Diff1 1 testData/c101time1+tlrc \
             c101   Diff2 2 testData/c101time2+tlrc \
             c101   Diff3 3 testData/c101time3+tlrc \
             c101   Diff4 4 testData/c101time4+tlrc \
             c101   Diff5 5 testData/c101time5+tlrc \
             c101   Diff6 6 testData/c101time6+tlrc \
             c101   Diff7 7 testData/c101time7+tlrc \
             c103   Diff0 0 testData/c103time0+tlrc \
             c103   Diff1 1 testData/c103time1+tlrc \
             ...
         
    
    Example 2 --- one within-subject factor (conditions: House and Face), one
    within-subject quantitative variable (reaction time, RT) and one between-
    subjects covariate (age). RT values don't differ significantly between the
    two conditions, and thus are centered via grand mean. Random effects are
    intercept and RT effect whose correlation is estimated from the data.
    -------------------------------------------------------------------------
       3dLME -prefix Example2 -jobs 24                                         \
              -model  "cond*RT+age"                                            \
              -qVars "RT,age"                                                  \
              -qVarCenters "105.35,34.7"                                       \
              -ranEff '~1+RT'                                                  \
              -SS_type 3                                                       \
              -num_glt 4                                                       \
              -gltLabel 1 'House' -gltCode  1 'cond : 1*House'    \
              -gltLabel 2 'Face-House' -gltCode  2 'cond : 1*Face -1*House'    \
              -gltLabel 3 'House-AgeEff' -gltCode  3 'cond : 1*House age :'    \
              -gltLabel 4 'House-Age2' -gltCode  4 'cond : 1*House age : 5.3'    \
              -num_glf 1                                                       \
              -glfLabel 1 'cond_age' -glfCode  1 'cond : 1*House & 1*Face age :'    \
              -dataTable                                                       \
              Subj  cond        RT   age        InputFile                      \
              s1    House      124   35  s1+tlrc'[House#0_Coef]'               \
              s2    House       97   51  s2+tlrc'[House#0_Coef]'               \
              s3    House      107   25  s3+tlrc'[House#0_Coef]'               \
              ... 
              s1    Face       110   35  s1+tlrc'[Face#0_Coef]'                \
              s2    Face        95   51  s2+tlrc'[Face#0_Coef]'                \
              s3    Face       120   25  s3+tlrc'[Face#0_Coef]'                \
              ...                                   
       
    
    Example 3 --- one within-subject factor (conditions: positive, negative,
    and neutral), and one between-subjects factors (groups: control and patients).
    Effect estimates for a few subjects are available for only one or two
    conditions. These subjects with missing data would have to be abandoned in
    the traditional ANOVA approach. All subjects can be included with 3dLME, and
    a random intercept is considered.
    -------------------------------------------------------------------------
       3dLME -prefix Example3 -jobs 24                                     \
              -model  "cond*group"                                         \
              -ranEff '~1'                                                 \
              -SS_type 3                                                   \
              -num_glt 6                                                   \
              -gltLabel 1 'pos-neu' -gltCode  1 'cond : 1*pos -1*neu'      \
              -gltLabel 2 'neg' -gltCode  2 'cond : 1*neg '      \
              -gltLabel 3 'pos+nue-neg' -gltCode  3 'cond : 1*pos +1*neu -1*neg'      \
              -gltLabel 4 'pat_pos-neu' -gltCode  4 'cond : 1*pos -1*neu group : 1*pat'    \
              -gltLabel 5 'pat_neg-neu' -gltCode  5 'cond : 1*neg -1*neu group : 1*pat'    \
              -gltLabel 6 'pat_pos-neg' -gltCode  6 'cond : 1*pos -1*neg group : 1*pat'    \
              -num_glf 1                                                   \
              -glfLabel 1 'pos-neu' -glfCode  1 'Group : 1*ctr & 1*pat cond : 1*pos -1*neu & 1*pos -1*neg'      \
              -dataTable                                                  \
              Subj  cond      group        InputFile                      \
              s1    pos        ctr    s1+tlrc'[pos#0_Coef]'               \
              s1    neg        ctr    s1+tlrc'[neg#0_Coef]'               \
              s1    neu        ctr    s1+tlrc'[neu#0_Coef]'               \
              ... 
              s21   pos        pat   s21+tlrc'[pos#0_Coef]'               \
              s21   neg        pat   s21+tlrc'[neg#0_Coef]'               \
              s21   neu        pat   s21+tlrc'[neu#0_Coef]'               \
              ...                                   
       
    
    Example 4 --- Computing ICC values for two within-subject factor (Cond:
    positive, negative, and neutral; Scanner: one, and two) plus subjects (factor
    Subj).
    -------------------------------------------------------------------------
       3dLME -prefix Example4 -jobs 12                                      \
              -model  "1"                                                   \
              -ranEff 'Cond+Scanner+Subj'                                   \
              -ICCb                                                         \
              -dataTable                                                    \
              Subj  Cond      Scanner        InputFile                      \
              s1    pos        one    s1_1+tlrc'[pos#0_Coef]'               \
              s1    neg        one    s1_1+tlrc'[neg#0_Coef]'               \
              s1    neu        one    s1_1+tlrc'[neu#0_Coef]'               \
              s1    pos        two    s1_2+tlrc'[pos#0_Coef]'               \
              s1    neg        two    s1_2+tlrc'[neg#0_Coef]'               \
              s1    neu        two    s1_2+tlrc'[neu#0_Coef]'               \
              ... 
              s21   pos        two   s21_2+tlrc'[pos#0_Coef]'               \
              s21   neg        two   s21_2+tlrc'[neg#0_Coef]'               \
              s21   neu        two   s21_2+tlrc'[neu#0_Coef]'               \
              ...                                   
       
    
    Options in alphabetical order:
    ------------------------------
    
       -cio: Use AFNI's C io functions, which is default. Alternatively -Rio
             can be used.
    
       -corStr FORMULA: Specify the correlation structure of the residuals. For example,
             when analyzing the effect estimates from multiple basis functions,
             one may consider account for the temporal structure of residuals with
             AR or ARMA.
     
       -cutoff threshold: Specify the cutoff value to obtain voxel-wise accuracy
             in logistic regression analysis. Default is 0 (no accuracy will
             be estimated).
    
       -dataTable TABLE: List the data structure with a header as the first line.
    
             NOTE:
    
             1) This option has to occur last; that is, no other options are
             allowed thereafter. Each line should end with a backslash except for
             the last line.
    
             2) The first column is fixed and reserved with label 'Subj', and the
             last is reserved for 'InputFile'. Each row should contain only one
             effect estimate in the table of long format (cf. wide format) as
             defined in R. The level labels of a factor should contain at least
             one character. Input files can be in AFNI, NIfTI or surface format.
             AFNI files can be specified with sub-brick selector (square brackets
             [] within quotes) specified with a number or label.
    
             3) It is fine to have variables (or columns) in the table that are
             not modeled in the analysis.
    
             4) The context of the table can be saved as a separate file, e.g.,
             called table.txt. Do not forget to include a backslash at the end of
             each row. In the script specify the data with '-dataTable @table.txt'.
             This option is useful: (a) when there are many input files so that
             the program complains with an 'Arg list too long' error; (b) when
             you want to try different models with the same dataset.
    
       -dbgArgs: This option will enable R to save the parameters in a
             file called .3dLME.dbg.AFNI.args in the current directory
              so that debugging can be performed.
    
       -glfCode k CODING: Specify the k-th general linear F-test (GLF) through a
             weighted combination among factor levels. The symbolic coding has
             to be within (single or double) quotes. For example, the coding
             'Condition : 1*A -1*B & 1*A -1*C Emotion : 1*pos' tests the main
             effect of Condition at the positive Emotion. Similarly the coding
             'Condition : 1*A -1*B & 1*A -1*C Emotion : 1*pos -1*neg' shows
             the interaction between the three levels of Condition and the two.
             levels of Emotion.
    
             NOTE:
    
             1) The weights for a variable do not have to add up to 0.
    
             2) When a quantitative variable is present, other effects are
             tested at the center value of the covariate unless the covariate
             value is specified as, for example, 'Group : 1*Old Age : 2', where
             the Old Group is tested at the Age of 2 above the center.
    
             3)  The absence of a categorical variable in a coding means the
             levels of that factor are averaged (or collapsed) for the GLF.
    
             4) The appearance of a categorical variable has to be followed
             by the linear combination of its levels.
    
       -glfLabel k label: Specify the label for the k-th general linear F-test
             (GLF). A symbolic coding for the GLF is assumed to follow with
             each -glfLabel.
    
       -gltCode k CODING: Specify the k-th general linear test (GLT) through a
             weighted combination among factor levels. The symbolic coding has
             to be within (single or double) quotes. For example, the following
             'Condition : 2*House -3*Face Emotion : 1*positive '
             requests for a test of comparing 2 times House condition
             with 3 times Face condition while Emotion is held at positive
             valence.
    
             NOTE:
    
             1) The weights for a variable do not have to add up to 0.
    
             2) When a quantitative variable is present, other effects are
             tested at the center value of the covariate unless the covariate
             value is specified as, for example, 'Group : 1*Old Age : 2', where
             the Old Group is tested at the Age of 2 above the center.
    
             3) The effect for a quantitative variable can be specified with,
             for example, 'Group : 1*Old Age : ', or 
             'Group : 1*Old - 1*Young Age : '
    
             4) The absence of a categorical variable in a coding means the
             levels of that factor are averaged (or collapsed) for the GLT.
    
             5) The appearance of a categorial variable has to be followed
             by the linear combination of its levels. Only a quantitative
             is allowed to have a dangling coding as seen in 'Age :'
    
       -gltLabel k label: Specify the label for the k-th general linear test
             (GLT). A symbolic coding for the GLT is assumed to follow with
             each -gltLabel.
    
       -help: this help message
    
       -ICC: This option allows 3dLME to compute voxel-wise intra-class correlation
             for the variables specified through option -ranEff. See Example 4 in
             in the help.
     
       -ICCb: This option allows 3dLME to compute voxel-wise intra-class correlation
             through a Bayesian approach with Gamma priors for the variables
             specified through option -ranEff. The computation will take much
             longer due the sophistication involved. However, the Bayesian method is
             preferred to the old approach with -ICC for the typical FMRI data. R
             package 'blme' is required for this option.
     
       -jobs NJOBS: On a multi-processor machine, parallel computing will speed 
             up the program significantly.
             Choose 1 for a single-processor computer.
    
       -LOGIT: This option allows 3dLME to perform voxel-wise logistic modeling.
            Currently no random effects are allowed ('-ranEff NA'), but this
            limitation can be removed later if demand occurs. The InputFile
            column is expected to list subjects' responses in 0s and 1s. In
            addition, one voxel-wise covariate is currently allowed. Each
            regression coefficient (including the intercept) and its z-statistic
            are saved in the output.
     
       -logLik: Add this option if the voxel-wise log likelihood is wanted in the output.
             This option currently cannot be combined with -ICC, -ICCb, -LOGIT.
    
       -mask MASK: Process voxels inside this mask only.
              Default is no masking.
    
       -model FORMULA: Specify the terms of fixed effects for all explanatory,
             including quantitative, variables. The expression FORMULA with more
             than one variable has to be surrounded within (single or double)
             quotes. Variable names in the formula should be consistent with
             the ones used in the header of -dataTable. A+B represents the
             additive effects of A and B, A:B is the interaction between A
             and B, and A*B = A+B+A:B. Subject should not occur in the model
             specification here.
    
       -num_glf NUMBER: Specify the number of general linear F-tests (GLFs). A glf
             involves the union of two or more simple tests. See details in 
             -glfCode.
    
       -num_glt NUMBER: Specify the number of general linear t-tests (GLTs). A glt
             is a linear combination of a factor levels. See details in 
             -gltCode.
    
       -prefix PREFIX: Output file name. For AFNI format, provide prefix only,
             with no view+suffix needed. Filename for NIfTI format should have
             .nii attached, while file name for surface data is expected
             to end with .niml.dset. The sub-brick labeled with the '(Intercept)',
             if present, should be interpreted as the effect with each factor
             at the reference level (alphabetically the lowest level) for each
             factor and with each quantitative covariate at the center value.
    
       -qVarCenters VALUES: Specify centering values for quantitative variables
             identified under -qVars. Multiple centers are separated by 
             commas (,) without any other characters such as spaces and should
             be surrounded within (single or double) quotes. The order of the
             values should match that of the quantitative variables in -qVars.
             Default (absence of option -qVarsCetners) means centering on the
             average of the variable across ALL subjects regardless their
             grouping. If within-group centering is desirable, center the
             variable YOURSELF first before the values are fed into -dataTable.
    
       -qVars variable_list: Identify quantitative variables (or covariates) with
             this option. The list with more than one variable has to be
             separated with comma (,) without any other characters such as
             spaces and should be surrounded within (single or double) quotes.
              For example, -qVars "Age,IQ"
             WARNINGS:
             1) Centering a quantitative variable through -qVarsCenters is
             very critical when other fixed effects are of interest.
             2) Between-subjects covariates are generally acceptable.
             However EXTREME caution should be taken when the groups
             differ significantly in the average value of the covariate.
             3) Within-subject covariates are better modeled with 3dLME.
    
       -ranEff FORMULA: Specify the random effects. The simplest and most common
             one is random intercept, "~1", meaning each subject deviates some
             amount (called random effect) from the group average. "~RT" or "~1+RT"
             means that each subject has a unique intercept as well as a slope,
             and the correlation between the two random effects are estimated, not
             assumed, from the data. "~0+RT" indicates that only a random effect
             of slope is desired. Compound symmetry for a variance-covariance metric
             across the levels of factor A can be specified through pdCompSymm(~0+A)
             The list of random terms should be separated by space within (single or
             double) quotes.
             Notice: In the case of computing ICC values, list all the factors with
             which the ICC is to be obtained. For example, with two factors "Scanner"
             and "Subj", set it as -ranEff "Scanner+Subj". See Example 4 in the
             the help.
    
       -resid PREFIX: Output file name for the residuals. For AFNI format, provide
             prefix only without view+suffix. Filename for NIfTI format should
             have .nii attached, while file name for surface data is expected
             to end with .niml.dset. The sub-brick labeled with the '(Intercept)',
             if present, should be interpreted as the effect with each factor
             at the reference level (alphabetically the lowest level) for each
             factor and with each quantitative covariate at the center value.
    
       -Rio: Use R's io functions. The alternative is -cio.
    
       -show_allowed_options: list of allowed options
    
       -SS_type NUMBER: Specify the type for sums of squares in the F-statistics.
             Two options are currently supported: sequential (1) and marginal (3).
     
       -vVarCenters VALUES: Specify centering values for voxel-wise covariates
             identified under -vVars. Multiple centers are separated by 
             commas (,) within (single or double) quotes. The order of the
             values should match that of the quantitative variables in -qVars.
             Default (absence of option -vVarsCetners) means centering on the
             average of the variable across ALL subjects regardless their
             grouping. If within-group centering is desirable, center the
             variable YOURSELF first before the files are fed into -dataTable.
    
       -vVars variable_list: Identify voxel-wise covariates with this option.
             Currently one voxel-wise covariate is allowed only, but this
             may change if demand occurs...
             By default mean centering is performed voxel-wise across all
             subjects. Alternatively centering can be specified through a
             global value under -vVarsCenters. If the voxel-wise covariates
             have already been centered, set the centers at 0 with -vVarsCenters.
