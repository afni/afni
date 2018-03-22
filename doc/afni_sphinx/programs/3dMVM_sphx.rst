*****
3dMVM
*****

.. _ahelp_3dMVM:

.. contents:: 
    :depth: 4 

| 

    

Welcome to 3dMVM
================

.. code-block:: none

        AFNI Group Analysis Program with Multi-Variate Modeling Approach
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Version 3.9.4, Dec 21, 2016
    Author: Gang Chen (gangchen@mail.nih.gov)
    Website - https://afni.nimh.nih.gov/sscc/gangc/MVM.html
    SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    

Usage:
======

.. code-block:: none

     3dMVM is a group-analysis program that performs traditional ANOVA- and ANCOVA-
     style computations. In addition, it can run multivariate modeling in the sense
     of multiple simultaneous response variables. For univariate analysis, no bound
     is imposed on the  numbers of explanatory variables, and these variables can be
     either categorical (factor) or numerical/quantitative (covariate). F-statistics
     for all main effects and interactions are automatically included in the output.
     In addition, general linear tests (GLTs) can be requested via symbolic coding.
     
     Input files for 3dMVM can be in AFNI, NIfTI, or surface (niml.dset) format.
     Note that unequal number of subjects across groups are allowed, but scenarios 
     with missing data for a within-subject factor are better modeled with 3dLME. 
     Cases with quantitative variables (covariates) that vary across the levels of 
     a within-subject variable are also better handled with 3dLME. Computational 
     cost with 3dMVM is very high compared to 3dttest++ or 3dANOVAx, but it has the
     capability to correct for sphericity violations when within-subject variables
     with more than two levels are involved.
     

Please cite:
============


If you want to cite the analysis approach for AN(C)OVA, use the following
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

     
     Chen, G., Adleman, N.E., Saad, Z.S., Leibenluft, E., Cox, R.W. (2014). 
     Applications of Multivariate Modeling to Neuroimaging Group Analysis: A
     Comprehensive Alternative to Univariate General Linear Model. NeuroImage 99,
     571-588. 10.1016/j.neuroimage.2014.06.027
     https://afni.nimh.nih.gov/pub/dist/HBM2014/Chen_in_press.pdf
    

For group analyis with effect estimates from multiple basis funcitons, cite:
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
     Chen, G., Saad, Z.S., Adleman, N.E., Leibenluft, E., Cox, R.W. (2015). 
     Detecting the subtle shape differences in hemodynamic responses at the
     group level. Front. Neurosci., 26 October 2015.
     http://dx.doi.org/10.3389/fnins.2015.00375
    

Installation requirements:
==========================

.. code-block:: none

     In addition to R installation, the following two R packages need to be acquired
     in R first before running 3dMVM: "afex" and "phia". In addition, the "snow" package
     is also needed if one wants to take advantage of parallel computing. To install
     these packages, run the following command at the terminal:
    
     rPkgsInstall -pkgs ALL
    
     Alternatively you may install them in R:
    
     install.packages("afex")
     install.packages("phia")
     install.packages("snow")
    
     More details about 3dMVM can be found at 
     https://afni.nimh.nih.gov/sscc/gangc/MVM.html
     

Running:
========

.. code-block:: none

     Once the 3dMVM command script is constructed, it can be run by copying and
     pasting to the terminal. Alternatively (and probably better) you save the 
     script as a text file, for example, called MVM.txt, and execute it with the 
     following  (assuming on tc shell),
     
     tcsh -x MVM.txt &
     
     or,
     
     tcsh -x MVM.txt > diary.txt &
     tcsh -x MVM.txt |& tee diary.txt &
    
     The advantage of the latter command is that the progression is saved into
     the text file diary.txt and, if anything goes awry, can be examined later.
     
     Thanks to the R community, Henrik Singmann, and Helios de Rosario for the 
     strong technical support.
    

Examples:
=========

    

Example 1 --- 3 between-subjects and 2 within-subject variables:
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

       Three between-subjects (genotype, sex, and scanner) and two within-subject 
       (condition and emotion) variables.
    
       3dMVM  -prefix Example1 -jobs 4            \
              -bsVars  'genotype*sex+scanner'      \
              -wsVars "condition*emotion"         \
              -SS_type 2                          \
              -num_glt 14                         \
              -gltLabel 1 face_pos_vs_neg -gltCode  1 'condition : 1*face emotion : 1*pos -1*neg'            \
              -gltLabel 2 face_emot_vs_neu -gltCode 2 'condition : 1*face emotion : 1*pos +1*neg -2*neu'     \
              -gltLabel 3 sex_by_condition_interaction -gltCode 3 'sex : 1*male -1*female condition : 1*face -1*house' \
              -gltLabel 4 3way_interaction -gltCode 4 'sex : 1*male -1*female condition : 1*face -1*house emotion : 1*pos -1*neg' \
              -num_glf 3                         \
              -glfLabel 1 male_condXEmo -glfCode 1 'sex : 1*male condition : 1*face -1*house emotion : 1*pos -1*neg & 1*pos -1*neu' \
              -glfLabel 2 face_sexXEmo -glfCode 2 'sex : 1*male -1*female condition : 1*face emotion : 1*pos -1*neg & 1*pos -1*neu' \
              -glfLabel 3 face_sex2Emo -glfCode 3 'sex : 1*male & 1*female condition : 1*face emotion : 1*pos -1*neg & 1*pos -1*neu' \
              -dataTable                                                                                     \
              Subj  genotype   sex    scanner  condition   emotion   InputFile                               \
              s1    TT         male   scan1   face        pos       s1+tlrc'[face_pos_beta]'                 \
              s1    TT         male   scan1   face        neg       s1+tlrc'[face_neg_beta]'                 \
              s1    TT         male   scan1   face        neu       s1+tlrc'[face_neu_beta]'                 \
              s1    TT         male   scan1   house       pos       s1+tlrc'[house_pos_beta]'                \
              s68   TN         female scan2   house       pos       s68+tlrc'[face_pos_beta]'                \
              s68   TN         female scan2   house       neg       s68+tlrc'[face_neg_beta]'                \
              s68   TN         female scan2   house       neu       s68+tlrc'[house_pos_beta]'                    
    

NOTE:
~~~~~

.. code-block:: none

              1) The 3rd GLT is for the 2-way 2 x 2 interaction between sex and condition, which
              is essentially a t-test (or one degree of freedom for the numerator of F-statistic).
              Multiple degrees of freedom for the numerator of F-statistic can be obtained through
              option -glfCode (see GLFs #1, #2, and #3).
              2) Similarly, the 4th GLT is a 3-way 2 x 2 x 2 interaction, which is a partial (not full)
              interaction between the three factors because 'emotion' has three levels. The F-test for
              the full 2 x 2 x 3 interaction is automatically spilled out by 3dMVM.
              3) The two GLFs showcase the user how to specify sub-interactions.
              5) Option '-SS_type 2' specifies the hierarchial type for the sume of squares in the
              omnibus F-statistics in the output. See more details in the help.
    

Example 2 --- 2 between-subjects, 1 within-subject, 2 quantitative variables:
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
       Two between-subjects (genotype and sex), one within-subject
       (emotion) factor, plus two quantitative variables (age and IQ).
    
       3dMVM -prefix Example2 -jobs 24        \
              -bsVars  "genotype*sex+age+IQ"  \
              -wsVars emotion                \
              -qVars  "age,IQ"               \
              -qVarCenters '25,105'          \
              -num_glt 10                    \
              -gltLabel 1 pos_F_vs_M   -gltCode 1 'sex : 1*female -1*male emotion : 1*pos'          \
              -gltLabel 2 age_pos_vs_neg -gltCode 2 'emotion : 1*pos -1*neg age :'                  \
              -gltLabel 3 age_pos_vs_neg -gltCode 3 'emotion : 1*pos -1*neg age : 5'                \
              -gltLabel 4 genotype_by_sex -gltCode 4 'genotype : 1*TT -1*NN sex : 1*male -1*female' \
              -gltLabel 5 genotype_by_sex_emotion -gltCode 5 'genotype : 1*TT -1*NN sex : 1*male -1*female emotion : 1*pos -1*neg' \
              -dataTable                                                                   \
              Subj  genotype  sex    age  IQ     emotion   InputFile                       \
              s1    TT         male   24   107    pos       s1+tlrc'[pos_beta]'            \
              s1    TT         male   24   107    neg       s1+tlrc'[neg_beta]'            \
              s1    TT         male   24   107    neu       s1+tlrc'[neu_beta]'            \
              s63   NN         female 29   110    pos       s63+tlrc'[pos_beta]'           \
              s63   NN         female 29   110    neg       s63+tlrc'[neg_beta]'           \
              s63   NN         female 29   110    neu       s63+tlrc'[neu_beta]'         
    

NOTE:
~~~~~

.. code-block:: none

              1) The 2nd GLT shows the age effect (slope) while the 3rd GLT reveals the contrast
              between the emotions at the age of 30 (5 above the center). On the other hand,
              all the other GLTs (1st, 4th, and 5th) should be interpreted at the center Age
              value, 25 year old.
              2) The 4rd GLT is for the 2-way 2 x 2 interaction between genotype and sex, which
              is essentially a t-test (or one degree of freedom for the numerator of F-statistic).
              Multiple degrees of freedom for the numerator of F-statistic is currently unavailable.
              3) Similarly, the 5th GLT is a 3-way 2 x 2 x 2 interaction, which is a partial (not full)
              interaction between the three factors because 'emotion' has three levels. The F-test for
              the full 2 x 2 x 3 interaction is automatically spilled out by 3dMVM.
    

Example 3 --- Getting more complicated:
+++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
       BOLD response was modeled with multiple basis functions at individual
       subject level. In addition, there are one between-subjects (Group) and one within-
       subject (Condition) variable. Furthermore, the variable corresponding to the number 
       of basis functions, Time, is also a within-subject variable. In the end, the F-
       statistics for the interactions of Group:Condition:Time, Group:Time, and 
       Condition:Time are of specific interest. And these interactions can be further
       explored with GLTs in 3dMVM.
    
       3dMVM -prefix Example3 -jobs 12   \
             -bsVars Group               \
             -wsVars 'Condition*Time'   \
             -num_glt 32                \
             -gltLabel 1 old_t0 -gltCode 1 'Group : 1*old Time : 1*t0' \
             -gltLabel 2 old_t1 -gltCode 2 'Group : 1*old Time : 1*t1' \
             -gltLabel 3 old_t2 -gltCode 3 'Group : 1*old Time : 1*t2' \
             -gltLabel 4 old_t3 -gltCode 4 'Group : 1*old Time : 1*t3' \
             -gltLabel 5 yng_t0 -gltCode 5 'Group : 1*yng Time : 1*t0' \
             -gltLabel 6 yng_t1 -gltCode 6 'Group : 1*yng Time : 1*t1' \
             -gltLabel 7 yng_t2 -gltCode 7 'Group : 1*yng Time : 1*t2' \
             -gltLabel 8 yng_t3 -gltCode 8 'Group : 1*yng Time : 1*t3' \
             -gltLabel 17 old_face_t0 -gltCode 17 'Group : 1*old Condition : 1*face Time : 1*t0' \
             -gltLabel 18 old_face_t1 -gltCode 18 'Group : 1*old Condition : 1*face Time : 1*t1' \
             -gltLabel 19 old_face_t2 -gltCode 19 'Group : 1*old Condition : 1*face Time : 1*t2' \
             -gltLabel 20 old_face_t3 -gltCode 20 'Group : 1*old Condition : 1*face Time : 1*t3' \
             -dataTable                                            \
             Subj  Group  Condition Time InputFile                 \
             s1    old    face      t0   s1+tlrc'[face#0_beta]'    \
             s1    old    face      t1   s1+tlrc'[face#1_beta]'    \
             s1    old    face      t2   s1+tlrc'[face#2_beta]'    \
             s1    old    face      t3   s1+tlrc'[face#3_beta]'    \
             s40   yng    house     t0   s40+tlrc'[house#0_beta]'  \
             s40   yng    house     t1   s40+tlrc'[house#1_beta]'  \
             s40   yng    house     t2   s40+tlrc'[house#2_beta]'  \
             s40   yng    house     t3   s40+tlrc'[house#3_beta]'      
    

NOTE:
~~~~~

.. code-block:: none

              The model for the analysis can also be set up as and is equivalent to 
              'Group*Condition*Time'.
       

Options:
========

.. code-block:: none

       
    
    Options in alphabetical order:
    
       -bsVars FORMULA: Specify the fixed effects for between-subjects factors 
             and quantitative variables. When no between-subject factors
             are present, simply put 1 for FORMULA. The expression FORMULA
             with more than one variable has to be surrounded within (single or
             double) quotes. No spaces are allowed in the FORMULA expression.
             Variable names in the formula should be consistent with the ones
             used in the header underneath -dataTable. A+B represents the
             additive effects of A and B, A:B is the interaction between A
             and B, and A*B = A+B+A:B. The effects of within-subject
             factors, if present under -wsVars are automatically assumed
             to interact with the ones specified here. Subject as a variable
             should not occur in the model specification here.
    
       -cio: Use AFNI's C io functions, which is default. Alternatively -Rio
             can be used.
    
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
             [] within quotes) specified with a number or label. Unequal number of
             subjects across groups is allowed, but situations with missing data
             for a within-subject factor are better handled with 3dLME.
    
             3) It is fine to have variables (or columns) in the table that are
             not modeled in the analysis.
    
             4) The context of the table can be saved as a separate file, e.g.,
             called table.txt. Do not forget to include a backslash at the end of
             each row. In the script specify the data with '-dataTable @table.txt'.
             This option is useful: (a) when there are many input files so that
             the program complains with an 'Arg list too long' error; (b) when
             you want to try different models with the same dataset (see 3) above).
    
       -dbgArgs: This option will enable R to save the parameters in a
             file called .3dMVM.dbg.AFNI.args in the current directory
              so that debugging can be performed.
    
       -GES: As an analog of the determination coefficient R^2 in multiple
             regression, generalized eta-squared (GES) provides a measure
             of effect size for each F-stat in ANOVA or general GLM, and
             renders a similar interpretation: proportion of variance in
             the response variable by the explanatory variable on hand.
             It ranges within [0, 1]. Notice that this option is only
             available with R version 3.2 and afex version 0.14 or later.
    
       -glfCode k CODING: Specify the k-th general linear F-test (GLF) through a
             weighted combination among factor levels. The symbolic coding has
             to be within (single or double) quotes. For example, the coding
             'Condition : 1*A -1*B & 1*A -1*C Emotion : 1:pos' tests the main
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
    
       -gltCode k CODING: Specify the k-th general linear t-test (GLT) through a
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
    
             3) The effect for a quantitative variable (or slope) can be specified
             with, for example, 'Group : 1*Old Age : ', or 
             'Group : 1*Old - 1*Young Age : '
    
             4) When a quantitative covariate is involved in the model, the
    
             absence of the covariate in the GLT coding means that the test
    
             will be performed at the center value of the covarite. However,
    
             with a value after the colon, the effect would be tested at the
    
             value of 2 above the center. For example, 'Group : 1*Old Age : 2'
             shows the effect of the Old Group at the age of 2 years older than
    
             the center age. On the other hand, 'Group : 1*Old' tests for the
    
             effect of the Old Group at the center age.
    
             5) The absence of a categorical variable in a coding means the
             levels of that factor are averaged (or collapsed) for the GLT.
    
             6) The appearance of a categorical variable has to be followed
             by the linear combination of its levels. Only a quantitative
             is allowed to have a dangling coding as seen in 'Age :'
    
             7) Some special interaction effects can be tested under -gltCode
             when the numerical DF is 1. For example, 'Group : 1*Old - 1*Young
             Condition : 1*House -1*Face Emotion : 1*positive'. Even though
             this is typically an F-test that can be coded under -glfCode, it
             can be tested under -gltCode as well. An extra bonus is that the
             t-test shows the directionality while F-test does not.
    
       -gltLabel k label: Specify the label for the k-th general linear t-test
             (GLT). A symbolic coding for the GLT is assumed to follow with
             each -gltLabel.
    
       -help: this help message
    
       -jobs NJOBS: On a multi-processor machine, parallel computing will speed 
             up the program significantly.
             Choose 1 for a single-processor computer.
    
       -mask MASK: Process voxels inside this mask only.
              Default is no masking.
    
       -model FORMULA: This option will phase out at some point. So use -bsVars
             instead. Specify the fixed effects for between-subjects factors 
             and quantitative variables. When no between-subject factors
             are present, simply put 1 for FORMULA. The expression FORMULA
             with more than one variable has to be surrounded within (single or double)
             quotes. Variable names in the formula should be consistent with
             the ones used in the header of -dataTable. A+B represents the
             additive effects of A and B, A:B is the interaction between A
             and B, and A*B = A+B+A:B. The effects of within-subject
             factors, if present under -wsVars are automatically assumed
             to interact with the ones specified here. Subject as a variable
             should not occur in the model specification here.
    
       -mVar variable: With this option, the levels of the within-subject factor
             will be treated as simultaneous variables in a multivariate model.
             For example, when the hemodynamic response time course is modeled
             through multiple basis functions such as TENT, TENTzero, CSPLIN,
             CSPLINzero, SPMG2/3, etc., the effect estimates at the multiple
             time points can be treated as simultaneous response variables in
             a multivariate model. Only one within-subject variable is allowed
             currently under -mVar. In addition, in the presence of -mVar, no
             other within-subject factors should be included. If modeling
             extra within-subject factors with -mVar is desirable, consider
             flattening such factors; that is, perform multiple analyses
             at each level or their contrasts of the factor. The output
             for multivariate testing are labeled with -MV0- in the sub-brick
             names.
    
       
       
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
             if present, should be interpreted as the overall average
             across factor levels at the center value of each covariate.
    
       -qVarCenters VALUES: Specify centering values for quantitative variables
             identified under -qVars. Multiple centers are separated by 
             commas (,) within (single or double) quotes. The order of the
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
             3) Within-subject covariates vary across the levels of a
             within-subject factor, and can be analyzed with 3dLME,
             but not 3dMVM.
    
       -Rio: Use R's io functions. The alternative is -cio.
    
       -robust: Robust regression is performed so that outliers can be
             reasonably handled through MM-estimation. Currently it
             only works without involving any within-subject factors.
             That is, anything that can be done with 3dttest++ could
             be analyzed through robust regression here (except for
             one-sample which can be added later one if requested).
             pairwise comparisons can be performed by providing
             contrast from each subject as input). Post hoc F-tests
             through option -glfCode are currently not available with
             robust regression. This option requires that the user
             install R package robustbase.
    
       -SC: If a within-subject factor with more than *two* levels is
             involved in the model, 3dMVM automatically provides the
             F-statistics for main and interaction effects with
             sphericity assumption. If the assumption is violated,
             the F-statistics could be inflated to some extent. This option,
             will enable 3dMVM to additionally output the F-statistics of
             sphericity correction for main and interaction effects, which
             are labeled with -SC- in the sub-brick names.
             NOTE: this option should be used only when at least one
             within-subject factor has more than TWO levesl.
    
       -show_allowed_options: list of allowed options
    
       -SS_type 2/3: Specify the type for the sums of squares for the omnibus
             F-statistics. Type 2 is hierarchical or partially sequential
             while type 3 is marginal. Type 2 is more powerful if all the
             relevant higher-oder interactions do not exist. The default
             is 3. The controversy surrounding the different types can be
             found at https://afni.nimh.nih.gov/sscc/gangc/SS.html
    
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
    
       -wsE2: If at least one within-subject factor is involved in the model, any
             omnibus F-test associated with a within-subject factor is assessed
             with both univariate and within-subject multivariate tests. Use
             the option only if at least one within-subject factor has more
             than two levels. By default 3dMVM provides an F-stat through the
             univariate testing (UVT) method for each effect that involves a
             within-subject factor. With option -wsE2 UVT is combined with the
             within-subject multivariate approach, and the merged result remains
             the same as UVT most of the time (or in most brain regions), but
             occasionally it may be more powerful.
    
       -wsMVT: By default 3dMVM provides an F-stat through univariate testing (UVT)
             for each effect that involves a within-subject factor. If at least
             one within-subject factor is involved in the model, option -wsMVT
             provides within-subject multivariate testing for any effect
             associated with a within-subject variable. The testing strategy is
             different from the conventional univariate GLM, see more details in
             Chen et al. (2014), Applications of Multivariate Modeling to
             Neuroimaging Group Analysis: A Comprehensive Alternative to
             Univariate General Linear Model. NeuroImage 99, 571-588. If
             all the within-subject factors have two levels, the multivariate
             testing would render the same results as the univariate version.
             So use the option only if at least one within-subject factor has
             more than two levels. The F-statistics from the multivariate
             testing are labeled with -wsMVT- in the sub-brick names. Note that
             the conventional univariate F-statistics are automatically included
             in the beginning of the output regardless the presence of this option.
    
       -wsVars FORMULA: Within-subject factors, if present, have to be listed
             here; otherwise the program will choke. If no within-subject 
             exists, don't include this option in the script. Coding for
             additive effects and interactions is the same as in -bsVars. The
             FORMULA with more than one variable has to be surrounded 
             within (single or double) quotes. Note that the within-subject
             variables are assumed to interact with those between-subjects
             variables specified under -bsVars. The hemodynamic response
             time course are better modeled as simultaneous outcomes through
             option -mVar, and not as the levels of a within-subject factor.
             The varialbes under -wsVars and -mVar are exclusive from each
             other.
