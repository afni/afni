*********
3dttest++
*********

.. _3dttest++:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Gosset (Student) t-test of sets of 3D datasets.
    
          [* Also consider program 3dMEMA, which can carry out a  *]
          [* more sophisticated type of 't-test' that also takes  *]
          [* into account the variance map of each input dataset. *]
    
    * Usage can be similar (not identical) to the old 3dttest;
      for example [SHORT form of dataset input]:
    
        3dttest++ -setA a+tlrc'[3]' b+tlrc'[3]' ...
    
    * OR, usage can be similar to 3dMEMA; for example [LONG form]:
    
        3dttest++ -setA Green sub001 a+tlrc'[3]' \
                              sub002 b+tlrc'[3]' \
                              sub003 c+tlrc'[3]' \
                                ...              \
                    -covariates Cfile
    
    * Please note that in the second ('LONG') form of the '-setA' option,
      the first value after '-setA' is a label for the set (here, 'Green').
     ++ After that, pairs of values are given; in each pair, the first
        entry is a label for the dataset that is the second entry.
     ++ This dataset label is used as a key into the covariates file.
     ++ If you want to have a label for the set, but do not wish (or need)
        to have a label for each dataset in the set, then you can use
        the SHORT form (first example above), and then provide the overall
        label for the set with the '-labelA' option.
     ++ The set label is used to create sub-brick labels in the output dataset,
        to make it simpler for a user to select volumes for display in the
        AFNI GUI. Example:
          -labelA Nor -label Pat
        then the difference between the setA and setB means will get the
        label 'Nor-Pat_mean', and the corresponding t-statistic will get
        the label 'Nor-Pat_Tstat'.
     ++ See the section 'STRUCTURE OF THE OUTPUT DATASET' (far below) for
        more infomation on how the results are formatted.
    
    * You can input 1 or 2 sets of data (labeled 'A' and 'B' by default).
    
    * With 1 set ('-setA'), the mean across input datasets (usually subjects)
       is tested against 0.
    
    * With 2 sets, the difference in means across each set is tested
       against 0.  The 1 sample results for each set are also provided, since
       these are often of interest to the investigator (e.g., YOU).
      ++ With 2 sets, the default is to produce the difference as setA - setB.
      ++ You can use the option '-BminusA' to get the signs reversed.
    
    * Covariates can be per-dataset (input=1 number) and/or per-voxel/per-dataset
       (input=1 dataset sub-brick).
      ++ Note that voxel-level covariates will slow the program down, since
          the regression matrix for the covariates must be re-inverted for
          each voxel separately.  For most purposes, the program is so fast
          that this slower operation won't be important.
    
    * The new-ish options '-Clustsim' and '-ETAC' will use randomization and
      permutation simulation to produce cluster-level threshold values that
      can be used to control the false positive rate (FPR) globally. These
      options are slow, since they will run 1000s of simulated 3D t-tests in
      order to get cluster-level statistics about the 1 actual test.
    
    * You can input plain text files of numbers, provided their filenames end
      in the AFNI standard '.1D'. If you have two columns of numbers in files
      AA.1D and BB.1D, you could test their means for equality with a command like
        3dttest++ -prefix stdout: -no1sam setA AA.1D\' -setB BB.1D\'
      Here, the \' at the end of the filename tells the program to transpose
      the column files to row files, since AFNI treats a single row of numbers
      as the multiple values for a single 'voxel'. The output (on stdout) from
      such a command will be one row of numbers: the first value is the
      difference in the means between the 2 samples, and the second value is
      the t-statistic for this difference. (There will also be a bunch of text
      on stderr, with various messages.)
    
    * This program is meant (for most uses) to replace the original 3dttest,
       which was written in 1994, "When grass was green and grain was yellow".
      ++ And when the program's author still had hair on the top of his head /:(
    
    ------------------
    SET INPUT OPTIONS
    ------------------
    
    * At least the '-setA' option must be given.
    
    * '-setB' is optional, and if it isn't used, then the mean of the dataset
       values from '-setA' is t-tested against 0 (1 sample t-test).
    
    * Two forms for the '-setX' (X='A' or 'B') options are allowed.  The first
       (short) form is similar to the original 3dttest program, where the option
       is just followed by a list of datasets to use.
    
    * The second (long) form is similar to the 3dMEMA program, where you specify
       a label for each input dataset sub-brick (a difference between this
       option and the version in 3dMEMA is only that you do not give a second
       dataset ('T_DSET') with each sample in this program).
    
    ***** SHORT FORM *****
    
     -setA BETA_DSET BETA_DSET ...
    [-setB]
    
    * In this form of input, you specify the datasets for each set
       directly following the '-setX' option.
      ++ Unlike 3dttest, you can specify multiple sub-bricks in a dataset:
            -setA a+tlrc'[1..13(2)]'
         which inputs 7 sub-bricks at once (1,3,5,7,9,11,13).
       *** See the '-brickwise' option (far below) for more information ***
       *** on how multiple sub-brick datasets will be processed herein. ***
      ++ If multiple sub-bricks are input from a single dataset, then
         covariates cannot be used (sorry, Charlie).
      ++ For some limited compatibility with 3dttest, you can use '-set2' in
         place of '-setA', and '-set1' in place of '-setB'.
      ++ [19 Jun 2012, from Beijing Normal University, during AFNI Bootcamp]
         For the SHORT FORM only, you can use the wildcards '*' and/or '?' in
         the BETA_DSET filenames, along with sub-brick selectors, to make it
         easier to create the command line.
         To protect the wildcards from the shell, the entire filename should be
         inside single ' or double " quote marks.  For example:
           3dttest++ -setA '*.beta+tlrc.HEAD[Vrel#0_Coef]' \
                     -setB '*.beta+tlrc.HEAD[Arel#0_Coef]' -prefix VAtest -paired
         will do a paired 2-sample test between the symbolically selected sub-bricks
         from a collection of single-subject datasets (here, 2 different tasks).
    
    ***** LONG FORM *****
    
     -setA SETNAME            \
    [-setB]  LABL_1 BETA_DSET \
             LABL_2 BETA_DSET \
             ...    ...       \
             LABL_N BETA_DSET
    
    * In this form of input, you specify an overall name for the set of datasets,
       and a label to be associated with each separate input dataset.  (This label
       is used with the '-covariates' option, described later.)
    
       SETNAME   is the name assigned to the set (used in the output labels).
       LABL_K    is the label for the Kth input dataset name, whose name follows.
       BETA_DSET is the name of the dataset of the beta coefficient or GLT.
                 ++ only 1 sub-brick can be specified here!
       Note that the labels 'SETNAME' and 'LABL_K' are limited to 12
       characters -- any more will be thrown away without warning.
    
         ** The program determines if you are using the short form or long **
         ** form to specify the input datasets based on the first argument **
         ** after the '-setX' option.  If this argument can be opened as a **
         ** dataset, the short form is used. If instead, the next argument **
         ** cannot be opened as a dataset,  then the long form is assumed. **
    
     -labelA SETNAME = for the short form of '-setX', this option allows you
    [-labelB]          to attach a label to the set, which will be used in
                       the sub-brick labels in the output dataset.  If you don't
                       give a SETNAME, then '-setA' will be named 'SetA', etc.
    
      ***** NOTE WELL: The sign of a two sample test is A - B.          *****
      ***              Thus, '-setB' corresponds to '-set1' in 3dttest,   ***
      ***                and '-setA' corresponds to '-set2' in 3dttest.   ***
      *****            This ordering of A and B matches 3dGroupInCorr.  *****
      *****-------------------------------------------------------------*****
      ***** ALSO NOTE: You can reverse this sign by using the option    *****
      ***              '-BminusA', in which case the test is B - A.       ***
      ***              The option '-AminusB' can be used to explicitly    ***
      *****            specify the standard subtraction order.          *****
    
    ---------------------------------------------------------------
    TESTING A SINGLE DATASET VERSUS THE MEAN OF A GROUP OF DATASETS
    ---------------------------------------------------------------
    
    This new [Mar 2015] option allows you to test a single value versus
    a group of datasets.  To do this, replace the '-setA' option with the
    '-singletonA' option described below, and input '-setB' normally
    (that is, '-setB' must have more than 1 dataset).
    
    The '-singletonA' option comes in 3 different forms:
    
     -singletonA dataset_A
       *OR*
     -singletonA LABL_A dataset_A
       *OR*
     -singletonA FIXED_NUMBER
    
    * In the first form, just give the 1 sub-brick dataset name after the option.
    
    * In the second form, you can provide a dataset 'label' to be used for
      covariates extraction.  As in the case of the long forms for '-setA' and
      '-setB', the 'LABL_A' argument cannot be the name of an existing dataset;
      otherwise, the program will assume you are using the first form.
    
    * In the third form, instead of giving a dataset, you give a fixed number
      (e.g., '0.5'), to test the -setB collection against this 1 number.
      ++ In this form, '-singleton_variance_ratio' is set to a very small number,
         since you presumably aren't testing against an instance of a random
         variable.
      ++ Also, '-BminusA' is turned on when FIXED_NUMBER is used, to give the
         effect of a 1-sample test against a constant.  For example,
           -singletonA 0.0 -set B x y z
         is equivalent to the 1-sample test with '-setA x y z'. The only advantage
         of using '-singletonA FIXED_NUMBER' is that you can test against a
         nonzero constant this way.
      ++ You cannot use covariates with this FIXED_NUMBER form of '-singletonA' /:(
    
    * The output dataset will have 2 sub-bricks:
      ++ The difference (at each voxel) between the dataset_A value and the
         mean of the setB dataset values.
      ++ (In the form where 'dataset_A' is replaced by a fixed)
         (number, the output is instead the difference between)
         (the mean of the setB values and the fixed number.   )
      ++ The t-statistic corresponding to this difference.
    
    * If covariates are used, at each voxel the slopes of the setB data values with
      respect to the covariates are estimated (as usual).
      ++ These slopes are then used to project the covariates out of the mean of
         the setB values, and are also applied similarly to the single value from
         the singleton dataset_A (using its respective covariate value).
      ++ That is, the covariate slopes from setB are applied to the covariate values
         for dataset_A in order to subtract the covariate effects from dataset_A,
         as well as from the setB mean.
      ++ Since it impossible to independently estimate the covariate slopes for
         dataset_A, this procedure seems (to me) like the only reasonable way to use
         covariates with a singleton dataset.
    
    * The t-statistic is computed assuming that the variance of dataset_A is the
      same as the variance of the setB datasets.
      ++ Of course, it is impossible to estimate the variance of dataset_A at each
         voxel from its single number!
      ++ In this way, the t-statistic differs from testing the setB mean against
         a (voxel-dependent) constant, which would not have any variance.
      ++ In particular, the t-statistic will be smaller than in the more usual
         'test-against-constant' case, since the test here allows for the variance
         of the dataset_A value.
      ++ As a special case, you can use the option
           -singleton_variance_ratio RRR
         to set the (assumed) variance of dataset_A to be RRR times the variance
         of set B. Here, 'RRR' must be a positive number -- it cannot be zero,
         so if you really want to test against a voxel-wise constant, use something
         like 0.000001 for RRR (this is the setting automatically made when
         'dataset_A' is replaced by a fixed number, in the third form above).
    
    * Statistical inference on a single sample (dataset_A values) isn't really
      possible.  The purpose of '-singletonA' is to give you some guidance when
      a voxel value in dataset_A is markedly different from the distribution of
      values in setB.
      ++ However, a statistician would caution you that when an elephant walks into
         the room, it might be a 500,000 standard deviation mouse, so you can't
         validly conclude it is a different species until you get some more data.
    
    * At present, '-singletonA' cannot be used with '-brickwise'.
      ++ Various other options don't make sense with '-singletonA', including
         '-paired' and '-center SAME'.
    
    * Note that there is no '-singletonB' option -- the only reason this is labeled
      as '-singletonA' is to remind the user (you) that this option replaces the
      '-setA' option.
    
    --------------------------------------
    COVARIATES - per dataset and per voxel
    --------------------------------------
    
     -covariates COVAR_FILE
    
    * COVAR_FILE is the name of a text file with a table for the covariate(s).
       Each column in the file is treated as a separate covariate, and each
       row contains the values of these covariates for one sample (dataset). Note
       that you can use '-covariates' only ONCE -- the COVAR_FILE should contain
       the covariates for ALL input samples from both sets.
    
    * Rows in COVAR_FILE whose first column don't match a dataset label are
       ignored (silently).
      ++ This feature allows you to analyze subsets of data collections while
         using the covariates file for a large group of subjects -- some of whom
         might not be in a given subset analysis.
    
    * An input dataset label that doesn't match a row in COVAR_FILE, on the other
       hand, is a fatal error.
      ++ The program doesn't know how to get the covariate values for such a
         dataset, so it can't continue.
    
    * There is no provision for missing values -- the entire table must be filled!
    
    * The format of COVAR_FILE is similar to the format used in 3dMEMA and
       3dGroupInCorr (generalized to allow for voxel-wise covariates):
    
         FIRST LINE -->   subject IQ   age  GMfrac
         LATER LINES -->  Elvis   143   42  Elvis_GM+tlrc[8]
                          Fred     85   59  Fred_GM+tlrc[8]
                          Ethel   109   49  Ethel_GM+tlrc[8]
                          Lucy    133   32  Lucy_GM+tlrc[8]
                          Ricky   121   37  Ricky_GM+tlrc[8]
    
    * The first line of COVAR_FILE contains column headers.  The header label
       for the first column (#0) isn't used for anything.  The later header labels
       are used in the sub-brick labels stored in the output dataset.
    
    * The first column contains the dataset labels that must match the dataset
       LABL_K labels given in the '-setX' option(s).
    
    * If you used a short form '-setX' option, each dataset label is
       the dataset's prefix name (truncated to 12 characters).
      ++ e.g.,  Klaatu+tlrc'[3]' ==>  Klaatu
      ++ e.g.,  Elvis.nii.gz     ==>  Elvis
    
    * '-covariates' can only be used with the short form '-setX' option
       when each input dataset has only 1 sub-brick (so that each label
       refers to exactly 1 volume of data).
      ++ Duplicate labels in the dataset list or in the covariates file
         will not work well!
    
    * The later columns in COVAR_FILE contain numbers (e.g., 'IQ' and 'age',
        above), OR dataset names.  In the latter case, you are specifying a
        voxel-wise covariate (e.g., 'GMfrac').
      ++ Do NOT put the dataset names or labels in this file in quotes.
    
    * A column can contain numbers only, OR datasets names only.  But one
       column CANNOT contain a mix of numbers and dataset names!
     ++ In the second line of the file (after the header line), a column entry
        that is purely numeric indicates that column will be all numbers.
     ++ A column entry that is not numeric indicates that column will be
        dataset names.
     ++ You are not required to make the columns and rows line up neatly,
        (separating entries in the same row with 1 or more blanks is OK),
        but your life will be much nicer if you DO make them well organized.
    
    * You cannot enter covariates as pure labels (e.g., 'Male' and 'Female').
       To assign such categorical covariates, you must use numeric values.
       A column in the covariates file that contains strings rather than
       numbers is assumed to be a list of dataset names, not category labels!
    
    * If you want to omit some columns in COVAR_FILE from the analysis, you
       can do so with the standard AFNI column selector '[...]'.  However,
       you MUST include column #0 first (the dataset labels) and at least
       one more column.  For example:
         -covariates Cov.table'[0,2..4]'
       to skip column #1 but keep columns #2, #3, and #4.
    
    * Only the -paired and -pooled options can be used with covariates.
      ++ If you use -unpooled, it will be changed to -pooled.
    
    * If you use -paired, then the covariate values for setB will be the
       same as those for setA, even if the dataset labels are different!
      ++ If you want to use different covariates for setA and setB in the
         paired test, then you'll have to subtract the setA and setB
         datasets (with 3dcalc), and then do a 1-sample test, using the
         differences of the original covariates as the covariates for
         this 1-sample test.
      ++ This subtraction technique works because a paired t-test is really
         the same as subtracting the paired samples and then doing a
         1-sample t-test on these differences.
      ++ For example, you do FMRI scans on a group of subjects, then
         train them on some task for a week, then re-scan them, and
         you want to use their behavioral scores on the task, pre- and
         post-training, as the covariates.
    
    * See the section 'STRUCTURE OF THE OUTPUT DATASET' for details of
       what is calculated and stored by 3dttest++.
    
    * If you are having trouble getting the program to read your covariates
      table file, then set the environment variable AFNI_DEBUG_TABLE to YES
      and run the program.  A lot of progress reports will be printed out,
      which may help pinpoint the problem; for example:
         3dttest++ -DAFNI_DEBUG_TABLE=YES -covariates cfile.txt |& more
    
    * A maximum of 31 covariates are allowed.  If you have more, then
       seriously consider the likelihood that you are completely deranged.
    
    * N.B.: The simpler forms of the COVAR_FILE that 3dMEMA allows are
            NOT supported here!  Only the format described above will work.
    
    * N.B.: IF you are entering multiple sub-bricks from the same dataset in
            one of the '-setX' options, AND you are using covariates, then
            you must use the 'LONG FORM' of input for the '-setX' option,
            and give each sub-brick a distinct label that matches something
            in the covariates file.  Otherwise, the program will not know
            which covariate to use with which input sub-brick, and bad
            things will happen.
    
    * N.B.: Please be careful in setting up the covariates file and dataset
            labels, as the program only does some simple error checking.
            ++ If you REALLY want to see the regression matrices
               used with covariates, use the '-debug' option.
            ++ Which you give you a LOT of output (to stderr), so redirect:
                 3dttest++ .... |& tee debug.out
    
    ***** CENTERING (this subject is very important -- read and think!) *******
    
     ++ This term refers to how the mean across subjects of a covariate
        will be processed.  There are 3 possibilities:
    
     -center NONE = Do not remove the mean of any covariate.
     -center DIFF = Each set will have the means removed separately.
     -center SAME = The means across both sets will be computed and removed.
                    (This option only applies to a 2-sample test, obviously.)
    
     ++ These operations (DIFF or SAME) can be altered slightly by the following:
          -cmeth MEAN   = When centering, subtract the mean.
          -cmeth MEDIAN = When centering, subtract the median.
        (Per the request of the Musical Neuroscientist, AKA Steve Gotts.)
    
     ++ If you use a voxel-wise (dataset) covariate, then the centering method
        is applied to each voxel's collection of covariate values separately.
    
     ++ The default operation is '-center DIFF'.
    
     ++ '-center NONE' is for the case where you have pre-processed the
        covariate values to meet your needs; otherwise, it is not recommended!
    
     ++ Centering can be important.  For example, suppose that the mean
        IQ in setA is significantly higher than in setB, and that the beta
        values are positively correlated with IQ.  Then the mean in
        setA will be higher than in setB simply from the IQ effect.
        To attempt to allow for this type of inter-group mean differences,
        you would have to center the two groups together, rather than
        separately (i.e., '-center SAME').
    
     ++ How to choose between '-center SAME' or '-center DIFF'?  You have
        to understand what your model is and what effect the covariates
        are likely to have on the data.  You shouldn't just blindly use
        covariates 'just in case'.  That way lies statistical madness.
      -- If the two samples don't differ much in the mean values of their
          covariates, then the results with '-center SAME' and '-center DIFF'
          should be nearly the same.
      -- For fixed covariates (not those taken from datasets), the program
          prints out the results of a t-test of the between-group mean
          covariate values.  This test is purely informative; no action is
          taken if the t-test shows that the two groups are significantly
          different in some covariate.
      -- If the two samples DO differ much in the mean values of their
          covariates, then you should read the next point carefully.
    
     ++ The principal purpose of including covariates in an analysis (ANCOVA)
        is to reduce the variance of the beta values due to extraneous causes.
        Some investigators also wish to use covariates to 'factor out' significant
        differences between groups.  However, there are those who argue
        (convincingly) that if your two groups differ markedly in their mean
        covariate values, then there is NO statistical test that can tell if
        their mean beta values (dependent variable) would be the same or
        different if their covariate values were all the same instead:
          Miller GM and Chapman JP. 'Misunderstanding analysis of covariance',
          J Abnormal Psych 110: 40-48 (2001) 
          http://dx.doi.org/10.1037/0021-843X.110.1.40
          http://psycnet.apa.org/journals/abn/110/1/40.pdf
      -- For example, if all your control subjects have high IQs and all your
          patient subjects have normal IQs, group differences in activation can
          be due to either cause (IQ or disease status) and you can't turn the
          results from a set of high IQ controls into the results you would have
          gotten from a set of normal IQ controls (so you can compare them to the
          patients) just by linear regression and then pretending the IQ issue
          goes away.
      -- The decision as to whether a mean covariate difference between groups
          makes the t-test of the mean beta difference invalid or valid isn't
          purely a statistical question; it's also a question of interpretation
          of the scientific issues of the study.  See the Miller & Chapman paper
          for a lengthy discussion of this issue.
      -- It is not clear how much difference in covariate levels is acceptable.
          You could carry out a t-test on the covariate values between the
          2 groups and if the difference in means is not significant at some
          level (i.e., if p > 0.05?), then accept the two groups as being
          'identical' in that variable.  But this is just a suggestion.
          (In fact, the program now carries out this t-test for you; cf supra.)
      -- Thanks to Andy Mayer for pointing out this article to me.
    
     ++ At this time, there is no option to force the SLOPES of the
        regression vs. covariate values to be the same in the two-sample
        analysis.  [Adding this feature would be too much like work.]
    
    -------------
    OTHER OPTIONS
    -------------
    
     -paired   = Specifies the use of a paired-sample t-test to
                  compare setA and setB.  If this option is used,
                  setA and setB must have the same cardinality (duh).
                 ++ Recall that if '-paired' is used with '-covariates',
                     the covariates for setB will be the same as for setA.
                 ++ If you don't understand the difference between a
                    paired and unpaired t-test, I'm not going to teach you
                    in this help file. But please consult someone or you
                    will undoubtedly come to grief.
    
     -unpooled = Specifies that the variance estimates for setA and
                  setB be computed separately (not pooled together).
                 ++ This only makes sense if -paired is NOT given.
                 ++ '-unpooled' cannot be used with '-covariates'.
                 ++ Unpooled variance estimates are supposed to
                     provide some protection against heteroscedasticty
                     (significantly different inter-subject variance
                     between the two different collections of datasets).
                 ++  Our experience is that for most FMRI data, using
                     '-unpooled' is not needed; the option is here for
                     those who like to experiment or who are very cautious.
    
     -toz      = Convert output t-statistics to z-scores
                 ++ -unpooled implies -toz, since t-statistics won't be
                     comparable between voxels as the number of degrees
                     of freedom will vary between voxels.
             -->>++ -toz is automatically turned on with the -Clustsim option.
                    The reason for this is that -Clustsim (and -ETAC) work by
                    specifying voxel-wise thresholds via p-values -- z-statistics
                    are simpler to compute in the external clustering programs
                    (3dClustSim and 3dXClustSim) than t-statistics, since converting
                    a z=N(0,1) value to a p-value doesn't require knowing any
                    extra parameters (such as the t DOF).
                    -- In other words, I did this to make my life simpler.
                 ++ If for some bizarre reason you want to convert a z-statistic
                    to a t-statistic, you can use 3dcalc with a clumsy expression
                    of the form
                      'cdf2stat(stat2cdf(x,5,0,0,0),3,DOF,0,0)'
                    where 'DOF' is replaced with the number of degrees of freedom.
                    The following command will show the effect of such a conversion:
                      1deval -xzero -4 -del 0.01 -num 801                         \
                             -expr 'cdf2stat(stat2cdf(x,5,0,0,0),3,10,0,0)' |     \
                      1dplot -xzero -4 -del 0.01 -stdin -xlabel z -ylabel 't(10)'
    
     -zskip [n]= Do not include voxel values that are zero in the analysis.
                 ++ This option can be used when not all subjects' datasets
                     overlap perfectly.
                 ++ -zskip implies -toz, since the number of samples per
                     voxel will now vary, so the number of degrees of
                     freedom will be spatially variable.
                 ++ If you follow '-zskip' with a positive integer (> 1),
                     then that is the minimum number of nonzero values (in
                     each of setA and setB, separately) that must be present
                     before the t-test is carried out.  If you don't give
                     this value, but DO use '-zskip', then its default is 5
                     (for no good reason).
                 ++ At this time, you can't use -zskip with -covariates,
                     because that would require more extensive re-thinking
                     and then re-programming.
                 ++ You can't use -zskip with -paired, for obvious reasons.
                 ++ You can also put a decimal fraction between 0 and 1 in
                     place of 'n' (e.g., '0.9', or '90%').  Such a value
                     indicates that at least 90% (e.g.) of the values in each
                     set must be nonzero for the t-test to proceed. [08 Nov 2010]
                     -- In no case will the number of values tested fall below 2!
                     -- You can use '100%' for 'n', to indicate that all data
                        values must be nonzero for the test to proceed.
    
     -rankize  = Convert the data (and covariates, if any) into ranks before
                  doing the 2-sample analyses.  This option is intended to make
                  the statistics more 'robust', and is inspired by the paper
                    WJ Conover and RL Iman.
                    Analysis of Covariance Using the Rank Transformation,
                    Biometrics 38: 715-724 (1982).
                    http://www.jstor.org/stable/2530051
                    Also see http://www.jstor.org/stable/2683975
                 ++ Using '-rankize' also implies '-no1sam' (infra), since it
                     doesn't make sense to do 1-sample t-tests on ranks.
                 ++ Don't use this option unless you understand what it does!
                     The use of ranks herein should be considered very
                     experimental or speculative!!
    
     -no1sam   = When you input two samples (setA and setB), normally the
                  program outputs the 1-sample test results for each set
                  (comparing to zero), as well as the 2-sample test results
                  for differences between the sets.  With '-no1sam', these
                  1-sample test results will NOT be calculated or saved.
    
     -nomeans  = You can also turn off output of the 'mean' sub-bricks, OR
     -notests  = of the 'test' sub-bricks if you want, to reduce the size of
                  the output dataset.  For example, '-nomeans -no1sam' will
                  result in only getting the t-statistics for the 2-sample
                  tests.  These options are intended for use with '-brickwise',
                  where the amount of output sub-bricks can become overwhelming.
                 ++ You CANNOT use both '-nomeans' and '-notests', because
                     then you would be asking for no outputs at all!
    
     -nocov    = Do not output the '-covariates' results.  This option is
                 intended only for internal testing, and it's hard to see
                 why the ordinary user would want it.
    
     -mask mmm = Only compute results for voxels in the specified mask.
                 ++ Voxels not in the mask will be set to 0 in the output.
                 ++ If '-mask' is not used, all voxels will be tested.
             -->>++ It is VERY important to use '-mask' when you use '-ClustSim'
                    or '-ETAC' to computed cluster-level thresholds.
                 ++ NOTE: voxels whose input data is constant (in either set)
                     will NOT be processed and will get all zero outputs.  This
                     inaction happens because the variance of a constant set of
                     data is zero, and division by zero is forbidden by the
                     Deities of Mathematics -- cf., http://www.math.ucla.edu/~tao/
    
     -exblur b  = Before doing the t-test, apply some extra blurring to the input
                  datasets; parameter 'b' is the Gaussian FWHM of the smoothing
                  kernel (in mm).
                  ++ This option is how '-ETAC_blur' is implemented, so it isn't
                     usually needed by itself.
                  ++ The blurring is done inside the mask; that is, voxels outside
                     the mask won't be used in the blurring process. Such blurring
                     is done the same way as in program 3dBlurInMask (using a
                     finite difference evolution with Neumann boundary conditions).
                  ++ Gaussian blurring is NOT additive in the FWHM parameter.
                     If the inputs to 3dttest++ were blurred by FWHM=4 mm
                     (e.g., via afni_proc.py), then giving an extra blur of
                     FWHM=6 mm is more-or-less equivalent to applying a single
                     blur of sqrt(4*4+6*6)=7.2 mm, NOT to 4+6=10 mm!
                  ++ '-exblur' does not work with '-brickwise'.
                  ++ '-exblur' only works with 3D datasets.
                  ++ If any covariates are datasets, you should be aware that the
                     covariate datasets are NOT blurred by the '-exblur' process.
    
     -brickwise = This option alters the way this program works with input
                   datasets that have multiple sub-bricks (cf. the SHORT FORM).
                  ++ If you use this option, it must appear BEFORE either '-set'
                      option (so the program knows how to do the bookkeeping
                      for the input datasets).
                  ++ WITHOUT '-brickwise', all the input sub-bricks from all
                      datasets in '-setA' are gathered together to form the setA
                      sample (similarly for setB, of course).  In this case, there
                      is no requirement that all input datasets have the same
                      number of sub-bricks.
                  ++ WITH '-brickwise', all input datasets (in both sets)
                      MUST have the same number of sub-bricks.  The t-tests
                      are then carried out sub-brick by sub-brick; that is,
                      if you input a collection of datasets with 10 sub-bricks
                      in each dataset, then you will get 10 t-test results.
                  ++ Each t-test result will be made up of more than 1 sub-brick
                      in the output dataset.  If you are doing a 2-sample test,
                      you might want to use '-no1sam' to reduce the number of
                      volumes in the output dataset.  In addition, if you are
                      only interested in the statistical tests and not the means
                      (or slopes for covariates), then the option '-nomeans'
                      will reduce the dataset to just the t (or z) statistics
                      -- e.g., the combination '-no1sam -nomeans' will give you
                         one statistical sub-brick per input sub-brick.
                  ++ If you input a LOT of sub-bricks, you might want to set
                      environment variable AFNI_AUTOMATIC_FDR to NO, in order
                      to suppress the automatic calculation of FDR curves for
                      each t-statistic sub-brick -- this FDR calculation can
                      be time consuming when done en masse.
              -->>++ The intended application of this option is to make it
                      easy to take a collection of time-dependent datasets
                      (e.g., from MEG or from moving-window RS-FMRI analyses),
                      and get time-dependent t-test results.  It is possible to do
                      the same thing with a scripted loop, but that way is painful.
                  ++ You CAN use '-covariates' with '-brickwise'. You should note
                      that each t-test will re-use the same covariates -- that is,
                      there is no provision for time-dependent covariate values --
                      for that, you'd have to use scripting to run 3dttest++
                      multiple times.
                  ++ EXAMPLE:
                      Each input dataset (meg*.nii) has 100 time points; the 'X'
                      datasets are for one test condition and the 'Y' datasets are
                      for another. In this example, the subjects are the same in
                      both conditions, so the '-paired' option makes sense.
                        3dttest++ -brickwise -prefix megXY.nii -no1sam -paired\
                                  -setA meg01X.nii meg02X.nii meg03X.nii ... \
                                  -setB meg01Y.nii meg02Y.nii meg03Y.nii ... 
                    * The output dataset will have 200 sub-bricks: 100 differences
                       of the means between 'X' and 'Y', and 100 t-statistics.
                    * You could extract the output dataset t-statistics (say)
                       into a single dataset with a command like
                         3dTcat -prefix megXY_tstat.nii megXY.nii'[1..$(2)]'
                       (Or you could have used the '-nomeans' option.)
                       This dataset could then be used to plot the t-statistic
                       versus time, make a movie, or otherwise do lots of fun things.
                    * If '-brickwise' were NOT used, the output dataset would just
                       get 2 sub-bricks, as all the inputs in setA would be lumped
                       together into one super-sized sample (and similarly for setB).
                    * Remember that with the SHORT FORM input (needed for option
                       '-brickwise') you can use wildcards '*' and '?' together with
                       '[...]' sub-brick selectors.
    
     -prefix p = Gives the name of the output dataset file.
                  ++ For surface-based datasets, use something like:
                      -prefix p.niml.dset or -prefix p.gii.dset 
                     Otherwise you may end up files containing numbers but
                     not a full set of header information.
    
     -resid q  = Output the residuals into a dataset with prefix 'q'.
                  ++ The residuals are the difference between the data values
                     and their prediction from the set mean (and set covariates).
                  ++ For use in further analysis of the results (e.g., 3dFWHMx).
                  ++ Cannot be used with '-brickwise' (sorry).
                  ++ If used with '-zskip', values which were skipped in the
                     analysis will get residuals set to zero.
    
     -ACF      = If residuals are saved, also compute the ACF parameters from
                 them using program 3dFHWMx -- for further use in 3dClustSim
                 (which must be run separately).
                 ++ HOWEVER, the '-Clustsim' option below provides a resampling
                    alternative to using the parameteric '-ACF' method in
                    program 3dClustSim.
    
     -dupe_ok  = Duplicate dataset labels are OK.  Do not generate warnings
                 for dataset pairs.
                ** This option must preceed the corresponding -setX options.
                ** Such warnings are issued only when '-covariates' is used
                   -- when the labels are used to extract covariate values
                   from the covariate table.
    
     -debug    = Prints out information about the analysis, which can
                  be VERY lengthy -- not for general usage (or even for colonels).
                 ++ Two copies of '-debug' will give even MORE output!
    
    -----------------------------------------------------------------------------
    ClustSim Options -- for global cluster-level thresholding and FPR control
    -----------------------------------------------------------------------------
    
    The following options are for using randomization/permutation to simulate
    noise-only generated t-tests, and then run those results through the
    cluster-size threshold simulation program 3dClustSim. The goal is to
    compute cluster-size thresholds that are not based on a fixed model
    for the spatial autocorrelation function (ACF) of the noise.
    
    ETAC (infra) and ClustSim are parallelized. The randomized t-test steps are
    done by spawning multiple 3dttest++ jobs using the residuals as input.
    Then the 3dClustSim program (for -Clustsim) and 3dXClustSim program (for -ETAC)
    use multi-threaded processing to carry out their clusterization statistics.
    If your computer does NOT have multiple CPU cores, then these options will
    run very slowly.
    
    You can use both -ETAC and -Clustsim in the same run. The main reason for
    doing this is to compare the results of the two methods. Using both methods
    in one 3dttest++ run will be very slow.
     ++ In such a dual-use case, and if '-ETAC_blur' is also given, note that
         3dClustSim will be run once for each blur level, giving a set of cluster-
         size threshold tables for each blur case. This process is necessary since
         3dClustSim does not have a multi-blur thresholding capability, unlike
         ETAC (via program 3dXClustSim).
     ++ The resulting 3dClustSim tables are to be applied to each of the auxiliary
         t-test files produced, one for each blur case. Unless one of those blur
         cases is '0.0', the 3dClustSim tables do NOT apply to the main output
         dataset produced by this program.
     ++ These auxiliary blur case t-test results get names of the form
           PREFIX.B8.0.nii
        where PREFIX was given in the '-prefix' option, and in this example,
        the amount of extra blurring was 8.0 mm. These files are the result
        of re-running the commanded t-tests using blurred input datasets.
    
     -Clustsim   = With this option, after the commanded t-tests are done, then:
                    (a) the residuals from '-resid' are used with '-randomsign' to
                        simulate about 10000 null 3D results, and then
                    (b) 3dClustSim is run with those to generate cluster-threshold
                        tables, and then
                    (c) 3drefit is used to pack those tables into the main output
                        dataset, and then
                    (d) the temporary files created in this process are deleted.
                   The goal is to provide a method for cluster-level statistical
                   inference in the output dataset, to be used with the AFNI GUI
                   Clusterize controls.
                  ++ If you want to keep ALL the temporary files, use '-CLUSTSIM'.
                  ++ Since the simulations are done with '-toz' active, the program
                     also turns on the '-toz' option for your output dataset. This
                     means that the output statistics will be z-scores, not t-values.
                  ++ If you have less than 14 datasets total (setA & setB combined),
                     this option will not work! (There aren't enough random subsets.)
                   ** And it will not work with '-singletonA'.
              -->>++ '-Clustsim' runs step (a) in multiple jobs, for speed.  By
                     default, it tries to auto-detect the number of CPUs on the 
                     system and uses that many separate jobs.  If you put a positive
                     integer immediately following the option, as in '-Clustsim 12',
                     it will instead use that many jobs (e.g., 12).  This capability
                     is to be used when the CPU count is not auto-detected correctly.
                   ** You can also set the number of CPUs to be used via the Unix
                      environment variable OMP_NUM_THREADS.
              -->>++ It is important to use a proper '-mask' option with '-Clustsim'.
                     Otherwise, the statistics of the clustering will be skewed.
              -->>++ You can change the number of simulations from the default 10000
                     by setting Unix environment variable AFNI_TTEST_NUMCSIM to a
                     different value (in the range 1000..1000000). Note that the
                     3dClustSim tables go down to a cluster-corrected false positive
                     rate of 0.01, so that reducing the number of simulations below
                     10000 will produce notably less accurate results for such small
                     FPR (alpha) values.
            **-->>++ The primary reason for reducing AFNI_TTEST_NUMCSIM below its
                     default value is testing '-Clustsim' and/or '-ETAC' more quickly
              -->>++ The clever scripter can pick out a particular value from a
                     particular 3dClustSim output .1D file using the '{row}[col]'
                     syntax of AFNI, as in the tcsh command
                       set csize = `1dcat Fred.NN1_1sided.1D"{10}[6]"`
                     to pick out the number in the #10 row, #6 column (counting
                     from #0), which is the p=0.010 FPR=0.05 entry in the table.
                     (-: Further adventures in scripting I leave to your whimsy :-)
    
      ---==>>> PLEASE NOTE: This option has been tested for 1- and 2-sample
      ---==>>> unpaired and paired tests vs. resting state data -- to see if the
      ---==>>> false positive rate (FPR) was near the nominal 5% level (it was).
      ---==>>> The FPR for the covariate effects (as opposed to the main effect)
      ---==>>> is still somewhat biased away from the 5% level /:(
    
     ****** The following options affect both '-Clustsim' and '-ETAC' ******
    
     -prefix_clustsim cc = Use 'cc' for the prefix for the '-Clustsim' temporary
                           files, rather than a randomly generated prefix.
                           You might find this useful if scripting.
                          ++ By default, the Clustsim (and ETAC) prefix will
                             be the same as that given by '-prefix'.
                      -->>++ If you use option '-Clustsim', then the simulations
                             keep track of the maximum (in mask) voxelwise
                             z-statistic, compute the threshold for 5% global FPR,
                             and write those values (for 1-sided and 2-sided
                             thresholding) to a file named 'cc'.5percent.txt --
                             where 'cc' is the prefix given here. Using such a
                             threshold in the AFNI GUI will (presumably) give you
                             a map with a 5% chance of false positive WITHOUT
                             clustering. Of course, these thresholds generally come
                             with a VERY stringent per-voxel p-value.
                            ** In one analysis, the 5% 2-sided test FPR p-value was
                               about 7e-6 for a mask of 43000 voxels, which is
                               bigger (less strict) than the 1.2e-6 one would get
                               from the Bonferroni correction, but is still very
                               stringent for many purposes. This threshold value
                               was also close to the threshold at which the FDR
                               q=1/43000, which may not be a coincidence.
                      -->>++ This file has been updated to give the voxel-wise
                             statistic threshold for global FPRs from 1% to 9%.
                             However, the name is still '.5percent.txt' for the
                             sake of nostalgia.
    
     -no5percent         = Don't output the 'cc'.5percent.txt file that comes
                           for free with '-Clustsim' and/or '-ETAC'.
                         ++ But whyyy? Don't you like free things?
    
     -tempdir ttt        = Store temporary files for '-Clustsim' in this directory,
                           rather than in the current working directory.
                     -->>++ This option is for use when you have access to a fast
                            local disk (e.g., SSD) compared to general storage
                            on a rotating disk, RAID, or network storage.
                         ++ Using '-tempdir' can make a significant difference
                            in '-Clustsim' and '-ETAC' runtime, if you have
                            a local solid state drive available!
                           [NOTE: with '-CLUSTSIM', these files aren't deleted!]
    
     -seed X [Y] = This option is used to set the random number seed for
                   '-randomsign' to the positive integer 'X'. If a second integer
                   'Y' follows, then that value is used for the random number seed
                   for '-permute'.
                 ++ The purpose of setting seeds (rather than letting the program
                    pick them) is for reproducibility. It is not usually needed by
                    the ordinary user.
                 ++ Option '-seed' is used by the multi-blur analysis possible
                    with '-ETAC', so that the different blur levels use the same
                    randomizations, to make their results compatible for multi-
                    threshold combination.
                 ++ Example:  -seed 3217343 1830201
    
     ***** These options (below) are not often directly used, but *****
     ***** are described here for completeness and for reference. *****
     ***** They are invoked by options '-Clustsim' and '-ETAC'.   *****
    
     -randomsign = Randomize the signs of the datasets.  Intended to be used
                   with the output of '-resid' to generate null hypothesis
                   statistics in a second run of the program (probably using
                   '-nomeans' and '-toz').  Cannot be used with '-singletonA'
                   or with '-brickwise'.
                 ++ You will never get an 'all positive' or 'all negative' sign
                    flipping case -- each sign will be present at least 15%
                    of the time.
                 ++ There must be at least 4 samples in each input set to
                    use this option, and at least a total of 14 samples in
                    setA and setB combined.
                 ++ If you following '-randomsign' with a number (e.g.,
                    '-randomsign 1000'), then you will get 1000 iterations
                    of random sign flipping, so you will get 1000 times the
                    as many output sub-bricks as usual. This is intended for
                    for use with simulations such as '3dClustSim -inset'.
             -->>++ This option is usually not used directly, but will be
                    invoked by the use of '-Clustsim'.  It is documented here
                    for the sake of telling the Galaxy how the program works.
    
     -permute    = With '-randomsign', and when both '-setA' and '-setB' are used,
                   this option will add inter-set permutation to the randomization.
                 ++ If only '-setA' is used (1-sample test), there is no permutation.
                 ++ If '-randomsign' is NOT given, but '-Clustsim' is used, then
                    '-permute' will be passed for use with the '-Clustsim' tests
                    (again, only if '-setA' and '-setB' are both used).
                 ++ If '-randomsign' is given and if the following conditions
                    are ALL true, then '-permute' is assumed:
                      (a) You have a 2-sample test.
                          [Permutation is meaningless without 2 samples!]
                      (b) You are not using '-unpooled'.
                      (c) You are not using '-paired'.
                      (c) You are not using '-covariates'.
             -->>++ You only NEED to use '-permute' if you want inter-set
                    permutation used AND you give at least one of '-unpooled' or
                    '-paired' or '-covariates'. Normally, you don't need '-permute'.
                 ++ There is no option to do permutation WITHOUT sign randomization.
             -->>++ This option is also not usually used directly by the user;
                    it will be invoked by the '-Clustsim' or '-ETAC' operations.
    
     -nopermute  = This option is present if you want to turn OFF the automatic
                   use of inter-set permutation with '-randomsign'.
                 ++ I'm not sure WHY you would want this option, but it is here
                    for completeness of the Galactic Chronosynclastic Infundibulum.
    
    ------------
    ETAC Options -- [promulgated May 2017 == still experimental!]
    ------------
    
    The following options use the ETAC (Equitable Thresholding And Clustering)
    method to provide a method for thresholding the results of 3dttest++.
    -ETAC uses randomization/permutation to generate null distributions,
    as does -Clustsim. The main difference is that ETAC also allows:
      * use of multiple per-voxel p-value thresholds simultaneously
      * use of cluster-size and/or cluster-square-sum as threshold parameters
      * use of multiple amounts of blurring simultaneously
      * use of spatially variable cluster sizes.
    
    'Equitable' means that each combination of the above choices is treated
    to contribute approximately the same to the False Positive Rate (FPR).
    The FPR is also balanced across voxels, so that the cluster-FOM thresholds
    are depend on location -- that is, brain regions that have less intrinsic
    smoothness will tend to get smaller thresholds (unlike the global -Clustsim).
    In FMRI, this seems to mean that the base (ventral part) of the brain gets
    the smallest thresholds and the top (superior occipital and retrosplenial)
    parts of the brain get the largest thresholds. (YMMV :)
    
    Major differences between '-Clustsim' and '-ETAC':
     * -Clustsim produces a number: the cluster-size threshold to be used everywhere.
     * -ETAC produces a map: the cluster figure of merit (FOM) threshold to be
         used as a function of location.
     * -ETAC allows use of a FOM that is more general than the cluster-size.
     * -ETAC allows the use of multiple per-voxel p-value thresholds simultaneously.
     * -ETAC allows the use of multiple blur levels simultaneously.
    
     *** ALSO see the description of the '-prefix_clustsim', '-tempdir', and  ***
     *** '-seed' options above, since these also affect the operation of ETAC ***
    
     *** The 'goal' of ETAC is a set of thresholds that give a 5% FPR. You   ***
     *** can modify this goal by setting the 'fpr=' parameter via '-ETAC_opt' ***
    
     * ETAC can use a lot of memory; about 100000 * Ncase * Nmask bytes,
       where Ncase = number of blur cases in option '-ETAC_blur' and
             Nmask = number of voxels in the mask.
       For example, 50000 voxels in the mask and 4 blur cases might use about
       50000 * 100000 * 4 = 20 billion bytes of memory.
     * Run time depends a lot on the parameters and the computer hardware, but
       will typically be 10-100 minutes. Get another cup of tea (or coffee).
    
             *** You should use ETAC only on a computer with ***
             ***     multiple CPU cores and lots of RAM!     ***
    
             ***    If 3dXClustSim fails with the message    ***
             ***   'Killed', this means that the operating   ***
             ***   system stopped the program for trying to  ***
             ***           use too much memory.              ***
    
     -ETAC [ncpu]         = This option turns ETAC computations on.
                           ++ You can put the maximum number of CPUs to use
                              after '-ETAC' if you want, but it is usually
                              not needed -- just let the program choose.
                           ++ The ETAC algorithms are implemented in program
                              3dXClustSim, which 3dttest++ will run for you.
                           ++ As with '-Clustsim', you can put the number of CPUs
                              to be used after the '-ETAC' option, or let the
                              program figure out how many to use.
    
     -ETAC_mem            = This option tells the program to print out the
                            estimate of how much memory is required by the ETAC
                            run ordered, and then stop.
                           ++ No data analysis of any kind will be performed.
                           ++ You have to give all the options (-setA, -ETAC, etc.)
                              that you would use to run the analysis.
                           ++ The purpose of this option is to help you choose
                              the computer setup for your run.
    
     -ETAC_blur b1 b2 ... = This option says to use multiple levels of spatial
                            blurring in the t-tests and ETAC analysis.
                           ++ If you do NOT use -ETAC_blur, then no extra
                              blurring is used, beyond whatever might have
                              been used on the inputs to 3dttest++.
                           ++ Note that Gaussian blurring is NOT additive
                              in the FWHM parameter, but is rather additive in
                              the square of FWHM. If the inputs to 3dttest++
                              are blurred by FWHM=4 mm (for example), then giving
                              an extra blur of FWHM=6 mm is equivalent to a
                              single blur of sqrt(4*4+6*6)=7.2 mm, NOT to 10 mm!
                           ++ The list of blur FWHM parameters can have up to 5
                              entries, but I recommend no more than 2 or 3 of them.
                              3dXClustSim memory usage goes up sharply as the
                              number of blur cases rises.
                           ++ You can use '0' for one of the blur parameters here,
                              meaning to not apply any extra blurring for that case.
                           ++ You can only use '-ETAC_blur' once.
    
     -ETAC_opt params     = This option lets you choose the non-blurring parameters
                            for ETAC. You can use this option more than once, to
                            have different thresholding cases computed. The 'params'
                            string is one argument, with different parts separated
                            by colon ':' characters. The parts are
                        NN=1 or NN=2 or NN=3 } spatial connectivity for clustering
                        sid=1 or sid=2       } 1-sided or 2-sided t-tests
                        pthr=p1,p2,...       } list of p-values to use
                        hpow=h1,h2,...       } list of H powers (0, 1, and/or 2)
                        fpr=value            } FPR goal, between 2 and 9 (percent)
                                             } - must be an integer
                                             } - or the word 'ALL' to output
                                             }   results for 2, 3, 4, ..., 9.
                        name=Something       } a label to distinguish this case
                            For example:
                 -ETAC_opt NN=2:sid=2:hpow=0,2:pthr=0.01,0.005,0.002,0.01:name=Fred
                            The H powers ('hpow') allowed are 0, 1, and/or 2;
                            the clustering figure of merit (FOM) is defined as the
                            sum over voxels in a cluster of the voxel absolute
                            z-scores raised to the H power; H=0 is the number of
                            voxels in a cluster (what 3dClustSim uses).
                           ++ You can use '-ETAC_opt' more than once, to make
                              efficient re-use of the randomized/permuted cases.
                         -->> Just give each use within the same 3dttest++ run a
                              different label after 'name='.
                           ++ There's no built-in upper limit to the number of
                              '-ETAC_opt' cases you can run.
                              Each time you use '-ETAC_opt', 3dXClustSim will be
                              run (using the same set of randomizations).
                           ++ It is important to use distinct names for each
                              different '-ETAC_opt' case, so that the output
                              file names will be distinct (see below).
                           ++ If you do not use '-ETAC_opt' at all, a built-in set
                              of parameters will be used. These are
                                NN=2 sid=2 hpow=2 name=default
                                pthr=0.01,0.0056,0.0031,0.0018,0.0010
                                    =0.01 * 0.1^(i/4) for i=0..4
                                    =geometrically distributed from 0.001 to 0.01
                                fpr=5
    
     -ETAC_arg something  = This option is used to pass extra options to the
                            3dXClustSim program (which is what implements ETAC).
                            There is almost no reason to use this option that I
                            can think of, except perhaps this example:
                              -ETAC_arg -verb
                            which will cause 3dXClustSim to print more verbose
                            information as it progresses through the ETAC stages.
    
    -----------------
    ETAC Output Files
    -----------------
    ETAC produces a number of output files. Some of these are the multi-threshold
    datasets that can be used with program 3dMultiThresh to get thresholded
    results. Others of these are a binary mask that indicate which voxels passed
    these at least one of the multiple tests, and another mask that indicates
    which tests were passed (in each voxel). These masks are produced by running
    3dMultiThresh for each blur case, then combining the results across blur cases.
    
    In the example below, assume
      * Two blurring cases are specified using '-ETAC_blur 4 7'
      * The prefix for normal 3dttest++ files is 'P', as in '-prefix P'
      * The prefix for ETAC output files is 'Px', as in '-prefix_clustsim Px'
      * The name for the ETAC analysis is 'name=N' in option '-ETAC_opt'
        (remember, you can run more than one ETAC analysis in a single 3dttest++)
      * That a 2-sided analysis is ordered with 'sid=2 in option '-ETAC_opt'
      * The default 'fpr=5' is used in option '-ETAC_opt'
    
    Output filename                     Description and Contents
    ----------------------------------  -------------------------------------------
    P+tlrc.HEAD                         normal 3dttest++ output from input datasets
    P.B4.0.nii                          3dttest++ output from blurred datasets
    P.B7.0.nii                            (4 and 7 mm, respectively)
    Px.B4.0.5percent.txt                voxel-wise threshold list for a variety
    Px.B7.0.5percent.txt                  of global FPRs, for blurs 4 and 7
    Px.N.ETAC.mthresh.B4.0.5perc.nii    Multi-threshold datasets for blur=4 and =7,
    Px.N.ETAC.mthresh.B7.0.5perc.nii      for overall 5% global false positive rate
    Px.N.ETACmask.2sid.5perc.nii.gz     Binary (0 or 1) mask of 'active voxels'
    PX.N.ETACmaskALL.2sid.5perc.nii.gz  Multi-volume mask showing which ETAC
                                          sub-method(s) passed in each voxel:
                                          There is one sub-brick per p-value,
                                          per blur case (e.g., 5*2=10), and each
                                          mask value encodes which hpow value(s)
                                          had a positive result, as the sum of
                                            1 == hpow=0 passed
                                            2 == hpow=1 passed
                                            4 == hpow=2 passed
                                          Sub-bricks in this dataset will have
                                          labels of the form
                                            'B4.0:p=0.0100'
                                          indicating the sub-method was blur=4
                                          with pthr=0.01.
    * If a different 'fpr' value was given (say 2), then the filenames containing
      'ETAC' will have the '5perc' component changed to that value (e.g., '4perc').
    * If 'fpr=ALL', there would be outputs for '2perc', '3perc', ... '9perc'.
    * If 'sid=1' were given in '-ETAC_opt', then each mask filename containing
      '2sid' will instead be replaced by TWO files, one with '1neg' and one
      with '1pos', indicating the results of 1-sided t-test thresholding with
      the negative and positive sides, respectively.
    * It is quite possible that the various ETACmask files are all zero,
      indicating that nothing survived the multi-thresholding operations.
    -----------
    *** WARNING: ETAC consumes a lot of CPU time, and a lot of memory  ***
    ***         (especially with many -ETAC_blur cases, or 'fpr=ALL')! ***
    
    +++ (: One of these days, I'll expand this section and explain ETAC more :) +++
    +++ (: ------------------------------ MAYBE ---------------------------- :) +++
    -------------------------------------------------------------------------------
    
    -------------------------------
    STRUCTURE OF THE OUTPUT DATASET
    -------------------------------
    
    * The output dataset is stored in float format; there is no option
       to store it in scaled short format :)
    
    * For each covariate, 2 sub-bricks are produced:
      ++ The estimated slope of the beta values vs covariate
      ++ The t-statistic of this slope
      ++ If there are 2 sets of subjects, then each pair of sub-bricks is
          produced for the setA-setB, setA, and setB cases, so that you'll
          get 6 sub-bricks per covariate (plus 6 more for the mean, which
          is treated as a special covariate whose values are all 1).
      ++ Thus the number of sub-bricks produced is 6*(m+1) for the two-sample
          case and 2*(m+1) for the one-sample case, where m=number of covariates.
    
    * For example, if there is one covariate 'IQ', and a two sample analysis
       is carried out ('-setA' and '-setB' both used), then the output
       dataset will contain the following 12 (6*2) sub-bricks:
          #0  SetA-SetB_mean      = difference of means [covariates removed]
          #1  SetA-SetB_Tstat
          #2  SetA-SetB_IQ        = difference of slopes wrt covariate IQ
          #3  SetA-SetB_IQ_Tstat
          #4  SetA_mean           = mean of SetA [covariates removed]
          #5  SetA_Tstat
          #6  SetA_IQ             = slope of SetA wrt covariate IQ
          #7  SetA_IQ_Tstat
          #8  SetB_mean           = mean of SetB [covariates removed]
          #9  SetB_Tstat
          #10 SetB_IQ             = slope of SetB wrt covariate IQ
          #11 SetB_IQ_Tstat
    
    * In the above, 'wrt' is standard mathematical shorthand for the
       phrase 'with respect to'.
    
    * If option '-BminusA' is given, then the 'SetA-SetB' sub-bricks would
       be labeled 'SetB-SetA' instead, of course.
    
    * If option '-toz' is used, the 'Tstat' will be replaced with 'Zscr'
       in the statistical sub-brick labels.
    
    * If the long form of '-setA' is used, or '-labelA' is given, then
       'SetA' in the sub-brick labels above is replaced with the
       corresponding SETNAME.  (Mutatis mutandis for 'SetB'.)
    
    * If you produce a NIfTI-1 (.nii) file, then the sub-brick labels are
       saved in the AFNI extension in the .nii file.  Processing further
       in non-AFNI programs will probably cause these labels to be lost
       (along with other AFNI niceties, such as the history field).
    
    * If you are doing a 2-sample run and don't want the 1-sample results,
       then the '-no1sam' option can be used to eliminate these sub-bricks
       from the output, saving space and time and mental energy.
    
    * The largest Tstat that will be output is 99.
    * The largest Zscr that will be output is 13.
      ++ FYI: the 1-sided Gaussian tail probability of z=13 is 6.1e-39.
    
    -------------------
    HOW COVARIATES WORK
    -------------------
    
    Covariates work by forming a regression problem for each voxel, to
    estimate the mean of the input data and the slopes of the data with
    respect to variations in the covariates.
    
    For each input set of sub-bricks, a matrix is assembled.  There is one
    row for each sub-brick, and one column for each covariate, plus one
    more column for the mean.  So if there are 5 sub-bricks and 2 covariates,
    the matrix would look like so
    
         [ 1  0.3  1.7 ]
         [ 1  0.5  2.2 ]
     X = [ 1  2.3  3.3 ]
         [ 1  5.7  7.9 ]
         [ 1  1.2  4.9 ]
    
    The first column is all 1s, and models the mean value of the betas.
    The remaining columns are the covariates for each sub-brick.  (The
    numbers above are values I just made up, obviously.)
    
    The matrix is centered by removing the mean from each column except
    the first one.  In the above matrix, the mean of column #2 is 2,
    and the mean of column #3 is 4, so the centered matrix is
    
          [ 1 -1.7 -2.3 ]
          [ 1 -1.5 -1.8 ]
     Xc = [ 1  0.3 -0.7 ]
          [ 1  3.7  3.9 ]
          [ 1 -0.8  0.9 ]
    
    (N.B.: more than one centering option is available; this is the default.)
    
    The set of equations to be solved is [Xc] [b] = [z], where [b] is
    the column vector desired (first element = de-covariate-ized mean
    of the data values, remaining elements = slopes of data values
    with respect to the covariates), and [z] is the column vector of
    data values extracted from the input datasets.
    
    This set of equations is solved by forming the pseudo-inverse of the
    matrix [Xc]: [Xp] = inverse[Xc'Xc] [Xc'], so that [b] = [Xp] [z].
    (Here, ' means transpose.) For the sample matrix above, we have
    
          [  0.2         0.2         0.2       0.2        0.2      ]
     Xp = [  0.0431649  -0.015954    0.252887  0.166557  -0.446654 ]
          [ -0.126519   -0.0590721  -0.231052  0.0219866  0.394657 ]
    
    Because of the centering, the first column of [Xc] is orthgonal to
    the other columns, so the first row of [Xp] is all 1/N, where N is
    the number of data points (here, N=5).
    
    In reality, the pseudo-inverse [Xp] is computed using the SVD, which
    means that even a column of all zero covariates will not cause a
    singular matrix problem.
    
    In addition, the matrix [Xi] = inverse[Xc'Xc] is computed.  Its diagonal
    elements are needed in the t-test computations.  In the above example,
    
          [ 0.2 0        0       ]
     Xi = [ 0   0.29331 -0.23556 ]
          [ 0  -0.23556  0.22912 ]
    
    For a 1-sample t-test, the regression values computed in [b] are the
    '_mean' values stored in the output dataset.  The t-statistics are
    computed by first calculating the regression residual vector
      [r] = [Xc][b] - [z]  (the mismatch between the data and the model)
    and then the estimated variance v of the residuals is given by
    
            i=N
      q = sum  { r[i]*r[i] }  and then  v = q / (N-m)
            i=1
    
    where N=number of data points and m=number of matrix columns=number of
    parameters estimated in the regression model.  The t-statistic for the
    k-th element of [b] is then given by
    
      t[k] = b[k] / sqrt( v * Xi[k,k] )
    
    Note that for the first element, the factor Xi[1,1] is just 1/N, as
    is the case in the simple (no covariates) t-test.
    
    For a 2-sample unpaired t-test, the '_mean' output for the k-th column
    of the matrix [X] is bA[k]-bB[k] where 'A' and 'B' refer to the 2 input
    collections of datasets.  The t-statistic is computed by
    
      vAB  = (qA+qB) / (NA+NB-2*m)
    
      t[k] = (bA[k]-bB[k]) / sqrt( vAB * (XiA[k,k]+XiB[k,k]) )
    
    For a 2-sample paired t-test, the t-statistic is a little different:
    
            i=N
      q = sum  { (rA[i]-rB[i])^2 }  and then  vAB = q / (N-m)
            i=1
    
    and then
    
      t[k] = (bA[k]-bB[k]) / sqrt( vAB * XiA[k,k] )
    
    A paired t-test is basically a 1-sample test with the 'data' being
    the difference [zA]-[zB] of the two input samples.
    
    Note the central role of the diagonal elements of the [Xi] matrix.
    These numbers are the variances of the estimates of the [b] if the
    data [z] is corrupted by additive white noise with variance=1.
    (In the case of an all zero column of covariates, the SVD inversion)
    (that yields [Xi] will make that diagonal element 0.  Division by 0)
    (being a not-good thing, in such a case Xi[k,k] is replaced by 1e9.)
    
    For cases with voxel-wise covariates, each voxel gets a different
    [X] matrix, and so the matrix inversions are carried out many many
    times.  If the covariates are fixed values, then only one set of
    matrix inversions needs to be carried out.
    
    -------------------------------------------
    HOW SINGLETON TESTING WORKS WITH COVARIATES
    -------------------------------------------
    
    (1) For setB, the standard regression is carried out to give the
        covariate slope estimates (at each voxel):
          [b] = [Xp] [z]
        where [z]  = column vector of the setB values
              [Xp] = pseudo-inverse of the [X] matrix for the setB covariates
              [b]  = covariate parameter estimates
        Under the usual assumptions, [b] has mean [b_truth] and covariance
        matrix sigma^2 [Xi], where sigma^2 = variance of the zB values, and
        [Xi] = inverse[X'X].  (Again, ' = tranpose.)
        (If centering is used, [X] is replaced by [Xc] in all of the above.)
    
    (2) Call the singletonA value (at each voxel) y;
        then the statistical model for y is
           y = yoff + [c]'[b_truth] + Normal(0,sigma^2)
        where the column vector [c] is the transpose of the 1-row matrix [X]
        for the singletonA dataset -- that is, the first element of [c] is 1,
        and the other elements are the covariate values for this dataset.
        (The null hypothesis is that the mean offset yoff is 0.)
        The covariate slopes [b] from step (1) are projected out of y now:
          y0 = y - [c]'[b]
        which under the null hypothesis has mean 0 and variance
          sigma^2 ( 1 + [c]'[Xi][c] )
        Here, the '1' comes from the variance of y, and the [c]'[Xi][c] comes
        from the variance of [b] dotted with [c].  Note that in the trivial
        case of no covariates, [X] = 1-column matrix of all 1s and [c] = scalar
        value of 1, so [c]'[Xi][c] = 1/N where N = number of datasets in setB.
    
    (3) sigma^2 is as usual estimated by s^2 = sum[ (z_i - mean(z))^2 ] / (N-m-1)
        where N = number of datasets in setB and m = number of covariates.
        Under the usual assumptions, s^2 is distributed like a random variable
        ( sigma^2 / (N-m-1) ) * ChiSquared(N-m-1).
    
    (4) Consider the test statistic
          tau = y0 / sqrt(s^2)
        Under the null hypothesis, this has the distribution of a random variable
          Normal(0,1 + [c]'[Xi][c]) / sqrt( ChiSquared(N-m-1)/(N-m-1) )
        So tau is not quite t-distributed, but dividing out the scale factor works:
          t = y0 / sqrt( s^2 * (1 + [c]'[Xi][c]) )
        and under the null hypothesis, this value t has a Student(N-m-1) distribution.
        Again, note that in the case of no covariates, [c]'[Xi][c] = 1/N, so that
          t = y / sqrt( s^2 * (1+1/N) )
        If we were testing against a constant y, rather than y itself being random,
        we'd have
          t_con = y / sqrt( s^2 / (N-1) )
        which shows that the t statistic for the '-singletonA' test will usually be
        much smaller than the t statistic for the 'test against constant' case --
        because we have to allow for the variance of the singleton dataset value y.
    
    Please note that the singleton dataset is assumed to be statistically
    independent of the reference datasets -- if you put the singleton dataset
    into the reference collection, then you are violating this assumption --
    a different statistic would have to be computed.
    
    A test script that simulates random values and covariates has verified the
    distribution of the results in both the null hypothesis (yoff == 0) case and the
    alternative hypothesis (yoff !=0) case -- where the value t now takes on the
    non-central Student distribution.
    
    Below is a sketch of how a covariate might be useful in singleton tests:
     * the 'z' labels are voxel values from setB
     * the 'y' label is the voxel value from singletonA
     * y is not markedly different from some of the z values
     * but for the singleton subject's age, y IS very different
     * a test WITHOUT the age covariate would not give a large t-statistic for y
     * a test WITH the age covariate will show a larger t-statistic for y
                  --------------------------------
                D |                   z          |
                a |                      z       |
                t |              z  z  z   z     |
                a |            z z z  z          |
                  |          z z  z  z  z        |
                v |        z z   z  z z          |
                a |       z z   z z z            |
                l |    z  z   z   z              |
                u |   z    z   z           y     |
                e |      z  z                    |
                  |                              |
                  |                              |
                  |                              |
                  --------------------------------
                         Subject age
    
    After linear regression removes the covariate effect (values at smaller
    ages are increased and values at larger ages are decreased), the cartoon
    graph would look something like this, where the modified y value is
    now clearly far away from the cluster of z values:
                  --------------------------------
              R D |                              |
              e a |                              |
              g t |    z       z z               |
              r a |   z   zz z z z  z z          |
              e   |       z  z    zz             |
              s v |      z  z    z     z z       |
              s a |        z  z z z zzz    z     |
              e l |            z  z z            |
              d u |         z         z z        |
                e |                              |
                  |                              |
                  |                              |
                  |                        y     |
                  --------------------------------
                         Subject age
    
    ---------------------
    A NOTE ABOUT p-VALUES (everyone's favorite subject :)
    ---------------------
    
    The 2-sided p-value of a t-statistic value T is the likelihood (probability)
    that the absolute value of the t-statistic computation would be bigger than
    the absolute value of T, IF the null hypothesis of no difference in the means
    (2-sample test) were true.  For example, with 30 degrees of freedom, a T-value
    of 2.1 has a p-value of 0.0442 -- that is, if the null hypothesis is true
    and you repeated the experiment a lot of times, only 4.42% of the time would
    the T-value get to be 2.1 or bigger (and -2.1 or more negative).
    
    You can NOT interpret this to mean that the alternative hypothesis (that the
    means are different) is 95.58% likely to be true.  (After all, this T-value
    shows a pretty weak effect size -- difference in the means for a 2-sample
    t-test, magnitude of the mean for a 1-sample t-test, scaled by the standard
    deviation of the noise in the samples.)  A better way to think about it is
    to pose the following question:
         Assuming that the alternative hypothesis is true, how likely
         is it that you would get the p-value of 0.0442, versus how
         likely is p=0.0442 when the null hypothesis is true?
    This is the question addressed in the paper
         Calibration of p Values for Testing Precise Null Hypotheses.
         T Sellke, MJ Bayarri, and JO Berger.
         The American Statistician v.55:62-71, 2001.
         http://www.stat.duke.edu/courses/Spring10/sta122/Labs/Lab6.pdf
    The exact interpretation of what the above question means is somewhat
    tricky, depending on if you are a Bayesian heretic or a Frequentist
    true believer.  But in either case, one reasonable answer is given by
    the function
         alpha(p) = 1 / [ 1 - 1/( e * p * log(p) ) ]
    (where 'e' is 2.71828... and 'log' is to the base 'e').  Here,
    alpha(p) can be interpreted as the likelihood that the given p-value
    was generated by the null hypothesis, versus being from the alternative
    hypothesis.  For p=0.0442, alpha=0.2726; in non-quantitative words, this
    p-value is NOT very strong evidence that the alternative hypothesis is true.
    
    Why is this so -- why isn't saying 'the null hypothesis would only give
    a result this big 4.42% of the time' similar to saying 'the alternative
    hypothesis is 95.58% likely to be true'?  The answer is because it is
    only somewhat more likely the t-statistic would be that value when the
    alternative hypothesis is true.  In this example, the difference in means
    cannot be very large, or the t-statistic would almost certainly be larger.
    But with a small difference in means (relative to the standard deviation),
    the alternative hypothesis (noncentral) t-value distribution isn't that
    different than the null hypothesis (central) t-value distribution.  It is
    true that the alternative hypothesis is more likely to be true than the
    null hypothesis (when p < 1/e = 0.36788), but it isn't AS much more likely
    to be true than the p-value itself seems to say.
    
    In short, a small p-value says that if the null hypothesis is true, the
    experimental results that you have aren't very likely -- but it does NOT
    say that the alternative hypothesis is vastly more likely to be correct,
    or that the data you have are vastly more likely to have come from the
    alternative hypothesis case.
    
    Some values of alpha(p) for those too lazy to calculate just now:
         p = 0.0005 alpha = 0.010225
         p = 0.001  alpha = 0.018431
         p = 0.005  alpha = 0.067174
         p = 0.010  alpha = 0.111254
         p = 0.015  alpha = 0.146204
         p = 0.020  alpha = 0.175380
         p = 0.030  alpha = 0.222367
         p = 0.040  alpha = 0.259255
         p = 0.050  alpha = 0.289350
    You can also try this AFNI package command to plot alpha(p) vs. p:
         1deval -dx 0.001 -xzero 0.001 -num 99 -expr '1/(1-1/(exp(1)*p*log(p)))' |
           1dplot -stdin -dx 0.001 -xzero 0.001 -xlabel 'p' -ylabel '\alpha(p)'
    Another example: to reduce the likelihood of the null hypothesis being the
    source of your t-statistic to 10%, you have to have p = 0.008593 -- a value
    more stringent than usually seen in scientific publications.  To get the null
    hypothesis likelihood below 5%, you have to get p below 0.003408.
    
    Finally, none of the discussion above is limited to the case of p-values that
    come from 2-sided t-tests.  The function alpha(p) applies (approximately) to
    many other situations.  However, it does NOT apply to 1-sided tests (which
    are not testing 'Precise Null Hypotheses').  See the paper by Sellke et al.
    for a lengthier and more precise discussion.  Another paper to peruse is
         Revised standards for statistical evidence.
         VE Johnson.  PNAS v110:19313-19317, 2013.
         http://www.pnas.org/content/110/48/19313.long
    For the case of 1-sided t-tests, the issue is more complex; the paper below
    may be of interest:
         Default Bayes Factors for Nonnested Hypthesis Testing.
         JO Berger and J Mortera.  J Am Stat Assoc v:94:542-554, 1999.
         http://www.jstor.org/stable/2670175 [PDF]
         http://ftp.isds.duke.edu/WorkingPapers/97-44.ps [PS preprint]
    What I have tried to do herein is outline the p-value interpretation issue
    using (mostly) non-technical words.
    
    ((***** What does this all mean for FMRI?  I'm still thinking about it. *****))
    
    --------------------
    TESTING THIS PROGRAM
    --------------------
    
    A simple 2-sample test of this program is given by the script below,
    which creates 2 datasets with standard deviation (sigma) of 1; the
    first one (ZZ_1) has mean 1 and the second one (ZZ_0) has mean 0;
    then the program tests these datasets to see if their means are different,
    and finally prints out the average value of the estimated differences
    in their means, and the average value of the associated t-statistic:
     3dUndump -dimen 128 128 32 -prefix ZZ
     3dcalc -a ZZ+orig -b '1D: 14@0' -expr 'gran(1,1)' -prefix ZZ_1.nii -datum float
     3dcalc -a ZZ+orig -b '1D: 10@0' -expr 'gran(0,1)' -prefix ZZ_0.nii -datum float
     3dttest++ -setA ZZ_1.nii -setB ZZ_0.nii -prefix ZZtest.nii -no1sam
     echo '=== mean of mean estimates follows, should be about 1 ==='
     3dBrickStat -mean ZZtest.nii'[0]'
     echo '=== mean of t-statistics follows, should be about 2.50149 ==='
     3dBrickStat -mean ZZtest.nii'[1]'
     \rm ZZ*
    The expected value of the t-statistic with 14 samples in setA and
    10 samples in setB is calculated below:
      delta_mean / sigma / sqrt( 1/NA + 1/NB ) / (1 - 3/(4*NA+4*NB-9) )
     =     1     / 1     / sqrt( 1/14 + 1/10 ) / (1 - 3/87            ) = 2.50149
    where division by (1-3/(4*NA+4*NB-9)) is the correction factor
    for the skewness of the non-central t-distribution --
    see http://en.wikipedia.org/wiki/Noncentral_t-distribution .
    
    -------------------------
    VARIOUS LINKS OF INTEREST
    -------------------------
    
    * http://en.wikipedia.org/wiki/T_test
    * http://www.statsoft.com/textbook/basic-statistics/
    * http://en.wikipedia.org/wiki/Mutatis_mutandis
    
    ---------------------------------------------------
    AUTHOR -- RW Cox -- don't whine TO me; wine WITH me (e.g., a nice Pinot Noir)
    ---------------------------------------------------
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
