********************
gen_group_command.py
********************

.. _gen_group_command.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    =============================================================================
    gen_group_command.py    - generate group commands: 3dttest++, 3dMEMA,
                              3dANOVA2, 3dANOVA3
                            - generate generic commands
                            - todo (maybe): 3dttest, GroupAna
    
       This program is to assist in writing group commands.  The hardest part (or
       most tedious) is generally listing datasets and such, particularly including
       sub-brick selection, and that is the main benefit of using this program.
    
       If used without sufficient options (which might be typical), the generated
       commands will not be complete (e.g. they might fail).  So either provide
       sufficient passed options via -options or plan to edit the resulting script.
    
       If -write_script is not given, the command is written to stdout.
    
       ** NOTE: this program expects one dataset per subject.  Single condition
                volumes are accessed using sub-brick selectors via -subs_betas 
                and possbily -subs_tstats.
    
       This program can parse subject IDs from dataset names when the IDs are the
       varying part of dataset names (e.g. stats_subj1234+tlrc.HEAD), as in:
    
                gen_group_command.py -command 3dttest++        \
                    -dsets stats*+tlrc.HEAD
    
       or when the subject IDs are the varying part of the directory names (while
       the actual file names are identical), as in:
    
                gen_group_command.py -command 3dttest++        \
                    -dsets subject_results/*/*.results/stats+tlrc.HEAD
    
    
       Generic commands do not need to be part of AFNI.  Perhaps one just wants
       an orderly and indented list of file names to be part of a bigger script.
       consider:
    
            gen_group_command.py -command ls -dsets group_results/OL*D
    
       or perhaps using 3dTcat to collect a sub-brick from each subject:
    
            gen_group_command.py -command 3dTcat -subs_betas 'Arel#0_Coef' \
                                 -dsets group_results/OL*D
    
    ------------------------------------------
    examples (by program)
    
       A. 3dttest++ (not 3dttest)
    
          Note: these commands apply to the sample group data under
                AFNI_data6/group_results.
    
        * Note: The 3dttest++ program defaults to setA minus setB, which is the
                opposite of 3dttest and 3dMEMA (though it might be more natural).
                The direction of the test can be further specified using either
                -AminusB or -BminusA, which is always included in the resulting
                command if there are 2 sets of data.
    
                This program will always supply one of -AminusB or -BminusA, to be
                clear.  If the user does not provide one, -AminusB will be used.
    
                Note also that 3dttest uses sub-brick labels which should make
                this clear.
    
          1. The most simple case, providing just the datasets.  The subject IDs
             will be extracted from the dataset names.  Since no sub-bricks are
             provided, the betas will default to sub-brick 0 and the test will be
             the mean compared with 0.
    
                gen_group_command.py -command 3dttest++        \
                                     -dsets REML*.HEAD
    
          2. Specify the sub-bricks and set labels to compare Vrel vs. Arel.
             Write the command to the file cmd.tt++.2.
    
                gen_group_command.py -command 3dttest++        \
                                     -write_script cmd.tt++.2  \
                                     -prefix tt++.2_V-A        \
                                     -dsets REML*.HEAD         \
                                     -set_labels Vrel Arel     \
                                     -subs_betas 'Vrel#0_Coef' 'Arel#0_Coef'
    
          3. Request a paired t-test and apply a mask.
    
                gen_group_command.py -command 3dttest++                         \
                                     -write_script cmd.tt++.3                   \
                                     -prefix tt++.3_V-A_paired                  \
                                     -dsets REML*.HEAD                          \
                                     -set_labels Vrel Arel                      \
                                     -subs_betas  'Vrel#0_Coef' 'Arel#0_Coef'   \
                                     -options                                   \
                                        -paired -mask mask+tlrc
    
          4. Exclude voxels that are identically zero across more than 20% of the
             input datasets (presumably masked at the single subject level).
             Convert output directly to z, since the DOF will vary across space.
    
                gen_group_command.py -command 3dttest++                         \
                                     -write_script cmd.tt++.4                   \
                                     -prefix tt++.4_V-A_zskip                   \
                                     -dsets REML*.HEAD                          \
                                     -set_labels Vrel Arel                      \
                                     -subs_betas  'Vrel#0_Coef' 'Arel#0_Coef'   \
                                     -options                                   \
                                        -zskip 0.8 -toz
    
          5. Use covariates to account for a sex difference.  We might encode
             females as 0 and males as 1 to get an intercept (main effect) that
             applies to females (if we do not do any centering).  However, we
             want a main effect for the average between males and females, and
             therefore have used -1 for males and +1 for females.  Add NONE
             for centering so that 3dttest++ does not do any.
    
             Females have subject indices: 0, 1, 2, 3 and 5.
             Males   have subject indices: 4 and 6 through 9 (the last).
    
                gen_group_command.py -command 3dttest++             \
                                     -write_script cmd.tt++.5       \
                                     -prefix tt++.5_covary          \
                                     -dsets data/OLSQ*.HEAD         \
                                     -subs_betas 'Vrel#0_Coef'      \
                                     -options                       \
                                        -covariates sex_encode.txt  \
                                        -center NONE
    
    
          6. Use -dset_index0_list to compare female subjects to males.
             Both subject types are in the same directory (10 subjects total).
             So the -dsets options will both specify the same list, which will
             then be paired down via -dset_index0_list to indicate only females
             and only males.
    
             Females have subject indices: 0, 1, 2, 3 and 5.
             Males   have subject indices: 4 and 6 through 9 (the last).
    
                gen_group_command.py -command 3dttest++             \
                                     -write_script cmd.tt++.6       \
                                     -prefix tt++.6_F-M             \
                                     -dsets data/OLSQ*.HEAD         \
                                     -dset_index0_list '0..3,5'     \
                                     -dsets data/OLSQ*.HEAD         \
                                     -dset_index0_list '4,6..$'     \
                                     -set_labels female male        \
                                     -subs_betas 'Vrel#0_Coef'
    
    
       See "3dttest++ -help" for details on its options.
    
       --------------------
    
       B. 3dMEMA
    
          Note: these commands apply to the sample group data under
                AFNI_data6/group_results.
    
          Note: As with 3dttest, group comparisons are done as the second set minus
                the first set.
    
    
          1. The most simple case, providing just the datasets.  The subject IDs
             will be extracted from the dataset names.  Since no sub-bricks are
             provided, the betas will be 0 and t-stats will be 1.
    
                gen_group_command.py -command 3dMEMA           \
                                     -dsets REML*.HEAD
    
          2. This does not quite apply to AFNI_data6.  Assuming there are 2 group
             directories, write a 2-sample command.
    
                gen_group_command.py -command 3dMEMA           \
                                     -write_script cmd.mema.2  \
                                     -dsets groupA/REML*.HEAD  \
                                     -dsets groupB/REML*.HEAD
    
          3. Run 3dMEMA, but restrict the subjects to partial lists from within
             an entire list.  This applies -dset_index0_list (or the sister
             -dset_index1_list).
    
                # assume these 9 subjects represent all under the 'data' dir
                set subjects = ( AA BB CC DD EE FF GG HH II )
    
             a. Do a simple test on subjects AA, HH, II and FF.  Indices are:
                   0-based: 0, 7, 8, 5 (AA=0, ..., II=8)
                   1-based: 1, 8, 9, 6 (AA=1, ..., II=9)
    
                gen_group_command.py -command 3dMEMA              \
                                     -write_script cmd.mema.3a    \
                                     -dsets data/REML*.HEAD       \
                                     -dset_index0_list '0,7,8,5'
    
             b. Do a test on sub-lists of subjects.
    
                gen_group_command.py -command 3dMEMA                            \
                                     -write_script cmd.mema.3b                  \
                                     -dsets data/REML*.HEAD                     \
                                     -dset_index0_list '0,7,8,5'                \
                                     -dsets data/REML*.HEAD                     \
                                     -dset_index0_list '3,4,6,9'                \
                                     -subs_betas  'Arel#0_Coef'                 \
                                     -subs_tstats 'Arel#0_Tstat'
    
             See "3dMEMA -help" for details on the extra options.
    
       --------------------
    
       C. 3dANOVA2
    
          Note: these commands apply to the sample group data under
                AFNI_data6/group_results.
    
          Note: it seems better to create the script without any contrasts, and
                add them afterwards (so the user can format well).  However, if
                no contrasts are given, the program will add 1 trivial one.
    
    
          1. The most simple case, providing just the datasets and a list of
             sub-bricks.  
    
                gen_group_command.py -command 3dANOVA2         \
                                     -dsets OLSQ*.HEAD         \
                                     -subs_betas 0 1
    
          2. Get more useful:
                - apply with a directory
                - specify a script name
                - specify a dataset prefix for the 3dANOVA2 command
                - use labels for sub-brick indices
                - specify a simple contrast
    
                gen_group_command.py -command 3dANOVA2                           \
                                     -write_script cmd.A2.2                      \
                                     -prefix outset.A2.2                         \
                                     -dsets AFNI_data6/group_results/REML*.HEAD  \
                                     -subs_betas 'Vrel#0_Coef' 'Arel#0_Coef'     \
                                     -options                                    \
                                        -adiff 1 2 VvsA
    
       --------------------
    
       D. 3dANOVA3
    
          Note: these commands apply to the sample group data under
                AFNI_data6/group_results.
    
          Note: it seems better to create the script without any contrasts, and
                add them afterwards (so the user can format well).  However, if
                no contrasts are given, the program will add 2 trivial ones,
                just for a starting point.
    
          Note: this applies either -type 4 or -type 5 from 3dANOVA3.
                See "3dANOVA3 -help" for details on the types.
    
                The user does not specify type 4 or 5.
    
                type 4: there should be one -dsets option and a -factors option
                type 5: there should be two -dsets options and no -factor
    
          1. 3dANOVA3 -type 4
    
             This is a simple example of a 2-way factorial ANOVA (color by image
             type), across many subjects.  The colors are pink and blue, while the
             images are of houses, faces and donuts.  So there are 6 stimulus types
             in this 2 x 3 design:
    
                    pink house      pink face       pink donut
                    blue house      blue face       blue donut
    
             Since those were the labels given to 3dDeconvolve, the beta weights
             will have #0_Coef appended, as in pink_house#0_Coef.  Note that in a
             script, the '#' character will need to be quoted.
    
             There is only one set of -dsets given, as there are no groups.
    
                gen_group_command.py -command 3dANOVA3                          \
                   -dsets OLSQ*.HEAD                                            \
                   -subs_betas                                                  \
                     "pink_house#0_Coef" "pink_face#0_Coef" "pink_donut#0_Coef" \
                     "blue_house#0_Coef" "blue_face#0_Coef" "blue_donut#0_Coef" \
                   -factors 2 3
    
          2. 3dANOVA3 -type 4
    
             Get more useful:
                - apply with an input data directory
                - specify a script name
                - specify a dataset prefix for the 3dANOVA3 command
                - specify simple contrasts
    
                gen_group_command.py -command 3dANOVA3                          \
                   -write_script cmd.A3.2                                       \
                   -prefix outset.A3.2                                          \
                   -dsets AFNI_data6/group_results/OLSQ*.HEAD                   \
                   -subs_betas                                                  \
                     "pink_house#0_Coef" "pink_face#0_Coef" "pink_donut#0_Coef" \
                     "blue_house#0_Coef" "blue_face#0_Coef" "blue_donut#0_Coef" \
                   -factors 2 3                                                 \
                   -options                                                     \
                     -adiff 1 2 pink_vs_blue                                    \
                     -bcontr -0.5 -0.5 1.0 donut_vs_house_face
    
          3. 3dANOVA3 -type 5
    
             Here is a simple case, providing just 2 groups of datasets and a list
             of sub-bricks.  
    
                gen_group_command.py -command 3dANOVA3         \
                                     -dsets OLSQ*.HEAD         \
                                     -dsets REML*.HEAD         \
                                     -subs_betas 0 1
    
          4. 3dANOVA3 -type 5
    
             Get more useful:
                - apply with an input data directory
                - specify a script name
                - specify a dataset prefix for the 3dANOVA3 command
                - use labels for sub-brick indices
                - specify simple contrasts
    
                gen_group_command.py -command 3dANOVA3                           \
                                     -write_script cmd.A3.4                      \
                                     -prefix outset.A3.2                         \
                                     -dsets AFNI_data6/group_results/OLSQ*.HEAD  \
                                     -dsets AFNI_data6/group_results/REML*.HEAD  \
                                     -subs_betas 'Vrel#0_Coef' 'Arel#0_Coef'     \
                                     -options                                    \
                                        -adiff 1 2 OvsR                          \
                                        -bdiff 1 2 VvsA
    
       --------------------
    
       E. generic/other programs
    
          These commands apply to basically any program, as specified.  Options
          may be provided, along with 1 or 2 sets of data.  If provided, the
          -subs_betas selectors will be applied.
    
          This might be useful for simply making part of a longer script, where
          the dataset names are explicit.
    
    
          1. perhaps a fairly useless example with 'ls', just for demonstration
    
            gen_group_command.py -command ls -dsets group_results/OL*D
    
          2. using 3dTcat to collect a sub-brick from each subject
    
            gen_group_command.py -command 3dTcat -subs_betas 'Arel#0_Coef' \
                                 -dsets group_results/OL*D
    
          3. including 2 sets of subjects, with a different sub-brick per set
    
            gen_group_command.py -command 3dTcat -subs_betas 0 1 \
                                 -dsets group_results/OLSQ*D     \
                                 -dsets group_results/REML*D
    
          4. 2 sets of subjects (in different directories, and with different
             sub-brick selectors), along with:
    
                - a script name (to write the script to a text file)
                - a -prefix
                - options for the command (just 1 in this case)
                - common sub-brick selectors for dataset lists
    
            gen_group_command.py -command 3dMean                    \
                                 -write_script cmd.3dmean.txt       \
                                 -prefix aud_vid_stdev              \
                                 -options -stdev                    \
                                 -subs_betas 'Arel#0_Coef'          \
                                 -dsets group_results/OLSQ*D        \
                                 -dsets group_results/REML*D
    
    ------------------------------------------
    terminal options:
    
       -help                     : show this help
       -hist                     : show module history
       -show_valid_opts          : list valid options
       -ver                      : show current version
    
    required parameters:
    
       -command COMMAND_NAME     : resulting command, such as 3dttest++
    
            The current list of group commands is: 3dttest++, 3dMEMA, 3dANOVA2,
            3dANOVA3.
    
               3dANOVA2:    applied as -type 3 only (factor x subjects)
               3dANOVA3:    -type 4: condition x condition x subject
                                     (see -factors option)
                            -type 5: group x condition x subject
    
       -dsets   datasets ...     : list of datasets
    
            Each use of this option essentially describes one group of subjects.
            All volumes for a given subject should be in a single dataset.
    
            This option can be used multiple times, once per group.
    
    other options:
    
       -dset_index0_list values...  : restrict -dsets datasets to this 0-based list
       -dset_index1_list values...  : restrict -dsets datasets to this 1-based list
    
            In some cases it is easy to use a wildcard to specify datasets via
            -dsets, but there may be a grouping of subjects within that list.
            For example, if both males and females are in the list of datasets
            provided by -dsets, and if one wants a comparison between those 2
            groups, then a pair of -dset_index0_list could be specified (1 for
            each -dset) option to list which are the females and males.
    
            Consider this example:
    
                 -dsets all/stats.*.HEAD            \
                 -dset_index0_list '0..5,10..15'    \
                 -dsets all/stats.*.HEAD            \
                 -dset_index0_list '6..9,16..$'     \
    
            Note that -dsets is used twice, with IDENTICAL lists of datasets.
            The respective -dset_index0_list options then restrict those lists to
            0-based index lists, one for females, the other for males.
    
          * One must be careful to get the indices correct, so check the output
            command script to be sure the correct subjects are in each group.
    
            The difference between -dset_index0_list and -dset_index1_list is just
            that the former is a 0-based list (such as is used by AFNI programs),
            while the latter is 1-based (such as is used by tcsh).  A 0-based list
            begins counting at 0 (as in offsets), while a list 1-based starts at 1.
            Since use of either makes sense, both are provided.
    
            For example, these options are equivalent:
    
                    -dset_index0_list 0,5..8
                    -dset_index1_list 1,6..9
    
            The format for these index lists is the same as for AFNI sub-brick
            selection.
    
       -factors NF1 NF2 ...         : list of factor levels, per condition
    
               example: -factors 2 3
    
            This option is currently only for '3dANOVA3 -type 4', which is a
            condition x condition x subject test.  It is meant to parse the
            -subs_betas option, which lists all sub-bricks input to the ANOVA.
            
            Assuming condition A has nA levels, and B has nB (2 and 3 in the
            above example), then this option (applied '-factors nA nB', and
            -subs_betas) would take nA * nB parameters (for the cross product of
            factor A and factor B levels).
            The betas should be specified in A major order, as in:
    
               -subs_betas A1B1_name A1B2_name ... A1BnB A2B1 A2B2 ... AnABnB_name
    
            or as in the 2 x 3 case:
    
               -subs_betas A1B1 A1B2 A1B3 A2B1 A2B2 A2B3   -factors 2 3
    
            e.g. for pink/blue x house/face/donut, output be 3dDeconvolve
                 (i.e. each betas probably has #0_Coef attached)
    
               -subs_betas                                                   \
                  "pink_house#0_Coef" "pink_face#0_Coef" "pink_donut#0_Coef" \
                  "blue_house#0_Coef" "blue_face#0_Coef" "blue_donut#0_Coef" \
               -factors 2 3                                                  \
    
            Again, these factor combination names should be either sub-brick labels
            or indices (labels are suggested, to avoid confusion).
    
            See the example with '3dANOVA3 -type 4' as part of example D, above.
            See also -subs_betas.
    
       -keep_dirent_pre             : keep directory entry prefix
    
            Akin to -subj_prefix, this flag expands the subject prefix list to
            include everything up to the beginning of the directory names (at
            the level that varies across input datasets).
    
            Example 1:
               datasets:
                  subj.FP/betas+tlrc   subj.FR/betas+tlrc   subj.FT/betas+tlrc
                  subj.FV/betas+tlrc   subj.FW/betas+tlrc   subj.FX/betas+tlrc
                  subj.FY/betas+tlrc   subj.FZ/betas+tlrc
    
               The default subject IDs would be:
                  P R T V W X Y Z
    
               When using -keep_dirent_pre, subject IDs would be:
                  subj.FP subj.FR subj.FT subj.FV subj.FW subj.FX subj.FY subj.FZ
    
               Note that these IDs come at the directory level, since the dataset
               names do not vary.
    
            Example 2:
               datasets:
                  subj.FP/OLSQ.FP.betas+tlrc   subj.FR/OLSQ.FR.betas+tlrc
                  subj.FT/OLSQ.FT.betas+tlrc   subj.FV/OLSQ.FV.betas+tlrc
                  subj.FW/OLSQ.FW.betas+tlrc   subj.FX/OLSQ.FX.betas+tlrc
                  subj.FY/OLSQ.FY.betas+tlrc   subj.FZ/OLSQ.FZ.betas+tlrc
    
               The default subject IDs would be:
                  P R T V W X Y Z
    
               When using -keep_dirent_pre, subject IDs would be:
                  OLSQ.FP OLSQ.FR OLSQ.FT OLSQ.FV OLSQ.FW OLSQ.FX OLSQ.FY OLSQ.FZ
    
               Note that these IDs come at the dataset level, since the dataset
               names vary.
    
       -options OPT1 OPT2 ...       : list of options to pass along to result
    
            The given options will be passed directly to the resulting command.  If
            the -command is 3dMEMA, say, these should be 3dMEMA options.  This
            program will not evaluate or inspect the options, but will put them at
            the end of the command.
    
       -prefix PREFIX               : apply as COMMAND -prefix
       -set_labels LAB1 LAB2 ...    : labels corresponding to -dsets entries
       -subj_prefix PREFIX          : prefix for subject names (3dMEMA)
       -subj_suffix SUFFIX          : suffix for subject names (3dMEMA)
       -subs_betas B0 B1            : sub-bricks for beta weights (or similar)
    
            If this option is not given, sub-brick 0 will be used.  The entries
            can be either numbers or labels (which should match what is seen in
            the afni GUI, for example).
    
            If there are 2 -set_labels, there should be 2 betas (or no option).
    
       -subs_tstats T0 T1           : sub-bricks for t-stats (3dMEMA)
    
            If this option is not given, sub-brick 1 will be used.  The entries can
            be either numbers or labels (which should match what is seen in the
            afni GUI, for example).
    
            This option applies only to 3dMEMA currently, and in that case, its use
            should match that of -subs_betas.
    
            See also -subs_betas.
    
       -type TEST_TYPE              : specify the type of test to perform
    
            The test type may depend on the given command, but generally implies
            there are multiple sets of values to compare.  Currently valid tests
            are (for the given program):
           
              3dMEMA: paired, unpaired
    
            If this option is not applied, a useful default will be chosen.
    
       -verb LEVEL                  : set the verbosity level
    
       -write_script FILE_NAME      : write command script to FILE_NAME
    
            If this option is given, the command will be written to the specified
            file name.  Otherwise, it will be written to the terminal window.
           
    -----------------------------------------------------------------------------
    R Reynolds    October 2010
    =============================================================================
