#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
import option_list as OL
import afni_util as UTIL        # not actually used, but probably will be
import lib_subjects as SUBJ

# ----------------------------------------------------------------------
# globals

g_help_string = """
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

            gen_group_command.py -command 3dttest++        \\
                -dsets stats*+tlrc.HEAD

   or when the subject IDs are the varying part of the directory names (while
   the actual file names are identical), as in:

            gen_group_command.py -command 3dttest++        \\
                -dsets subject_results/*/*.results/stats+tlrc.HEAD


   Generic commands do not need to be part of AFNI.  Perhaps one just wants
   an orderly and indented list of file names to be part of a bigger script.
   consider:

        gen_group_command.py -command ls -dsets group_results/OL*D

   or perhaps using 3dTcat to collect a sub-brick from each subject:

        gen_group_command.py -command 3dTcat -subs_betas 'Arel#0_Coef' \\
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

            gen_group_command.py -command 3dttest++        \\
                                 -dsets REML*.HEAD

      2. Specify the sub-bricks and set labels to compare Vrel vs. Arel.
         Write the command to the file cmd.tt++.2.

            gen_group_command.py -command 3dttest++        \\
                                 -write_script cmd.tt++.2  \\
                                 -prefix tt++.2_V-A        \\
                                 -dsets REML*.HEAD         \\
                                 -set_labels Vrel Arel     \\
                                 -subs_betas 'Vrel#0_Coef' 'Arel#0_Coef'

      3. Request a paired t-test and apply a mask.

            gen_group_command.py -command 3dttest++                         \\
                                 -write_script cmd.tt++.3                   \\
                                 -prefix tt++.3_V-A_paired                  \\
                                 -dsets REML*.HEAD                          \\
                                 -set_labels Vrel Arel                      \\
                                 -subs_betas  'Vrel#0_Coef' 'Arel#0_Coef'   \\
                                 -options                                   \\
                                    -paired -mask mask+tlrc

      4. Exclude voxels that are identically zero across more than 20% of the
         input datasets (presumably masked at the single subject level).
         Convert output directly to z, since the DOF will vary across space.

            gen_group_command.py -command 3dttest++                         \\
                                 -write_script cmd.tt++.4                   \\
                                 -prefix tt++.4_V-A_zskip                   \\
                                 -dsets REML*.HEAD                          \\
                                 -set_labels Vrel Arel                      \\
                                 -subs_betas  'Vrel#0_Coef' 'Arel#0_Coef'   \\
                                 -options                                   \\
                                    -zskip 0.8 -toz

      5. Use covariates to account for a sex difference.  We might encode
         females as 0 and males as 1 to get an intercept (main effect) that
         applies to females (if we do not do any centering).  However, we
         want a main effect for the average between males and females, and
         therefore have used -1 for males and +1 for females.  Add NONE
         for centering so that 3dttest++ does not do any.

         Females have subject indices: 0, 1, 2, 3 and 5.
         Males   have subject indices: 4 and 6 through 9 (the last).

            gen_group_command.py -command 3dttest++             \\
                                 -write_script cmd.tt++.5       \\
                                 -prefix tt++.5_covary          \\
                                 -dsets data/OLSQ*.HEAD         \\
                                 -subs_betas 'Vrel#0_Coef'      \\
                                 -options                       \\
                                    -covariates sex_encode.txt  \\
                                    -center NONE


      6. Use -dset_index0_list to compare female subjects to males.
         Both subject types are in the same directory (10 subjects total).
         So the -dsets options will both specify the same list, which will
         then be paired down via -dset_index0_list to indicate only females
         and only males.

         Females have subject indices: 0, 1, 2, 3 and 5.
         Males   have subject indices: 4 and 6 through 9 (the last).

            gen_group_command.py -command 3dttest++             \\
                                 -write_script cmd.tt++.6       \\
                                 -prefix tt++.6_F-M             \\
                                 -dsets data/OLSQ*.HEAD         \\
                                 -dset_index0_list '0..3,5'     \\
                                 -dsets data/OLSQ*.HEAD         \\
                                 -dset_index0_list '4,6..$'     \\
                                 -set_labels female male        \\
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

            gen_group_command.py -command 3dMEMA           \\
                                 -dsets REML*.HEAD

      2. This does not quite apply to AFNI_data6.  Assuming there are 2 group
         directories, write a 2-sample command.

            gen_group_command.py -command 3dMEMA           \\
                                 -write_script cmd.mema.2  \\
                                 -dsets groupA/REML*.HEAD  \\
                                 -dsets groupB/REML*.HEAD

      3. Run 3dMEMA, but restrict the subjects to partial lists from within
         an entire list.  This applies -dset_index0_list (or the sister
         -dset_index1_list).

            # assume these 9 subjects represent all under the 'data' dir
            set subjects = ( AA BB CC DD EE FF GG HH II )

         a. Do a simple test on subjects AA, HH, II and FF.  Indices are:
               0-based: 0, 7, 8, 5 (AA=0, ..., II=8)
               1-based: 1, 8, 9, 6 (AA=1, ..., II=9)

            gen_group_command.py -command 3dMEMA              \\
                                 -write_script cmd.mema.3a    \\
                                 -dsets data/REML*.HEAD       \\
                                 -dset_index0_list '0,7,8,5'

         b. Do a test on sub-lists of subjects.

            gen_group_command.py -command 3dMEMA                            \\
                                 -write_script cmd.mema.3b                  \\
                                 -dsets data/REML*.HEAD                     \\
                                 -dset_index0_list '0,7,8,5'                \\
                                 -dsets data/REML*.HEAD                     \\
                                 -dset_index0_list '3,4,6,9'                \\
                                 -subs_betas  'Arel#0_Coef'                 \\
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

            gen_group_command.py -command 3dANOVA2         \\
                                 -dsets OLSQ*.HEAD         \\
                                 -subs_betas 0 1

      2. Get more useful:
            - apply with a directory
            - specify a script name
            - specify a dataset prefix for the 3dANOVA2 command
            - use labels for sub-brick indices
            - specify a simple contrast

            gen_group_command.py -command 3dANOVA2                           \\
                                 -write_script cmd.A2.2                      \\
                                 -prefix outset.A2.2                         \\
                                 -dsets AFNI_data6/group_results/REML*.HEAD  \\
                                 -subs_betas 'Vrel#0_Coef' 'Arel#0_Coef'     \\
                                 -options                                    \\
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

            gen_group_command.py -command 3dANOVA3                          \\
               -dsets OLSQ*.HEAD                                            \\
               -subs_betas                                                  \\
                 "pink_house#0_Coef" "pink_face#0_Coef" "pink_donut#0_Coef" \\
                 "blue_house#0_Coef" "blue_face#0_Coef" "blue_donut#0_Coef" \\
               -factors 2 3

      2. 3dANOVA3 -type 4

         Get more useful:
            - apply with an input data directory
            - specify a script name
            - specify a dataset prefix for the 3dANOVA3 command
            - specify simple contrasts

            gen_group_command.py -command 3dANOVA3                          \\
               -write_script cmd.A3.2                                       \\
               -prefix outset.A3.2                                          \\
               -dsets AFNI_data6/group_results/OLSQ*.HEAD                   \\
               -subs_betas                                                  \\
                 "pink_house#0_Coef" "pink_face#0_Coef" "pink_donut#0_Coef" \\
                 "blue_house#0_Coef" "blue_face#0_Coef" "blue_donut#0_Coef" \\
               -factors 2 3                                                 \\
               -options                                                     \\
                 -adiff 1 2 pink_vs_blue                                    \\
                 -bcontr -0.5 -0.5 1.0 donut_vs_house_face

      3. 3dANOVA3 -type 5

         Here is a simple case, providing just 2 groups of datasets and a list
         of sub-bricks.  

            gen_group_command.py -command 3dANOVA3         \\
                                 -dsets OLSQ*.HEAD         \\
                                 -dsets REML*.HEAD         \\
                                 -subs_betas 0 1

      4. 3dANOVA3 -type 5

         Get more useful:
            - apply with an input data directory
            - specify a script name
            - specify a dataset prefix for the 3dANOVA3 command
            - use labels for sub-brick indices
            - specify simple contrasts

            gen_group_command.py -command 3dANOVA3                           \\
                                 -write_script cmd.A3.4                      \\
                                 -prefix outset.A3.2                         \\
                                 -dsets AFNI_data6/group_results/OLSQ*.HEAD  \\
                                 -dsets AFNI_data6/group_results/REML*.HEAD  \\
                                 -subs_betas 'Vrel#0_Coef' 'Arel#0_Coef'     \\
                                 -options                                    \\
                                    -adiff 1 2 OvsR                          \\
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

        gen_group_command.py -command 3dTcat -subs_betas 'Arel#0_Coef' \\
                             -dsets group_results/OL*D

      3. including 2 sets of subjects, with a different sub-brick per set

        gen_group_command.py -command 3dTcat -subs_betas 0 1 \\
                             -dsets group_results/OLSQ*D     \\
                             -dsets group_results/REML*D

      4. 2 sets of subjects (in different directories, and with different
         sub-brick selectors), along with:

            - a script name (to write the script to a text file)
            - a -prefix
            - options for the command (just 1 in this case)
            - common sub-brick selectors for dataset lists

        gen_group_command.py -command 3dMean                    \\
                             -write_script cmd.3dmean.txt       \\
                             -prefix aud_vid_stdev              \\
                             -options -stdev                    \\
                             -subs_betas 'Arel#0_Coef'          \\
                             -dsets group_results/OLSQ*D        \\
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

             -dsets all/stats.*.HEAD            \\
             -dset_index0_list '0..5,10..15'    \\
             -dsets all/stats.*.HEAD            \\
             -dset_index0_list '6..9,16..$'     \\

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

           -subs_betas                                                   \\
              "pink_house#0_Coef" "pink_face#0_Coef" "pink_donut#0_Coef" \\
              "blue_house#0_Coef" "blue_face#0_Coef" "blue_donut#0_Coef" \\
           -factors 2 3                                                  \\

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
"""

g_history = """
   gen_group_command.py history:

   0.0  Sep 09, 2010 - initial version
   0.1  Oct 25, 2010 - handle some 3dMEMA cases
   0.2  Oct 26, 2010 - MEMA updates
   0.3  Nov 08, 2010 - can generate 3dttest++ commands
   0.4  Jun 15, 2011 - if constant dset names, extract SIDs from dir names
                          (done for R Momenan)
   0.5  Jun 27, 2011
        - added -dset_index0_list/-dset_index1_list options (for R Momenan)
        - ttest++ and MEMA commands now apply directories to datasets
        - changed Subject.atrs to be VarsObject instance, not dictionary
   0.6  Jun 22, 2012
        - added commands 3dANOVA2 and 3dANOVA3
        - added -factors for 3dANOVA3 -type 4
   0.7  Jun 25, 2012 - added help for -factors and 3dANOVA3 -type 4 examples
   0.8  Sep 04, 2012 - fixed error message
   0.9  Oct 03, 2012 - some options do not allow dashed parameters
   0.10 Oct 30, 2013 - added -keep_dirent_pre
   0.11 Apr 24, 2015 - tiny help update (example)
   0.12 Aug 12, 2015 - allow generic (unknown) commands (via -command)
   0.13 Mar 29, 2016 - 3dMEMA now requires paired test to be via input contrast
   1.0  Dec 27, 2017 - python3 compatible
"""

g_version = "gen_group_command.py version 1.00, December 27, 2017"


class CmdInterface:
   """interface class for getting commands from SubjectList class
   """

   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # general variables
      self.command         = ''         # program name to make command for
      self.ttype           = None       # test type (e.g. paired)
      self.comp_dir        = '-AminusB' # contrast direction (or -BminusA)
      self.prefix          = None       # prefix for command result
      self.write_script    = None       # file to write output to (else stdout)
      self.betasubs        = None       # list of beta weight sub-brick indices
      self.tstatsubs       = None       # list of t-stat sub-brick indices
      self.lablist         = None       # list of set labels
      self.factors         = []         # list of factors of each type

      self.subj_prefix     = ''         # prefix for each subject ID
      self.subj_suffix     = ''         # suffix for each subject ID
      self.dent_pre        = 0          # flag: keep dir entry prefix
      self.verb            = verb

      # lists
      self.options         = []         # other command options
      self.slist           = []         # list of SubjectList elements
      self.dsets           = []         # list of lists of filenames
      self.index0_list     = []         # 0-based sub-list of 'dsets'
      self.index1_list     = []         # 1-based sub-list of 'dsets'

      # initialize valid_opts
      self.init_options()

   def show(self):
      print("---------------------------- setup -----------------------------")
      print("command          : %s" % self.command)
      print("test type        : %s" % self.ttype)
      print("prefix           : %s" % self.prefix)
      print("write_script     : %s" % self.write_script)
      print("beta sub-bricks  : %s" % self.betasubs)
      print("tstat sub-bricks : %s" % self.tstatsubs)
      print("label list       : %s" % self.lablist)
      print("subject prefix   : %s" % self.subj_prefix)
      print("subject suffix   : %s" % self.subj_suffix)
      print("dirent prefix    : %s" % self.dent_pre)
      print("verb             : %s" % self.verb)

      print("options          : %s" % self.options)
      print("subject list(s)  : %s" % self.slist)
      print("index0_list      : %s" % self.index0_list)
      print("index1_list      : %s" % self.index1_list)
      print("datasets         : %s" % self.dsets)        # last

      if self.verb > 3:
         print("status           : %s" % self.status)
         self.user_opts.show(mesg="user options     : ")
      print("----------------------------------------------------------------")

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # terminal options
      self.valid_opts.add_opt('-help', 0, [],           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-ver', 0, [],            \
                      helpstr='display the current version number')

      # required parameters
      self.valid_opts.add_opt('-command', 1, [], 
                      helpstr='specify the program used in the output command')
      self.valid_opts.add_opt('-dsets', -1, [], okdash=0,
                      helpstr='specify a list of input datasets')

      # other options
      self.valid_opts.add_opt('-AminusB', 0, [], 
                      helpstr='apply 3dttest++ test as set A minus set B')
      self.valid_opts.add_opt('-BminusA', 0, [], 
                      helpstr='apply 3dttest++ test as set B minus set A')
      self.valid_opts.add_opt('-dset_index0_list', -1, [], okdash=0,
                      helpstr='restrict dsets to 0-based index list')
      self.valid_opts.add_opt('-dset_index1_list', -1, [], okdash=0,
                      helpstr='restrict dsets to 1-based index list')
      self.valid_opts.add_opt('-factors', -1, [], okdash=0,
                      helpstr='num factors, per condition (probably 2 ints)')
      self.valid_opts.add_opt('-keep_dirent_pre', 0, [], 
                      helpstr='keep directory entry prefix')
      self.valid_opts.add_opt('-options', -1, [], 
                      helpstr='specify options to pass to the command')
      self.valid_opts.add_opt('-prefix', 1, [], 
                      helpstr='specify output prefix for the command')
      self.valid_opts.add_opt('-set_labels', -1, [], okdash=0,
                      helpstr='list of labels for each set of subjects')
      self.valid_opts.add_opt('-subj_prefix', 1, [], 
                      helpstr='specify prefix for each subject ID')
      self.valid_opts.add_opt('-subj_suffix', 1, [], 
                      helpstr='specify suffix for each subject ID')
      self.valid_opts.add_opt('-subs_betas', -1, [], okdash=0,
                      helpstr='beta weight sub-bricks, one per subject list')
      self.valid_opts.add_opt('-subs_tstats', -1, [], okdash=0,
                      helpstr='t-stat sub-bricks, one per subject list')
      self.valid_opts.add_opt('-type', 1, [], 
                      helpstr='specify the test type (e.g. paired)')
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')
      self.valid_opts.add_opt('-write_script', 1, [], 
                      helpstr='specify file to write command into')

      return 0

   def process_options(self, argv=sys.argv):

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, apply -help
      if len(argv) <= 1 or '-help' in argv:
         print(g_help_string)
         return 0

      if '-hist' in argv:
         print(g_history)
         return 0

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 0

      if '-ver' in argv:
         print(g_version)
         return 0

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # require a list of files, at least

      # ------------------------------------------------------------
      # process options, go after -verb first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      for opt in uopts.olist:

         # main options
         if opt.name == '-AminusB':
            self.comp_dir = opt.name
            continue

         if opt.name == '-BminusA':
            self.comp_dir = opt.name
            continue

         if opt.name == '-command':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return 1
            self.command = val
            continue

         if opt.name == '-dsets':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.dsets.append(val)      # allow multiple such options
            continue

         if opt.name == '-dset_index0_list':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.index0_list.append(val)      # allow multiple such options
            continue

         if opt.name == '-dset_index1_list':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.index1_list.append(val)      # allow multiple such options
            continue

         if opt.name == '-factors':
            val, err = uopts.get_type_list(int, '', opt=opt)
            if val == None or err: return 1
            self.factors = val
            continue

         if opt.name == '-keep_dirent_pre':
            self.dent_pre = 1
            continue

         if opt.name == '-options':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.options = val
            continue

         if opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return 1
            self.prefix = val
            continue

         if opt.name == '-set_labels':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.lablist = val
            continue

         if opt.name == '-subj_prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return 1
            self.subj_prefix = val
            continue

         if opt.name == '-subj_suffix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return 1
            self.subj_suffix = val
            continue

         if opt.name == '-subs_betas':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.betasubs = val
            continue

         if opt.name == '-subs_tstats':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return 1
            self.tstatsubs = val
            continue

         if opt.name == '-type':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return 1
            self.ttype = val
            continue

         if opt.name == '-verb': continue       # already handled

         if opt.name == '-write_script':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err: return 1
            self.write_script = val
            continue

         # general options

         # an unhandled option
         print('** option %s not yet supported' % opt.name)
         return 1

      if self.verb > 2: self.show()

      # process -dset_index_list_0 and _1
      if self.update_dset_lists(): return 1

      return None

   def update_dset_lists(self):
      """process and -dset_index0_list or -dset_index1_list options
         (do not allow both)
      """

      oname0 = '-dset_index0_list'
      oname1 = '-dset_index1_list'

      uopts = self.user_opts

      # if no list selectors, there is nothing to do
      if len(self.index0_list) == 0 and len(self.index1_list) == 0: return 0

      # both option types is an error
      if len(self.index0_list) > 0 and len(self.index1_list) > 0:
         print('** cannot use both %s and %s' % (oname0, oname1))
         return 1

      otype = 0
      oname = oname0
      olist = self.index0_list
      nopt  = len(olist)

      if nopt == 0:
         otype = 1
         oname = oname1
         olist = self.index1_list
         nopt  = len(olist)

      # require one -dset_index option per -dsets option
      if nopt != len(self.dsets):
         print('** num -dset_indexX_list opts must match num -dsets opts' \
               ' (%d != %d)' % (nopt, len(self.dsets)))
         return 1

      new_dsets = []
      for dind, dlist in enumerate(self.dsets):
         status, newlist = UTIL.restrict_by_index_lists(dlist, olist[dind],
                                        otype, nonempty=1, verb=self.verb)
         if status:
            print('** bad use of %s' % oname)
            return 1
         new_dsets.append(newlist)

      self.dsets = new_dsets

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1:
         print('-- make %s command with %d set(s) of dsets of length(s): %s' \
               % (self.command, len(self.dsets), 
                  ', '.join([str(len(dlist)) for dlist in self.dsets]) ))

      # might deal with subject IDs and attributes later
      for ind, dlist in enumerate(self.dsets):
         slist = SUBJ.SubjectList(dset_l=dlist, verb=self.verb)
         if slist.status: return 1
         if slist.set_ids_from_dsets(prefix=self.subj_prefix,
                                     suffix=self.subj_suffix,
                                     dpre=self.dent_pre):
            print('** cannot set subject IDs from datasets')
            return 1
         self.slist.append(slist)
         if self.verb > 2: slist.show("slist %d" % ind)

      cmd = None
      if self.command == '3dMEMA':
         cmd = self.get_mema_command()
      elif self.command == '3dttest++':
         cmd = self.get_ttpp_command()
      elif self.command == '3dANOVA2':
         cmd = self.get_anova2_command()
      elif self.command == '3dANOVA3':
         cmd = self.get_anova3_command()
      elif self.command:
         cmd = self.get_generic_command()
      else:
         print('** command not implemented: %s' % self.command)

      # bail on failure, else wrap command
      if cmd == None:
         print('** failed making %s command' % self.command)
         return 1
      cmd = UTIL.add_line_wrappers(cmd)

      # either write to file or print
      if self.write_script:
         if UTIL.write_text_to_file(self.write_script, cmd):
            print("** failed to write command to file '%s'" % self.write_script)
            return 1
         if self.verb > 0:
            print('++ command written to file %s' % self.write_script)
      else: print(cmd)

   def get_mema_command(self):
      if len(self.slist) > 1: s2 = self.slist[1]
      else:                   s2 = None
      if (self.betasubs != None and self.tstatsubs == None) or \
         (self.betasubs == None and self.tstatsubs != None):
         print('** MEMA: -subs_betas and -subs_tstats must be used together')
         return None
      return self.slist[0].make_mema_command(set_labs=self.lablist,
                     bsubs=self.betasubs, tsubs=self.tstatsubs, subjlist2=s2,
                     prefix=self.prefix, ttype=self.ttype, options=self.options)

   def get_generic_command(self):
      if len(self.slist) > 1: s2 = self.slist[1]
      else:                   s2 = None
      return self.slist[0].make_generic_command(self.command,
                     bsubs=self.betasubs, subjlist2=s2, prefix=self.prefix,
                     options=self.options)

   def get_ttpp_command(self):
      if len(self.slist) > 1: s2 = self.slist[1]
      else:                   s2 = None
      return self.slist[0].make_ttestpp_command(set_labs=self.lablist,
                     bsubs=self.betasubs, subjlist2=s2, prefix=self.prefix,
                     comp_dir=self.comp_dir, options=self.options)

   def get_anova2_command(self):
      """generate 3dANOVA2 command
                type 2: requires one group and one list of betas
      """
      if self.betasubs == None:
         print('** missing required -subs_betas option for sub-brick list')
         return None

      return self.slist[0].make_anova2_command( bsubs=self.betasubs,
               prefix=self.prefix, options=self.options, verb=self.verb)

   def get_anova3_command(self):
      """generate 3dANOVA3 command
                type 5: requires 2 groups and one list of betas
      """

      return self.slist[0].make_anova3_command( bsubs=self.betasubs,
               prefix=self.prefix, subjlists=self.slist, options=self.options,
               factors=self.factors, verb=self.verb)

   def help_mema_command(self):
      helpstr = """
        3dMEMA command help:

           This is for help in deciding which MEMA command format to use, which
           command parameters are needed, and how a command would be organized.

           As with 3dttest, there are 3 basic ways to run 3dMEMA.

              1. as a one-sample test  (see example 1 from '3dMEMA -help')
              2. as a two-sample test  (see example 3 from '3dMEMA -help')
              3. as a paired test      (see example 2 from '3dMEMA -help')

           1. For the one-sample test, the required inputs are the datasets.
              It is best to also supply corresponding beta and t-stat sub-brick
              indexes or labels as well.

              minimum:

                 -dsets stats.*+tlrc.HEAD

              suggested:

                 -subs_betas 0 -subs_tstats 1
                    OR
                 -subs_betas  'Vrel#0_Coef' -subs_tstats 'Vrel#0_Tstat'

           2. 
      """

   def help_datasets(self):
      helpstr = """
           Dataset configuration and naming:
              When using this command generator, each subject should have all
              data (betas and t-stats) in a single dataset.  Dataset names
              should preferably be consistent, varying over only subject ID
              codes (this is not a requirement, but makes life easier).

              For example, this allows one to specify datasets at once (via a
              wildcard) and let the program sort it out.  Consider using:

                -dsets stats.*+tlrc.HEAD

              This will expand alphabetically, with subject IDs coming from
              the part of the dataset names that varies.

      """

   def ready_for_action(self):

      ready = 1

      if self.command == '':
         if self.verb > 0: print('** missing execution command')
         ready = 0

      if len(self.dsets) < 1:
         if self.verb > 0: print('** missing datasets for command')
         ready = 0

      return ready

   def init_from_file(self, fname):
      """load a 1D file, and init the main class elements"""

      self.status = 1 # init to failure
      adata = L1D.Afni1D(fname, name='1d_tool data', verb=self.verb)
      if not adata.ready:
         print("** failed to read 1D data from '%s'" % fname)
         return 1

      if self.verb > 1: print("++ read 1D data from file '%s'" % fname)

      self.ad = adata
      self.status = 0

      return 0

   def test(self, verb=3):
      print('------------------------ initial tests -----------------------')
      self.verb = verb
      # first try AFNI_data4, then regression data

      print('------------------------ reset files -----------------------')

      print('------------------------ should fail -----------------------')

      print('------------------------ more tests ------------------------')

      return None

def main():
   me = CmdInterface()
   if not me: return 1

   rv = me.process_options()
   if rv != None: return rv

   rv = me.execute()
   if rv != None: return rv

   return me.status

if __name__ == '__main__':
   sys.exit(main())


