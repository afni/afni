#!/usr/bin/env python

# note: in the script, runs are 1-based (probably expected)

import string, sys, os
from time import asctime

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *
from db_mod import *
import ask_me

# ----------------------------------------------------------------------
# globals

g_help_string = """
    ===========================================================================
    afni_proc.py        - generate a tcsh script for an AFNI process stream

    This python script can generate a processing script via a command-line
    interface, with an optional question/answer session (-ask_me), or by a tk
    GUI (eventually).

    The user should provide at least the input datasets (-dsets) and stimulus
    files (-regress_stim_*), in order to create an output script.  See the
    'DEFAULTS' section for a description of the default options for each block.

    The output script, when executed will create a results directory, copy
    input files into it, and perform all processing there.  So the user can
    delete the results directory and re-run the script at their whim.

    Note that the user need not actually run the output script.  The user
    should feel free to modify the script for their own evil purposes, before
    running it.

    The text interface can be accessed via the -ask_me option.  It invokes a
    question & answer session, during which this program sets user options on
    the fly.  The user may elect to enter some of the options on the command
    line, even if using -ask_me.  See "-ask_me EXAMPLES", below.

    --------------------------------------------------
    TIMING FILE NOTE:

    One issue that the user must be sure of is the timing of the stimulus
    files (whether -regress_stim_files or -regress_stim_times is used).

    The 'tcat' step will remove the number of pre-steady-state TRs that the
    user specifies (defaulting to 0).  The stimulus files, provided by the
    user, must match datasets that have had such TRs removed (i.e. the stim
    files should start _after_ steady state has been reached).

    --------------------------------------------------
    NOTE on having runs of different lengths:

    In the case that the EPI datasets are not all of the same length, here
    are some issues that may come up, listed by relevant option:

        -volreg_align_to        If aligning to "last" afni_proc.py might get
                                an inaccurate index for the volreg -base.

        -regress_polort         If this option is not used, then the degree of
                                polynomial used for the baseline will come from
                                the first run.

        -regress_est_blur_epits This may fail, as afni_proc.py may have trouble
                                teasing the different runs apart from the errts
                                dataset.

        -regress_use_stim_files This may fail, as make_stim_times.py is not
                                currently prepared to handle runs of different
                                lengths.
    --------------------------------------------------
    PROCESSING STEPS (of the output script):

    The output script will go through the following steps, unless the user
    specifies otherwise.

    automatic steps (the tcsh script will always perform these):

        setup       : check subject arg, set run list, create output dir, and
                      copy stim files
        tcat        : copy input datasets and remove unwanted initial TRs

    default steps (the user may skip these, or alter their order):

        tshift      : slice timing alignment on volumes (default is -time 0)
        volreg      : volume registration (default to third volume)
        blur        : blur each volume (default is 4mm fwhm)
        mask        : create a 'brain' mask from the EPI data (dilate 1 voxel)
        scale       : scale each run mean to 100, for each voxel (max of 200)
        regress     : regression analysis (default is GAM, peak 1, with motion
                      params)

    optional steps (the default is _not_ to apply these blocks)

        despike     : truncate spikes in each voxel's time series
        empty       : placehold for some user command (using 3dTcat as sample)

    --------------------------------------------------
    EXAMPLES (options can be provided in any order):

        1. Minimum use, provide datasets and stim files (or stim_times files).
           Note that a dataset suffix (e.g. HEAD) must be used with wildcards,
           so that datasets are not applied twice.  In this case, a stim_file
           with many columns is given, allowing the script to change it to
           stim_times files.

                afni_proc.py -dsets epiRT*.HEAD              \\
                             -regress_stim_files stims.1D

           or without any wildcard, the .HEAD suffix is not needed:

                afni_proc.py -dsets epiRT_r1+orig epiRT_r2+orig epiRT_r3+orig \\
                             -regress_stim_files stims.1D

     ** The following examples can be run from the AFNI_data2 directory, and
        are examples of how one might process the data for subject ED.

        Because the stimuli are on a 1-second grid, while the EPI data is on a
        2-second grid (TR = 2.0), we ran make_stim_times.py to generate the
        stim_times files (which are now distributed in AFNI_data2) as follows:

            make_stim_times.py -prefix stim_times -tr 1.0 -nruns 10 -nt 272 \\
                   -files misc_files/all_stims.1D

        If your AFNI_data2 directory does not have misc_files/stim_times.*,
        then you can run the make_stim_times.py command from AFNI_data2.


        2. This example shows basic usage, with the default GAM regressor.
           We specify the output script name, the subject ID, removal of the
           first 2 TRs of each run (before steady state), and volume alignment
           to the end of the runs (the anat was acquired after the EPI).

           The script name will default to proc.ED, based on -subj_id.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD      \\
                             -subj_id ED                     \\
                             -tcat_remove_first_trs 2        \\
                             -volreg_align_to first          \\
                             -regress_stim_times misc_files/stim_times.*.1D

        3. Similar to #2, but add labels for the 4 stim types, and apply TENT
           as the basis function to get 14 seconds of response, on a 2-second
           TR grid.  Also, copy the anat dataset(s) to the results directory,
           and align volumes to the third TR, instead of the first.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD                      \\
                             -subj_id ED.8                                   \\
                             -copy_anat ED/EDspgr                            \\
                             -tcat_remove_first_trs 2                        \\
                             -volreg_align_to third                          \\
                             -regress_stim_times misc_files/stim_times.*.1D  \\
                             -regress_stim_labels ToolMovie HumanMovie       \\
                                                  ToolPoint HumanPoint       \\
                             -regress_basis 'TENT(0,14,8)'

        4. This is the current AFNI_data2 class example.

           Similar to #3, but append a single -regress_opts_3dD option to
           include contrasts.  The intention is to create a script very much
           like analyze_ht05.  Note that the contrast files have been renamed
           from contrast*.1D to glt*.txt, though the contents have not changed.

           afni_proc.py -dsets ED/ED_r??+orig.HEAD                         \\
                  -subj_id ED.8.glt                                        \\
                  -copy_anat ED/EDspgr                                     \\
                  -tcat_remove_first_trs 2                                 \\
                  -volreg_align_to third                                   \\
                  -regress_stim_times misc_files/stim_times.*.1D           \\
                  -regress_stim_labels ToolMovie HumanMovie                \\
                                       ToolPoint HumanPoint                \\
                  -regress_basis 'TENT(0,14,8)'                            \\
                  -regress_opts_3dD                                        \\
                      -gltsym ../misc_files/glt1.txt -glt_label 1 FullF    \\
                      -gltsym ../misc_files/glt2.txt -glt_label 2 HvsT     \\
                      -gltsym ../misc_files/glt3.txt -glt_label 3 MvsP     \\
                      -gltsym ../misc_files/glt4.txt -glt_label 4 HMvsHP   \\
                      -gltsym ../misc_files/glt5.txt -glt_label 5 TMvsTP   \\
                      -gltsym ../misc_files/glt6.txt -glt_label 6 HPvsTP   \\
                      -gltsym ../misc_files/glt7.txt -glt_label 7 HMvsTM

        5. Similar to #4, but replace some glt files with SYM, and request
           to run @auto_tlrc.

           Also, compute estimates of the smoothness in both the EPI (all_runs)
           and errts (via -regress_est_blur_*).

           afni_proc.py -dsets ED/ED_r??+orig.HEAD                           \\
              -subj_id ED.8.gltsym                                           \\
              -copy_anat ED/EDspgr                                           \\
              -tlrc_anat                                                     \\
              -tcat_remove_first_trs 2                                       \\
              -volreg_align_to third                                         \\
              -regress_stim_times misc_files/stim_times.*.1D                 \\
              -regress_stim_labels ToolMovie HumanMovie                      \\
                                   ToolPoint HumanPoint                      \\
              -regress_basis 'TENT(0,14,8)'                                  \\
              -regress_opts_3dD                                              \\
                -gltsym 'SYM: -ToolMovie +HumanMovie -ToolPoint +HumanPoint' \\
                -glt_label 1 HvsT                                            \\
                -gltsym 'SYM: +HumanMovie -HumanPoint'                       \\
                -glt_label 2 HMvsHP                                          \\
              -regress_est_blur_epits                                        \\
              -regress_est_blur_errts

        6. Similar to #3, but find the response for the TENT functions on a
           1-second grid, such as how the data is processed in the class
           script, s1.analyze_ht05.  This is similar to using '-stim_nptr 2',
           and requires the addition of 3dDeconvolve option '-TR_times 1.0' to  
           see the -iresp output on a 1.0 second grid.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD                      \\
                             -subj_id ED.15                                  \\
                             -copy_anat ED/EDspgr                            \\
                             -tcat_remove_first_trs 2                        \\
                             -volreg_align_to third                          \\
                             -regress_stim_times misc_files/stim_times.*.1D  \\
                             -regress_stim_labels ToolMovie HumanMovie       \\
                                                  ToolPoint HumanPoint       \\
                             -regress_basis 'TENT(0,14,15)'                  \\
                             -regress_opts_3dD -TR_times 1.0

        7. Similar to #2, but add the despike block, and skip the tshift and
           mask blocks (so the others must be specified).  The user wants to
           apply a block that afni_proc.py does not deal with, putting it after
           the 'despike' block.  So 'empty' is given after 'despike'.

           Also, apply a 4 second BLOCK response function, prevent the output
           of a fit time series dataset, run @auto_tlrc at the end, and specify
           an output script name.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD                   \\
                         -blocks despike empty volreg blur scale regress  \\
                         -script process_ED.b4                            \\
                         -subj_id ED.b4                                   \\
                         -copy_anat ED/EDspgr                             \\
                         -tlrc_anat                                       \\
                         -tcat_remove_first_trs 2                         \\
                         -volreg_align_to third                           \\
                         -regress_stim_times misc_files/stim_times.*.1D   \\
                         -regress_basis 'BLOCK(4,1)'                      \\
                         -regress_no_fitts

    --------------------------------------------------
    -ask_me EXAMPLES:

        a1. Apply -ask_me in the most basic form, with no other options.

                afni_proc.py -ask_me

        a2. Supply input datasets.

                afni_proc.py -ask_me -dsets ED/ED_r*.HEAD

        a3. Same as a2, but supply the datasets in expanded form.
            No suffix (.HEAD) is needed when wildcards are not used.

                afni_proc.py -ask_me                          \\
                     -dsets ED/ED_r01+orig ED/ED_r02+orig     \\
                            ED/ED_r03+orig ED/ED_r04+orig     \\
                            ED/ED_r05+orig ED/ED_r06+orig     \\
                            ED/ED_r07+orig ED/ED_r08+orig     \\
                            ED/ED_r09+orig ED/ED_r10+orig

        a4. Supply datasets, stim_times files and labels.

                afni_proc.py -ask_me                                    \\
                        -dsets ED/ED_r*.HEAD                            \\
                        -regress_stim_times misc_files/stim_times.*.1D  \\
                        -regress_stim_labels ToolMovie HumanMovie       \\
                                             ToolPoint HumanPoint

    --------------------------------------------------
    DEFAULTS: basic defaults for each block (not all defaults)

        setup:    - use 'SUBJ' for the subject id
                        (option: -subj_id SUBJ)
                  - create a t-shell script called 'proc_subj'
                        (option: -script proc_subj)
                  - use results directory 'SUBJ.results'
                        (option: -out_dir SUBJ.results)

        tcat:     - do not remove any of the first TRs

        empty:    - do nothing (just copy the data using 3dTcat)

        despike:  - NOTE: by default, this block is _not_ used
                  - use no extra options (so automask is default)

        tshift:   - align slices to the beginning of the TR
                  - use quintic interpolation for time series resampling
                        (option: -tshift_interp -quintic)

        volreg:   - align to third volume of first run, -zpad 1
                        (option: -volreg_align_to third)
                        (option: -volreg_zpad 1)
                  - use cubic interpolation for volume resampling
                        (option: -volreg_interp -cubic)

        blur:     - blur data using a 4 mm FWHM filter
                        (option: -blur_filter -1blur_fwhm)
                        (option: -blur_size 4)

        mask:     - apply union of masks from 3dAutomask on each run

        scale:    - scale each voxel to mean of 100, clip values at 200

        regress:  - use GAM regressor for each stim
                        (option: -regress_basis)
                  - compute the baseline polynomial degree, based on run length
                        (e.g. option: -regress_polort 2)
                  - output fit time series
                  - output ideal curves for GAM/BLOCK regressors
                  - output iresp curves for non-GAM/non-BLOCK regressors

    --------------------------------------------------
    OPTIONS: (information options, general options, block options)
             (block options are ordered by block)

        ------------ information options ------------

        -help                   : show this help
        -hist                   : show the module history
        -show_valid_opts        : show all valid options (brief format)
        -ver                    : show the version number

        ------------ general execution and setup options ------------

        -ask_me                 : ask the user about the basic options to apply

            When this option is used, the program will ask the user how they
            wish to set the basic options.  The intention is to give the user
            a feel for what options to apply (without using -ask_me).

        -bash                   : show example execution command in bash form

            After the script file is created, this program suggests how to run
            it (piping stdout/stderr through 'tee').  If the user is running
            the bash shell, this option will suggest the 'bash' form of a
            command to execute the newly created script.

            example of tcsh form for execution:

                tcsh -x proc.ED.8.glt |& tee output.proc.ED.8.glt

            example of bash form for execution:

                tcsh -x proc.ED.8.glt 2>&1 | tee output.proc.ED.8.glt

            Please see "man bash" or "man tee" for more information.

        -blocks BLOCK1 ...      : specify the processing blocks to apply

                e.g. -blocks volreg blur scale regress
                e.g. -blocks despike tshift volreg blur scale regress
                default: tshift volreg blur mask scale regress

            The user may apply this option to specify which processing blocks
            are to be included in the output script.  The order of the blocks
            may be varied, and blocks may be skipped.

            See also '-do_block' (e.g. '-do_block despike').

        -copy_anat ANAT         : copy the ANAT dataset to the results dir

                e.g. -copy_anat Elvis/mprage+orig

            This will apply 3dcopy to copy the anatomical dataset(s) to the
            results directory.  Note that if a +view is not given, 3dcopy will
            attempt to copy +acpc and +tlrc datasets, also.

            See also '3dcopy -help'.

        -copy_files file1 ...   : copy file1, etc. into the results directory

                e.g. -copy_files glt_AvsB.txt glt_BvsC.1D glt_eat_cheese.txt
                e.g. -copy_files contrasts/glt_*.txt

            This option allows the user to copy some list of files into the
            results directory.  This would happen before the tcat block, so
            such files may be used for other commands in the script (such as
            contrast files in 3dDeconvolve, via -regress_opts_3dD).

        -do_block BLOCK_NAME ...: add extra blocks in their default positions

                e.g. -do_block despike

            Currently, the 'despike' block is the only block not applied by
            default (in the processing script).  Any block not included in
            the default list can be added via this option.

            The default position for 'despike' is between 'tcat' and 'tshift'.

            This option should not be used with '-blocks'.

            See also '-blocks'.

        -dsets dset1 dset2 ...  : (REQUIRED) specify EPI run datasets

                e.g. -dsets Elvis_run1+orig Elvis_run2+orig Elvis_run3+orig
                e.g. -dsets Elvis_run*.HEAD

            The user must specify the list of EPI run datasets to analyze.
            When the runs are processed, they will be written to start with
            run 1, regardless of whether the input runs were just 6, 7 and 21.
        
            Note that when using a wildcard it is essential for the EPI
            datasets to be alphabetical, as that is how the shell will list
            them on the command line.  For instance, epi_run1+orig through
            epi_run11+orig is not alphabetical.  If they were specified via
            wildcard their order would end up as run1 run10 run11 run2 ...

            Note also that when using a wildcard it is essential to specify
            the datasets suffix, so that the shell doesn't put both the .BRIK
            and .HEAD filenames on the command line (which would make it twice
            as many runs of data).

        -keep_rm_files          : do not have script delete rm.* files at end

                e.g. -keep_rm_files

            The output script may generate temporary files in a block, which
            would be given names with prefix 'rm.'.  By default, those files
            are deleted at the end of the script.  This option blocks that
            deletion.

        -move_preproc_files     : move preprocessing files to preproc.data dir

            At the end of the output script, create a 'preproc.data' directory,
            and move most of the files there (dfile, outcount, pb*, rm*).

            See also -remove_preproc_files.

        -no_proc_command        : do not print afni_proc.py command in script

                e.g. -no_proc_command

            If this option is applied, the command used to generate the output
            script will be stored at the end of the script.

        -out_dir DIR            : specify the output directory for the script

                e.g. -out_dir ED_results
                default: SUBJ.results

            The AFNI processing script will create this directory and perform
            all processing in it.

        -remove_preproc_files   : delete pre-processed data

            At the end of the output script, delete the intermetiate data (to
            save disk space).  Delete dfile*, outcount*, pb* and rm*.

            See also -move_preproc_files.

        -script SCRIPT_NAME     : specify the name of the resulting script

                e.g. -script ED.process.script
                default: proc_subj

            The output of this program is a script file.  This option can be
            used to specify the name of that file.

            See also -scr_overwrite, -subj_id.

        -scr_overwrite          : overwrite any existing script

                e.g. -scr_overwrite

            If the output script file already exists, it will be overwritten
            only if the user applies this option.

            See also -script.

        -subj_id SUBJECT_ID     : specify the subject ID for the script

                e.g. -subj_id elvis
                default: SUBJ

            The subject ID is used in dataset names and in the output directory
            name (unless -out_dir is used).  This option allows the user to
            apply an appropriate naming convention.

        -tlrc_anat              : run @auto_tlrc on '-copy_anat' dataset

                e.g. -tlrc_anat

            After the regression block, run @auto_tlrc on the anatomical
            dataset provided by '-copy_anat'.  By default, warp the anat to
            align with TT_N27+tlrc, unless the '-tlrc_base' option is given.

            The -copy_anat option specifies which anatomy to transform.

            Please see '@auto_tlrc -help' for more information.
            See also -copy_anat, -tlrc_base, -tlrc_no_ss.

        -tlrc_base BASE_DSET    : run "@auto_tlrc -base BASE_DSET"

                e.g. -tlrc_base TT_icbm452+tlrc
                default: -tlrc_base TT_N27+tlrc

            This option is used to supply an alternate -base dataset for
            @auto_tlrc.  Otherwise, TT_N27+tlrc will be used.

            Note that the default operation of @auto_tlrc is to "skull strip"
            the input dataset.  If this is not appropriate, consider also the
            '-tlrc_no_ss' option.

            Please see '@auto_tlrc -help' for more information.
            See also -tlrc_anat, -tlrc_no_ss.

        -tlrc_no_ss             : add the -no_ss option to @auto_tlrc

                e.g. -tlrc_no_ss

            This option is used to tell @auto_tlrc not to perform the skull
            strip operation.

            Please see '@auto_tlrc -help' for more information.

        -tlrc_rmode RMODE       : apply RMODE resampling in @auto_tlrc

                e.g. -tlrc_rmode NN

            This option is used to apply '-rmode RMODE' in @auto_tlrc.

            Please see '@auto_tlrc -help' for more information.

        -tlrc_suffix SUFFIX     : apply SUFFIX to result of @auto_tlrc

                e.g. -tlrc_suffix auto_tlrc

            This option is used to apply '-suffix SUFFIX' in @auto_tlrc.

            Please see '@auto_tlrc -help' for more information.

        -verb LEVEL             : specify the verbosity of this script

                e.g. -verb 2
                default: 1

            Print out extra information during execution.

        ------------ block options ------------

        These options pertain to individual processing blocks.  Each option
        starts with the block name.

        -tcat_remove_first_trs NUM : specify how many TRs to remove from runs

                e.g. -tcat_remove_first_trs 3
                default: 0

            Since it takes several seconds for the magnetization to reach a
            steady state (at the beginning of each run), the initial TRs of
            each run may have values that are significantly greater than the
            later ones.  This option is used to specify how many TRs to
            remove from the beginning of every run.

        -despike_opts_3dDes OPTS... : specify additional options for 3dDespike

                e.g. -despike_opts_3dDes -nomask -ignore 2

            By default, 3dDespike is used with only -prefix.  Any other options
            must be applied via -despike_opts_3dDes.

            Note that the despike block is not applied by default.  To apply
            despike in the processing script, use either '-do_block despike'
            or '-blocks ... despike ...'.

            Please see '3dDespike -help' for more information.
            See also '-do_blocks', '-blocks'.

        -tshift_align_to TSHIFT OP : specify 3dTshift alignment option

                e.g. -tshift_align_to -slice 14
                default: -tzero 0

            By default, each time series is aligned to the beginning of the
            TR.  This option allows the users to change the alignment, and
            applies the option parameters directly to the 3dTshift command
            in the output script.

            It is likely that the user will use either '-slice SLICE_NUM' or
            '-tzero ZERO_TIME'.

            Note that when aligning to an offset other than the beginning of
            the TR, and when applying the -regress_stim_files option, then it
            may be necessary to also apply -regress_stim_times_offset, to
            offset timing for stimuli to later within each TR.

            Please see '3dTshift -help' for more information.
            See also '-regress_stim_times_offset'.
            
        -tshift_interp METHOD   : specify the interpolation method for tshift

                e.g. -tshift_interp -Fourier
                e.g. -tshift_interp -cubic
                default -quintic

            Please see '3dTshift -help' for more information.

        -tshift_opts_ts OPTS ... : specify extra options for 3dTshift

                e.g. -tshift_opts_ts -tpattern alt+z

            This option allows the user to add extra options to the 3dTshift
            command.  Note that only one -tshift_opts_ts should be applied,
            which may be used for multiple 3dTshift options.

            Please see '3dTshift -help' for more information.

        -volreg_align_to POSN   : specify the base position for volume reg

                e.g. -volreg_align_to last
                default: third

            This option takes 'first', 'third' or 'last' as a parameter.
            It specifies whether the EPI volumes are registered to the first
            or third volume (of the first run) or the last volume (of the last
            run).  The choice of 'first' or 'third' should correspond to when
            the anatomy was acquired before the EPI data.  The choice of 'last'
            should correspond to when the anatomy was acquired after the EPI
            data.

            The default of 'third' was chosen to go a little farther into the
            steady state data.

            Note that this is done after removing any volumes in the initial
            tcat operation.

            Please see '3dvolreg -help' for more information.
            See also -tcat_remove_first_trs, -volreg_base_ind.

        -volreg_base_ind RUN SUB : specify volume/brick indices for base

                e.g. -volreg_base_ind 10 123
                default: 0 0

            This option allow the user to specify exactly which dataset and
            sub-brick to use as the base registration image.  Note that the
            SUB index applies AFTER the removal of pre-steady state images.

          * The RUN number is 1-based, matching the run list in the output
            shell script.  The SUB index is 0-based, matching the sub-brick of
            EPI time series #RUN.  Yes, one is 1-based, the other is 0-based.
            Life is hard.

            The user can apply only one of the -volreg_align_to and
            -volreg_base_ind options.

            See also -volreg_align_to, -tcat_remove_first_trs.

        -volreg_interp METHOD   : specify the interpolation method for volreg

                e.g. -volreg_interp -quintic
                e.g. -volreg_interp -Fourier
                default -cubic

            Please see '3dTvolreg -help' for more information.

        -volreg_opts_vr OPTS ... : specify extra options for 3dvolreg

                e.g. -volreg_opts_vr -noclip -nomaxdisp

            This option allows the user to add extra options to the 3dvolreg
            command.  Note that only one -volreg_opts_vr should be applied,
            which may be used for multiple 3dvolreg options.

            Please see '3dvolreg -help' for more information.

        -volreg_zpad N_SLICES   : specify number of slices for -zpad

                e.g. -volreg_zpad 4
                default: -volreg_zpad 1

            This option allows the user to specify the number of slices applied
            via the -zpad option to 3dvolreg.

        -blur_filter FILTER     : specify 3dmerge filter option

                e.g. -blur_filter -1blur_rms
                default: -1blur_fwhm

            This option allows the user to specify the filter option from
            3dmerge.  Note that only the filter option is set here, not the
            filter size.  The two parts were separated so that users might
            generally worry only about the filter size.

            Please see '3dmerge -help' for more information.
            See also -blur_size.

        -blur_size SIZE_MM      : specify the size, in millimeters

                e.g. -blur_size 6.0
                default: 4

            This option allows the user to specify the size of the blur used
            by 3dmerge.  It is applied as the 'bmm' parameter in the filter
            option (such as -1blur_fwhm).

            Please see '3dmerge -help' for more information.
            See also -blur_filter.

        -blur_opts_merge OPTS ... : specify extra options for 3dmerge

                e.g. -blur_opts_merge -2clip -20 50

            This option allows the user to add extra options to the 3dmerge
            command.  Note that only one -blur_opts_merge should be applied,
            which may be used for multiple 3dmerge options.

            Please see '3dmerge -help' for more information.

        -mask_type TYPE         : specify 'union' or 'intersection' mask type

                e.g. -mask_type intersection
                default: union

            This option is used to specify whether the mask applied to the
            analysis is the union of masks from each run, or the intersection.
            The only valid values for TYPE are 'union' and 'intersection'.

            This is not how to specify that no mask is applied at all, that is
            done by excluding the mask block with the '-blocks' option.

            Please see '3dAutomask -help', '3dMean -help' or '3dcalc -help'.
            See also -mask_dilate, -blocks.

        -mask_dilate NUM_VOXELS : specify the number of time to dilate the mask

                e.g. -mask_dilate 3
                default: 1

            By default, the masks generated from the EPI data are dilated by
            1 step (voxel), via the -dilate option in 3dAutomask.  With this
            option, the user may specify the dilation.  Valid integers must
            be at least zero.

            Please see '3dAutomask -help' for more information.
            See also -mask_type.

        -scale_max_val MAX      : specify the maximum value for scaled data

                e.g. -scale_max_val 1000
                default 200

            The scale step multiples the time series for each voxel by a
            scalar so that the mean for that particular run is 100 (allowing
            interpretation of EPI values as a percentage of the mean).

            Values of 200 represent a 100% change above the mean, and so can
            probably be considered garbage (or the voxel can be considered
            non-brain).  The output values are limited so as not to sacrifice
            the precision of the values of short datasets.  Note that in a
            short (2-byte integer) dataset, a large range of values means
            bits of accuracy are lost for the representation.

            No max will be applied if MAX is <= 100.

            Please see 'DATASET TYPES' in the output of '3dcalc -help'.
            See also -scale_no_max.

        -scale_no_max           : do not apply a limit to the scaled values

            The default limit for scaled data is 200.  Use of this option will
            remove any limit from being applied.

            See also -scale_max_val.

        -regress_basis BASIS    : specify the regression basis function

                e.g. -regress_basis 'BLOCK(4,1)'
                e.g. -regress_basis 'BLOCK(5)'
                e.g. -regress_basis 'TENT(0,14,8)'
                default: GAM

            This option is used to set the basis function used by 3dDeconvolve
            in the regression step.  This basis function will be applied to
            all user-supplied regressors (please let me know if there is need
            to apply different basis functions to different regressors).
        
            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_basis_normall, -regress_stim_times.

        -regress_basis_normall NORM : specify the magnitude of basis functions

                e.g. -regress_basis_normall 1.0

            This option is used to set the '-basis_normall' parameter in
            3dDeconvolve.  It specifies the height of each basis function.

            For the example basis functions, -basis_normall is not recommended.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_basis.

        -regress_est_blur_epits      : estimate the smoothness of the EPI data

            This option specifies to run 3dFWHMx on each of the EPI datasets
            used for regression, the results of which are averaged.  These blur
            values are saved to the file blur_est.$subj.1D, along with any
            similar output from errts.

            These blur estimates may be input to AlphaSim, for any multiple
            testing correction done for this subject.  If AlphaSim is run at
            the group level, it is reasonable to average these estimates
            across all subjects (assuming they were scanned with the same
            protocol and at the same scanner).

            The mask block is required for this operation (without which the
            estimates are not reliable).  If masking is not desired for the
            regression, use the option '-regress_no_mask'.

            Please see '3dFWHMx -help' for more information.
            See also -regress_est_blur_errts, -regress_no_mask.

        -regress_est_blur_errts      : estimate the smoothness of the errts

            This option specifies to run 3dFWHMx on the errts dataset, output
            from the regression (by 3dDeconvolve).

            These blur estimates may be input to AlphaSim, for any multiple
            testing correction done for this subject.  If AlphaSim is run at
            the group level, it is reasonable to average these estimates
            across all subjects (assuming they were scanned with the same
            protocol and at the same scanner).

            Note that the errts blur estimates should be not only slightly
            more accurate than the epits blur estimates, but they should be
            slightly smaller, too (which is beneficial).

            The mask block is required for this operation (without which the
            estimates are not reliable).  If masking is not desired for the
            regression, use the option '-regress_no_mask'.

            Please see '3dFWHMx -help' for more information.
            See also -regress_est_blur_epits, -regress_no_mask.

        -regress_errts_prefix PREFIX : specify a prefix for the -errts option

                e.g. -regress_fitts_prefix errts

            This option is used to add a -errts option to 3dDeconvolve.  As
            with -regress_fitts_prefix, only the PREFIX is specified, to which
            the subject ID will be added.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_fitts_prefix.

        -regress_fitts_prefix PREFIX : specify a prefix for the -fitts option

                e.g. -regress_fitts_prefix model_fit
                default: fitts

            By default, the 3dDeconvolve command in the script will be given
            a '-fitts fitts' option.  This option allows the user to change
            the prefix applied in the output script.

            The -regress_no_fitts option can be used to eliminate use of -fitts.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_no_fitts.

        -regress_iresp_prefix PREFIX : specify a prefix for the -iresp option

                e.g. -regress_iresp_prefix model_fit
                default: iresp

            This option allows the user to change the -iresp prefix applied in
            the 3dDeconvolve command of the output script.  

            By default, the 3dDeconvolve command in the script will be given a
            set of '-iresp iresp' options, one per stimulus type, unless the
            regression basis function is GAM.  In the case of GAM, the response
            form is assumed to be known, so there is no need for -iresp.

            The stimulus label will be appended to this prefix so that a sample
            3dDeconvolve option might look one of these 2 examples:

                -iresp 7 iresp_stim07
                -iresp 7 model_fit_donuts

            The -regress_no_iresp option can be used to eliminate use of -iresp.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_no_iresp, -regress_basis.

        -regress_make_ideal_sum IDEAL.1D : create IDEAL.1D file from regressors

                e.g. -regress_make_ideal_sum ideal_all.1D

            If the -regress_basis function is a single parameter function
            (either GAM or some form of BLOCK), then this option can be
            applied to create an ideal response curve which is the sum of
            the individual stimulus response curves.

            Use of this option will add a 3dTstat command to sum the regressor
            (of interest) columns of the 1D X-matrix, output by 3dDeconvolve.

            This is similar to the default behavior of creating ideal_STIM.1D
            files for each stimulus label, STIM.

            Please see '3dDeconvolve -help' and '3dTstat -help'.
            See also -regress_basis, -regress_no_ideals.

        -regress_motion_file FILE.1D  : use FILE.1D for motion parameters

                e.g. -regress_motion_file motion.1D

            Particularly if the user performs motion correction outside of
            afni_proc.py, they may wish to specify a motion parameter file
            other than dfile.rall.1D (the default generated in the volreg
            block).

            If the motion parameter file is in an external directory, the
            user should copy it via the -copy_files option.

            See also -copy_files.

        -regress_no_fitts       : do not supply -fitts to 3dDeconvolve

                e.g. -regress_no_fitts

            This option prevents the program from adding a -fitts option to
            the 3dDeconvolve command in the output script.

            See also -regress_fitts_prefix.

        -regress_no_ideals      : do not generate ideal response curves

                e.g. -regress_no_ideals

            By default, if the GAM or BLOCK basis function is used, ideal
            response curve files are generated for each stimulus type (from
            the output X matrix using '3dDeconvolve -x1D').  The names of the
            ideal response function files look like 'ideal_LABEL.1D', for each
            stimulus label, LABEL.

            This option is used to suppress generation of those files.

            See also -regress_basis, -regress_stim_labels.

        -regress_no_iresp       : do not supply -iresp to 3dDeconvolve

                e.g. -regress_no_iresp

            This option prevents the program from adding a set of -iresp
            options to the 3dDeconvolve command in the output script.

            By default -iresp will be used unless the basis function is GAM.

            See also -regress_iresp_prefix, -regress_basis.

        -regress_no_mask        : do not apply the mask in regression

            This option prevents the program from applying the mask dataset
            in the regression step.  It also prevents the program from applying
            the mask in the scaling step.

            If the user does not want to apply a mask in the regression
            analysis, but wants the full_mask dataset for other reasons
            (such as computing blur estimates), this option is needed.

            See also -regress_est_blur_epits, -regress_est_blur_errts.

        -regress_no_motion      : do not apply motion params in 3dDeconvolve

                e.g. -regress_no_motion

            This option prevents the program from adding the registration
            parameters (from volreg) to the 3dDeconvolve command.

        -regress_opts_3dD OPTS ...   : specify extra options for 3dDeconvolve

                e.g. -regress_opts_3dD -gltsym ../contr/contrast1.txt  \\
                                       -glt_label 1 FACEvsDONUT        \\
                                       -xjpeg Xmat

            This option allows the user to add extra options to the 3dDeconvolve
            command.  Note that only one -regress_opts_3dD should be applied,
            which may be used for multiple 3dDeconvolve options.

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004

        -regress_polort DEGREE  : specify the polynomial degree of baseline

                e.g. -regress_polort 2
                default: 1 + floor(run_length / 150.0)

            3dDeconvolve models the baseline for each run separately, using
            Legendre polynomials (by default).  This option specifies the
            degree of polynomial.  Note that this will create DEGREE * NRUNS
            regressors.

            The default is computed from the length of a run, in seconds, as
            shown above.  For example, if each run were 320 seconds, then the
            default polort would be 3 (cubic).

            Please see '3dDeconvolve -help' for more information.

        -regress_stim_labels LAB1 ...   : specify labels for stimulus types

                e.g. -regress_stim_labels houses faces donuts
                default: stim01 stim02 stim03 ...

            This option is used to apply a label to each stimulus type.  The
            number of labels should equal the number of files used in the
            -regress_stim_times option, or the total number of columns in the
            files used in the -regress_stim_files option.

            These labels will be applied as '-stim_label' in 3dDeconvolve.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_stim_times, -regress_stim_labels.

        -regress_stim_times FILE1 ... : specify files used for -stim_times

                e.g. -regress_stim_times ED_stim_times*.1D
                e.g. -regress_stim_times times_A.1D times_B.1D times_C.1D

            3dDeconvolve will be run using '-stim_times'.  This option is
            used to specify the stimulus timing files to be applied, one
            file per stimulus type.  The order of the files given on the 
            command line will be the order given to 3dDeconvolve.  Each of
            these timing files will be given along with the basis function
            specified by '-regress_basis'.

            The user must specify either -regress_stim_times or 
            -regress_stim_files if regression is performed, but not both.
            Note the form of the files is one row per run.  If there is at
            most one stimulus per run, please add a trailing '*'.

            Labels may be specified using the -regress_stim_labels option.

            These two examples of such files are for a 3-run experiment.  In
            the second example, there is only 1 stimulus at all, occurring in
            run #2.

                e.g.            0  12.4  27.3  29
                                *
                                30 40 50

                e.g.            *
                                20 *
                                *

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_stim_files, -regress_stim_labels, -regress_basis,
                     -regress_basis_normall, -regress_polort.

        -regress_stim_files FILE1 ... : specify TR-locked stim files

                e.g. -regress_stim_times ED_stim_file*.1D
                e.g. -regress_stim_times stim_A.1D stim_B.1D stim_C.1D

            Without the -regress_use_stim_files option, 3dDeconvolve will be
            run using '-stim_times', not '-stim_file'.  The user can still
            specify the 3dDeconvolve -stim_file files here, but they would
            then be converted to -stim_times files using the script,
            make_stim_times.py .

            It might be more educational for the user to run make_stim_times.py
            outside afni_proc.py (such as was done before example 2, above), or
            to create the timing files directly.

            Each given file can be for multiple stimulus classes, where one
            column is for one stim class, and each row represents a TR.  So
            each file should have NUM_RUNS * NUM_TRS rows.

            The stim_times files will be labeled stim_times.NN.1D, where NN
            is the stimulus index.

            Note that if the stimuli were presented at a fixed time after
            the beginning of a TR, the user should consider the option,
            -regress_stim_times_offset, to apply that offset.

            ---

            If the -regress_use_stim_files option is provided, 3dDeconvolve
            will be run using each stim_file as a regressor.  The order of the
            regressors should match the order of any labels, provided via the
            -regress_stim_labels option.

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_stim_times, -regress_stim_labels, -regress_basis,
                     -regress_basis_normall, -regress_polort,
                     -regress_stim_times_offset, -regress_use_stim_files.

        -regress_stim_times_offset OFFSET : add OFFSET to -stim_times files

                e.g. -stim_times_offset 1.25
                default: 0

            If the -regress_stim_files option is used (so the script converts
            -stim_files to -stim_times before 3dDeconvolve), the user may want
            to add an offset to the times in the output timing files.

            For example, if -tshift_align_to is applied, and the user chooses
            to align volumes to the middle of the TR, it would be appropriate
            to add TR/2 to the times of the stim_times files.

            This OFFSET will be applied to the make_stim_times.py command in
            the output script.

            Please see 'make_stim_times.py -help' for more information.
            See also -regress_stim_files, -regress_use_stim_files,
                     -tshift_align_to.

        -regress_use_stim_times : use -stim_file in regression, not -stim_times

            The default operation of afni_proc.py is to convert TR-locked files
            for the 3dDeconvolve -stim_file option to timing files for the
            3dDeconvolve -stim_times option.

            If the -regress_use_stim_times option is provided, then no such
            conversion will take place.  This assumes the -regress_stim_files
            option is applied to provide such -stim_file files.

            This option has been renamed from '-regress_no_stim_times'.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_stim_files, -regress_stim_times, 
                     -regress_stim_labels.

    - R Reynolds  Dec, 2006                             thanks to Z Saad
    ===========================================================================
"""

g_history = """
    afni_proc.py history:

    1.0  Dec 20, 2006 : initial release
    1.1  Dec 20, 2006 : added -regress_use_stim_files
    1.2  Dec 21, 2006 : help, start -ask_me, updated when to use -iresp/ideal
    1.3  Dec 22, 2006 : change help to assumme ED's stim_times files exist
    1.4  Dec 25, 2006 : initial -ask_me
    1.5  Dec 27, 2006 : ask_me help
    1.6  Dec 28, 2006 : -gylsym examples, min(a,b) in scale block
    1.7  Jan 03, 2007 : help updates, no blank '\\' line from -gltsym
    1.8  Jan 08, 2007 :
         - changed default script name to proc.SUBJ_ID, and removed -script
           from most examples
         - added options -bash, -copy_files, -volreg_zpad, -tlrc_anat,
           -tlrc_base, -tlrc_no_ss, -tlrc_rmode, -tlrc_suffix
    1.9  Jan 09, 2007 : added aligned line wrapping (see afni_util.py)
    1.10 Jan 12, 2007 : set subj = $argv[1], added index to -glt_label in -help
    1.11 Jan 12, 2007 :
         - added options -move_preproc_files, -regress_no_motion
         - use $output_dir var in script, and echo version at run-time
         - append .$subj to more output files 
    1.12 Jan 16, 2007 : allow no +view when using -tlrc_anat
    1.13 Jan 17, 2007 : if -tlrc_anat, apply default option '-tlrc_suffix NONE'
    1.14 Jan 26, 2007 :
         - if only 1 run, warn user, do not use 3dMean
         - changed all True/False uses to 1/0 (for older python versions)
    1.15 Feb 02, 2007 :
         - output float for -blur_size
         - put execution command at top of script
    1.16 Feb 21, 2007 :
         - added optional 'despike' block
         - added options -do_block and -despike_opts_3dDes
    1.17 Feb 27, 2007 :
         - volreg_align_to defaults to 'third' (was 'first')
         - added +orig to despike input
         - added 'empty' block type, for a placeholder
    1.18 Mar 15, 2007 : minor changes on the ides of March (oooooooh...)
         - x1D output file uses x1D suffix
         - removed now unneeded -full_first option in 3dDeconvolve
    1.19 Mar 19, 2007 : allow for dataset TR stored in depreciated ms
    1.20 Mar 25, 2007 : added -help for long-existing -regress_use_stim_files
    1.21 Apr 19, 2007 : apply +orig in 1-run mean using 3dcopy
    1.22 May 08, 2007 :
         - change read_options() to be compatible with python version 2.2
         - '-basis_normall 1' is no longer used by default
         - rename -regress_no_stim_times to -regress_use_stim_files
    1.23 Jun 01, 2007 :
         - changed name of Xmat to X.xmat.1D
         - by default, apply -xjpeg in 3dDeconvolve
    1.24 Jun 04 2007 : added -scale_no_max
    1.25 Jun 27 2007 : on error, display failed command
    1.26 Oct 03 2007 : set default polort based on run length (like 3dDecon)
    1.27 Nov 26 2007 : added -volreg_interp, default is -cubic (was Fourier)
    1.28 Jan 22 2008 : estimating smoothness
         - added -regress_est_blur_errts, -regress_est_blur_epits options
           for estimating the blur in the EPI and errts data
         - added -regress_no_mask, -regress_errts_prefix and -show_valid_opts
    1.29 Jun 12 2008 : move code to afni_util.get_dset_reps_tr
    1.30 Jun 30 2008 : added -gen_epi_review and -no_epi_review options
    1.31 Sep 23 2008 : added -remove_preproc_files
    1.32 Oct 27 2008 : added -regress_motion_file

"""

g_version = "version 1.32, Oct 27, 2008"

# ----------------------------------------------------------------------
# dictionary of block types and modification functions

BlockLabels  = ['tcat', 'despike', 'tshift', 'volreg',
                'blur', 'mask', 'scale', 'regress', 'empty']
BlockModFunc  = {'tcat'   : db_mod_tcat,     'despike': db_mod_despike,
                 'tshift' : db_mod_tshift,
                 'volreg' : db_mod_volreg,   'blur'   : db_mod_blur,
                 'mask'   : db_mod_mask,     'scale'  : db_mod_scale,
                 'regress': db_mod_regress,  'empty'  : db_mod_empty}
BlockCmdFunc  = {'tcat'   : db_cmd_tcat,     'despike': db_cmd_despike,
                 'tshift' : db_cmd_tshift,
                 'volreg' : db_cmd_volreg,   'blur'   : db_cmd_blur,
                 'mask'   : db_cmd_mask,     'scale'  : db_cmd_scale,
                 'regress': db_cmd_regress,  'empty'  : db_cmd_empty}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# default block labels, and other labels (along with the label they follow)
DefLabels   = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
OtherDefLabels = {'despike':'tcat'}
OtherLabels    = ['empty']

# ----------------------------------------------------------------------
# data processing stream class
class SubjProcSream:
    def __init__(self, label):
        self.label      = label         # name for stream
        self.valid_opts = None          # list of possible user options
        self.user_opts  = None          # list of given user options

        self.blocks     = []            # list of ProcessBlock elements
        self.dsets      = []            # list of afni_name elements
        self.stims_orig = []            # orig list of stim files to apply
        self.stims      = []            # list of stim files to apply
        self.mot_labs   = []            # labels for motion params
        self.mot_file   = 'dfile.rall.1D' # motion parameter file
        self.opt_src    = 'cmd'         # option source
        self.subj_id    = 'SUBJ'        # hopefully user will replace this
        self.subj_label = '$subj'       # replace this for execution
        self.out_dir    = ''            # output directory for use by script
        self.od_var     = ''            # output dir variable: $output_dir
        self.script     = None          # script name, default proc.SUBJ
        self.overwrite  = 0             # overwrite script file?
        self.fp         = None          # file object
        self.anat       = None          # anatomoy to copy (afni_name class)
        self.rm_rm      = 1             # remove rm.* files
        self.gen_review = '@epi_review.$subj' # filename for gen_epi_review.py

        self.verb       = 1             # verbosity level

        self.tr         = 0.0           # TR, in seconds
        self.reps       = 0             # TRs per run
        self.runs       = 0             # number of runs
        self.mask       = None          # mask dataset
        self.regmask    = 1             # apply any full_mask in regression
        self.view       = '+orig'       # view could also be '+tlrc'

        self.bindex     = 0             # current block index
        self.pblabel    = ''            # previous block label

        return
        
    def show(self, mesg):
        print '%sSubjProcSream: %s' % (mesg, self.label)
        for block in self.blocks:
            block.show('    Block %d: ' % self.blocks.index(block))
        print '    Dsets : ',
        if self.verb > 2:
            print
            for dset in self.dsets: dset.show()
        else:
            for dset in self.dsets: print dset.pv(),
            print
        if self.verb > 2: self.valid_opts.show('valid_opts: ')
        if self.verb > 1: self.user_opts.show('user_opts: ')

    def init_opts(self):
        self.valid_opts = OptionList('init_opts')

        # input style options  rcr - update
        # self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

        # general execution options
        self.valid_opts.add_opt('-blocks', -1, [])
        self.valid_opts.add_opt('-do_block', -1, [])
        self.valid_opts.add_opt('-dsets', -1, [])

        self.valid_opts.add_opt('-out_dir', 1, [])
        self.valid_opts.add_opt('-scr_overwrite', 0, [])
        self.valid_opts.add_opt('-script', 1, [])
        self.valid_opts.add_opt('-subj_id', -1, [])

        self.valid_opts.add_opt('-ask_me', 0, [])       # QnA session
        self.valid_opts.add_opt('-bash', 0, [])
        self.valid_opts.add_opt('-copy_anat', 1, [])
        self.valid_opts.add_opt('-copy_files', -1, [])
        self.valid_opts.add_opt('-gen_epi_review', 1, [])
        self.valid_opts.add_opt('-no_epi_review', 0, [])
        self.valid_opts.add_opt('-keep_rm_files', 0, [])
        self.valid_opts.add_opt('-move_preproc_files', 0, [])
        self.valid_opts.add_opt('-remove_preproc_files', 0, [])
        self.valid_opts.add_opt('-tlrc_anat', 0, [])
        self.valid_opts.add_opt('-tlrc_base', 1, [])
        self.valid_opts.add_opt('-tlrc_no_ss', 0, [])
        self.valid_opts.add_opt('-tlrc_rmode', 1, [])
        self.valid_opts.add_opt('-tlrc_suffix', 1, [])

        # self.valid_opts.add_opt('-remove_pXX_files', 0, [])

        # block options
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [])

        self.valid_opts.add_opt('-despike_opts_3dDes', -1, [])

        self.valid_opts.add_opt('-tshift_align_to', -1, [])
        self.valid_opts.add_opt('-tshift_interp', 1, [])
        self.valid_opts.add_opt('-tshift_opts_ts', -1, [])

        self.valid_opts.add_opt('-volreg_align_to', 1, [],
                                ['first','third', 'last'])
        self.valid_opts.add_opt('-volreg_base_ind', 2, [])
        self.valid_opts.add_opt('-volreg_interp', 1, [])
        self.valid_opts.add_opt('-volreg_opts_vr', -1, [])
        self.valid_opts.add_opt('-volreg_zpad', 1, [])

        self.valid_opts.add_opt('-blur_filter', 1, [])
        self.valid_opts.add_opt('-blur_size', 1, [])
        self.valid_opts.add_opt('-blur_opts_merge', -1, [])

        self.valid_opts.add_opt('-mask_type', 1, [], ['union','intersection'])
        self.valid_opts.add_opt('-mask_dilate', 1, [])

        self.valid_opts.add_opt('-scale_max_val', 1, [])
        self.valid_opts.add_opt('-scale_no_max', 0, [])

        self.valid_opts.add_opt('-regress_basis', 1, [])
        self.valid_opts.add_opt('-regress_basis_normall', 1, [])
        self.valid_opts.add_opt('-regress_polort', 1, [])
        self.valid_opts.add_opt('-regress_stim_files', -1, [])
        self.valid_opts.add_opt('-regress_stim_labels', -1, [])
        self.valid_opts.add_opt('-regress_stim_times', -1, [])
        self.valid_opts.add_opt('-regress_no_stim_times', 0, [])
        self.valid_opts.add_opt('-regress_stim_times_offset', 1, [])
        self.valid_opts.add_opt('-regress_use_stim_files', 0, [])
        self.valid_opts.add_opt('-regress_motion_file', 1, [])

        self.valid_opts.add_opt('-regress_est_blur_epits', 0, [])
        self.valid_opts.add_opt('-regress_est_blur_errts', 0, [])
        self.valid_opts.add_opt('-regress_errts_prefix', 1, [])
        self.valid_opts.add_opt('-regress_fitts_prefix', 1, [])
        self.valid_opts.add_opt('-regress_iresp_prefix', 1, [])
        self.valid_opts.add_opt('-regress_make_ideal_sum', 1, [])
        self.valid_opts.add_opt('-regress_no_fitts', 0, [])
        self.valid_opts.add_opt('-regress_no_ideals', 0, [])
        self.valid_opts.add_opt('-regress_no_iresp', 0, [])
        self.valid_opts.add_opt('-regress_no_mask', 0, [])
        self.valid_opts.add_opt('-regress_no_motion', 0, [])
        self.valid_opts.add_opt('-regress_opts_3dD', -1, [])


        # other options
        self.valid_opts.add_opt('-help', 0, [])
        self.valid_opts.add_opt('-hist', 0, [])
        self.valid_opts.add_opt('-show_valid_opts', 0, [])
        self.valid_opts.add_opt('-ver', 0, [])
        self.valid_opts.add_opt('-verb', 1, [])

        self.valid_opts.trailers = 0   # do not allow unknown options
        
    def get_user_opts(self):
        self.user_opts = read_options(sys.argv, self.valid_opts)
        if self.user_opts == None: return 1     # error condition
        if len(self.user_opts.olist) == 0:      # no options: apply -help
            print g_help_string
            return 0
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt: print "** seem to have trailers, but cannot find them!"
            else: print "** have invalid trailing args: %s", opt.show()
            return 1  # failure

        # maybe the users justs wants a complete option list
        if self.user_opts.find_opt('-show_valid_opts'):
            self.valid_opts.show('afni_proc.py:', 1)
            return 0  # gentle termination
        
        # apply the user options
        rv = self.apply_initial_opts(self.user_opts)
        if rv != None: return rv

        # update out_dir now (may combine option results)
        if self.out_dir == '': self.out_dir = '%s.results' % self.subj_label
        self.od_var = '$output_dir'

        if self.verb > 1: show_args_as_command(sys.argv, "executing command:")
        if self.verb > 3: self.show('end get_user_opts ')

    # apply the general options - many terminate program
    def apply_initial_opts(self, opt_list):
        opt = opt_list.find_opt('-verb')   # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        if opt_list.find_opt('-help'):     # just print help
            print g_help_string
            return 0  # gentle termination
        
        if opt_list.find_opt('-hist'):     # print the history
            print g_history
            return 0  # gentle termination
        
        if opt_list.find_opt('-ver'):      # show the version string
            print g_version
            return 0  # gentle termination
        
        opt = opt_list.find_opt('-copy_anat')
        if opt != None: self.anat = afni_name(opt.parlist[0])

        opt = opt_list.find_opt('-gen_epi_review')  # name epi review script
        if opt != None: self.gen_review = opt.parlist[0]

        opt = opt_list.find_opt('-no_epi_review') # no epi review script
        if opt != None: self.gen_review = None

        opt = opt_list.find_opt('-keep_rm_files')
        if opt != None: self.rm_rm = 0

        opt = opt_list.find_opt('-out_dir')
        if opt != None: self.out_dir = opt.parlist[0]

        opt = opt_list.find_opt('-subj_id') # -- needs to be before script
        if opt != None: self.subj_id = opt.parlist[0]

        opt = opt_list.find_opt('-script')
        if opt != None: self.script = opt.parlist[0]
        else:           self.script = 'proc.%s' % self.subj_id

        opt = opt_list.find_opt('-scr_overwrite')
        if opt != None: self.overwrite = 1

    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        # first, note datasets
        opt = self.user_opts.find_opt('-dsets')
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))

        blocklist = DefLabels  # init to defaults

        # check for -do_block options
        opt = self.user_opts.find_opt('-do_block')
        if opt and opt.parlist and len(opt.parlist) > 0:
            if self.user_opts.find_opt('-blocks'):
                print '** error: -do_block invalid when using -blocks'
                return 1

            # check additional blocks one by one
            errs = 0
            for bname in opt.parlist:
                if bname in OtherDefLabels:
                    preindex = blocklist.index(OtherDefLabels[bname])
                    if preindex < 0:
                        print "** error: -do_block failure for '%s'" % bname
                        errs += 1
                    # so add the block to blocklist
                    preindex += 1
                    blocklist[preindex:preindex] = [bname]
                else:
                    print "** error: '%s' is invalid in '-do_block'" % bname
                    errs += 1
            if errs > 0 : return 1

        opt = self.user_opts.find_opt('-blocks')
        if opt:  # then create blocklist from user opts (but prepend tcat)
            if opt.parlist[0] != 'tcat':
                blocklist = ['tcat'] + opt.parlist
            else: blocklist = opt.parlist

        for label in blocklist:
            rv = self.add_block(label)
            if rv != None: return rv

        # maybe the user wants to be quizzed for options
        uopt = self.user_opts.find_opt('-ask_me')
        if uopt != None:
            if ask_me.ask_me_subj_proc(self):
                return 1
            for label in blocklist:
                block = self.find_block(label)
                if not block:
                    print "** error: missing block '%s' in ask_me update"%label
                    return 1
                BlockModFunc[label](block, self, self.user_opts) # modify block

        # rcr - if there is another place to update options from, do it
        uopt = self.user_opts.find_opt('-opt_source')
        if uopt != None:
            print '** not ready to update options from %s' % str(uopt.parlist)
            return 1

        # do some final error checking
        if len(self.dsets) == 0:
            print 'error: dsets have not been specified (consider -dsets)'
            return 1

        if not uniq_list_as_dsets(self.dsets, 1): return 1
        if not db_tlrc_opts_okay(self.user_opts): return 1

    # create script from blocks and options
    def create_script(self):
        rv = self.get_run_info()
        if rv != None: return rv

        rv = self.init_script()         # create the script file and header
        if rv != None: return rv

        errs = 0
        for block in self.blocks:
            if not block.apply: continue        # skip such blocks
            cmd_str = BlockCmdFunc[block.label](self, block)
            if cmd_str == None:
                print "** script creation failure for block '%s'" % block.label
                errs += 1
            else:
                self.fp.write(add_line_wrappers(cmd_str))
                if self.verb>2: block.show('+d post command creation: ')
                if self.verb>1: print '+d %s command: %s'%(block.label, cmd_str)

        if self.gen_review:
            cmd_str = db_cmd_gen_review(self)
            if cmd_str:
                self.fp.write(add_line_wrappers(cmd_str))
                if self.verb > 1:
                    print "+d generated EPI review script %s" % self.gen_review
            else:
                errs += 1
                if self.verb > 1:
                    print '** failed to generate EPI review script'

        rv = self.finalize_script()     # finish the script
        if rv: errs += 1

        if self.fp: self.fp.close()

        if errs > 0: return 1    # so we print all errors before leaving

        if self.verb > 0:
            if self.runs == 1:
                print "\n-------------------------------------\n" \
                        "** warning have only 1 run to analyze\n" \
                        "-------------------------------------"
            print "\n--> script is file: %s" % self.script
            if self.user_opts.find_opt('-bash'): # give bash form
                print '    (consider: "tcsh -x %s 2>&1 | tee output.%s") ' \
                                % (self.script, self.script)
            else:                                # give tcsh form
              print '    (consider the command "tcsh -x %s |& tee output.%s")' \
                                % (self.script, self.script)

        return

    def get_run_info(self):
        self.runs = len(self.dsets)
        if self.runs < 1:
            print "** have no dsets to analyze"
            return 1

        # updated by 'tcat' opteration (and -remove_trs option)
        dset = self.dsets[0].rpv()

        err, self.reps, self.tr = get_dset_reps_tr(dset, self.verb)
        if err: return 1   # check for failure

    # create a new block for the given label, and append it to 'blocks'
    def add_block(self, label):
        block = ProcessBlock(label, self)
        if not block.valid:
            print '** invalid block : %s' % block.label
            return 1
        if self.verb > 2: block.show('+d post init block: ')
        self.blocks.append(block)

    def find_block(self, label):
        for block in self.blocks:
            if block.label == label: return block
        return None

    def find_block_index(self, label):
        block = self.find_block(label)
        if block: return self.blocks.index(block)
        return None

    # set subj shell variable, check output dir, create and cd
    def init_script(self):
        if not self.overwrite and os.path.isfile(self.script):
            print "error: script file '%s' already exists, exiting..." % \
                  self.script
            return 0
        try: self.fp = open(self.script,'w')
        except:
            print "cannot open script file '%s' for writing" % self.script
            return 1

        self.fp.write('#!/usr/bin/env tcsh\n\n')
        self.fp.write('echo "auto-generated by afni_proc.py, %s"\n' % asctime())
        self.fp.write('echo "(%s)"\n\n'% g_version)

        # include execution method in script
        if self.user_opts.find_opt('-bash'): # give bash form
            self.fp.write('# execute via : tcsh -x %s 2>&1 | tee output.%s\n\n'\
                            % (self.script, self.script))
        else:                                # give tcsh form
            self.fp.write('# execute via : tcsh -x %s |& tee output.%s\n\n' \
                            % (self.script, self.script))

        self.fp.write('# --------------------------------------------------\n'
                      '# script setup\n\n'
                      '# the user may specify a single subject to run with\n')
        self.fp.write('if ( $#argv > 0 ) then  \n'
                      '    set subj = $argv[1] \n'
                      'else                    \n'
                      '    set subj = %s       \n'
                      'endif\n\n' % self.subj_id )
        self.fp.write('# assign output directory name\n'
                      'set output_dir = %s\n\n' % self.out_dir)
        self.fp.write('# verify that the results directory does not yet exist\n'
                      'if ( -d %s ) then \n'
                      '    echo output dir "$subj.results" already exists\n'
                      '    exit                     \n'
                      'endif\n\n' % self.od_var)
        self.fp.write('# set list of runs\n')
        self.fp.write('set runs = (`count -digits 2 1 %d`)\n\n' % self.runs)

        self.fp.write('# create results directory\n')
        self.fp.write('mkdir %s\n\n' % self.od_var)

        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
            str = '# create stimuli directory, and copy stim files\n' \
                  'mkdir %s/stimuli\ncp ' % self.od_var
            for ind in range(len(self.stims)):
                str += '%s ' % self.stims_orig[ind]
            str += '%s/stimuli\n\n' % self.od_var
            self.fp.write(add_line_wrappers(str))

        if self.anat:
            str = '# copy anatomy to results dir\n'     \
                  '3dcopy %s %s/%s\n\n' %               \
                      (self.anat.rpv(), self.od_var, self.anat.prefix)
            self.fp.write(add_line_wrappers(str))

        opt = self.user_opts.find_opt('-copy_files')
        if opt and len(opt.parlist) > 0:
            str = '# copy extra files into results dir\n' \
                  'cp -rv %s %s\n\n' %                    \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.fp.write(add_line_wrappers(str))

        self.fp.flush()

    # and last steps
    def finalize_script(self):
        str = '# -------------------------------------------------------\n\n'
        self.fp.write(str)

        if self.rm_rm:
            self.fp.write('# remove temporary rm.* files\n'
                          '\\rm -f rm.*\n\n')

        if self.user_opts.find_opt('-move_preproc_files'):
            cmd_str = \
              "# move preprocessing files to 'preproc.data' directory\n"   \
              "mkdir preproc.data\n"                                       \
              "mv dfile.r??.1D outcount* pb??.$subj.r??.* rm.* preproc.data\n\n"
            self.fp.write(add_line_wrappers(cmd_str))
        elif self.user_opts.find_opt('-remove_preproc_files'):
            cmd_str = \
              "# remove preprocessing files to save disk space\n"   \
              "\\rm dfile.r??.1D pb??.$subj.r??.* rm.*\n\n"
            self.fp.write(add_line_wrappers(cmd_str))

        if self.user_opts.find_opt('-tlrc_anat'):
            cmd_str = db_cmd_tlrc(self.anat.pv(), self.user_opts)
            if cmd_str == None:
                print "** script creation failure for block 'tlrc'"
                return 1
            self.fp.write(add_line_wrappers(cmd_str))

        self.fp.write('# return to parent directory\n'
                      'cd ..\n\n')

        opt = self.user_opts.find_opt('-no_proc_command')
        str = '\n\n\n'                                                       \
              '# -------------------------------------------------------\n'  \
              '# script generated by the command:\n'                         \
              '#\n'                                                          \
              '# %s %s\n' % (os.path.basename(sys.argv[0]),
                                 ' '.join(quotize_list(sys.argv[1:],'')))
        self.fp.write(add_line_wrappers(str))

        if self.user_opts.find_opt('-ask_me'):
            str = '#\n# all applied options: '
            for opt in self.user_opts.olist:
                if opt.name == '-ask_me': continue
                str += opt.name+' '
                str += ' '.join(quotize_list(opt.parlist,''))
            str += '\n'
            self.fp.write(add_line_wrappers(str))

    # given a block, run, return a prefix of the form: pNN.SUBJ.rMM.BLABEL
    #    NN = block index, SUBJ = subj label, MM = run, BLABEL = block label
    def prefix_form(self, block, run):
        return 'pb%02d.%s.r%02d.%s' %    \
                (self.bindex, self.subj_label, run, block.label)

    # same, but leave run as a variable
    def prefix_form_run(self, block):
        return 'pb%02d.%s.r$run.%s' % (self.bindex, self.subj_label,block.label)

    # same as prefix_form, but use previous block values (index and label)
    # (so we don't need the block)
    def prev_prefix_form(self, run):
        return 'pb%02d.%s.r%02d.%s' %    \
                (self.bindex-1, self.subj_label, run, self.pblabel)

    # same, but leave run as a variable
    def prev_prefix_form_run(self):
        return 'pb%02d.%s.r$run.%s' %    \
                (self.bindex-1, self.subj_label, self.pblabel)

    # same, but leave run wild
    def prev_prefix_form_rwild(self):
        return 'pb%02d.%s.r??.%s' %    \
                (self.bindex-1, self.subj_label, self.pblabel)

    # like prefix, but list the whole dset form, in wildcard format
    def dset_form_wild(self, blabel):
        bind = self.find_block_index(blabel)
        if bind == None:
            print "** DFW: failed to find block for label '%s'" % blabel
            return ''
        return 'pb%02d.%s.r??.%s%s.HEAD' %      \
               (bind, self.subj_label, blabel, self.view)

class ProcessBlock:
    def __init__(self, label, proc):
        self.label = label      # label is block type
        self.valid = 0          # block is not yet valid
        self.apply = 1          # apply block to output script
        self.verb  = proc.verb  # verbose level
        if not label in BlockModFunc: return

        self.opts = OptionList(label)                   # init option list
        BlockModFunc[label](self, proc, proc.user_opts) # add block

        if self.verb > 2:
            if self.valid: self.show('+d successful add ')
            else:          print '+d invalid ProcessBlock: %s' % label

    def show(self, mesg):
        print '------- %sProcessBlock: %s -------' % (mesg, self.label)
        self.opts.show('new options: ')

def run_proc():

    ps = SubjProcSream('subject regression')
    ps.init_opts()

    rv = ps.get_user_opts()
    if rv != None:  # 0 is a valid return
        if rv != 0:
            show_args_as_command(sys.argv, "** failed command (get_user_opts):")
        return rv

    if ps.create_blocks():
        show_args_as_command(sys.argv, "** failed command (create_blocks):")
        return rv

    rv = ps.create_script()
    if rv != None:  # terminal, but do not display command on 0
        if rv != 0:
            show_args_as_command(sys.argv, "** failed command (create_script):")
        return rv

# main
if __name__ == '__main__':

    run_proc()

