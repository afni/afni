#!/usr/bin/env python

# todo: - verify whether dsets contain subj_id as sub-string
#
#
# For now, do not consider re-running old process steps from
# script dir.  Since the purpose of this python script is to
# create a tcsh script, then that script would need to be
# responsible for creation of the python state.  Maybe we can
# create a state file with the subject ID, and allow the user
# to specify it along with an execution directory.
#
# dset name form is index.user_prefix.run.operation
# e.g. p02.SUBJ.r03.volreg (+orig)
#
# note: in the script, runs are 1-based (probably expected)

import sys
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
    interface by a tk GUI (eventually).  The user should minimally provide
    the input datasets (-dsets) and stimulus files (-regress_stim_*).

    The output script will create a results directory, copy input files into
    it, and perform all processing there.  So the user can delete the results
    directory and re-run the script at their leisure.

    Note that the output script need to be ever run.  The user should feel
    free to modify the script for their own evil purposes.

    --------------------------------------------------
    The output script will go through the following steps, unless the user
    specifies otherwise.

    automatic steps:

        setup       : check subject arg, set run list, create output dir, and
                      copy stim files
        tcat        : copy input datasets and remove unwanted initial TRs

    optional steps:

        tshift      : slice timing alignment on volumes (default is -time 0)
        volreg      : volume registration (default to first volume)
        blur        : blur each volume (default is 4mm fwhm)
        mask        : create a 'brain' mask from the EPI data (dilate 1 voxel)
        scale       : scale each run mean to 100, for each voxel (max of 200)
        regress     : regression analysis (default is GAM, peak 1, with motion
                      params)

    --------------------------------------------------
    EXAMPLES (options can be provided in any order):

        1. Minimum use, provide datasets and stim files (or stim_times files).
           Note that a dataset suffix (e.g. HEAD) must be used with wildcards,
           so that datasets are not applied twice.  In this case, a stim_file
           with many columns is given, allowing the script to change it to
           stim_times files.

                afni_proc.py -dsets epiRT*.HEAD              \\
                             -regress_stim_files stims.1D


     ** The following examples can be run from the AFNI_data2 directory, and
        are examples of how one might process the data for subject ED.

        Because the stimuli are on a 1-second grid, while the EPI data is on a
        2-second grid (TR = 2.0), we will run make_stim_times.py externally
        (as opposed to giving stim_files to the afni_proc.py program) as
        follows:

            make_stim_times.py -prefix ED_times -tr 1.0 -nruns 10 -nt 272 \\
                   -files misc_files/all_stims.1D

        Then we will apply misc_files/ED_times.*.1D as the stim timing files.

        2. This example shows basic usage, with the default GAM regressor.
           We specify the output script name, the subject ID, removal of the
           first 2 TRs of each run (before steady state), and volume alignment
           to the end of the runs (the anat was acquired after the EPI).

                afni_proc.py -dsets ED/ED_r??+orig.HEAD      \\
                             -script process_ED              \\
                             -subj_id ED                     \\
                             -tcat_remove_first_trs 2        \\
                             -volreg_align_to first          \\
                             -regress_stim_times misc_files/ED_times.*.1D

        3. Similar to #2, but add labels for the 4 stim types, and apply TENT
           as the basis function to get 14 seconds of response, on a 2-second
           TR grid.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD                    \\
                             -script process_ED.8                          \\
                             -subj_id ED.8                                 \\
                             -tcat_remove_first_trs 2                      \\
                             -volreg_align_to first                        \\
                             -regress_stim_times misc_files/ED_times.*.1D  \\
                             -regress_stim_labels ToolMovie HumanMovie     \\
                                                  ToolPoint HumanPoint     \\
                             -regress_basis 'TENT(0,14,8)'

        4. Similar to #3, but find the response for the TENT functions on a
           1-second grid, such as how the data is processed in the class
           script, s1.analyze_ht05.  This is similar to using '-stim_nptr 2',
           and requires the addition of 3dDeconvolve option '-TR_times 1.0' to  
           see the -iresp output on a 1.0 second grid.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD                    \\
                             -script process_ED.15                         \\
                             -subj_id ED.15                                \\
                             -tcat_remove_first_trs 2                      \\
                             -volreg_align_to first                        \\
                             -regress_stim_times misc_files/ED_times.*.1D  \\
                             -regress_stim_labels ToolMovie HumanMovie     \\
                                                  ToolPoint HumanPoint     \\
                             -regress_basis 'TENT(0,14,15)'                \\
                             -regress_opts_3dD -TR_times 1.0

        5. Similar to #2, but skip tshift and mask steps (so the others must
           be specified), and apply a 4 second BLOCK response function.  Also,
           prevent output of a fit time series dataset, and copy the anatomical
           dataset(s) to the results directory.

                afni_proc.py -dsets ED/ED_r??+orig.HEAD                 \\
                         -blocks volreg blur scale regress              \\
                         -script process_ED.b4                          \\
                         -subj_id ED.b4                                 \\
                         -copy_anat ED/EDspgr                           \\
                         -tcat_remove_first_trs 2                       \\
                         -volreg_align_to first                         \\
                         -regress_stim_times misc_files/ED_times.*.1D   \\
                         -regress_basis 'BLOCK(4,1)'                    \\
                         -regress_no_fitts

    --------------------------------------------------
    OPTIONS:

        ------------ information options ------------

        -help                   : show this help
        -hist                   : show the module history
        -ver                    : show the version number

        ------------ general execution options ------------

        -blocks BLOCK1 ...      : specify the processing blocks to apply

                e.g. -blocks volreg blur scale regress
                default: tshift volreg blur mask scale regress

            The user may apply this option to specify which processing blocks
            are to be included in the output script.  The order of the blocks
            may be varied, and blocks may be skipped.

        -copy_anat ANAT         : copy the ANAT dataset to the results dir

                e.g. -copy_anat Elvis/mprage+orig

            This will apply 3dcopy to copy the anatomical dataset(s) to the
            results directory.  Note that if a +view is not given, 3dcopy will
            attempt to copy +acpc and +tlrc datasets, also.

            See also '3dcopy -help'.

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
            wildward their order would end up as run1 run10 run11 run2 ...

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

        -no_proc_command        : do not print afni_proc.py command in script

                e.g. -no_proc_command

            If this option is applied, the command used to generate the output
            script will be stored at the end of the script.

        -out_dir DIR            : specify the output directory for the script

                e.g. -out_dir ED_results
                default: SUBJ.results

            The AFNI processing script will create this directory and perform
            all processing in it.

        -script SCRIPT_NAME     : specify the name of the resulting script

                e.g. -script @ED.process.script
                default: @proc_subj

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

            The suject ID is used in dataset names and in the output directory
            name (unless -out_dir is used).  This option allows the user to
            apply an appropriate naming convention.

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

            Since the scanner takes several seconds to reach a steady state,
            the initial TRs of each run may have values that are significantly
            greater than the later ones.  This option is used to specify how
            many TRs to remove from the beginning of every run.

        -tshift_align_to TSHIFT OP : specify 3dTshift alignment option

                e.g. -tshift_align_to -slice 14
                default: -tzero 0

            By default, each time series is aligned to the beginning of the TR.
            This option allows the users to change the alignment, and applies
            the option parmeters directly to the 3dTshift command in the output
            script.

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
                default: first

            This option takes either 'first' or 'last' as a parameter.  It
            specifies whether the EPI volumes are registered to the first
            volume (of the first run) or the last volume (of the last run).
            The choice of 'first' or 'last' should corresponding to when
            anatomical dataset was acquired.

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

            The RUN number is 1-based, matching the run list in the output
            shell script.  The SUB index is 0-based, matching the sub-brick of
            EPI time series #RUN.  Yes, one is 1-based, the other is 0-based.
            Life is hard.

            The user can apply only one of the -volreg_align_to and
            -volreg_base_ind options.

            See also -volreg_align_to, -tcat_remove_first_trs.

        -volreg_opts_vr OPTS ... : specify extra options for 3dvolreg

                e.g. -volreg_opts_vr -noclip -nomaxdisp

            This option allows the user to add extra options to the 3dvolreg
            command.  Note that only one -volreg_opts_vr should be applied,
            which may be used for multiple 3dvolreg options.

            Please see '3dvolreg -help' for more information.

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

            The user may remove the limit by applying a value <= 0.

            Please see 'DATASET TYPES' in the output of '3dcalc -help'.

        -regress_basis BASIS    : specify the regression basis function

                e.g. -regress_basis 'BLOCK(4,1)'
                e.g. -regress_basis 'TENT(8,14)'
                default: GAM

            This option is used to set the basis function used by 3dDeconvolve
            in the regression step.  This basis function will be applied to
            all user-supplied regressors (please let me know if there is need
            to apply different basis functions to different regressors).
        
            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_basis_normall, -regress_stim_times.

        -regress_basis_normall NORM : specify the magnitude of basis functions

                e.g. -regress_basis_normall 3.14159
                default: 1.0

            This option is used to set the '-basis_normall' parameter in
            3dDeconvolve.  It specifies the height of each basis function.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_basis.

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

            This is simlilar to the default behavior of creating ideal_STIM.1D
            files for each stimulus label, STIM.

            Please see '3dDeconvolve -help' and '3dTstat -help'.
            See also -regress_basis, -regress_no_ideals.

        -regress_no_fitts       : do not supply -fitts to 3dDeconvolve

                e.g. -regress_no_fitts

            This option prevents the program from adding a -fitts option to
            the 3dDeconvolve command in the output script.

            See also -regress_fitts_prefix.

        -regress_no_ideals      : do not generate ideal response curves

                e.g. -regress_no_ideals

            By default, if the GAM or BLOCK basis function is used, ideal
            resonse curve files are generated for each stimulus type (from
            the output X matrix using '3dDeconvolve -x1D').  The names of the
            ideal response function files look like 'ideal_LABEL.1D', for each
            stimulus label, LABEL.

            This option is used to supress generation of those files.

            See also -regress_basis, -regress_stim_labels.

        -regress_no_iresp       : do not supply -iresp to 3dDeconvolve

                e.g. -regress_no_iresp

            This option prevents the program from adding a set of -iresp
            options to the 3dDeconvolve command in the output script.

            By default -iresp will be used unless the basis function is GAM.

            See also -regress_iresp_prefix, -regress_basis.

        -regress_opts_3dD OPTS ...   : specify extra options for 3dDeconvolve

                e.g. -regress_opts_3dD -gltsym ../contr/contrast1.1D \\
                                       -glt_label FACEvsDONUT        \\
                                       -xjpeg Xmat

            This option allows the user to add extra options to the 3dDeconvolve
            command.  Note that only one -regress_opts_3dD should be applied,
            which may be used for multiple 3dDeconvolve options.

            Please see '3dDeconvolve -help' for more information.

        -regress_polort DEGREE  : specify the polynomial degree of baseline

                e.g. -regress_polort 1
                default: 2

            3dDeconvolve models the baseline for each run separately, using
            Legendre polynomials (by default).  This option specifies the
            degree of polynomial.  Note that this will create DEGREE * NRUNS
            regressors.

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004

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

            These two examples of such files are for a 3-run experiment.
            In the second example, there is only 1 stimulus at all, occuring
            in run 2.

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

        -regress_stim_files FILE1 ... : specify TR-based stim files

                e.g. -regress_stim_times ED_stim_file*.1D
                e.g. -regress_stim_times stim_A.1D stim_B.1D stim_C.1D

            3dDeconvolve will be run using '-stim_times', no '-stim_file'.
            The user is given the option to specify the antiquated stim_file
            files here, which would then be replace using the script,
            make_stim_times.py .  It might be more educational for the user
            to run make_stim_times.py, or to create the timing files directly.

            Each given file can be for multiple stimulus classes, where one
            column is for one stim class, and each row represents a TR.  So
            each file should have NUM_RUNS * NUM_TRS rows.

            The stim_times files will be labeled stim_times.NN.1D, where NN
            is the stimulus index.

            Note that if the stimuli were presented at a fixed time after
            the beginning of a TR, the user should consider the option,
            -regress_stim_times_offset, to apply that offset.

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_stim_times, -regress_stim_labels, -regress_basis,
                     -regress_basis_normall, -regress_polort,
                     -regress_stim_times_offset.

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
            See also -regress_stim_files, -tshift_align_to.

    - R Reynolds  Dec, 2006                             thanks to Z Saad
    ===========================================================================
"""

g_history = """
    afni_proc.py history:

    1.0  Dec 20, 2006 : initial release
    1.1  Dec 20, 2006 : added -regress_no_stim_times
"""

g_version = "version 1.0, December 20, 2006"

# ----------------------------------------------------------------------
# dictionary of block types and modification functions
BlockLabels  = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
BlockModFunc  = {'tcat'   : db_mod_tcat,     'tshift' :db_mod_tshift,
                 'volreg' : db_mod_volreg,   'blur'   :db_mod_blur,
                 'mask'   : db_mod_mask,     'scale'  : db_mod_scale,
                 'regress':db_mod_regress}
BlockCmdFunc  = {'tcat'   : db_cmd_tcat,     'tshift' :db_cmd_tshift,
                 'volreg' : db_cmd_volreg,   'blur'   :db_cmd_blur,
                 'mask'   : db_cmd_mask,     'scale'  : db_cmd_scale,
                 'regress':db_cmd_regress}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

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
        self.opt_src    = 'cmd'         # option source
        self.subj_id    = 'SUBJ'        # hopefully user will replace this
        self.subj_label = '$subj'       # replace this for execution
        self.out_dir    = ''            # output directory for use by script
        self.script     = '@proc_subj'
        self.overwrite  = False         # overwrite script file?
        self.fp         = None          # file object
        self.anat       = None          # anatomoy to copy (afni_name class)
        self.rm_rm      = True          # remove rm.* files
        self.mot_labs   = []            # labels for motion params

        self.verb       = 1             # verbosity level

        self.tr         = 0.0           # TR, in seconds
        self.reps       = 0             # TRs per run
        self.runs       = 0             # number of runs
        self.mask       = None          # mask dataset

        self.bindex     = 0             # current block index
        self.pblabel    = ''            # previous block label

        return
        
    def show(self, mesg):       # rcr - update
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
        self.valid_opts.add_opt('-dsets', -1, [])

        self.valid_opts.add_opt('-out_dir', 1, [])
        self.valid_opts.add_opt('-scr_overwrite', 0, [])
        self.valid_opts.add_opt('-script', 1, [])
        self.valid_opts.add_opt('-subj_id', -1, [])

        self.valid_opts.add_opt('-ask_me', 0, [])       # QnA session
        self.valid_opts.add_opt('-copy_anat', 1, [])
        self.valid_opts.add_opt('-keep_rm_files', 0, [])
        # self.valid_opts.add_opt('-remove_pXX_files', 0, [])

        # block options
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [])

        self.valid_opts.add_opt('-tshift_align_to', -1, [])
        self.valid_opts.add_opt('-tshift_interp', 1, [])
        self.valid_opts.add_opt('-tshift_opts_ts', -1, [])

        self.valid_opts.add_opt('-volreg_align_to', 1, [], ['first','last'])
        self.valid_opts.add_opt('-volreg_base_ind', 2, [])
        self.valid_opts.add_opt('-volreg_opts_vr', -1, [])

        self.valid_opts.add_opt('-blur_filter', 1, [])
        self.valid_opts.add_opt('-blur_size', 1, [])
        self.valid_opts.add_opt('-blur_opts_merge', -1, [])

        self.valid_opts.add_opt('-mask_type', 1, [], ['union','intersection'])
        self.valid_opts.add_opt('-mask_dilate', 1, [])

        self.valid_opts.add_opt('-scale_max_val', 1, [])

        self.valid_opts.add_opt('-regress_basis', 1, [])
        self.valid_opts.add_opt('-regress_basis_normall', 1, [])
        self.valid_opts.add_opt('-regress_polort', 1, [])
        self.valid_opts.add_opt('-regress_stim_files', -1, [])
        self.valid_opts.add_opt('-regress_stim_labels', -1, [])
        self.valid_opts.add_opt('-regress_stim_times', -1, [])
        self.valid_opts.add_opt('-regress_no_stim_times', 0, [])
        self.valid_opts.add_opt('-regress_stim_times_offset', 1, [])

        self.valid_opts.add_opt('-regress_fitts_prefix', 1, [])
        self.valid_opts.add_opt('-regress_iresp_prefix', 1, [])
        self.valid_opts.add_opt('-regress_make_ideal_sum', 1, [])
        self.valid_opts.add_opt('-regress_no_fitts', 0, [])
        self.valid_opts.add_opt('-regress_no_ideals', 0, [])
        self.valid_opts.add_opt('-regress_no_iresp', 0, [])
        self.valid_opts.add_opt('-regress_opts_3dD', -1, [])


        # other options
        self.valid_opts.add_opt('-help', 0, [])
        self.valid_opts.add_opt('-hist', 0, [])
        self.valid_opts.add_opt('-ver', 0, [])
        self.valid_opts.add_opt('-verb', 1, [])

        self.valid_opts.trailers = False   # do not allow unknown options
        
    def get_user_opts(self):
        self.user_opts = read_options(sys.argv, self.valid_opts)
        if self.user_opts == None: return 1     # error condition
        if len(self.user_opts.olist) == 0:      # no options: apply -help
            print g_help_string
            return 1
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt: print "** seem to have trailers, but cannot find them!"
            else: print "** have invalid trailing args: %s", opt.show()
            return 1  # failure

        opt = self.user_opts.find_opt('-verb')    # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        opt = self.user_opts.find_opt('-help')    # does the user want help?
        if opt != None:
            print g_help_string
            return 1  # terminate
        
        opt = self.user_opts.find_opt('-hist')    # print the history
        if opt != None:
            print g_history
            return 1  # terminate
        
        opt = self.user_opts.find_opt('-ver')    # show the version string
        if opt != None:
            print g_version
            return 1  # terminate
        
        opt = self.user_opts.find_opt('-subj_id')
        if opt != None: self.subj_id = opt.parlist[0]

        # get datasets
        opt = self.user_opts.find_opt('-dsets')
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))

        opt = self.user_opts.find_opt('-out_dir')
        if opt != None: self.out_dir = opt.parlist[0]

        opt = self.user_opts.find_opt('-script')
        if opt != None: self.script = opt.parlist[0]

        opt = self.user_opts.find_opt('-scr_overwrite')
        if opt != None: self.overwrite = True

        opt = self.user_opts.find_opt('-copy_anat')
        if opt != None: self.anat = afni_name(opt.parlist[0])

        opt = self.user_opts.find_opt('-keep_rm_files')
        if opt != None: self.rm_rm = False

        # done checking options

        # update out_dir now (may combine option results)
        if self.out_dir == '': self.out_dir = '%s.results' % self.subj_label

        if self.verb > 3: self.show('end get_user_opts ')

    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        blocklist = BlockLabels
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

        # rcr - if there is another place to update options from, do it
        uopt = self.user_opts.find_opt('-opt_source')
        if uopt != None:
            print '** not ready to update options from %s' % str(uopt.parlist)
            return 1

        # do some final error checking
        if len(self.dsets) == 0:
            print 'error: dsets have not been specified (consider -dsets)'
            return 1
        if not uniq_list_as_dsets(self.dsets, True):
            return 1

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
                self.fp.write(cmd_str)
                if self.verb>2: block.show('+d post command creation: ')
                if self.verb>1: print '+d %s command: %s'%(block.label, cmd_str)

        rv = self.finalize_script()     # finish the script
        if rv: errs += 1

        if self.fp: self.fp.close()

        if errs > 0: return 1    # so we print all errors before leaving

        if self.verb > 0:
            print "\n--> script is file: %s" % self.script
            print '    (consider the command "tcsh -x %s |& tee output.%s") '% \
                  (self.script, self.script)

        return

    def get_run_info(self):
        self.runs = len(self.dsets)
        if self.runs < 1:
            print "** have no dsets to analyze"
            return 1

        # updated by 'tcat' opteration (and -remove_trs option)
        dset = self.dsets[0].rpv()
        list = read_attribute(dset, 'TAXIS_NUMS')
        if list == None: return 1
        try: self.reps = int(list[0])
        except:
            print "** reps '%s' is not an int?" % list[0]
            return 1
        if self.reps < 1:
            print "** invalid nreps (%d) for dset %s" % (self.reps, dset)
            return 1

        list = read_attribute(dset, 'TAXIS_FLOATS')
        if list == None: return 1
        try: self.tr = float(list[1])
        except:
            print "** TR '%s' is not an int?" % list[0]
            return 1

        if self.verb > 1: print '(reps, runs, tr) = (%d, %d, %f)' %  \
                                 (self.reps, self.runs, self.tr)

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
        block = find_block(label)
        if block: return self.blocks.index(block)
        return None

    # set subj shell variable, check output dir, create and cd
    def init_script(self):
        if not self.overwrite and os.path.isfile(self.script):
            print "error: script file '%s' already exists, exiting..." % \
                  self.script
            return 1
        try: self.fp = open(self.script,'w')
        except:
            print "cannot open script file '%s' for writing" % self.script
            return 1

        self.fp.write('#!/usr/bin/env tcsh\n\n')
        self.fp.write('# auto-generated by afni_proc.py, %s\n\n' % asctime())
        self.fp.write('# --------------------------------------------------\n'
                      '# script setup\n\n'
                      '# the user may specify a single subject to run with\n')
        self.fp.write('if ( $#argv > 0 ) then  \n'
                      '    set subj = $argv[0] \n'
                      'else                    \n'
                      '    set subj = %s       \n'
                      'endif\n\n' % self.subj_id )
        self.fp.write('# verify that the results directory does not yet exist\n'
                      'if ( -d %s ) then \n'
                      '    echo output dir "$subj.results" already exists\n'
                      '    exit                     \n'
                      'endif\n\n' % self.out_dir)
        self.fp.write('# set list of runs\n')
        self.fp.write('set runs = (`count -digits 2 1 %d`)\n\n' % self.runs)

        self.fp.write('# create results directory\n')
        self.fp.write('mkdir %s\n\n' % self.out_dir)
        
        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
            self.fp.write('# create stimuli directory, and copy stim files\n')
            self.fp.write('mkdir %s/stimuli\ncp ' % self.out_dir)
            for ind in range(len(self.stims)):
                self.fp.write('%s ' % self.stims_orig[ind])
            self.fp.write('%s/stimuli\n\n' % self.out_dir)

        if self.anat:
            self.fp.write('# copy anatomy to results dir\n')
            self.fp.write('3dcopy %s %s/%s\n\n' %
                          (self.anat.rpv(), self.out_dir, self.anat.prefix))

        self.fp.flush()

    # and last steps
    def finalize_script(self):
        if self.rm_rm:
            self.fp.write('# remove temporary rm.* files\n'
                          '\\rm -f rm.*\n\n')
        self.fp.write('# return to parent directory\n'
                      'cd ..\n\n')

        opt = self.user_opts.find_opt('-no_proc_command')
        self.fp.write('\n\n\n'
                  '# -------------------------------------------------------\n'
                  '# script generated by the command:\n'
                  '#\n'
                  '# %s %s\n\n' % (os.path.basename(sys.argv[0]),
                                   ' '.join(quotize_list(sys.argv[1:]))))

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

class ProcessBlock:
    def __init__(self, label, proc):
        self.label = label      # label is block type
        self.valid = False      # block is not yet valid
        self.apply = True       # apply block to output script
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
    if rv != None: return rv

    rv = ps.create_blocks()
    if rv != None: return rv

    rv = ps.create_script()
    if rv != None: return rv

# main
if __name__ == '__main__':

    run_proc()

