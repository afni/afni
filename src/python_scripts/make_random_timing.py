#!/usr/bin/env python

import sys, random, os, math
import option_list, afni_util as UTIL

g_help_string = """
===========================================================================
Create random stimulus timing files.

    The object is to create a set of random stimulus timing files, suitable
    for use in 3dDeconvolve.  These times will not be TR-locked (unless the
    user requests it).  Stimulus presentation times will never overlap, though
    their responses can.

    This can easily be used to generate many sets of random timing files to
    test via "3dDeconvolve -nodata", in order to determine good timing, akin
    to what is done in HowTo #3 using RSFgen.  Note that the -save_3dd_cmd
    can be used to create a sample "3dDeconvolve -nodata" script.

    given:
        num_stim        - number of stimulus classes
        num_runs        - number of runs
        num_reps        - number of repetitions for each class (same each run)
        stim_dur        - length of time for each stimulus, in seconds
        run_time        - total amount of time, per run
        pre_stim_rest   - time before any first stimulus (same each run)
        post_stim_rest  - time after last stimulus (same each run)

    This program will create one timing file per stimulus class, num_runs lines
    long, with num_stim stimulus times per line.

    Time for rest will be run_time minus all stimulus time, and can be broken
    into pre_stim_rest, post_stim_rest and randomly distributed rest.  Consider
    the sum, assuming num_reps and stim_dur are constant (per run and stimulus
    class).

          num_stim * num_reps * stim_dur  (total stimulus duration for one run)
        + randomly distributed rest       (surrounding stimuli)
        + pre_stim_rest
        + post_stim_rest                  (note: account for response time)
        -----------
        = run_time

    Other controlling inputs include:

        across_runs - distribute num_reps across all runs, not per run
        min_rest    - time of rest to immediately follow each stimulus
                      (this is internally added to stim_dur)
        seed        - optional random number seed
        t_gran      - granularity of time, in seconds (default 0.1 seconds)
        tr_locked   - make all timing locked with the accompanying TR

    The internal method used is similar to that of RSFgen.  For a given run, a
    list of num_reps stimulus intervals for each stimulus class is generated
    (each interval is stim_dur seconds).  Appended to this is a list of rest
    intervals (each of length t_gran seconds).  This accounts for all time
    except for pre_stim_rest and post_stim_rest.

    This list (of numbers 0..num_stim, where 0 means rest) is then randomized.
    Timing comes from the result.

    Reading the list (still for a single run), times are accumulated, starting
    with pre_stim_rest seconds.  As the list is read, a 0 means add t_gran
    seconds to the current time.  A non-zero value means the given stimulus
    type occurred, so the current time goes into that stimulus file and the
    time is incremented by stim_dur seconds.

  * Note that stimulus times will never overlap, though response times can.

  * The following options can be specified as one value or as a list:

        -run_time       : time for each run, or a list of run times
        -stim_dur       : duration of all stimuli, or a list of every duration
        -num_reps       : nreps for all stimuli, or a list of nreps for each

    Note that varying these parameters can lead to unbalanced designs.  Use
    the list forms with caution.

    Currently, -pre_stim_rest and -post_stim_rest cannot vary over runs.

----------------------------------------
getting TR-locked timing

    If TR-locked timing is desired, it can be enforced with the -tr_locked
    option, along with which the user must specify "-tr TR".  The effect is
    to force stim_dur and t_gran to be equal to (or a multiple of) the TR.

    It is illegal to use both -tr_locked and -t_gran (since -tr is used to
    set t_gran).

----------------------------------------
distributing stimuli across all runs at once (via -across_runs)

    The main described use is where there is a fixed number of stimlus events 
    in each run, and of each type.  The -num_reps option specifies that number
    (or those numbers).  For example, if -num_reps is 8 and -num_runs is 4,
    each stimulus class would have 8 repetitions in each of the 4 runs (for a
    total of 32 repetitions).

    That changes if -across_runs is applied.

    With the addition of the -across_runs option, the meaning of -num_reps
    changes to be the total number of repetitions for each class across all
    runs, and the randomization changes to occur across all runs.  So in the
    above example, with -num_reps equal to 8, 8 stimluli (of each class) will
    be distributed across 4 runs.  The average number of repetitions per run
    would be 2.

    In such a case, note that it would be possible for some runs not to have
    any stimuli of a certain type.

----------------------------------------------------------------------
examples:

    1. Create a timing file for a single stimulus class for a single run.
       The run will be 100 seconds long, with (at least) 10 seconds before
       the first stimulus.  The stimulus will occur 20 times, and each lasts
       1.5 seconds.

       The output will be written to 'stimesA_01.1D'.

            make_random_timing.py -num_stim 1 -num_runs 1 -run_time 100  \\
                -stim_dur 1.5 -num_reps 20 -pre_stim_rest 10 -prefix stimesA

    2. A typical example.

       Make timing files for 3 stim classes over 4 runs of 200 seconds.  Every
       stimulus class will have 8 events per run, each lasting 3.5 seconds.
       Require 20 seconds of rest before the first stimulus in each run, as
       well as after the last.

       Also, add labels for the 3 stimulus classes: houses, faces, donuts.
       They will be appended to the respective filenames.  And finally, display
       timing statistics for the user.

       The output will be written to stimesB_01.houses.1D, etc.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_dur 3.5 -num_reps 8 -prefix stimesB                \\
                -pre_stim_rest 20 -post_stim_rest 20                     \\
                -stim_labels houses faces donuts                         \\
                -show_timing_stats

       Consider adding the -save_3dd_cmd option.

    3. Distribute stimuli over all runs at once.

       Similar to #2, but distribute the 8 events per class over all 4 runs.
       In #2, each stim class has 8 events per run (so 32 total events).
       Here each stim class has a total of 8 events.  Just add -across_runs.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_dur 3.5 -num_reps 8 -prefix stimesC                \\
                -pre_stim_rest 20 -post_stim_rest 20                     \\
                -across_runs -stim_labels houses faces donuts

    4. TR-locked example.

       Similar to #2, but make the stimuli TR-locked.  Set the TR to 2.0
       seconds, along with the length of each stimulus event.  This adds
       options -tr_locked and -tr, and requires -stim_dur to be a multiple
       (or equal to) the TR.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_dur 2.0 -num_reps 8 -prefix stimesD                \\
                -pre_stim_rest 20 -post_stim_rest 20 -tr_locked -tr 2.0

    5. Esoteric example.

       Similar to #2, but require an additional 0.7 seconds of rest after
       each stimulus (exactly the same as adding 0.7 to the stim_dur), set
       the granularity of random sequencing to 0.001 seconds, apply a random
       number seed of 17, and set the verbose level to 2.

       Save a 3dDeconvolve -nodata command in @cmd.3dd .
       
            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_dur 3.5 -num_reps 8 -prefix stimesE                \\
                -pre_stim_rest 20 -post_stim_rest 20                     \\
                -min_rest 0.7 -t_gran 0.001 -seed 17 -verb 2             \\
                -show_timing_stats -save_3dd_cmd @cmd.3dd

    6. Example with varying number of events.

    ** Note that this does not make for a balanced design.

       Similar to #2, but require each stimulus class to have a different
       number of events.  Class #1 will have 8 reps per run, class #2 will
       have 10 reps per run and class #3 will have 15 reps per run.  The
       -num_reps option takes either 1 or -num_stim parameters.  Here, 3
       are supplied.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_dur 3.5 -num_reps 8 10 15 -prefix stimesF          \\
                -pre_stim_rest 20 -post_stim_rest 20

    7. Catch trials.

       If every time a main stimulus 'M' is presented it must follow another
       stimulus 'C', catch trials can be used to separate them.  If the TRs
       look like ...CM.CM.....CM...CMCM, it is hard to separate the response
       to M from the response to C.  When separate C stimuli are also given,
       the problem becomes simple : C..CM.CM...C.CM...CMCM.  Now C and M can
       be measured separately.

       In this example we have 4 8-second main classes (A1, A2, B1, B2) that
       always follow 2 types of 8-second catch classes (A and B).  The times
       of A1 are always 8 seconds after the times for A, for example.

       Main stimuli are presented 5 times per run, and catch trials are given
       separately an additional 4 times per run.  That means, for example, that
       stimulus A will occur 14 times per run (4 as 'catch', 5 preceeding A1,
       5 preceeding A2).  Each of 3 runs will last 9 minutes.

       Initially we will claim that A1..B2 each lasts 16 seconds.  Then each of
       those events will be broken into a 'catch' event at the beginning, 
       followed by a 'main' event after another 8 seconds.  Set the minumum
       time between any 2 events to be 1.5 seconds.

       Do this in 3 steps:

          a. Generate stimulus timing for 6 classes: A, B, A1, A2, B1, B2.
             Stim lengths will be 8, 8, and 16, 16, 16, 16 seconds, at first.
             Note that both the stimulus durations and frequencies will vary.

               make_random_timing.py -num_stim 6 -num_runs 3 -run_time 540  \\
                   -stim_dur 8 8 16 16 16 16 -num_reps 4 4 5 5 5 5          \\
                   -stim_labels A B A1 A2 B1 B2 -min_rest 1.5 -seed 54321   \\
                   -prefix stimesG 

          b. Separate 'catch' trials from main events.  Catch trails for A will
             occur at the exact stim times of A1 and A2.  Therefore all of our
             time for A/A1/A2 are actually times for A (and similarly for B).
             Concatenate the timing files and save them.

                1dcat stimesG_??_A.1D stimesG_??_A?.1D > stimesG_A_all.1D
                1dcat stimesG_??_B.1D stimesG_??_B?.1D > stimesG_B_all.1D

          c. To get stim times for the 'main' regressors we need to add 8
             seconds to every time.  Otherwise, the times will be identical to
             those in stimesG.a_03_A?.1D (and B).

             There are many ways to add 8 to the timing files.  In this case,
             just run the program again, with the same seed, but add an offset
             of 8 seconds to all times.  Then simply ignore the new files for
             A and B, while keeping those of A1, A2, B1 and B2.

             Also, save the 3dDeconvolve command to run with -nodata.

               make_random_timing.py -num_stim 6 -num_runs 3 -run_time 540  \\
                   -stim_dur 8 8 16 16 16 16 -num_reps 4 4 5 5 5 5          \\
                   -stim_labels A B A1 A2 B1 B2 -min_rest 1.5 -seed 54321   \\
                   -offset 8.0 -save_3dd_cmd @cmd.3dd.G -prefix stimesG 

          d. Finally, fix the 3dDeconvolve command in @cmd.3dd.G.

             1. Use the timing files stimesG_A_all.1D and stimesG_B_all.1D from
                step b, replacing files stimesG_01_A.1D and stimesG_01_B.1D.

             2. Update the stimulus durations of A1, A2, B1 and B2 from 16
                seconds to the correct 8 seconds (the second half of the 16
                second intervals).

             This is necessary because the command in step (c) does not know
             about the updated A/B files from step (b).  The first half of each
             16 second A1/A2 stimulus is actually stimulus A, while the second
             half is really A1 or A2.  Similarly for B.
             
        
       The resulting files are kept (and applied in and 3dDeconvolve commands):

            stimesG_[AB]_all.1D    : the 'catch' regressors, 14 stimuli per run
                                     (from step b)
            stimesG_*_[AB][12].1D  : the 4 main regressors (at 8 sec offsets)
                                     (from step c)

       --- end of (long) example #7 ---

----------------------------------------------------------------------
informational arguments:

    -help                       : display this help
    -hist                       : display the modification history
    -show_valid_opts            : display all valid options (short format)
    -ver                        : display the version number

----------------------------------------
required arguments:

    -num_runs  NRUNS            : set the number of runs

        e.g. -num_runs 4

        Use this option to specify the total number of runs.  Output timing
        files will have one row per run (for -local_times in 3dDeconvolve).

    -run_time  TIME             : set the total time, per run (in seconds)

        e.g. -run_time 180
        e.g. -run_time 180 150 150 180

        This option specifies the total amount of time per run, in seconds.
        This time includes all rest and stimulation.  This time is per run,
        even if -across_runs is used.

    -num_stim  NSTIM            : set the number of stimulus classes

        e.g. -num_stim 3

        This specifies the number of stimulus classes.  The program will
        create one output file per stimulus class.

    -num_reps  REPS             : set the number of repetitions (per class?)

        e.g. -num_reps 8
        e.g. -num_reps 8 15 6

        This specifies the number of repetitions of each stimulus type, per run
        (unless -across_runs is used).  If one parameter is provided, every
        stimulus class will be given that number of repetitions per run (unless
        -across_runs is given, in which case each stimulus class will be given
        a total of that number of repetitions, across all runs).

        The user can also specify the number of repetitions for each of the
        stimulus classes separatly, as a list.

            see also: -across_runs

    -prefix    PREFIX           : set the prefix for output filenames

        e.g. -prefix stim_times

                --> might create: stim_times_001.1D

        The option specifies the prefix for all output stimulus timing files.
        The files will have the form: PREFIX_INDEX[_LABEL].1D, where PREFIX
        is via this option, INDEX is 01, 02, ... through the number of stim
        classes, and LABEL is optionally provided via -stim_labels.

        Therefore, output files will be sorted alphabetically, regardless of
        any labels, in the order that they are given to this program.

            see also -stim_labels

    -show_timing_stats          : show statistics from the timing

        e.g. -show_timing_stats

        If this option is set, the program will output statistical information
        regarding the stimulus timing, and on ISIs (inter-stimulus intervals)
        in particular.  One might want to be able to state what the min, mean,
        max and stdev of the ISI are.

    -stim_dur TIME              : set the duration for a single stimulus

        e.g. -stim_dur 3.5
        e.g. -stim_dur 3.5 1.0 4.2

        This specifies the length of time taken for a single stimulus, in
        seconds.  These stimulation intervals never overlap (with either rest
        or other stimulus intervals) in the output timing files.

        If a single TIME parameter is given, it applies to all of the stimulus
        classes.  Otherwise, the user can provide a list of durations, one per
        stimulus class.

----------------------------------------
optional arguments:

    -across_runs                : distribute stimuli across all runs at once

        e.g. -across_runs

        By default, each of -num_stim stimuli are randomly distributed within
        each run separately, per class.  But with the -across_runs option,
        these stimuli are distributed across all runs at once (so the number
        of repetitions per run will vary).

        For example, using -num_stim 2, -num_reps 24 and -num_runs 3, assuming
        -across_runs is _not_used, there would be 24 repetitions of each stim
        class per run (for a total of 72 repetitions over 3 runs).  However, if
        -across_runs is applied, then there will be only the 24 repetitions
        over 3 runs, for an average of 8 per run (though there will probably
        not be exactly 8 in every run).

    -min_rest REST_TIME         : specify extra rest after each stimulus

        e.g. -min_rest 0.320

                --> would add 320 milliseconds of rest after each stimulus

        There is no difference between applying this option and instead
        adding the REST_TIME to that of each regressor.  It is merely another
        way to partition the stimulus time period.

        For example, if each stimulus lasts 1.5 seconds, but it is required
        that at least 0.5 seconds separates each stimulus pair, then there
        are 2 equivalent ways to express this:

            A: -stim_dur 2.0
            B: -stim_dur 1.5 -min_rest 0.5

        These have the same effect, but perhaps the user wants to keep the
        terms logically separate.

        However the program simply adds min_rest to each stimulus length.

    -offset OFFSET              : specify an offset to add to every stim time

        e.g. -offset 4.5

        Use this option to offset every stimulus time by OFFSET seconds.

    -pre_stim_rest REST_TIME    : specify minimum rest period to start each run

        e.g. -pre_stim_rest 20

        Use this option to specify the amount of time that should pass at
        the beginning of each run before the first stimulus might occur.
        The random placing of stimuli and rest will occur after this time in
        each run.

        As usual, the time is in seconds.

    -post_stim_rest REST_TIME   : specify minimum rest period to end each run

        e.g. -post_stim_rest 20

        Use this option to specify the amount of time that should pass at
        the end of each run after the last stimulus might occur.

        One could consider using -post_stim_rest of 12.0, always, to account
        for the decay of the BOLD response after the last stimulus period ends.

        Note that the program does just prevent a stimulus from starting after
        this time, but the entire stimulation period (described by -stim_dur)
        will end before this post_stim_rest period begins.

        For example, if the user provides "-run_time 100", "-stim_dur 2.5"
        and "-post_stim_rest 15", then the latest a stimulus could possibly
        occur at is 82.5 seconds into a run.  This would allow 2.5 seconds for
        the stimulus, plus another 15 seconds for the post_stim_rest period.

    -save_3dd_cmd FILENAME      : save a 3dDeconvolve -nodata example

        e.g. -save_3dd_cmd sample.3dd.command

        Use this option to save an example of running "3dDeconvolve -nodata"
        with the newly created stim_times files.  The saved script includes
        creation of a SUM regressor (if more than one stimulus was given) and
        a suggestion of how to run 1dplot to view the regressors created from
        the timing files.

        The use of the SUM regressor is to get a feel for what the expected
        response might look at a voxel that response to all stimulus classes.
        If, for example, the SUM never goes to zero in the middle of a run,
        one might wonder whether it is possible to accurately separate each
        stimulus response from the baseline.

    -seed SEED                  : specify a seed for random number generation

        e.g. -seed 3141592

        This option allows the user to specify a seed for random number
        generation in the program.  The main reason to do so is to be able
        to duplicate results.

        By default, the seed is based on the current system time.

    -stim_labels LAB1 LAB2 ...  : specify labels for the stimulus classes

        e.g. -stim_labels houses faces donuts

        Via this option, one can specify labels to become part of the output
        filenames.  If the above example were used, along with -prefix stim,
        the first stimulus timing would be written to stim_01_houses.1D.

        The stimulus index (1-based) is always part of the filename, as that
        keeps the files alphabetical in the order that the stimuli were
        specified to the program.

        There must be exactly -num_stim labels provided.

    -t_digits DIGITS            : set the number of decimal places for times

        e.g. -t_digits 3

        Via this option one can control the number of places after the
        decimal that are used when writing the stimulus times to each output
        file.  

        The default is 1, printing times in tenths of a second.  But if a
        higher time granularity is requested via -t_gran, one might want
        more places after the decimal.

        Note that if a user-supplied -t_gran does not round to a tenth of a
        second, the default t_digits changes to 3, to be in milliseconds.

    -t_gran GRANULARITY         : set the time granularity

        e.g. -t_gran 0.001

        The default time granularity is 0.1 seconds, and rest timing is
        computed at that resolution.  This option can be applied to change
        the resolution.  There are good reasons to go either up or down.

        One might want to use 0.001 to obtain a temporal granularity of a
        millisecond, as times are often given at that resolution.

        Also, one might want to use the actual TR, such as 2.5 seconds, to
        ensure that rest and stimuli occur on the TR grid.  Note that such a
        use also requires -stim_dur to be a multiple of the TR.

    -tr TR                      : set the scanner TR

        e.g. -tr 2.5

        The TR is needed for the -tr_locked option (so that all times are
        multiples of the TR), and for the -save_3dd_cmd option (the TR must
        be given to 3dDeconvolve).

        see also: -save_3dd_cmd, -tr_locked

    -verb LEVEL                 : set the verbose level

        e.g. -verb 2

        The default level is 1, and 0 is consider 'quiet' mode, only reporting
        errors.  The maximum level is currently 4.


- R Reynolds  May 7, 2008               motivated by Ikuko Mukai
===========================================================================
"""

g_history = """
    make_random_timing.py history:

    0.1  May 07, 2008: initial release
    0.2  May 18, 2008:
         - changed -stim_time option to -stim_dur
         - added options -tr_locked, -tr and -save_3dd_cmd
    0.3  June 6, 2008: get_*_opt now returns error code
    0.4  Oct 27, 2008:
         - actually implemented -min_rest, which I apparently forgot to do
         - added -offsets option
    0.5  Oct 31, 2008:
         - added -show_timing_stats option
         - made small change that affects timing (old results will not match)
"""

g_version = "version 0.5, October 31, 2008"

gDEF_VERB       = 1      # default verbose level
gDEF_T_GRAN     = 0.1    # default time granularity, in seconds
gDEF_MIN_T_GRAN = 0.0001 # minimum time granularity, in seconds
gDEF_DEC_PLACES = 1      # decimal places, when printing time

class RandTiming:
    def __init__(self, label):
        # actual stimulus timing lists
        self.stimes     = []
        self.fnames     = []            # output filenames

        # general parameters
        self.label = label
        self.verb  = gDEF_VERB
        self.valid_opts = None          # OptionList
        self.user_opts  = None

        # required arguments
        self.num_stim   = 0             # number of stimulus classes
        self.num_runs   = 0             # number of runs
        self.prefix     = None          # prefix for output files
                                        # add .LABEL.INDEX.1D

        self.run_time   = []            # total time per run (seconds)
        self.num_reps   = []            # number of stimuli, per class (per run)
        self.stim_dur   = []            # time of single stimulus (seconds)
                                        #   - per stim class

        # optional arguments
        self.across_runs    = 0         # flag: stimuli span all runs
        self.pre_stim_rest  = 0         # seconds before first stim
        self.post_stim_rest = 0         # seconds after last stim
        self.min_rest = 0.0             # minimum rest after each stimulus
        self.offset   = 0.0             # offset for all stimulus times
        self.seed     = None            # random number seed
        self.t_gran   = gDEF_T_GRAN     # time granularity for rest
        self.t_digits = gDEF_DEC_PLACES # digits after decimal when showing time
        self.labels   = None            # labels to be applied to filenames

        self.tr_locked      = 0         # flag: require TR-locked timing
        self.tr             = 0.0       # TR, for use in output 3dDecon cmd
        self.control_breaks = 1         # flag: across runs, add max stim time
                                        # to post_stim_rest
        self.file_3dd_cmd   = None      # file for 3dD -nodata command

        # statistics
        self.show_timing_stats = 0      # do we show ISI statistics?
        self.isi            = []        # list of isi's, per run
        self.prerest        = []        # pre-stim rest, per run
        self.postrest       = []        # post-stim rest, per run

    def init_opts(self):
        global g_help_string
        self.valid_opts = option_list.OptionList('for input')

        # short, terminal arguments
        self.valid_opts.add_opt('-help', 0, [],      \
                        helpstr='display program help')
        self.valid_opts.add_opt('-hist', 0, [],      \
                        helpstr='display the modification history')
        self.valid_opts.add_opt('-show_valid_opts', 0, [], \
                        helpstr='display all valid options')
        self.valid_opts.add_opt('-ver', 0, [],       \
                        helpstr='display the current version number')

        # required arguments
        self.valid_opts.add_opt('-num_stim', 1, [], req=1,
                        helpstr='number of stimulus types')
        self.valid_opts.add_opt('-num_runs', 1, [], req=1,
                        helpstr='number of scanning runs')

        self.valid_opts.add_opt('-prefix', 1, [], req=1,
                        helpstr='prefix for output stimulus timing files')

        self.valid_opts.add_opt('-num_reps', -1, [], req=1,
                        helpstr='number of stimulus reps per run, per class')
        self.valid_opts.add_opt('-run_time', -1, [], req=1,
                        helpstr='total length of each run, in seconds')
        self.valid_opts.add_opt('-stim_dur', -1, [], req=1,
                        helpstr='length of each stimulus, in seconds')

        # optional arguments
        self.valid_opts.add_opt('-across_runs', 0, [],
                        helpstr='distribute stim reps across all runs')
        self.valid_opts.add_opt('-min_rest', 1, [],
                        helpstr='minimum rest time after each stimulus')
        self.valid_opts.add_opt('-offset', 1, [],
                        helpstr='offset to add to every stimulus time')
        self.valid_opts.add_opt('-pre_stim_rest', 1, [],
                        helpstr='time before first stimulus, in seconds')
        self.valid_opts.add_opt('-post_stim_rest', 1, [],
                        helpstr='time after last stimulus, in seconds')
        self.valid_opts.add_opt('-save_3dd_cmd', 1, [],
                        helpstr='file for "3dDeconvolve -nodata" command')
        self.valid_opts.add_opt('-seed', 1, [],
                        helpstr='seed for random number generation (integer)')
        self.valid_opts.add_opt('-show_timing_stats', 0, [],
                        helpstr='show statistics for inter-stimulus intervals)')
        self.valid_opts.add_opt('-stim_labels', -1, [],
                        helpstr='specify stimulus labels for filenames')
        self.valid_opts.add_opt('-t_digits', 1, [],
                        helpstr='digits after decimal, for printing times')
        self.valid_opts.add_opt('-t_gran', 1, [],
                        helpstr='time_granularity for rest, in seconds')
        self.valid_opts.add_opt('-tr', 1, [],
                        helpstr='specify TR for 3dDeconvolve command');
        self.valid_opts.add_opt('-tr_locked', 0, [],
                        helpstr='specify TR and enforce TR-locked timing');

        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr='verbose level (0=quiet, 1=default, ...)')


    def read_opts(self):
        """check for terminal arguments, then read the user options"""

        # ------------------------------------------------------------
        # first check for terminal arguments

        # if argv has only the program name, or user requests help, show it
        if len(sys.argv) <= 1 or '-help' in sys.argv:
            print g_help_string
            return 0

        # check for -ver and -hist, too
        if '-hist' in sys.argv:
            print g_history
            return 0

        if '-ver' in sys.argv:
            print g_version
            return 0

        # maybe the user wants to list valid options
        if '-show_valid_opts' in sys.argv:
            self.valid_opts.show('', 1)
            return 0

        # ------------------------------------------------------------
        # read all user options

        self.user_opts = option_list.read_options(sys.argv, self.valid_opts)
        if not self.user_opts: return 1         # error condition

        return None     # normal completion

    def process_opts(self):
        """apply each option, and turn list options with single params
           into complete lists (e.g. stim_dur is per stim class)"""

        # ----------------------------------------
        # set verb first
        self.verb, err = self.user_opts.get_type_opt(int, '-verb')
        if self.verb == None: self.verb = gDEF_VERB
        elif err: return 1

        # ----------------------------------------
        # required args - single parameter
        self.num_stim, err = self.user_opts.get_type_opt(int, '-num_stim')
        if self.num_stim == None or err: return 1

        self.num_runs, err = self.user_opts.get_type_opt(int, '-num_runs')
        if self.num_runs == None or err: return 1

        self.prefix, err = self.user_opts.get_string_opt('-prefix')
        if self.prefix == None or err: return 1

        if self.verb > 1:
            print '-- have %d stim and %d runs, prefix = %s' %  \
                  (self.num_stim, self.num_runs, self.prefix)

        # ----------------------------------------
        # required args - (possibly) multiple parameter

        # set num_reps list of length num_runs
        self.run_time, err = self.user_opts.get_type_list(float, '-run_time',
                                       self.num_runs, 'num_runs', self.verb)
        if self.run_time == None or err: return 1

        # set num_reps list of length num_runs
        self.num_reps, err = self.user_opts.get_type_list(int, '-num_reps',
                                      self.num_stim, 'num_stim', self.verb)
        if self.num_reps == None or err: return 1

        # set stim_dur list of length num_stim
        self.stim_dur, err = self.user_opts.get_type_list(float, '-stim_dur',
                                       self.num_stim, 'num_stim', self.verb)
        if self.stim_dur == None or err: return 1

        # ----------------------------------------
        # optional arguments (if failure, use default)

        # check for the 'across_runs' flag
        opt = self.user_opts.find_opt('-across_runs')
        if opt: self.across_runs = 1

        self.pre_stim_rest, err = self.user_opts.get_type_opt(float,
                                                              '-pre_stim_rest')
        if self.pre_stim_rest == None: self.pre_stim_rest = 0.0
        elif err: return 1

        self.post_stim_rest, err = self.user_opts.get_type_opt(float, \
                                                          '-post_stim_rest')
        if self.post_stim_rest == None: self.post_stim_rest = 0.0
        elif err: return 1

        self.min_rest, err = self.user_opts.get_type_opt(float,'-min_rest')
        if self.min_rest == None: self.min_rest = 0
        elif err: return 1

        self.offset, err = self.user_opts.get_type_opt(float,'-offset')
        if self.offset == None: self.offset = 0
        elif err: return 1

        self.seed, err = self.user_opts.get_type_opt(float,'-seed')
        if self.seed == None: self.seed = None
        elif err: return 1

        if self.verb > 1 or self.user_opts.find_opt('-show_timing_stats'):
            self.show_timing_stats = 1

        self.t_gran, err = self.user_opts.get_type_opt(float,'-t_gran')
        if err: return 1
        # check the result after -tr_locked

        self.tr, err = self.user_opts.get_type_opt(float,'-tr')
        if not self.tr: self.tr = 0.0
        elif err: return 1

        opt = self.user_opts.find_opt('-tr_locked')
        if opt: self.tr_locked = 1

        if self.tr_locked:  # set t_gran, and check stim_lengths
            if self.t_gran:
                print '** cannot use both -tr_locked and -t_gran'
                return 1
            if self.tr == 0.0:
                print '** -tr_locked option requires -tr'
                return 1
            self.t_gran = self.tr;
            if self.verb > 1:
                print '++ setting t_gran to TR, %0.1f s' % self.tr
            if not UTIL.vals_are_multiples(self.tr, self.stim_dur, digits=4):
                print '** want TR-locked, but stim durations are not' + \
                      ' multiples of TR %.3f\n' +                       \
                      '   duration(s): %s' % (self.tr, self.stim_dur)

        # if t_gran is still not set, apply the default
        if not self.t_gran: self.t_gran = gDEF_T_GRAN

        # t_digits must come after t_gran is set
        self.t_digits, err = self.user_opts.get_type_opt(float,'-t_digits')
        if self.t_digits == None:
            if self.t_gran == round(self.t_gran,1):
                self.t_digits = gDEF_DEC_PLACES
            else:
                self.t_digits = 3
        elif err: return 1

        self.labels, err = self.user_opts.get_string_list('-stim_labels')
        if self.labels and len(self.labels) != self.num_stim:
            print '** error: %d stim classes but %d labels: %s' \
                  % (self.num_stim, len(self.lables), self.labels)

        self.file_3dd_cmd, err = self.user_opts.get_string_opt('-save_3dd_cmd')
        if err: return 1

        if self.verb > 1:
            print '-- pre_stim_rest=%.1f, post_stim_rest=%.1f, seed=%s'     \
                  % (self.pre_stim_rest, self.post_stim_rest, self.seed)
            print '   min_rest=%.3f, offset=%.3f, t_gran=%.3f, t_digits=%d' \
                  % (self.min_rest, self.offset, self.t_gran, self.t_digits)
            if self.labels: print '   labels are: %s' % ', '.join(self.labels)

    def create_timing(self):
        """create stimulus timing files
            - for each run
                - compute random rest time as total run time minus stim time
                  (sum of reps * time, per class) minus pre and post stimulus
                  rest times
                - divide rest be gran to get number of rest trials
                - randomize: sequence of trials per class and rest
                - accumulate time: offset + gran (rest) or stim_dur
                - each class gets a list of times
            - write all timing lists to files """

        if self.verb > 1: print '\n++ creating timing...'

        if self.seed != None:
            if self.verb > 1: print '++ init random with seed %d' % self.seed
            random.seed(self.seed)

        # possibly get timing across all runs at once
        if self.across_runs:
            self.stimes = self.make_rand_timing(self.num_runs, self.run_time[0])

            if self.stimes == None: return 1
            if len(self.stimes) != self.num_stim or     \
                    len(self.stimes[0]) != self.num_runs:
                print '** make_rand_timing failure: bad list size'
                return 1

        # create a timing list for each run separately
        else:

            # init the 3D array: class by run by stim
            # each element (class) is a list (run) of lists (stim)
            # (for each run, append a stim_dur list to each class)
            self.stimes = [[] for i in range(self.num_stim)]

            for run in range(self.num_runs):
                # get timing for current run
                stim_list = self.make_rand_timing(1, self.run_time[run])

                # check for failure
                if stim_list == None:
                    print '** rand_timing failure for run %d' % (run+1)
                    return 1

                if len(stim_list) != self.num_stim:
                    print '** bad stim_list length %d' % len(stim_list)
                    return 1

                if len(stim_list[0]) != 1:
                    print '** bad stim_list[0] length %d' % len(stim_list[0])
                    return 1

                # add lists to stimes
                for index in range(self.num_stim):
                    self.stimes[index].append(stim_list[index][0])

        if self.verb > 2:
            print '--------- final list ---------'
            print self.stimes

        return None

    def set_filenames(self):
        """create a list of filenames for writing"""

        if self.prefix: prefix = self.prefix    # be sure we have a prefix
        else:           prefix = 'stim_times'

        self.fnames = []
        for sind in range(self.num_stim):
            if self.labels:
                fname = '%s_%02d_%s.1D' % (prefix, sind+1, self.labels[sind])
            else:
                fname = '%s_%02d.1D' % (prefix, sind+1)
            self.fnames.append(fname)

    def write_timing_files(self):
        """write timing from slist to files from the prefix"""

        if len(self.stimes) != self.num_stim or self.num_stim <= 0:
            print '** bad stim data for file write'
            return 1

        if len(self.fnames) != self.num_stim:
            print '** missing filenames for timing output'
            return 1

        # compute min and max stim times, for verbose output
        mint = max(self.run_time)
        maxt = 0
        for sind in range(self.num_stim):
            stim_all = self.stimes[sind]    # all times for this stim
            stim = sind+1                   # 1-based index

            # open file, write each row (run), and close
            fname = self.fnames[sind]
            fp = open(fname, 'w')
            if not fp:
                print "** failed to open timing file '%s'" % fname
                return 1

            if self.verb > 0: print 'writing timing file: %s' % fname

            for rind in range(self.num_runs):
                stims = stim_all[rind]
                if len(stims) == 0:      # empty run, use '*'
                    if rind == 0: fp.write('* *\n')
                    else:         fp.write('*\n')
                    continue

                # write out all times on one line
                for ttime in stims:
                    fp.write('%.*f ' % (self.t_digits, ttime))
                    if ttime < mint: mint = ttime
                    if ttime > maxt: maxt = ttime
                if rind == 0 and len(stims) == 1: fp.write('*')
                fp.write('\n')

            fp.close()

            # and add this filename to the list, in case we want it later

        if self.verb > 1:
            print 'min, max stim times are: %.1f, %.1f' % (mint, maxt)

        return None

    def make_3dd_cmd(self):
        """write sample usage of 3dDeconvolve -nodata to a file"""

        if not self.file_3dd_cmd:       return
        if os.path.isfile(self.file_3dd_cmd):
            print "** 3dD command file '%s' already exists, failing..." \
                        % self.file_3dd_cmd
            return

        if len(self.fnames) != self.num_stim:
            print '** fname list does not have expected length'
            return

        # set tr and nt for the command
        if self.tr != 0.0:      tr = self.tr
        elif self.t_gran > 1.0: tr = self.t_gran
        else:                   tr = 1.0

        nt = round(sum(self.run_time) / tr)

        cmd  = '# -------------------------------------------------------\n' \
               '# create 3dDeconvolve -nodata command\n\n'      \

        # separate the 3dDecon command, to apply wrappers
        c2   = '3dDeconvolve   \\\n'                            \
            +  '    -nodata %d %.1f    \\\n' % (nt, tr)         \
            +  '%s' % make_concat_from_times(self.run_time,tr)  \
            +  '    -num_stimts %d    \\\n' % self.num_stim

        for ind in range(len(self.fnames)):
            c2 += '    -stim_times %d %s %s    \\\n' %          \
                  (ind+1,self.fnames[ind],basis_from_time(self.stim_dur[ind]))
        c2 += '    -x1D X.xmat.1D\n\n'

        if len(self.run_time) > 1:
            c2 += '# compute sum\n'                                          \
                  "3dTstat -sum -prefix sum_ideal.1D X.xmat.1D'[%d..$]'\n\n" \
                    % (2*len(self.run_time))
            ynames = '-ynames SUM - sum_ideal.1D '
            c2 += "# consider: 1dplot -xlabel Time %sX.xmat.1D'[%d..$]'\n"   \
                    % (ynames, 2*len(self.run_time))
        else:
            c2 += "# consider: 1dplot -xlabel Time X.xmat.1D'[%d]'\n"        \
                    % (2*len(self.run_time))

        cmd += UTIL.add_line_wrappers(c2)

        if self.verb > 0:
            print "saving 3dD command to file '%s'...\n" % self.file_3dd_cmd
        if self.verb > 1:
            print cmd

        fp = open(self.file_3dd_cmd,"w")
        if not fp:
            print '** failed to open %s for writing 3dDecon cmd'        \
                  % self.file_3dd_cmd,
            return

        fp.write(cmd)
        fp.close()

    def disp_time_stats(self):
        """display statistics of ISIs (inter-stimulus intervals)"""

        if not self.isi:
            if self.verb: print '-- no ISI stats to show'
            return

        print '\nISI statistics :\n\n'                                  \
              'data              min      mean     max     stdev\n'     \
              '-----------     -------  -------  -------  -------'

        print 'pre-rest        %7.3f  %7.3f  %7.3f  %7.3f' %            \
              (min_mean_max_stdev(self.prerest))

        print 'post-rest       %7.3f  %7.3f  %7.3f  %7.3f\n' %          \
              (min_mean_max_stdev(self.postrest))

        for ind in range(len(self.isi)):
           m0, m1, m2, s = min_mean_max_stdev(self.isi[ind])
           print 'run #%d ISI      %7.3f  %7.3f  %7.3f  %7.3f' %        \
                 (ind, m0, m1, m2, s)

        allruns = []
        for run in self.isi: allruns.extend(run)

        print '\nall runs ISI    %7.3f  %7.3f  %7.3f  %7.3f\n' %        \
              (min_mean_max_stdev(allruns))

        if self.verb > 3:
            for ind in range(len(self.isi)):
                print '\nISI #%d  :  %s' % (ind, self.isi[ind])
                
        if self.verb > 3:
            for ind in range(len(self.isi)):
                self.isi[ind].sort()
                print '\nsorted ISI #%d  :  %s' % (ind, self.isi[ind])
                

    def make_rand_timing(self, nruns, run_time):
        """Create random timing for stimuli over all runs (or for a single run),
           where the random placement of stimuli is over all runs at once (so
           the number per run will probably vary).

           This can be called per run, or for all runs at once.

            nruns           : number of runs
            run_time        : total time, per run (in seconds)
            nstim           : number of stimulus classes
            reps_list       : number of repetitions per class (total over runs)
            sdur_list       : duration of each stimulus class
            min_rest        : minimum duration between any 2 stimuli
            offset          : time offset to add to each stimulus time
            tinitial        : initial rest time, per run
            tfinal          : final rest time, per run

            ctrl_breaks     : multi-runs: add max stim time to tfinal
            tgran           : granularity of time, to partition rest
            verb            : verbose level
        """

        nstim       = self.num_stim             # number of stimulus types
        reps_list   = self.num_reps             # num reps of each stim class
        sdur_list   = self.stim_dur             # duration per class
        tinitial    = self.pre_stim_rest
        tfinal      = self.post_stim_rest
        min_rest    = self.min_rest
        offset      = self.offset
        ctrl_breaks = self.control_breaks
        tgran       = self.t_gran
        verb        = self.verb

        if verb > 2:
            print '-- make_rand_timing: nruns = %d' % nruns
            print '   rtime, offset, tinit, tfinal = %.1f, %.1f, %.1f, %.1f' \
                      % (run_time, offset, tinitial, tfinal)
            print '   reps_list  = %s' % reps_list
            print '   sdur_list = %s' % sdur_list
            print '   tgran = %.3f, verb = %d' % (tgran, verb)

        # verify inputs
        if nruns <= 0:
            print '** make_rand_timing error: nruns = %d' % nruns
            return
        if run_time <= 0.0 or nstim <= 0 or tinitial < 0 or tfinal < 0:
            print '** bad rand_timing inputs: rtime, nstim, tinit, tfinal = '+ \
                  '%.1f, %d, %.1f, %.1f' % (run_time, nstim, tinitial, tfinal)
            return
        if tgran < gDEF_MIN_T_GRAN:
            print '** time granularity (%f) below minimum (%f)' %   \
                  (tgran, gDEF_MIN_T_GRAN)
        if not reps_list or not sdur_list or       \
           len(reps_list) != nstim or len(sdur_list) != nstim:
            print '** invalid rand_timing input lists: reps, stimes = %s %s '% \
                  (reps_list, sdur_list)
            return

        # steal a copy of sdur_list so that we can trash it with min_rest
        stim_durs = sdur_list[:]

        # if multi runs and ctrl_breaks, add max stim time (-tgran) to tfinal
        if nruns > 1 and ctrl_breaks:
            smax = max(sdur_list)
            if verb > 1:
                print '++ adding max stim (%.1f) to post_stim_rest' % smax
            tfinal += smax

        # compute total stim time across all runs together
        # min_rest is essentially added to each stimulus time
        stime = 0.0 
        for ind in range(nstim):
            if reps_list[ind] < 0 or stim_durs[ind] < 0:
                print '** invalid negative reps or stimes list entry in %s, %s'\
                      % (reps_list, stim_durs)
                return
            stime += reps_list[ind]*stim_durs[ind]

        # compute rest time across all runs together
        tot_rest  = nruns * run_time - stime
        isi_rtime = nruns * (run_time - tinitial - tfinal) - stime

        # account for min_rest, and apply with stim_durs
        if min_rest < 0.0: min_rest = 0.0
        rtime = isi_rtime - min_rest * sum(reps_list)
        nrest = int(rtime / tgran)

        if verb > 1 or self.show_timing_stats:
            print '\n++ total time = %.1f, (stim = %.1f, rest = %.1f)\n'     \
                  '   init rest = %.1f, end rest = %.1f\n'                   \
                  '   min_rest following stimuli = %.1f (= %.1f * %d)\n'     \
                  '   total ISI = %.1f, rand ISI = %.1f\n'                   \
                  '   rand ISI rest time = %d intervals of %.3f seconds\n' % \
                  (nruns*run_time, stime, tot_rest, tinitial, tfinal,
                  min_rest * sum(reps_list), min_rest, sum(reps_list),
                  isi_rtime, rtime, nrest, tgran)

        if rtime == 0: print '** warning, exactly no time remaining for rest...'
        elif rtime < 0:
            print '** required stim and rest time exceed run length, failing...'
            return
            
        # create a list of all events, repeated stimuli and rest
        elist = []
        for stim in range(nstim):
            elist += [(stim+1) for i in range(reps_list[stim])]
        elist.extend([0 for i in range(nrest)])

        if verb > 2: print '++ elist (len %d, sorted): %s' % (len(elist), elist)

        # random.shuffle(elist)
        shuffle(elist)  # local version

        if verb > 2:
            print '++ elist (len %d, random): %s' % (len(elist), elist)
            for stim in range(nstim):
                print '   number of type %d = %d' % (stim+1,elist.count(stim+1))

        # convert the event list into a list of stimulus lists
        slist = [[[] for i in range(nruns)] for j in range(nstim)]

        # keep some statistics
        s_isi = []                  # isi's, per run
        s_first = 1                 # first event, per run
        s_laststim = 0              # end of last stimulus (in current run)
        ctime = tinitial
        run = 0
        for event in elist:
            # maybe we are done with this run
            if ctime >= run_time - tfinal:
                # statistics
                s_first = 1                 # have first event in this run
                self.isi.append(s_isi)      # store previous list
                s_isi = []
                self.postrest.append(run_time - s_laststim)
                s_laststim = 0

                # normal work
                run += 1
                ctime = tinitial
                if run >= nruns:    # we've gone beyond our bounds
                    print '** failure!  total run time has been exeeded...'
                    return

            if not event:
                ctime += tgran                  # rest event: tgran seconds
            else:
                # statistics
                if s_first:                     # then first event in this run
                    self.prerest.append(ctime)
                    s_first = 0
                else:
                    s_isi.append(ctime - s_laststim)

                eind = event - 1                # event is one more than index
                atime = ctime+offset            # add any requested offset
                slist[eind][run].append(atime)  # append cur time to event list
                ctime += stim_durs[eind]        # increment time by this stim
                s_laststim = ctime              # store the stim end time
                ctime += min_rest               # and rest

        # and set end-of-run stats
        self.postrest.append(run_time - s_laststim)
        self.isi.append(s_isi)

        if verb > 3:
            for stim in range(nstim):
                print '++ stim list[%d] = %s' % (stim+1,slist[stim])

        return slist

def min_mean_max_stdev(data):
    """return 4 values for data: min, max, mean, stdev (unbiased)"""
    if not data: return 0,0,0,0
    length = len(data)
    if length <  1: return 0,0,0,0
    if length == 1: return data[0], data[0], data[0], 0.0

    minval  = min(data)
    maxval  = max(data)
    meanval = sum(data)/float(length)

    return minval, meanval, maxval, stdev_ub(data)

def stdev_ub(data):
    """unbiased standard deviation"""
    length = len(data)
    if length <  2: return 0.0

    meanval = sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    return math.sqrt((ssq - length*meanval*meanval)/(length-1.0))

def stdev(data):
    """(biased) standard deviation"""
    length = len(data)
    if length <  2: return 0.0

    meanval = sum(data)/float(length)
    # compute standard deviation
    ssq = 0.0
    for val in data: ssq += val*val
    return math.sqrt((ssq - length*meanval*meanval)/length)

def basis_from_time(stim_len):
    if stim_len > 1.0: return "'BLOCK(%.1f,1)'" % stim_len
    else: return 'GAM'

def make_concat_from_times(run_times, tr):
    if len(run_times) < 2: return ''
    str = "    -concat '1D: 0"
    cind = 0
    for ind in range(len(run_times)-1):
        cind += round(run_times[ind]/tr)
        str += ' %d' % cind
    return str + "' \\\n"

def shuffle(vlist):
    """mostly like RSFgen, but for each index, search for swap in [index,n]

       this makes each permutation equally likely

       -- random.shuffle() cannot produce all possibilities
       -- RSFgen style permute_array() does not produce results with equal
          likelihood"""

    # don't rely on randint, as that comes from 2.4

    size = len(vlist)
    newlist = [-1 for i in range(size)]
    remain = size       # remaining space (of -1's) in newlist

    for index in range(size):
        # find random index in [index,n] = index+rand[0,n-index]
        i2 = index + int((size-index)*random.random())

        if i2 != index:         # if we want a new location, swap
            val = vlist[i2]
            vlist[i2] = vlist[index]
            vlist[index] = val

def process():
    timing = RandTiming('make random timing')
    timing.init_opts()
    rv = timing.read_opts()
    if rv != None:      # 0 is okay, else error code
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command:")
        return rv

    rv = timing.process_opts()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command:")
        return rv

    rv = timing.create_timing()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command:")
        return rv

    rv = timing.set_filenames()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command:")
        return rv

    rv = timing.write_timing_files()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command:")
        return rv

    if timing.file_3dd_cmd: timing.make_3dd_cmd()  # ignore return value

    if timing.show_timing_stats: timing.disp_time_stats() # ignore return value

    return 0

if __name__ == "__main__":
    rv = process()
    sys.exit(rv)

