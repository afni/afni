#!/usr/bin/env python

import sys, random, os, math
import option_list as OL, afni_util as UTIL

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

    The main described use is where there is a fixed number of stimulus events 
    in each run, and of each type.  The -num_reps option specifies that number
    (or those numbers).  For example, if -num_reps is 8 and -num_runs is 4,
    each stimulus class would have 8 repetitions in each of the 4 runs (for a
    total of 32 repetitions).

    That changes if -across_runs is applied.

    With the addition of the -across_runs option, the meaning of -num_reps
    changes to be the total number of repetitions for each class across all
    runs, and the randomization changes to occur across all runs.  So in the
    above example, with -num_reps equal to 8, 8 stimuli (of each class) will
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
       In #2, each stim class has 8 events per run (so 24 total events).
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
       number seed of 31415, and set the verbose level to 2.

       Save a 3dDeconvolve -nodata command in @cmd.3dd .
       
            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_dur 3.5 -num_reps 8 -prefix stimesE                \\
                -pre_stim_rest 20 -post_stim_rest 20                     \\
                -min_rest 0.7 -max_rest 7.0                              \\
                -t_gran 0.001 -seed 31415 -verb 2                        \\
                -show_timing_stats -save_3dd_cmd @cmd.3dd

    6. Example with varying number of events, durations and run times.

    ** Note that this does not make for a balanced design.

       Similar to #2, but require each stimulus class to have a different
       number of events.  Class #1 will have 8 reps per run, class #2 will
       have 10 reps per run and class #3 will have 15 reps per run.  The
       -num_reps option takes either 1 or -num_stim parameters.  Here, 3
       are supplied.

            make_random_timing.py -num_stim 3 -num_runs 4       \\
                -run_time 200 190 185 225                       \\
                -stim_dur 3.5 4.5 3 -num_reps 8 10 15           \\
                -pre_stim_rest 20 -post_stim_rest 20            \\
                -prefix stimesF

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
       stimulus A will occur 14 times per run (4 as 'catch', 5 preceding A1,
       5 preceding A2).  Each of 3 runs will last 9 minutes.

       Initially we will claim that A1..B2 each lasts 16 seconds.  Then each of
       those events will be broken into a 'catch' event at the beginning, 
       followed by a 'main' event after another 8 seconds.  Set the minimum
       time between any 2 events to be 1.5 seconds.

       Do this in 4 steps:

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

             Perhaps consider sorting the stimulus times per run, since the
             1dcat command does not do that.  Use timing_tool.py.  The new
             'sorted' timing files would replace the 'all' timing files.

                timing_tool.py -timing stimesG_A_all.1D -sort  \\
                               -write_timing stimesG_A_sorted.1D
                timing_tool.py -timing stimesG_B_all.1D -sort  \\
                               -write_timing stimesG_B_sorted.1D

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

             1. Use timing files stimesG_A_sorted.1D and stimesG_B_sorted.1D
                from step b, replacing stimesG_01_A.1D and stimesG_01_B.1D.

             2. Update the stimulus durations of A1, A2, B1 and B2 from 16
                seconds to the correct 8 seconds (the second half of the 16
                second intervals).

             This is necessary because the command in step (c) does not know
             about the updated A/B files from step (b).  The first half of each
             16 second A1/A2 stimulus is actually stimulus A, while the second
             half is really A1 or A2.  Similarly for B.
             
        
       The resulting files are kept (and applied in and 3dDeconvolve commands):

            stimesG_[AB]_sorted.1D : the (sorted) 'catch' regressors,
                                     14 stimuli per run (from step b)
            stimesG_*_[AB][12].1D  : the 4 main regressors (at 8 sec offsets)
                                     (from step c)

       --- end of (long) example #7 ---

    8. Example requiring partially fixed stimulus ordering.

       Suppose we have 2 sets of stimuli, question/answer/score along with
       face/doughnut.  Anytime a question is given it is followed by an answer
       (after random rest) and then a score (after random rest).  The face and
       doughnut stimuli are random, but cannot interrupt the q/a/s triples.

       Effectively, this means question, face and doughnut are random, but
       answer and score must always follow question.  Rest should be randomly
       distributed anywhere.

       The q/a/s stimuli are each 1.5 seconds, but since we require a minimum
       of 1 second after 'q' and 'a', and 1.5 seconds after 's', those stimulus
       durations are given as 2.5, 2.5 and 3.0 seconds, respectively.  The
       'f' and 'd' stimuli are each 1 second.

       Each stimulus has 8 repetitions per run, over 4 240 second runs.  The
       first and last 20 seconds of each run will be left to rest.

         make_random_timing.py -num_runs 4 -run_time 240                \\
                 -num_stim 5 -num_reps 8                                \\
                 -stim_labels question answer score face doughnut       \\
                 -stim_dur 2.5 2.5 3 1 1                                \\
                 -ordered_stimuli question answer score                 \\
                 -pre_stim_rest 20 -post_stim_rest 20                   \\
                 -show_timing_stats -seed 31415 -prefix stimesH

       To verify the stimulus order, consider using timing_tool.py to convert
       timing files to an event list.  The corresponding command might be the
       following, output on a TR grid of 1.0 s.

         timing_tool.py -multi_timing stimesH*.1D                       \\
                -multi_timing_to_events events.stimesH.txt              \\
                -multi_stim_dur 2.5 2.5 3 1 1                           \\
                -tr 1.0 -min_frac 0.5 -per_run -run_len 240


    9. TR-locked example, fixed seed, limited consecutive events.

       Similar to #4, but restrict the number of consecutive events of each
       type to 2.

         make_random_timing.py -num_stim 3 -num_runs 2 -run_time 200     \\
                 -stim_dur 2.0 -num_reps 10 30 10 -prefix stimesI        \\
                 -pre_stim_rest 20 -post_stim_rest 20 -tr_locked -tr 2.0 \\
                 -max_consec 2

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
        stimulus classes separately, as a list.

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

    -make_3dd_contrasts         : add all pairwise contrasts to 3dDeconvolve

        This option is particularly useful if make_random_timing.py is part of
        an experiment design search script.  In any case, this option can be
        used to add all possible pairwise contrasts to the 3dDeonvolve command
        specified by -save_3dd_cmd.

        Options -save_3dd_cmd and -stim_labels are also required.

    -max_consec c1 c2 ... cn    : specify maximum consecutive stimuli per class

        e.g. A.  -max_consec 2
        e.g. B.  -max_consec 2 2 2 2
        e.g. C.  -max_consec 0 4 2 0

        This option is used to limit the number of consecutive events of one
        or more classes.

        Assuming 4 stimulus classes, examples A and B limit each event type
        to having at most 2 consecutive events of that type.  Example C shows
        limiting only the second and third stimulus classes to consecutive
        events of length 4 and 2, respectively.

        A limit of 0 means no limit (num_reps, effectively).

    -max_rest REST_TIME         : specify maximum rest between stimuli

        e.g. -max_rest 7.25

        This option applies a second phase in ordering events.  After events
        have been randomized, non-pre- and non-post-stim rest periods are
        limited to the max_rest duration.  Any rest intervals exceeding this
        duration are distributed randomly into intervals below this maximum.

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

    -ordered_stimuli STIM1 STIM2 ... : specify a partial ordering of stimuli

        e.g. -ordered_stimuli primer choice reward
        e.g. -ordered_stimuli 4 2 5
        e.g. -ordered_stimuli stimA replyA -ordered stimuli stimB replyB
        e.g. -ordered_stimuli 1 2 -ordered_stimuli 3 4 -ordered_stimuli 5 6

        This option is used to require that some regressors are ordered.
        For example, every time a question stimulus occurs it is followed by a
        response stimulus, with only random rest in between.  There might be
        other stimuli, but they cannot break the question/response pair.

        So all the stimuli and rest periods are still random, except that given
        regressors must maintain the specified order.

        Given the first example, whenever primer occurs it is followed first
        by choice and then by reward.  Other stimuli might come before primer
        or after reward, but not in between.

        In the third example the stim/reply pairs are never broken, so stimA
        and replyA are always together, as are stimB and replyB.

        Note: - Multiple -ordered_stimuli options may be used.
              - A single stimulus may not appear in more than one such option.
              - Stimulus entries can be either labels (requiring -labels to be
                specified first) or 1-based indices, running from 1..N.

        See example 8 above.

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
        e.g. -t_digits -1

        Via this option one can control the number of places after the
        decimal that are used when writing the stimulus times to each output
        file.  The special value of -1 implies %g format.

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
    0.6  Dec 01, 2008:
         - moved min_mean_max_stdev to afni_util.py
         - modified examples to correspond with those in timing_tool.py
    0.7  Dec 24, 2008: redefine 'sum' for old python versions
    0.8  Feb 05, 2009: added timing_tool.py use to catch trial example
    0.9  Apr 17, 2009: added -ordered_stimuli
    0.10 Jun 17, 2009:
         - print block durations with general decimal accuracy
         - added -make_3dd_contrasts
    1.0  Jul 14, 2009: call this a release version
         - added -max_rest for Steffen S (see message board post 30189)
    1.1  May 01, 2010: added -max_consec for Liat of Cornell
    1.2  Aug 24, 2010: small updates for 3dDeconvolve -nodata script
         - update polort and write -nodata TR using 3 decimal places
    1.3  Mar 09, 2011: fixed bug writing comment text in 3dD script
         - noted by Z Saad and P Kaskan
    1.4  Jun 08, 2011: tr-lock test: fixed print and added min_rest to durs
    1.5  Apr 08, 2012: -ordered_stimuli can now use labels
    1.6  May 01, 2012: -ordered_stimuli now works with -max_consec
         - requested by Liat
"""

g_version = "version 1.6, May 1, 2012"

gDEF_VERB       = 1      # default verbose level
gDEF_T_GRAN     = 0.1    # default time granularity, in seconds
gDEF_MIN_T_GRAN = 0.0001 # minimum time granularity, in seconds
gDEF_DEC_PLACES = 1      # decimal places when printing time (-1 ==> %g format)

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
        self.max_rest = 0.0             # maximum rest after each stimulus
        self.min_rest = 0.0             # minimum rest after each stimulus
        self.offset   = 0.0             # offset for all stimulus times
        self.seed     = None            # random number seed
        self.orderstim= []              # list of ordered stimulus lists
        self.max_consec = []            # max consectutive stimuli per type
        self.t_gran   = gDEF_T_GRAN     # time granularity for rest
        self.t_digits = gDEF_DEC_PLACES # digits after decimal when showing time
                                        # (-1 means to use %g)
        self.labels   = None            # labels to be applied to filenames

        self.tr_locked      = 0         # flag: require TR-locked timing
        self.tr             = 0.0       # TR, for use in output 3dDecon cmd
        self.control_breaks = 1         # flag: across runs, add max stim time
                                        # to post_stim_rest
        self.file_3dd_cmd   = None      # file for 3dD -nodata command
        self.make_3dd_contr = 0         # flag to make all 3dD contrasts

        # statistics
        self.show_timing_stats = 0      # do we show ISI statistics?
        self.isi            = []        # list of isi's, per run
        self.prerest        = []        # pre-stim rest, per run
        self.postrest       = []        # post-stim rest, per run

    def init_opts(self):
        global g_help_string
        self.valid_opts = OL.OptionList('for input')

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
        self.valid_opts.add_opt('-make_3dd_contrasts', 0, [],
                        helpstr='add contrasts pairs to 3dDeconvolve script')
        self.valid_opts.add_opt('-max_consec', -1, [],
                        helpstr='max consecutive occurances of each stim type')
        self.valid_opts.add_opt('-max_rest', 1, [],
                        helpstr='maximum rest time after each stimulus')
        self.valid_opts.add_opt('-min_rest', 1, [],
                        helpstr='minimum rest time after each stimulus')
        self.valid_opts.add_opt('-offset', 1, [],
                        helpstr='offset to add to every stimulus time')
        self.valid_opts.add_opt('-ordered_stimuli', -1, [],
                        helpstr='require these stimuli to be so ordered')
        self.valid_opts.add_opt('-pre_stim_rest', 1, [],
                        helpstr='time before first stimulus, in seconds')
        self.valid_opts.add_opt('-post_stim_rest', 1, [],
                        helpstr='time after last stimulus, in seconds')
        self.valid_opts.add_opt('-save_3dd_cmd', 1, [],
                        helpstr='file for "3dDeconvolve -nodata" command')
        self.valid_opts.add_opt('-seed', 1, [],
                        helpstr='seed for random number generation (integer)')
        self.valid_opts.add_opt('-show_timing_stats', 0, [],
                        helpstr='show statistics for inter-stimulus intervals')
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

        # process any optlist_ options
        self.valid_opts.check_special_opts(sys.argv)

        # ------------------------------------------------------------
        # check for terminal arguments

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

        self.user_opts = OL.read_options(sys.argv, self.valid_opts)
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
                                    self.num_runs, 'num_runs')
        if self.run_time == None or err: return 1
        if self.verb > 1:
            print UTIL.gen_float_list_string(self.run_time, '-- run_time : ')

        # set num_reps list of length num_runs
        self.num_reps, err = self.user_opts.get_type_list(int, '-num_reps',
                                            self.num_stim, 'num_stim')
        if self.num_reps == None or err: return 1
        if self.verb > 1:
            print UTIL.int_list_string(self.num_reps, '-- num_reps : ')

        # set stim_dur list of length num_stim
        self.stim_dur, err = self.user_opts.get_type_list(float, '-stim_dur',
                                            self.num_stim, 'stim_dur')
        if self.stim_dur == None or err: return 1
        if self.verb > 1:
            print UTIL.gen_float_list_string(self.stim_dur, '-- stim_dur : ')

        # ----------------------------------------
        # optional arguments (if failure, use default)

        # check for the 'across_runs' flag
        opt = self.user_opts.find_opt('-across_runs')
        if opt: self.across_runs = 1

        opt = self.user_opts.find_opt('-make_3dd_contrasts')
        if opt: self.make_3dd_contr = 1

        self.pre_stim_rest, err = self.user_opts.get_type_opt(float,
                                                              '-pre_stim_rest')
        if self.pre_stim_rest == None: self.pre_stim_rest = 0.0
        elif err: return 1

        self.post_stim_rest, err = self.user_opts.get_type_opt(float, \
                                                          '-post_stim_rest')
        if self.post_stim_rest == None: self.post_stim_rest = 0.0
        elif err: return 1

        self.max_rest, err = self.user_opts.get_type_opt(float,'-max_rest')
        if self.max_rest == None: self.max_rest = 0
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

        # maybe there is a limit on the number of consecutive events per type
        opt = self.user_opts.find_opt('-max_consec')
        if opt:
           self.max_consec,err = self.user_opts.get_type_list(int,'-max_consec',
                         length=self.num_stim, len_name='num_stim', opt=opt)
           if err: return 1
           if self.max_consec and self.verb > 1:
               print UTIL.int_list_string(self.max_consec, '-- max_consec : ')

        # get any labels
        self.labels, err = self.user_opts.get_string_list('-stim_labels')
        if self.labels and len(self.labels) != self.num_stim:
            print '** error: %d stim classes but %d labels: %s' \
                  % (self.num_stim, len(self.labels), self.labels)
            return 1

        # gather a list of lists specified by -ordered_stimuli options
        olist = self.user_opts.find_all_opts('-ordered_stimuli')
        for opt in olist:
            # check for non-int: if labels require labels to be already set
            slist, err = self.user_opts.get_type_list(int, opt=opt, verb=0)
            if err:
                # see if they are labels
                slist, err = self.user_opts.get_string_list(opt=opt)
                if err:
                    print '** -ordered_stimuli: need list of ints or labels'
                    return 1
                # see if we have labels to convert into indices
                ilist, err = self.labels_to_indices(slist)
                if err:
                    print '** -ordered_stimuli requires indices or known labels'
                    return 1
                slist = ilist
            self.orderstim.append(slist)
            if self.verb>1: print UTIL.int_list_string(slist, '-- orderstim : ')

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
            # fixed print and added min_rest to durations   8 Jun, 2011
            sd = [dur + self.min_rest for dur in self.stim_dur]
            if not UTIL.vals_are_multiples(self.tr, sd, digits=4):
                print '** want TR-locked, but stim durations are not'  \
                      ' multiples of TR %.3f' % self.tr
                print '   min_rest + duration(s): %s' % sd
                return 1

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

        self.file_3dd_cmd, err = self.user_opts.get_string_opt('-save_3dd_cmd')
        if err: return 1

        if self.make_3dd_contr:
            if not self.file_3dd_cmd:
                print '** cannot use -make_3dd_contrasts without -save_3dd_cmd'
                return 1
            elif not self.labels:
                print '** cannot use -make_3dd_contrasts without -stim_labels'
                return 1

        if self.verb > 1:
            print '-- pre_stim_rest=%g, post_stim_rest=%g, seed=%s'     \
                  % (self.pre_stim_rest, self.post_stim_rest, self.seed)
            print '   min_rest=%g, max_rest=%g,'        \
                  % (self.min_rest, self.max_rest),     \
                  'offset=%g, t_gran=%g, t_digits=%d'   \
                  % (self.offset, self.t_gran, self.t_digits)
            if self.labels: print '   labels are: %s' % ', '.join(self.labels)

    def labels_to_indices(self, labels, print_err=1):
        """convert the labels list into an index list
              - entries must be in self.labels
           return index_list, err (0 = success)
        """
        if len(labels) == 0: return [], 0
        if len(self.labels) == 0:
           if print_err: print '** missing labels for conversion'
           return [], 1
        ilist = []
        for lab in labels:
           try: ind = self.labels.index(lab)
           except:
              print '** assumed label %s is not in label list' % lab
              return [], 1
           ilist.append(ind+1)
        return ilist, 0

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
            for s in range(len(self.stimes)):
                for r in range(len(self.stimes[0])):
                    print UTIL.gen_float_list_string(self.stimes[s][r],
                               '++ stim list[%d][%d] = '%((s+1),r))
            # print self.stimes

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
                    # if t_digits is not specified, use general format
                    if self.t_digits < 0:  fp.write('%.g ' % ttime)
                    else: fp.write('%.*f ' % (self.t_digits, ttime))

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

        nt = round(UTIL.loc_sum(self.run_time) / tr)

        cmd  = '# -------------------------------------------------------\n' \
               '# create 3dDeconvolve -nodata command\n\n'

        polort = UTIL.run_time_to_polort(self.run_time[0])
        # separate the 3dDecon command, to apply wrappers
        c2   = '3dDeconvolve   \\\n'                            \
            +  '    -nodata %d %.3f   \\\n' % (nt, tr)          \
            +  '    -polort %d        \\\n' % polort            \
            +  '%s' % make_concat_from_times(self.run_time,tr)  \
            +  '    -num_stimts %d    \\\n' % self.num_stim

        for ind in range(len(self.fnames)):
            c2 += '    -stim_times %d %s %s    \\\n' %                  \
                  (ind+1,self.fnames[ind],basis_from_time(self.stim_dur[ind]))
            # add labels, but play it safe
            if self.labels and len(self.labels) == len(self.fnames):
                c2 += '    -stim_label %d %s \\\n' % (ind+1,self.labels[ind])
        if self.make_3dd_contr and self.labels:
            c2 += self.make_3dd_contr_str(prefix='    ')
        c2 += '    -x1D X.xmat.1D\n\n'

        first = (polort+1) * len(self.run_time)
        if len(self.run_time) > 1:
            # note first non-poly baseline index
            c2 += '# compute the sum of non-baseline regressors\n'           \
                  "3dTstat -sum -prefix sum_ideal.1D X.xmat.1D'[%d..$]'\n\n" \
                  % first
            ynames = '-ynames SUM - sum_ideal.1D '
            c2 += "# consider plotting the SUM below non-polort regressors\n"\
                  "# command: 1dplot -xlabel Time %sX.xmat.1D'[%d..$]'\n"    \
                  % (ynames, first)
        else:
            c2 += "# consider plotting the SUM below non-polort regressors\n"\
                  "# command: 1dplot -xlabel Time X.xmat.1D'[%d]'\n" % first

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

    def make_3dd_contr_str(self, prefix=''):
        """return a string with all pairwise contrasts"""
        if not self.labels: return ''
        cstr = ''
        cind = 0
        llist = self.labels # just to make shorter
        for first in range(len(llist)-1):
            for next in range(first+1,len(llist)):
                cstr += "%s-gltsym 'SYM: %s -%s' -glt_label %d %s-%s \\\n" % \
                        (prefix, llist[first], llist[next],
                         cind+1, llist[first], llist[next])
                cind += 1

        if cind > 0:
            nstr = "%s-num_glt %d \\\n" % (prefix, cind)
            return nstr + cstr
        return ''

    def disp_time_stats(self):
        """display statistics of ISIs (inter-stimulus intervals)"""

        if not self.isi:
            if self.verb: print '-- no ISI stats to show'
            return

        print '\nISI statistics :\n\n'                                  \
              'data              min      mean     max     stdev\n'     \
              '-----------     -------  -------  -------  -------'

        print 'pre-rest        %7.3f  %7.3f  %7.3f  %7.3f' %            \
              (UTIL.min_mean_max_stdev(self.prerest))

        print 'post-rest       %7.3f  %7.3f  %7.3f  %7.3f\n' %          \
              (UTIL.min_mean_max_stdev(self.postrest))

        for ind in range(len(self.isi)):
           m0, m1, m2, s = UTIL.min_mean_max_stdev(self.isi[ind])
           print 'run #%d ISI      %7.3f  %7.3f  %7.3f  %7.3f' %        \
                 (ind, m0, m1, m2, s)

        allruns = []
        for run in self.isi: allruns.extend(run)

        print '\nall runs ISI    %7.3f  %7.3f  %7.3f  %7.3f\n' %        \
              (UTIL.min_mean_max_stdev(allruns))

        if self.verb > 3:
            for ind in range(len(self.isi)):
                print UTIL.gen_float_list_string(self.isi[ind],'ISI #%d : '%ind)
            print
                
        if self.verb > 3:
            for ind in range(len(self.isi)):
                self.isi[ind].sort()
                print UTIL.gen_float_list_string(self.isi[ind],
                           'sorted ISI #%d : '%ind)
                

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
            print '   rtime, offset, tinit, tfinal = %g, %g, %g, %g' \
                      % (run_time, offset, tinitial, tfinal)
            print '   reps_list  = %s' % reps_list
            print UTIL.gen_float_list_string(sdur_list,'   sdur_list = ')
            # print '   sdur_list = %s' % sdur_list
            print '   tgran = %.4f, verb = %d' % (tgran, verb)

        # verify inputs
        if nruns <= 0:
            print '** make_rand_timing error: nruns = %d' % nruns
            return
        if run_time <= 0.0 or nstim <= 0 or tinitial < 0 or tfinal < 0:
            print '** bad rand_timing inputs: rtime, nstim, tinit, tfinal = '+ \
                  '%g, %d, %g, %g' % (run_time, nstim, tinitial, tfinal)
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
        rtime = isi_rtime - min_rest * UTIL.loc_sum(reps_list)
        nrest = int(rtime / tgran)

        if verb > 1 or self.show_timing_stats:
            print '\n++ total time = %.1f, (stim = %.1f, rest = %.1f)\n'     \
                  '   init rest = %.1f, end rest = %.1f, min rest = %.1f\n'  \
                  '   total ISI = %.1f, rand ISI = %.1f\n'                   \
                  '   rand ISI rest time = %d intervals of %.3f seconds\n' % \
                  (nruns*run_time, stime, tot_rest, tinitial, tfinal, min_rest,
                  isi_rtime, rtime, nrest, tgran)

        if rtime == 0: print '** warning, exactly no time remaining for rest...'
        elif rtime < 0:
            print '** required stim and rest time exceed run length, failing...'
            return

        # make a random events list, based on reps_list[] and nrest
        err, elist = self.randomize_events(nrest)
        if err: return

        if self.max_rest > 0:
            if self.apply_max_rest(elist): return
            
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
            print '-- end make_random_timing() --'
            for s in range(nstim):
                for r in range(nruns):
                    print UTIL.gen_float_list_string(slist[s][r],
                               '++ stim list[%d][%d] = '%((s+1),r))

        return slist

    def valid_orderstim(self):
        """- every index in {1..num_stim} can occur only once over all
             orderstim lists
           - only values {1..num_stim} can occur in orderstim lists
           - for each olist, make sure num_reps is constant
       
           return 1 if valid, 0 if not"""

        if len(self.orderstim) < 1: return 1

        used  = []

        errs = 0
        for olist in self.orderstim:
            if self.verb > 4: print "-- testing olist: %s" % olist
            if len(olist) < 2:
                print "** orderstim too short: %s" % olist
                errs += 1
            for val in olist:
                if val < 1 or val > self.num_stim:
                    print "** orderstim list has values outside {1..%d}: %s" \
                          % (self.num_stim, olist)
                    errs += 1
                elif val in used:
                    print "** orderstim '%d' is not unique in lists" % val
                    errs += 1
                else: used.append(val)

            if errs: continue

            # and check that all reps are the same 
            vlist = [self.num_reps[v-1] for v in olist]
            if not UTIL.vals_are_constant(vlist, cval=None):
                print "** stimuli in order list need constant nreps\n"  \
                      "   orderstim: %s\n"                          \
                      "   nreps: %s\n" % (olist, vlist)
                errs += 1

        if errs:
            if self.verb > 1: print '** valid_orderstim: %d errors' % errs
            return 0
        else:
            if self.verb > 1:
                print '-- %d orderstim lists are valid' % len(self.orderstim)
            return 1

    def randomize_ordered_events(self, nrest):
        """create a list of randomized events, subject to orderstim

           - return error code (0=success) and event list
        """

        if len(self.orderstim) < 1:
            print '** no orderstim to apply'
            return 1, None
        if not self.valid_orderstim(): return 1, None

        # add anything that is not used
        used = []
        for olist in self.orderstim:
            used.extend(olist)

        elist = []
        nstim = self.num_stim

        # ------------------------- step 1 ---------------------------
        # catenate lists of event types that are either not in orderstim
        # or are the first element in one

        # fill elist with unused stimuli
        for stim in range(nstim):
            sval = stim+1 # stim val, for ease of typing
            if sval not in used:
                if self.verb > 1: print '++ filling unordered stim %d' % sval
                elist += [sval for i in range(self.num_reps[stim])]

        if self.verb > 3:
            print '++ elist (len %d, unordered): %s' % (len(elist), elist)

        # and append 'first' entries to elist
        for ind in range(len(self.orderstim)):
            olist = self.orderstim[ind]
            sval = olist[0]
            elist += [sval for i in range(self.num_reps[sval-1])]
            if self.verb > 1: print '++ filling order[0] stim %d' % sval

        if self.verb > 3:
            print '++ elist (len %d, order base): %s' % (len(elist), elist)

        # ------------------------- step 2 ---------------------------
        # randomize this list (other orderlist stimuli are not random)

        UTIL.shuffle(elist)
        if self.verb > 3:
            print '++ elist (len %d, base, rand): %s' % (len(elist), elist)

        # ------------------------- step 3 ---------------------------
        # fill in orderstim events:
        # insert orderstim list at each orderstim initial element
        # given: elist, firstdict, orderstim  ==> create new elist

        # create 'firstdict' lookup table for follower insertion
        firstdict = self.make_ordered_firstdict(self.orderstim)

        elist = self.insert_order_followers(elist, firstdict, self.orderstim)

        # ------------------------- step 4 ---------------------------
        # randomly merge with rest:
        # - shuffle binary list of rest/stim
        # - for each stim val, replace with respective stim index

        stat, mergelist = self.merge_elist_with_rest(elist, nrest)
        if stat: return 1, None
        
        return 0, mergelist

    def merge_elist_with_rest(self, elist, nrest):
        """make a new list with rest elements randomly dispersed along elist
        """
        mergelist = [0 for i in range(nrest)] + [1 for i in range(len(elist))]
        UTIL.shuffle(mergelist)

        eind = 0  # counter for filling from elist
        for mind in range(len(mergelist)):
            if mergelist[mind]:
                mergelist[mind] = elist[eind]
                eind += 1
        if eind != len(elist):
            print '** randomize panic: mergelist/elist mismatch'
            return 1, None

        return 0, mergelist

    def make_ordered_firstdict(self, orderstim):
        """return a dictionary of orderstim indices, indexed by first stims"""
        firstdict = {}
        for ind in range(len(orderstim)):
            olist = self.orderstim[ind]
            sval = olist[0]
            firstdict[sval] = ind # which list contains sval

        if self.verb > 3:
            print '++ firstdict lookup table: %s' % firstdict

        return firstdict

    def insert_order_followers(self, elist, firstdict, orderstim):
        """fill in followers from orderstim list
           inputs:
               elist    : current list of first and unordered elements
               firstdict: dict of {first order element: index into orderstim}
               orderstim: list of ordered stim lists
           output:
               new elist: with followers inserted
        """
        enew = []
        for val in elist:
            if firstdict.has_key(val):          # then append orderlist
                enew.extend(orderstim[firstdict[val]])
            else: enew.append(val)
        if self.verb > 3:
            print '++ elist with followers (len %d, all stim): %s' \
                  % (len(enew), enew)
        return enew


    def randomize_events(self, nrest):
        """make a list of all event types and then rest, randomize the list
           - this may be subject to lists of ordered stimuli (orderstim)
             (stimuli must be unique across all such lists)
           - return error code (0=success) and event list"""

        if nrest < 0:
            print "** randomize_events: nrest < 0"
            return 1, []

        # might also handle orderstim case
        if len(self.max_consec) == self.num_stim:
            return self.randomize_limited_events(nrest)

        if len(self.orderstim) > 0:
            return self.randomize_ordered_events(nrest)

        nstim = self.num_stim
        reps_list = self.num_reps

        elist = []
        for stim in range(nstim):
            elist += [(stim+1) for i in range(reps_list[stim])]
        elist.extend([0 for i in range(nrest)])

        if self.verb > 2:
            print '++ elist (len %d, sorted): %s' % (len(elist), elist)

        UTIL.shuffle(elist)

        if self.verb > 2:
            print '++ elist (len %d, random): %s' % (len(elist), elist)
            for stim in range(nstim):
                print '   number of type %d = %d' % (stim+1,elist.count(stim+1))

        return 0, elist

    def randomize_limited_events(self, nrest):
        """return a random list of events, where each event type may have a
           limit on consecutive occurances
             - keep list of # remaining for each type (include rest)
             - fraction of # remaining provides probability for next event
             - do not include event type at limit
             
             - this should succeed until one event type remains which has
               reached its limit
             - then try to dispurse remaining events

             - finally, merge in rest
        """

        # if stimuli are ordered, clear num_reps for followers
        if len(self.orderstim) > 0:
            if self.verb > 1: print '-- have limited events w/ordered stim...'
            num_reps = self.num_reps[:]
            for orderlist in self.orderstim:
                # stim in lists are 1-based, num_reps_is 0-based
                for follower in orderlist[1:]: num_reps[follower-1] = 0
        else: 
            num_reps = self.num_reps

        # rcr - test this!

        # initial pass on creation of events list
        rv, clist, rtype, rcount =  \
            self.init_limited_event_list(num_reps, self.max_consec)
        if rv:
            print '** failure of randomize_limited_events'
            return 1, None

        # if we ran out of space for one event type, try to fill
        # prev must be remaining event type
        if rcount > 0:
            rv, clist = self.fill_remaining_limited_space(self.max_consec,
                                                          clist, rtype, rcount)

        # next, rewrite as elist   ---> convert to 1-based index list here 
        elist = []
        for entry in clist:
            elist.extend([entry[0]+1 for i in range(entry[1])])
        if self.verb > 2: print "== clist (0-based) %s" % clist
        if self.verb > 2: print "== elist (1-based) %s" % elist

        # if orderstim, create a firstdict table and insert follower events
        if len(self.orderstim) > 0:
           if self.verb > 1: print '-- inserting order followers...'
           firstdict = self.make_ordered_firstdict(self.orderstim)
           elist = self.insert_order_followers(elist, firstdict, self.orderstim)

        # finally, merge with rest
        elist = UTIL.random_merge(elist, [0 for i in range(nrest)])

        return 0, elist


    def init_limited_event_list(self, num_reps, max_consec):
        """first pass on limited event list:
                Given num_reps and max_consec of each type, create a list
                [[event, count], ...] of event and consecutive counts.  Make
                this sequentially, only appending what is possible given the
                limits.  The probability of each new event will be the relative
                fraction of remaining events of that type, or 0 if the limit
                has been reached.

             inputs:  num_reps and max_consec
             outputs: status    - success or no from function
                      clist     - event count list [[event, count]]
                      rtype     - remaining type
                      rcount    - remaining count

             Note clist has 0-based event indices, they should be incremented.
        """

        remain   = num_reps[:]
        nremain  = sum(remain)
        t_remain = remain[:]            # same, but might get 0 at some index

        # convert max_consec, change any 0 to num_reps
        max_consec = max_consec
        for i, m in enumerate(max_consec):
            if m <= 0: max_consec[i] = num_reps[i]

        if self.verb > 1:
            print "-- limited events reps = %s, max = %s" % (remain, max_consec)

        # create a list of [[event, count]], from which to generate elist
        # (so insertion of over-the-limit events will be easier)
        #
        # note t_remain = remain, except that if the previous event type
        #      hit its consec limit, temporarily set that t_remain to 0
        clist = [[-1, 0]]       # init with no rest events, say
        cind = 0
        prev = -1
        for ind in range(nremain):
            # if we are at a limit, do not let limited event happen
            if prev >= 0:
                if clist[cind][1] >= max_consec[prev]:
                    t_remain[prev] = 0
            t_nremain = sum(t_remain)

            if t_nremain == 0:
                # we are either completely done, or should move on to insertion
                # of remaining 'prev' type events
                break

            # so insert a new event

            # make eind the next chosen event type; it must be usable
            rind = int(t_nremain*random.random()) # in [0,t_nremain-1]
            eind = 0
            while eind < len(t_remain) and rind >= t_remain[eind]:
                rind -= t_remain[eind]
                eind += 1

            # verify that we didin't screw up
            if eind >= len(t_remain):
                print '** eind too big for t_remain, failing...'
                return 1, None, 0, 0

            # account for applied event index and resync t_remain
            remain[eind] -= 1
            t_remain[eind] -= 1
            if prev >= 0: t_remain[prev] = remain[prev]

            # modify clist and monitor limit
            if eind == prev:    # if same as previous, increment
                clist[cind][1] += 1
            else:               # else, have a new event type
                clist.append([eind, 1])
                cind += 1
                prev = eind

            nremain -= 1

        if remain[prev] != nremain:
            print '** LE failure, nremain = %d, remain = %s' % (nremain,remain)
            return 1, None, 0, 0
        if not self.limited_events_are_valid(clist, max_consec):
            print '** LE failure, some events exceed maximum'
            return 1, None, 0, 0

        if self.verb > 1:
            if nremain == 0: print '-- have 0 events to insert'
            else:
               print '++ have %d events of type %d to insert' % (nremain, prev)
               print '   (note: events are currently 0-based)'

        return 0, clist, prev, nremain

    def fill_remaining_limited_space(self, max_consec, clist, rtype, rcount):
        """
        """
        pmax = max_consec[rtype]

        if clist[-1] != [rtype, pmax]:
            print "** messed up max for limited events..."
            return 1, None

        # do we have enough space for remaining events?
        space = self.count_limited_space(clist, rtype, pmax)
        if space < rcount:        # we are in trouble
            print '** limited_events: only %d positions for %d inserts' \
                  % (space, rcount)
            return 1, None
        if self.verb > 2: print '-- space for insert: %d' % space
        if self.verb > 3: print '   clist %s' % clist

        # okay, insert the remaining rcount events of type rtype
        # rely on rcount being small and do linear searches for each insert
        for ind in range(rcount): # rcount times, insert rtype event
            rind = int(space*random.random())  # insertion index
            if self.verb > 2: print '-- random insertion index %d' % rind
            ecount = 0  # count of all potential events
            # proceed until rind < nvalid at current index
            for cind in range(1, len(clist)):
                c0, c1 = clist[cind][0], clist[cind][1]
                # how many valid positions are at the current index?
                if c0 == rtype and c1 == pmax: continue  # no space

                # if rind is too big to insert here, move on
                # (take care if rtype bin is full)
                if clist[cind-1] == [rtype, pmax] and rind >= c1-1:
                    rind -= c1-1
                    continue
                elif rind >= c1:
                    rind -= c1
                    continue

                # we have a winner!  break and insert
                break

            if cind >= len(clist):
                print '** LE: failed to find insertion index'
                return 1, None

            if self.verb > 2: print '++ inserting at index %d' % cind

            # if rtype bin is full, skip one event
            if clist[cind-1] == [rtype, pmax]: rind += 1

            # if adjacent to an 'rtype' event, increment event count
            # (and adjust space, max event count means subtr pmax)
            if c0 == rtype:
                clist[cind][1] += 1
                if clist[cind][1] == pmax: space -= pmax+1
            elif clist[cind-1][0] == rtype and rind == 0:
                clist[cind-1][1] += 1
                if clist[cind-1][1] == pmax: space -= pmax+1
            # neither matches, so insert 'rtype', and possibly break c0
            elif rind == 0:
                clist.insert(cind, [rtype, 1])
            else: # must break clist[cind] apart
                clist.insert(cind, [c0, rind])
                clist.insert(cind+1, [rtype, 1])
                clist[cind+2][1] -= rind

            # fill cases subtraced pmax+1, so all can add 1 here
            space += 1
            if self.verb > 3: print '   clist %s' % clist # RCR
            if self.verb > 3: print 'new space: %d' % space
            if space != self.count_limited_space(clist, rtype, pmax):
                print "** space count failure"

        if not self.limited_events_are_valid(clist, max_consec):
            print '** LE fill remain failure, some events exceed maximum'
            return 1, None

        return 0, clist

    def count_limited_space(self, clist, eind, emax):
        """given an event class list, and event index and the maximum
           number of sequential events of that type, return the number of
           positions available to insert a new event"""
        space = 0
        for ind in range(1, len(clist)):
            # if next is full, skip
            if clist[ind] == [eind, emax]: continue
    
            # if previous is full, subtract 1 (can go before all but first)
            if clist[ind-1] == [eind, emax]: space += clist[ind][1] - 1
            else:                            space += clist[ind][1]
        return space

    def limited_events_are_valid(self, clist, maxlist):
        """verify that each event type in clist does not have too many
           in a row
           return 1 if valid, 0 if not"""
        for ind in range(1, len(clist)):
            # if next is full, skip
            if clist[ind][1] > maxlist[clist[ind][0]]:
                print '** check events failure for maxlist %s' % maxlist
                print '%s' % clist
                return 0
        return 1

    def apply_max_rest(self, eventlist):
        """modify eventlist so that there is never more than max_rest rest
           before any event"""

        if self.max_rest <= 0: return

        max_rest = self.max_rest
        if self.min_rest > 0:
            max_rest -= self.min_rest
            if self.verb > 2: print '-- updating max_rest from %g to %g' \
                                    % (self.max_rest, max_rest)

        # note the maximum rest as a number of intervals
        maxnrest = int(max_rest/self.t_gran)

        # create a list of stim index, post-stim rest count, stim type lists
        rlist = []
        elist = [-1, 0, 0]      # pre-event rest 'event'
        rlist.append(elist)
        for ind in range(len(eventlist)):
            val = eventlist[ind]
            if val > 0:         # non-rest event, add new elist to rlist
                elist = [ind, 0, val]
                rlist.append(elist)
            else: elist[1] += 1

        if self.verb > 3: print '++ event rlist', rlist

        # make extendable and fix lists
        elist = []              # extend list (space to add rest)
        flist = []              # fix list (need to remove space)
        tot = 0
        for ind in range(len(rlist)):
            if rlist[ind][1] > maxnrest:
                flist.append(ind)
                tot += (rlist[ind][1] - maxnrest)
            elif rlist[ind][1] < maxnrest:
                elist.append(ind)

        if self.verb > 4:
            print '-- maxrest extendable lists:', elist
            print '-- maxrest fix lists:', flist

        # maybe there is nothing to do
        if len(flist) < 1:
            if self.verb > 1: print '-- no event block exceeds max rest'
            return
        # note how much rest we need to move around
        ftotal = UTIL.loc_sum([rlist[flist[i]][1]-maxnrest
                                for i in range(len(flist))])
        etotal = UTIL.loc_sum([maxnrest-rlist[elist[i]][1]
                                for i in range(len(elist))])

        if self.verb > 1:
            print '-- shifting %g seconds of excessive rest' \
                  % (ftotal*self.t_gran)
            if self.verb > 3:
                print '== excessive rest exeeds %d events...' % maxnrest
                print '-- elist:', elist
                print '-- flist:', flist

        if etotal < ftotal:
            print "** max_rest too small to adjust (%g < %g)" % (etotal, ftotal)
            return
        elif self.verb > 3:
            print '--> distributing %d rest events among %d slots' \
                  % (ftotal, etotal)

        for fix in flist:
            nshift = rlist[fix][1] - maxnrest
            for count in range(nshift):
                if len(elist) <= 0:
                    print '** panic: empty elist but rest to shift'
                    sys.exit(1)
                # choose where to shift rest to and shift it
                rind = int(len(elist)*random.random())
                # shift: insert (at rlist[elist[rind]]+1 and remove
                roff = rlist[elist[rind]][0]+1  # new rest offset
                eventlist[roff:roff] = [0]      # insert the rest
                for iind in range(elist[rind]+1, len(rlist)): # adjust rlist
                    rlist[iind][0] += 1
                # maybe we are out of rest for this elist element
                rlist[elist[rind]][1] += 1
                if rlist[elist[rind]][1] >= maxnrest:
                    elist[rind:rind+1] = []     # then remove from elist
            # delete the extra rest from this event
            rlist[fix][1] -= nshift
            roff = rlist[fix][0] + 1            # rest to delete
            eventlist[roff:roff+nshift] = []    # actual deletion
            # and drop fix rest down by nshift
            for iind in range(fix+1, len(rlist)): # adjust rlist
                rlist[iind][0] -= nshift
                
        if self.verb > 3:
            print '++ fixed event rlist', rlist
            if self.verb > 4: print '++ updated eventlist', eventlist
            etotal = UTIL.loc_sum([maxnrest-rlist[elist[i]][1]
                                  for i in range(len(elist))])
            print '-- updated etotal = %d' % etotal

def basis_from_time(stim_len):
    if stim_len > 1.0: return "'BLOCK(%g,1)'" % stim_len
    else: return 'GAM'

def make_concat_from_times(run_times, tr):
    if len(run_times) < 2: return ''
    str = "    -concat '1D: 0"
    cind = 0
    for ind in range(len(run_times)-1):
        cind += round(run_times[ind]/tr)
        str += ' %d' % cind
    return str + "' \\\n"

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

