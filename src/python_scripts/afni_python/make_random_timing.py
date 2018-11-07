#!/usr/bin/env python

# python3 status: started

from __future__ import print_function

import sys, random, os, math, copy
import option_list as OL, afni_util as UTIL
import lib_rand_timing as LRT
import lib_afni1D as LAD

g_help_string = """
===========================================================================
make_random_timing.py - Create random stimulus timing files.

    Basic usage (consider the Advanced usage): ~1~

    The object is to create a set of random stimulus timing files, suitable
    for use in 3dDeconvolve.  These times will not be TR-locked (unless the
    user requests it).  Stimulus presentation times will never overlap, though
    their responses can.

    Consider using this in conjunction with @stim_analyze, at:

       https://afni.nimh.nih.gov/pub/dist/edu/data/CD.expanded/AFNI_data6/ht03/@stim_analyze

    ---------------------------------------------------------------------------
    note on advance usage: ~2~

    **  There is now basic (old) and advanced usage.  Until I decide how to
        properly merge the help, consider:

            make_random_timing.py -help_advanced

        Otherwise, this help covers the complete basic usage, followed by
        the "Advanced Usage" (search for that string).  Perhaps in the future
        the basic usage will just be moved below the advanced.
    ---------------------------------------------------------------------------
    background: ~2~

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
getting TR-locked timing ~2~

    If TR-locked timing is desired, it can be enforced with the -tr_locked
    option, along with which the user must specify "-tr TR".  The effect is
    to force stim_dur and t_gran to be equal to (or a multiple of) the TR.

    It is illegal to use both -tr_locked and -t_gran (since -tr is used to
    set t_gran).

----------------------------------------
distributing stimuli across all runs at once (via -across_runs) ~2~

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
examples: ~2~

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
NOTE: distribution of ISI ~2~

    To picture the distribution, consider the probability of starting with
    r rest events, given R total rest events and T total task events.

    The probability of starting with 0 rest events is actually the maximum, and
    equals the probability of selecting a task event first, which is T/(T+R).

    Let X be a random variable indicating the number of rest events to start
    a run.  Then P(X=0) = T/(T+R).
    While this may look "large" (as in possibly close to 1), note that
    typically R >> T.  For example, maybe there are 50 task events and 1000
    rest "events" (e.g. 0.1 s, each).  Then P(X=0) = 50/1050 = 0.0476.
    This ratio is generally closer to T/R than to 1.0.  T/R is 0.05 here.

    More details...

    To take one step back, viewing this as the probability of having t task
    events among the first n events, it follows a hypergeometric distribution.
    That is because for each event type that is selected, there are fewer such
    events of that type remaining for subsequent selections.  The selection is
    done *without* replacement.  The total numbers of each type of class are
    fixed, as is the total rest.

    This differentiates it from the binomial distribution, where selection
    is done *with* replacement.

    Taking a simplistic view, go back to the probability of starting with
    exactly r rest events, as stated in the beginning.  That means starting
    with r rest events followed by one task event, which in turn means first
    choosing r rest events ((R choose r) / ((R+T) choose r)), then choosing
    one task event, T/(R+T-r).

                 (R)
                 (r)        T            R!        (R+T-r-1)!
        P(X=r) = ----- * ------      = ----- * T * ----------
                 (R+T)   (R+T-r)       (R-r)!        (R+T)!
                 (r  )

    While this may not provide much insight on its own, consider the ratio
    of incremental probabilities P(X=r+1) / P(X=r):

        P(X=r+1)     R-r                                   R     - r
        -------- = -------   = for visual significance = -----------
         P(X=r)    R+T-1-r                               R+T-1   - r

    The left side of that ratio is fixed at R/(R+T-1) = 1000/(1049) = .953
    for the earlier example.  It may by common to be in that ballpark.
    For subsequent r values, that ratio goes down, eventually hitting 0 when
    the rest is exhausted (r=R).

    This means that the distribution of such rest actually falls _below_ an
    exponential decay curve.  It is close to (R/(R+T-1))^r at first, decaying
    more rapidly until hitting 0.
     
    ==> The overall distribution of ISI rest looks like an exponential decay
        curve, with a peak at r=0 (no rest) and probability close to T/R.

    Note that the average ISI should be approximately equal to
    total rest time / # task events
    (e.g. 100s / 50 stimuli = 2s (per stim)).
    So the cumulative distribution function would hit 0.5 where r corresponds
    to this ratio, e.g. r = 20, where each rest event is 0.1s.

    Test this:

    Create a histogram of all ISI durations based on 100 2-second events in a
    single run of length 300 (so 200 s for task, 100 s for rest), with rest
    distributed randomly on a 0.1 s time grid.  Note that what matters is the
    number of stim events (100) and the number of rest events (1000), not their
    respective durations (unless there are user-imposed limits).

    Given the timing, "timing_tool.py -multi_timing_to_event_list" can be used
    to output ISIs (for example).  Use that to simply make a list of ISIs, and
    then make a histogram.  Let us repeat the process of generating events and
    ISIs, accumulating a list of ISIs, a total of 100 times.  The generate and
    plot of histogram of all ISI duration counts.

    Since rest is on a 0.1 s grid, we will scale by 10 and make an integer
    histogram.

       echo -n "" > isis_all.1D
       foreach rep ( `count 1 100` )
          echo simulation $rep
          make_random_timing.py -num_stim 1 -num_runs 1 -run_time 300 \\
              -stim_dur 2 -num_reps 100 -prefix t -verb 0
          ( timing_tool.py -multi_timing t_01.1D -multi_stim_dur 2    \\
              -multi_timing_to_event_list GE:o - -verb 0              \\
              | 1deval -a - -expr '10*a' >> isis_all.1D ) >& /dev/null
       end
       3dhistog -int isis_all.1D | tee isis_hist.1D
       1dplot -sepscl isis_hist.1D'[1,2]'

    Note that the histogram might be scaled down by a factor of 100 to get
    an expected ISI frequency per run (since we effectively accumulated the
    ISI lists over 100 runs).

    Basically, we are looking for something like a exponential decay curve
    in the frequency histogram (the lower plot).

    Include plot of probabilities, computed incrementally (no factorials).
    Use the same event counts, 100 task and 1000 rest events.  Truncate this
    histogram to plot them together.

       set nhist = `1dcat isis_hist.1D | wc -l`
       make_random_timing.py -verb 0 -show_isi_pdf 100 1000 > pure_probs.1D
       grep -v prob pure_probs.1D | grep -v result | grep -v '\-----' \\
           | head -n $nhist > prob.1D
       1dplot -sepscl prob.1D'[1]' isis_hist.1D'[1,2]'

    Side note assuming replacement and the binomial distribution:

       In the case of replacement, we get a binomial distribution.  In the same
       P(X=r) case (starting with r rest events), the probabilities are simple.
          P(X=r) = [R/(R+T)]^r  * T/(R+T)
       Each rest probability is simply R/(R+T), while task is T/(R+T).
       The incremental probability is simply that of getting one more rest,
       which is R/(R+T) because of independence (samples are "replaced").

       In this case, the PDF should more exactly follow an exponential decay
       curve.

----------------------------------------------------------------------
options and arguments ~2~

----------------------------------------
informational arguments:

    -help                       : display this help
    -help_advanced              : display help for advanced usage
    -help_concerns              : display general concerns for timing
    -help_todo                  : display list of things to do
    -hist                       : display the modification history
    -show_valid_opts            : display all valid options (short format)
    -ver                        : display the version number

----------------------------------------
advanced arguments/options:

    -help_advanced              : display help for advanced usage
    -help_decay_fixed           : display background on decay_fixed dist type
    -help_concerns              : display general concerns for timing
    -help_todo                  : "to do" list is mostly for advanced things

    -add_timing_class           : create a new timing class (stim or rest)
    -add_stim_class             : describe a new stimulus class (timing, etc.)
    -rand_post_stim_rest yes/no : allow rest after final stimulus
    -show_rest_events           : show details of rest timing, per type
    -write_event_list FILE      : create FILE listing all events and times
    -save_3dd_cmd FILE          : write 3dDeconvolve script to FILE
    -make_3dd_contrasts         : include pairwise contrasts in 3dD script
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

    -not_first LAB LAB ...      : specify classes that should not start a run

        e.g. -not_first base_task

        If there are any stimulus tasks that should not occur first within a
        run, those labels can be provided with this option.

        This cannot (currently) be applied with -across_runs or -max_consec.

    -not_last LAB LAB ...       : specify classes that should not end a run

        e.g. -not_last base_task

        If there are any stimulus tasks that should not occur last within a
        run, those labels can be provided with this option.

        This cannot (currently) be applied with -across_runs or -max_consec.

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

g_help_advanced = """
===========================================================================
Advanced usage (make_random_timing.py) ~1~

   With advanced usage, timing classes are defined for both stimulus periods
   and rest periods.  Timing classes specify duration types that have different
   distributions (min, mean, max and distribution type), which can be applied
   to stimulus events or to rest events.

   overview of timing classes: ~2~

   When specifying a timing class, one can provide:

        min     : min, mean and maximum for possible durations
        mean    : -1 means unspecified, to be computed by the program
                : mean determines total time for class, if specified
                * for a uniform distribution, the mean or max implies
                  the other, while that is not true for decay
        max     : -1 means unspecified, likely meaning no limit for decay class
        dtype   : distribution type (default=decay)
                  decay:        shorter events are more likely
                                see "NOTE: distribution of ISI"
                              * new method, as of Feb 3, 2017
                  decay_fixed:  precise decay method, which properly follows a
                                scaled e^-x PDF, where durations are implied by
                                the parameters (for a fixed set of params, only
                                the order of durations is random)
                              * new method, as of Oct 31, 2017
                           see: make_random_timing.py -help_decay_fixed
                  decay_old:    old decay method, which can bunch up at max
                                limit, if one is applied
                  uniform_rand: randomly chosen durations with uniform dist
                  uniform_grid: durations spread evenly across grid
                  fixed:        one duration is specified
                  INSTANT:      duration = 0
        t_grid  : all durations are fixed on this grid (default=0.01s)

   One can provide subsets:

        min                             : implies fixed
        min, mean, max                  : implies decay on default t_grid
        min, mean, max, dtype           : implies default t_grid
        min, mean, max, dtype, t_grid

   NOTE: dtype and t_grid are specified as named parameters, e.g.

            dtype=decay_fixed
            t_grid=0.001


   Every stimulus class type is followed by a fixed rest class type.  So rest
   periods are "attached" to the preceding stimulus periods.  For example, the
   'faces' class events might last for 0.5 - 1.5 seconds, and be uniformly
   distributed (so average = 1s).  Those face events might then be followed by
   0.5 - 8.5 seconds of rest with a 'decay' distribution (so shorter durations
   are more probable than longer durations).

   The 'decay' distribution type matches that of the basic (non-advanced) use
   this program.  See "NOTE: distribution of ISI" in the -help output.

   ============================================================
   Examples: ~2~

   -------------------------------------------------------
   Advanced Example 1: basic, with 3 conditions ~3~

     - This is a simple case with 3 conditions, each having 8 events per run
       of duration 3.5 s.  Rest is randomly distributed using the default
       'decay' distribution (meaning shorter periods are more likely than
       longer ones).  The first and last 20 s is also allocated for rest.

     - Do this for 4 runs of length 200 s each.

     - Also, do not allow any extra rest (beyond the specified 10 s) after
       the final stimulus event.

     - Generate 3dDeconvolve command script (and with pairwise contrasts).

     - Show timing statistics.  Save a complete event list (events.adv.1.txt).

         make_random_timing.py -num_runs 4 -run_time 200         \\
            -pre_stim_rest 10 -post_stim_rest 10                 \\
            -rand_post_stim_rest no                              \\
            -add_timing_class stim 3.5                           \\
            -add_timing_class rest 0 -1 -1                       \\
            -add_stim_class houses 10 stim rest                  \\
            -add_stim_class faces  10 stim rest                  \\
            -add_stim_class donuts 10 stim rest                  \\
            -show_timing_stats                                   \\
            -write_event_list events.adv.1.txt                   \\
            -save_3dd_cmd cmd.3dd.eg1.txt                        \\
            -make_3dd_contrasts                                  \\
            -seed 31415 -prefix stimes.adv.1


   -------------------------------------------------------
   Advanced Example 2: varying stimulus and rest timing classes ~3~

     - This has 4 stimulus conditions employing 3 different stimulus timing
       classes and 3 different rest timing classes.

       timing classes (stim and rest periods):

           stima: durations in [0.5, 10], ave = 3s (decay distribution)
           stimb: durations in [0.1, 3], ave = 0.5s (decay distribution)
           stimc: durations of 2s

           resta: durations in [0.2, 1.2], ave = 0.7 (uniform rand dist)
           restb: durations in [0.5, 1.5], ave = 1.0 (uniform grid dist)
           restc: durations in (0, inf) (decay dist) - absorbs remaining rest

       conditions (each has stim timing type and subsequent rest timing type)

                    # events (per run)  stim timing        rest timing
                    --------            -----------        -----------
           houses :    20                  stima              resta
           faces  :    20                  stimb              restb
           donuts :    20                  stimb              restb
           pizza  :    20                  stimc              restc

     - Do not allow any rest (aside from -post_stim_rest) after final stim
       (per run).  So there will be exactly the rest from -post_stim_rest at
       the end of each run, 10s in this example.

         make_random_timing.py -num_runs 2 -run_time 400         \\
            -pre_stim_rest 10 -post_stim_rest 10                 \\
            -rand_post_stim_rest no                              \\
            -add_timing_class stima 0.5 3 10                     \\
            -add_timing_class stimb 0.1 0.5 3                    \\
            -add_timing_class stimc 2                            \\
            -add_timing_class stimd 1 2 6 dist=decay_fixed       \\
            -add_timing_class resta 0.2 .7 1.2 dist=uniform_rand \\
            -add_timing_class restb 0.5 1  1.5 dist=uniform_grid \\
            -add_timing_class restc 0 -1 -1                      \\
            -add_stim_class houses 20 stima resta                \\
            -add_stim_class faces  20 stimb restb                \\
            -add_stim_class donuts 20 stimb restb                \\
            -add_stim_class tacos  20 stimc restc                \\
            -add_stim_class pizza  40 stimd restc                \\
            -write_event_list events.adv.2                       \\
            -show_timing_stats                                   \\
            -seed 31415 -prefix stimes.adv.2


   -------------------------------------------------------
   Advanced Example 3: ordered event types ~3~

     - Every cue event is followed by test and then result.
     - Every pizza1 event is followed by pizza2 and then pizza3.
     - The stimc timing class has durations on a grid of 0.1s, rather
       than the default of 0.01s.
     - Write a corresponding 3dDeconvolve script, cmd.3dd.eg3.txt.

         make_random_timing.py -num_runs 2 -run_time 300         \\
            -pre_stim_rest 10 -post_stim_rest 10                 \\
            -rand_post_stim_rest no                              \\
            -add_timing_class stima 0.5 3 10                     \\
            -add_timing_class stimb 0.1 0.5 3                    \\
            -add_timing_class stimc 0.1 2.5 10 t_gran=0.1        \\
            -add_timing_class stimd 2                            \\
            -add_timing_class resta 0.2 .7 1.2 dist=uniform_rand \\
            -add_timing_class restb 0.5 1  1.5 dist=uniform_grid \\
            -add_timing_class restc 0 -1 -1                      \\
            -add_stim_class cue    20 stima resta                \\
            -add_stim_class test   20 stimb restb                \\
            -add_stim_class result 20 stimb restb                \\
            -add_stim_class pizza1 10 stimc restc                \\
            -add_stim_class pizza2 10 stimc restc                \\
            -add_stim_class pizza3 10 stimc restc                \\
            -add_stim_class salad  10 stimd restc                \\
            -write_event_list events.adv.3                       \\
            -show_timing_stats                                   \\
            -ordered_stimuli cue test result                     \\
            -ordered_stimuli pizza1 pizza2 pizza3                \\
            -save_3dd_cmd cmd.3dd.eg3.txt                        \\
            -seed 31415 -prefix stimes.adv.3

   -------------------------------------------------------
   Advanced Example 4: limit consecutive events per class type ~3~

     - Use simple 1s stim events and random rest (decay).
     - For entertainment, houses/faces and tuna/fish are
       ordered event pairs.
     - Classes houses, faces, tuna and fish are restricted to a
       limit of 3 consecutive events.
     - There is no limit on donuts.   Why would there be?

         make_random_timing.py -num_runs 2 -run_time 600         \\
            -pre_stim_rest 0 -post_stim_rest 0                   \\
            -add_timing_class stim 1                             \\
            -add_timing_class rest 0 -1 -1                       \\
            -add_stim_class houses 100 stim rest                 \\
            -add_stim_class faces  100 stim rest                 \\
            -add_stim_class tuna 100 stim rest                   \\
            -add_stim_class fish 100 stim rest                   \\
            -add_stim_class donuts 100 stim rest                 \\
            -ordered_stimuli houses faces                        \\
            -ordered_stimuli tuna fish                           \\
            -max_consec 3 3 3 3 0                                \\
            -show_timing_stats                                   \\
            -write_event_list events.adv.4                       \\
            -seed 31415 -prefix stimes.adv.4 -verb 2

---------------------------------------------------------------------
options (specific to the advanced usage): ~2~

    -help_advanced              : display help for advanced usage
    -help_concerns              : display general concerns for timing
    -help_decay_fixed           : display background on decay_fixed dist type
    -help_todo                  : "to do" list is mostly for advanced things

    -add_timing_class           : create a new timing class (stim or rest)
    -add_stim_class             : describe a new stimulus class (timing, etc.)
    -rand_post_stim_rest yes/no : allow rest after final stimulus
    -show_rest_events           : show details of rest timing, per type
    -write_event_list FILE      : create FILE listing all events and times

----------------------------------------------------------------------
R Reynolds  Jan 20, 2017          motivated by K Kircanski and A Stringaris
===========================================================================
"""

g_help_concerns = """
===========================================================================
general concerns regarding random timing (to be expanded) ~2~

   (some of this only applies to the advanced usage)

   - should pre-steady state time be included in these timing files
      - see -pre_stim_rest
   - otherwise, one might prefer pre-stim rest = 0 (default in advanced)
   - it is nice to have some minimum post-stim at the end of the run
      - else the last event is wasted 
      - consider 6-10 s
      - see -post_stim_rest
   - it may be nice to have only post-stim rest, but not any extra random
     rest attached to the final event
      - consider "-rand_post_stim_rest no"
===========================================================================
"""

g_decay_fixed_details = """
===========================================================================
background on creating decay_fixed curves ~2~
 
 Notes:
 Given A,M,B,N = desired min,mean,max for list of N (pseudo-randomly ?)
    generated numbers that follow a decay type distribution (e^-x),
    generate such a list.

    The first step is to find L such that the fractional mean of e^-x on [0,L]
    equals the fractional mean of the inputs.  The fractional mean of the
    inputs is m=(M-A)/(B-A), and the fractional mean of e^-x on [0,L] is the
    expected value of x (in PDF e^-x) divided by L.
  
 Basic equations: ~3~
  
       (1) integral [e^-x] on (a,b) = e^-a - e^-b
       (2) integral [xe^-x] on (a,b) = (a+1)e^-a - (b+1)e^-b
       (3) E[x] = int[xe^-x] / int[e^-x] on (a,b) = (a+1)e^-a - (b+1)e^-b
                                                    ---------------------
                                                        e^-a - e^-b

    Since E[x] on [0,L] = (1-(L+1)e^-L)/(1-e^-L) = 1 - L/(e^L - 1), define

       (4) F(L) = E[x]/L = 1/L - 1/(e^L - 1)

  * This is the principle equation we want to invert.  Given mean M, converted
    to a fractional offset mean m = (M-A)/(B-A), we can solve m = F(L) for L,
    defining a decay function with the given fractional mean.  So if e^-x on
    [0,L] has mean m, x in the function can be shifted and scaled to be on
    [A,B] with the given mean M.
   

 a) Approximation games... ~3~

    Given A,M,B, find L such that E[x]/L = (M-a)/(b-a).
    So if m is the fractional mean on [a,b], solve m = F(L) = E[x]/L for m.
  
    F(L) can be reasonably well approximated by invertible functions on
    (0,3], (3,6) and [6,inf), being linear, scaled 1/L and 1/L.
  
          F(0) -> 0.5     (limit as L->0 of F(L))
          F(3) ~= 0.281
          F(6) ~= 0.164 ~= 1/6 (not great, consider using F(8)=.125)
  
       i) on (0,3], approximate F(L) with line:
  
          F1(L)  = 0.5 - 0.73*L
          F1I(m) = (0.5 - m)/0.73
  
       ii) on (3,6), map 3 -> 1/f1(3) = 3.5587,  map 6 -> 6
           can use F2_0(x) = 1.1174 + .8138*x
           So define F2(x) = 1/F2_0(x).
  
          F2(L)  = 1 / (1.1174 + 0.8138 * L)
          F2I(L) = (1.0/m - 1.1174) / 0.8138
  
       iii) on [6,inf), approximate F(L) with 1/L
  
          F3(L)  = 1/L     (is invertible and is its own inverse)
          F3I(L) = F3(L)


    Better approximation:

       on [0,2]   f1(L) = 0.5 - 0.1565*L/2.0
       on (2,3]   f2(L) = 1.0 / (1.6154 + 0.6478*L)
       on (3,6)   f3(L) = 1.0 / (1.1174 + 0.8138 * L)
       on [6,inf) f4(L) = 1.0 / L

    This looks great, but is still a little unfulfilling (now I want beer).

    Note: if the solution of L is not fairly precise, then generating many
          random values with slightly incorrect mean M' might throw off the
          length of a run.  Optimally, (M'-M)*N < t_gran, say.
    
    Hmmmm, in the immortal words of the famous philosopher Descartes,
    "this is kinda stupid".  Forget approximations.  Moving on...
  

 b) Solve m = F(L) for L using Newton's method. ~3~

    F(L) is very smooth, behaving like a line for L<2 and then scaled
    versions of 1/L beyond that.  So iterate to invert, which should be
    quick and have any desired accuracy.

    First find an approximate solution, then apply Newton's method where
    the next iteration of x1 uses the previous one and the approximate
    slope at x0, slope ~= (f(x+h)-f(x))/h for small h.

    i) define basic approximation by pivot at L=4 as initial guess
       {Exactly why should we throw out the accurate approximations above?
        I don't know.  Okay, let's say that we prefer a single, simple
        function.}

       Invert m = { .5 - L/16   L in [0,4]
                  { 1/L         L > 4

          1.0/m , if m <= 0.25
          8-16*m, if 0.25 < m < 0.5

    ii) define a step to get the next iteration value

       Use inverse slope (delta x / delta  y) to scale change in y to
       change in x.

          decay_newton_step(fn, y_goal, x0, dx):
             y0 = fn(x0)
             dxdy = dx / (fn(x0+dx) - y0)
             return x0 + (y_goal-y0) * dxdy

    iii) solve by taking initial guess and iterating until close

       find x s.t. fn(x) ~= y_goal, where |fn(x) - y_goal| < prec

          decay_solve(fn, y_goal, prec, maxind=100):
             x = decay_guess(y_goal)
             f = fn(x0)
             while abs(f - y_goal) > prec
                x = decay_newton_step(fn, y_goal, x, prec)
                f = fn(x)

       We can be precise here.  Not only does this converge quickly, but
       it is a one time operation per such timing class.

 Handle m in different ranges according to:

    m <= 0             illegal
    0 < m < 0.5        expected
    m ~= 0.5           should use uniform distribution instead of decay
    0.5 < m < 1.0      expected, solve using 1-m and reflect about the mean
    m >= 1.0           illegal

 c) Get durations that follow PDF and have mean m. ~3~

    Given L, get a list of N exact durations that follow PDF and have mean m.

    This can still apply to [0,inf) with fractional mean m in (0,0.5).

       times = decay_get_PDF_times(L, N)

    To do this, break the interval [0,L] into N pieces (a,b), such that the
    integral over each (a,b) is 1/N (of the full one).  Then find E[x] on each 
    interval (a,b) so that E[x]*(a-b) = 1/N.  Then the sum of such expected
    x values is 1 (is the full integral).

  * Note that such values have the desired mean m and follow the PDF on [0,L].

 d) Scale the times. ~3~

    Given A, M, B, and N times[], map each time from [0,L] to [A,B] (which
    should map the mean m to mean M).

         newval = A + oldval * (B-A)/L

 e) Round the times. ~3~

    Maintain CS = cumulative sum of differences between the exact and rounded
    times.  If the new abs(CS) >= prec (precision), round in the opposite
    direction, to maintain abs(CS) < prec, which should be sufficient.  We
    *could* require abs(CS) < prec/2, but that might mean more would be
    rounded in the "wrong" direction.  Please discuss amongst yourselves...

 f) if original m > 0.5, reflect times over (A+B)/2 ~3~

 g) Verify the mean. ~3~

    Since the difference between the precise sum and the rounded sum
    should be less than prec, the difference between the means should be
    less than prec/N, which is all we can ask for in life.
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
    1.7  Oct 03, 2012: some options do not allow dashed parameters
    1.8  Nov 14, 2012: fixed checks for random space in -max_consec case
    1.9  Aug 21, 2015: added help for understanding the distribution of ISI
                       see: NOTE: distribution of ISI
    1.10 Jun 01, 2016: minor updates to verbose output

    2.0  Jan 20, 2017: basically a new program
         * Advanced usage: applying user-defined timing classes for stim/rest.
           see:   make_random_timing.py -help_advanced
    2.1  Jan 23, 2017: allow use of INSTANT timing class; reorder example opts
    2.2  Feb  3, 2017:
         - decay class now follows a better curve
         - added decay_old class for old decay method
    2.3  May  9, 2017: applied -offset for advanced case
    2.4  May 24, 2017: advanced -save_3dd_cmd and -make_3dd_contrasts
    2.5  Sep 26, 2017: adv: clarify use of -1 for mean, max in help
    2.6  Oct 31, 2017: adv: added decay_fixed distribution type
    2.7  Nov  1, 2017:
         - decay_fixed: make L more precise; make demos accessible
         - add -help_decay_fixed
    3.0  Nov  9, 2017: python3 compatible
    3.1  Nov 21, 2017: added -not_first and -not_last for C Smith
                       (still needs to be added to advanced case)
    3.2  Jan 30, 2018: added -help_concerns
    3.3  Oct  9, 2018:
         - fixed decay rest with non-zero min
         - block unlimited decay stim dur
    3.4  Nov  5, 2018: make some insufficient time failures more descriptive
"""

g_version = "version 3.4 November 5, 2018"

g_todo = """
   - add -show_consec_stats option?
   - c23 shows small post-stim rest...
   - reconcile t_grid as global vs per class (init/pass as single parameters)
   - make new method for decay that better handles max duration, w/out spike
   - add warning if post-stim rest < 3 seconds
   - add option to specify timing classes for pre and post stim rest
   - add related dist_types rand_unif and rand_gauss?
   - global "-across_runs" should still apply

   - maybe this program should just be a new one?  gen_random_timing.py?
      - MRT could be forked and depricated, but maybe people want to use it?
      - or perhaps it could be clear by calling it old_MRT.py
      -no, just try to partition options
         - OLD: -num_stim, -num_reps, -stim_dur, -stim_labels, -min_rest
         - ADV: -add_stim_class, -add_timing_class
"""

gDEF_VERB       = 1      # default verbose level
gDEF_OLD_T_GRAN = 0.1    # old-style time granularity, in seconds
gDEF_OLD_DEC_PLACES = 1  # old-style digits after decimal for times
gDEF_MIN_T_GRAN = 0.0001 # minimum time granularity, in seconds

g_instant_timing_class = LRT.TimingClass('INSTANT', 0, 0, 0)


g_style_opts_old = ['-num_stim', '-num_reps', '-stim_dur', '-stim_labels',
                    '-min_rest']
g_style_opts_new = ['-add_stim_class', '-add_timing_class']

# sample usage:
#
# -add_timing_class stimA 3 
# -add_timing_class stimA 3 3  3 decay 0.1
# -add_timing_class stimA 3 5 10
# -add_timing_class stimA 3 5 10 decay
# -add_timing_class stimA 3 5 10 decay 0.1
# -add_timing_class stimA 3 5  7 uniform 1
# 
# -add_timing_class restA 3
# -add_timing_class restA 3 5 10 decay
# -add_timing_class restA 1 5  9 uniform 0.1  (decide how to sample)

# order of stim evens should be controlled by:
#    - randomization
#    - max consec
#    - ordered_stimuli
#    - across_runs (sum (sdur+ave rest) to match run lengths)
#    - catch trials (can do later)
#
# stim events:
#    randomize events subject to any constraints (max consec, ordered stim)
#    compute random timing for events
# apply rest:
#    add all minimum rest: is design possible?
#    add all maximum rest: is design possible?
#    

                        
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

        # ------------------------------------------------------------
        # required arguments for advanced method
        self.tclasses   = [g_instant_timing_class] # TimingClass instances
        self.sclasses   = []            # StimClass instances
        self.advanced   = 0

        # advanced options
        self.rand_post_stim_rest = 1    # include random rest from last event?
        self.show_rest_events = 0       # show stats and rest events

        # pre- and post-rest timing classes
        self.pre_stimc  = g_instant_timing_class
        self.post_stimc = g_instant_timing_class

        # other parameters for advanced method
        self.stim_event_list = []       # stim index, duration lists
        self.full_event_list = []       # index, duration, time lists
        self.stim_adata      = []       # AfniData instance, per stim class
        self.stimdict        = {}       # name:index Stim dict, for name lookup

        # ------------------------------------------------------------
        # required arguments for basic method
        self.num_reps   = []            # number of stimuli, per class (per run)
        self.stim_dur   = []            # time of single stimulus (seconds)
                                        #   - per stim class

        # ------------------------------------------------------------
        # other required arguments
        self.num_stim   = 0             # number of stimulus classes
        self.num_runs   = 0             # number of runs
        self.prefix     = None          # prefix for output files
                                        # add .LABEL.INDEX.1D

        self.run_time   = []            # total time per run (seconds)

        # ------------------------------------------------------------
        # optional arguments
        self.across_runs    = 0         # flag: stimuli span all runs
        self.pre_stim_rest  = 0         # seconds before first stim
        self.post_stim_rest = 0         # seconds after last stim
        self.max_rest = 0.0             # maximum rest after each stimulus
        self.min_rest = 0.0             # minimum rest after each stimulus
        self.offset   = 0.0             # offset for all stimulus times
        self.seed     = None            # random number seed

        self.notfirst = []              # class indices that cannot start a run
        self.notlast  = []              # class indices that cannot end a run
        self.nflabs   = []              # class labels that cannot start a run
        self.nllabs   = []              # class labels that cannot start a run

        self.orderstim= []              # list of ordered stimulus lists (ints)
        self.orderlabs= []              # list of ordered stimulus lists (strs)
        self.osleaders= []              # leaders and followers
        self.osfollow = []
        self.osdict   = {}              # list of followers per leader

        self.max_consec = []            # max consectutive stimuli per type
        self.t_gran   = gDEF_OLD_T_GRAN # time granularity for rest
        self.tgset    = 0               # was this field set?
        self.t_digits = gDEF_OLD_DEC_PLACES # digits after decimal for times
                                        # (-1 means to use %g)
        self.labels   = None            # labels to be applied to filenames

        self.tr_locked      = 0         # flag: require TR-locked timing
        self.tr             = 0.0       # TR, for use in output 3dDecon cmd
        self.control_breaks = 1         # flag: across runs, add max stim time
                                        # to post_stim_rest
        self.file_3dd_cmd   = None      # file for 3dD -nodata command
        self.make_3dd_contr = 0         # flag to make all 3dD contrasts
        self.file_elist     = ''        # file for stimulus event list

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
        self.valid_opts.add_opt('-help_advanced', 0, [],      \
                        helpstr='display program help for ADVANCED usage')
        self.valid_opts.add_opt('-help_concerns', 0, [],      \
                        helpstr='display general timing concerns')
        self.valid_opts.add_opt('-help_decay_fixed', 0, [],      \
                        helpstr='display background for decay_fixed dist')
        self.valid_opts.add_opt('-help_todo', 0, [],      \
                        helpstr='display stupid todo list')
        self.valid_opts.add_opt('-hist', 0, [],      \
                        helpstr='display the modification history')
        self.valid_opts.add_opt('-show_isi_pdf', 2, [], \
                        helpstr='show init ISI pdf given NTASK NREST')
        self.valid_opts.add_opt('-show_isi_f_pdf', 2, [], \
                        helpstr='show init fact ISI pdf given NTASK NREST')
        self.valid_opts.add_opt('-show_valid_opts', 0, [], \
                        helpstr='display all valid options')
        self.valid_opts.add_opt('-ver', 0, [],       \
                        helpstr='display the current version number')

        # new 'required' arguments
        self.valid_opts.add_opt('-add_timing_class', -2, [], req=0,
                        helpstr='timing class: name min mean max pdf tgran')
        self.valid_opts.add_opt('-add_stim_class', 4, [], req=0,
                        helpstr='stim class: name nreps stiming rtiming')

        # new optional arguments
        self.valid_opts.add_opt('-rand_post_stim_rest', 1, [], req=0,
                        acplist=['no','yes'],
                        helpstr='include random rest after final stimulus? y/n')
        self.valid_opts.add_opt('-show_rest_events', 0, [], req=0,
                        helpstr='show stats on each rest class (y/n)')
        self.valid_opts.add_opt('-write_event_list', 1, [],
                        helpstr="write event list to file ('-' for stdout)")

        # old 'required' arguments
        self.valid_opts.add_opt('-num_stim', 1, [], req=0,
                        helpstr='number of stimulus types')
        self.valid_opts.add_opt('-num_runs', 1, [], req=1,
                        helpstr='number of scanning runs')
        self.valid_opts.add_opt('-num_reps', -1, [], req=0, okdash=0,
                        helpstr='number of stimulus reps per run, per class')
        self.valid_opts.add_opt('-stim_dur', -1, [], req=0, okdash=0,
                        helpstr='length of each stimulus, in seconds')

        # required arguments
        self.valid_opts.add_opt('-prefix', 1, [], req=1,
                        helpstr='prefix for output stimulus timing files')
        self.valid_opts.add_opt('-run_time', -1, [], req=1, okdash=0,
                        helpstr='total length of each run, in seconds')

        # optional arguments
        self.valid_opts.add_opt('-across_runs', 0, [],
                        helpstr='distribute stim reps across all runs')
        self.valid_opts.add_opt('-make_3dd_contrasts', 0, [],
                        helpstr='add contrasts pairs to 3dDeconvolve script')
        self.valid_opts.add_opt('-max_consec', -1, [], okdash=0,
                        helpstr='max consecutive occurances of each stim type')
        self.valid_opts.add_opt('-max_rest', 1, [],
                        helpstr='maximum rest time after each stimulus')
        self.valid_opts.add_opt('-min_rest', 1, [],
                        helpstr='minimum rest time after each stimulus')
        self.valid_opts.add_opt('-not_first', -1, [],
                        helpstr='list of event labels that should not start a run')
        self.valid_opts.add_opt('-not_last', -1, [],
                        helpstr='list of event labels that should not end a run')
        self.valid_opts.add_opt('-offset', 1, [],
                        helpstr='offset to add to every stimulus time')
        self.valid_opts.add_opt('-ordered_stimuli', -1, [], okdash=0,
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
        self.valid_opts.add_opt('-stim_labels', -1, [], okdash=0,
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
            print(g_help_string)
            print(g_help_advanced)
            print(g_help_concerns)
            return 0

        if '-help_advanced' in sys.argv:
            print(g_help_advanced)
            return 0

        if '-help_concerns' in sys.argv:
            print(g_help_concerns)
            return 0

        if '-help_decay_fixed' in sys.argv:
            print(g_decay_fixed_details)
            return 0

        if '-help_todo' in sys.argv:
            print(g_todo)
            return 0

        # check for -ver and -hist, too
        if '-hist' in sys.argv:
            print(g_history)
            return 0

        if '-ver' in sys.argv:
            print(g_version)
            return 0

        # maybe the user wants to list valid options
        if '-show_valid_opts' in sys.argv:
            self.valid_opts.show('', 1)
            return 0

        # ----------------------------------------
        # terminal processing args that involve processing
        if '-show_isi_pdf' in sys.argv or '-show_isi_f_pdf' in sys.argv:
           return do_isi_pdfs(sys.argv)

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

        # -----------------------------------------------------------------
        # determine advanced or basic usage styles and get relevant args

        # ADVANCED: -add_stim_class, -add_timing_class
        if self.has_any_opts(g_style_opts_new):
           if self.has_any_opts(g_style_opts_old):
              print('** have both new- and old-styled options, use only one')
              print('   ADVAN: %s' % ', '.join(g_style_opts_new))
              print('   BASIC: %s' % ', '.join(g_style_opts_old))
              return 1

           # note that we have the advanced style
           self.advanced = 1

           # get timing classes first, required for stim classes
           olist = self.user_opts.find_all_opts('-add_timing_class')
           if len(olist) == 0:
               print('** ADV STYLE: missing option -add_timing_class')
               return 1
           for opt in olist:
               if self.apply_opt_timing_class(opt):
                  return 1

           olist = self.user_opts.find_all_opts('-add_stim_class')
           if len(olist) == 0:
               print('** ADV STYLE: missing option -add_stim_class')
               return 1
           for opt in olist:
               if self.apply_opt_stim_class(opt):
                  return 1

           # default to including post-stim rest
           if self.user_opts.have_no_opt('-rand_post_stim_rest'):
              self.rand_post_stim_rest = 0

           if self.user_opts.find_opt('-show_rest_events'):
              self.show_rest_events = 1

           # and set some of the old-style parameters
           self.num_stim = len(olist)
           self.labels = [scl.name for scl in self.sclasses]

        # OLD: -num_stim, -num_reps, -stim_dur, -stim_labels, -min_rest
        elif self.has_any_opts(g_style_opts_old):
           self.num_stim, err = self.user_opts.get_type_opt(int, '-num_stim')
           if self.num_stim == None or err: return 1

           # set num_reps list of length num_runs
           self.num_reps, err = self.user_opts.get_type_list(int, '-num_reps',
                                               self.num_stim, 'num_stim')
           if self.num_reps == None or err: return 1
           if self.verb > 1:
               print(UTIL.int_list_string(self.num_reps, '-- num_reps : '))

           # set stim_dur list of length num_stim
           self.stim_dur, err = self.user_opts.get_type_list(float, '-stim_dur',
                                               self.num_stim, 'stim_dur')
           if self.stim_dur == None or err: return 1
           if self.verb > 1:
               print(UTIL.gen_float_list_string(self.stim_dur, '-- stim_dur : '))

           # get any labels
           self.labels, err = self.user_opts.get_string_list('-stim_labels')
           if self.labels and len(self.labels) != self.num_stim:
               print('** error: %d stim classes but %d labels: %s' \
                     % (self.num_stim, len(self.labels), self.labels))
               return 1

           self.min_rest, err = self.user_opts.get_type_opt(float,'-min_rest')
           if self.min_rest == None: self.min_rest = 0
           elif err: return 1


        # end: process OLD and ADV style options
        # -----------------------------------------------------------------

        
        # ----------------------------------------
        # required args - single parameter
        self.num_runs, err = self.user_opts.get_type_opt(int, '-num_runs')
        if self.num_runs == None or err: return 1

        self.prefix, err = self.user_opts.get_string_opt('-prefix')
        if self.prefix == None or err: return 1

        if self.verb > 1:
            print('-- have %d stim and %d runs, prefix = %s' %  \
                  (self.num_stim, self.num_runs, self.prefix))

        # ----------------------------------------
        # required args - (possibly) multiple parameter

        # set run_time list of length num_runs
        self.run_time, err = self.user_opts.get_type_list(float, '-run_time',
                                    self.num_runs, 'num_runs')
        if self.run_time == None or err: return 1
        if self.verb > 1:
            print(UTIL.gen_float_list_string(self.run_time, '-- run_time : '))

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

        # check for not_first or not_last options
        self.nflabs, err = self.user_opts.get_string_list('-not_first')
        if self.nflabs and not err:
           self.notfirst, err = self.labels_to_indices(self.nflabs, onebased=0)
           if err:
              print('** failure processing -not_first %s' % ' '.join(self.nflabs))
              return 1

        self.nllabs, err = self.user_opts.get_string_list('-not_last')
        if self.nllabs and not err:
           self.notlast, err = self.labels_to_indices(self.nllabs, onebased=0)
           if err:
              print('** failure processing -not_last %s' % ' '.join(self.nllabs))
              return 1

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
               print(UTIL.int_list_string(self.max_consec, '-- max_consec : '))

        # gather a list of lists specified by -ordered_stimuli options
        oname = '-ordered_stimuli'
        olist = self.user_opts.find_all_opts(oname)
        for opt in olist:
            # check for non-int: if labels require labels to be already set
            slist, err = self.user_opts.get_type_list(int, opt=opt, verb=0)
            if err:
                # see if they are labels
                slist, err = self.user_opts.get_string_list(opt=opt)
                if err:
                    print('** %s: need list of ints or labels' % oname)
                    return 1
                # see if we have labels to convert into indices
                ilist, err = self.labels_to_indices(slist)
                if err:
                    print('** %s requires indices or known labels' % oname)
                    print('   have: %s %s' % (oname, ' '.join(slist)))
                    return 1
                self.orderlabs.append(slist)
                slist = ilist
            self.orderstim.append(slist)
            if self.verb>1:
               print(UTIL.int_list_string(slist, '-- orderstim (1-based): '))

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
                print('** cannot use both -tr_locked and -t_gran')
                return 1
            if self.tr == 0.0:
                print('** -tr_locked option requires -tr')
                return 1
            self.t_gran = self.tr;
            if self.verb > 1:
                print('++ setting t_gran to TR, %0.1f s' % self.tr)
            # fixed print and added min_rest to durations   8 Jun, 2011
            sd = [dur + self.min_rest for dur in self.stim_dur]
            if not UTIL.vals_are_multiples(self.tr, sd, digits=4):
                print('** want TR-locked, but stim durations are not'  \
                      ' multiples of TR %.3f' % self.tr)
                print('   min_rest + duration(s): %s' % sd)
                return 1

        # if t_gran was set, note the fact, else apply default
        if self.t_gran:
           self.tgset = 1
        else:
           self.t_gran = gDEF_OLD_T_GRAN

        # t_digits must come after t_gran is set
        self.t_digits, err = self.user_opts.get_type_opt(float,'-t_digits')
        if self.t_digits == None:
            if self.t_gran == round(self.t_gran,1):
                self.t_digits = gDEF_OLD_DEC_PLACES
            else:
                self.t_digits = 3
        elif err: return 1

        self.file_3dd_cmd, err = self.user_opts.get_string_opt('-save_3dd_cmd')
        if err: return 1

        fname, err = self.user_opts.get_string_opt('-write_event_list')
        if err: return 1
        self.file_elist = fname

        # have all options

        if self.make_3dd_contr:
            if not self.file_3dd_cmd:
                print('** cannot use -make_3dd_contrasts without -save_3dd_cmd')
                return 1
            # advanced style has labels built in
            elif not self.labels and not self.advanced:
                print('** cannot use -make_3dd_contrasts without -stim_labels')
                return 1

        # is notfirst appropriate?
        if len(self.notfirst) > 0 or len(self.notlast) > 0:
           if self.across_runs or self.max_consec:
              print("** illegal: -not_first/last with -across_runs or -max_consec")
              return 1
           # for now, do not allow this with advanced timing
           # apply: rv, efirst, elast = self.select_first_last_events(reps_list)
           if self.advanced:
              print("** illegal: advanced usage not ready for -not_first/last")
              return 1

        if self.verb > 1:
            print('-- pre_stim_rest=%g, post_stim_rest=%g, seed=%s'     \
                  % (self.pre_stim_rest, self.post_stim_rest, self.seed))
            print('   min_rest=%g, max_rest=%g,'        \
                  % (self.min_rest, self.max_rest),     \
                  'offset=%g, t_gran=%g, t_digits=%d'   \
                  % (self.offset, self.t_gran, self.t_digits))
            if self.labels: print('   labels are: %s' % ', '.join(self.labels))

    def has_any_opts(self, olist):
       """check user_opts for any name in olist"""
       for oname in olist:
          if self.user_opts.find_opt(oname):
             return 1
       return 0

    def apply_opt_timing_class(self, opt):
       """apply all -add_timing_class options

          usage: -add_timing_class label mindur [[mean max] [dtype [tgran]]]

          required: label mindur
          more:     label mindur meandur maxdur
          all:      label mindur meandur maxdur dtype tgran

          sample usage:
             -add_timing_class stimA 3 
             -add_timing_class stimA 3 5 10
             -add_timing_class stimA 3 5 10 dist=decay
             -add_timing_class stimA 1 3  9 dist=decay t_grid=0.1
       """

       params = opt.parlist
       nparm  = len(params)
       oname  = '-add_timing_class'
       error_string = "** bad usage in '%s %s'" % (oname, ' '.join(params))

       if nparm < 2:
          print(error_string)
          return 1

       # get label (verify that it is not a number)
       name = params[0]        
       try:
          ff = float(name)
          print(error_string)
          print('   first parameter should be a timing class label')
          return 1
       except: pass

       # ------------------------------------------------------------
       # get all durations

       try: min_dur = float(params[1])
       except:
          print(error_string)
          print('   second parameter should be a duration')
          return 1

       # just one duration implies all 3
       if nparm == 2:
          mean_dur = max_dur = min_dur

       # two is not allowed
       elif nparm == 3:
          print(error_string)
          print("   either supply a fixed duration or 'min mean max'")
          return 1

       # get all 3
       elif nparm >= 4:
          try:
             mean_dur = float(params[2])
             max_dur  = float(params[3])
          except:
             print(error_string)
             print('   not all 3 durations convert to float')
             return 1

       # ------------------------------------------------------------
       # check non-decreasing, for any durs not equal to -1
       # (min must be set)

       # and check for non-decreasing or negative (allow zero)
       if min_dur < 0:
          print(error_string)
          print("   invalid negative duration")
          return 1

       # meandur is allowed to be -1, for unspecified
       if mean_dur == -1:
          if self.verb > 2:
             print("-- timing class %s will have no fixed mean duration" % name)
       elif min_dur > mean_dur:
          print(error_string)
          print("   durations must have: min <= mean")
          return 1

       # max can also be -1, for unspecified
       if max_dur == -1:
          if self.verb > 2:
             print("-- timing class %s will have no maximum duration" % name)
       # max set, mean unset, so check min vs max
       elif mean_dur == -1 and min_dur > max_dur:
          print(error_string)
          print("   durations must have: min <= max")
          return 1
       # max set, mean set, so check mean vs max
       elif mean_dur != -1 and mean_dur > max_dur:
          print(error_string)
          print("   durations must have: mean <= max")
          return 1

       # ------------------------------------------------------------
       # ready to roll, create the actual timing class instance
       #
       # just pass all other parameters, plus verb
       
       tclass = LRT.TimingClass(name, min_dur, mean_dur, max_dur,
                                params=params[4:], verb=self.verb)
       if tclass.status:
          return 1

       self.tclasses.append(tclass)

       return 0

    def apply_opt_stim_class(self, opt):
       """apply all -add_stim_class options

          usage: -add_stim_class label Nreps stim_class rest_class

          All parameters are required.
       """

       params = opt.parlist
       nparm  = len(params)
       oname  = '-add_stim_class'
       error_string = "** bad usage in '%s %s'" % (oname, ' '.join(params))

       if nparm != 4:
          print(error_string)
          return 1

       # ------------------------------------------------------------
       # check the 4 parameters: name, nreps, stname, rtname

       # get label (verify that it is not a number)
       name = params[0]
       try:
          ff = float(name)
          print(error_string)
          print('   first parameter should be a stim class label')
          return 1
       except: pass

       # get nreps (verify that it is a positive integer)
       try:
          nreps = int(params[1])
       except:
          print(error_string)
          print('   second parameter should be nreps (integer > 0)')
          return 1

       # get stim and rest timing classes, verify existence
       stname = params[2]
       rtname = params[3]

       sclass = self.get_timing_class(stname)
       if sclass == None:
          print(error_string)
          print("** did not find timing class '%s' for stim" % stname)
          return 1

       rclass = self.get_timing_class(rtname)
       if rclass == None:
          print(error_string)
          print("** did not find timing class '%s' for rest" % rtname)
          return 1

       # ------------------------------------------------------------
       # ready to roll, create the actual stim class instance
       sclass = LRT.StimClass(name, nreps, sclass, rclass, verb=self.verb)
       if sclass.status:
          print(error_string)
          return 1

       if self.verb > 1: print("++ adding new Stim class '%s'" % name)
       if self.verb > 2: sclass.show('new stim class')

       self.sclasses.append(sclass)

       return 0

    def get_timing_class(self, name):
        """given name, return an LRT.TimingClass instance from tclasses
           if not found, return None
        """
        for cc in self.tclasses:
           if cc.name == name:
              return cc

        return None

    def get_stim_class(self, name):
        """given name, return an LRT.StimClass instance from sclasses
           if not found, return None
        """
        for cc in self.sclasses:
           if cc.name == name:
              return cc

        return None

    def labels_to_indices(self, labels, print_err=1, onebased=1):
        """convert the labels list into an index list
              - entries must be in self.labels

           self.labels should be populated in either basic or advanced usage

           return index_list, err (0 = success)
        """

        if len(labels) == 0: return [], 0

        if len(self.labels) == 0:
           if print_err: print('** missing labels for conversion')
           return [], 1

        ilist = []
        for lab in labels:
           try: ind = self.labels.index(lab)
           except:
              print('** label to index: label %s is not in label list' % lab)
              return [], 1
           if onebased: ilist.append(ind+1)
           else:        ilist.append(ind)

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
            - write all timing lists to files
        """

        if self.verb > 1: print('\n++ creating timing...')

        if self.seed != None:
            if self.verb > 1: print('++ init random with seed %d' % self.seed)
            random.seed(self.seed)

        # possibly get timing across all runs at once
        if self.across_runs:
            if not UTIL.vals_are_constant(self.run_time):
               print('** sorry, no timing across runs if run lengths vary')
               return 1

            self.stimes = self.make_rand_timing(self.num_runs, self.run_time[0])

            if self.stimes == None: return 1
            if len(self.stimes) != self.num_stim or     \
                    len(self.stimes[0]) != self.num_runs:
                print('** make_rand_timing failure: bad list size')
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
                    print('** rand_timing failure for run %d' % (run+1))
                    return 1

                if len(stim_list) != self.num_stim:
                    print('** bad stim_list length %d' % len(stim_list))
                    return 1

                if len(stim_list[0]) != 1:
                    print('** bad stim_list[0] length %d' % len(stim_list[0]))
                    return 1

                # add lists to stimes
                for index in range(self.num_stim):
                    self.stimes[index].append(stim_list[index][0])

        if self.verb > 2:
            print('--------- final list ---------')
            for s in range(len(self.stimes)):
                for r in range(len(self.stimes[0])):
                    print(UTIL.gen_float_list_string(self.stimes[s][r],
                               '++ stim list[%d][%d] = '%((s+1),r)))
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

    def adv_write_timing_files(self):
        """write files using AfniData class"""

        if self.prefix: prefix = self.prefix    # be sure we have a prefix
        else:           prefix = 'stim_times'

        for sind, sc in enumerate(self.sclasses):
            sc.adata.fname = '%s_%02d_%s.1D' % (prefix, sind+1, sc.name)
            sc.adata.dur_len = -1.0
            if sc.adata.write_as_timing():
               return 1

        return 0

    def write_event_list(self, fname):
       """write event list to given file ('' or '-' or 'stdout' means stdout)

          self.full_event.list = per run list of [cind sdur tdur time]

          eall = [ [ [1 3.2] [4 0.7] .. ]
                   [ [0 4.2] [2 3.1] .. ]
                   [ [4 2.0] [2 1.9] .. ]
                 ]
       """

       # rcr - if no fname, sort times?

       if fname:
          if self.verb > 3:
             print("++ writing event list to file '%s'" % fname)
          fd = open(fname, 'w')
          if not fd:
             print("** failed to open '%s' for writing event list" % fname)
             return 1
       else:
          # if we will write to stderr, flush stdout to synchronize output
          sys.stdout.flush()
          fd = sys.stderr

       # note how much space will we need for '(NAME)'
       maxlen = 2 + max([len(sc.name) for sc in self.sclasses])
       emptystr = '%*s' % (maxlen, '')

       if fd == sys.stderr:
          fd.write('# ==== all events ====\n')
       for rind, erun in enumerate(self.full_event_list):
          fd.write('-- run %d events --\n' % (rind+1))
          fd.write('class %s  start      dur     rest\n' % emptystr)
          fd.write('----- %s  -----     ----     ----\n' % emptystr)
          for event in erun:
             nstr = '(%s)' % self.sclasses[event[0]].name
             fd.write('%3d %-*s   %6.2f   %6.2f   %6.2f\n' \
                   % (event[0], maxlen, nstr, event[3], event[1], event[2]))
          fd.write('\n')

       if fd != sys.stderr:
          fd.close()

       return 0

    def write_timing_files(self):
        """write timing from slist to files from the prefix"""

        if len(self.stimes) != self.num_stim or self.num_stim <= 0:
            print('** bad stim data for file write')
            return 1

        if len(self.fnames) != self.num_stim:
            print('** missing filenames for timing output')
            return 1

        # rcr - 
        # if using AfniData class, then when writing timing:
        #    if durations are constant, do not write them to file
        #      to do this, set mtype to 0 before writing and reset

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
                print("** failed to open timing file '%s'" % fname)
                return 1

            if self.verb > 0: print('writing timing file: %s' % fname)

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
            print('min, max stim times are: %.1f, %.1f' % (mint, maxt))

        return None

    def make_3dd_cmd(self):
        """write sample usage of 3dDeconvolve -nodata to a file"""

        if not self.file_3dd_cmd:       return
        if os.path.isfile(self.file_3dd_cmd):
            print("** 3dD command file '%s' already exists, failing..." \
                        % self.file_3dd_cmd)
            return

        if len(self.fnames) != self.num_stim:
            print('** fname list does not have expected length')
            return

        # set tr and nt for the command
        if self.tr != 0.0:      tr = self.tr
        elif self.t_gran > 1.0: tr = self.t_gran
        else:                   tr = 1.0
        tr = float(tr)  # to be sure

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
            c2 += self.make_3dd_contr_str(self.labels, prefix='    ')
        c2 += '    -x1D X.xmat.1D\n\n'

        first = (polort+1) * len(self.run_time)
        if len(self.fnames) > 1:
            # note first non-poly baseline index
            c2 += '# compute the sum of non-baseline regressors\n'           \
                  "3dTstat -sum -prefix sum_ideal.1D X.xmat.1D'[%d..$]'\n\n" \
                  % first
            ynames = '-ynames SUM - sum_ideal.1D '
            c2 += "# consider plotting the SUM below non-polort regressors\n"\
                  "# command: 1dplot -xlabel Time %sX.xmat.1D'[%d..$]'\n"    \
                  % (ynames, first)
        else:
            c2 += "# consider plotting the desired regressor\n"\
                  "# command: 1dplot -xlabel Time X.xmat.1D'[%d]'\n" % first

        cmd += UTIL.add_line_wrappers(c2)

        if self.verb > 0:
            print("saving 3dD command to file '%s'...\n" % self.file_3dd_cmd)
        if self.verb > 1:
            print(cmd)

        fp = open(self.file_3dd_cmd,"w")
        if not fp:
            print('** failed to open %s for writing 3dDecon cmd'        \
                  % self.file_3dd_cmd)
            return

        fp.write(cmd)
        fp.close()

    def make_3dd_contr_str(self, labels, prefix=''):
        """return a string with all pairwise contrasts"""
        if not labels: return ''
        cstr = ''
        cind = 0
        llist = labels # just to make shorter
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
            if self.verb: print('-- no ISI stats to show')
            return

        print('\nISI statistics :\n\n'                                  \
              'data              min      mean     max     stdev\n'     \
              '-----------     -------  -------  -------  -------')

        print('pre-rest        %7.3f  %7.3f  %7.3f  %7.3f' %            \
              (UTIL.min_mean_max_stdev(self.prerest)))

        print('post-rest       %7.3f  %7.3f  %7.3f  %7.3f\n' %          \
              (UTIL.min_mean_max_stdev(self.postrest)))

        for ind in range(len(self.isi)):
           m0, m1, m2, s = UTIL.min_mean_max_stdev(self.isi[ind])
           print('run #%d ISI      %7.3f  %7.3f  %7.3f  %7.3f' %        \
                 (ind, m0, m1, m2, s))

        allruns = []
        for run in self.isi: allruns.extend(run)

        print('\nall runs ISI    %7.3f  %7.3f  %7.3f  %7.3f\n' %        \
              (UTIL.min_mean_max_stdev(allruns)))

        if self.verb > 3:
           for ind in range(len(self.isi)):
              print(UTIL.gen_float_list_string(self.isi[ind],'ISI #%d : '%ind))
           print('')
                
        if self.verb > 3:
            for ind in range(len(self.isi)):
                self.isi[ind].sort()
                print(UTIL.gen_float_list_string(self.isi[ind],
                           'sorted ISI #%d : '%ind))
                

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
            print('-- make_rand_timing: nruns = %d' % nruns)
            print('   rtime, offset, tinit, tfinal = %g, %g, %g, %g' \
                      % (run_time, offset, tinitial, tfinal))
            print('   reps_list  = %s' % reps_list)
            print(UTIL.gen_float_list_string(sdur_list,'   sdur_list = '))
            # print '   sdur_list = %s' % sdur_list
            print('   tgran = %.4f, verb = %d' % (tgran, verb))

        # verify inputs
        if nruns <= 0:
            print('** make_rand_timing error: nruns = %d' % nruns)
            return
        if run_time <= 0.0 or nstim <= 0 or tinitial < 0 or tfinal < 0:
            print('** bad rand_timing inputs: rtime, nstim, tinit, tfinal = '+ \
                  '%g, %d, %g, %g' % (run_time, nstim, tinitial, tfinal))
            return
        if tgran < gDEF_MIN_T_GRAN:
            print('** time granularity (%f) below minimum (%f)' %   \
                  (tgran, gDEF_MIN_T_GRAN))
        if not reps_list or not sdur_list or       \
           len(reps_list) != nstim or len(sdur_list) != nstim:
            print('** invalid rand_timing input lists: reps, stimes = %s %s '% \
                  (reps_list, sdur_list))
            return

        # steal a copy of sdur_list so that we can trash it with min_rest
        stim_durs = sdur_list[:]

        # if multi runs and ctrl_breaks, add max stim time (-tgran) to tfinal
        if nruns > 1 and ctrl_breaks:
            smax = max(sdur_list)
            if verb > 1:
                print('++ adding max stim (%.1f) to post_stim_rest' % smax)
            tfinal += smax

        # compute total stim time across all runs together
        # min_rest is essentially added to each stimulus time
        stime = 0.0 
        for ind in range(nstim):
            if reps_list[ind] < 0 or stim_durs[ind] < 0:
                print('** invalid negative reps or stimes list entry in %s, %s'\
                      % (reps_list, stim_durs))
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
            print('\n++ total time = %.1f, (stim = %.1f, rest = %.1f)\n'     \
                  '   init rest = %.1f, end rest = %.1f, min rest = %.1f\n'  \
                  '   total ISI = %.1f, rand ISI = %.1f\n'                   \
                  '   rand ISI rest time = %d intervals of %.3f seconds\n' % \
                  (nruns*run_time, stime, tot_rest, tinitial, tfinal, min_rest,
                  isi_rtime, rtime, nrest, tgran))

        if rtime == 0: print('** warning, exactly no time remaining for rest...')
        elif rtime < 0:
            print('** required stim and rest time exceed run length, failing...')
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
                    print('** failure!  total run time has been exeeded...')
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
            print('-- end make_random_timing() --')
            for s in range(nstim):
                for r in range(nruns):
                    print(UTIL.gen_float_list_string(slist[s][r],
                               '++ stim list[%d][%d] = '%((s+1),r)))

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
            if self.verb > 4: print("-- testing olist: %s" % olist)
            if len(olist) < 2:
                print("** orderstim too short: %s" % olist)
                errs += 1
            for val in olist:
                if val < 1 or val > self.num_stim:
                    print("** orderstim list has values outside {1..%d}: %s" \
                          % (self.num_stim, olist))
                    errs += 1
                elif val in used:
                    print("** orderstim '%d' is not unique in lists" % val)
                    errs += 1
                else: used.append(val)

            if errs: continue

            # and check that all reps are the same 
            vlist = [self.num_reps[v-1] for v in olist]
            if not UTIL.vals_are_constant(vlist, cval=None):
                print("** stimuli in order list need constant nreps\n"  \
                      "   orderstim: %s\n"                          \
                      "   nreps: %s\n" % (olist, vlist))
                errs += 1

        if errs:
            if self.verb > 1: print('** valid_orderstim: %d errors' % errs)
            return 0
        else:
            if self.verb > 1:
                print('-- %d orderstim lists are valid' % len(self.orderstim))
            return 1

    def randomize_ordered_events(self, nrest):
        """create a list of randomized events, subject to orderstim

           - return error code (0=success) and event list
        """

        if len(self.orderstim) < 1:
            print('** no orderstim to apply')
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
                if self.verb > 1: print('++ filling unordered stim %d' % sval)
                elist += [sval for i in range(self.num_reps[stim])]

        if self.verb > 3:
            print('++ elist (len %d, unordered): %s' % (len(elist), elist))

        # and append 'first' entries to elist
        for ind in range(len(self.orderstim)):
            olist = self.orderstim[ind]
            sval = olist[0]
            elist += [sval for i in range(self.num_reps[sval-1])]
            if self.verb > 1: print('++ filling order[0] stim %d' % sval)

        if self.verb > 3:
            print('++ elist (len %d, order base): %s' % (len(elist), elist))

        # ------------------------- step 2 ---------------------------
        # randomize this list (other orderlist stimuli are not random)

        UTIL.shuffle(elist)
        if self.verb > 3:
            print('++ elist (len %d, base, rand): %s' % (len(elist), elist))

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
            print('** randomize panic: mergelist/elist mismatch')
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
            print('++ firstdict lookup table: %s' % firstdict)

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
            if val in firstdict:          # then append orderlist
                enew.extend(orderstim[firstdict[val]])
            else: enew.append(val)
        if self.verb > 3:
            print('++ elist with followers (len %d, all stim): %s' \
                  % (len(enew), enew))
        return enew


    def randomize_events(self, nrest):
        """make a list of all event types and then rest, randomize the list
           - this may be subject to lists of ordered stimuli (orderstim)
             (stimuli must be unique across all such lists)
           - return error code (0=success) and event list"""

        if nrest < 0:
            print("** randomize_events: nrest < 0")
            return 1, []

        # might also handle orderstim case
        if len(self.max_consec) == self.num_stim:
            return self.randomize_limited_events(nrest)

        if len(self.orderstim) > 0:
            return self.randomize_ordered_events(nrest)

        # copy reps list, in case it gets temporarily modified
        nstim = self.num_stim
        reps_list = self.num_reps[:]

        # ------------------------------------------------------------
        # allow the user to specify tasks that cannot come first or last
        # (these event types are 0-based)
        rv, efirst, elast = self.select_first_last_events(reps_list)
        if rv: return 1, []

        elist = []
        for stim in range(nstim):
            elist += [(stim+1) for i in range(reps_list[stim])]
        elist.extend([0 for i in range(nrest)])

        if self.verb > 2:
            print('++ elist (len %d, sorted): %s' % (len(elist), elist))

        UTIL.shuffle(elist)

        # if we have already chosen the first or last element, insert
        # (convert to 1-based)
        if efirst >= 0: elist.insert(0, efirst+1)
        if elast >= 0:  elist.append(elast+1)

        if self.verb > 2:
            print('++ elist (len %d, random): %s' % (len(elist), elist))
            for stim in range(nstim):
                print('   number of type %d = %d' % (stim+1,elist.count(stim+1)))

        return 0, elist

    def select_first_last_events(self, reps):
        """apply notfirst and notlast,
           - return status and first and last event indices
           - adjust reps for any applied first and last events
        """

        efirst = -1  # first event type (1-based; 0=unspecified)
        elast = -1   # last  event type (1-based; 0=unspecified)

        if len(self.notfirst) == 0 and len(self.notlast) == 0:
            return 0, efirst, elast

        if len(self.notfirst) > 0:
           efirst = self.get_random_allowed_index(reps, self.notfirst)
           if efirst < 0: 
              print("** failed to get random event index to start run")
              return 1, efirst, elast
           # decrement this event count
           reps[efirst] -= 1
           if self.verb > 3:
              print("-- ignoring %s for first event" % ', '.join(self.nflabs))
           if self.verb > 2:
              print("-- saving %s for first event" % self.labels[efirst])
        if len(self.notlast) > 0:
           elast = self.get_random_allowed_index(reps, self.notlast)
           if elast < 0: 
              print("** failed to get random event index to end run")
              return 1, efirst, elast
           # decrement this event count
           reps[elast] -= 1
           if self.verb > 3:
              print("-- ignoring %s for last event" % ', '.join(self.nllabs))
           if self.verb > 2:
              print("-- saving %s for last event" % self.labels[elast])

        return 0, efirst, elast

    def get_random_allowed_index(self, reps, ignore):
        """given an array of number of repetitions and an ignore list,
           return a random index into reps which is not in ignore list

           return -1 on failure, index on success
        """
        # total indices not in ignore list
        etotal = 0
        for ind in range(len(reps)):
           if ind in ignore: continue
           etotal += reps[ind]

        if etotal == 0:
           return -1

        # choose random index
        rind = int(etotal*random.random())

        # find corresponding index
        rtotal = 0
        for ind in range(len(reps)):
           if ind in ignore: continue
           rtotal += reps[ind]
           if rtotal > rind:
              return ind                    # success

        print("** GRAI index error: reps %s, ignore %s, rind %d, rtotal %d" \
              % (reps, ignore, rind, rtotal))

        return -1

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
            if self.verb > 1: print('-- have limited events w/ordered stim...')
            num_reps = self.num_reps[:]
            for orderlist in self.orderstim:
                # stim in lists are 1-based, num_reps_is 0-based
                for follower in orderlist[1:]: num_reps[follower-1] = 0
        else: 
            num_reps = self.num_reps

        rv, clist = self.make_limited_space_list(num_reps, self.max_consec)

        # next, rewrite as elist   ---> convert to 1-based index list here 
        elist = []
        for entry in clist:
            elist.extend([entry[0]+1 for i in range(entry[1])])
        if self.verb > 2: print("== clist (0-based) %s" % clist)
        if self.verb > 2: print("== elist (1-based) %s" % elist)

        # if orderstim, create a firstdict table and insert follower events
        if len(self.orderstim) > 0:
           if self.verb > 1: print('-- inserting order followers...')
           firstdict = self.make_ordered_firstdict(self.orderstim)
           elist = self.insert_order_followers(elist, firstdict, self.orderstim)

        # finally, merge with rest
        elist = UTIL.random_merge(elist, [0 for i in range(nrest)])

        return 0, elist

    def make_limited_space_list(self, num_reps, max_consec):
        """given a list of num_reps per stim_class and max_consec for each,
           return an event count list of the form [[eind, count] ...]

           return status, clist
        """
        # initial pass on creation of events list
        rv, clist, rtype, rcount =  \
            self.init_limited_event_list(num_reps, max_consec)
        if rv:
            print('** failure of randomize_limited_events')
            return 1, None

        # if we ran out of space for one event type, try to fill
        # prev must be remaining event type
        if rcount > 0:
            rv, clist = self.fill_remaining_limited_space(max_consec,
                                                          clist, rtype, rcount)
            if rv: return 1, None

        if self.verb > 4:
           print('++ MLSL: reps list %s' % clist)

        return 0, clist

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

        if self.verb > 3:
           print('-- init_limited_event_list\n' \
                 '      num_reps   = %s\n'       \
                 '      max_consec = %s' % (num_reps, max_consec))

        remain   = num_reps[:]
        nremain  = sum(remain)
        t_remain = remain[:]            # same, but might get 0 at some index

        # convert max_consec, change any 0 to num_reps
        max_consec = max_consec
        for i, m in enumerate(max_consec):
            if m <= 0: max_consec[i] = num_reps[i]

        if self.verb > 1:
            print("-- limited events reps = %s, max = %s" % (remain, max_consec))

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
                print('** eind too big for t_remain, failing...')
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
            print('** LE failure, nremain = %d, remain = %s' % (nremain,remain))
            return 1, None, 0, 0
        if not self.limited_events_are_valid(clist, max_consec):
            print('** LE failure, some events exceed maximum')
            return 1, None, 0, 0

        if self.verb > 1:
            if nremain == 0: print('-- have 0 events to insert')
            else:
               print('++ have %d events of type %d to insert' % (nremain, prev))
               print('   (note: events are currently 0-based)')

        return 0, clist, prev, nremain

    def fill_remaining_limited_space(self, max_consec, clist, rtype, rcount):
        """Try to add rcount events of type rtype into clist, subject to limits
           imposed by max_consec.

           Note that there should not be space for rtype at the end of the list
           (else it would have already been added).

           Note that if inserting an rtype event between two events of other
           types, then there is actually space for max_consec[rtype] events in
           that position.
        """
        pmax = max_consec[rtype]

        if clist[-1] != [rtype, pmax]:
            print("** messed up max for limited events...")
            return 1, None

        # do we have enough space for remaining events?
        space, npos = self.count_limited_space(clist, rtype, max_consec[rtype])
        if space < rcount:        # we are in trouble
            print('** limited_events: only %d positions for %d inserts' \
                  % (space, rcount))
            return 1, None
        if self.verb > 2:
           print('-- space for insert: %d, positions %d ' % (space, npos))
           if self.verb > 3: print('   clist %s' % clist)

        # okay, insert the remaining rcount events of type rtype
        # rely on rcount being small and do linear searches for each insert
        for ind in range(rcount): # rcount times, insert rtype event
            rind = int(npos*random.random())  # insertion index
            if self.verb > 2:
               print('-- random insertion index %d of %d' % (rind, npos))
            ecount = 0  # count of all potential events

            # find valid position index rind (check positions per cind)
            for cind in range(1, len(clist)):
                c0, c1 = clist[cind][0], clist[cind][1]

                # if rind is too big to insert here, move on
                # (take care if rtype bin is full)

                # if rtype, either no postions or c1+1 postions available
                # (consider after last as one more position)
                if c0 == rtype:
                   if c1 >= pmax: continue   # no postions

                   if rind >= c1+1:
                      rind -= c1+1
                      continue
                   # else, this is good

                # if prev is rtype, one fewer position
                elif clist[cind-1][0] == rtype:
                   if rind >= c1-1:
                       rind -= (c1-1)
                       continue
                   # else, this is good; add 1 to skip first element
                   rind += 1

                # else can go before any
                elif rind >= c1:
                    rind -= c1
                    continue

                # we have a winner!  break and insert
                break

            if cind >= len(clist):
                print('** LE: failed to find insertion index')
                return 1, None

            if self.verb>3: print('++ inserting at index %d (of %d), rind %d' \
                                  % (cind, len(clist), rind))

            # if adjacent to an 'rtype' event, increment event count
            if c0 == rtype:
                clist[cind][1] += 1
            # if at beginning, insert a new event
            elif rind == 0:
                clist.insert(cind, [rtype, 1])
            else: # must break clist[cind] apart
                # decrement by rind, insert new event, insert prev rind
                clist[cind][1] -= rind # decrement and insert by rind
                clist.insert(cind, [rtype, 1])
                clist.insert(cind, [c0, rind])

            # fill cases subtraced pmax+1, so all can add 1 here
            space -= 1
            if self.verb > 5: print('   clist %s' % clist)
            snew, npos = self.count_limited_space(clist, rtype,
                                                  max_consec[rtype])
            if space != snew:
               print("** space count failure, space = %d, count = %d" \
                     % (space, snew))
               return 1, None

        if not self.limited_events_are_valid(clist, max_consec):
            print('** LE fill remain failure, some events exceed maximum')
            return 1, None

        return 0, clist

    def count_limited_space(self, clist, eind, emax):
        """Given an event class list, and event index and the maximum number
           of sequential events of each type, return the number of such events
           that could possibly be added, as well as the number of positions
           available to insert a new event.

           Note: eind must be last event type.
        """
        space = 0
        positions = 0
        if self.verb > 5:
           print('== eind, emax: %d, %d' % (eind, emax))
           print('== CLS clist: %s' % clist)
        for ind in range(1, len(clist)):
            if self.verb > 5: print('  %02d  %s  ' % (ind, clist[ind]), end='')
            # if next is eind, can go up to max
            ncur = clist[ind][1] # note number of current entries
            if clist[ind][0] == eind:
               space += emax - ncur
               if emax - ncur > 0: positions += ncur
               if self.verb > 5: print(space, positions)
               continue
    
            # have ncur postions times emax space, unless prev was eind
            space += ncur*emax
            positions += ncur

            # now adjust if prev was eind:
            #   no additional space before first event
            #   if prev is full, position cannot be before first event
            if clist[ind-1][0] == eind:
               space -= emax
               if clist[ind-1][1] == emax: positions -= 1

            if self.verb > 5: print(space, positions)

        # maybe there is space at the end
        ind = len(clist)-1
        if clist[ind][0] != eind:
           print('** space at end (index %d of %d), this should not happen' \
                 % (ind, len(clist)))
           print('== clist: %s' % clist)
           space += emax
           positions += 1
        
        return space, positions

    def limited_events_are_valid(self, clist, maxlist):
        """verify that each event type in clist does not have too many
           in a row
           return 1 if valid, 0 if not"""
        for ind in range(1, len(clist)):
            # if next is full, skip
            if clist[ind][1] > maxlist[clist[ind][0]]:
                print('** check events failure for maxlist %s' % maxlist)
                print('%s' % clist)
                return 0
        return 1

    def apply_max_rest(self, eventlist):
        """modify eventlist so that there is never more than max_rest rest
           before any event"""

        if self.max_rest <= 0: return

        max_rest = self.max_rest
        if self.min_rest > 0:
            max_rest -= self.min_rest
            if self.verb > 2: print('-- updating max_rest from %g to %g' \
                                    % (self.max_rest, max_rest))

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

        if self.verb > 3: print('++ event rlist', rlist)

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

        if self.verb > 3:
            print('-- maxnrest = %d (%s/%s = %s)' \
                      % (maxnrest, max_rest, self.t_gran, max_rest/self.t_gran))
            print('-- maxrest extendable lists:', elist)
            print('-- maxrest fix lists:', flist)
            fl = [rlist[flist[i]][1]-maxnrest for i in range(len(flist))]
            el = [maxnrest-rlist[elist[i]][1] for i in range(len(elist))]
            print('===========================================================')
            print('fix : %s' % ' '.join(['%s' % v for v in fl]))
            print('===========================================================')
            print('ext : %s' % ' '.join(['%s' % v for v in el]))
            print('===========================================================')

        # maybe there is nothing to do
        if len(flist) < 1:
            if self.verb > 1: print('-- no event block exceeds max rest')
            return
        # note how much rest we need to move around
        ftotal = UTIL.loc_sum([rlist[flist[i]][1]-maxnrest
                                for i in range(len(flist))])
        etotal = UTIL.loc_sum([maxnrest-rlist[elist[i]][1]
                                for i in range(len(elist))])

        if self.verb > 1:
            print('-- shifting %g seconds of excessive rest (%g available)' \
                  % (ftotal*self.t_gran, etotal*self.t_gran))
            if self.verb > 3:
                print('== excessive rest exeeds %d events...' % maxnrest)
                print('   (%s/%s = %s)' % (max_rest, self.t_gran,
                                           max_rest/self.t_gran))

        if etotal < ftotal:
            print("** max_rest too small to adjust (%g < %g)" % (etotal, ftotal))
            return
        elif self.verb > 3:
            print('--> distributing %d rest events among %d slots' \
                  % (ftotal, etotal))

        for fix in flist:
            nshift = rlist[fix][1] - maxnrest
            for count in range(nshift):
                if len(elist) <= 0:
                    print('** panic: empty elist but rest to shift')
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
            print('++ fixed event rlist', rlist)
            if self.verb > 4: print('++ updated eventlist', eventlist)
            etotal = UTIL.loc_sum([maxnrest-rlist[elist[i]][1]
                                  for i in range(len(elist))])
            print('-- updated etotal = %d' % etotal)

    # ======================================================================
    # ADV: functions for modern timing method

    def adv_create_timing(self):
        """create stimulus timing event list
              create list of events (array of event type/duration)
                 - put 2D list of married data elements in StimClass
                   (mdata element = [time [AM list] duration], [t [] d])

           fill with rest
              do for each run separately
                 rest for last stim might be zero
        """

        if self.verb > 1: print('\n++ new: creating timing...')

        if self.seed != None:
            if self.verb > 1: print('++ init with random seed %d' % self.seed)
            random.seed(self.seed)

        # create stimdict, a name -> StimClass index list
        for sind, sc in enumerate(self.sclasses):
           self.stimdict[sc.name] = sind
        if self.verb > 3:
           print('++ have len %d stimdict : %s' \
                 % (len(list(self.stimdict.keys())), self.stimdict))

        # apply any such command line options to the stim classes
        if self.set_max_consec():
           return 1

        # durations are created per stim class even if the timing types are
        # the same (so more time in one class does not cost time in another)
        if LRT.create_duration_lists(self.sclasses, self.num_runs,
                                     self.across_runs, verb=self.verb):
           return 1

        # simlpy randomize order of all events per run
        if self.adv_randomize_event_lists():
           return 1

        # add rest, return final event list
        if self.adv_create_afnidata_list():
           return 1

        if self.file_elist or self.verb > 4:
           if self.write_event_list(self.file_elist):
              return 1

        return 0

    def set_max_consec(self):
       """apply max_consec list here    return 0 on success, 1 on error"""

       nmc = len(self.max_consec)
       nsc = len(self.sclasses)
       if nmc == 0 or nsc == 0: return 0

       if nmc == 1:
          # expand list to linclude all
          self.max_consec = [self.max_consec[0]] * nsc
       elif nmc != nsc:
          print('** have %d -max_consec params, but %d stim classes'%(nmc,nsc))
          return 1

       mclist = self.max_consec

       mcnames = [self.sclasses[ind].name for ind in range(nmc) \
                                          if mclist[ind] > 0]
       print('++ applying max_consec to: %s' % ', '.join(mcnames))
       if len(mcnames) == 0: return 0

       # apply to each stim class
       for sind, sc in enumerate(self.sclasses):
          if mclist[sind] <= 0: continue
          if sc.max_consec > 0:
             print('** max_consec alread set for class %s' % sc.name)
             return 1
          sc.max_consec = mclist[sind]
          if self.verb > 1:
             print('-- setting max_consec = %d for class %s' \
                   % (sc.max_consec, sc.name))

       return 0
        
    def adv_randomize_event_lists(self):
       """For each run (or across), put all events in a list and randomize.

          return an array of run lists of [stimind duration], e.g.

          eall = [ [ [1 3.2] [4 0.7] .. ]
                   [ [0 4.2] [2 3.1] .. ]
                   [ [4 2.0] [2 1.9] .. ]
                 ]
       """

       if self.across_runs: ntodo = 1
       else:                ntodo = self.num_runs

       ordered = self.valid_ordered_stim()
       if ordered < 0: return 1

       eall = []
       for rind in range(ntodo):
          # either applying max_consec or not
          if len(self.max_consec) == len(self.sclasses):
             erun = self.adv_limited_shuffled_run(rind, ordered)
             if erun == None: return 1
          else:
             # consider applying -not_first or -not_last    21 Nov 2017
             #
             # create a reps_list out of sclasses (follower = 0), then apply:
             # rv, efirst, elast = self.select_first_last_events(reps_list)
             # to get sind values, and pop random durlist entry
             #
             # to preserve durlist, maybe in sind loop, pop random index,
             #    add to erun, insert back at same index

             erun = []
             for sind, sc in enumerate(self.sclasses):
                # if ordered stim and sc is not a leader: skip
                if ordered and sc.name in self.osfollow:
                   if self.verb>3:
                      print('-- no shuffle for follower %s' % sc.name)
                   continue
                erun.extend([[sind, dur] for dur in sc.durlist[rind]])
             UTIL.shuffle(erun)

          if self.verb > 1:
             print('-- randomized event lists (no followers) for run %d' % rind)
             self.disp_consec_event_counts([erun])

          # if ordered stim, insert followers
          if ordered:
             erun = self.adv_insert_followers(rind, erun)

          eall.append(erun)

       self.stim_event_list = eall

       if self.across_runs:
          if self.adv_partition_sevents_across_runs():
             return 1

       if self.verb > 4:
          print('-- randomized event lists across all runs')
          self.disp_consec_event_counts(eall)

       return 0

    def disp_consec_event_counts(self, eall):
       # first convert to a list of counts per class, across runs
       numc = len(self.sclasses)

       call = [[] for sind in range(numc)]
       for erun in eall:
          eind = 0
          elen = len(erun)
          while eind < elen:
             ecur = eind
             eind += 1
             cind = erun[ecur][0]
             while eind < elen:
                if erun[eind][0] != cind: break
                eind += 1
             # have (eind-ecur) events of type cind
             call[cind].append(eind-ecur)

       if self.verb > 1:
          print('++ consec event counts:')
       slen = max([len(sc.name) for sc in self.sclasses])

       if self.verb > 4:
          for sind, sc in enumerate(self.sclasses):
             print('-- consec list for #%2d=%-*s (len %3d): %s' \
                   % (sind, slen, sc.name, len(call[sind]), call[sind]))
          print('')

       for sind, sc in enumerate(self.sclasses):
          mi, mn, mx, st = UTIL.min_mean_max_stdev(call[sind])
          if self.verb > 1:
             print('   consec for %*s, sum %4d,'        \
                   ' mmms: %7.3f  %7.3f  %7.3f  %7.3f'  \
                   % (slen, sc.name, sum(call[sind]), mi, mn, mx, st))

    def adv_limited_shuffled_run(self, rind, ordered):
       """return randomized events subject to max_consec
          - if ordered stimuli, do not include followers
       """
       numc = len(self.sclasses)
       if len(self.max_consec) != numc:
          print('** ALSR: bad max_consec list: %s' % self.max_consec)
          return None

       # create a modified reps list that omits followers
       reps = [len(sc.durlist[rind]) for sc in self.sclasses]
       if ordered:
          for sind, sc in enumerate(self.sclasses):
             if sc.name in self.osfollow:
                if self.verb>3: print('-- no consec for follower %s' % sc.name)
                reps[sind] = 0

       # generate a random list of the form [[sind nconsec] ...]
       rv, clist = self.make_limited_space_list(reps, self.max_consec)
       if rv: return None

       clist.pop(0)     # remove initial [-1,0]

       # convert to form [[sind dur] ...]
       enew = []
       for cind, consec in enumerate(clist):
          # append consec[1] durations of type consec[0]
          sind = consec[0]
          sc = self.sclasses[sind]
          for tind in range(consec[1]):
             dur = sc.durlist[rind].pop(0)
             enew.append([sind, dur])

       return enew

    def adv_insert_followers(self, rind, events):
       """given list of [sind, dur], insert appropriate followers

          return new event list"""
       if self.verb > 3:
          print('-- insert followers: starting with %d events' % len(events))
       enew = []
       for event in events:
          ename = self.sclasses[event[0]].name
          enew.append(event)

          # possibly insert followers
          if ename in self.osleaders:
             for fname in self.osdict[ename]:
                sind = self.stimdict[fname]
                sc = self.sclasses[sind]
                dur = sc.durlist[rind].pop(0)
                enew.append([sind, dur])

       if self.verb > 3:
          print('-- insert followers: returning %d events' % len(enew))

       return enew

    def valid_ordered_stim(self):
       oname = '-ordered_stimuli'

       nostim = len(self.orderstim)
       nolabs = len(self.orderlabs)
       if nostim > 0 and nolabs == 0:
          print('** advanced usage requires labels in %s' % oname)
          return -1
       if nolabs == 0: return 0

       errs = 0
       alllabs = []
       leaders = []
       followers = []
       for olabs in self.orderlabs:
          if len(olabs) < 2:
             print('** %s requires at least 2 labels, have: %s' \
                   % (oname, ' '.join(olabs)))
             errs += 1
             continue

          lead = olabs[0]
          follow = olabs[1:]
          for lab in olabs:
             if lab not in self.labels:
                print('** invalid label %s in %s %s' \
                      % (lab, oname, ' '.join(olabs)))
                return -1   # fatal
             if lab in alllabs:
                print('** %s: label %s used multiple times' % (oname, lab))
                errs += 1
             alllabs.append(lab)

          # number of events must match
          ls = self.sclasses[self.stimdict[lead]]
          for fname in follow:
             fs = self.sclasses[self.stimdict[fname]]
             if fs.nreps != ls.nreps:
                print('** -ordered_stimuli: classes must have equal num stim')
                print('   %s has %d, %s has %d' \
                      % (lead, ls.nreps, fname, fs.nreps))
                errs += 1

          self.osdict[lead] = follow
          leaders.append(lead)
          followers.extend(follow)

       if errs: return -1

       self.osleaders = leaders
       self.osfollow = followers

       if self.verb > 2:
          print('-- have %d ordered stim, with %d leaders and %d followers' \
                % (nolabs, len(leaders), len(followers)))
          if self.verb > 3:
             print('   leaders  : %s' % ', '.join(leaders))
             print('   followers: %s' % ', '.join(followers))

       return 1


    def adv_partition_sevents_across_runs(self):
       """break stim_event_list into num_runs"""
       if not self.across_runs: return 0

       print('** rcr - adv_partition_sevents_across_runs')

       return 1

    # in across_runs case, we do not yet know run breaks
    def adv_create_afnidata_list(self):
       """create AfniData instances per stim class
          (decide on rest, which implies all timing)
          (across_runs: nothing special to do anymore)

          convert stim_events to full_events (i.e. attach times, base on rest)
          create corresponding mdata lists per stim class
          create corresponding AfniData instances
       """

       # add rest to convert stim_event_list to full_event_list
       if self.adv_create_full_event_list():
          return 1

       # convert to AfniData instances
       if self.adv_create_adata_list():
          return 1

       return 0

    def adv_create_adata_list(self):
       """convert full_event_list to AfniData objects
          given a list of [sind sdur rdur stime] per run
          make a list of mdata = [stime [] sdur] per run, per sclass
          convert mdata to AfniData object, stored as adata in stim class
          - 
       """
       # add any offset
       offset = self.offset
       if offset != 0 and self.verb > 1:
          print('++ applying offset of %g to all times' % offset)
          
       for sind, sc in enumerate(self.sclasses):
          sc.mdata = []
          for rind, erun in enumerate(self.full_event_list):
             srun = [event for event in erun if event[0] == sind]
             mrun = [[se[3]+offset, [], se[1]] for se in srun]
             sc.mdata.append(mrun)
          sc.adata = LAD.AfniData(mdata=sc.mdata, verb=self.verb)
          sc.adata.name = sc.name

       return 0

    def adv_create_full_event_list(self):
       """given stim_event_list of form:
             [ [ [sind dur] [sind dur] ... ]
               [ [sind dur] [sind dur] ... ] ]
          create full_event_list (append rest dur and event time) of form:
             [ [ [sind sdur rdur stime] [sind sdur rdur stime] ... ]
               [ [sind sdur rdur stime] [sind sdur rdur stime] ... ] ]

          return 0 on success
       """

       # input selist, output felist
       selist = self.stim_event_list
       felist = []
       for rind, erun in enumerate(selist):
          rv, ferun = self.adv_stim2full_run_elist(rind, erun)
          if rv: return 1
          if self.verb > 5:
             print('== full timing list: run %d, elist:' % (rind+1))
             for fe in ferun:
                print('   %s' % ', '.join(['%g'%e for e in fe]))
          felist.append(ferun)

       self.full_event_list = felist

       return 0

    def adv_stim2full_run_elist(self, rind, events):
       """convert an event_list line:
               [ [sind dur] [sind dur] ... ] ]
          into a full_event_list line (includes rest dur and stim time):
               [ [sind sdur rdir time] [sind sdur rdir time] ... ]

          - add pre-stim and post-stim rest events, in case they have
            needed timing classes
          - compute total rest time (run_time - sum of stim durs)
               rest time includes pre/post stim rest
          - make list of all used rest classes (for this run)
          - partition total rest time across rest classes
          - distribute across applied rest classes

          return status, full_event_list
       """
       # quick checks:
       nevents = len(events)
       if nevents == 0:
          return 0, events
       elif nevents == 1:
          events[0].append(self.pre_stim_rest)
          return 0, events

       # total stim time before pre-/post-stim rest events are included
       stime = sum([e[1] for e in events])

       # -----------------------------------------------------------------
       # include pre- and post- stimulus rest events :
       #    dur entry comes from options
       #    rest entry comes from timing class
       # (these rest events come and go just within this function)
       # also, self.rand_post_stim_rest affects rest for last stim
       # rcr - explain in help
       events.insert(0, [-2, self.pre_stim_rest])
       events.append([-1, self.post_stim_rest])

       pprest = self.pre_stim_rest+self.post_stim_rest

       rtime = self.run_time[rind] - stime
       randtime = rtime-pprest

       if self.verb > 2 or randtime < 0:
          print('-- run %02d: run time = %g, stime = %g, rtime = %g' \
                % (rind+1, self.run_time[rind], stime, rtime))
          print('   pre-rest = %g, post-rest = %g, random rest = %g' \
                % (self.pre_stim_rest, self.post_stim_rest, randtime))

       if randtime < 0:
          print('** no rest for the wicked!')
          print('   - unsolvable random timing leaves no time for rest')
          print('   - stim time + pre/post-rest time  >  run time')
          return 1, []

       rv, rcounts, rtypes = self.count_all_rest_types(events)
       if rv: return 1, events

       rv, rtimes = self.partition_rest_time(randtime, rcounts, rtypes)
       if rv: return 1, events
       if self.verb > 3:
          print('== partitioned rest:')
          for cind, rc in enumerate(rtypes):
             print("   %4d rest events of type '%s', time = %g" \
                   %(rcounts[cind], rc.name, rtimes[cind]))

       # get rest events, and apply to timing (append accumulated time)
       for rtind, rc in enumerate(rtypes):
          rc.etimes = LRT.random_duration_list(rcounts[rtind], rc,
                                               rtimes[rtind], force_total=1)
          # add option, show rest details
          if self.verb > 5 or self.show_rest_events:
             mesg='run %d rest' % rind
             rc.show_durlist_stats(rc.etimes, mesg=mesg, details=1)

       # quick test
       if self.verb > 3:
          rc = sum([len(rc.etimes) for rc in rtypes])
          ec = len(events)
          print('== have %d rest events and %d stim events' % (rc, ec))

       # for each event, get the current rest time
       rall = 0
       try:
          for eind, event in enumerate(events):
             sind = event[0]

             if eind == nevents and not self.rand_post_stim_rest:
                # no post-stim rest (this is not in any rest list,
                # since it does not match rest class for stim)
                # (also, #nevents is before inserting pre-stim rest)
                rc = g_instant_timing_class
             elif sind == -2: rc = self.pre_stimc
             elif sind == -1: rc = self.post_stimc
             else:            rc = self.sclasses[event[0]].rclass
             rtime = rc.etimes.pop(0)
             event.append(rtime)
       except:
          print('** rest time extraction error')
          return 0, events

       # replace rest time with event time
       ctime = 0
       for event in events:
          # append current time, and increment by stim and rest times
          event.append(ctime)
          ctime += event[1] + event[2]

       # and finally remove the pre- and post- rest events
       if events[0][0] != -2 or events[-1][0] != -1:
          print('** pre/post-stim rest events out of order')
       else:
          events.pop(0)
          events.pop()

       return 0, events

    def partition_rest_time(self, rtot, rcounts, rtypes):
       """given counts and corresponding rest types, partition total rest time
          per rest type

          - try to allocate min time for all types
          - for all types with means, decide on total needed time (above min)
          - if there is not quite enough time, rescale their times
          - if no mean, distribute remaining (still above min)
          - for now, extra rest will trickle to end

          return status, rtimes
       """
          
       ntypes = len(rtypes)

       # -----------------------------------------------------------------
       # compute and distribute min_time
       tot_min = 0
       tot_mean = 0
       rtimes = [0] * ntypes
       for rind, rc in enumerate(rtypes):
          # compute min total, store it and accumuate
          mt = rc.min_dur * rcounts[rind]
          rtimes[rind] = mt
          tot_min += mt

          # track total mean here, too, temporarily just for error messages
          mt = rc.mean_dur * rcounts[rind]
          tot_mean += mt

       remain = rtot - tot_min
       if remain < 0:
          print('** partition rest: insufficient time for minimum rest')
          print('   (avail time = %g  <  min rest = %g, mean rest = %g)' \
                % (rtot, tot_min, tot_mean))
          return 1, []

       if self.verb > 3:
          print("-- part rest: tot %s = min %s + remain %s" \
                % (rtot, tot_min, remain))

       # now go back and track those with applied means (mean > min)
       # - also, note whether we can alter the times later
       tot_mean = 0
       have_rand = 0    # have unrestricted rest
       can_alter = [0] * ntypes   # we can adjust times for these classes
       for rind, rc in enumerate(rtypes):
          offset = rc.mean_dur - rc.min_dur
          if offset != 0:
             can_alter[rind] = 1

          # do we have classes with unrestricted rest?
          if offset < 0: have_rand = 1

          # now skip anything without known space
          if offset <= 0: continue

          # total requested remaining time is mean * nevents
          mt = offset * rcounts[rind]
          tot_mean += mt
       if self.verb > 3:
          print("-- part rest: tot_mean = %s, rand = %s, alter = %s" \
                % (tot_mean, have_rand, can_alter))

       # -----------------------------------------------------------------
       # check for early exit if remain == 0 (warn user if not complete)
       # (remain < 0 was checked above, but be safe)
       if remain <= 0:
          if tot_mean > 0:
             print('** PRT: no time left for above-min mean-based rest')
          if have_rand == 1:
             print('** PRT: no time left for above-min random rest')
          return 0, rtimes

       # -----------------------------------------------------------------
       # possibly scale down offsets for means
       mean_scalar = 1
       if tot_mean > remain:
          mean_scalar = remain * 1.0 / tot_mean
          if self.verb > 2:
             print("++ scaling mean down from %s to %s (by 1/%s)" \
                   % (remain, tot_mean, mean_scalar))
       # or, scale UP if there are no max-less classes
       elif have_rand == 0 and tot_mean > 0:
          # same computation as above means scaling up this time
          mean_scalar = remain * 1.0 / tot_mean
          if self.verb > 2:
             print("++ scaling mean up from %s to %s (by %s)" \
                   % (remain, tot_mean, 1/mean_scalar))

       # possibly warn on rest reduction
       if abs(mean_scalar-1) > 0.05 or tot_mean - remain > 10:
          print('** warning: insufficient time for mean rest\n'
                '            avail = %g, mean rest time = %g' \
                % (remain, tot_mean))

       # possibly provide more details on scalar
       if abs(mean_scalar-1) > 0.05 or self.verb > 2:
          print('-- have %g of %g mean seconds available, scaling by %g' \
                % (remain, tot_mean, mean_scalar))
          if remain > tot_mean and have_rand:
             print('   (no scaling due to unlimited rest classes)')
       
       # -----------------------------------------------------------------
       # now actually distribute into rtimes

       # min time is done, so next distribute postive mean time
       if tot_mean > 0:
          for rind, rc in enumerate(rtypes):
             offset = rc.mean_dur - rc.min_dur
             if offset <= 0: continue
             mt = mean_scalar * offset * rcounts[rind]
             rtimes[rind] += mt
       remain -= tot_mean

       # for have_rand, partition remaining time based on number of events
       if remain > 0 and have_rand > 0:
          nevents = 0
          # first get total number of events
          for rind, rc in enumerate(rtypes):
             if rc.mean_dur < rc.min_dur:
                nevents += rcounts[rind]
          # then partition time based on fractional number of events
          for rind, rc in enumerate(rtypes):
             if rc.mean_dur < rc.min_dur:
                # add in the remaining offset
                rtimes[rind] += remain * rcounts[rind] * 1.0 / nevents

       # any undistributed rest will trickle to post-stim rest

       return 0, rtimes

    def count_all_rest_types(self, elist):
       """given an event list of [etype, dur] elements, get a unique
          list of rest timing classes and their counts

          return status, [rcount], [rtype]
       """

       rcounts = []
       rtypes = []

       nume = len(elist)

       for eind, event in enumerate(elist):
          # there are special cases for which rest class to use
          if eind == 0:
             # pre-stim rest event
             rc = self.pre_stimc
          elif eind == (nume-1):
             # post-stim rest event
             rc = self.post_stimc
          elif eind == (nume-2) and not self.rand_post_stim_rest:
             # use INSTANT rest after last event
             rc = g_instant_timing_class
          else:
             # ahhh, the usual case, get rest class out of stim class
             sc = self.sclasses[event[0]]
             rc = sc.rclass

          # if we already have this one, increment the count, else append
          if rc in rtypes:
             rind = rtypes.index(rc)
             rcounts[rind] += 1
          else:
             rtypes.append(rc)
             rcounts.append(1)

       if self.verb > 2:
          print("-- have %d rest events across %d rest classes" \
                % (sum(rcounts), len(rcounts)))
          if self.verb > 3:
             for cind, rc in enumerate(rtypes):
                print("   %4d rest events of type '%s'"%(rcounts[cind],rc.name))

       return 0, rcounts, rtypes

    def adv_rest_across_runs(self, sall):
       print('** RCR - rest across runs...')
       return []

    def adv_make_3dd_cmd(self):
        """write sample usage of 3dDeconvolve -nodata to a file"""

        if not self.file_3dd_cmd:       return
        if os.path.isfile(self.file_3dd_cmd):
            print("** 3dD command file '%s' already exists, failing..." \
                        % self.file_3dd_cmd)
            return

        nstim = len(self.sclasses)

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

        for ind, sc in enumerate(self.sclasses):
            basis = adv_basis_from_time(sc)
            # if duration modulation as stim durs vary, use AM1
            if basis == 'dmUBLOCK':
               c2 += '    -stim_times_AM1 %d %s %s    \\\n' %                  \
                     (ind+1, sc.adata.fname, basis)
            else:
               c2 += '    -stim_times %d %s %s    \\\n' %                  \
                     (ind+1, sc.adata.fname, basis)
            c2 += '    -stim_label %d %s \\\n' % (ind+1,sc.name)

        # and possibly add contrasts
        if self.make_3dd_contr:
            labels = [sc.name for sc in self.sclasses]
            c2 += self.make_3dd_contr_str(labels, prefix='    ')

        if self.prefix: xmat = 'X.%s.xmat.1D' % self.prefix
        else:           xmat = 'X.xmat.1D'

        c2 += '    -x1D %s\n\n' % xmat

        cmd += UTIL.add_line_wrappers(c2)


        # add additional commands for plotting
        c2 = ''
        first = (polort+1) * len(self.run_time)
        tlabel = '-xlabel Time'
        if len(self.sclasses) > 1:
            # note first non-poly baseline index
            c2 += '# compute the sum of non-baseline regressors\n'           \
                  "3dTstat -sum -prefix sum_ideal.1D %s'[%d..$]'\n\n" \
                  % (xmat, first)
            ynames = '-ynames SUM -'
            c2 += "# consider plotting the SUM below non-polort regressors:\n"\
                  '# (if(0) allows easy copy-and-paste)\n'                    \
                  "if ( 0 ) then\n"                                           \
                  "   1dplot -sepscl %s %s sum_ideal.1D %s'[%d..$]'\n"        \
                  "endif\n\n"                                                 \
                  % (tlabel, ynames, xmat, first)

            # include a more detailed version, just for kicks
            tlabel = '-xlabel "Time index"'
            ynames = '-ynames SUM %s -' \
                     % ' '.join([sc.name for sc in self.sclasses])

            c2 += "# or the more completely labeled command:\n"         \
                  "if ( 0 ) then\n"                                     \
                  "   1dplot -sepscl %s \\\n"                           \
                  "          %s \\\n "                                  \
                  "          sum_ideal.1D %s'[%d..$]'\n"                \
                  "endif\n\n"                                           \
                  % (tlabel, ynames, xmat, first)
        else:
            c2 += "# consider plotting the desired regressor\n"\
                  "# command: 1dplot %s %s'[%d]'\n" % (tlabel, xmat, first)
        c2 += '\n'

        cmd += UTIL.add_line_wrappers(c2)

        if self.verb > 0:
            print("saving 3dD command to file '%s'...\n" % self.file_3dd_cmd)
        if self.verb > 1:
            print(cmd)

        if UTIL.write_text_to_file(self.file_3dd_cmd, cmd):
            print('** failed to write 3dD command to %s' % self.file_3dd_cmd)
            return

def adv_basis_from_time(sclass):
    const, cdur = sclass.adata.check_constant_duration()
    if const:
       if cdur > 1: return "'BLOCK(%g,1)'" % cdur
       return 'GAM'
    else:
       return 'dmUBLOCK'

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

def do_isi_pdfs(argv):
   oname = '-show_isi_f_pdf'
   if oname in argv:
      ind = argv.index(oname)
      try:
         NT = int(argv[ind+1])
         NR = int(argv[ind+2])
      except:
         print('** %s requires 2 integer params, NTask, NRest' % oname)
         return 1
      show_sum_pswr(NT, NR)

   oname = '-show_isi_pdf'
   if oname in argv:
      ind = argv.index(oname)
      try:
         NT = int(argv[ind+1])
         NR = int(argv[ind+2])
      except:
         print('** %s requires 2 integer params, NTask, NRest' % oname)
         return 1
      show_isi_pdf(NT, NR)

   return 0

def show_isi_pdf(T, R):
   """akin to show_sum_pswr, but init and accumulate:

      P(R=0) = T/(T+R)
      P(R=r+1)/P(R=r) = (R-r)/(R+T-1-r)
   """
   pcur = 1.0*T/(T+R)
   cump = pcur
   rat  = pcur

   print('nstart   prob        inc')
   print('------   ----------  ----------')
   for r in range(0,R+1):
      print("%5d   %-10g   %-10g" % (r, pcur, rat))
      rat = (1.0*R - r) / (R + T - 1 - r)
      pcur *= rat
      cump += pcur
   print('cum result is %g' % cump)

def show_sum_pswr(nT, nR):
   cp = 0.0
   prev = 0
   print('nstart   prob        inc')
   print('------   ----------  ----------')
   for r in range(nR+1):
      p = prob_start_with_R(nT,nR,r)
      cp += p
      # print 'prob at %3d = %g (cum %g)' % (r, p, cp)
      if prev == 0: prev = p
      print(r, p, p/prev)
      prev = p
   print('cum result is %g' % cp)


def prob_start_with_R(nA, nB, nS):
    """return the probability of starting nS (out of nB) class B elements
       should equal: choose(nB, nS)*nS! * nA *(nB+nA-nS-1)! / (nA+nB)!
       or: factorial(nB, init=nB-nS+1) * nA / fact(nA+nB, init=nA+nB-nS)

       or: choose(nB,nS)/choose(nA+nB,nS) * nA/(nA+nB-nS)

    """
    return 1.0 * nA * UTIL.factorial(nB,    init=nB-nS+1) \
                    / UTIL.factorial(nA+nB, init=nA+nB-nS)


def main():
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

    # new, advanced way
    if len(timing.sclasses) > 0:
       rv = timing.adv_create_timing()
       if rv:
           UTIL.show_args_as_command(sys.argv,"** failed command:")
           return 1

       if timing.adv_write_timing_files():
          return 1

       if timing.file_3dd_cmd: timing.adv_make_3dd_cmd()  # ignore return value

       if timing.show_timing_stats:
          LAD.show_multi_isi_stats(timing.sclasses, timing.run_time, timing.tr,
                                   verb=(timing.verb>2))

    # old way
    else:
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

       if timing.show_timing_stats: timing.disp_time_stats() # ignore rval

    return 0

if __name__ == "__main__":
    rv = main()
    sys.exit(rv)

