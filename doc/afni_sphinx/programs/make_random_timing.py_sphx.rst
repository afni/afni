*********************
make_random_timing.py
*********************

.. _ahelp_make_random_timing.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    ===========================================================================
    Create random stimulus timing files.
    
        The object is to create a set of random stimulus timing files, suitable
        for use in 3dDeconvolve.  These times will not be TR-locked (unless the
        user requests it).  Stimulus presentation times will never overlap, though
        their responses can.
    
    
        ---------------------------------------------------------------------------
        **  There is now basic (old) and advanced usage.  Until I decide how to
            properly merge the help, consider:
    
                make_random_timing.py -help_advanced
    
            Otherwise, this help covers the complete basic usage, followed by
            the "Advanced Usage" (search for that string).  Perhaps in the future
            the basic usage will just be moved below the advanced.
        ---------------------------------------------------------------------------
    
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
    
                make_random_timing.py -num_stim 1 -num_runs 1 -run_time 100  \
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
    
                make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \
                    -stim_dur 3.5 -num_reps 8 -prefix stimesB                \
                    -pre_stim_rest 20 -post_stim_rest 20                     \
                    -stim_labels houses faces donuts                         \
                    -show_timing_stats
    
           Consider adding the -save_3dd_cmd option.
    
        3. Distribute stimuli over all runs at once.
    
           Similar to #2, but distribute the 8 events per class over all 4 runs.
           In #2, each stim class has 8 events per run (so 24 total events).
           Here each stim class has a total of 8 events.  Just add -across_runs.
    
                make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \
                    -stim_dur 3.5 -num_reps 8 -prefix stimesC                \
                    -pre_stim_rest 20 -post_stim_rest 20                     \
                    -across_runs -stim_labels houses faces donuts
    
        4. TR-locked example.
    
           Similar to #2, but make the stimuli TR-locked.  Set the TR to 2.0
           seconds, along with the length of each stimulus event.  This adds
           options -tr_locked and -tr, and requires -stim_dur to be a multiple
           (or equal to) the TR.
    
                make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \
                    -stim_dur 2.0 -num_reps 8 -prefix stimesD                \
                    -pre_stim_rest 20 -post_stim_rest 20 -tr_locked -tr 2.0
    
        5. Esoteric example.
    
           Similar to #2, but require an additional 0.7 seconds of rest after
           each stimulus (exactly the same as adding 0.7 to the stim_dur), set
           the granularity of random sequencing to 0.001 seconds, apply a random
           number seed of 31415, and set the verbose level to 2.
    
           Save a 3dDeconvolve -nodata command in @cmd.3dd .
           
                make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \
                    -stim_dur 3.5 -num_reps 8 -prefix stimesE                \
                    -pre_stim_rest 20 -post_stim_rest 20                     \
                    -min_rest 0.7 -max_rest 7.0                              \
                    -t_gran 0.001 -seed 31415 -verb 2                        \
                    -show_timing_stats -save_3dd_cmd @cmd.3dd
    
        6. Example with varying number of events, durations and run times.
    
        ** Note that this does not make for a balanced design.
    
           Similar to #2, but require each stimulus class to have a different
           number of events.  Class #1 will have 8 reps per run, class #2 will
           have 10 reps per run and class #3 will have 15 reps per run.  The
           -num_reps option takes either 1 or -num_stim parameters.  Here, 3
           are supplied.
    
                make_random_timing.py -num_stim 3 -num_runs 4       \
                    -run_time 200 190 185 225                       \
                    -stim_dur 3.5 4.5 3 -num_reps 8 10 15           \
                    -pre_stim_rest 20 -post_stim_rest 20            \
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
    
                   make_random_timing.py -num_stim 6 -num_runs 3 -run_time 540  \
                       -stim_dur 8 8 16 16 16 16 -num_reps 4 4 5 5 5 5          \
                       -stim_labels A B A1 A2 B1 B2 -min_rest 1.5 -seed 54321   \
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
    
                    timing_tool.py -timing stimesG_A_all.1D -sort  \
                                   -write_timing stimesG_A_sorted.1D
                    timing_tool.py -timing stimesG_B_all.1D -sort  \
                                   -write_timing stimesG_B_sorted.1D
    
              c. To get stim times for the 'main' regressors we need to add 8
                 seconds to every time.  Otherwise, the times will be identical to
                 those in stimesG.a_03_A?.1D (and B).
    
                 There are many ways to add 8 to the timing files.  In this case,
                 just run the program again, with the same seed, but add an offset
                 of 8 seconds to all times.  Then simply ignore the new files for
                 A and B, while keeping those of A1, A2, B1 and B2.
    
                 Also, save the 3dDeconvolve command to run with -nodata.
    
                   make_random_timing.py -num_stim 6 -num_runs 3 -run_time 540  \
                       -stim_dur 8 8 16 16 16 16 -num_reps 4 4 5 5 5 5          \
                       -stim_labels A B A1 A2 B1 B2 -min_rest 1.5 -seed 54321   \
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
    
             make_random_timing.py -num_runs 4 -run_time 240                \
                     -num_stim 5 -num_reps 8                                \
                     -stim_labels question answer score face doughnut       \
                     -stim_dur 2.5 2.5 3 1 1                                \
                     -ordered_stimuli question answer score                 \
                     -pre_stim_rest 20 -post_stim_rest 20                   \
                     -show_timing_stats -seed 31415 -prefix stimesH
    
           To verify the stimulus order, consider using timing_tool.py to convert
           timing files to an event list.  The corresponding command might be the
           following, output on a TR grid of 1.0 s.
    
             timing_tool.py -multi_timing stimesH*.1D                       \
                    -multi_timing_to_events events.stimesH.txt              \
                    -multi_stim_dur 2.5 2.5 3 1 1                           \
                    -tr 1.0 -min_frac 0.5 -per_run -run_len 240
    
    
        9. TR-locked example, fixed seed, limited consecutive events.
    
           Similar to #4, but restrict the number of consecutive events of each
           type to 2.
    
             make_random_timing.py -num_stim 3 -num_runs 2 -run_time 200     \
                     -stim_dur 2.0 -num_reps 10 30 10 -prefix stimesI        \
                     -pre_stim_rest 20 -post_stim_rest 20 -tr_locked -tr 2.0 \
                     -max_consec 2
    
    ----------------------------------------------------------------------
    NOTE: distribution of ISI
    
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
              make_random_timing.py -num_stim 1 -num_runs 1 -run_time 300 \
                  -stim_dur 2 -num_reps 100 -prefix t -verb 0
              ( timing_tool.py -multi_timing t_01.1D -multi_stim_dur 2    \
                  -multi_timing_to_event_list GE:o - -verb 0              \
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
           grep -v prob pure_probs.1D | grep -v result | grep -v '\-----' \
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
    
    
    ===========================================================================
    make_random_timing.py - Advanced Usage
    
       With advanced usage, timing classes are defined for both stimulus periods
       and rest periods.  Timing classes specify duration types that have different
       distributions (min, mean, max and distribution type), which can be applied
       to stimulus events or to rest events.
    
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
    
    
       Every stimulus class type is followed by a fixed rest class type.  So rest
       periods are "attached" to the preceding stimulus periods.  For example, the
       'faces' class events might last for 0.5 - 1.5 seconds, and be uniformly
       distributed (so average = 1s).  Those face events might then be followed by
       0.5 - 8.5 seconds of rest with a 'decay' distribution (so shorter durations
       are more probable than longer durations).
    
       The 'decay' distribution type matches that of the basic (non-advanced) use
       this program.  See "NOTE: distribution of ISI" in the -help output.
    
       -------------------------------------------------------
       Advanced Example 1: basic, with 3 conditions
    
         - This is a simple case with 3 conditions, each having 8 events per run
           of duration 3.5 s.  Rest is randomly distributed using the default
           'decay' distribution (meaning shorter periods are more likely than
           longer ones).  The first and last 20 s is also allocated for rest.
    
         - Do this for 4 runs of length 200 s each.
    
         - Also, do not allow any extra rest (beyond the specified 10 s) after
           the final stimulus event.
    
         - Generate 3dDeconvolve command script (and with pairwise contrasts).
    
         - Show timing statistics.  Save a complete event list (events.adv.1.txt).
    
             make_random_timing.py -num_runs 4 -run_time 200         \
                -pre_stim_rest 10 -post_stim_rest 10                 \
                -rand_post_stim_rest no                              \
                -add_timing_class stim 3.5                           \
                -add_timing_class rest 0 -1 -1                       \
                -add_stim_class houses 10 stim rest                  \
                -add_stim_class faces  10 stim rest                  \
                -add_stim_class donuts 10 stim rest                  \
                -show_timing_stats                                   \
                -write_event_list events.adv.1.txt                   \
                -save_3dd_cmd cmd.3dd.eg1.txt                        \
                -make_3dd_contrasts                                  \
                -seed 31415 -prefix stimes.adv.1
    
    
       -------------------------------------------------------
       Advanced Example 2: varying stimulus and rest timing classes
    
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
    
             make_random_timing.py -num_runs 2 -run_time 400         \
                -pre_stim_rest 10 -post_stim_rest 10                 \
                -rand_post_stim_rest no                              \
                -add_timing_class stima 0.5 3 10                     \
                -add_timing_class stimb 0.1 0.5 3                    \
                -add_timing_class stimc 2                            \
                -add_timing_class stimd 1 2 6 dist=decay_fixed       \
                -add_timing_class resta 0.2 .7 1.2 dist=uniform_rand \
                -add_timing_class restb 0.5 1  1.5 dist=uniform_grid \
                -add_timing_class restc 0 -1 -1                      \
                -add_stim_class houses 20 stima resta                \
                -add_stim_class faces  20 stimb restb                \
                -add_stim_class donuts 20 stimb restb                \
                -add_stim_class tacos  20 stimc restc                \
                -add_stim_class pizza  40 stimd restc                \
                -write_event_list events.adv.2                       \
                -show_timing_stats                                   \
                -seed 31415 -prefix stimes.adv.2
    
    
       -------------------------------------------------------
       Advanced Example 3: ordered event types
    
         - Every cue event is followed by test and then result.
         - Every pizza1 event is followed by pizza2 and then pizza3.
         - The stimc timing class has durations on a grid of 0.1s, rather
           than the default of 0.01s.
         - Write a corresponding 3dDeconvolve script, cmd.3dd.eg3.txt.
    
             make_random_timing.py -num_runs 2 -run_time 300         \
                -pre_stim_rest 10 -post_stim_rest 10                 \
                -rand_post_stim_rest no                              \
                -add_timing_class stima 0.5 3 10                     \
                -add_timing_class stimb 0.1 0.5 3                    \
                -add_timing_class stimc 0.1 2.5 10 t_gran=0.1        \
                -add_timing_class stimd 2                            \
                -add_timing_class resta 0.2 .7 1.2 dist=uniform_rand \
                -add_timing_class restb 0.5 1  1.5 dist=uniform_grid \
                -add_timing_class restc 0 -1 -1                      \
                -add_stim_class cue    20 stima resta                \
                -add_stim_class test   20 stimb restb                \
                -add_stim_class result 20 stimb restb                \
                -add_stim_class pizza1 10 stimc restc                \
                -add_stim_class pizza2 10 stimc restc                \
                -add_stim_class pizza3 10 stimc restc                \
                -add_stim_class salad  10 stimd restc                \
                -write_event_list events.adv.3                       \
                -show_timing_stats                                   \
                -ordered_stimuli cue test result                     \
                -ordered_stimuli pizza1 pizza2 pizza3                \
                -save_3dd_cmd cmd.3dd.eg3.txt                        \
                -seed 31415 -prefix stimes.adv.3
    
       -------------------------------------------------------
       Advanced Example 4: limit consecutive events per class type
    
         - Use simple 1s stim events and random rest (decay).
         - For entertainment, houses/faces and tuna/fish are
           ordered event pairs.
         - Classes houses, faces, tuna and fish are restricted to a
           limit of 3 consecutive events.
         - There is no limit on donuts.   Why would there be?
    
             make_random_timing.py -num_runs 2 -run_time 600         \
                -pre_stim_rest 0 -post_stim_rest 0                   \
                -add_timing_class stim 1                             \
                -add_timing_class rest 0 -1 -1                       \
                -add_stim_class houses 100 stim rest                 \
                -add_stim_class faces  100 stim rest                 \
                -add_stim_class tuna 100 stim rest                   \
                -add_stim_class fish 100 stim rest                   \
                -add_stim_class donuts 100 stim rest                 \
                -ordered_stimuli houses faces                        \
                -ordered_stimuli tuna fish                           \
                -max_consec 3 3 3 3 0                                \
                -show_timing_stats                                   \
                -write_event_list events.adv.4                       \
                -seed 31415 -prefix stimes.adv.4 -verb 2
    
    ---------------------------------------------------------------------
    options (specific to the advanced usage):
    
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
    
    
    ===========================================================================
    general concerns regarding random timing (to be expanded)
    
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
