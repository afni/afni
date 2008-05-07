#!/usr/bin/env python

import sys, random
import option_list, afni_util as UTIL

g_help_string = """
===========================================================================
Create random stimulus timing files.

    The object is to create a set of random stimulus timing files, suitable
    for use in 3dDeconvolve.  These times will not be TR-locked (the TR is
    not even specified).  Stimulus presentation times will never overlap,
    though their responses can.

    This can easily be used to generate many sets of random timing files to
    test via "3dDeconvolve -nodata", in order to determine good timing, akin
    to what is done in HowTo #3 using RSFgen.

    given:
        num_stim        - number of stimulus classes
        num_runs        - number of runs
        num_reps        - number of repetitions for each class (same each run)
        stim_time       - length of time for each stimulus
        run_time        - total amount of time, per run
        pre_stim_rest   - time before any first stimulus (assume same each run)
        post_stim_rest  - time after last stimulus (assume same each run)

    This program will create one timing file per stimulus class, num_runs lines
    long, with num_stim stimulus times per line.

    Time for rest will be run_time minus all stimulus time, and can be broken
    into pre_stim_rest, post_stim_rest and randomly distributed rest.  Consider
    the sum, assuming num_reps and stim_time are constant (per run and stimulus
    class).

          num_stim * num_reps * stim_time (total stimulus duration for one run)
        + randomly distributed rest       (surrounding stimuli)
        + pre_stim_rest
        + post_stim_rest                  (account for response time)
        -----------
        = run_time

    Other controlling inputs include:

        across_runs - distribute num_reps across all runs, not per run
        t_gran      - granularity of time, in seconds (default 0.1 seconds)
        min_rest    - time of rest to immediately follow each stimulus
                      (this is internally added to stim_time)
        seed        - optional random number seed

    The method used is similar to that of RSFgen.  For a given run, a list of
    num_reps stimuli for each stimulus class is generated (these intervals are
    each stim_time seconds).  Appended to this is a list of rest intervals
    (each of length t_gran seconds).  This accounts for all time except for
    pre_stim_rest and post_stim_rest.

    This list (of numbers 0..num_stim, where 0 means rest) is then randomized.
    Timing comes from the result.

    Reading the list (still for a single run), times are accumulated, starting
    with pre_stim_rest seconds.  As the list is read, a 0 means add t_gran
    seconds to the current time.  A non-zero value means the given stimulus
    type occurred, so the current time goes into that stimulus file and the
    time is incremented by stim_time seconds.

  * Note that stimulus times will never overlap, though response times can.

  * Note that if TR-locked timing is desired, it can be achieved by having
    stim_time and t_gran being equal to (or a multiple of) the TR.

  * The following options can be specified as one value or as a list:

        -run_time       : time for each run, or a list of run times
        -stim_time      : duration of all stimuli, or a list of each duration
        -num_reps       : nreps for all stimuli, or a list of nreps for each

    Note that varying these parameters can lead to unbalanced designs.  Use
    the list forms with caution.

    Currently, -pre_stim_rest and -post_stim_rest cannot vary.

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
                -stim_time 1.5 -num_reps 20 -pre_stim_rest 10 -prefix stimesA

    2. A typical example.

       Make timing files for 3 stim classes over 4 runs of 200 seconds.  Every
       stimulus class will have 8 events per run, each lasting 3.5 seconds.
       Require 20 seconds of rest before the first stimulus in each run, as
       well as after the last.

       The output will be written to stimesB_01.1D (and 02.1D and 03.1D).

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_time 3.5 -num_reps 8 -prefix stimesB               \\
                -pre_stim_rest 20 -post_stim_rest 20

    3. Distribute stimuli over all runs at once.

       Similar to #2, but distribute the 8 events per class over all 4 runs.
       In #2, each stim class has 8 events per run (so 32 total events).
       Here each stim class has a total of 8 events.  Just add -across_runs.

       Also, add labels for the 3 stimulus classes: houses, faces, donuts.
       They will be appended to each filename.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_time 3.5 -num_reps 8 -prefix stimesC               \\
                -pre_stim_rest 20 -post_stim_rest 20                     \\
                -across_runs -stim_labels houses faces donuts

    4. TR-locked example.

       Similar to #2, but make the stimuli TR-locked.  Assuming the TR is 2.0
       seconds, along with the length of each stimulus event, use the -t_gran
       option to set the granularity of the rest timing grid equal to the TR.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 100  \\
                -stim_time 2.0 -num_reps 8 -prefix stimesD               \\
                -pre_stim_rest 10 -post_stim_rest 10 -t_gran 2.0

    5. Similar to #2, but require an additional 0.7 seconds of rest after
       each stimulus (exactly the same as adding 0.7 to the stim_time), set
       the granularity of random sequencing to 0.001 seconds, apply a random
       number seed of 17, and set the verbose level to 2.
       
            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_time 3.5 -num_reps 8 -prefix stimesE               \\
                -pre_stim_rest 20 -post_stim_rest 20                     \\
                -min_rest 0.7 -t_gran 0.001 -seed 17 -verb 2

    6. Similar to #2, but require each stimulus class to have a different
       number of events.  Class #1 will have 8 reps per run, class #2 will
       have 10 reps per run and class #3 will have 15 reps per run.  The
       -num_reps option takes either 1 or -num_stim parameters.  Here, 3
       are supplied.

    ** Note that this does not make for a balanced design.

            make_random_timing.py -num_stim 3 -num_runs 4 -run_time 200  \\
                -stim_time 3.5 -num_reps 8 10 15 -prefix stimesF         \\
                -pre_stim_rest 20 -post_stim_rest 20


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
        stimulus classes separetly, as a list.

            see also: -across_runs

    -stim_time TIME             : set the amount of time for a single stimulus

        e.g. -stim_time 3.5
        e.g. -stim_time 3.5 1.0 4.2

        This specifies the length of time taken for a single stimulus, in
        seconds.  These stimulation intervals never overlap (with either rest
        or other stimulus intervals).

        If a single TIME parameter is given, it applies to all of the stimulus
        classes.  Otherwise, the user can provide a list of times, one per
        stimulus class.

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

            A: -stim_time 2.0
            B: -stim_time 1.5 -min_rest 0.5

        These have the same effect, but perhaps the user wants to keep the
        terms logically separate.

        However the program simply adds min_rest to each stimulus length.

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
        this time, but the entire stimulation period (described by -stim_time)
        will end before this post_stim_rest period begins.

        For example, if the user provides "-run_time 100", "-stim_time 2.5"
        and "-post_stim_rest 15", then the latest a stimulus could possibly
        occur at is 82.5 seconds into a run.  This would allow 2.5 seconds for
        the stimulus, plus another 15 seconds for the post_stim_rest period.

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
        use also requires -stim_time to be a multiple of the TR.

    -verb LEVEL                 : set the verbose level

        e.g. -verb 2

        The default level is 1, and 0 is consider 'quiet' mode, only reporting
        errors.  The maximum level is currently 4.


- R Reynolds  May 7, 2008               motivated by Ikuko Mukai
===========================================================================
"""

g_history = """
    make_random_timing.py history:

    0.1  May 07  2008: initial release
"""

g_version = "version 1.0, May 7, 2008"

gDEF_VERB       = 1      # default verbose level
gDEF_T_GRAN     = 0.1    # default time granularity, in seconds
gDEF_MIN_T_GRAN = 0.0001 # minimum time granularity, in seconds
gDEF_DEC_PLACES = 1      # decimal places, when printing time

class RandTiming:
    def __init__(self, label):
        # actual stimulus timing lists
        self.stimes     = []

        # general parameters
        self.label = label
        self.verb  = gDEF_VERB
        self.valid_opts = None          # OptionList
        self.user_opts  = None

        # required arguments
        self.num_stim   = 0             # number of stimulus classes
        self.num_runs   = 0             # number of runs
        self.prefix     = None          # prefix for output files
                                        # add .001.1D (stim_index.1D)

        self.run_time   = []            # total time per run (seconds)
        self.num_reps   = []            # number of stimuli, per class (per run)
        self.stim_time  = []            # time of single stimulus (seconds)
                                        #   - per stim class

        # optional arguments
        self.across_runs    = 0         # flag: stimuli span all runs
        self.pre_stim_rest  = 0         # seconds before first stim
        self.post_stim_rest = 0         # seconds after last stim
        self.min_rest = 0.0             # minimum rest after each stimulus
        self.seed     = None            # random number seed
        self.t_gran   = gDEF_T_GRAN     # time granularity for rest
        self.t_digits = gDEF_DEC_PLACES # digits after decimal when showing time
        self.labels   = None            # labels to be applied to filenames

        self.control_breaks = 1         # flag: across runs, add max stim time
                                        # to post_stim_rest

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
        self.valid_opts.add_opt('-stim_time', -1, [], req=1,
                        helpstr='length of each stimulus, in seconds')

        # optional arguments
        self.valid_opts.add_opt('-across_runs', 0, [],
                        helpstr='distribute stim reps across all runs')
        self.valid_opts.add_opt('-min_rest', 1, [],
                        helpstr='minimum rest time after each stimulus')
        self.valid_opts.add_opt('-pre_stim_rest', 1, [],
                        helpstr='time before first stimulus, in seconds')
        self.valid_opts.add_opt('-post_stim_rest', 1, [],
                        helpstr='time after last stimulus, in seconds')
        self.valid_opts.add_opt('-seed', 1, [],
                        helpstr='seed for random number generation (integer)')
        self.valid_opts.add_opt('-stim_labels', -1, [],
                        helpstr='specify stimulus labels for filenames')
        self.valid_opts.add_opt('-t_digits', 1, [],
                        helpstr='digits after decimal, for printing times')
        self.valid_opts.add_opt('-t_gran', 1, [],
                        helpstr='time_granularity for rest, in seconds')

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
           into complete lists (e.g. stim_time is per stim class)"""

        # ----------------------------------------
        # set verb first
        self.verb = self.user_opts.get_type_opt(int, '-verb')
        if self.verb == None: self.verb = gDEF_VERB

        # ----------------------------------------
        # required args - single parameter
        self.num_stim = self.user_opts.get_type_opt(int, '-num_stim')
        if self.num_stim == None: return 1

        self.num_runs = self.user_opts.get_type_opt(int, '-num_runs')
        if self.num_runs == None: return 1

        self.prefix = self.user_opts.get_string_opt('-prefix')
        if self.prefix == None: return 1

        if self.verb > 1:
            print '-- have %d stim and %d runs, prefix = %s' %  \
                  (self.num_stim, self.num_runs, self.prefix)

        # ----------------------------------------
        # required args - (possibly) multiple parameter

        # set num_reps list of length num_runs
        self.run_time = self.user_opts.get_type_list(float, '-run_time',
                                       self.num_runs, 'num_runs', self.verb)
        if self.run_time == None: return 1

        # set num_reps list of length num_runs
        self.num_reps = self.user_opts.get_type_list(int, '-num_reps',
                                      self.num_stim, 'num_stim', self.verb)
        if self.num_reps == None: return 1

        # set stim_time list of length num_stim
        self.stim_time = self.user_opts.get_type_list(float, '-stim_time',
                                       self.num_stim, 'num_stim', self.verb)
        if self.stim_time == None: return 1

        # ----------------------------------------
        # optional arguments (if failure, use default)

        # check for the 'across_runs' flag
        opt = self.user_opts.find_opt('-across_runs')
        if opt: self.across_runs = 1

        self.pre_stim_rest = self.user_opts.get_type_opt(float,'-pre_stim_rest')
        if self.pre_stim_rest == None: self.pre_stim_rest = 0.0

        self.post_stim_rest = self.user_opts.get_type_opt(float, \
                                                          '-post_stim_rest')
        if self.post_stim_rest == None: self.post_stim_rest = 0.0

        self.min_rest = self.user_opts.get_type_opt(float,'-min_rest')
        if self.min_rest == None: self.min_rest = 0

        self.seed = self.user_opts.get_type_opt(float,'-seed')
        if self.seed == None: self.seed = None

        self.t_gran = self.user_opts.get_type_opt(float,'-t_gran')
        if self.t_gran == None: self.t_gran = gDEF_T_GRAN

        self.t_digits = self.user_opts.get_type_opt(float,'-t_digits')
        if self.t_digits == None:
            if self.t_gran == round(self.t_gran,1):
                self.t_digits = gDEF_DEC_PLACES
            else:
                self.t_digits = 3

        self.labels = self.user_opts.get_string_list('-stim_labels')
        if self.labels and len(self.labels) != self.num_stim:
            print '** error: %d stim classes but %d labels: %s' \
                  % (self.num_stim, len(self.lables), self.labels)

        if self.verb > 1:
            print '-- pre_stim_rest = %.1f, post_stim_rest = %.1f, seed = %s' \
                  % (self.pre_stim_rest, self.post_stim_rest, self.seed)
            print '   min_rest = %.1f, t_gran = %.3f, t_digits = %d'          \
                  % (self.min_rest, self.t_gran, self.t_digits)
            if self.labels: print '   labels are: %s' % ', '.join(self.labels)

    def create_timing(self):
        """create stimulus timing files
            - for each run
                - compute random rest time as total run time minus stim time
                  (sum of reps * time, per class) minus pre and post stimulus
                  rest times
                - divide rest be gran to get number of rest trials
                - randomize: sequence of trials per class and rest
                - accumulate time: offset + gran (rest) or stim_time
                - each class gets a list of times
            - write all timing lists to files """

        if self.verb > 1: print '\n++ creating timing...'

        if self.seed != None:
            if self.verb > 1: print '++ init random with seed %d' % self.seed
            random.seed(self.seed)

        # possibly get timing across all runs at once
        if self.across_runs:
            self.stimes = make_rand_timing(self.num_runs, self.run_time[0],
                                self.num_stim, self.num_reps, self.stim_time,
                                self.pre_stim_rest, self.post_stim_rest,
                                self.control_breaks,
                                tgran=self.t_gran, verb=self.verb)

            if self.stimes == None: return 1
            if len(self.stimes) != self.num_stim or     \
                    len(self.stimes[0]) != self.num_runs:
                print '** make_rand_timing failure: bad list size'
                return 1

        # create a timing list for each run separately
        else:

            # init the 3D array: class by run by stim
            # each element (class) is a list (run) of lists (stim)
            # (for each run, append a stim_time list to each class)
            self.stimes = [[] for i in range(self.num_stim)]

            for run in range(self.num_runs):
                # get timing for current run
                stim_list = make_rand_timing(1, self.run_time[run],
                                self.num_stim, self.num_reps, self.stim_time,
                                self.pre_stim_rest, self.post_stim_rest,
                                self.control_breaks,
                                tgran=self.t_gran, verb=self.verb)

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

    def write_timing_files(self):
        """write timing from slist to files from the prefix"""

        if len(self.stimes) != self.num_stim or self.num_stim <= 0:
            print '** bad stim data for file write'
            return 1

        if self.prefix: prefix = self.prefix    # be sure we have a prefix
        else:           prefix = 'stim_times'

        # compute min and max stim times, for verbose output
        mint = max(self.run_time)
        maxt = 0
        for sind in range(self.num_stim):
            stim_all = self.stimes[sind]    # all times for this stim
            stim = sind+1                   # 1-based index

            # open file, write each row (run), and close
            if self.labels:
                fname = '%s_%02d_%s.1D' % (prefix, stim, self.labels[sind])
            else:
                fname = '%s_%02d.1D' % (prefix, stim)
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

        if self.verb > 1:
            print 'min, max stim times are: %.1f, %.1f' % (mint, maxt)

        return None

def make_rand_timing(nruns, run_time, nstim, reps_list, stime_list, tinitial,
                     tfinal, ctrl_breaks=1, tgran=gDEF_T_GRAN, verb=gDEF_VERB):
    """Create random timing for stimuli over all runs (or for a single run),
       where the random placement of stimuli is over all runs at once (so
       the number per run will probably vary).

       This can be called per run, or for all runs at once.

        nruns           : number of runs
        run_time        : total time, per run (in seconds)
        nstim           : number of stimulus classes
        reps_list       : number of repetitions per class (total for all runs)
        stime_list      : duration of each stimulus class
        tinitial        : initial rest time, per run
        tfinal          : final rest time, per run

        ctrl_breaks     : multi-runs: add max stim time to tfinal
        tgran           : granularity of time, to partition rest
        verb            : verbose level
    """

    if verb > 2:
        print '-- make_rand_timing: nruns = %d' % nruns
        print '   rtime, tinit, tfinal = %.1f, %.1f, %.1f'      \
                  % (run_time, tinitial, tfinal)
        print '   reps_list  = %s' % reps_list
        print '   stime_list = %s' % stime_list
        print '   tgran = %.3f, verb = %d' % (tgran, verb)

    # verify inputs
    if nruns <= 0:
        print '** make_rand_timing error: nruns = %d' % nruns
        return
    if run_time <= 0.0 or nstim <= 0 or tinitial < 0 or tfinal < 0:
        print '** bad rand_timing inputs: rtime, nstim, tinit, tfinal = ' +  \
              '%.1f, %d, %.1f, %.1f' % (run_time, nstim, tinitial, tfinal)
        return
    if tgran < gDEF_MIN_T_GRAN:
        print '** time granularity (%f) below minimum (%f)' %   \
              (tgran, gDEF_MIN_T_GRAN)
    if not reps_list or not stime_list or       \
       len(reps_list) != nstim or len(stime_list) != nstim:
        print '** invalid rand_timing input lists: reps, stimes = %s %s ' % \
              (reps_list, stime_list)
        return

    # if multi runs and ctrl_breaks, add max stim time (-tgran) to tfinal
    if nruns > 1 and ctrl_breaks:
        smax = max(stime_list)
        if verb > 1:
            print '++ adding max stim (%.1f) to post_stim_rest' % smax
        tfinal += smax

    # compute total stim time across all runs together
    stime = 0.0 
    for ind in range(nstim):
        if reps_list[ind] < 0 or stime_list[ind] < 0:
            print '** invalid negative reps or stimes list entry in %s, %s' \
                  % (reps_list, stime_list)
            return
        stime += reps_list[ind]*stime_list[ind]

    # compute rest time across all runs together
    rtime = nruns * (run_time - tinitial - tfinal) - stime
    nrest = int(rtime / tgran)

    if verb > 1:
        print '++ total stim_time = %.1f, rest_time = %.1f' % (stime,rtime)
        print '   (rest time = %d intervals of %.3f seconds)' % \
                       (nrest, tgran)
        
    # create a list of all events, repeated stimuli and rest
    elist = []
    for stim in range(nstim):
        elist += [(stim+1) for i in range(reps_list[stim])]
    elist.extend([0 for i in range(nrest)])

    if verb > 2: print '++ elist (len %d, sorted): %s' % (len(elist), elist)

    random.shuffle(elist)  # rcr: replace this, as not all lists are possible

    if verb > 2:
        print '++ elist (len %d, random): %s' % (len(elist), elist)
        for stim in range(nstim):
            print '   number of type %d = %d' % (stim+1,elist.count(stim+1))

    # convert the event list into a list of stimulus lists
    slist = [[[] for i in range(nruns)] for j in range(nstim)]

    ctime = tinitial
    run = 0
    for event in elist:
        # maybe we are done with this run
        if ctime >= run_time - tfinal:
            run += 1
            ctime = tinitial
            if run >= nruns:    # we've gone beyond our bounds
                print '** failure!  total run time has been exeeded...'
                return

        if not event:
            ctime += tgran                  # rest event: tgran seconds
        else:
            eind = event - 1                # event is one more than index
            slist[eind][run].append(ctime)  # append cur time to event's list
            ctime += stime_list[eind]       # increment time by this stim

    if verb > 3:
        for stim in range(nstim):
            print '++ stim list[%d] = %s' % (stim+1,slist[stim])

    return slist

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

    rv = timing.write_timing_files()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command:")
        return rv

    return 0

if __name__ == "__main__":
    rv = process()
    sys.exit(rv)

