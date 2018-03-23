.. _ahelp_make_stim_times.py:

******************
make_stim_times.py
******************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    ===========================================================================
    Convert a set of 0/1 stim files into a set of stim_times files, or
    convert real-valued files into those for use with -stim_times_AM2.
    
    Each input stim file can have a set of columns of stim classes,
         and multiple input files can be used.  Each column of an
         input file is expected to have one row per TR, and a total
         of num_TRs * num_runs rows.
    
         The user must provide -files, -prefix, -nruns, -nt and -tr,
         where NT * NRUNS should equal (or be less than) the number
         of TR lines in each file.
    
    Note: Since the output times are LOCAL (one row per run) in the
         eyes of 3dDeconvolve, any file where the first stimulus is
         the only stimulus in that run will have '*' appended to that
         line, so 3dDeconvolve would treat it as a multi-run file.
    
    Sample stim_file with 3 stim classes over 7 TRs:
    
            0       0       0
            1       0       0
            0       1       0
            0       1       0
            1       0       0
            0       0       0
            0       0       1
    
    Corresponding stim_times files, assume TR = 2.5 seconds:
    
            stim.01.1D:     2.5 10
            stim.02.1D:     5    7.5
            stim.03.1D:     15
    
    ---------------------------------------------------------------------------
    
    Options: -files file1.1D file2.1D ...   : specify stim files
             -prefix PREFIX                 : output prefix for files
             -run_trs TR1 TR2 ...           : specify TRs/run, if they differ
                                              (if same, can use -nruns/-nt)
             -nruns  NRUNS                  : number of runs
             -nt     NT                     : number of TRs per run
             -tr     TR                     : TR time, in seconds
             -offset OFFSET                 : add OFFSET to all output times
             -labels LAB1 LAB2 ...          : provide labels for filenames
             -no_consec                     : do not allow consecutive events
             -show_valid_opts               : output all options
             -verb   LEVEL                  : provide verbose output
    
    complex options:
             -amplitudes                    : "marry" times with amplitudes
    
                    This is to make files for -stim_times_AM1 or -stim_times_AM2
                    in 3dDeconvolve (for 2-parameter amplitude modulation).
    
                    With this option, the output files do not just contain times,
                    they contain values in the format 'time*amplitude', where the
                    amplitude is the non-zero value in the input file.
    
                    For example, the input might look like:
    
                       0
                       2.4
                       0
                       0
                       -1.2
    
                    On a TR=2.5 grid, this would (skip zeros as usual and) output:
    
                       2.5*2.4 10*-1.2
    
    ---------------------------------------------------------------------------
    
    examples:
    
        1. Given 3 stimulus classes, A, B and C, each with a single column
           file spanning 7 runs (with some number of TRs per run), create
           3 stim_times files (stimes.01.1D, stimes.02.1D, stimes.02.1D)
           having the times, in seconds, of the stimuli, one run per row.
    
                make_stim_times.py -files stimA.1D stimB.1D stimC.1D   \
                                   -prefix stimes1 -tr 2.5 -nruns 7 -nt 100
    
        2. Same as 1, but suppose stim_all.1D has all 3 stim types (so 3 columns).
    
                make_stim_times.py -files stim_all.1D -prefix stimes2 -tr 2.5 \
                                   -nruns 7 -nt 100
    
        2b. Same, but maybe the run lengths differ.
    
                make_stim_times.py -files stim_all.1D -prefix stimes2 -tr 2.5 \
                                   -run_trs 100 110 90 100 110 90 100
    
        3. Same as 2, but the stimuli were presented at the middle of the TR, so
           add 1.25 seconds to each stimulus time.
    
                make_stim_times.py -files stim_all.1D -prefix stimes3 -tr 2.5 \
                                   -nruns 7 -nt 100 -offset 1.25
    
        4. An appropriate conversion of stim_files to stim_times for the 
           example in AFNI_data2 (HowTo #5).  The labels will appear in the
           resulting filenames.
    
                make_stim_times.py -prefix stim_times -tr 1.0 -nruns 10 -nt 272 \
                               -files misc_files/all_stims.1D                   \
                               -labels ToolMovie HumanMovie ToolPoint HumanPoint
    
        5. Generate files for 2-term amplitude modulation in 3dDeconvolve (i.e.
           for use with -stim_times_AM2).  For any TR that has a non-zero value
           in the input, the output will have that current time along with the
           non-zero amplitude value in the format time:value.
    
           Just add -amplitudes to any existing command.
    
                make_stim_times.py -files stim_weights.1D -prefix stimes5 -tr 2.5 \
                                   -nruns 7 -nt 100 -amplitudes
    
    - R Reynolds, Nov 17, 2006
    ===========================================================================
