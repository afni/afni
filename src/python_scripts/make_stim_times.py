#!/usr/bin/env python

import sys, os, string
import option_list, afni_util

g_help_string = """
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
         -nruns  NRUNS                  : number of runs
         -nt     NT                     : number of TRs per run
         -tr     TR                     : TR time, in seconds
         -offset OFFSET                 : add OFFSET to all output times
         -labels LAB1 LAB2 ...          : provide labels for filenames
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

            make_stim_times.py -files stimA.1D stimB.1D stimC.1D   \\
                               -prefix stimes1 -tr 2.5 -nruns 7 -nt 100

    2. Same as 1, but suppose stim_all.1D has all 3 stim types (so 3 columns).

            make_stim_times.py -files stim_all.1D -prefix stimes2 -tr 2.5 \\
                               -nruns 7 -nt 100

    3. Same as 2, but the stimuli were presented at the middle of the TR, so
       add 1.25 seconds to each stimulus time.

            make_stim_times.py -files stim_all.1D -prefix stimes3 -tr 2.5 \\
                               -nruns 7 -nt 100 -offset 1.25

    4. An appropriate conversion of stim_files to stim_times for the 
       example in AFNI_data2 (HowTo #5).  The labels will appear in the
       resulting filenames.

            make_stim_times.py -prefix stim_times -tr 1.0 -nruns 10 -nt 272 \\
                           -files misc_files/all_stims.1D                   \\
                           -labels ToolMovie HumanMovie ToolPoint HumanPoint

    5. Generate files for 2-term amplitude modulation in 3dDeconvolve (i.e.
       for use with -stim_times_AM2).  For any TR that has a non-zero value
       in the input, the output will have that current time along with the
       non-zero amplitude value in the format time:value.

       Just add -amplitudes to any existing command.

            make_stim_times.py -files stim_weights.1D -prefix stimes5 -tr 2.5 \\
                               -nruns 7 -nt 100 -amplitudes

- R Reynolds, Nov 17, 2006
===========================================================================
"""

g_mst_history = """
    make_stim_times.py history:

    1.0  Dec     2006: initial release
    1.1  Feb 02, 2007:
         - only print needed '*' (or two) for first run
         - added options -hist, -ver
    1.2  May 21, 2008:
         - added -amplitudes for Rutvik Desai
    1.3  June 19, 2008:
         - help update and change ':' separator to '*' when using -amplitudes
         - added -show_valid_opts
    1.4  Sep 17, 2008:
         - added -labels option
"""

g_mst_version = "version 1.3, May 21, 2008"

def get_opts():
    global g_help_string
    okopts = option_list.OptionList('for input')
    okopts.add_opt('-help', 0, [],      \
                   helpstr='display program help')
    okopts.add_opt('-hist', 0, [],      \
                   helpstr='display the modification history')
    okopts.add_opt('-show_valid_opts', 0, [],   \
                   helpstr='display all valid options')
    okopts.add_opt('-ver', 0, [],       \
                   helpstr='display the current version number')

    okopts.add_opt('-amplitudes', 0, [],        \
                   helpstr='output is in -stim_times_AM1 format')
    okopts.add_opt('-files', -1, [], req=1,     \
                   helpstr='set the list of input files')
    okopts.add_opt('-prefix', 1, [], req=1,     \
                   helpstr='specify the prefix for output files')
    okopts.add_opt('-tr', 1, [], req=1, \
                   helpstr='set the TR time, in seconds')
    okopts.add_opt('-nt', 1, [], req=1, \
                   helpstr='set the number of TRs per run')
    okopts.add_opt('-nruns', 1, [], req=1,      \
                   helpstr='set the number of runs')
    okopts.add_opt('-offset', 1, [],    \
                   helpstr='specify offset to add to all output times')
    okopts.add_opt('-labels', -1, [],   \
                   helpstr='add these labels to the file names')
    okopts.add_opt('-verb', 1, [],      \
                   helpstr='set the verbosity level')
    okopts.trailers = 1

    # process any optlist_ options
    okopts.check_special_opts(sys.argv)

    # if argv has only the program name, or user requests help, show it
    if len(sys.argv) <= 1 or '-help' in sys.argv:
        print g_help_string
        return

    if '-hist' in sys.argv:                 # print history
        print g_mst_history
        return

    if '-show_valid_opts' in sys.argv:      # show all valid options
        okopts.show('', 1)
        return

    if '-ver' in sys.argv:                  # print version
        print g_mst_version
        return

    opts = option_list.read_options(sys.argv, okopts)

    return opts

def proc_mats(uopts):
    offset = 0.0        # offset for output times

    if uopts == None: return

    opt = uopts.find_opt('-verb')
    if opt:
        try: verb = int(opt.parlist[0])
        except:
            print "** error: verb must be int, have '%s'" % opt.parlist[0]
            return
    else: verb = 0

    opt    = uopts.find_opt('-files')
    files  = opt.parlist
    opt    = uopts.find_opt('-nruns')
    nruns  = int(opt.parlist[0])

    opt    = uopts.find_opt('-offset')
    if opt:
        try: offset = float(opt.parlist[0])
        except:
            print "** error: offset must be float, have '%s'" % opt.parlist[0]
            return

    opt    = uopts.find_opt('-prefix')
    prefix = opt.parlist[0]

    opt    = uopts.find_opt('-tr')
    try: tr = float(opt.parlist[0])
    except:
        print "** error: TR must be float, have '%s'" % opt.parlist[0]
        return
    
    opt    = uopts.find_opt('-nt')
    try: nt = int(opt.parlist[0])
    except:
        print "** error: -nt must be int, have '%s'" % opt.parlist[0]
        return

    opt = uopts.find_opt('-labels')
    if opt: labels = opt.parlist
    else:   labels = []
    nlab = len(labels)

    # print some info
    if verb: print "-- nt = %d, nruns = %d, TR = %s, %d labels" % \
                   (nt, nruns, str(tr), nlab)

    # new option, -amplitudes (columns of amplitudes to Marry, not just 1s)
    use_amp = 0
    opt = uopts.find_opt('-amplitudes')
    if opt:
        use_amp = 1
        if verb: print '-- using amplitudes to Marry with times...'
    
    newfile_index = 1   # index over output files
    for file in files:
        mat = afni_util.read_1D_file(file, -1, verb=verb)
        if not mat:
            print "read_1D_file failed for file: %s" % file
            return
        mat = afni_util.transpose(mat)

        if len(mat[0]) < nt * nruns:
            print '** error: file %s has only %d entries (%d required)' % \
                  (file, len(mat[0]), nt*nruns)
            return
        elif len(mat[0]) > nt * nruns:
            print '** warning: file %s has %d entries (expected only %d)' % \
                  (file, len(mat[0]), nt*nruns)

        for row in mat:

            # maybe add a label to the file name
            if newfile_index <= nlab: label = ".%s" % labels[newfile_index-1]
            elif nlab == 0:           label = ""
            else:
                print "** %d labels given, but we are on column %d..." % \
                      (nlab, newfile_index)
                label = ".label%d" % (newfile_index)

            newp = "%s.%02d%s" % (prefix,newfile_index,label)
            newfile = afni_util.change_path_basename(file, newp, ".1D")
            if newfile == None: return
            fp = open(newfile, 'w')

            need_ast = 1         # '*' filler in case of 1 stim per run, max
            for run in range(nruns):
                rindex = run * nt   # point to start of run

                # if empty run, use placeholders
                if not stim_in_run(row[rindex:rindex+nt], use_amp):
                    if run == 0: fp.write('* *\n')  # first run gets 2
                    else:        fp.write('*\n')
                    continue
                time = 0        # in this run
                nstim = 0       # be sure we have more than 1 somewhere
                for lcol in range(nt):
                    if row[rindex+lcol]:
                        nstim += 1
                        # if -amplitudes write as time:amplitude
                        if use_amp:
                            fp.write('%s*%s ' % \
                                     (str(time+offset),str(row[rindex+lcol])))
                        else:
                            fp.write('%s ' % str(time+offset))
                    time += tr
                if run == 1 and need_ast and nstim == 1:
                    fp.write('*')   # if first time has 1 stim, add '*'
                    need_ast = 0
                elif nstim > 1: need_ast = 0     # no worries
                fp.write('\n')

            fp.close()
            newfile_index += 1

def stim_in_run(values, non_zero):
    """search for any value==1, if non_zero, search for any non-zero

       this might return either 1/0 or True/False, depending on python ver"""

    if not non_zero:
        return 1 in values

    # so any non-zero is okay
    for val in values:
        if val: return 1

    return 0

if __name__ == "__main__":
    proc_mats(get_opts())

