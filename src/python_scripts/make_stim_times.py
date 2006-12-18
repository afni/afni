#!/usr/bin/env python

import sys, os, string
import option_list
from afni_util import *

g_help_string = """
===========================================================================
Convert a set of 0/1 stim files into a set of stim_times files.

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

Options: -files file1.1D file2.1D ...   : specify stim files
         -prefix PREFIX                 : output prefix for files
         -nruns  NRUNS                  : number of runs
         -nt     NT                     : number of TRs per run
         -tr     TR                     : TR time, in seconds
         -offset OFFSET                 : add OFFSET to all output times
         -verb   LEVEL                  : provide verbose output

examples:

    1. Given 3 stimulus classes, A, B and C, each with a single column
       file spanning 7 runs (with some number of TRs per run), create
       3 stim_times files (stimes.01.1D, stimes.02.1D, stimes.02.1D)
       having the times, in seconds, of the stimuli, one run per row.

            make.stim.files -files stimA.1D stimB.1D stimC.1D   \\
                            -prefix stimes -tr 2.5 -nruns 7 -nt 100

    2. Same as 1, but suppose stim_all.1D has all 3 stim types (so 3 columns).

            make.stim.files -files stim_all.1D -prefix stimes -tr 2.5 \\
                            -nruns 7 -nt 100

    3. Same as 2, but the stimuli were presented at the middle of the TR, so
       add 1.25 seconds to each stimulus time.

            make.stim.files -files stim_all.1D -prefix stimes -tr 2.5 \\
                            -nruns 7 -nt 100 -offset 1.25

    4. An appropriate conversion of stim_files to stim_times for the 
       example in AFNI_data2 (HowTo #5).

            make_stim_times.py -prefix ED_times -tr 1.0 -nruns 10 -nt 272 \\
                               -files misc_files/all_stims.1D

- R Reynolds, Nov 17, 2006
===========================================================================
"""

def get_opts():
    global g_help_string
    okopts = option_list.OptionList('for input')
    okopts.add_opt('-files', -1, [], req=True)
    okopts.add_opt('-prefix', 1, [], req=True)
    okopts.add_opt('-tr', 1, [], req=True)
    okopts.add_opt('-nt', 1, [], req=True)
    okopts.add_opt('-nruns', 1, [], req=True)
    okopts.add_opt('-offset', 1, [])
    okopts.add_opt('-verb', 1, [])
    okopts.trailers = True

    # if argv has only the program name, or user requests help, show it
    if len(sys.argv) <= 1 or '-help' in sys.argv:
        print g_help_string
        return None

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
    
    if verb: print "-d nt = %d, nruns = %d, TR = %f" % (nt, nruns, tr)

    newfile_index = 1   # index over output files
    for file in files:
        mat = read_1D_file(file, -1, verb=verb)
        if not mat:
            print "read_1D_file failed for file: %s" % file
            return
        mat = transpose(mat)

        if len(mat[0]) != nt * nruns:
            print 'warning: file %s has %d entries (expected %d)' % \
                  (file, len(mat[0]), nt*nruns)

        for row in mat:

            newp = "%s.%02d" % (prefix,newfile_index)
            newfile = change_path_basename(file, newp, ".1D")
            if newfile == None: return
            fp = open(newfile, 'w')

            need_ast = True         # '*' filler in case of 1 stim per run, max
            for run in range(nruns):
                rindex = run * nt   # point to start of run

                if not 1 in row[rindex:rindex+nt]:  # no stim in this run
                    fp.write('*\n')
                    continue
                time = 0        # in this run
                nstim = 0       # be sure we have more than 1 somewhere
                for lcol in range(nt):
                    if row[rindex+lcol]:
                        nstim += 1
                        fp.write('%f ' % (time+offset))
                    time += tr
                if need_ast and nstim == 1: # first time of 1 stim, add '*'
                    fp.write('*')
                    need_ast = False
                elif nstim > 1: need_ast = False     # no worries
                fp.write('\n')

            fp.close()
            newfile_index += 1

if __name__ == "__main__":
    proc_mats(get_opts())

