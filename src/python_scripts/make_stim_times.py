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

Options: -files file1.1D file2.1D ...  : specify stim files
         -prefix PREFIX                : output prefix for files
         -tr TR                        : TR time, in seconds
         -nruns NRUNS                  : number of runs
         -verb LEVEL                   : provide verbose output

examples:

    1. Given 3 stimulus classes, A, B and C, each with a single column
       file spanning 7 runs (with some number of TRs per run), create
       3 stim_times files (stimes.01.1D, stimes.02.1D, stimes.02.1D)
       having the times, in seconds, of the stimuli, one run per row.

            make.stim.files -files stimA.1D stimB.1D stimC.1D   \\
                            -prefix stimes -tr 2.5 -nruns 7

    2. Same as 1, but suppose stim_all.1D has all 3 stim types (so 3 columns).

            make.stim.files -files stim_all.1D -prefix stimes -tr 2.5 -nruns 7

- R Reynolds, Nov 17, 2006
===========================================================================
"""

def get_opts():
    global g_help_string
    okopts = option_list.OptionList('for input')
    okopts.add_opt('-files', -1, [], req=True)
    okopts.add_opt('-prefix', 1, [], req=True)
    okopts.add_opt('-tr', 1, [], req=True)
    okopts.add_opt('-nruns', 1, [], req=True)
    okopts.add_opt('-verb', 1, [])
    okopts.trailers = True

    # if argv has only the program name, or user requests help, show it
    if len(sys.argv) <= 1 or '-help' in sys.argv:
        print g_help_string
        return None

    opts = option_list.read_options(sys.argv, okopts)
    return opts

def proc_mats(uopts):
    if uopts == None: return

    opt = uopts.find_opt('-verb')
    if opt: verb = int(opt.parlist[0])
    else:   verb = 0

    opt    = uopts.find_opt('-files')
    files  = opt.parlist
    opt    = uopts.find_opt('-nruns')
    nruns  = int(opt.parlist[0])
    opt    = uopts.find_opt('-prefix')
    prefix = opt.parlist[0]
    opt    = uopts.find_opt('-tr')
    tr     = float(opt.parlist[0])
    
    for file in files:
        mat = read_1D_file(file, -1, verb=verb)
        mat = transpose(mat)
        if not mat:
            print "read_1D_file failed for file: %s" % file
            return

        nt = len(mat[0])/nruns
        if verb: print "file %s, nt = %d, nruns = %d" % (file, nt, nruns)

        newfile_index = 1
        for row in mat:

            newp = "%s.%02d" % (prefix,newfile_index)
            newfile = change_path_basename(file, newp, ".1D")
            if newfile == None: return
            fp = open(newfile, 'w')
            for run in range(nruns):
                rindex = run * nt   # point to start of run

                if not 1 in row[rindex:rindex+nt]:  # no stim in this run
                    fp.write('*\n')
                    continue
                time = 0        # in this run
                for lcol in range(nt):
                    if row[rindex+lcol]: fp.write('%f '%time)
                    time += tr
                fp.write('\n')

            fp.close()
            newfile_index += 1

if __name__ == "__main__":
    proc_mats(get_opts())

