#!/usr/bin/env python

import sys, os, string
import option_list
from afni_util import *

g_help_string = """
Convert a set of binary stim files to a set of stim_times files.

Each input stim file can have a set of columns of stim classes,
     and multiple input files can be used.  Each column of an
     input file is expected to have one row per TR, and a total
     of num_TRs * num_runs rows.

Options: -files file1.1D file2.1D ...  : specify stim files
         -prefix PREFIX                : output prefix for files
         -tr TR                        : TR time, in seconds
         -nruns NRUNS                  : number of runs
         -verb LEVEL                   : provide verbose output

example:

    make.stim.files -files stimA.1D stimB.1D stimC.1D   \\
                    -prefix stimes -TR 2.5 -nruns 7

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

