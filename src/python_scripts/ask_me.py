import sys, string

# the main interface for updating SubjProcStream options via questions
def ask_me_subj_proc(proc):

    # start by displaying overview
    print """
    ----------------------------------------------------------------------
    This ask_me dialogue is designed to give users a feel for the basic
    options they may want to supply without using -ask_me.  It reviews only
    a subset of possible options.

    (see "afni_proc.py -help" for the full list :).

    This program minimally requires a list of AFNI datasets to process,
    and a list of stimulus files, if regression is to be run.

    Unless options were previously specified, options for each of these
    processing blocks will be requested:

        setup, tcat, tshift, volreg, blur, mask, scale, regress

    At the end, there will be a chance to store the complete afni_proc.py
    command in a text file for future use.
    ----------------------------------------------------------------------

"""

    # start by looking for datasets
    if len(proc.dsets) == 0:
        print '-- Datasets are required, and must be in the order\n'  \
              '   that they should be processed.\n'

        ndsets = read_one_int('++ enter the number of datasets: ',pos=True)

        print 'ndsets = %d' % ndsets

    print '\n ask_me: not ready yet...\n'
    return 1    # on failure

# read a single integer from a command line, and return it
#       - try until success
#       - pos -> integer must be positive
def read_one_int(prompt, pos=False, nonneg=False):
    while True:
        print prompt,
        words = string.split(sys.stdin.readline())
        if len(words) < 1: continue

        try: val = int(words[0])
        except: continue

        if pos and val <= 0:   continue
        if nonneg and val < 0: continue
    
        return val  # if we're here, all is well
