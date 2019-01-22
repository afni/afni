#!/usr/bin/env python

# python3 status: started

from __future__ import print_function

import sys, string, glob, os
import afni_base, afni_util

# the main interface for updating SubjProcStream options via questions
def ask_me_subj_proc(proc):

    # start by displaying overview
    print("""
    -----------------------------------------------------------------------
    ***  The -ask_me method is no longer well supported.  Consider      ***
    ***  following the examples from the -help output.  Or better yet,  ***
    ***  try uber_subject.py, the graphical interface to afni_proc.py.  ***
    -----------------------------------------------------------------------

    This ask_me dialogue is designed to give users a feel for the basic
    options they may want to supply when not using -ask_me.  It reviews
    only a subset of possible options.

    (see "afni_proc.py -help" for the full list :).

    This program minimally requires a list of AFNI datasets to process,
    and a list of stimulus files, if regression is to be run.

    Unless options were previously specified, options for each of these
    processing blocks will be requested:

        setup, tcat, tshift, volreg, blur, mask, scale, regress

    At the end, there will be a chance to store the complete afni_proc.py
    command in a text file for future use.
    -----------------------------------------------------------------------

""")

    # start by looking for datasets
    if len(proc.dsets) == 0:
        if get_datasets(proc): return 1

    print('---------- setup block --------------------------------------\n')
    if not proc.user_opts.find_opt('-subj_id'):
        print("----- requesting '-subj_id' option\n")
        print("    The subject identifier is used in the processing director\n"\
              "    name, as well as names of datasets which are created.\n")
        word = read_one_word('++ please enter a subject ID: ')
        print("    (applying '-subj_id %s')\n" % word)
        proc.user_opts.add_opt('-subj_id', 1, [word],setpar=1)

    if not proc.user_opts.find_opt('-script'):
        print("----- requesting '-script' option\n")
        print("    This is the name of the output processing script.\n")
        word = read_one_word('++ please enter a script name: ')
        print("    (applying '-script %s')\n" % word)
        proc.user_opts.add_opt('-script', 1, [word],setpar=1)

    print('---------- tcat block --------------------------------------\n')
    if not proc.user_opts.find_opt('-tcat_remove_first_trs'):
        print("----- requesting '-tcat_remove_first_trs' option\n")
        print("    This option is used to remove the EPI volumes that\n"   \
              "    were acquired before the scanner reached a steady\n"    \
              "    state.  The default number to remove is 0.\n\n"         \
              "    Note: it is essential that the stimulus timing files\n" \
              "          match the removal of these TRs.\n")
        val = read_one_int('++ please enter the number of TRs to remove: ')
        print("    (applying '-tcat_remove_first_trs %d')\n" % val)
        proc.user_opts.add_opt('-tcat_remove_first_trs', 1, [str(val)],
                                                            setpar=1)

    print('---------- tshift block --------------------------------------\n')
    print("    Slices will be temporally aligned with the start of each TR.\n")
    print("    (applying '-tshift_align_to -tzero 0')\n")

    print('---------- volreg block --------------------------------------\n')
    if not proc.user_opts.find_opt('-volreg_align_to'):
        print("----- requesting '-volreg_align_to' option\n")
        print("    All EPI volumes will be aligned to the volume that was\n"   \
              "    acquired closest to when the anatomical volume was\n"       \
              "    acquired.  Valid inputs are 'first' and 'last', meaning\n"  \
              "    the first volume of the first run, or the last volume of\n" \
              "    the last run.\n")
        word = read_one_word("++ align to 'first' or 'last' volume: ",
                             ['first', 'last'])
        print("    (applying '-volreg_align_to %s')\n" % word)
        proc.user_opts.add_opt('-volreg_align_to', 1, [word],setpar=1)

    print('---------- mask block --------------------------------------\n')
    print("    '3dAutomask -dilate 1' will be used to create a mask for")
    print("    each run.  From those, a union mask will be created.\n")
    print("    (applying default '-mask_dilate 1' and '-mask_type union')\n")

    print('---------- scale block --------------------------------------\n')
    print("    Each EPI run will be scaled to a mean of 100.  Values above\n")
    print("    200 will be truncated to 200.\n")
    print("    (applying default '-scale_max_val 200')\n")

    print('---------- regress block --------------------------------------\n')
    if not proc.user_opts.find_opt('-regress_basis'):
        print("---------- requesting '-regress_basis' option\n")
        print("    The basis function will be convolved with the stimulus\n"  \
          "    onset times within 3dDeconvolve (the same function will be\n"  \
          "    applied to each stimulus class).\n"                            \
          "    Examples: GAM          - the default\n"                      \
          "              BLOCK(5,1)   - for 5 second stimuli\n"             \
          "              BLOCK(20,1)  - for 20 second stimuli\n"            \
          "              TENT(0,14,8) - deconvolution, with an expected\n"  \
          "                             reponse lasting 14 seconds after\n" \
          "                             each stimulus, with beta weights\n" \
          "                             locked to a 2 second TR grid over\n"\
          "                             7 intervals (so 8 tents)\n")
        word = read_one_word('++ please enter a basis function: ')
        print("    (applying -regress_basis '%s')\n" % word)
        proc.user_opts.add_opt('-regress_basis', 1, [word],setpar=1)

    # get stim_times or stim_files
    if not proc.user_opts.find_opt('-regress_stim_times') and   \
       not proc.user_opts.find_opt('-regress_stim_files'):
        if get_stim_files(proc, word): return 1

    if not proc.user_opts.find_opt('-regress_stim_labels'):
        if get_stim_labels(proc,type): return 1

    # apply the new options (and old ones, again)
    if proc.apply_initial_opts(proc.user_opts): return 1

def get_stim_labels(proc, type):
    print("---------- requesting stimulus labels\n")
    print("Stimulus labels should match the order of the -stim_times or\n" \
          "  -stim_files parameters.\n")

    nlabs = read_one_int('++ enter the number of stimulus labels: ', pos=1)

    if proc.verb > 1: print('want %d stim labels\n' % nlabs)

    while 1:  # break when done
        print('Please enter the list of stim labels.\n')
        nremain = nlabs
        labels = []
        while nremain > 0 :
            print('++ (%d remain) please enter label(s): ' % nremain, end='')
            llist = string.split(sys.stdin.readline())
            if not llist or len(llist) <= 0: continue

            labels += llist
            nremain -= len(llist)

        if nremain < 0:
            print('\n** error: too many labels were given (%d > %d)\n' \
                  '   please start again...\n' % (nlabs-nremain, nlabs))
            continue

        break

    print("    (applying: -regress_stim_labels %s)\n" % (' '.join(labels)))
    proc.user_opts.add_opt('-regress_stim_labels', -1, labels, setpar=1)

def get_stim_files(proc, type):
    print("---------- requesting stimuli...\n")
    print("Stimuli can be given as one of\n"                            \
          "  'times' : stim_times files, with times in seconds\n"       \
          "  'files' : 0/1 stim_files, listing which TRs had stimuli\n")
    type = read_one_word("++ apply stim 'files' or 'times': ",
                         ['files','times'])

    nfiles = read_one_int('++ enter the number of files for stim_%s: '%type,
                          pos=1)

    if proc.verb > 1: print('want %d stim_%s\n' % (nfiles,type))

    while 1:  # break when done
        print('Please enter stim names, wildcard use is okay.\n' \
              '(wildcards okay only if order of names is alphabetical)\n')
        nremain = nfiles
        files = []
        while nremain > 0 :
            print('++ (%d remain) please enter file name(s): ' % nremain,end='')
            flist = read_one_glob_file_list()
            if not flist or len(flist) <= 0: continue

            # we could test for valid stim_files/times here

            files += flist
            nremain -= len(flist)

        if nremain < 0:
            print('\n** error: too many names were given (%d > %d)\n' \
                  '   please start again...\n' % (nfiles-nremain, nfiles))
            continue

        break

    print("    (applying: -regress_stim_%s %s)\n" % (type, ' '.join(files)))
    proc.user_opts.add_opt('-regress_stim_%s'%type, -1, files, setpar=1)


def get_datasets(proc):
    print('-- Datasets are required (no -dsets option was given).\n' \
          '   Enter the names in the order that should will be processed.\n')

    ndsets = read_one_int('++ enter the number of datasets: ',pos=1)
    if proc.verb > 1: print('ndsets = %d\n' % ndsets)

    while 1:  # break when done
        print('Please enter dataset names, wildcard use is okay.\n' \
              '(wildcards okay only if order of names is alphabetical)\n')
        nremain = ndsets
        dsets = []
        errs = 0
        while nremain > 0 and errs == 0:
            print('++ (%d remain) please enter dataset name(s): ' \
                  % nremain, end='')
            words = string.split(sys.stdin.readline())
            print() # for separation
            new_dsets = afni_util.list_to_datasets(words, whine=1)

            if new_dsets and len(new_dsets) == 0: continue
            if not new_dsets: continue # good idea?

            # if we god a new dataset list, append it and check for repeats
            dsets += new_dsets
            if not afni_util.uniq_list_as_dsets(dsets, 1):
                errs += 1
            else:
                nremain -= len(new_dsets)
                if proc.verb > 1:
                    print('+d have %d valid dset name(s): %s'%(len(words),words))

        # 2 checks for continuation: errors or too many datasets
        if errs:
            print('\nsorry, there were errors, please start over...\n')
            continue

        if nremain < 0:
            print('\n** error: too many names were given (%d > %d)\n' \
                  '   please start again...\n' % (ndsets-nremain, ndsets))
            continue

        break  # if we get here, things look good

    if proc.verb > 0: print('-- have %d valid datasets (-dsets)\n'%(len(dsets)))
    proc.dsets = dsets

# read a list of file names from the command line
# exand any wildcards
# check each for existence
# return None on error
def read_one_glob_file_list():
    words = string.split(sys.stdin.readline())
    print() # for separation

    # walk through list, instead of using map(), allows error checking
    wlist = []
    for word in words:  # each is either a word or a glob expression
        glist = glob.glob(word)
        if glist:
            glist.sort()
            wlist += glist
        else: wlist.append(word)

    flist = []
    errs = 0
    for file in wlist:  # now check for existence
        if os.path.isfile(file): flist.append(file)
        else:
            print("** no file match for '%s'" % file)
            errs = 1

    if errs: return None

    return flist

# read a single word from a command line, and return it
#       - a list of valid possibilities may be passed
#       - try until success
def read_one_word(prompt, valid_list=None):
    while 1:
        print(prompt, end='')
        words = string.split(sys.stdin.readline())
        print() # for separation
        if len(words) <= 0: continue
        if not valid_list or words[0] in valid_list: break

    return words[0]

# read a single integer from a command line, and return it
#       - try until success
#       - pos -> integer must be positive
def read_one_int(prompt, pos=0, nonneg=0):
    while 1:
        print(prompt, end='')
        words = string.split(sys.stdin.readline())
        print() # for separation
        if len(words) < 1: continue

        if words[0] in ['quit', 'QUIT', 'exit', 'EXIT']:
           print('exiting on user request...\n')
           sys.exit(0)

        try: val = int(words[0])
        except: continue

        if pos and val <= 0:   continue
        if nonneg and val < 0: continue
    
        return val  # if we're here, all is well
