
# the main interface for updating SubjProcStream options via questions
def ask_me_subj_proc(proc):

    # start by displaying overview
    print """
    ----------------------------------------------------------------------
    This ask_me dialogue is designed to give users a feel for the basic options
    they may want to supply without using -ask_me.  It reviews only a subset of
    possible options (see 'afni_proc.py -help' for the full list :).

    This program minimally requires a list of AFNI datasets to process,
    and a list of stimulus files, if regression is to be run.

    With only those option, the processing blocks:

        setup, tcat, tshift, volreg, blur, mask, scale, regress

    will be run with the following defaults:

        setup:    - use 'SUBJ' for the subject id
                        (option: -subj_id SUBJ)
                  - create a t-shell script called '@proc_subj'
                        (option: -script @proc_subj)
                  - use results directory 'SUBJ.results'
                        (option: -out_dir SUBJ.results)

        tshift:           
        regress:        


        blah blah blah...
"""


    print 'ask_me: not ready yet'
    return 1    # on failure
