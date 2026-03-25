#!/usr/bin/env python

# A library of testing functions for evaluating lib_read_head.py.
#
# Basically, we compare with what '3dAttribute -all ...' would output,
# taking that as (mostly) the right answer.
#
# auth: PA Taylor (SSCC, NIMH, NIH, USA)
#
# ============================================================================

import os, sys, copy

from afnipy import lib_read_head as LRH
from afnipy import afni_base     as AB

# ============================================================================

def compare_report_files(reportA, reportB, prefix_diff=None, verb=1):
    """Compare two attribute report files, reportA and reportB, presumably
made by different functionality (like 3dAttribute and lib_read_head.py). 

If a prefix kwarg is provided, then the results of the diff will be
saved to that text file.

This function uses the command line tool 'diff' to do the comparison.

Parameters
----------
reportA : str
    name of a report
reportB : str
    name of another report
prefix_diff : str
    optional name of text file for saving diff info
verb : int
    verbosity level

Returns
-------
is_fail : int
    0 for success, nonzero for failure

    """

    BAD_RETURN = -1

    reportA_full = os.path.expanduser(reportA)
    reportB_full = os.path.expanduser(reportB)
    if not(prefix_diff is None) :
        prefix_diff_full = os.path.expanduser(prefix_diff)

    cmd  = """diff {} {} """.format(reportA_full, reportB_full)
    if not(prefix_diff is None) :
        cmd+= """ > {} """.format(prefix_diff_full)
    com  = AB.shell_com(cmd, capture=1)
    stat = com.run()
    text_list = com.so
    nlines = len(text_list)

    if verb :
        msg = "++ Comparison complete. "
        msg+= "Displaying {} lines of diffs:".format(nlines)
        print(msg)
        print("-"*40)
        print('\n'.join(text_list))
        print("-"*40)

    if not(prefix_diff is None) and not(stat) :
        print("++ Please see saved diff file: {}".format(prefix_diff_full))

    return stat


def create_report_3dAttribute(inset, prefix, verb=1):
    """Provide the name of a BRIK/HEAD file called inset, and then use
'3dAttribute -all ...' to create a report file of attributes, which is
saved as the name prefix.

Parameters
----------
inset : str
    name of input BRIK/HEAD file
prefix : str
    name of output text report file
verb : int
    verbosity level

Returns
-------
is_fail : int
    0 for success, nonzero for failure

    """

    BAD_RETURN = -1

    inset_full  = os.path.expanduser(inset)
    prefix_full = os.path.expanduser(prefix)

    cmd  = """3dAttribute -all {} > {}""".format(inset_full, prefix_full)
    com  = AB.shell_com(cmd, capture=1)
    stat = com.run()
    
    return stat

def create_report_read_head(inset, prefix, verb=1):
    """Provide the name of a BRIK/HEAD file called inset, and then use
lib_read_head.py functions to create a report file of attributes,
which is saved as the name prefix.

Parameters
----------
inset : str
    name of input BRIK/HEAD file
prefix : str
    name of output text report file
verb : int
    verbosity level

Returns
-------
is_fail : int
    0 for success, nonzero for failure

    """

    BAD_RETURN = -1

    inset_full  = os.path.expanduser(inset)
    prefix_full = os.path.expanduser(prefix)

    x = LRH.HeadFile(inset_full)
    is_fail = x.write_report(prefix_full)
    
    return is_fail

# ============================================================================

if __name__ == "__main__" :

    inset = '~/AFNI_data6/FT_analysis/FT.results/FT_anat+orig.HEAD'
    rep00 =  '~/AFNI_data6/FT_analysis/FT.results/report_00_3dA.txt'
    rep01 =  '~/AFNI_data6/FT_analysis/FT.results/report_01_lrh.txt'
    diff1 =  '~/AFNI_data6/FT_analysis/FT.results/DIFF_00_01.txt'
    

    is_fail_00 = create_report_3dAttribute(inset, rep00)
    is_fail_01 = create_report_read_head(inset, rep01)

    is_fail_10 = compare_report_files(rep00, rep01, prefix_diff=diff1)

    print("++ Check out these text files:")
    print("   reports   : {} {}".format(rep00, rep01))
    print("   diff file : {}".format(diff1))
