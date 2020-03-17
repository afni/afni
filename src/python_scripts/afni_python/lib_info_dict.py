

# Make a dictionary of the 3dinfo props of a dset

ver='1.0' ; date='Sept 16, 2019'
# + [PT] 
#
###############################################################################

import sys, os
import json
from afnipy import afni_base as BASE
from afnipy import afni_util as UTIL

list_3dinfo_ignore = [ '-hview', '-h_view',
                       '-hweb',  '-h_web', 
                       '-help',  '-HELP',
                       '-h_aspx', '-h_spx', 
                       '-h_find', '-h_raw', '-h',
                       '-all_opts',
                       '-echo_edu',         # shouldn't happen-- just in case
                       '-atr_delim', '-sb_delim',
                       '-NA_flag', 
                       '-sval_diff', '-val_diff', # require 2 dsets
                       '-same_all_grid',    # these '-same*' require 2 dsets
                       '-same_center',
                       '-same_delta',
                       '-same_dim',
                       '-same_grid',
                       '-same_obl',
                       '-same_orient',
                       '-n[i|j|k',          # woe; luckily, covered under '-n4'
]


#fname = 'MNI152_2009_template_SSW.nii.gz'

def get_all_3dinfo_dset_basic(fname) :
    '''Input: a volume 'pon which to run 3dinfo.

    Output: dictionary whose keys are 3dinfo opt names relevant for a
    single volume (not helpy ones or comparison ones like -same*); the
    '-' part is part of the key here, at the moment.

    There is no filtering of empty fields here-- the bog standard
    outputs are all returned; each value in the dictionary is a list
    of string(s), which may be null.  See
    "get_all_3dinfo_dset_neatly()" for some filtering/proc'ing of
    opts.

    '''

    # ------- Quick check on dset being valid
    cmd = '''3dinfo -prefix {fff}'''.format(fff=fname)
    com = BASE.shell_com(cmd, capture=1, save_hist=0)
    com.run()

    if com.so[0] == 'NO-DSET' :
        print('+* WARNING: No valid dset of that name: {fff}\n.'
              '   Bye.'.format(fff=fname))
        return {}

    # ------- Get list of all opts in 3dinfo
    cmd = '''apsearch -list_popts 3dinfo'''
    com = BASE.shell_com(cmd, capture=1, save_hist=0)
    com.run()

    # filter out some ones we don't want
    list_3dinfo_opts = []
    for x in com.so:
        y = x.strip()
        if not(list_3dinfo_ignore.__contains__(y)) :
            list_3dinfo_opts.append(y)

    dict_info = {}

    for opt in list_3dinfo_opts:
        cmd = '''3dinfo {ooo} {fff}'''.format( ooo=opt, fff=fname )
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        dict_info[opt] = com.so

    return dict_info

# -------------------------------------

def make_neater_3dinfo_dict(dd) :
    '''
    Input: dict dd

    Output: a "neater" version of the values in dd. Specifically:
    + single line strings are processed as follows
      + try splitting at whitespace; 
      + if nothing split, try splitting at |
      + and attach either a list of str (if something split) or else
        just the str itself 
'''

    dd_new = {}

    for key in dd.keys():

        # no items: leave as null string
        if not(dd[key]) :
            dd_new[key] = ''
            
        # single item: see if it should be split
        elif len(dd[key]) == 1: 
            sss = dd[key][0].strip()
            if sss :
                # first try splitting this at whitespaces
                ttt = sss.split()
                # if nothing split, then try to split the original
                # with the pipe char
                if len(ttt) == 1 :
                    ttt = sss.split('|')
                
                # if neither splitter had any effect, just attach the
                # string; otherwise, attach the list
                if len(ttt) == 1 :
                    dd_new[key] = sss
                else:
                    dd_new[key] = ttt

        # multiple items:  just keep as is
        else:
            dd_new[key] = dd[key]

    return dd_new

# -------------------------------------

def get_all_3dinfo_dset_neatly(fname) :
    DD     = get_all_3dinfo_dset_basic(fname)
    DD_new = make_neater_3dinfo_dict(DD)
    return DD_new














