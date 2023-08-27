#!/usr/bin/env python

import sys, os
import copy
import json
from   afnipy  import lib_physio_opts    as lpo
from   afnipy  import lib_physio_util    as lpu
from   afnipy  import lib_format_cmd_str as lfcs

# ===========================================================================

def make_retobj_oname(suffix, retobj, ext='txt'):
    """Construct an output filename for log/text info, from the retobj
obj.

Parameters
----------
suffix : str
    label for type of time series: 'card', 'resp', etc.
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
ext : str
    file extension name; if 'None', then no do not concatenate: 
    '.' + ext.

Returns
-------
oname : str
    name of file to be written

    """

    odir   = retobj.out_dir
    prefix = retobj.prefix
    oname  = suffix

    # add pieces
    if prefix :  oname = prefix + '_' + oname
    if odir   :  oname = odir.rstrip('/') + '/' + oname
    if ext    :  oname = oname + '.' + ext

    return oname

def get_physio_arg_str(argv, do_niceify=True):
    """Parse the command line call argv, which is a list of all the pieces
therein.  The pieces can just be joined together with whitespace, or
more nicely spaced out across multiple lines and vertically aligned
with do_niceify.

Parameters
----------
argv: list
    list of strings, being the pieces of the command line command
do_niceify: bool
    should we space things out nicely across multiple lines?
    (def: of course we should!)

Returns
-------
arg_str : str
    The single string that is the full command line command

    """

    arg_str = ' '.join(argv)

    if do_niceify :
        # might be simpler way to get from parser?
        all_opts = ["-" + key for key in lpo.DEF.keys()]

        # create str
        is_diff, arg_str = \
            lfcs.afni_niceify_cmd_str(arg_str, list_cmd_args = all_opts)

    return arg_str
        
def make_cmd_logs(args_dict, retobj):
    """Output text and json log files of input params and parsed input
dict, respectively.  These fils are written in the output dir, with the
output prefix.

Parameters
----------
args_dict : dict
    dictionary whose keys are option names and values are the
    user-entered values (which might still need separate interpreting
    later)
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs

Returns
-------
0 upon successful completion

    """

    # ----- log the command used

    # get a copy of niceified cmd str (argv) and output name
    cmd_str   = get_physio_arg_str(args_dict['argv'])
    oname_cmd = make_retobj_oname('cmd', retobj, ext='txt')

    # write out
    fff = open(oname_cmd, 'w')
    fff.write(cmd_str)
    fff.close()

    # ----- log the parsed inputs

    # output a copy of parsed info: everything in dict *except* argv
    args_dict_log = copy.deepcopy(args_dict)
    args_dict_log.pop('argv')
    oname_info = make_retobj_oname('info', retobj, ext='json')

    # write out
    ojson = json.dumps( args_dict_log, indent=4 )
    fff = open( oname_info, "w" )
    fff.write( ojson )
    fff.close()

    return 0

def make_ts_obj_review_log(retobj, label=None, ext='txt', 
                           mode='w', verb=0):
    """Write out (or append to) a file of review information for the given
retobj's 'label' ts_obj (like, for label = 'card', 'resp' etc.).  The
extension ext can be changed.  

By default, this function will overwrite any existing file, but
setting mode to 'a' will have it append.

Parameters
----------
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
label : str
    (non-optional kwarg) label to determine which time series is
    processed, and in which sub-object information is stored.  Allowed
    labels are stored in the PO_all_label list.
ext : str
    file extension name; if 'None' or '', then no do not concatenate: 
    '.' + ext.
mode : str
    an allowed mode for open(), for writing the file; default 'w' is
    to overwrite, but changing this to 'a' will switch to appending.

Returns
-------
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """
    
    # get output fname
    phobj  = retobj.data[label]
    suffix = 'review'
    if label :
        suffix = label + '_' + suffix
    oname_rev = make_retobj_oname(suffix, retobj, ext='txt')

    # get dict of items to write
    dict_rev = get_ts_obj_review_info(phobj)
        
    # write out
    fff = open( oname_rev, mode )
    for key in dict_rev:
        fff.write("{:30s} : {:s}\n".format(key, dict_rev[key]))
    fff.close()

    return 0

def get_ts_obj_review_info(phobj):
    """What info do we want from the ts_obj class phobj?  That is defined
here."""


    D = {}

    D['filename'] = phobj.fname
    D['label']    = phobj.label
    
    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['ts_orig num points']     = str(phobj.n_ts_orig)
    D['ts_orig sampling rate']  = str(phobj.samp_rate)
    D['ts_orig sampling freq']  = str(phobj.samp_freq)
    D['ts_orig start time']     = str(phobj.start_time)
    D['ts_orig end time']       = str(phobj.end_time)
    D['ts_orig duration']       = str(phobj.duration_ts_orig)

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['peaks num'] = str(phobj.n_peaks)
    minval, maxval, meanval, stdval = phobj.stats_mmms_peaks
    q25, q50, q75 = phobj.stats_quarts_peaks
    D['peaks min max'] = str("{:9.6f} {:9.6f}".format(minval, maxval))
    D['peaks mean std'] = str("{:9.6f} {:9.6f}".format(meanval, stdval))
    D['peaks q25 q50 q75'] = str("{:9.6f} {:9.6f} {:9.6f}".format(q25, q50, q75))

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['troughs num'] = str(phobj.n_troughs)
    if phobj.n_troughs :
        minval, maxval, meanval, stdval = phobj.stats_mmms_troughs
        q25, q50, q75 = phobj.stats_quarts_troughs
        D['troughs min max'] = str("{:9.6f} {:9.6f}".format(minval, maxval))
        D['troughs mean std'] = str("{:9.6f} {:9.6f}".format(meanval, 
                                                                 stdval))
        D['troughs q25 q50 q75'] = str("{:9.6f} {:9.6f} {:9.6f}".format(q25, 
                                                                        q50, 
                                                                        q75))

    return D
