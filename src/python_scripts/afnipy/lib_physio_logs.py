#!/usr/bin/env python

import sys, os
import copy
import json
from   afnipy  import lib_physio_opts    as lpo
from   afnipy  import lib_physio_util    as lpu
from   afnipy  import lib_format_cmd_str as lfcs
from   afnipy  import afni_base          as BASE

# ===========================================================================

def make_pcobj_oname(suffix, pcobj, ext='txt', is_extra=False):
    """Construct an output filename for log/text info, from the pcobj
obj.  Output will be in the odir by default, or odir/*_extras if
is_extra=True.

Parameters
----------
suffix : str
    label for type of time series: 'card', 'resp', etc.
pcobj : pcalc_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of ts_objs
ext : str
    file extension name; if 'None', then no do not concatenate: 
    '.' + ext.
is_extra : bool
    if True, place in odir/*_extras/;  otherwise, place in odir/.

Returns
-------
oname : str
    name of file to be written

    """

    odir   = pcobj.out_dir
    edir   = pcobj.extras_dir
    prefix = pcobj.prefix
    oname  = suffix

    # add pieces
    if prefix :
        oname = prefix + '_' + oname
    if is_extra :
        oname = edir + '/' + oname
    elif odir :
        oname = odir.rstrip('/') + '/' + oname
    if ext :
        oname = oname + '.' + ext

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
        
def save_cmd_opts_parsed(pcobj):
    """Output a json log file of the parsed input dict.  This file is
written in the output dir, with the output prefix.

Parameters
----------
pcobj : pcalc_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of ts_objs (and, most
    relevant here, the args_dict dictionary)

Returns
-------
0 upon successful completion

    """

    # ----- log the parsed inputs

    # output a copy of parsed info: everything in dict *except* argv
    AD = copy.deepcopy(pcobj.args_dict)
    AD.pop('argv')

    # output filename
    oname_info = make_pcobj_oname('info', pcobj, ext='json', is_extra=True)

    # write out
    ojson = json.dumps( AD, indent=4 )
    fff = open( oname_info, "w" )
    fff.write( ojson )
    fff.close()

    return 0

def make_ts_obj_review_log(pcobj, label=None, ext='txt', 
                           mode='w', verb=0):
    """Write out (or append to) a file of review information for the given
pcobj's 'label' ts_obj (like, for label = 'card', 'resp' etc.).  The
extension ext can be changed.  

By default, this function will overwrite any existing file, but
setting mode to 'a' will have it append.

Parameters
----------
pcobj : pcalc_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of ts_objs
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
    tsobj  = pcobj.data[label]
    suffix = 'review'
    if label :
        suffix = label + '_' + suffix
    oname_rev = make_pcobj_oname(suffix, pcobj, ext='txt', is_extra=True)

    # get dict of items to write
    dict_rev = get_ts_obj_review_info(tsobj)
        
    # write out
    fff = open( oname_rev, mode )
    for key in dict_rev:
        fff.write("{:35s} : {:s}\n".format(key, dict_rev[key]))
    fff.close()

    return 0

def get_ts_obj_review_info(tsobj):
    """What info do we want from the ts_obj class tsobj?  That is defined
here."""


    D = {}

    D['filename'] = tsobj.fname
    D['label']    = tsobj.label
    idxA, idxB    = tsobj.indices_vol # MRI dset interval ranges

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['read_in sampling freq']  = str(tsobj.prefilt_init_freq)
    D['prefilt mode']           = str(tsobj.prefilt_mode)
    if tsobj.prefilt_mode != None and tsobj.prefilt_mode != 'none' :
        D['prefilt window, sec'] = str(tsobj.prefilt_win)
    else:
        D['prefilt window, sec'] = 'NA'
    D['is user interact on?'] = str(tsobj.do_interact)
    D['num peaks changed'] = str(tsobj.ndiff_inter_peaks)
    if tsobj.n_troughs :
        D['num troughs changed'] = str(tsobj.ndiff_inter_troughs)

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['ts_orig sampling freq']  = str(tsobj.samp_freq)
    D['ts_orig sampling delt']  = str(tsobj.samp_delt)
    D['ts_orig num points']     = str(tsobj.n_ts_orig)
    D['ts_orig start time']     = str(tsobj.start_time)
    D['ts_orig end time']       = str(tsobj.end_time)
    D['ts_orig duration']       = str(tsobj.duration_ts_orig)

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['dset tr']          = "{:.6f}".format(tsobj.vol_tr)
    D['dset start time']  = "{:.6f}".format(0.0)
    D['dset end time']    = "{:.6f}".format(tsobj.duration_vol)
    D['dset duration']    = "{:.6f}".format(tsobj.duration_vol)

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    D['peak num total'] = str(tsobj.n_peaks)
    minval, maxval, meanval, stdval = tsobj.stats_ival_mmms("peaks")
    q25, q50, q75 = tsobj.stats_ival_percentiles("peaks") # def: quartiles
    D['peak ival min max'] = str("{:8.6f} {:8.6f}".format(minval, 
                                                          maxval))
    D['peak ival mean std'] = str("{:8.6f} {:8.6f}".format(meanval,
                                                           stdval))
    D['peak ival q25 q50 q75'] = str("{:8.6f} {:8.6f} {:8.6f}".format(q25, 
                                                                      q50, 
                                                                      q75))

    D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

    # info over subset of MRI dset duration
    D['peak num over dset'] = str(tsobj.stats_count_pt("peaks", 
                                                       min_idx=idxA,
                                                       max_idx=idxB))
    minval, maxval, meanval, stdval = tsobj.stats_ival_mmms("peaks", 
                                                            min_idx=idxA,
                                                            max_idx=idxB)
    q25, q50, q75 = tsobj.stats_ival_percentiles("peaks", min_idx=idxA,
                                                 max_idx=idxB)
    D['peak ival over dset min max'] = str("{:8.6f} {:8.6f}".format(minval, 
                                                                    maxval))
    D['peak ival over dset mean std'] = str("{:8.6f} {:8.6f}".format(meanval,
                                                                     stdval))
    D['peak ival over dset q25 q50 q75'] = \
        str("{:8.6f} {:8.6f} {:8.6f}".format(q25, 
                                             q50, 
                                             q75))


    if tsobj.n_troughs :
        D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

        D['trough num total'] = str(tsobj.n_troughs)
        minval, maxval, meanval, stdval = tsobj.stats_ival_mmms("troughs")
        q25, q50, q75 = tsobj.stats_ival_percentiles("troughs")
        D['trough ival min max'] = str("{:8.6f} {:8.6f}".format(minval, 
                                                                maxval))
        D['trough ival mean std'] = str("{:8.6f} {:8.6f}".format(meanval, 
                                                                 stdval))
        D['trough ival q25 q50 q75'] = \
            str("{:8.6f} {:8.6f} {:8.6f}".format(q25, 
                                                 q50, 
                                                 q75))

        D[list(D.keys())[-1]] += '\n'  # insert space, attached to last value

        # info over subset of MRI dset duration
        D['trough num over dset'] = str(tsobj.stats_count_pt("troughs", 
                                                             min_idx=idxA,
                                                             max_idx=idxB))
        if tsobj.n_troughs :
            minval, maxval, meanval, stdval = \
                tsobj.stats_ival_mmms("troughs",
                                      min_idx=idxA,
                                      max_idx=idxB)
            q25, q50, q75 = tsobj.stats_ival_percentiles("troughs", 
                                                         min_idx=idxA,
                                                         max_idx=idxB)
            D['trough ival over dset min max'] = \
                str("{:8.6f} {:8.6f}".format(minval, 
                                             maxval))
            D['trough ival over dset mean std'] = \
                str("{:8.6f} {:8.6f}".format(meanval, 
                                             stdval))
            D['trough ival over dset q25 q50 q75'] = \
                str("{:8.6f} {:8.6f} {:8.6f}".format(q25, 
                                                     q50, 
                                                     q75))

    return D

# ----------------------------------------------------------------------------
# save copy of physio_calc.py command that was used

def save_cmd_orig(pcobj, verb=1):
    """Write the physio_calc.py command that was used to the extras_dir."""

    if not(pcobj.args_orig) :
        print("+* WARN: no copy of input cmd to save to file")
        return -1

    # make the filename
    fname = 'pc_cmd.tcsh'
    if pcobj.prefix  :     fname = pcobj.prefix + '_' + fname
    if pcobj.extras_dir :  fname = pcobj.extras_dir + '/' + fname

    # --------------------- make strings of info -----------------------------
    
    sheb_str = '#!/bin/tcsh\n\n'

    comm_str = '# A backup of the physio_calc.py command use to create '
    comm_str+= 'this set of data\n\n'

    time_str = '# created  : {}\n'.format(lpo.now_str)

    # get afni ver, and split immediately at colon to put across 2 lines
    cmd    = '''afni -ver'''
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()
    # Temporary fix to stop program crashing in Spyder
    if len(com.so) > 0: vlist  = com.so[0].split(':')
    else:
        vlist = [0,0]
        vlist[0] = 'Operating system not obtained'
        vlist[1] = "AFNI version not obtained')"

    aver_str = '# AFNI ver : '
    pad      = ' ' * (len(aver_str) - 2)
    aver_str+= vlist[0].strip() + '\n'
    aver_str+= '# {}{}\n\n'.format(pad, vlist[1].strip())

    # make the command str, in a niceified way
    cmd_orig = ' '.join(pcobj.args_orig)
    all_opt  = ['-'+opt for opt in lpo.DEF.keys()]
    is_ok, cmd_str = \
        lfcs.afni_niceify_cmd_str(cmd_orig, list_cmd_args=all_opt)

    fff = open(fname, 'w')
    fff.write(sheb_str)
    fff.write(time_str)
    fff.write(aver_str)
    fff.write(comm_str)
    fff.write(cmd_str)
    fff.close()
    
    if verb :
        print("++ Saved copy of input cmd to file: {}".format(fname))

    return 0


# -------------------------------------------------------------------------
# dump/save peaks and troughs, if (politely) asked

def save_peaks_troughs_file_1D( pcobj, label=None, verb=0 ):
    """Write out peak and/or trough indices to separate simple column
text files.

Parameters
----------
pcobj : pcalc_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of ts_objs
label : str
    (non-optional kwarg) label to determine which time series is
    processed, and in which sub-object information is stored.  Allowed
    labels are stored in the PO_all_label list.

Returns
-------
is_bad : int
    was processing OK (= 0) or not (= nonzero)

    """

    # if no data for this label, then we are done
    if not(pcobj.data[label]) :
        return 0

    # renamings for convenience: no changes expected
    tsobj  = pcobj.data[label]
    prefix = pcobj.prefix
    odir   = pcobj.out_dir
    edir   = pcobj.extras_dir

    # save peaks, if they exist
    if pcobj.save_proc_peaks and tsobj.n_peaks :

        # !!! later, check about overwriting!!!

        # make the filename
        fname = '{}_{}_{}.1D'.format(label, 'peaks', '00')
        if prefix :
            fname = prefix + '_' + fname
        if edir :
            fname = edir + '/' + fname
        elif odir :
            fname = odir + '/' + fname

        # make str: one column of all values
        ostr = '\n'.join([str(val) for val in tsobj.peaks])
        ostr+= '\n'

        # write the file
        fff = open(fname, 'w')
        fff.write(ostr)
        fff.close()
    
        if verb :
            print("++ Saved integer {} trough indices to file: {}"
                  "".format(label, fname))

    # save troughs, if they exist
    if pcobj.save_proc_troughs and tsobj.n_troughs :

        # !!! later, check about overwriting!!!

        # make the filename
        fname = '{}_{}_{}.1D'.format(label, 'troughs', '00')
        if prefix :
            fname = prefix + '_' + fname
        if edir :
            fname = edir + '/' + fname
        elif odir :
            fname = odir + '/' + fname

        # make str: one column of all values
        ostr = '\n'.join([str(val) for val in tsobj.troughs])
        ostr+= '\n'

        # write the file
        fff = open(fname, 'w')
        fff.write(ostr)
        fff.close()
    
        if verb :
            print("++ Saved integer {} trough indices to file: {}"
                  "".format(label, fname))
            
    # If save filtered time series if they esist
    if pcobj.save_proc_filtered_ts and hasattr(pcobj, 'ts_orig_bp') :

        # !!! later, check about overwriting!!!

        # make the filename
        fname = '{}_{}_{}.1D'.format(label, 'filtered_ts', '00')
        if prefix :
            fname = prefix + '_' + fname
        if edir :
            fname = edir + '/' + fname
        elif odir :
            fname = odir + '/' + fname

        # make str: one column of all values
        ostr = '\n'.join([str(val) for val in pcobj.ts_orig_bp[label]])
        ostr+= '\n'

        # write the file
        fff = open(fname, 'w')
        fff.write(ostr)
        fff.close()
    
        if verb :
            print("++ Saved integer {} filtered times series to file: {}"
                  "".format(label, fname))

    return 0
