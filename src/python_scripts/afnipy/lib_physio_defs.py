#!/usr/bin/env python

# A library of default settings for: physio_calc.py
# ============================================================================

from datetime import datetime

# ============================================================================
# version and default parameter settings

#version = '1.0'
#version = '1.1'   # add remove_val_list, for some vals to get replaced
#version = '1.2'   # better bandpassing and tapering, no 'add missing' for now
#version = '1.3'   # can read in previous peaks/troughs
#version = '1.4'   # separate sli/vol regr; implement RVTRRF, too
#version = '1.5'   # control on/off of all regressors with the -regress_types*
#version = '2.0'   # refactor opts processing to be more AFNI-like
#version = '2.01'  # more refactoring and AFNI-izing, and setting new defaults
#version = '2.02'  # more help updates and cleaning
version = '2.1'  # add redraw to interactive mode

# threshold values for some floating point comparisons
EPS_TH = 1.e-3

# min beats or breaths per minute
DEF_min_bpm_card = 25.0
DEF_min_bpm_resp = 6.0

# max beats or breaths per minute
DEF_max_bpm_card = 250.0
DEF_max_bpm_resp = 60.0

# RVT shifts: either no RVT, direct list, or linspace set of pars
# (units: sec)
all_rvt_opt = ['rvt_shift_list', 'rvt_shift_linspace']
DEF_rvt_shift_list     = '0 1 2 3 4'  # split+listified, below, if used
DEF_rvt_shift_linspace = None         # can be pars for NumPy linspace(A,B,C)

DEF_regress_types_card = 'retro'
DEF_regress_types_resp = 'retro rvt'

# some QC image plotting options that the user can change
DEF_img_figsize   = []
DEF_img_fontsize  = 10
DEF_img_line_time = 120              # units = seconds, ergo def: 2mins/line
DEF_img_fig_line  = 8                # max num lines per fig
DEF_img_dot_freq  = 50               # points per sec
DEF_img_bp_max_f  = 5.0              # Hz, for bandpass plot

# some init proc options for phys time series
DEF_prefilt_max_freq  = 50            # Hz, for init filter to reduce ts
all_prefilt_mode = ['none', 'median'] # list of possible downsamp types
DEF_prefilt_mode = 'median'           # str, keyword for filtering in downsamp
DEF_prefilt_win_card  = 0.10          # flt, window size (s) for median filter
DEF_prefilt_win_resp  = 0.25          # flt, window size (s) for median filter


# key+mouse bindings for interactive peak/trough editing
TEXT_interact_key_mouse = '''Key+mouse bindings being used:

            5  : refresh peak/trough interval band colors after editing
            4  : delete the vertex (peak or trough) nearest to mouse point
            3  : add a peak vertex
            2  : add a trough vertex
            1  : toggle vertex visibility+editability on and off
   Left-click  : select closest vertex, which can then be dragged along
                the reference line.

   Some additional Matplotlib keyboard shortcuts:
            f  : toggle fullscreen view of panel
            o  : toggle zoom-to-rectangle mode
            p  : toggle pan/zoom mode
            r  : reset panel view (not point edits, but zoom/scroll/etc.)
            q  : quit/close viewer (also Ctrl+w), when done editing
'''


# ============================================================================
# option grouping, reconciliation and validation definitions

# list of keys for volume-related items, that will get parsed
# separately so we peel them off when parsing initial opts
vol_key_list = [
    'dset_epi',
    'dset_tr',
    'dset_nslice',
    'dset_nt',
    'dset_slice_times',
    'dset_slice_pattern',
]

# ---- sublists to check for properties ----

# list of lists of corresponding args_dict and phys_json entries,
# respectively; for each, there is also an EPS value for how to
# compare them if needing to reconcile command line opt values with a
# read-in value; more can be added over time
ALL_AJ_MATCH = [
    ['freq', 'SamplingFrequency', EPS_TH],
    ['start_time', 'StartTime', EPS_TH],
]

AJM_str = "    {:15s}   {:20s}   {:9s}\n".format('ARG/OPT', 'JSON KEY',
                                                 'EPS VAL')
for ii in range(len(ALL_AJ_MATCH)):
    sss = "    -{:14s}   {:20s}   {:.3e}\n".format(ALL_AJ_MATCH[ii][0],
                                                   ALL_AJ_MATCH[ii][1],
                                                   ALL_AJ_MATCH[ii][2])
    AJM_str+= sss

# for dset_epi matching; following style of aj_match, but key names
# don't differ and some things are integer.
ALL_EPIM_MATCH = [
    ['dset_tr', EPS_TH],
    ['dset_nt', EPS_TH],
    ['dset_nslice', EPS_TH],
]

# extension of the above if the tested object is a list; the items in
# the list will be looped over and compared at the given eps
ALL_EPIM_MATCH_LISTS = [
    ['dset_slice_times', EPS_TH],
]

EPIM_str = "    {:15s}   {:9s}\n".format('ITEM', 'EPS VAL')
for ii in range(len(ALL_EPIM_MATCH)):
    sss = "    {:15s}   {:.3e}\n".format(ALL_EPIM_MATCH[ii][0],
                                          ALL_EPIM_MATCH[ii][1])
    EPIM_str+= sss
for ii in range(len(ALL_EPIM_MATCH_LISTS)):
    sss = "    {:15s}   {:.3e}\n".format(ALL_EPIM_MATCH_LISTS[ii][0],
                                          ALL_EPIM_MATCH_LISTS[ii][1])
    EPIM_str+= sss

# quantities that must be strictly > 0
all_quant_gt_zero = [
    'freq',
    'dset_nslice',
    'dset_nt',
    'dset_tr',
    'img_line_time',
    'img_fig_line',
    'img_fontsize',
    'img_dot_freq',
    'img_bp_max_f',
    'prefilt_win_card',
    'prefilt_win_resp',
]

# quantities that must be >= 0
all_quant_ge_zero = [
    'min_bpm_card',
    'min_bpm_resp',
    'max_bpm_card',
    'max_bpm_resp',
]

# --------------------------------------------------------------------------
# codes for volumetric physio regressors

# resp list
list_volbase_resp = [
    'NONE',
    'retro',
    'rvt',
    'rvtrrf',
]
# ... and as a comma-separated string list
all_volbase_resp = ', '.join(list_volbase_resp)

# card list
list_volbase_card = [
    'NONE',
    'retro',
    'hrcrf',
]
# ... and as a comma-separated string list
all_volbase_card = ', '.join(list_volbase_card)

# default outdir name
now      = datetime.now() # current date and time
now_str  = now.strftime("%Y-%m-%d-%H-%M-%S")
odir_def = 'retro_' + now_str

# Each key here should have an option listing in lib_physio_opts.py, and
# vice versa.
DOPTS = {
    'resp_file'         : None,      # (str) fname for resp data
    'card_file'         : None,      # (str) fname for card data
    'phys_file'         : None,      # (str) fname of physio input data
    'phys_json'         : None,      # (str) fname of json file
    'prefilt_max_freq'  : DEF_prefilt_max_freq,  # (num) init phys ts downsample
    'prefilt_mode'      : DEF_prefilt_mode,      # (str) kind of downsamp
    'prefilt_win_card'  : DEF_prefilt_win_card,  # (num) window size for dnsmpl
    'prefilt_win_resp'  : DEF_prefilt_win_resp,  # (num) window size for dnsmpl
    'do_interact'       : 'No',      # (str) Yes/1 or No/0 -> bool after parsing
    'do_slibase_out'    : 'No',      # (str) Yes/1 or No/0 -> bool after parsing
    'dset_epi'          : None,      # (str) name of MRI dset, for vol pars
    'dset_tr'           : None,      # (float) TR of MRI
    'dset_nslice'       : None,      # (int) number of MRI vol slices
    'dset_nt'           : None,      # (int) Ntpts (e.g., len MRI time series)
    'dset_slice_times'  : None,      # (list) FMRI dset slice times
    'dset_slice_pattern': None,      # (str) code or file for slice timing
    'freq'              : None,      # (float) freq, in Hz
    'start_time'        : None,      # (float) leave none, bc can be set in json
    'out_dir'           : odir_def,  # (str) output dir name
    'prefix'            : 'physio',  # (str) output filename prefix
    'do_fix_nan'        : 'No',      # (str) Yes/1, No/0 -> bool after parsing
    'do_fix_null'       : 'No',      # (str) Yes/1, No/0 -> bool after parsing
    'do_fix_outliers'   : 'No',      # (str) Yes/1, No/0 -> bool after parsing
    'extra_fix_list'    : [],        # (list) extra values to fix
    'remove_val_list'   : [],        # (list) purge some values from ts
    'min_bpm_resp'      : DEF_min_bpm_resp, # (float) min breaths per min
    'min_bpm_card'      : DEF_min_bpm_card, # (float) min beats per min
    'max_bpm_resp'      : DEF_max_bpm_resp, # (float) max breaths per min
    'max_bpm_card'      : DEF_max_bpm_card, # (float) max beats per min
    'verb'              : 0,         # (int) verbosity level
    'disp_all_slice_patterns' : 'No', # (str) Yes/1, No/0 -> bool after parsing
    'disp_all_opts'     : 'No',      # (str) Yes/1, No/0 -> bool after parsing
    'ver'               : False,     # (bool) do show ver num?
    'help'              : False,     # (bool) do show help in term?
    'hview'             : False,     # (bool) do show help in text ed?
    'rvt_shift_list'    : None,      # (str) space sep list of nums
    'rvt_shift_linspace': DEF_rvt_shift_linspace, # (str) pars for RVT shift
    'regress_types_resp': DEF_regress_types_resp, # (str) if resp, which regr?
    'regress_types_card': DEF_regress_types_card, # (str) if card, which regr?
    'img_verb'          : 1,         # (int) amount of graphs to save
    'img_figsize'       : DEF_img_figsize,   # (tuple) figsize dims for QC imgs
    'img_fontsize'      : DEF_img_fontsize,  # (float) font size for QC imgs
    'img_line_time'     : DEF_img_line_time, # (float) time per line in QC imgs
    'img_fig_line'      : DEF_img_fig_line,  # (int) lines per fig in QC imgs
    'img_dot_freq'      : DEF_img_dot_freq,  # (float) max dots per sec in img
    'img_bp_max_f'      : DEF_img_bp_max_f,  # (float) xaxis max for bp plot
    'save_proc_peaks'   : False,     # (bool) dump peaks to text file
    'save_proc_troughs' : False,     # (bool) dump troughs to text file
    'load_proc_peaks_card'   : None, # (str) file of peaks to read in
    'load_proc_peaks_resp'   : None, # (str) file of peaks to read in
    'load_proc_troughs_resp' : None, # (str) file of troughs to read in
}

DOPTS_all_keys = list(DOPTS.keys())

# ============================================================================

if __name__ == "__main__" :

    print("++ No example")
