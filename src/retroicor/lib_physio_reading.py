#!/usr/bin/env python

import sys
import os
import json
import gzip
import copy
import numpy          as np
from   afnipy import  lib_physio_opts    as lpo
from   afnipy import  lib_physio_funcs   as lpf
from   afnipy import  lib_physio_util    as lpu
from   afnipy import  lib_format_cmd_str as lfcs

# ==========================================================================

DEF_outlier_perc = [10, 90]

# list of labels for available methods for interpolating bad points
ALL_fix_method = [     
    'interp_linear',
]

# ==========================================================================

class phys_ts_obj:
    """An object for holding physio time series (e.g., card and resp) and
derived data.

    """

    def __init__(self, ts_orig, samp_freq = 0.0,
                 label=None, fname=None, ts_unfilt = None,
                 min_bps = 0.0, max_bps = sys.float_info.max, 
                 start_time = 0.0, img_dot_freq = lpo.DEF_img_dot_freq,
                 verb=0):
        """Create object holding a physio time series data.

        """

        self.verb       = verb                # verbosity level
        self.label      = label               # str, e.g., 'card', 'resp', ...
        self.fname      = fname               # str, fname, just for info

        self.ts_orig    = np.array(ts_orig)   # arr, original time series
        self.samp_freq  = float(samp_freq)    # float, sampling freq (in Hz)
        self.start_time = start_time         # float, time offset (<=0, in s)
                                             # from start of MRI
        self.min_bps    = min_bps            # float, min beats/breaths per sec
        self.max_bps    = max_bps            # float, max beats/breaths per sec
        self.ts_unfilt  = np.array(ts_unfilt) # arr, store raw ts
        self.ts_orig_bp = np.zeros(0, dtype=float) # arr, orig ts post-bandpass
        self.bp_idx_freq_mode = 0.           # flt, peak freq idx in bandpass

        # plotting specific
        self.img_idx   = 0                   # int, for naming QC plots
        self.img_dot_freq = img_dot_freq     # flt, pt density in plts

        # array selectors, for slicewise regressors.  Basically, these 
        # contain a tuple for each slice:  label, list_of_ind
        # the list_of_ind refer to time point values within self.tvalues,
        # and can pick out values from any time series with len(self.ts_orig),
        # esp. self.phases and self.rvt_ts
        self.list_slice_sel_phys = []        # list, lab+ind for ts_orig sel
        self.list_slice_sel_rvt  = []        # list, lab+ind for ts_orig sel

        # peak/trough stuff
        self.peaks     = []                      # list, for indices of peaks
        self.troughs   = []                      # list, for indices of troughs

        # phase stuff (M is upper summation index in Glover et
        # al. 2000, Eq. 1)
        self.phases    = np.zeros(0, dtype=int)  # arr, for phase info
        self.M         = 2                       # int, later can vary

        # rvt stuff (only becomes non-trivial for resp, prob)
        self.rvt_ts    = np.zeros(0, dtype=float) # arr, 'raw' rvt time series

        # regressor stuff: lists of labels and the actual values
        # NB: at present, rvt likely only for rest (but doesn't matter deeply)
        self.regress_dict_phys = {}      # dict of list, (lab, value)
        self.regress_dict_rvt  = {}      # dict of list, (lab, value)


    @property
    def tvalues(self):
        """The array of time values, using the start_time and samp_rate, so
        this has physical units."""
        return self.start_time + np.arange(self.n_ts_orig)*self.samp_rate

    @property
    def n_ts_orig(self):
        """The number of time points in original time series."""
        return len(self.ts_orig)

    @property
    def samp_rate(self):
        """The physical sampling rate (in sec)."""
        try:
            rate = 1.0/self.samp_freq
        except:
            print("** WARNING: undefined sampling rate")
            rate = np.nan
        return rate

    @property
    def duration_ts_orig(self):
        """The total amount of time (in sec) in the original time series."""
        return self.n_ts_orig * self.samp_rate

    @property
    def end_time(self):
        """The end time (in sec) of the original time series, starting from
        the start_time."""
        return self.start_time + self.duration_ts_orig

    @property
    def start_phys_idx(self):
        """The integer index for selecting the window of physio data due to
        having a nonzero start_time.  Note: we think of this as:
           start_time / samp_rate
        but mechanically accomplish this by multiplication with samp_freq.
        """
        if self.start_time < 0 :
            return round(- self.start_time * self.samp_freq)
        else:
            return 0

    @property
    def stats_med_ts_orig(self):
        """Median value for ts_orig."""

        return np.median(self.ts_orig)

    @property
    def n_peaks(self):
        """The number of peaks in the peaks collection."""
        return len(self.peaks)

    @property
    def stats_perc_peaks(self):
        """Percentile-based stats for the peak intervals, which are scaled to
        units of time. Percentiles calc'ed: (10, 25, 40, 50, 60, 75,
        90)"""

        all_stat = \
            lpu.calc_interval_stats_perc(self.peaks, samp_rate=self.samp_rate,
                                         all_perc=(10, 25, 40, 50, 60, 75, 90))
        return all_stat

    @property
    def stats_perc_troughs(self):
        """Percentile-based stats for the trough intervals, which are scaled
        to units of time. Percentiles calc'ed: (10, 25, 40, 50, 60,
        75, 90)"""

        all_stat = \
            lpu.calc_interval_stats_perc(self.troughs, samp_rate=self.samp_rate,
                                         all_perc=(10, 25, 40, 50, 60, 75, 90))
        return all_stat

    @property
    def stats_quarts_peaks(self):
        """Quartile (25, 50, 75) stats for the peak intervals, which are
        scaled to units of time."""

        all_stat = \
            lpu.calc_interval_stats_perc(self.peaks, samp_rate=self.samp_rate,
                                         all_perc=(25, 50, 75))
        return all_stat

    @property
    def stats_quarts_troughs(self):
        """Quartile (25, 50, 75) stats for the trough intervals, which are
        scaled to units of time."""

        all_stat = \
            lpu.calc_interval_stats_perc(self.troughs, samp_rate=self.samp_rate,
                                         all_perc=(25, 50, 75))
        return all_stat

    @property
    def stats_mmms_peaks(self):
        """Min/max/mean/std stats for the peak intervals, which are scaled to
        units of time."""

        all_stat = \
            lpu.calc_interval_stats_mmms(self.peaks, samp_rate=self.samp_rate)
        return all_stat

    @property
    def stats_mmms_troughs(self):
        """Min/max/mean/std stats for the trough intervals, which are scaled
        to units of time."""

        all_stat = \
            lpu.calc_interval_stats_mmms(self.troughs, samp_rate=self.samp_rate)
        return all_stat

    @property
    def n_troughs(self):
        """The number of troughs in the troughs collection."""
        return len(self.troughs)

    @property
    def n_slice_sel_phys(self):
        """The number of elements in list_slice_sel_phys, which (when
        populated) represents the number of slices for which there is
        slice timing."""
        return len(self.list_slice_sel_phys)

    @property
    def n_regress_phys(self):
        """The number of plain ol' physio regressors (probably 4)."""
        return len(self.regress_dict_phys)

    @property
    def n_regress_rvt(self):
        """The number of fancy RVT regressors (probably 5 or 0, but user can
        choose)."""
        return len(self.regress_dict_rvt)

    @property
    def regress_rvt_phys(self):
        """The keys of the physio regressors."""
        return list(self.regress_dict_phys.keys())

    @property
    def regress_rvt_keys(self):
        """The keys of the RVT regressors."""
        return list(self.regress_dict_rvt.keys())

    @property
    def img_arr_step(self):
        """When plotting lines in QC figs, don't need to plot each one---save
        time and space by reducing that number.  The step is the ratio
        of samp_freq to img_dot_freq."""
        if self.img_dot_freq :
            return max(int(self.samp_freq // self.img_dot_freq),1)
        else:
            return 1

    @property
    def ft_delta_f(self):
        """The delta_f value of ts_orig in the Fourier domain; that is, the
        step size along x-axis for frequency spectrum."""
        return self.samp_freq / self.n_ts_orig

    @property
    def ft_nyquist_idx(self):
        """The index of the Nyquist frequency of ts_orig in the Fourier
        domain; that is, the index of the max indep freq in the FT
        decomp."""
        return (self.n_ts_orig // 2) + (self.n_ts_orig % 2)

    @property
    def ft_nyquist_freq(self):
        """The physical values of the Nyquist frequency (in Hz) of ts_orig in
        the Fourier domain; that is, the max indep freq in the FT
        decomp."""
        return self.samp_freq * self.ft_nyquist_idx

    @property
    def ft_freq_mode_phys(self):
        """The physical value of the peak frequency (in Hz) of ts_orig in the
        Fourier domain; that is, the mode of the freq spectrum in the
        FT decomp."""
        return self.bp_idx_freq_mode * self.ft_delta_f

# -------------------------------------------------------------------------

class retro_obj:
    """An object for starting the retroicor process for making physio
regressors for MRI data.

Each phys_ts_obj is now held as a value to the data[LABEL] dictionary here

    """

    def __init__(self, args_dict=None, args_orig=[], verb=0):
        """Create object holding all retro info

        """
        
        # save copy of input args_dict, for reference, as well as
        # args_orig list (to record in a text file), if input
        self.args_dict = copy.deepcopy(args_dict)
        self.args_orig = copy.deepcopy(args_orig)

        # physio data: refer to each as value of data dictionary.
        # NB: RVT is contained within each card/resp/etc. data obj here
        self.data = {
            'card' : None,             # obj for card data
            'resp' : None,             # obj for resp data
        }

        # maybe not keep these in this obj?
        self.phys_jdict      = None    # dict from JSON file, maybe col labels
        self.phys_file       = None    # phys file, might contain cardio/resp

        self.exit_on_rag     = True    # exit if raggedness in data files?
        self.exit_on_nan     = True    # exit if NaN values in data files?
        self.exit_on_null    = True    # exit if null values in data files?
        self.do_fix_outliers = False   # exit if null values in data files?
        self.extra_fix_list  = []      # list of to-be-bad values (-> interp)
        self.remove_val_list = []      # list of values to be purged
        self.rvt_shift_list  = []      # list of RVT shift values

        # physio info (-> some now in data['resp'] and data['card'] objs)
        self.start_time   = None       # float, time offset (<=0, in s) from 
                                       # start of MRI

        # MRI EPI volumetric info
        self.vol_slice_times = []      # list of floats for slice timing
        self.vol_slice_pat   = None    # str, name of slice pat (for ref)
        self.vol_tr          = 0.0     # float, TR of MRI EPI
        self.vol_nv          = 0       # int, Nvol (dset_nt) FMRI EPI 

        # I/O info
        self.verb         = verb       # int, verbosity level
        self.out_dir      = None       # str, name of output dir
        self.prefix       = None       # str, prefix of output filenames
        self.niml         = False      # bool, use niml in output ***
        self.do_out_rvt   = True       # bool, flag
        self.do_out_card  = True       # bool, flag
        self.do_out_resp  = True       # bool, flag
        self.img_verb     = 1          # int, amount of graphs to save
        self.img_fontsize = lpo.DEF_img_fontsize   # flt, FS for output images
        self.img_figsize  = lpo.DEF_img_figsize    # 2-ple, img height/wid
        self.img_line_time = lpo.DEF_img_line_time # flt, time per line in plt
        self.img_dot_freq  = lpo.DEF_img_dot_freq  # flt, pts per sec
        self.img_bp_max_f  = lpo.DEF_img_bp_max_f  # flt, Hz for bp plot


        # -----------------------------------------------------------------

        if args_dict != None :
            self.apply_cmd_line_args(args_dict)

        IS_BAD = 0

        # check, please! ... based on final slice of final vol
        for label in lpf.PO_all_label:
            if self.data[label] :
                check = self.check_end_time_phys_ge_final_slice(label)
                if not(check) :
                    print("** ERROR: {} physio data too short for MRI data"
                          "".format(label))
                    IS_BAD+=1 

        if IS_BAD :
            sys.exit(3)
                
    def apply_cmd_line_args(self, args_dict):
        """The main way to populate object fields at present.  The input
        args_dict should be a dictionary from lib_retro_opts of
        checked+verified items input from the command line.
        """

        # *** this method is in progress ***

        self.verb             = args_dict['verb']

        self.vol_slice_times  = copy.deepcopy(args_dict['dset_slice_times'])
        self.vol_slice_pat    = args_dict['dset_slice_pattern']
        self.vol_tr           = args_dict['dset_tr']
        self.vol_nv           = args_dict['dset_nt']

        self.start_time       = args_dict['start_time']

        self.out_dir          = args_dict['out_dir']
        self.prefix           = args_dict['prefix']
        self.img_verb         = args_dict['img_verb']
        self.img_figsize      = copy.deepcopy(args_dict['img_figsize'])
        self.img_fontsize     = args_dict['img_fontsize']
        self.img_line_time    = args_dict['img_line_time']
        self.img_dot_freq     = args_dict['img_dot_freq']
        self.img_bp_max_f     = args_dict['img_bp_max_f']
        self.niml             = args_dict['niml']
        self.do_out_rvt       = not(args_dict['rvt_off'])
        self.do_out_card      = not(args_dict['no_card_out'])
        self.do_out_resp      = not(args_dict['no_resp_out'])

        #self.exit_on_rag -> NB: prob never try to fix
        self.exit_on_nan      = not(args_dict['do_fix_nan'])
        self.exit_on_null     = not(args_dict['do_fix_null'])
        self.do_fix_outliers  = args_dict['do_fix_outliers']
        self.extra_fix_list   = copy.deepcopy(args_dict['extra_fix_list'])
        self.remove_val_list  = copy.deepcopy(args_dict['remove_val_list'])
        self.rvt_shift_list   = copy.deepcopy(args_dict['rvt_shift_list'])

        # run these file reads+checks last, because they use option
        # items from above
        if args_dict['phys_file'] and args_dict['phys_json_dict'] :
            is_bad = self.set_data_from_phys_file_and_json(args_dict)
        if args_dict['resp_file'] :
            is_bad = self.set_data_from_solo_file(args_dict, label='resp')
        if args_dict['card_file'] :
            is_bad = self.set_data_from_solo_file(args_dict, label='card')

        if is_bad :
            print("** ERROR: fatal problem reading in data.")
            sys.exit(2)

    # -----------------------

    def set_data_from_solo_file(self, args_dict, label=''):
        """Using information stored in args_dict, try opening and reading
        either the resp_file or card_file.  Use this to populate one
        or more data objs.

        While reading in data files, also identify bad points, and
        apply fixes.

        The 'label' is required, and must be one of:
        'resp'
        'card'

        Populate one of: 
        data['resp']
        data['card']
        
        Return 0 if OK, and nonzero if bad.

        """

        if   label == 'resp' :  fname = args_dict['resp_file']
        elif label == 'card' :  fname = args_dict['card_file']
        else:
            print("** ERROR: need a recognized label, not '{}'"
                  "".format(label))
            return 1

        all_col = self.read_and_check_data_file(fname)
        arr     = self.extract_list_col(all_col, 0) 
        arr2, nrem = \
            check_and_remove_arr_values(arr, 
                                        bad_vals=self.remove_val_list,
                                        verb=self.verb)
        arr_fixed, nfix = \
            check_and_fix_arr_badness(arr2, 
                                      bad_nums=self.extra_fix_list,
                                      outliers_bad=self.do_fix_outliers,
                                      verb=self.verb)
        # if fixing is needed, put copy of orig as ts_unfilt
        if nfix :    ts_unfilt = copy.deepcopy(arr)
        else:        ts_unfilt = None

        if label == 'resp' :
            self.data['resp'] = phys_ts_obj(arr_fixed,
                                         samp_freq = args_dict['freq'],
                                         label=label, fname=fname, 
                                         ts_unfilt = ts_unfilt,
                                         min_bps = args_dict['min_bpm_resp']/60.,
                                         max_bps = args_dict['max_bpm_resp']/60.,
                                         start_time = args_dict['start_time'],
                                         img_dot_freq = args_dict['img_dot_freq'],
                                         verb=self.verb)
        elif label == 'card' :
            self.data['card'] = phys_ts_obj(arr_fixed,
                                         samp_freq = args_dict['freq'],
                                         label=label, fname=fname, 
                                         ts_unfilt = ts_unfilt,
                                         min_bps = args_dict['min_bpm_card']/60.,
                                         max_bps = args_dict['max_bpm_card']/60.,
                                         start_time = args_dict['start_time'],
                                         img_dot_freq = args_dict['img_dot_freq'],
                                         verb=self.verb)
        return 0

    def set_data_from_phys_file_and_json(self, args_dict):
        """Using information stored in args_dict, try opening and reading the
        phys_file, using a dictionary made from its accompanying JSON.
        Use this to populate one or more data objs.

        While reading in data files, also identify bad points, and
        apply fixes.

        The 'label' is required, and must be one of:
        'resp'
        'card'

        Populate one of: 
        data['resp']
        data['card']
        
        Return 0 if OK, and nonzero if bad.

        !!! PT note: can condense the 2 separate if-conditions here

        """

        fname      = args_dict['phys_file']
        D          = args_dict['phys_json_dict']
        samp_freq  = args_dict['freq']
        all_col    = self.read_and_check_data_file(fname)
        USE_COL    = 0

        if 'respiratory' in D['Columns'] :
            if self.verb:
                print("++ Reading _resp_ data from {}".format(fname))
            USE_COL+= 1
            idx = D["Columns"].index('respiratory')
            arr = self.extract_list_col(all_col, idx)
            arr2, nrem = \
                check_and_remove_arr_values(arr, 
                                            bad_vals=self.remove_val_list,
                                            verb=self.verb)
            arr_fixed, nfix = \
                check_and_fix_arr_badness(arr2, 
                                          bad_nums=self.extra_fix_list,
                                          outliers_bad=self.do_fix_outliers,
                                          verb=self.verb)
            # if fixing is needed, put copy of orig as ts_unfilt
            if nfix :    ts_unfilt = copy.deepcopy(arr)
            else:        ts_unfilt = None

            self.data['resp'] = phys_ts_obj(arr_fixed, 
                                         samp_freq = samp_freq,
                                         label='resp', fname=fname, 
                                         ts_unfilt = ts_unfilt,
                                         min_bps = args_dict['min_bpm_resp']/60.,
                                         max_bps = args_dict['max_bpm_resp']/60.,
                                         start_time = args_dict['start_time'],
                                         img_dot_freq = args_dict['img_dot_freq'],
                                         verb=self.verb)
        if 'cardiac' in D['Columns'] :
            if self.verb:
                print("++ Reading _card_ data from {}".format(fname))
            USE_COL+= 1
            idx = D["Columns"].index('cardiac')
            arr = self.extract_list_col(all_col, idx)
            arr2, nrem = \
                check_and_remove_arr_values(arr, 
                                            bad_vals=self.remove_val_list,
                                            verb=self.verb)
            arr_fixed, nfix = \
                check_and_fix_arr_badness(arr2, 
                                          bad_nums=self.extra_fix_list,
                                          outliers_bad=self.do_fix_outliers,
                                          verb=self.verb)
            # if fixing is needed, put copy of orig as ts_unfilt
            if nfix :    ts_unfilt = copy.deepcopy(arr)
            else:        ts_unfilt = None

            self.data['card'] = phys_ts_obj(arr_fixed, 
                                         samp_freq = samp_freq,
                                         label='card', fname=fname, 
                                         ts_unfilt = ts_unfilt,
                                         min_bps = args_dict['min_bpm_card']/60.,
                                         max_bps = args_dict['max_bpm_card']/60.,
                                         start_time = args_dict['start_time'],
                                         img_dot_freq = args_dict['img_dot_freq'],
                                         verb=self.verb)
        if not(USE_COL) :
            print("** ERROR: could not find any columns in {} that were "
                  "labelled like data".format(fname))
            return 1

        return 0

    def extract_list_col(self, all_col, idx):
        """For data that has been read in as a list of lists, extract the
        [idx] column.

        Return that column as an array of floats.
        """

        ncol = len(all_col[0])
        if idx >= ncol :
            print("** ERROR: index {} cannot be used, since ncol = {}"
                  "".format(idx, ncol))

        N   = len(all_col)
        arr = np.zeros(N, dtype=float)
        for ii in range(N):
            arr[ii] = all_col[ii][idx]

        return arr

    def read_and_check_data_file(self, fname):
        """Try to read in datafile fname (which has 1 or more columns of
        numbers), and put it through The Gauntlet of basic checks for
        badness.  Fail (or not) according to the user's whims.

        Return a list of the data, one list per row.
        """

        HAVE_BADNESS = 0
        all_col, bad_nanlist, bad_nulllist, dict_of_len = \
            read_column_text_to_float_list(fname, verb=self.verb)

        if self.exit_on_null and len(bad_nulllist) :
            print("** ERROR: exit due to null values present in {}"
                  "".format(fname))
            HAVE_BADNESS+= 1
        if self.exit_on_nan and len(bad_nanlist) :
            print("** ERROR: exit due to nan values present in {}"
                  "".format(fname))
            HAVE_BADNESS+= 1
        if self.exit_on_rag and len(dict_of_len)>1 :
            print("** ERROR: exit due to raggedness present in {}"
                  "".format(fname))
            HAVE_BADNESS+= 1
        if HAVE_BADNESS :
            sys.exit(3)

        return all_col
            
    def check_end_time_phys_ge_vol(self, label):
        """Check if the 'label' physio time series's end time (which is offset
        by the start_time parameter) is greater than or equal to that
        of the volumetric data (= MRI).  This kinda determines if it
        makes sense to continue with the calculation---see 
        check_end_time_phys_ge_final_slice(...) for a more official one.
        """

        if not(self.have_label(label)) :
            return False

        phys_end_time = self.data[label].end_time

        if phys_end_time < self.duration_vol :
            return False
        else:
            return True

    def check_end_time_phys_ge_final_slice(self, label):
        """Check if the 'label' physio time series's end time (which is offset
        by the start_time parameter) is greater than or equal to the
        time at which the final slice of the volumetric data (= MRI)
        is acquired.  This essentially determines if it makes sense to
        continue with the calculation.
        """

        if not(self.have_label(label)) :
            return False

        phys_end_time = self.data[label].end_time

        print("++ Start time physio ({}) : {}".format(label, self.start_time))
        print("++ End times of physio ({}) and MRI:".format(label))
        print("   physio end time      : ", phys_end_time)
        print("   final MRI slice time : ", self.vol_final_slice_time)

        if phys_end_time < self.vol_final_slice_time :
            print("** -- Final duration problem ({}) -- ".format(label))
            return False
        else:
            return True

    def have_label(self, label):
        """Do we appear to have a 'label' data obj?"""

        if label not in list(self.data.keys()):
            print("+* WARN: label '{}' does not appear in data dict"
                  "".format(label))
            return False
        else: 
            return self.data[label] != None

    @property
    def n_slice_times(self):
        """Length of volumetric slice times list."""
        return len(self.vol_slice_times)

    @property
    def duration_vol(self):
        """The total amount of time (in sec) of the volumetric (= MRI)
        data, defined as the TR times the number of volumes."""
        return self.vol_tr * self.vol_nv

    @property
    def vol_final_slice_time(self): 
        """The actual maximum duration of data in the MRI, taking into account
        slice timing. This is the acquisition *time* of the last
        volumetric slice.
        """
        return self.duration_vol - self.vol_tr + max(self.vol_slice_times)


def find_bad_vals(x, bad_nums=None, verb=0):
    """For a 1D array x of length N, check for NaNs as well as any list of
possibly bad (finite) values to report, such as 0s.  Output a
True/False array of length N, where True highlights a bad value.

Parameters
----------
x : np.ndarray (1D)
    input array
bad_nums : list
    list of integer or float values that will be considered bad
verb : int
    verbosity level

Returns
-------
arr_bad : np.ndarray (1D)
    array of dtype=bool and len(x), with True values pointing out bad
    values in x.    

    """
    N = len(x)

    # find nans
    arr_bad = np.isnan(x)

    if verb and len(bad_nums) :
        print("++ List of bad 'extra fix' numbers: [{}]"
              "".format(', '.join([str(x) for x in bad_nums])))

    # ... and check for additional bad values, if listed
    nbadnum = 0
    if not(bad_nums is None) :
        for ii in range(N):
            if x[ii] in bad_nums :
                arr_bad[ii] = True
                nbadnum+= 1

    if verb:
        nbad = np.sum(arr_bad)
        print("++ Number of bad 'extra fix' values found: {}".format(nbadnum))

    return arr_bad

def find_out_vals(x, arr_bad=None, out_perc=DEF_outlier_perc, verb=0):
    """For a 1D array x of length N, check for outliers to report.  Output
a True/False array of length N, where True highlights an outlier
value.

Parameters
----------
x : np.ndarray (1D)
    input array 
bad_arr : array
    boolean array of len(x), where True values are bad values to be
    avoided in the outlier estimation; if not provided, all values are used
out_perc : list
    a list of 2 numbers, for calculating an interquartile range (or
    analogue, when the values are changed). Outliers are defined to be
    + when > (value at upper perc ran) + 1.5 * (perc ran interval)
    + when < (value at lower perc ran) - 1.5 * (perc ran interval)
verb : int
    verbosity level

Returns
-------
arr_out : np.ndarray (1D)
    array of dtype=bool and len(x), with True values pointing out outlier
    values in x.

    """

    if len(out_perc) != 2:
        sys.exit("** ERROR: out_perc must have 2 values, not {}"
                 "".format(len(out_perc)))
    if out_perc[1] <= out_perc[0]:
        sys.exit("** ERROR: must have out_perc[1] > out_perc[0]")

    N = len(x)

    # make array of good values; kind of waste in some cases, but
    # simplifies later lines
    if not(arr_bad is None) :
        if len(arr_bad) != N:
            sys.exit("** ERROR: mismatched length arrays: {} and {}"
                     "".format(N, len(arr_bad)))
        arr_good = np.invert(arr_bad)
    else:
        arr_good = np.ones(N, dtype=bool)

    median      = np.percentile(x[arr_good], 50)
    ran_bot, ran_top = np.percentile(x[arr_good], out_perc)
    ran_magn    = ran_top - ran_bot
    out_bnd_top = ran_top + 1.5*ran_magn
    out_bnd_bot = ran_bot - 1.5*ran_magn

    # make outlier array of len=N, where True points to outlier
    arr_out = x > out_bnd_top
    arr_out+= x < out_bnd_bot

    if verb:
        nout = np.sum(arr_out)
        print("++ Outlier check info:")
        print("   median        = {:.3f}".format(median))
        print("   [{:5.2f} %ile]  = {:.3f}".format(out_perc[0], ran_bot))
        print("   [{:5.2f} %ile]  = {:.3f}".format(out_perc[1], ran_top))
        print("   inlier range  = [{:.3f}, {:.3f}]"
              "".format(out_bnd_bot, out_bnd_top))
        print("++ Number of outliers found: {}".format(nout))

    return arr_out

def calc_max_streak_true(B, verb=0):
    """For a 1D array of bools B, calculate the max streak of True values.
That is, what is the maximum number of times True occurs in a row.
Also output the array index at which the first max-length streak occurred.

If there are no True (= bad) values, then return: 0, -1.

Parameters
----------
B : np.ndarray
    1D array of boolean values
verb : int
    verbosity level

Returns
-------
max_len : int
    integer value of the length of the max streak
true_ind_strk_max : int
    index in array where (first) max-length streak started

    """

    N = len(B)

    if not(N) :    return 0, -1

    all_ind  = np.arange(N)        # all indices
    true_ind = all_ind[B]          # indices where True appears
    Ntrue    = len(true_ind)

    if verb :
        print("++ Total num of 'bad' values  : {}".format(Ntrue))

    # special cases: Ntrue <=1
    if not(Ntrue) :    
        return 0, -1
    elif Ntrue == 1 :  
        if verb :
            print("++ Max streak of 'bad' values : {}".format(1))
            print("   First occurred at index    : {}".format(true_ind[0]))
        return 1, true_ind[0]
    
    # general cases: Ntrue >1
    all_strk, all_slen = make_list_of_streaks(B, verb=verb)
    
    # get max and loc of max, to report
    max_len = -1
    max_ind = -1
    for ii in range(len(all_slen)) :
        if all_slen[ii] > max_len :
            max_len = all_slen[ii]
            max_ind = ii
    true_ind_strk_max = all_strk[ii][0]

    if verb :
        print("++ Max streak of 'bad' values : {}".format(max_len))
        print("   First occurred at index    : {}".format(true_ind_strk_max))

    return max_len, true_ind_strk_max

def make_list_of_streaks(B, verb=0):
    """For a 1D array of bools B, calculate where streaks of consecutive
True values occur (creating a list of streak lists).  Also output a
list of the lengths of each streak.

Parameters
----------
B : np.ndarray
    1D array of boolean values
verb : int
    verbosity level

Returns
-------
all_strk : list
    list of all streaks, where each streak is a list of consecutive
    indices (so, a list of lists of ints)
all_slen: list
    list of the lengths of each streak (so, a list of ints of same len
    as all_strk)

    """

    N        = len(B)

    if not(N) :    return [], []

    all_ind  = np.arange(N)        # all indices
    true_ind = all_ind[B]          # indices where True appears
    Ntrue    = len(true_ind)

    # simple cases: Ntrue <=1
    if not(Ntrue) :    
        return [], []
    elif Ntrue == 1 :  
        return [[true_ind[0]]], [1]

    # general cases: Ntrue >1
    all_strk = []                  # hold each streak list
    all_slen = []                  # len of each streak list
    strk = [true_ind[0]]
    diff_ind = np.diff(true_ind)   # diffs of consec true_ind elements
    for ii in range(Ntrue-1):
        if diff_ind[ii] == 1:
            strk.append(true_ind[ii+1])
        else:
            # end streak...
            all_slen.append(len(strk))
            all_strk.append(copy.deepcopy(strk))
            # ... and start a new one
            strk = [true_ind[ii+1]]
    if strk:
        all_slen.append(len(strk))
        all_strk.append(copy.deepcopy(strk))

    if verb:
        nstrk = len(all_strk)
        maxlen = max(all_slen)
        print("++ Found {} streaks, with max length : {}"
              "".format(nstrk, maxlen))

    return all_strk, all_slen

def check_and_remove_arr_values(x, bad_vals=[], verb=0):
    """Check 1D array x for values that will be removed/purged.  If any of
the int or float values in the bad_vals list appear in x, then the
output array will be shorter than the input.

Parameters
----------
x : np.ndarray (1D)
    input array
bad_vals : list
    list of integer or float values that will be removed
verb : int
    verbosity level

Returns
-------
x_fixed : np.ndarray
    the input array x with any bad values removed
nrem : int
    number of points removed

"""

    # simple case, nothing really to do
    if not(len(bad_vals)) :    return copy.deepcopy(x), 0

    N = len(x)

    y = list(x)
    nrem = 0            # count number of points removed
    if verb :
        list_idx = []   # for verb, make list of points removed

    for ii in range(N-1, -1, -1):
        if y[ii] in bad_vals :
            tmp = y.pop(ii)
            nrem+= 1
            if verb :
                list_idx.append(ii)

    x_fixed = np.array(y)

    if verb :
        print("++ Removed this many bad values from the time series: {}"
              "".format(nrem))
        if nrem :
            list_idx.reverse()
            str_idx = ', '.join([str(idx) for idx in list_idx])
            print("   These occurred at these indices:\n"
                  "   {}".format(str_idx))

    return x_fixed, nrem

# ---------------------------------------------------------------------

def check_and_fix_arr_badness(x, thr_nbad=None, thr_bad_strk=None,
                              fix_method='interp_linear',
                              bad_nums=[], outliers_bad=False,
                              out_perc=DEF_outlier_perc, verb=0):
    """Check 1D array x for badness (with some options/flexibility about
what 'badness' can mean), and either fix it or exit.  'Fixing' means
applying the fix method, such as linear interpolation.  The user can
set thr_nbad to be a threshold number of bad elements that x can have
before just exiting; thr_bad_strk can be set as a max number of
consecutive bad elements to allow before just exiting.  

If all values are bad, exit regardless of anything else.

By default, only NaN values are bad. The user can supply a list of
elements that will be flagged as bad (e.g., 0), or can turn on the
ability to flag outliers as bad (based on percentiles, which can also
be controlled).

Parameters
----------
x : np.ndarray (1D)
    input array
thr_nbad : int
    threshold value for proceeding/fixing: if there are thr_nbad or
    more bad elements in x, then exit
thr_bad_strk : int
    threshold value for proceeding/fixing: if there is a streak of
    thr_bad_strk or more *consecutive* bad elements in x, then exit
fix_method : str
    the fix method to apply, from a list of: 'interp_linear'.
bad_nums : list
    list of integer or float values that will be considered bad
outliers_bad : bool
    control whether outlier values will be added to the output list
    of bad indices
out_perc : list
    a list of 2 numbers, for calculating an interquartile range (or
    analogue, when the values are changed). Outliers are defined to be
    + when > (value at upper perc ran) + 1.5 * (perc ran interval)
    + when < (value at lower perc ran) - 1.5 * (perc ran interval)
verb : int
    verbosity level

Returns
-------
x_fixed : np.ndarray
    the input array x with any bad values 'fixed' or replaced by the
    applied fix method.
nfix : int
    number of points fixed

    """

    if not(fix_method in ALL_fix_method) :
        print("** ERROR: {} is not in allowed list of fix_methods:"
              "   {}".format(fix_method, ','.join(ALL_fix_method)))
        sys.exit(10)

    # get the information on where badness occurs
    arr_bad, len_strk_bad, idx_strk_bad = \
        find_arr_bad_and_out( x, bad_nums=bad_nums, 
                              outliers_bad=outliers_bad,
                              out_perc=out_perc, verb=verb )
    tot_nbad = np.sum(arr_bad)

    # automatic check: tooo much badness
    if tot_nbad >= len(x) :
        print("** ERROR: all time points appeared to be bad :(")
        sys.exit(4)

    # apply any thresholds, if doing so
    if thr_nbad != None :
        print("++ Check for total num of bad points (threshold = {})"
              "".format(thr_nbad))
        if tot_nbad >= thr_nbad :
            print("** ERROR: exiting, too many bad points found : {}"
                  "".format(tot_nbad))
            sys.exit(9)
        else:
            print("++ OK, continuing since fewer bad points found : {}"
                  "".format(tot_nbad))
    if thr_bad_strk != None :
        print("++ Check for streak length of bad points (threshold = {})"
              "".format(thr_bad_strk))
        if len_strk_bad >= thr_bad_strk :
            print("** ERROR: exiting, bad point streak too long : {}"
                  "".format(len_strk_bad))
            sys.exit(9)
        else:
            print("++ OK, continuing since bad point streak is shorter : {}"
                  "".format(len_strk_bad))

    # simple case of fixing: no bad points!
    if not(tot_nbad) :
        x_fixed = copy.deepcopy(x)
        return x_fixed, 0

    # if this point is reached, there are fixes to be made
    if fix_method == 'interp_linear' :
        x_fixed = calc_linear_interp(x, arr_bad, verb=verb)

    return x_fixed, tot_nbad


def calc_linear_interp(x, arr_bad, verb=0):
    """Perform linear interpolation over streaks of bad points in a 1D
array x.  The 'bad points' are highlighted by the True values in the
1D array of bools arr_bad (which has same len as x).  Streaks of
badness that hit either endpoint are replaced with the closest,
non-bad value.

Parameters
----------
x : np.ndarray (1D)
    input array
arr_bad : np.ndarray (1D)
    array with dtype=bool, where True marks bad points to be
    interpolated
verb : int
    verbosity level

Returns
-------
x_fixed : np.ndarray
    the input array x with any bad values 'fixed' or replaced by the
    applied fix method.

    """

    N = len(x)
    if N != len(arr_bad) :
        print("** ERROR: inconsistent array lengths: {} and {}"
              "".format(N, len(arr_bad)))
        sys.exit(3)
    
    nbad    = np.sum(arr_bad)
    x_fixed = copy.deepcopy(x)

    # simple case
    if nbad == 0 :    return x_fixed

    # general case: nbad > 0

    # get list of streaks of indices where interp will happen (and
    # don't use second output from here, because we might alter
    # all_strk)
    all_strk, tmp_ignore = make_list_of_streaks(arr_bad, verb=verb)

    # deal with possible streaks at either end of time series, special
    # case: just replace with closest non-bad element, and remove
    # streak from list
    if len(all_strk) > 0 and all_strk[0][0] == 0 :            # first streak, starting index
        if verb :
            nnn = len(all_strk[0])
            print("+* WARN: replace badness at start with simple fill, N : {}"
                  "".format(nnn))
        good_ind = all_strk[0][-1]+1    # first index value after streak
        if good_ind >= N-1 :
            print("** ERROR: can't interpolate if whole time series is bad")
            sys.exit(5)
        val      = x_fixed[good_ind]
        for ii in range(good_ind):
            x_fixed[ii] = val
        tmp = all_strk.pop(0)
    if len(all_strk) > 0 and all_strk[-1][-1] == N-1 :         # last streak, ending index
        if verb :
            nnn = len(all_strk[-1])
            print("+* WARN: replace badness at end with simple fill, N : {}"
                  "".format(nnn))
        good_ind = all_strk[-1][0]-1    # first index value after streak
        if good_ind <= 0 :
            print("** ERROR: can't interpolate if whole time series is bad")
            sys.exit(5)
        val      = x_fixed[good_ind]
        for ii in range(good_ind, N):
            x_fixed[ii] = val
        tmp = all_strk.pop(-1)

    # ... and interp the rest/remainder of streaks now
    nstrk = len(all_strk)
    for ii in range(nstrk):
        strk = all_strk[ii]   # don't need to copy
        A = strk[0] - 1
        B = strk[-1] + 1
        delta_x = (x_fixed[B] - x_fixed[A])/(B-A)
        for nn in range(len(strk)):
            idx = strk[nn]
            x_fixed[idx] = x_fixed[A] + (1+nn)*delta_x

    return x_fixed



def find_arr_bad_and_out(x, bad_nums=[], outliers_bad=False,
                         out_perc=DEF_outlier_perc, verb=0):
    """Provide information (not decision making) about badness in a 1D
array x. Check for NaNs as well as any list of possibly bad (finite)
values to report, such as 0s.  The output list of bad indices will
point to values that might be replaced later with interpolation or
some other procedure.

Also check for outliers (estimated from arr elements that are not NaNs
or bad_nums).

Parameters
----------
x : np.ndarray (1D)
    input array
bad_nums : list
    list of integer or float values that will be considered bad
outliers_bad : bool
    control whether outlier values will be added to the output list
    of bad indices
out_perc : list
    a list of 2 numbers, for calculating an interquartile range (or
    analogue, when the values are changed). Outliers are defined to be
    + when > (value at upper perc ran) + 1.5 * (perc ran interval)
    + when < (value at lower perc ran) - 1.5 * (perc ran interval)
verb : int
    verbosity level

Returns
-------
arr_bad : np.ndarray
    1D array of bools: True marks bad elements, and False are OK ones;
    np.sum(arr_bad) gives the total number of bad elements
len_strk_bad: int
    count of longest streak of consecutive bad values
idx_strk_bad: int
    index in the array that starts the (first) longest streak of 
    consecutive bad values

    """

    if type(x) != np.ndarray :
        sys.exit("** ERROR: input must be numpy array")
    if len(np.shape(x)) != 1 :
        sys.exit("** ERROR: input must be 1D array")

    N = len(x)

    # point out bad values
    arr_bad = find_bad_vals(x, bad_nums=bad_nums, verb=verb)

    # find outliers: makes output to terminal, and the array can be
    # combined with arr_bad, below
    arr_out = find_out_vals(x, arr_bad=arr_bad,
                            out_perc=out_perc, verb=verb)

    # outliers can become part of the bad list
    if outliers_bad : 
        nout = np.sum(arr_out)
        print("++ Add any outliers to the bad list, N = {}".format(nout))
        arr_bad += arr_out

    # find bad streaks: calc max number of consecutive bad (= True
    # here) elements 
    len_strk_bad, idx_strk_bad = calc_max_streak_true(arr_bad, verb=verb)

    return arr_bad, len_strk_bad, idx_strk_bad

# ==========================================================================

def float_or_nan(x):
    """Take a number x and return float(x) if possible, or a numpy.nan if
it ain't.  Leading and trailing whitespace within the string should
not have any effect.

Parameters
----------
x : str
    possibly floatable str

Returns
-------
xfl : float
    a floatized version of x, either float(x) or NAN

    """

    try:
        return float(x)
    except:
        return np.nan

def float_or_nan_or_whitespace(x):
    """Take a number x and return '' if it is empty or only whitespace,
then float(x) if possible, or a numpy.nan if it ain't either.  Leading and
trailing whitespace within the string should not have any effect.


Parameters
----------
x : str
    possibly floatable str

Returns
-------
xfl : float or null str ('')
    a floatized version of x, either float(x) or NAN, or ''

    """

    y = x.strip()

    if not(y) :
        return ''

    try:
        return float(y)
    except:
        return np.nan

def check_raggedness_of_lines(L, verb=0):
    """For a given list of line lengths L (= simple list of integers),
determine and report on the raggedness of the lines it summarizes.

Parameters
----------
L : list 
    list of integer values, here corresponding to the line lengths of
    an input array
verb : int
    verbosity level whilst processing

Returns
-------
dict_of_len : dict
    dictionary of the list of lengths

    """

    # flag any raggedness, ignoring bad_nullist items
    maxlen = max(L)
    dict_of_len = {}
    for ii in range(1, maxlen+1):
        # keep track of how many row length varieties exist
        nnn = L.count(ii)
        if nnn :
            dict_of_len[ii] = nnn
    ndlen = len(dict_of_len)

    if verb and ndlen==1 :
        print("   No apparent raggedness in the data columns")

    if ndlen > 1 :
        print("+* WARN: apparent raggedness in the file")

        # make parallel arrays of keys (each line length) and values
        # (number of lines for a given length), sorted in descending
        # order of values
        akeys = np.zeros(ndlen, dtype=int)
        avals = np.zeros(ndlen, dtype=int)
        idx   = 0 
        for k, v in dict_of_len.items():
            akeys[idx] = k
            avals[idx] = v
            idx+=1
        sortmap = np.argsort(avals) # map of how to sort akeys and avals

        # printout in reversed order
        for nn in range(ndlen):
            ii = sortmap[ndlen-1-nn]
            k = akeys[ii]
            v = avals[ii]

            if not(nn):
                ext = " (the majority)"
            else:
                all_ind = list(np.where(L == k)[0] + 1)
                ind_str = ", ".join([str(ind) for ind in all_ind])
                ext     = ", in lines: {}".format(ind_str)
            print("   {:6d} rows have {} items{}".format(v, k, ext))

    return dict_of_len

def sep_for_file(fname):
    """Rules to decide what char/str to use as a within-line separator for
a particular data file (mainly from its extension), such as CSV,
*.csv.gz, TSV, etc.

Parameters
----------
fname : str
    full name of file (path not needed, but OK); file need not exist

Returns
-------
sep : str
    string that will be used to separate items within each row.

    """

    # decide what to use as a within-line separator
    fname_low = fname.lower()
    if fname_low.endswith('.tsv') or fname_low.endswith('.tsv.gz') :
        sep = '\t'
    elif fname_low.endswith('.csv') or fname_low.endswith('.csv.gz') :
        sep = ','
    else:
        sep = None    # whitespace

    return sep


def read_column_text_to_float_list(fname, verb=1):
    """Read in a text file fname that is supposed to be one or more
columns of numbers.  The file fname will often be TSV or CSV (and can
be zipped), and split appropriately; other extensions will be split at
whitespace.

Identifying and dealing the potential NaN values, such as lines with
alphabetic text or which are empty, are the greatest challenge here.

If a row is empty or space-only, or if an element within a row is
empty or space-only (e.g., if a CSV has only whitespace between two
commas),
-> put a null str '' there, and add to bad_nulllist

If an element within a row cannot be converted with float(x),
-> put a np.nan there, and add to bad_nanlist

Additionally, raggedness is checked for in the file
-> recorded in dict_of_len

** NB: if any of the following is true for the outputs:
     len(bad_nulllist) > 0
     len(bad_nanlist)  > 0
     len(dict_of_len)  > 1
   ... then there is some kind of badness in the input file

If empty and/or whitespace-only rows occur at the end of fname, that
is not considered a warning/error.

Parameters
----------
fname : str
    text file, assumed to be columnar
verb : int
    verbosity level whilst processing

Returns
-------
tlist : list
    list form of text file, all numbers converted to floats (including
    NaNs) or empty strings (''s).  Each element in tlist is a list of
    floats and ''s.  The idea is ''s will be flagged to be dealt with
    later, not just left as they are!
bad_nanlist : list (of lists)
    list of NaNs occurring; each element of nanlist is a sublist, with
    elements:
      [0] zerobase index of row with the NaN
      [1] the full string of the NaN-causing row
bad_nulllist : list (of lists)
    list of ''s occurring; each element of nulllist is a sublist, with
    elements:
      [0] zerobase index of row which led to ''
      [1] the full string of the ''-causing row
dict_of_len : dict
    a dictionary summarizing the lengths of each line in fname.  Each
    key is a length of line occurring in the file, and each value is
    how many times that line length occurred.

    """

    BAD_RETURN = {}

    if verb:
        print("++ Start reporting this file:\n   {}".format(fname))

    if not(os.path.isfile(fname)) :
        print("** ERROR: cannot read file:\n   {}".format(fname))
        return BAD_RETURN

    # fname can be either zipped or unzipped
    if fname.endswith('.gz'):    opener = gzip.open 
    else:                        opener = open

    # decide what to use as a within-line separator
    sep = sep_for_file(fname)

    last_nonempty_row = -1          # used to trim empty rows at end
    idx   = 0
    tlist = []                      # total list, of lists or ''
    bad_nanlist  = []               # bad list: record nans
    bad_nulllist = []               # bad list: null/empty/missing items
    all_lenlist  = []               # count items per row
    with opener(fname, 'rt') as fff:
        for row in fff:
            # split row based on type of file
            rowsp     = row.split(sep)
            len_rowsp = len(rowsp)

            if len_rowsp == 0 :  
                # when sep=None and a row is empty/whitespace
                bad_nulllist.append([idx, row])
                all_lenlist.append( 0 )
                tlist.append('')
            elif len_rowsp == 1 :
                # single item per line
                xfl = float_or_nan_or_whitespace(rowsp[0])
                if '' == xfl :
                    bad_nulllist.append([idx, row])
                    all_lenlist.append( 0 )
                elif np.isnan(xfl) :
                    bad_nanlist.append([idx, row])
                    all_lenlist.append( 1 )
                    last_nonempty_row = idx
                else:
                    all_lenlist.append( 1 )
                    last_nonempty_row = idx 
                tlist.append([xfl])
            else:
                # multicolumn
                xfllist = [float_or_nan_or_whitespace(x) for x in rowsp]
                # one row can be in multiple bad lists
                if '' in xfllist :
                    bad_nulllist.append([idx, row])
                if np.sum([np.isnan(x) for x in xfllist \
                           if isinstance(x, float)]) :
                    bad_nanlist.append([idx, row])
                all_lenlist.append( len_rowsp )
                tlist.append(xfllist)
                last_nonempty_row = idx
            idx+=1 

    # clean up stuff

    # count number of empty/whitespace-only rows at end
    Nempty_end = idx - last_nonempty_row - 1
    if verb and Nempty_end:
        print("+* NB: there were {} empty/whitespace-only rows at the "
              "end of the file".format(Nempty_end))

    # if empty rows are simply at end of file, don't treat as bad and
    # remove from list (doing so from back of list)
    Nnull = len(bad_nulllist)
    for ii in range(Nnull-1, -1, -1):
        if bad_nulllist[ii][0] > last_nonempty_row :
            tmp = bad_nulllist.pop(ii)
    Nnull = len(bad_nulllist)

    # remove empty end rows from these lists, too
    tlist       = copy.deepcopy(tlist[:last_nonempty_row+1])
    all_lenlist = copy.deepcopy(all_lenlist[:last_nonempty_row+1])

    # flag any raggedness, ignoring bad_nullist items
    dict_of_len = check_raggedness_of_lines(all_lenlist, verb=verb)

    # COMMENT:
    # at this point there are 2 kinds of badness marked in the tlist: 
    #   ''     : where a row or element value was '' or whitespace
    #            (and see bad_nulllist)
    #   np.nan : otherwise, where float(x) would cause an error
    #            (and see bad_nanlist)
    # additionally, if len(dict_of_len)>1, then there is badness in
    # the form of raggedness in the input file.

    tmp = report_on_bad_list(bad_nanlist, label='nan', verb=verb)
    tmp = report_on_bad_list(bad_nulllist, label='null', verb=verb)

    if verb:    print("++ End reporting.")

    return tlist, bad_nanlist, bad_nulllist, dict_of_len

def report_on_bad_list(L, label='', verb=0):
    """Print information to terminal about a bad list.  Recall that it is
the zero-based array index stored, and we generally want to report the
one-based line number."""

    N = len(L)
    if label[-1] != ' ':
        label = label + ' '

    # nothing to do
    if not(N): 
        print("++ Good: No items found in bad {}list".format(label))
        return 0

    print("+* WARN: Found items in bad {}list, N = {}".format(label, N))
    if verb:
        for x in L:
            idx = x[0] + 1
            txt = repr(x[1]) # raw/canonical string 
            print("   [line: {:5d}] {}".format(idx, txt))
        
    return 0
