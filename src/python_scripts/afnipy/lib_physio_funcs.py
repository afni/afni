#!/usr/bin/env python

import sys, os
import copy
import numpy  as np
from   scipy  import signal             as sps

from   afnipy import lib_physio_opts    as lpo
from   afnipy import lib_physio_peaks   as lpp
from   afnipy import lib_physio_phases  as lpph
from   afnipy import lib_physio_rvt     as lprvt
from   afnipy import lib_physio_convolve as lpcon
from   afnipy import lib_physio_plot    as lpplt
from   afnipy import lib_physio_util    as lpu


# ===========================================================================

# set of allowed time series labels for calculation at present
PO_all_label = ['card', 'resp']
PO_rvt_label = ['resp']
PO_hr_label  = ['card']

# ===========================================================================

def check_label_all(label):
    """Simple check for most main funcs, that label is both present and
valid."""

    if not(label) :
        print("** ERROR: must provide label kwarg from allowed list:")
        print("   {}".format(', '.join(PO_all_label)))
        sys.exit(3)
    elif not(label in PO_all_label) :
        print("** ERROR: label '{}' is not in allowed list:".format(label))
        print("   {}".format(', '.join(PO_all_label)))
        sys.exit(3)

def check_label_rvt(label):
    """Simple check for some main funcs, that label is both present and
valid for RVT calcs (e.g., might only apply to 'resp' label)."""

    if not(label) :
        print("** ERROR: must provide label kwarg from allowed RVT list:")
        print("   {}".format(', '.join(PO_rvt_label)))
        sys.exit(3)
    elif not(label in PO_rvt_label) :
        print("** ERROR: label '{}' is not in allowed RVT list:".format(label))
        print("   {}".format(', '.join(PO_rvt_label)))
        sys.exit(3)

def check_label_hr(label):
    """Simple check for some main funcs, that label is both present and
valid for HR calcs (e.g., might only apply to 'card' label)."""

    if not(label) :
        print("** ERROR: must provide label kwarg from allowed HR list:")
        print("   {}".format(', '.join(PO_hr_label)))
        sys.exit(3)
    elif not(label in PO_hr_label) :
        print("** ERROR: label '{}' is not in allowed HR list:".format(label))
        print("   {}".format(', '.join(PO_hr_label)))
        sys.exit(3)

def check_empty_list(x, count, lab_title, label):
    """Simple check whether a peak, trough or phase list x is empty or
not.  Whine if it is, and return nonzero.  If things are cool, just
return 0.

    """

    if len(x) == 0 :
        print("** ERROR: Step [{}] '{}' creates empty list for {} data"
              "".format(count, lab_title, label))
        return 1
    return 0


# ---------------------------------------------------------------------------

def calc_timing_selection_phys(pcobj, label=None, verb=0):
    """Calculate the 'timing selection array' for the ts_obj
(=pcobj.data[label]) time series tvalues, for each MRI slice based on
the slice timing information.  That is, for any of the
card/resp/etc. time series, we will likely output slicewise
regressors, and we need to know where to sample the physio time series
to match with each MRI slice.  The 'timing array' records this
information, for each type of data.  We basically step through the
finely-sampled physio data 'x-axis', and figure out which values
correspond to a given coarse-sampled MRI slice.

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
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    if verb : print("++ Start phys arr timing calc for {} data".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    slice_times = pcobj.vol_slice_times            # coarse, MRI timing
    nslice      = len(slice_times)                  # num of MRI slices
    vol_nv      = pcobj.vol_nv                     # num of MRI vols
    vol_tr      = pcobj.vol_tr                     # coarse, MRI sampling (s)
    tvals       = pcobj.data[label].tvalues        # fine, physio ts timing
    ntval       = len(tvals)                        # num of physio timepts
    samp_delt   = pcobj.data[label].samp_delt      # fine, physio sampling (s)
    
    # init output list of lists, holds: label, [timing indices]
    list_slice_sel_phys = []

    # go through, and find close time point in physio array to match MRI one
    for ii in range(nslice):
        lab = 's{:03d}.{}'.format(ii, label)
        # calc list of time-array indices for all MRI volumes for this slice
        all_ind = find_vol_slice_times(tvals, samp_delt, slice_times[ii], ii,
                                       vol_nv, vol_tr, verb=verb)
        list_slice_sel_phys.append([lab, copy.deepcopy(all_ind)])

    # done, store
    tsobj.list_slice_sel_phys = list_slice_sel_phys

    return 0

def calc_timing_selection_volbase(pcobj, label=None, verb=0):
    """Calculate the 'timing selection array' for the ts_obj
(=pcobj.data[label]) time series tvalues, **just starting at time 0,
for any volume-based regressor**, based on the slice timing
information.  That is, we need to know where to sample the physio time
series to match with the MRI volume (just initial slice, which we
assume is at t=0.0).  The 'timing array' records this information.  We
basically step through the finely-sampled physio data 'x-axis', and
figure out which values correspond to a given coarse-sampled MRI
slice.

NB: we *could* have just gotten this info as a special case of the
full physio slicewise regressors calculation, but decided to keep this
separate, to be more general.

Used for RVT, HR, etc.

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
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    if verb : print("++ Start volbase arr timing calc for {}".format(label))

    # this is for either card or resp data
    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    slice_times = pcobj.vol_slice_times            # coarse, MRI timing
    nslice      = len(slice_times)                  # num of MRI slices
    vol_nv      = pcobj.vol_nv                     # num of MRI vols
    vol_tr      = pcobj.vol_tr                     # coarse, MRI sampling (s)
    tvals       = pcobj.data[label].tvalues        # fine, physio ts timing
    ntval       = len(tvals)                        # num of physio timepts
    samp_delt   = pcobj.data[label].samp_delt      # fine, physio sampling (s)
    
    # calc list of time-array indices for all MRI volumes for this slice
    all_ind = find_vol_slice_times(tvals, samp_delt, 0.0, 0,
                                   vol_nv, vol_tr, verb=verb)

    # done, store
    tsobj.list_slice_sel_volbase = all_ind

    return 0

def find_vol_slice_times(tvals, samp_delt, slice_time0, slice_idx,
                         vol_nv, vol_tr, verb=0):
    """Take in a list of finely sampled physio data, whose 'x-axis' of
time values is tvals at sampling interval samp_delt.  Start at time
slice_time0, and find the list of indices corresponding to MRI slices
acquired every TR=vol_tr; there are vol_nv of them.

Parameters
----------
tvals : np.ndarray 
    1D array of physical time values (units = sec)
samp_delt : float
    sampling interval of the physio time series (basically, delta_t in tvals)
slice_time0 : float
    the value of the slice time to start from
slice_idx : int
    the index corresponding to which element in the slice timing list
    we are processing, for reporting purposes
vol_nv : int
    number of MRI volumes to get times for
vol_tr : float
    the MRI volume's TR (= sampling interval), in units of sec

Returns
-------
all_ind : list
    list of integers, representing where each MRI volume was acquired for
    this particular slice; later, these indices are used to select out values
    from other data times series that have the same length as tvals

    """

    ntval = len(tvals)              # len of tvalue array
    EPS   = 1.1* samp_delt / 2.0    # slightly generous epsilon

    all_ind  = []                   # init: empty list of indices
    start    = 0                    # init: first index in tvals
    jj       = 0                    # init: first volume in MRI
    while jj < vol_nv :
        sli_time = slice_time0 + jj*vol_tr
        for kk in range(start, ntval):
            if sli_time < tvals[kk] or abs(sli_time - tvals[kk])<=EPS:
                # decision making about closest kk value
                idx_store = kk
                if kk > 0 :
                    # the idx before we stopped *could* be closer
                    if abs(sli_time-tvals[kk]) > abs(sli_time-tvals[kk-1]):
                        idx_store = kk-1
                all_ind.append(idx_store)
                start = idx_store+1
                jj+= 1
                break

    if len(all_ind) != vol_nv:
        print("** ERROR in slice {}'s timing:\n"
              "   found {} times (not nv={} of them) in the interval\n"
              "   {}..{} s, samp_delt = {}s"
              "".format(slice_idx, len(all_ind), nslice, 
                        tvals[0], tvals[-1], samp_delt))
        sys.exit(5)

    return all_ind


# --------------------------------------------------------------------------

def make_str_ts_peak_trough(label, idx, lab_title, lab_short,
                            prefix='', odir='', ext='svg', 
                            all_subscript=True):
    """Construct an output filename for time series peak/trough plot, as
well as a title for the plot.

Parameters
----------
label : str
    label for type of time series: 'card', 'resp', etc.
idx : int
    this is for the idx-th step in processing, typically; gets 
    zeropadded to 2 numbers (00, 01, etc.)
lab_title : str
    longer descriptor of the idx-th step, for the title
lab_short : str
    short descriptor of the idx-th step, for the filename
prefix : str
    optional basename for full output
odir : str
    optional path for output directory
ext : str
    file extension type (prob stick with vector graphics)
all_subscript : bool
    bc one title has a subscript, make sure *all* do, so vertical
    space taken by title is constant (and figs don't jump); this adds
    a whitespace subscript at the end of the title, if no '$' is 
    already present in the title.

Returns
-------
fname : str
    name of file to be written
title : str
    string for the title at the top of the figure

    """

    # make the filename
    fname = '{}_{:02d}_{}.{}'.format(label, idx, lab_short, ext)
    if prefix :  fname = prefix + '_' + fname
    if odir :    fname = odir + '/' + fname

    # make the title string
    title = 'Process {} data: {}'.format(label, lab_title)

    # ensure subscript across all
    if all_subscript and not('$' in title) :
        title+= '$_{\\rm ~}$'

    return fname, title
    
# --------------------------------------------------------------------------

def calc_time_series_peaks(pcobj, label=None, verb=0):

    """Calculate the peaks for one of the ts_obj time series.  The time
series and processing is determined by the label given.  There is both
overlap and differentiation in the processing of various time series.

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
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    BAD_RETURN = 1    # in case things go awry

    if verb : 
        print("++ Start peak/trough calc for {} data".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    if verb > 1 :
        print("++ In plots, step interval for lines: {}"
              "".format(tsobj.img_arr_step))

    # -------- start of peak+trough estimation and refinement steps --------

    peaks   = []                                # null
    troughs = []                                # null; might not be calc'ed

    # each block of peak/trough detection works in about the same way,
    # and we comment on the pieces just in the first part

    # --------------
    count    = 0                                     # proc/filter steps
    lab_title = 'Bandpass and SciPy peak-finding'    # used in fig img
    lab_short = 'bp_scipy_peaks'                     # used in fig filename
    if verb :   print('++ ({}) {}'.format(label, lab_title))

    # calculate peak and/or trough list (here, plus some extra info)
    peaks, idx_freq_mode, xfilt = \
        lpp.get_peaks_from_bandpass(tsobj.ts_orig,
                                    tsobj.samp_freq,
                                    tsobj.min_bps,
                                    max_bps=tsobj.max_bps,
                                    label=label,
                                    pcobj=pcobj,
                                    bp_sig_fac=tsobj.bp_sig_fac,
                                    verb=verb)
    tsobj.ts_orig_bp = copy.deepcopy(xfilt)          # save BPed ver of ts
    tsobj.bp_idx_freq_mode = idx_freq_mode           # save peak freq's idx
    if check_empty_list(peaks, count, lab_title, label) :  return BAD_RETURN

    # save output figure, if desired
    if pcobj.img_verb > 1 :
        # make image title and filename
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short,
                                               prefix=prefix, odir=imdir)
        # make the image
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

        # bonus here: plot peaks on BP'ed time series
        lab_title = 'Bandpassed time series, with peaks'
        lab_short = 'bandpass_ts_peaks'
        fname, title = lpu.make_str_bandpass(label,
                                             lab_title, lab_short, 
                                             prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          use_bp_ts=True,
                                          verb=verb)

    # --------------
    count+= 1
    lab_title = 'Local peak refinement'
    lab_short = 'local_refine_peaks'
    if verb :   print('++ ({}) {}'.format(label, lab_title))
    peaks = lpp.refinePeakLocations(peaks, 
                                    tsobj.ts_orig,
                                    is_troughs = False,
                                    verb=verb)
    if check_empty_list(peaks, count, lab_title, label) :  return BAD_RETURN

    if pcobj.img_verb > 1 :
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short,
                                               prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

    # --------------
    count+= 1
    if label == 'card' :
        lab_title = 'Local peak percentile filter'
        lab_short = 'perc_local_peaks'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        peaks = lpp.percentileFilter_local(peaks, 
                                           tsobj.ts_orig,
                                           is_troughs = False,
                                           verb=verb)
    elif label == 'resp' :
        lab_title = 'Global peak percentile filter'
        lab_short = 'perc_global_peaks'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        peaks = lpp.percentileFilter_global(peaks, 
                                            tsobj.ts_orig,
                                            is_troughs = False,
                                            verb=verb)
    if check_empty_list(peaks, count, lab_title, label) :  return BAD_RETURN

    if pcobj.img_verb > 1 :
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short,
                                               prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

    # --------------
    count+= 1
    lab_title = 'Peak proximity filter'
    lab_short = 'proxim_filter_peaks'
    if verb :   print('++ ({}) {}'.format(label, lab_title))
    # NB: Originally used
    # lpp.getTimeSeriesPeriod_as_indices(tsobj.ts_orig) to get the
    # period_idx kwarg for this func; then seemed to make sense to use
    # the previously calc'ed idx_freq_mod instead, which should avoid
    # baseline drift.  *Then*, with some time series that have
    # multiple main freqs, came to view it best to put in *no*
    # period_idx par, but instead use med(interpeak interval) in the
    # code.
    peaks = lpp.removeClosePeaks(peaks, 
                                 tsobj.ts_orig,
                                 is_troughs = False,
                                 verb=verb)
    if check_empty_list(peaks, count, lab_title, label) :  return BAD_RETURN

    if pcobj.img_verb > 1 :
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short,
                                               prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

###### [PT: Nov 22, 2023] Disable this for now.  Too many datasets
###### have too many gaps and instances of large variability for doing
###### this *without* also checking for a local bump on which a peak
###### should be placed.  This can be revisited.
#    # --------------
#    count+= 1
#    lab_title = 'Add any missing peaks'
#    lab_short = 'add_missing_peaks'
#    if verb :   print('++ ({}) {}'.format(label, lab_title))
#    peaks = lpp.addMissingPeaks(peaks, 
#                                tsobj.ts_orig,
#                                is_troughs = False,
#                                verb=verb)
#    if check_empty_list(peaks, count, lab_title, label) :  return BAD_RETURN
#
#    if pcobj.img_verb > 1 :
#        fname, title = make_str_ts_peak_trough(label, count, 
#                                               lab_title, lab_short,
#                                               prefix=prefix, odir=imdir)
#        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
#                                          title=title, fname=fname,
#                                          pcobj=pcobj,
#                                          verb=verb)


    # -------- trough considerations: resp only
    if label == 'resp' :
        # at present, ONLY troughs change in this IF condition branch

        # --------------
        count+= 1
        lab_title = 'Bandpass and SciPy trough-finding'    # used in fig img
        lab_short = 'bp_scipy_troughs'                          
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        troughs, _ = sps.find_peaks(-xfilt,
                                    width=int(tsobj.samp_freq/8))
        if check_empty_list(troughs, count, lab_title, label) :  
            return BAD_RETURN

        if pcobj.img_verb > 1 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short,
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)

            # bonus here: plot peaks on BP'ed time series
            lab_title = 'Bandpassed time series, with troughs'
            lab_short = 'bandpass_ts_troughs'
            fname, title = lpu.make_str_bandpass(label,
                                                 lab_title, lab_short, 
                                                 prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, 
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              use_bp_ts=True,
                                              verb=verb)

        # --------------
        count+= 1
        lab_title = 'Local trough refinement'
        lab_short = 'local_refine_troughs'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        troughs = lpp.refinePeakLocations(troughs, 
                                          tsobj.ts_orig,
                                          is_troughs = True,
                                          verb=verb)
        if check_empty_list(troughs, count, lab_title, label) :
            return BAD_RETURN

        if pcobj.img_verb > 1 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short,
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)

        # --------------
        count+= 1
        lab_title = 'Global trough percentile filter'
        lab_short = 'perc_global_troughs'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        troughs = lpp.percentileFilter_global(troughs, 
                                              tsobj.ts_orig,
                                              perc_filt = 90.0,
                                              is_troughs = True, 
                                              verb=verb)
        if check_empty_list(troughs, count, lab_title, label) :
            return BAD_RETURN

        if pcobj.img_verb > 1 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short,
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks, 
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)

        # note: leaving out separate left/right merger checks, because
        # those are examined in global check

        # --------------
        count+= 1
        lab_title = 'Trough proximity filter'
        lab_short = 'proxim_filt_troughs'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        # NB: for period_idx arg here, could use
        # lpp.getTimeSeriesPeriod_as_indices(tsobj.ts_orig), but
        # instead use previously calc'ed idx_freq_mod, which should
        # avoid baseline drift.
        troughs = lpp.removeClosePeaks(troughs, 
                                       tsobj.ts_orig,
                                       is_troughs = True,
                                       verb=verb)
        if check_empty_list(troughs, count, lab_title, label) :
            return BAD_RETURN

        if pcobj.img_verb > 1 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short,
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)

###### [PT: Nov 22, 2023] Disable this for now.  Too many datasets
###### have too many gaps and instances of large variability for doing
###### this *without* also checking for a local bump on which a trough
###### should be placed.  This can be revisited.
#        # --------------
#        count+= 1
#        lab_title = 'Add any missing troughs'
#        lab_short = 'add_missing_troughs'
#        if verb :   print('++ ({}) {}'.format(label, lab_title))
#        troughs = lpp.addMissingPeaks(troughs, 
#                                      tsobj.ts_orig,
#                                      is_troughs = True,
#                                      verb=verb)
#        if check_empty_list(troughs, count, lab_title, label) :
#            return BAD_RETURN
#
#        if pcobj.img_verb > 1 :
#            fname, title = make_str_ts_peak_trough(label, count, 
#                                                   lab_title, lab_short,
#                                                   prefix=prefix, odir=imdir)
#            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
#                                              troughs=troughs,
#                                              title=title, fname=fname,
#                                              pcobj=pcobj,
#                                              verb=verb)


    # ----- DONE with peak+trough estimation+refinement

    # add calculated results to obj 
    tsobj.peaks = peaks
    if len(troughs) :
        tsobj.troughs = troughs
    tsobj.proc_count+= count
    
    # Record filtered time series for this label
    if (not hasattr(pcobj, 'ts_orig_bp')): pcobj.ts_orig_bp = {}
    pcobj.ts_orig_bp[label] = tsobj.ts_orig_bp

    return 0

def run_interactive_peaks(pcobj, label=None, verb=0):
    """Do the interactive peak/trough selection for the 'label' data in
the pcobj. This updates the peaks within pcobj itself.

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
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    BAD_RETURN = 1    # in case things go awry

    if verb : 
        print("++ Start interactive plot for {} data".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    # set up local quantities
    count = tsobj.proc_count
    peaks = tsobj.peaks
    troughs = tsobj.troughs    # might be empty list, which is fine
    p_ival = np.median([j-i for i, j in zip(peaks[:-1], peaks[1:])])
    p_ival*= tsobj.samp_delt
    # **** decide if we still need local peaks and troughs arrays in
    # **** this func

    if verb > 1 :
        print("++ In plots, step interval for lines: {}"
              "".format(tsobj.img_arr_step))

    # ----- run INTERACTIVE plot
    count+=1
    lab_title = 'Interactive peaks ($\\Delta t_{\\rm med}$ = '
    lab_title+= '{:0.3f} s)'.format(p_ival)
    lab_short = 'interact_peaks'
    if len(troughs) :
        lab_title+= ' and troughs'
        lab_short+= '_troughs'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        if pcobj.img_verb > 0 :
            # [PT] *** should we output to imdir here?
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short,
                                                   prefix=prefix, 
                                                   odir=odir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              troughs=troughs,
                                              add_ibandT=True,
                                              add_ibandB=True,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              do_show=True,
                                              do_interact=True,
                                              do_save=False,
                                              verb=verb)
            # ... and update local ones
            peaks   = copy.deepcopy(tsobj.peaks)
            troughs = copy.deepcopy(tsobj.troughs)
    else:
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        if pcobj.img_verb > 0 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short, 
                                                   prefix=prefix, 
                                                   odir=odir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              add_ibandT=True,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              do_show=True,
                                              do_interact=True,
                                              do_save=False,
                                              verb=verb)
            # ... and update local ones
            peaks = copy.deepcopy(tsobj.peaks)

    # add calculated results to obj:
    # NB: the peaks/troughts get updated in tsobj when the interactive
    # mode is on in pplt.makefig_tsobj_peaks_troughs(), which it is by
    # definition here
    tsobj.proc_count+= count

    return 0

def make_final_image_peaks(pcobj, label=None, verb=0):
    """Make the final plot of peak/trough values for the 'label' data in
the pcobj.

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
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    BAD_RETURN = 1    # in case things go awry

    if verb : 
        print("++ ({}) Make final peaks/troughs plot".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    # set up local quantities
    count = tsobj.proc_count
    peaks = tsobj.peaks
    troughs = tsobj.troughs    # might be empty list, which is fine
    p_ival = np.median([j-i for i, j in zip(peaks[:-1], peaks[1:])])
    p_ival*= tsobj.samp_delt

    # -------------- FINAL plot
    if count >= 10 :
        count+=1
    else:
        count = 10
    lab_title = 'Final peaks ($\\Delta t_{\\rm med}$ = '
    lab_title+= '{:0.3f} s)'.format(p_ival)
    lab_short = 'final_peaks'
    if len(troughs) :
        lab_title+= ' and troughs'
        lab_short+= '_troughs'
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        if pcobj.img_verb > 0 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short,
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              troughs=troughs,
                                              add_ibandT=True,
                                              add_ibandB=True,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)
    else:
        if verb :   print('++ ({}) {}'.format(label, lab_title))
        if pcobj.img_verb > 0 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short, 
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=peaks,
                                              add_ibandT=True,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)

    return 0

# ===========================================================================
# ===========================================================================

def calc_time_series_phases(pcobj, label=None, verb=0):
    """Calculate the phases of the peaks (possibly also using troughs) for
one of the ts_obj time series.  The time series and processing is
determined by the label given. 

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
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    if verb : print("++ Start phase calc for {} data".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    count     = 20                          # start with num >> peak/trough est
    lab_title = 'Estimating phase'
    lab_short = 'est_phase'
    if verb :   print('++ ({}) {}'.format(label, lab_title))

    # ------- card phase estimation
    if label == 'card' :
        # card case is simple method
        phases = lpph.calc_phases_M1(tsobj, verb=verb)
        if check_empty_list(phases, count, lab_title, label) :  return 1

        if pcobj.img_verb > 1 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short, 
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=tsobj.peaks,
                                              phases=phases,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)

    elif label == 'resp' :
        #phases = lpph.calc_phases_M2(tsobj, verb=verb)  # older method
        phases = lpph.calc_phases_M3(tsobj, verb=verb)
        if check_empty_list(phases, count, lab_title, label) :  return 1

        if pcobj.img_verb > 1 :
            fname, title = make_str_ts_peak_trough(label, count, 
                                                   lab_title, lab_short, 
                                                   prefix=prefix, odir=imdir)
            lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=tsobj.peaks,
                                              troughs=tsobj.troughs,
                                              phases=phases,
                                              title=title, fname=fname,
                                              pcobj=pcobj,
                                              verb=verb)


    # ----- DONE with phase estimation: add to obj -----
    tsobj.phases = phases

    return 0

# ===========================================================================

def calc_time_series_rvt(pcobj, label=None, verb=0):
    """Calculate regression volume per time (RVT), as described in:

    ``Separating respiratory-variation-related fluctuations from
    neuronal-activity-related fluctuations in fMRI'' by Rasmus
    M. Birn, Jason B.  Diamond, Monica A. Smith, and Peter
    A. Bandettini (2006).

This just uses the time series and respiration peak/trough info to
calculate an envelope for the time series, and the inter-peak
intervals to estimate 'instantaneous period'.

    """

    if verb : print("++ Start RVT calc for {} data".format(label))

    check_label_all(label)
    check_label_rvt(label)      # a practical RVT reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    # ------ time series envelope
    count     = 21 
    lab_title = 'RVT envelope estimation'
    lab_short = 'rvt_env'
    if verb :   print('++ ({}) {}'.format(label, lab_title))

    # calculate the upper and lower envelope
    upper_env = lprvt.interp_extrema_LIN(tsobj, tsobj.peaks, verb=verb)
    lower_env = lprvt.interp_extrema_LIN(tsobj, tsobj.troughs, verb=verb)    

    if pcobj.img_verb > 1 :
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=tsobj.peaks,
                                          troughs=tsobj.troughs,
                                          upper_env=upper_env,
                                          lower_env=lower_env,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

    # actual RVT (only plotted as regressor, later)
    count    += 1
    lab_title = 'RVT measure'
    lab_short = 'rvt_measure'
    if verb :   print('++ ({}) {}'.format(label, lab_title))
    insta_per = lprvt.interp_intervals_LIN(tsobj, tsobj.peaks, verb=verb)
    rvt_ts    = (upper_env - lower_env) / insta_per

    if pcobj.img_verb > 1 :
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=tsobj.peaks,
                                          troughs=tsobj.troughs,
                                          rvt=rvt_ts, 
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

    tsobj.rvt_ts = rvt_ts

    return 0

def calc_time_series_hr(pcobj, label=None, win=6, verb=0):
    """Calculate average heart rate (HR), as described in:

    ``Influence of heart rate on the BOLD signal: The cardiac response
    function'' by Catie Chang a, John P. Cunningham a, Gary H. Glover
    (2009).

NB: Output here is on the FMRI/EPI grid. 

This is a fairly straightforward calc that doesn't require pre-calc on
the card time series level.  It just uses the time series and card
inter-peak intervals to estimate 'heart rate'.  We calculate the mean
interpeak interval in a win of time (units: s) around a given TR. The
result is divided by 60, to have units of beats per minute.

    """

    if verb : print("++ Start HR regressor calc for {} data".format(label))

    check_label_all(label)
    check_label_hr(label)       # a practical HR reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    imdir  = pcobj.images_dir
    prefix = pcobj.prefix

    # ------ time series average interpeak interval (**EPI time grid**)
    count     = 22
    lab_title = 'HR average rate estimation'
    lab_short = 'hr_ave'
    if verb :   print('++ ({}) {}'.format(label, lab_title))

    # time-related values
    tr   = pcobj.vol_tr         # EPI data sampling interval (s)
    delt = tsobj.samp_delt       # phys data sampling interval (s) 
    win  = tsobj.hr_win          # window for average HR (s) 
    nperwin = int(win/delt)      # N phys samples per window

    # lists of ints on the physio grid
    peaks  = tsobj.peaks                   # indices of peaks
    npeaks = len(peaks)
    all_tr = tsobj.list_slice_sel_volbase  # indices of EPI TR locs
    ntr    = len(all_tr)

    if verb > 1 :
        print("   window size time, delt and nperwin:", win, delt, nperwin)

    ## *** Note *** 
    # Here we have 2 systems of indices we care about: 'time indices'
    # that maps directly onto physical time, if we multiplied it by
    # the physio sampling interval, delt; and 'peak indices' which are the
    # indices of the peaks list, which define where physio peaks
    # occurred. Below, bot and top define a time interval window in
    # terms of time indices; we search through to find peak indices
    # within that window, because we are interested in the average
    # interpeak interval within the time window.  Interpeak intervals
    # can/will generally fractionally extend beyond the time window,
    # and we try to account for those bits.  At the EPI TR boundaries,
    # we likely have to truncate the EPI windows in a one-sided manner.

    min_p = 0      # to be min index of peaks for per-TR window
    max_p = 0      # to be min index of peaks for per-TR window
    hr_ave = []    # store all average heart rate values

    for ii in range(ntr):
        # define current window around ii-th TR: the possible range of
        # time index indices within which to find peaks
        bot = all_tr[ii] - int(nperwin/2)
        top = all_tr[ii] + int(nperwin/2)

        # respect known boundaries of physio data index range
        if bot < 0 : 
            bot = 0
        if top >= tsobj.n_ts_orig : 
            top = tsobj.n_ts_orig - 1

        # find idx vals for min/max window range in peaks
        while peaks[min_p] < bot and min_p < npeaks :
            min_p+= 1
        max_p = min_p
        while peaks[max_p] < top and max_p < npeaks-1 :
            max_p+= 1
        max_p-= 1  # bc this went one too high
        # verify no obvious badness has occurred; bc of boundaries,
        # min_p cd be equal to all_tr[ii], etc.
        ## + it is OK to have "min_p == max_p", which just means there
        ##   was only 1 peak in the window (should still be fractions
        ##   around); for default peak of 6s, in card data this would only
        ##   likely happen for test data
        ## + it is actually OK for min_p to be above the TR index,
        ##   because the first peak might be after like TR=0
        ## + similarly, the last peak could be at an index below the 
        ##   max TR value
        if min_p >= npeaks or max_p >= npeaks or min_p > max_p :
            print("** ERROR: bad min_p or max_p calc: ({}, {})"
                  "".format(min_p, max_p))
            print("   (bot, top) = ({}, {}); peaks[min], peaks[max] = ({}, {})"
                  "".format(bot, top, peaks[min_p], peaks[max_p]))
            print("   npeaks: {}, [{}]th TR, all_tr[ii]: {}"
                  "".format(npeaks, ii, all_tr[ii]))
            sys.exit(3)

        # Part 1 of average: count num of peaks in this interval, and
        # their first peak-to-last peak time duration; time_ival will
        # be scaled by delt below, when we are done
        peak_count = float(max_p - min_p)
        time_ival  = float(peaks[max_p] - peaks[min_p])

        # Part 2 of average: see if we can/should add fractional
        # interval on left side of window; this means stepping back
        # one index in peaks, and calculating relative fraction of
        # peak-to-peak interval within the current window
        if min_p > 0 and peaks[min_p] > bot :
            numer = float(peaks[min_p] - bot)
            denom = float(peaks[min_p] - peaks[min_p - 1])
            if denom <= 0 or numer <= 0:
                print("** ERROR: neg val in left peak (numer, denom): ({}, {})"
                      "".format(numer, denom))
                sys.exit(3)
            frac = numer/denom
            if frac <= 0 :
                print("** ERROR: bad left peak frac=numer/denom: {}; "
                      "(numer, denom): ({}, {})".format(frac, numer, denom))
                sys.exit(3)
            peak_count+= frac
            time_ival+= numer

        # Part 3 of average: see if we can/should add fractional
        # interval on right side of window; this means stepping
        # forward one index in peaks, and calculating relative
        # fraction of peak-to-peak interval within the current window
        if max_p < npeaks and peaks[max_p] < top :
            numer = float(top - peaks[max_p])
            denom = float(peaks[max_p + 1] - peaks[max_p])
            if denom <= 0 or numer <= 0:
                print("** ERROR: neg val in right peak (numer, denom): ({}, {})"
                      "".format(numer, denom))
                sys.exit(3)
            frac = numer/denom
            if frac <= 0 :
                print("** ERROR: bad right peak frac=numer/denom: {}; "
                      "(numer, denom): ({}, {})".format(frac, numer, denom))
                sys.exit(3)
            peak_count+= frac
            time_ival+= numer

        # peak count should definitely be positive, and it seems
        # impossible for the value to be less than 1 for a reasonable
        # window, without some problem in the calc/data
        if peak_count < 1 :
            print("+* Warn: Peak count < 1 (which is quite small): {}"
                  "".format(peak_count))
            if verb > 7 :
                print("   [{}]th TR, win bot, top, time_ival are: {}, {}, {}"
                      "".format(ii, bot, top, time_ival))

        # scale the time interval to have units of physical time
        time_ival*= delt

        # *finally* the average HR here (= num peaks per time interval)
        if time_ival :
            hr = peak_count / time_ival
        else:
            hr = 0.0

        if verb > 5 :
            if ii == 0 :
                print("++ (card) ave HR per TR")
            print("   [{:4d}] peak_count = {:0.3f}, time_ival = {:0.3f} "
                  "hr = {:0.3f}".format(ii, peak_count, time_ival,
                                              hr))

        # append this average, and divide by 60 to be beats per min
        hr_ave.append( hr/60. )

    # after going through EPI time series, convert to array
    hr_ave = np.array(hr_ave, dtype=float)

    # actual RVT (only plotted as regressor, later)
    count    += 1
    lab_title = 'HR average'
    lab_short = 'hr_ave'
    if verb :   print('++ ({}) {}'.format(label, lab_title))

    if pcobj.img_verb > 1 :
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=imdir)
        lpplt.makefig_tsobj_peaks_troughs(tsobj, peaks=tsobj.peaks,
                                          troughs=tsobj.troughs,
                                          hr=hr_ave,
                                          title=title, fname=fname,
                                          pcobj=pcobj,
                                          verb=verb)

    tsobj.hr_ts = hr_ave
    
    return 0


# --------------------------------------------------------------------------


def calc_regress_retroicor(pcobj, label=None, verb=0):
    """Calculate physio regressors from the phase info, as described in
Eq. 1 of Glover et al., 2000.

**NB: right now, we just ignore calculating the A and B coeffs,
  because we just use these as regressors, and prefer to ignore
  non-unity scaling at the moment, anyways.


    """

    if verb : print("++ Start physio regressor calc for {} data".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    prefix = pcobj.prefix

    regress_dict_retro = {}

    for mm in range(1, tsobj.M+1):
        # make cos() regressors, and initialize list of lists
        lab = 'c{}'.format(mm)
        regress_dict_retro[lab] = []
        for nn in range(tsobj.n_slice_sel_phys):
            reg = np.cos(mm*tsobj.phases[tsobj.list_slice_sel_phys[nn][1]])
            regress_dict_retro[lab].append([tsobj.list_slice_sel_phys[nn][0],
                                            copy.deepcopy(reg)])

        # make sin() regressors, and initialize list of lists
        lab = 's{}'.format(mm)
        regress_dict_retro[lab] = []
        for nn in range(tsobj.n_slice_sel_phys):
            reg = np.sin(mm*tsobj.phases[tsobj.list_slice_sel_phys[nn][1]])
            regress_dict_retro[lab].append([tsobj.list_slice_sel_phys[nn][0],
                                            copy.deepcopy(reg)])


    tsobj.regress_dict_retro = regress_dict_retro

    return 0


def calc_regress_rvt(pcobj, label=None, verb=0):
    """Calculate RVT regressors, as described in Birn et al.,
2006.  Apply shifts here.

    """

    if verb : print("++ Start RVT regressor calc for {} data".format(label))

    check_label_all(label)
    check_label_rvt(label)      # a practical RVT reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    prefix = pcobj.prefix
    shift_list = pcobj.rvt_shift_list

    nshift = len(shift_list)
    regress_dict_rvt = {}

    if verb :
        print("++ The {} RVT shift values are: {}".format(nshift, shift_list))

    # shifts here are made by shifting a copy of the underlying
    # tvalues array, and then selecting the same MRI-snapshot points.
    # We use the time series median to pad values

    # the primary, unshifted RVT regressor
    ###rvt_regr = tsobj.rvt_ts[tsobj.list_slice_sel_volbase]

    for ii in range(nshift):
        # make shifted regressors
        lab = 'rvt{:02d}'.format(ii)
        shift = shift_list[ii]
        regress_dict_rvt[lab] = get_shifted_rvt(tsobj.rvt_ts,
                                                tsobj.samp_freq,
                                                tsobj.list_slice_sel_volbase,
                                                shift)

    tsobj.regress_dict_rvt = regress_dict_rvt

    # make lineplot image of the RVT regressors
    tmp = lpplt.plot_regressors_rvt(pcobj, label)

    return 0


def get_shifted_rvt(x, samp_freq, all_ind, shift):
    """Take input time series x and shift it by delta_t=shift to the left
or right.  'Gaps' left by shifting are filled in with the first or
last value present from the original time series in the direction of
that shift.

Parameters
----------
x : np.ndarray
    1D array
all_ind : np.ndarray
    array of all original indices for selecting values out of x
samp_freq : float
    sampling frequency of x, in units of Hz
shift: float
    amount of time shift to shift the time series left ('earlier') or
    right ('later')

Returns
-------
y : np.ndarray
    1D array, with shift applied

    """

    Nx   = len(x)                      # len of RVT time series
    Nind = len(all_ind)                # num of sampling points

    # to be output: will hold sampled values
    y = np.zeros(Nind, dtype=x.dtype)

    # shift along RVT grid, in terms of indices
    delta_ind = int(shift * samp_freq)

    # collection of shifted indices
    new_ind = np.array(all_ind) - delta_ind

    # which indices are valid for selecting within x
    arr_use = new_ind <  Nx
    arr_use*= new_ind >= 0

    # get mean value of valid signal points (for mean-padding, below)
    pad_val = np.mean(x[new_ind[arr_use]])

    # get the shifted data, with mean padding
    for ii in range(Nind):
        if arr_use[ii] :    y[ii] = x[new_ind[ii]]
        else:               y[ii] = pad_val

    return y

# ==========================================================================

def calc_regress_rvtrrf(pcobj, label=None, verb=0):
    """Calculate regressor that is the result of convolving the RVT
regressor with an empirical RRF function, as described in Birn et al.,
2008.  This (perhaps obviously) depends on having RVT already
calculated...

    """

    if verb : print("++ Start RVTRRF regressor calc for {} data".format(label))

    check_label_all(label)
    check_label_rvt(label)      # a practical RVT reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    prefix = pcobj.prefix

    regress_dict_rvtrrf = {}

    # the primary, unshifted RVT regressor
    rvt_regr = get_shifted_rvt(tsobj.rvt_ts,
                               tsobj.samp_freq,
                               tsobj.list_slice_sel_volbase,
                               0)

    # convolve RVT with RRF
    rvtrrf_reg = lpcon.convolve_with_kernel(rvt_regr, delt=pcobj.vol_tr,
                                            kernel='rrf_birn08')
    
    # add to dict
    lab = 'rvtrrf'
    regress_dict_rvtrrf[lab]  = rvtrrf_reg
    tsobj.regress_dict_rvtrrf = regress_dict_rvtrrf

    # make lineplot image of the RVTRRF regressors
    tmp = lpplt.plot_regressors_rvtrrf(pcobj, label)

    return 0

# --------------------------------------------------------------------------

def calc_regress_hr(pcobj, label=None, win=6, verb=0):
    """Calculate average heart rate (HR), as described in:

    ``Influence of heart rate on the BOLD signal: The cardiac response
    function'' by Catie Chang a, John P. Cunningham a, Gary H. Glover
    (2009).

Output here is on the FMRI/EPI grid. 

This is a fairly straightforward calc that doesn't require pre-calc on
the card time series level.  It just uses the time series and card
inter-peak intervals to estimate 'heart rate'.  We calculate the mean
interpeak interval in a win of time (units: s) around a given TR. The
result is divided by 60, to have units of beats per minute.

    """

    if verb : print("++ Start HR regressor calc for {} data".format(label))

    check_label_all(label)
    check_label_hr(label)       # a practical HR reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    tsobj  = pcobj.data[label]
    odir   = pcobj.out_dir
    prefix = pcobj.prefix

    regress_dict_hrcrf = {}

    # the average HR time series is already at the locations of EPI TRs
    hr_regr = tsobj.hr_ts

    # convolve RVT with RRF
    hrcrf_reg = lpcon.convolve_with_kernel(hr_regr, delt=pcobj.vol_tr,
                                           kernel='crf_chang09')
    
    # add to dict
    lab = 'hrcrf'
    regress_dict_hrcrf[lab]  = hrcrf_reg
    tsobj.regress_dict_hrcrf = regress_dict_hrcrf

    # make lineplot image of the HRCRF regressors
    tmp = lpplt.plot_regressors_hrcrf(pcobj, label)

    return 0
