

import sys, os
import copy
import numpy  as np
from   scipy  import signal             as sps

from   afnipy import lib_physio_opts    as lpo
from   afnipy import lib_physio_peaks   as lpp
from   afnipy import lib_physio_phases  as lpph
from   afnipy import lib_physio_rvt     as lprvt
from   afnipy import lib_physio_plot    as lpplt


# ===========================================================================

# set of allowed time series labels for calculation at present
PO_all_label = ['card', 'resp']
PO_rvt_label = ['resp']


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

# ---------------------------------------------------------------------------

def calc_timing_selection_phys(retobj, label=None, verb=0):
    """Calculate the 'timing selection array' for the phys_obj
(=retobj.data[label]) time series tvalues, for each MRI slice based on
the slice timing information.  That is, for any of the
card/resp/etc. time series, we will likely output slicewise
regressors, and we need to know where to sample the physio time series
to match with each MRI slice.  The 'timing array' records this
information, for each type of data.  We basically step through the
finely-sampled physio data 'x-axis', and figure out which values
correspond to a given coarse-sampled MRI slice.

Parameters
----------
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
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
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    slice_times = retobj.vol_slice_times            # coarse, MRI timing
    nslice      = len(slice_times)                  # num of MRI slices
    vol_nv      = retobj.vol_nv                     # num of MRI vols
    vol_tr      = retobj.vol_tr                     # coarse, MRI sampling (s)
    tvals       = retobj.data[label].tvalues        # fine, physio ts timing
    ntval       = len(tvals)                        # num of physio timepts
    samp_rate   = retobj.data[label].samp_rate      # fine, physio sampling (s)
    
    # init output list of lists, holds: label, [timing indices]
    list_slice_sel_phys = []

    # go through, and find close time point in physio array to match MRI one
    for ii in range(nslice):
        lab = 's{:03d}.{}'.format(ii, label)
        # calc list of time-array indices for all MRI volumes for this slice
        all_ind = find_vol_slice_times(tvals, samp_rate, slice_times[ii], ii,
                                       vol_nv, vol_tr, verb=verb)
        list_slice_sel_phys.append([lab, copy.deepcopy(all_ind)])

    # done, store
    phobj.list_slice_sel_phys = list_slice_sel_phys

    return 0

def calc_timing_selection_rvt(retobj, label=None, verb=0):
    """Calculate the 'timing selection array' for the phys_obj
(=retobj.data[label]) time series tvalues, **just starting at time 0,
for the RVT regressor**, based on the slice timing information.  That
is, we need to know where to sample the physio time series to match
with the MRI volume (just initial slice, which we assume is at t=0.0).
The 'timing array' records this information.  We basically step
through the finely-sampled physio data 'x-axis', and figure out which
values correspond to a given coarse-sampled MRI slice.

NB: we *could* have just gotten this info as a special case of the
full physio slicewise regressors calculation, but decided to keep this
separate, to be more general.

This function *also* calculates the list of shifts, which can be
affected by user input.

Parameters
----------
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
label : str
    (non-optional kwarg) label to determine which time series is
    processed, and in which sub-object information is stored.  Allowed
    labels are stored in the PO_all_label list.

Returns
-------
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    if verb : print("++ Start RVT arr timing calc for {} data".format(label))

    check_label_all(label)
    check_label_rvt(label)      # a practical RVT reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    slice_times = retobj.vol_slice_times            # coarse, MRI timing
    nslice      = len(slice_times)                  # num of MRI slices
    vol_nv      = retobj.vol_nv                     # num of MRI vols
    vol_tr      = retobj.vol_tr                     # coarse, MRI sampling (s)
    tvals       = retobj.data[label].tvalues        # fine, physio ts timing
    ntval       = len(tvals)                        # num of physio timepts
    samp_rate   = retobj.data[label].samp_rate      # fine, physio sampling (s)
    
    # calc list of time-array indices for all MRI volumes for this slice
    all_ind = find_vol_slice_times(tvals, samp_rate, 0.0, 0,
                                   vol_nv, vol_tr, verb=verb)

    # done, store
    phobj.list_slice_sel_rvt = all_ind

    return 0

def find_vol_slice_times(tvals, samp_rate, slice_time0, slice_idx,
                         vol_nv, vol_tr, verb=0):
    """Take in a list of finely sampled physio data, whose 'x-axis' of
time values is tvals at sampling rate samp_rate.  Start at time
slice_time0, and find the list of indices corresponding to MRI slices
acquired every TR=vol_tr; there are vol_nv of them.

Parameters
----------
tvals : np.ndarray 
    1D array of physical time values (units = sec)
samp_rate : float
    sampling rate of the physio time series (basically, delta_t in tvals)
slice_time0 : float
    the value of the slice time to start from
slice_idx : int
    the index corresponding to which element in the slice timing list
    we are processing, for reporting purposes
vol_nv : int
    number of MRI volumes to get times for
vol_tr : float
    the MRI volume's TR (=sampling rate), in units of sec

Returns
-------
all_ind : list
    list of integers, representing where each MRI volume was acquired for
    this particular slice; later, these indices are used to select out values
    from other data times series that have the same length as tvals

    """

    ntval = len(tvals)              # len of tvalue array
    EPS   = 1.1* samp_rate / 2.0    # slightly generous epsilon

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
              "   {}..{} s, samp_rate = {}s"
              "".format(slice_idx, len(all_ind), nslice, 
                        tvals[0], tvals[-1], samp_rate))
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
    if odir :    fname = odir.rstrip('/') + '/' + fname

    # make the title string
    title = 'Process {} data: {}'.format(label, lab_title)

    # ensure subscript across all
    if all_subscript and not('$' in title) :
        title+= '$_{\\rm ~}$'

    return fname, title
    
# --------------------------------------------------------------------------

def calc_time_series_peaks(retobj, label=None, verb=0):
    """Calculate the peaks for one of the phys_obj time series.  The time
series and processing is determined by the label given.  There is both
overlap and differentiation in the processing of various time series.

Parameters
----------
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
label : str
    (non-optional kwarg) label to determine which time series is
    processed, and in which sub-object information is stored.  Allowed
    labels are stored in the PO_all_label list.

Returns
-------
is_ok : int
    was processing OK (= 0) or not (= nonzero)

    """

    if verb : print("++ Start peak/trough calc for {} data".format(label))

    check_label_all(label)

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    # -------- start of peak+trough estimation and refinement steps --------

    peaks   = np.zeros(0, dtype=int)           # null
    troughs = np.zeros(0, dtype=int)           # null; might not be calc'ed

    # each block of peak/trough detection works in about the same way,
    # and we comment on the pieces just in the first part

    count    = 0                                     # proc/filter steps
    lab_title = 'Bandpass and SciPy peak-finding'    # used in fig img
    lab_short = 'bp_scipy_peaks'                     # used in fig filename
    if verb :   print('++', lab_title)
    # calculate peak and/or trough array (and here, output a bit more
    # info, too)
    peaks, idx_freq_mode, xfilt = \
        lpp.get_peaks_from_bandpass(phobj.ts_orig,
                                    phobj.samp_freq,
                                    phobj.min_bps,
                                    max_bps=phobj.max_bps,
                                    label=label,
                                    retobj=retobj,
                                    verb=verb)
    phobj.ts_orig_bp = copy.deepcopy(xfilt)          # save BPed ver of ts
    # check on peaks, don't want an empty array.
    if len(peaks) == 0 :
        print("** ERROR: no peaks after step {} '{}' for {} data"
              "".format(count, lab_title, label))
        return 1
    # create strings for fig image filename and title
    fname, title = make_str_ts_peak_trough(label, count, 
                                           lab_title, lab_short, 
                                           prefix=prefix, odir=odir)
    # save output figure, if desired
    if retobj.save_graph_level > 1 :
        lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                          #upper_env=xfilt,  # tmp+cheap check
                                          title=title, fname=fname,
                                          retobj=retobj,
                                          verb=verb)

    # --------------
    count+= 1
    lab_title = 'Local peak refinement'
    lab_short = 'local_refine_peaks'
    if verb :   print('++', lab_title)
    peaks = lpp.refinePeakLocations(peaks, 
                                    phobj.ts_orig,
                                    is_troughs = False,
                                    label=label,
                                    verb=verb)
    if len(peaks) == 0 :
        print("** ERROR: no peaks after step {} '{}' for {} data"
              "".format(count, lab_title, label))
        return 1
    fname, title = make_str_ts_peak_trough(label, count, 
                                           lab_title, lab_short, 
                                           prefix=prefix, odir=odir)
    if retobj.save_graph_level > 1 :
        lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                          title=title, fname=fname,
                                          retobj=retobj,
                                          verb=verb)

    # --------------
    count+= 1
    if label == 'card' :
        lab_title = 'Local peak percentile filter'
        lab_short = 'perc_local_peaks'
        if verb :   print('++', lab_title)
        peaks = lpp.percentileFilter_local(peaks, 
                                           phobj.ts_orig,
                                           is_troughs = False,
                                           label=label,
                                           verb=verb)
    elif label == 'resp' :
        lab_title = 'Global peak percentile filter'
        lab_short = 'perc_global_peaks'
        if verb :   print('++', lab_title)
        peaks = lpp.percentileFilter_global(peaks, 
                                            phobj.ts_orig,
                                            is_troughs = False,
                                            label=label,
                                            verb=verb)
    if len(peaks) == 0 :
        print("** ERROR: no peaks after step {} '{}' for {} data"
              "".format(count, lab_title, label))
        return 1
    fname, title = make_str_ts_peak_trough(label, count, 
                                           lab_title, lab_short, 
                                           prefix=prefix, odir=odir)
    if retobj.save_graph_level > 1 :
        lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                          title=title, fname=fname,
                                          retobj=retobj,
                                          verb=verb)

    # --------------
    count+= 1
    lab_title = 'Peak proximity filter'
    lab_short = 'proxim_filter_peaks'
    if verb :   print('++', lab_title)
    # !!! but just use idx_freq_mode from above???
    tsperiod_idx = \
        lpp.getTimeSeriesPeriod_as_indices(phobj.ts_orig)
    peaks = lpp.removeClosePeaks(peaks, 
                                 phobj.ts_orig,
                                 tsperiod_idx,
                                 is_troughs = False,
                                 label=label,
                                 verb=verb)
    if len(peaks) == 0 :
        print("** ERROR: no peaks after step {} '{}' for {} data"
              "".format(count, lab_title, label))
        return 1
    fname, title = make_str_ts_peak_trough(label, count, 
                                           lab_title, lab_short, 
                                           prefix=prefix, odir=odir)
    if retobj.save_graph_level > 1 :
        lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                          title=title, fname=fname,
                                          retobj=retobj,
                                          verb=verb)


    # !!! add in more refinement steps, and prob trough estimation, too



    ### !!!! for addMissingPeakes: only run this for card, because
    ### !!!! resp has peak+trough returns? ---> right now, do for BOTH
    ### !!!! card and resp, bc resp troughs are dealt with separately
    ### !!!! below
    # --------------
    count+= 1
    lab_title = 'Add any missing peaks'
    lab_short = 'add_missing_peaks'
    if verb :   print('++', lab_title)
    peaks = lpp.addMissingPeaks(peaks, 
                                phobj.ts_orig,
                                is_troughs = False,
                                verb=verb)
    if len(peaks) == 0 :
        print("** ERROR: no peaks after step {} '{}' for {} data"
              "".format(count, lab_title, label))
        return 1
    fname, title = make_str_ts_peak_trough(label, count, 
                                           lab_title, lab_short, 
                                           prefix=prefix, odir=odir)
    if retobj.save_graph_level > 1 :
        lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                          title=title, fname=fname,
                                          retobj=retobj,
                                          verb=verb)



    # -------- trough considerations: resp only
    if label == 'resp' :
        # at present, ONLY troughs change in this IF condition branch

        # --------------
        count+= 1
        lab_title = 'Bandpass and SciPy trough-finding'    # used in fig img
        lab_short = 'bp_scipy_troughs'                          
        if verb :   print('++', lab_title)
        troughs, _ = sps.find_peaks(-xfilt,
                                    width=int(phobj.samp_freq/8))
        if len(troughs) == 0 :
            print("** ERROR: no troughs after step {} '{}' for {} data"
                  "".format(count, lab_title, label))
            return 1
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)

        # --------------
        count+= 1
        lab_title = 'Global trough percentile filter'
        lab_short = 'perc_global_troughs'
        if verb :   print('++', lab_title)
        troughs = lpp.percentileFilter_global(troughs, 
                                              phobj.ts_orig,
                                              perc_filt = 90.0,
                                              is_troughs = True, 
                                              label=label,
                                              verb=verb)
        if len(troughs) == 0 :
            print("** ERROR: no troughs after step {} '{}' for {} data"
                  "".format(count, lab_title, label))
            return 1
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks, 
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)

        # !!! can leave out here separate trough merger checks for
        # !!! left and right individually, for the global case?  Right
        # !!! now, leaving out those separate checks
        

        # --------------
        count+= 1
        lab_title = 'Trough proximity filter'
        lab_short = 'proxim_filter_troughs'
        if verb :   print('++', lab_title)
        # !!! but just use idx_freq_mode from above???
        tsperiod_idx = \
            lpp.getTimeSeriesPeriod_as_indices(phobj.ts_orig)
        troughs = lpp.removeClosePeaks(troughs, 
                                       phobj.ts_orig,
                                       tsperiod_idx,
                                       is_troughs = True,
                                       label=label,
                                       verb=verb)
        if len(troughs) == 0 :
            print("** ERROR: no troughs after step {} '{}' for {} data"
                  "".format(count, lab_title, label))
            return 1
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)


        # --------------
        count+= 1
        lab_title = 'Add any missing troughs'
        lab_short = 'add_missing_troughs'
        if verb :   print('++', lab_title)
        troughs = lpp.addMissingPeaks(troughs, 
                                      phobj.ts_orig,
                                      is_troughs = True,
                                      verb=verb)
        if len(troughs) == 0 :
            print("** ERROR: no troughs after step {} '{}' for {} data"
                  "".format(count, lab_title, label))
            return 1
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                              troughs=troughs,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)


    # ----- DONE with peak+trough estimation+refinement: add to obj -----
    phobj.peaks = peaks
    p_ival = np.median([j-i for i, j in zip(peaks[:-1], peaks[1:])])
    p_ival*= phobj.samp_rate
    if len(troughs) :
        phobj.troughs = troughs

    # -------------- FINAL plot
    count+=1
    if len(troughs) :
        lab_title = 'Final peaks ($\Delta t_{{\\rm med}}$ = {:0.3f} s)'.format(p_ival)
        lab_title+= ' and troughs'
        lab_short = 'final_peaks_troughs'
        if verb :   print('++', lab_title)
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if 1 : #retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                              troughs=troughs,
                                              add_ibandT=True,
                                              add_ibandB=True,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)
    else:
        lab_title = 'Final peaks ($\Delta t_{{\\rm med}}$ = {:0.3f} s)'.format(p_ival)
        lab_short = 'final_peaks'
        if verb :   print('++', lab_title)
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if 1 : #retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=peaks,
                                              add_ibandT=True,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)

    return 0

# ===========================================================================
# ===========================================================================

def calc_time_series_phases(retobj, label=None, verb=0):
    """Calculate the phases of the peaks (possibly also using troughs) for
one of the phys_obj time series.  The time series and processing is
determined by the label given. 

Parameters
----------
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
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
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    count     = 20                          # start with num >> peak/trough est
    lab_title = 'Phase estimation'
    lab_short = 'est_phase'
    if verb :   print('++', lab_title)

    # ------- card phase estimation
    if label == 'card' :
        # use Method 1 phase finding for card peaks/time series
        phases = lpph.calc_phases_M1(phobj, verb=verb)
        if not(len(phases)) :
            print("** ERROR: problem after step {} '{}' for {} data"
                  "".format(count, lab_title, label))
            return 1
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=phobj.peaks,
                                              phases=phases,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)

    elif label == 'resp' :
        # use Method 1 phase finding for card peaks/time series
        #phases = lpph.calc_phases_M2(phobj, verb=verb)  # older method
        phases = lpph.calc_phases_M3(phobj, verb=verb)
        if not(len(phases)) :
            print("** ERROR: problem after step {} '{}' for {} data"
                  "".format(count, lab_title, label))
            return 1
        fname, title = make_str_ts_peak_trough(label, count, 
                                               lab_title, lab_short, 
                                               prefix=prefix, odir=odir)
        if retobj.save_graph_level > 1 :
            lpplt.makefig_phobj_peaks_troughs(phobj, peaks=phobj.peaks,
                                              troughs=phobj.troughs,
                                              phases=phases,
                                              title=title, fname=fname,
                                              retobj=retobj,
                                              verb=verb)


    # ----- DONE with phase estimation: add to obj -----
    phobj.phases = phases

    return 0

# ===========================================================================

def calc_time_series_rvt(retobj, label=None, verb=0):
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
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    # ------ time series envelope
    count     = 21 
    lab_title = 'RVT envelope estimation'
    lab_short = 'rvt_env'
    if verb :   print('++', lab_title)

    # calculate the upper and lower envelope
    upper_env = lprvt.interp_extrema_LIN(phobj, phobj.peaks, verb=verb)
    lower_env = lprvt.interp_extrema_LIN(phobj, phobj.troughs, verb=verb)    
    fname, title = make_str_ts_peak_trough(label, count, 
                                           lab_title, lab_short, 
                                           prefix=prefix, odir=odir)
    if retobj.save_graph_level > 1 :
        lpplt.makefig_phobj_peaks_troughs(phobj, peaks=phobj.peaks,
                                          troughs=phobj.troughs,
                                          upper_env=upper_env,
                                          lower_env=lower_env,
                                          title=title, fname=fname,
                                          retobj=retobj,
                                          verb=verb)


    # actual RVT
    count    += 1
    lab_title = 'RVT measure'
    lab_short = 'rvt_measure'
    if verb :   print('++', lab_title)
    insta_per = lprvt.interp_intervals_LIN(phobj, phobj.peaks, verb=verb)
    rvt_ts    = (upper_env - lower_env) / insta_per

    # !!! plot RVT vals

    phobj.rvt_ts = rvt_ts

    return 0

# --------------------------------------------------------------------------


def calc_regress_phys(retobj, label=None, verb=0):
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
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    regress_dict_phys = {}

    for mm in range(1, phobj.M+1):
        # make cos() regressors, and initialize list of lists
        lab = 'c{}'.format(mm)
        regress_dict_phys[lab] = []
        for nn in range(phobj.n_slice_sel_phys):
            reg = np.cos(mm*phobj.phases[phobj.list_slice_sel_phys[nn][1]])
            regress_dict_phys[lab].append([phobj.list_slice_sel_phys[nn][0],
                                           copy.deepcopy(reg)])

        # make sin() regressors, and initialize list of lists
        lab = 's{}'.format(mm)
        regress_dict_phys[lab] = []
        for nn in range(phobj.n_slice_sel_phys):
            reg = np.sin(mm*phobj.phases[phobj.list_slice_sel_phys[nn][1]])
            regress_dict_phys[lab].append([phobj.list_slice_sel_phys[nn][0],
                                           copy.deepcopy(reg)])


    phobj.regress_dict_phys = regress_dict_phys

    return 0


def calc_regress_rvt(retobj, label=None, verb=0):
    """Calculate RVT regressors, as described in Birn et al.,
2006.

    """

    if verb : print("++ Start RVT regressor calc for {} data".format(label))

    check_label_all(label)
    check_label_rvt(label)      # a practical RVT reality, at present

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix

    regress_dict_rvt = {}

    is_fail, all_shifts = lpo.interpret_rvt_shift_opts(retobj.rvt_shifts[0], 
                                                       retobj.rvt_shifts[1], 
                                                       retobj.rvt_shifts[2])
    nshift = len(all_shifts)
    if verb :
        print("++ The {} RVT shift values are: {}".format(nshift, all_shifts))

    # shifts here are made by shifting a copy of the underlying
    # tvalues array, and then selecting the same MRI-snapshot points.
    # We use the time series median to pad values



    # the primary, unshifted regressor
    rvt_regr = phobj.rvt_ts[phobj.list_slice_sel_rvt]

    for ii in range(nshift):
        # make shifted regressors
        lab = 'rvt{:02d}'.format(ii)
        shift = all_shifts[ii]
        regress_dict_rvt[lab] = get_shifted_rvt(phobj.rvt_ts,
                                                phobj.samp_freq,
                                                phobj.list_slice_sel_rvt,
                                                shift)

    phobj.regress_dict_rvt = regress_dict_rvt

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
