#!/usr/bin/env python

# basic functions for helping find/determine/highlight outliers in time
# series properties
# ==========================================================================

import numpy as np

from afnipy import afni_base as ab

# ==========================================================================

# All possible metrics for Mahalanobis scoring of intervals
LIST_malaha_matric = ['mad']

# All possible coordinate dimensions for Mahalanobis scoring of intervals
LIST_malaha_coord = ['del_x_same', 'del_y_same']

# ==========================================================================

def calc_MAD(x, mid='median', scale_fac=1.4826):
    """Calculate the mean absolute deviation of a 1D collection from a
chosen midpoint. The mid kwarg is the rule for calculating the
midpoint.  Currently accepted values are: 'median' (def, for
robustness to skew), 'mean'.

The scale_fac is present to make the returned MAD value more
comparable to a standard deviation.  See: 
https://en.wikipedia.org/wiki/Median_absolute_deviation#Relation_to_standard_deviation

Parameters
----------
x : array-like
    Nonempty 1D collection of numeric values.
mid : str
    Keyword to choose the way the midpoint is calculated
scale_fac : float
    Multiplicative factor for scaling the returned MAD value

Returns
-------
is_fail : int
    0 for success, nonzero for failure
mad : float
    Mean of the absolute differences between the values in x and
    the chosen middle value (mid).  Has the same units as x.

    """

    BAD_RETURN = (-1, 0.0)

    values = np.asarray(
        x if isinstance(x, (np.ndarray, list, tuple)) else list(x),
        dtype=float)
    if values.ndim != 1 or values.size == 0:
        ab.EP1('x must be a nonempty 1D collection')
        return BAD_RETURN

    if mid == 'mean' :
        mid  = np.mean(values)
    elif mid == 'median' :
        mid = np.median(values)
    else:
        ab.EP1("value for mid must be one of: 'mean', 'median'")
        return BAD_RETURN

    mad  = float(np.mean(np.abs(values - mid)))

    if scale_fac :
        mad*= scale_fac

    return 0, mad

# --------------

def calc_mahala(ax, ay=None, all_coord=['del_x_same'], metric='mad',
                mid='median'):
    """Combine squared, MAD-scaled successive differences of time
series ax and/or ay.

ax is a 1D array of x positions; the optional ay is a 1D array of y
positions.

Each selected difference series is divided by its mean absolute
deviation about its midpoint (defined by the kwarg mid), then squared.
The result sums these squared series pointwise.  This score does not
include covariance terms or subtract the median from the differences
before scaling.

Parameters
----------
ax : array-like
    1D collection of N numeric x-axis positions, in sample order.
ay : array-like or None, optional
    Corresponding 1D y-values.  If supplied, it must have N values.
    Required when 'del_y_same' is in all_coord.
all_coord : list of str
    list of keywords to choose the coordinates for the distance metric.
    Allowed keywords are:
      'del_x_same' : uses successive differences in ax
      'del_y_same' : uses successive differences in ay.
    Each keyword can occur once.

Returns
-------
is_fail : int
    0 for success, nonzero for failure
mahala : np.ndarray
    Float array of length N-1.  Each element is the sum of the
    squared, MAD-scaled differences for the selected keywords.

    """

    BAD_RETURN = (-1, np.array([]))

    # ----- check input array(s)

    # just ensure that ax is an array of floats
    ax = np.asarray(list(ax), dtype=float)

    if ax.ndim != 1 or ax.size < 2:
        ab.EP1('ax must be a 1D collection with at least 2 values')
        return BAD_RETURN

    if ay is not None:
        ay = np.asarray(ay if isinstance(ay, (np.ndarray, list, tuple))
                          else list(ay),
                        dtype=float)
        if ay.ndim != 1 or len(ay) != len(ax):
            ab.EP1('ax and ay must be 1D collections of equal length')
            return BAD_RETURN

    # check input kwargs

    if not isinstance(all_coord, (list, tuple)):
        ab.EP1('all_coord must be a list of strings')
        return BAD_RETURN

    if any(coord not in LIST_malaha_coord for coord in all_coord):
        ab.EP1('all_coord must be selected from LIST_malaha_coord')
        return BAD_RETURN

    if len(set(all_coord)) != len(all_coord) :
        ab.EP1('all_coord must not contain duplicates')
        return BAD_RETURN

    if 'del_y_same' in all_coord and ay is None:
        ab.EP1("ay is required for 'del_y_same'")
        return BAD_RETURN

    # ----- do calcs

    N = len(ax)
    K = len(all_coord)
    m = np.zeros((K, N-1), dtype=float)

    for ii, key in enumerate(all_coord):
        # calc diffs...
        if key == 'del_x_same' :
            all_diff = np.diff(ax)
        elif key == 'del_y_same' :
            all_diff = np.diff(ay)

        # ... and get normalization (denom)
        is_fail, scale = calc_MAD(all_diff, mid=mid, scale_fac=1.4826)
        if is_fail :
            ab.EP1('could not calculate MAD for ' + key)
            return BAD_RETURN
        if scale != 0 :
            m[ii] = (all_diff / scale)**2

    mahala = np.sum(m, axis=0)

    return 0, mahala
