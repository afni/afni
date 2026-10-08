#!/usr/bin/env python

# basic functions for helping find/determine/highlight outliers in time
# series properties
# ==========================================================================

import numpy as np

from afnipy import afni_base as ab

# ==========================================================================

# All possible metrics for Mahalanobis scoring of intervals
LIST_mahala_metric = ['mad']

# All possible coordinate dimensions for Mahalanobis scoring of intervals
LIST_mahala_coord = ['del_x_same', 'del_y_same']

# ==========================================================================

def calc_MAD(x, mid='median', scale_fac=1.4826):
    """Calculate the median absolute deviation of a 1D collection from a
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
    Median of the absolute differences between the values in x and
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

    mad  = float(np.median(np.abs(values - mid)))

    if scale_fac :
        mad*= scale_fac

    return 0, mad

# --------------

def calc_mahala(ax, ay=None, all_coord=['del_x_same'], metric='mad',
                mid='median'):
    """Calculate a MAD-scaled distance for successive extrema coordinates.

ax is a 1D array of x positions; the optional ay is a 1D array of y
positions.

Each selected difference series is centered on its midpoint (defined
by the kwarg mid), divided by its median absolute deviation, and
squared.  The distance is the square root of their pointwise sum.
It does not include covariance terms between coordinate dimensions.

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
    Float array of length N-1.  Each element is the square root of the
    sum of squared, MAD-scaled differences for the selected keywords.

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

    if metric not in LIST_mahala_metric :
        ab.EP1('metric must be selected from LIST_mahala_metric')
        return BAD_RETURN

    if not isinstance(all_coord, (list, tuple)):
        ab.EP1('all_coord must be a list of strings')
        return BAD_RETURN

    if any(coord not in LIST_mahala_coord for coord in all_coord):
        ab.EP1('all_coord must be selected from LIST_mahala_coord')
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

        # ... and get center and normalization (denom)
        if mid == 'median' :
            center = np.median(all_diff)
        elif mid == 'mean' :
            center = np.mean(all_diff)
        is_fail, scale = calc_MAD(all_diff, mid=mid, scale_fac=1.4826)
        if is_fail :
            ab.EP1('could not calculate MAD for ' + key)
            return BAD_RETURN
        deviation = all_diff - center
        if scale != 0 :
            m[ii] = (deviation / scale)**2
        else:
            # With otherwise identical intervals, any nonzero deviation
            # is an outlier even though the robust scale is zero.
            m[ii, deviation != 0] = np.inf

    mahala = np.sqrt(np.sum(m, axis=0))

    return 0, mahala


def find_outliers(ax, ay=None, all_coord=None, threshold=3.0):
    """Find intervals whose MAD-scaled distance exceeds a threshold.

    This first test uses successive x-coordinate differences by default.
    Its returned intervals span the two extrema at either end of each
    flagged interval.  Coordinates are sorted into x-axis order; when
    y-values are supplied, they are reordered with their x-values.

Parameters
----------
ax : array-like
    1D x-coordinates of the extrema (times in seconds in physio plots).
ay : array-like or None, optional
    Corresponding y-coordinates, required for 'del_y_same'.
all_coord : list of str or None, optional
    Coordinate differences to test.  By default only 'del_x_same' is
    used.  Other accepted values are listed in LIST_mahala_coord.
threshold : float, optional
    Intervals with a calc_mahala() output strictly greater than this
    value are reported.

Returns
-------
is_fail : int
    0 for success, nonzero for failure.
intervals : np.ndarray
    Float array of shape (M, 2), holding the start and end x-coordinate
    of each of the M flagged intervals.  An empty array has shape (0, 2).

    """

    BAD_RETURN = (-1, np.empty((0, 2), dtype=float))

    try:
        ax = np.asarray(list(ax), dtype=float)
        ay = None if ay is None else np.asarray(list(ay), dtype=float)
    except (TypeError, ValueError):
        ab.EP1('ax and ay must be numeric 1D collections')
        return BAD_RETURN

    if ax.ndim != 1 or (ay is not None and
                        (ay.ndim != 1 or len(ay) != len(ax))):
        ab.EP1('ax and ay must be 1D collections of equal length')
        return BAD_RETURN
    if not np.isfinite(threshold):
        ab.EP1('threshold must be finite')
        return BAD_RETURN
    if len(ax) < 2:
        return 0, np.empty((0, 2), dtype=float)

    order = np.argsort(ax)
    ax = ax[order]
    if ay is not None:
        ay = ay[order]

    if all_coord is None:
        all_coord = ['del_x_same']
    is_fail, distances = calc_mahala(ax, ay=ay, all_coord=all_coord)
    if is_fail:
        return BAD_RETURN

    flagged = distances > threshold
    intervals = np.column_stack((ax[:-1][flagged], ax[1:][flagged]))

    return 0, intervals


def find_nonalt_extrema(ax, bx):
    """Find extrema in ax that have no bx extremum between neighbors.

For respiratory peaks, pass peak times as ax and trough times as bx;
reverse the inputs to check troughs.  Both endpoints of every pair
with no intervening opposite extremum are returned, so a run of
three peaks (or troughs) flags all three.  Extrema at exactly the
same time do not count as alternating.  The calculation itself is
independent of signal type and can also be used for cardiac data.

Parameters
----------
ax : array-like
    1D numeric x-coordinates of the extrema being checked.
bx : array-like
    1D numeric x-coordinates of the opposite extrema.

Returns
-------
is_fail : int
    0 for success, nonzero for failure.
indices : np.ndarray
    Sorted integer indices into the original ax array of extrema in
    nonalternating runs.  Empty if ax has fewer than two points or
    alternates with bx throughout.

    """

    BAD_RETURN = (-1, np.array([], dtype=int))
    try:
        ax = np.asarray(list(ax), dtype=float)
        bx = np.asarray(list(bx), dtype=float)
    except (TypeError, ValueError):
        ab.EP1('ax and bx must be numeric 1D collections')
        return BAD_RETURN
    if ax.ndim != 1 or bx.ndim != 1 or not np.all(np.isfinite(ax)) \
            or not np.all(np.isfinite(bx)):
        ab.EP1('ax and bx must contain finite 1D coordinates')
        return BAD_RETURN
    if len(ax) < 2:
        return 0, np.array([], dtype=int)

    order = np.argsort(ax, kind='stable')
    sorted_ax = ax[order]
    sorted_bx = np.sort(bx)
    # A bx value separates adjacent ax values only when strictly between.
    between = (np.searchsorted(sorted_bx, sorted_ax[1:], side='left') -
               np.searchsorted(sorted_bx, sorted_ax[:-1], side='right'))
    same_run = between <= 0
    flagged = np.zeros(len(ax), dtype=bool)
    flagged[:-1] |= same_run
    flagged[1:] |= same_run
    return 0, np.sort(order[flagged])
