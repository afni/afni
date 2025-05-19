#!/usr/bin/env python

import copy, sys
import numpy             as np
import matplotlib.pyplot as plt

from afnipy import afni_base as ab

# ==========================================================================

# time interval ranges for all kernels defined here
all_kernel_trange = {
    'rrf_birn08'  : [0.0, 50.0],
    'crf_chang09' : [0.0, 25.0],
    }

# ==========================================================================

def rrf_birn_etal(t):
    """The respiratory response function (RRF) from Birn et al. (2008).
This is an empirical fit found from deconvolving some measured data.
Check out the paper for details.  The input parameter is either a
single time point or an array of time values, and this function will
output the corresponding functional RRF value(s).  The approximate
time scale for this function is 40-50 s.

Parameters
----------
t : float or array of floats
    time value(s)

Returns
-------
y : float or array of floats
    value(s) of RRF, which Rasmus assures us are correct

    """

    # any negative t has a 0 value output
    if isinstance(t, float) :
        if t <= 0 :
            return 0.0
    elif isinstance(t, (np.ndarray, list)) :
        N = len(t)
        u = np.zeros(N, dtype=float)
        for i in range(N):
            if t[i]>0.0 :
                u[i] = t[i]
        t = copy.deepcopy(u)
    else:
        print("+* WARN: unexpected dtype for t:", dtype(t))
        return -1

    aa = 0.6 * (t**2.1) * np.exp(-t / 1.6)
    bb = 0.0023 * (t**3.54) * np.exp(-t/4.25)
    y  = aa - bb

    return y


def crf_chang_etal(t):
    """The cardiac response function (RRF) from Chang et al. (2009).
This is an empirical fit found from deconvolving some measured data.
Check out the paper for details.  The input parameter is either a
single time point or an array of time values, and this function will
output the corresponding functional CRF value(s).  The approximate
time scale for this function is 30 s (hence, the output trange interval).

NB: To better match with Fig. 6D of Chang et al. (2009), this function
is scaled vertically by a factor of 0.5.  Eq. 5 of Chang et al. (2009)
appears to need this to have a plus/minus range of 1, in line with
that figure (and with other similar response functions).

Parameters
----------
t : float or array of floats
    time value(s)

Returns
-------
y : float or array of floats
    value(s) of CRF, which Rasmus assures us are correct

    """

    # any negative t has a 0 value output
    if isinstance(t, float) :
        if t <= 0 :
            return 0.0
    elif isinstance(t, (np.ndarray, list)) :
        N = len(t)
        u = np.zeros(N, dtype=float)
        for i in range(N):
            if t[i]>0.0 :
                u[i] = t[i]
        t = copy.deepcopy(u)
    else:
        print("+* WARN: unexpected dtype for t:", dtype(t))
        return -1

    # parameter
    sig = 3.0
    
    aa = 0.6 * (t**2.7) * np.exp(-t / 1.6)
    bb = (16.0 / np.sqrt(2*np.pi*sig**2)) * np.exp(-0.5*((t-12.0)/sig)**2)
    y  = aa - bb

    # this is not in the Chang et al. (2009) Eq. 5, but it appears
    # necessary for vertical scaling to be correct and match with
    # Fig. 6D there.
    y/= 2.0

    return y

# ----------------------------------------------------------------------------

def simple_kernel_image(oimg, kernel='rrf_birn08'):
    """ Simple plotting of a response shape kernel, where fname is the
output file name.  The kernel is any of the following string
labels, corresponding to an available response function of interest (and
this list might grow over time), which can be seen with:
    all_kernel_trange.keys()
    
"""

    if kernel not in all_kernel_trange.keys() :
        print("** ERROR: kernel '{}' not in list:".format(kernel))
        print("   {}".format(', '.join(all_allowed_kernel)))
        sys.exit(1)

    if kernel == 'rrf_birn08' :
        title = 'RRF (Birn et al., 2008)'
        trange = all_kernel_trange[kernel]
        N = int((trange[1] - trange[0]) * 5 + 1)
        t = np.linspace(trange[0], trange[1], N)
        y = rrf_birn_etal(t)
        ylab = "$\Delta$ BOLD (%)"
    elif kernel == 'crf_chang09' :
        title = 'CRF (Chang et al., 2009)'
        trange = all_kernel_trange[kernel]
        N = int((trange[1] - trange[0]) * 5 + 1)
        t = np.linspace(trange[0], trange[1], N)
        y = crf_chang_etal(t)
        ylab = "amplitude (no units)" #"$\Delta$ BOLD (%)"

    osize = (5, 3)
    dpi   = 300

    fff = plt.figure( oimg, figsize=osize )

    plt.axhline(y=0, color='0.5')
    plt.plot(t, y, color='tab:red')
    plt.title(title)
    plt.xlabel("t (s)")
    plt.ylabel(ylab)

    plt.xlim([t[0], t[-1]])
    plt.ylim([-1.1, 1.1])

    plt.tight_layout()

    plt.savefig(oimg, dpi=300)

    return 0

def convolve_with_kernel(x, delt=None, kernel=None):
    """Convolve the array x with the response specified by the kernel
label. See all_kernel_trange.keys() for the list of available kernels.
We also have to know delt, the time step in x.  

In Birn et al. (2008), the RRF was convolved with the RVT time series
that had already been downsampled to the FMRI TR, and we expect users
to follow suit here.

During the calculation, this function will pad x at both ends by the
time scale it takes for the response to go to 0.  For example, for
Birn et al. (2008)'s RRF, the duration is about 50 s.  This done by
simply connecting x's mean linearly to the first or last time point.

Parameters
----------
x : array (of floats)
    values to convolve with RRF
delt : float
    delta time, the sampling rate (in s), likely the TR of FMRI data;
    this value must be >0
kernel : str
    label of potential response functions, which are listed in
    all_kernel_trange.keys()

Returns
-------
z : array (of floats)
    result of convolving x with RRF; has the same length as x

    """

    if delt is None :
        AB.EP("Need to provide a delt value")
    elif delt <= 0 :
        AB.EP("Need to provide a delt value >0, not:", delt)

    if kernel not in all_kernel_trange.keys() :
        print("** ERROR: kernel '{}' not in list:".format(kernel))
        print("   {}".format(', '.join(all_allowed_kernel)))
        sys.exit(1)

    nx = len(x)
    meanx = np.mean(x)

    # num time points to pad input, based on RRF time scale (plus t=0)
    trange  = all_kernel_trange[kernel]
    tscale  = trange[1] - trange[0]
    n_extra = int(np.ceil( tscale / delt )) + 1

    # make intermediate time series (mean zeropadding here); bc RRF is
    # nonzero for only t>0, we just pad on one side for the convolution
    ny = nx + n_extra
    y  = np.ones(ny, dtype=float) * meanx
    y[n_extra:] = x

    # make response vector h, for convolving with x
    t = np.arange(0.0, n_extra*delt, delt)
    if kernel == 'rrf_birn08' :
        h = rrf_birn_etal(t)
    elif kernel == 'crf_chang09' :
        h = crf_chang_etal(t)

    # do convolution (again, kernel is positive definite, hence kk range)
    z = np.zeros(ny, dtype=float)
    for nn in range(n_extra, ny):
        for kk in range(n_extra):
            z[nn]+= h[kk] * y[nn-kk]

    # return the part corresponding to original time series length
    return z[n_extra:]
