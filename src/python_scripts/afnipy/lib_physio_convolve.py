#!/usr/bin/env python

import copy
import numpy             as np
import matplotlib.pyplot as plt

from afnipy import afni_base as ab


def rrf_birn_etal(t):
    """The respiratory response function (RRF) from Birn et al. (2008).
This is an empirical fit found from deconvolving some measured data.
Check out the paper for details.  The input parameter is either a
single time point or an array of time values, and this function will
output the corresponding functional RRF value(s).  The approximate
time scale for this function is 40-50 s.

If kwarg save

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

def rrf_simple_image(oimg):
    """
    Simple plotting of RRF.  fname is the output file name. 
"""

    t = np.linspace(0, 50, 201)
    rrf = rrf_birn_etal(t)

    osize = (5, 3)
    dpi   = 300

    fff = plt.figure( oimg, figsize=osize )

    plt.axhline(y=0, color='0.5')
    plt.plot(t, rrf, color='tab:red')
    plt.title("RRF")
    plt.xlabel("t (s)")
    plt.ylabel("$\Delta$ BOLD (%)")

    plt.xlim([t[0], t[-1]])
    plt.ylim([-1.1, 1.1])

    plt.tight_layout()

    plt.savefig(oimg, dpi=300)

    return 0

def convolve_with_rrf(x, delt=None):
    """Convolve the array x with the RRF function rrf_birn_etal(...).  We
have to know delt, the time step in x.  In Birn et al. (2008), the RRF
was convolved with the RVT time series that had already been
downsampled to the FMRI TR, and we expect users to follow suit here.

During the calculation, this function will pad x at both ends by the
time scale it takes for RRF to return to zero, which is about 50 s.
This done by simply connecting x's mean linearly to the first or last
time point.

Parameters
----------
x : array (of floats)
    values to convolve with RRF
delt : float
    delta time, the sampling rate (in s), likely the TR of FMRI data;
    this value must be >0

Returns
-------
z : array (of floats)
    result of convolving x with RRF; has the same length as x

    """

    if delt is None :
        AB.EP("Need to provide a delt value")
    elif delt <= 0 :
        AB.EP("Need to provide a delt value >0, not:", delt)

    nx = len(x)
    meanx = np.mean(x)

    # num time points to pad input, based on RRF time scale (plus t=0)
    tscale_rrf = 50.0
    n_extra = int(np.ceil( tscale_rrf / delt )) + 1

    # make intermediate time series (mean zeropadding here); bc RRF is
    # nonzero for only t>0, we just pad on one side for the convolution
    ny = nx + n_extra
    y  = np.ones(ny, dtype=float) * meanx
    y[n_extra:] = x

    # make rrf vector, for convolving with x
    t_rrf = np.arange(0.0, n_extra*delt, delt)
    h_rrf = rrf_birn_etal(t_rrf)

    # do convolution (again, kernel is positive definite, hence kk range)
    z = np.zeros(ny, dtype=float)
    for nn in range(n_extra, ny):
        for kk in range(n_extra):
            z[nn]+= h_rrf[kk] * y[nn-kk]

    # return the part corresponding to original time series length
    return z[n_extra:]
