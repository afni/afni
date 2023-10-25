import sys
import numpy as np
import matplotlib.pyplot as plt


def find_med_filt_grid_start_stop(N, winwid, step, verb=0):
    """From knowing the length N of the input time series, as well as
window size winwid and downsampling step size step, calculate the array
of subsampling index values to select out 'central point values' from
the original time series; this is saved as all_n.

Also calculate the first and last indices within all_n where simple
median filtering at the specified window size can occur; these are the
start and stop values, respectively.  Successively smaller filter
windows will be applied to points outside the [start, stop] range.

Parameters
----------
N : int
    number of time points in original time series
winwid : int
    window size for median calculation (units: number of time points)
step : int
    downsampling step size; every step-th point from original time 
    series will be center of median window

Returns
-------
all_n : array (of ints)
    array storing index values from original time series that will
    be center of each filter window; M = len(all_n), and will be
    length of new filtered+downsampled array
start : int
    index of all_n with first simple filter calculation (that is, 
    full window is simply centered around it)
stop : int
    index of all_n with last simple filter calculation (that is, 
    full window is simply centered around it)
    """

    all_n = np.arange(N)[::step]  # all downsamp indices
    M     = len(all_n)            # num of indices in downsampled ts

    hwin  = int(winwid // 2)        # half window

    # ----- start idx for simple med filt

    i = 0
    while i < M :
        if all_n[i] >= hwin :
            break
        i+= 1
    else:
        print("** ERROR: never found starting point for median filter")
        print("   N = {}, M = {}, step = {}, winwid = {}".format(N,M,step,
                                                                 winwid))
        sys.exit(8)
    start = i

    # ---- end idx for simple med filt

    i = M-1
    while i >= 0 :
        if N - 1 - all_n[i] > hwin :
            break
        i-= 1
    else:
        print("** ERROR: never found ending point for median filter")
        print("   N = {}, M = {}, step = {}, winwid = {}".format(N,M,step,
                                                                 winwid))
        sys.exit(8)
    stop = i

    if verb :
        print("++ Med filter simply filter index ranges:")
        print("   in downsamp: start = {}, stop = {}".format(start, stop))
        print("   in original: start = {}, stop = {}".format(all_n[start], 
                                                             all_n[stop]))

    return all_n, start, stop

def med_filt_and_downsamp(x, winwid=5, step=1, verb=0):
    """Process input 1D array (=time series) x with downsampling and/or
median filtering.  The window size of the median filter is set by
winwid, which represents a number of time points for the window. The
downsampling is determined by step, where every step-th time point in
the original time series (starting at 0) becomes the center of a
filtering window.  If winwid = 1, there is no median filtering, just
downsampling.

Parameters
----------
x : np.ndarray
    input 1D array or time series of real values
winwid : int
    window size for median calculation (units: number of time points)
step : int
    downsampling step size; every step-th point from original time 
    series will be center of median window

Returns
-------
y : np.ndarray
    output 1D array or time series of real values, after median filtering
    and/or downsampling

    """

    N      = len(x)
    winwid = int(winwid)
    step   = int(step)

    # trivial case
    if winwid == 1 and step == 1 :
        return x

    # cases to not process; could just error exit, but these also
    # should not happen
    if winwid <= 0 or step <= 0 or winwid >= N//2 or step >= N//2 :
        print("** ERROR: can't do med filt with these window or step params:")
        print("   N = {}, step = {}, winwid = {}".format(N, step, winwid))
        sys.exit(8)

    if verb :
        print("++ Prefilter info: winwid = {}, step = {}".format(winwid, step))

    all_n, start, stop = find_med_filt_grid_start_stop(N, winwid, step,
                                                       verb=verb)
    M = len(all_n)                # output ts size
    y = np.zeros(M, dtype=float)  # init output

    hwin = int(winwid // 2)         # half window

    # ----- simple median range work
    for i in range(start, stop+1):
        n = all_n[i]
        y[i] = np.median(x[n-hwin:n+hwin+1])

    # ----- use shrinking window for left edge
    for i in range(start):
        n = all_n[i]
        mini_hwin = n
        # use shrinking window, if n==0, use 1 neighbor to right
        if mini_hwin :    y[i] = np.median(x[n-mini_hwin:n+mini_hwin+1])
        else:             y[i] = np.median(x[n:n+2])

#    # ----- use shrinking window for right edge
    for i in range(stop+1, M):
        n = all_n[i]
        mini_hwin = N-n-1
        # use shrinking window; if n==0, use 1 neighbor to left (and to M)
        if mini_hwin :    y[i] = np.median(x[n-mini_hwin:n+mini_hwin+1])
        else:             y[i] = np.median(x[n-1:M])


    
        
    return y

# ==========================================================================

if __name__ == "__main__" :

    # ----- a simple example of median filtering with the functions here

    N  = 10001
    wt = 0.4

    t = np.linspace(0, 10, N)
    w = np.cos(2*np.pi*t) + np.cos(3*2*np.pi*t)
    x = w + wt*np.random.randn(N)

    step = 1
    ty = t[::step]
    y  = med_filt_and_downsamp(x, winwid=21, step=step)



    plt.plot(t,x, color='blue', alpha=0.25, label='x')
    plt.plot(t,w, color='red', lw=2, label='w')
    plt.plot(ty,y,color='black', lw=2, ls='--', label='y')
    plt.ion()
    plt.legend()
    plt.show()

    # ----- end of example

    sys.exit(0)
