__author__ = "Joshua Zosky"

"""
    Copyright 2015 Joshua Zosky
    joshua.e.zosky@gmail.com

    This file is part of "RetroTS".
    "RetroTS" is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    "RetroTS" is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with "RetroTS".  If not, see <http://www.gnu.org/licenses/>.
"""

import numpy

# from scipy import *
# from scipy.fftpack import fft
from numpy import real
from numpy.fft import fft, ifft
from numpy import zeros, ones, nonzero

# rcr: omit new style sub-library of pylab
from pylab import plot, subplot, show, text, figure
from scipy.signal import lfilter, firwin
from scipy.interpolate import interp1d
import glob

"""
function [r, e] = PeakFinder(var_vector, Opt)
In python, the above command looks like:
import PeakFinder as PF
(r, e) = PF.peak_finder(var_vector, **opt)
where opt is a dictionary with the parameters for the function
"""
# numpy.set_printoptions(threshold='nan')

# for checking whether quotients are sufficiently close to integral
g_epsilon = 0.00001


def fftsegs(ww, po, nv):
    """
    Returns the segements that are to be used for fft calculations.
    Example: (bli, ble, num) = fftsegs (100, 70, 1000);
    :param ww: Segment width (in number of samples)
    :param po: Percent segment overlap
    :param nv: Total number of samples in original symbol
    :return: (bli, ble, num): bli, ble: Two Nblck x 1 vectors defining the segments' starting and ending indices;
                                num: An nv x 1 vector containing the number of segments each sample belongs to
    return_tuple = (ww, po, nv)
    return return_tuple
    """
    if ww == 0:
        po = 0
        ww = nv
    elif nv < ww < 32:
        print("Error fftsegs: Bad value for window width of %d" % ww)
        return
    out = 0
    while out == 0:
        # clear bli ble
        bli = []
        ble = []
        # % How many blocks?
        jmp = numpy.floor((100 - po) * ww / 100)  # jump from block to block
        nblck = nv / jmp
        ib = 0
        cnt = 0
        while ble == [] or ble[-1] < (nv - 1):
            bli.append(ib)
            ble.append(min(ib + ww - 1, nv))
            cnt += 1
            ib += jmp
        # If the last block is too small, spread the love
        if ble[-1] - bli[-1] < (0.1 * ww):  # Too small a last block, merge
            ble[-2] = ble[-1]  # into previous
            cnt -= 1
            del ble[-1]
            del bli[-1]
            out = 1
        elif ble[-1] - bli[-1] < (0.75 * ww):  # Too large to merge, spread it
            ww = ww + numpy.floor((ble[-1] - bli[-1]) / nblck)
            out = 0
        else:  # Last block big enough, proceed
            out = 1
        # ble - bli + 1
        # out
    # bli
    # ble
    # ble - bli + 1
    # now figure out the number of estimates each point of the time series gets
    num = zeros((nv, 1))
    cnt = 1
    while cnt <= len(ble):
        num[bli[-1] : ble[-1] + 1] = num[bli[-1] : ble[-1] + 1] + ones(
            (ble[-1] - bli[-1] + 1, 1)
        )
        cnt += 1
    return bli, ble, num


def analytic_signal(vi, windwidth, percover, win):
    nvi = len(vi)
    # h = zeros(vi.shape)
    bli, ble, num = fftsegs(windwidth, percover, nvi)
    for ii in range(len(bli)):
        v = vi[bli[ii] : ble[ii] + 1]
        nv = len(v)
        if win == 1:
            fv = fft(v * numpy.hamming(nv))
        else:
            fv = fft(v)
        wind = zeros(v.size)
        # zero negative frequencies, double positive frequencies
        if nv % 2 == 0:
            wind[0] = 1  # keep DC
            wind[(nv // 2)] = 1
            wind[1 : (nv // 2)] = 2  # double pos. freq

        else:
            wind[0] = 1
            wind[list(range(1, (nv + 1) // 2))] = 2
        h = ifft(fv * wind)
    for i in range(len(h)):
        h[i] /= numpy.complex(num[i])
    return h


def remove_pn_duplicates(tp, vp, tn, vn, quiet):
    ok = zeros((1, len(tp)), dtype=numpy.int32)
    ok = ok[0]
    # ok[0] = 1
    j = 0
    for i in range(1, min(len(tp), len(tn))):
        #  0.3 is the minimum time before the next beat
        if (tp[i] != tp[i - 1]) and (tn[i] != tn[i - 1]) and (tp[i] - tp[i - 1] > 0.3):
            j += 1
            if j == 127:
                print("stop")
            ok[j] = i
        else:
            if not quiet:
                print("Dropped peak at %s sec" % tp[i])
    ok = ok[: j + 1]
    tp = tp[ok]
    vp = vp[ok]
    tn = tn[ok]
    vn = vn[ok]
    return tp, vp, tn, vn


def remove_duplicates(t, v, quiet):
    j = 0
    for i in range(1, len(t) + 1):
        #  0.3 is the minimum time before the next beat
        if t[i] != t[i - 1] and (t[i] - t[i - 1]) > 0.3:
            j += 1
            t[j] = t[i]
            v[j] = v[i]
        elif quiet == 0:
            print("Dropped peak at %s sec" % t[i])
    t = t[: j + 1]
    v = v[: j + 1]
    return t, v


def clean_resamp(v):
    i_nan = nonzero(numpy.isnan(v))  # the bad
    i_nan = i_nan[0]
    i_good = nonzero(numpy.isfinite(v))  # the good
    i_good = i_good[0]
    for i in range(0, len(i_nan)):
        if i_nan[i] < i_good[0]:
            v[i_nan[i]] = v[i_good[0]]
        elif i_nan[i] > i_good[-1]:
            v[i_nan[i]] = v[i_good[-1]]
        else:
            print("Error: Unexpected NaN case")
            v[i_nan[i]] = 0
    return v


def peak_finder(var_v, filename=None, v=None):
    """,
                phys_fs=(1 / 0.025),
                zero_phase_offset=0.5,
                quiet=0,
                resample_fs=(1 / 0.025),
                frequency_cutoff=10,
                fir_order=80,
                interpolation_style='linear',
                demo=0,
                as_window_width=0,
                as_percover=0,
                as_fftwin=0,
                sep_dups=0):
    """
    """
    Example: PeakFinder('Resp*.1D') or PeakFinder(var_vector) where var_vector is a column vector.
    If var_vector is a matrix, each column is processed separately.
    :param var_vector: column vector--list of list(s)
    :param phys_fs: Sampling frequency
    :param zero_phase_offset: Fraction of the period that corresponds to a phase of 0
                                0.5 means the middle of the period, 0 means the 1st peak
    :param quiet:
    :param resample_fs:
    :param frequency_cutoff:
    :param fir_order: BC ???
    :param interpolation_style:
    :param demo:
    :param as_window_width:
    :param as_percover:
    :param fftwin:
    :param sep_dups:
    :return: [r, e] r = Peak of var_vector; e = error value
    """
    # #clear all but var_vector (useful if I run this function as as script)
    # keep('var_vector', 'opt')
    var_vector = dict(
        phys_fs=(1 / 0.025),
        zero_phase_offset=0.5,
        quiet=0,
        resample_fs=(1 / 0.025),
        frequency_cutoff=10,
        fir_order=80,
        interpolation_style="linear",
        demo=0,
        as_window_width=0,
        as_percover=0,
        as_fftwin=0,
        sep_dups=0,
    )
    var_vector.update(var_v)
    default_div = 1 / 0.025
    if (var_vector["phys_fs"] != default_div) and (
        var_vector["resample_fs"] == default_div
    ):
        var_vector["resample_fs"] = var_vector["phys_fs"]
    if var_vector["demo"]:
        var_vector["quiet"] = 0
    else:
        pause = False  # pause off
    e = False  # default value for e
    r = {}
    # Some filtering
    nyquist_filter = var_vector["phys_fs"] / 2.0
    # w[1] = 0.1/filter_nyquist  # Cut frequencies below 0.1Hz
    # w[2] = var_vector['frequency_cutoff']/filter_nyquist  # Upper cut off frequency normalized
    # b = signal.firwin(var_vector['fir_order'], w, 'bandpass')  # FIR filter of order 40
    w = var_vector["frequency_cutoff"] / nyquist_filter
    b = firwin(
        numtaps=(var_vector["fir_order"] + 1), cutoff=w, window="hamming"
    )  # FIR filter of order 40
    b = numpy.array(b)
    no_dups = 1  # Remove duplicates that might come up when improving peak location
    """
    if isinstance(var_vector, str):
        L = glob(var_vector)  # NEED TO CONVERT ZGLOBB INTO LIST MAKER OF FILE OBJECTS; I.E. type(L) == list
        nl = len(L)
        #if isinstance(L, (int, long, float, complex)):
            #print 'Error: File (%s) not found\n', var_vector
            #e = True
            #return e
    else:
        L = []
        nl = len(var_vector)
        if nl < 1:
            print 'Error: No vectors\n', nl
            e = True
            return e
    """
    nl = 1  # temporary, delete this line when above lines get fixed with glob
    # del(r) # "Must clear it. Or next line fails" -- Probably unnecessary in Python
    r_list = []
    for i in range(nl):
        r = {
            "v_name": filename,
            "t": [],
            "x": [],
            "iz": [],  # zero crossing (peak) locations
            "p_trace": [],
            "tp_trace": [],
            "n_trace": [],
            "tn_trace": [],
            "prd": [],
            "t_mid_prd": [],
            "p_trace_mid_prd": [],
            "phase": [],
            "rv": [],
            "rvt": [],
        }
        r_list.append(r)
    """
    for i_column in range(nl):
        if L and not os.path.isdir(L):
            r_list[i_column]['v_name'] = '%s%s' % (sys.path, L[i_column]['name'])"""
    # with open(r['v_name'], "rb") as f:
    # v = f.read()
    # v = map(int, v)
    if v is None:
        v = []
        with open(r["v_name"], "rb") as h:
            for line in h:
                v.append(float(line.strip()))
    
    v_np = numpy.asarray(v)
    # else:
    # r_list[i_column]['v_name'] = 'vector input col %d' % i_column
    # v = var_vector[i_column]

    window_width = 0.2  # Window for adjusting peak location in seconds
    # Remove the mean
    v_np_mean = numpy.mean(v_np)
    v_np = v_np - v_np_mean
    r["v"] = v_np  # Store it for debugging (found it is used in phase estimator)
    # Filter both ways to cancel phase shift
    v_np = lfilter(b, 1, v_np, axis=0)
    v_np = numpy.flipud(v_np)
    v_np = lfilter(b, 1, v_np)
    v_np = numpy.flipud(v_np)
    # Get the analytic signal
    r["x"] = analytic_signal(
        v_np,
        var_vector["as_window_width"] * var_vector["phys_fs"],
        var_vector["as_percover"],
        var_vector["as_fftwin"],
    )

    # Using local version to illustrate, can use hilbert
    # Doing ffts over smaller windows can improve peak detection in the few instances that go undetected but
    # what value to use is not clear and there seems to be at times more errors introduced in the lower envelope.
    nt = len(r["x"])

    # force 't' to have the same length as 'x', rather than trusting
    # divisions (when it should come out evenly)  5 Jun, 2017 [rickr]
    #
    # r['t'] = numpy.arange(0.,
    #                       float(nt) / var_vector['phys_fs'],
    #                       (1. / var_vector['phys_fs']))  # # % FIX FIX FIX
    fsi = 1.0 / var_vector["phys_fs"]
    r["t"] = numpy.array([i * fsi for i in range(len(r["x"]))])

    iz = nonzero((r["x"][0 : nt - 1].imag * r["x"][1:nt].imag) <= 0)
    iz = iz[0]
    polall = -numpy.sign(r["x"][0 : nt - 1].imag - r["x"][1:nt].imag)
    pk = (r["x"][iz]).real
    pol = polall[iz]
    tiz = []
    for i in iz:
        tiz.append(r["t"][i])
    ppp = nonzero(pol > 0)
    ppp = ppp[0]
    p_trace = []
    tp_trace = []
    for i in ppp:
        p_trace.append(pk[i])
        tp_trace.append(tiz[i])
    ppp = nonzero(pol < 0)
    ppp = ppp[0]
    n_trace = []
    tn_trace = []
    for i in ppp:
        n_trace.append(pk[i])
        tn_trace.append(tiz[i])

    if var_vector["quiet"] == 0:
        print(
            "--> Load signal\n--> Smooth signal\n--> Calculate analytic signal Z\n--> Find zero crossing of imag(Z)\n"
        )
        figure(1)
        subplot(211)
        plot(r["t"], real(r["x"]), "g")
        # plot (r_list[i_column].t, imag(r_list[i_column]['x']),'g')
        plot(tp_trace, p_trace, "ro")
        plot(tn_trace, n_trace, "bo")
        # plot (r_list[i_column].t, abs(r_list[i_column]['x']),'k')
        subplot(413)
        vn = real(r["x"]) / (abs(r["x"]) + numpy.spacing(1))
        plot(r["t"], vn, "g")
        ppp = nonzero(pol > 0)
        ppp = ppp[0]
        for i in ppp:
            plot(tiz[i], vn[iz[i]], "ro")
        ppp = nonzero(pol < 0)
        ppp = ppp[0]
        for i in ppp:
            plot(tiz[i], vn[iz[i]], "bo")
        if var_vector["demo"]:
            # need to add a pause here - JZ
            # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
            pass

    # Some polishing
    if 1 == 1:
        nww = numpy.ceil((window_width / 2) * var_vector["phys_fs"])
        pkp = pk
        r["iz"] = iz
        for i in range(len(iz)):
            ###################### left off here, turns out there's a
            # difference in floating point precision in the calculation
            # of r['x'], maybe look into the reason why they'd be different

            # force these to ints    17 May 2017 [DNielson]
            n0 = int(max(2, iz[i] - nww))
            n1 = int(min(nt, iz[i] + nww))
            temp = (r["x"][n0 : n1 + 1]).real
            if pol[i] > 0:
                xx, ixx = numpy.max(temp, 0), numpy.argmax(temp, 0)
            else:
                xx, ixx = numpy.min(temp, 0), numpy.argmin(temp, 0)
            r["iz"][i] = n0 + ixx - 1
            pkp[i] = xx
            if i == 100:
                print("pause")
        tizp = r["t"][r["iz"]]

        ppp = nonzero(pol > 0)
        ppp = ppp[0]
        r["p_trace"] = pkp[ppp]
        r["tp_trace"] = tizp[ppp]
        ppp = nonzero(pol < 0)
        ppp = ppp[0]
        r["n_trace"] = pkp[ppp]
        r["tn_trace"] = tizp[ppp]

        if no_dups:
            # remove duplicates
            if var_vector["sep_dups"]:
                print("YOU SHOULD NOT BE USING THIS.\n")
                print(" left here for the record\n")
                [r["tp_trace"], r["p_trace"]] = remove_duplicates(
                    r["tp_trace"], r["p_trace"], var_vector["quiet"]
                )
                [r["tn_trace"], r["n_trace"]] = remove_duplicates(
                    r["tn_trace"], r["n_trace"], var_vector["quiet"]
                )
            else:
                r["tp_trace"], r["p_trace"], r["tn_trace"], r[
                    "n_trace"
                ] = remove_pn_duplicates(
                    r["tp_trace"],
                    r["p_trace"],
                    r["tn_trace"],
                    r["n_trace"],
                    var_vector["quiet"],
                )
            if len(r["p_trace"]) != len(r["n_trace"]):
                print("Bad news in tennis shoes. I'm outta here.\n")
                e = True
                return r, e

        if var_vector["quiet"] == 0:
            print("--> Improved peak location\n--> Removed duplicates")
            # style.use('ggplot')
            subplot(211)
            plot(r["tp_trace"], r["p_trace"], "r+", r["tp_trace"], r["p_trace"], "r")
            plot(r["tn_trace"], r["n_trace"], "b+", r["tn_trace"], r["n_trace"], "b")
            if var_vector["demo"]:
                # need to add a pause here - JZ
                # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
                pass
    else:
        tizp = tiz
        r["iz"] = iz
        pkp = pk
        r["p_trace"] = p_trace
        r[
            "n_trace"
        ] = (
            n_trace
        )  # This seems like a mistake, the .m file's version is highly suspect

    # Calculate the period
    nptrc = len(r["tp_trace"])
    print(r["tp_trace"])
    r["prd"] = r["tp_trace"][1:nptrc] - r["tp_trace"][0 : nptrc - 1]
    r["p_trace_mid_prd"] = (r["p_trace"][1:nptrc] + r["p_trace"][0 : nptrc - 1]) / 2.0
    r["t_mid_prd"] = (r["tp_trace"][1:nptrc] + r["tp_trace"][0 : nptrc - 1]) / 2.0
    if var_vector["quiet"] == 0:
        print("--> Calculated the period (from beat to beat)\n")
        subplot(211)
        plot(r["t_mid_prd"], r["p_trace_mid_prd"], "kx")
        for i in range(0, len(r["prd"])):
            text(r["t_mid_prd"][i], r["p_trace_mid_prd"][i], ("%.2f" % r["prd"][i]))
        if var_vector["demo"]:
            # need to add a pause here - JZ
            # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
            pass
        show()

    if var_vector["interpolation_style"] != "":
        # Interpolate to slice sampling time grid:
        step_interval = 1.0 / var_vector["resample_fs"]
        # r['tR'] = numpy.arange(0, max(r['t']) + step_interval, step_interval)

        # allow for a ratio that is barely below an integer
        #                               5 Jun, 2017 [rickr]
        step_size = int(max(r["t"]) / step_interval + g_epsilon) + 1
        r["tR"] = []

        for i in range(0, step_size):
            r["tR"].append(i * step_interval)
        """
        with open('tR.csv', 'w') as f:
            for i in r['tR']:
                f.write("%s\n" % i)
        # Squeeze these arrays down from an [x,y] shape to an [x,] shape in order to use interp1d
        r['tp_trace'] = r['tp_trace'].squeeze()
        r['p_trace'] = r['p_trace'].squeeze()
        """
        r["p_trace_r"] = interp1d(
            r["tp_trace"],
            r["p_trace"],
            var_vector["interpolation_style"],
            bounds_error=False,
        )
        r["p_trace_r"] = r["p_trace_r"](r["tR"])
        # r['tn_trace'] = r['tn_trace'].squeeze()
        # r['n_trace'] = r['n_trace'].squeeze()
        r["n_trace_r"] = interp1d(
            r["tn_trace"],
            r["n_trace"],
            var_vector["interpolation_style"],
            bounds_error=False,
        )(r["tR"])
        # r['t_mid_prd'] = r['t_mid_prd'].squeeze()
        # r['prd'] = r['prd'].squeeze()
        r["prdR"] = interp1d(
            r["t_mid_prd"],
            r["prd"],
            var_vector["interpolation_style"],
            bounds_error=False,
        )(r["tR"])
        # You get NaN when tR exceeds original signal time, so set those
        # to the last interpolated value
        r["p_trace_r"] = clean_resamp(r["p_trace_r"])
        r["n_trace_r"] = clean_resamp(r["n_trace_r"])
        r["prdR"] = clean_resamp(r["prdR"])

        # if (i_column != nl):
        # input('Hit enter to proceed...','s')
        # if var_vector['quiet'] == 0:
        # plotsign2(1)

    return r, e
