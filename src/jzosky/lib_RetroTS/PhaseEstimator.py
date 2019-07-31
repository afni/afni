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

from numpy import zeros, ones, nonzero, pi, argmin, sin, cos
from numpy import size, arange, clip, histogram, r_, Inf, divide, append, delete, array
from .zscale import z_scale
import matplotlib.pyplot as plt


def my_hist(x, bin_centers):
    """
    This frivolous yet convenient conversion from bin-edges to bin-centers is from Stack Overflow user Bas Swinckels
    http://stackoverflow.com/questions/18065951/why-does-numpy-histogram-python-leave-off-one-element-as-compared-to-hist-in-m
    :param x:dataset
    :param bin_centers:bin values in a list to be moved from edges to centers
    :return: counts = the data in bin centers ready for pyplot.bar
    """
    bin_edges = r_[-Inf, 0.5 * (bin_centers[:-1] + bin_centers[1:]), Inf]
    counts, edges = histogram(x, bin_edges)
    return counts


def phase_base(amp_type, phasee):
    """


    :type phasee: object
    :param amp_type:    if 0, it is a time-based phase estimation
                        if 1, it is an amplitude-based phase estimation
    :param phasee: phasee information
    :return:
    """
    if amp_type == 0:
        # Calculate the phase of the trace, with the peak to be the start of the phase
        nptrc = len(phasee["tp_trace"])
        phasee["phase"] = -2 * ones(size(phasee["t"]))
        i = 0
        j = 0
        while i <= (nptrc - 2):
            while phasee["t"][j] < phasee["tp_trace"][i + 1]:
                if phasee["t"][j] >= phasee["tp_trace"][i]:
                    # Note: Using a constant 244 period for each interval
                    # causes slope discontinuity within a period.
                    # One should resample period[i] so that it is
                    # estimated at each time in phasee['t'][j],
                    # dunno if that makes much of a difference in the end however.
                    if j == 10975:
                        pass
                    phasee["phase"][j] = (
                        phasee["t"][j] - phasee["tp_trace"][i]
                    ) / phasee["prd"][i] + phasee["zero_phase_offset"]
                    if phasee["phase"][j] < 0:
                        phasee["phase"][j] = -phasee["phase"][j]
                    if phasee["phase"][j] > 1:
                        phasee["phase"][j] -= 1
                j += 1
            if i == 124:
                pass
            i += 1

        # Remove the points flagged as unset
        temp = nonzero(phasee["phase"] < -1)
        phasee["phase"][temp] = 0.0
        # Change phase to radians
        phasee["phase"] = phasee["phase"] * 2 * pi
    else:  # phase based on amplitude
        # at first scale to the max
        mxamp = max(phasee["p_trace"])
        phasee["phase_pol"] = []
        gR = z_scale(phasee["v"], 0, mxamp)  # Scale, per Glover 2000's paper
        bins = arange(0.01, 1.01, 0.01) * mxamp
        hb_value = my_hist(gR, bins)
        # hb_value = histogram(gR, bins)
        if phasee["show_graphs"] == 1:
            center = (bins[:-1] + bins[1:]) / 2
            plt.bar(center, hb_value[: len(hb_value) - 1])  # , align='center')
            plt.show()
        # find the polarity of each time point in v
        i = 0
        itp = 0
        inp = 0
        tp = phasee["tp_trace"][0]
        tn = phasee["tn_trace"][0]
        while (
            (i <= len(phasee["v"])) and (phasee["t"][i] < tp) and (phasee["t"][i] < tn)
        ):
            phasee["phase_pol"].append(0)
            i += 1
        if tp < tn:
            # Expiring phase (peak behind us)
            cpol = -1
            itp = 1
        else:
            # Inspiring phase (bottom behind us)
            cpol = 1
            inp = 1
        phasee["phase_pol"] = zeros(
            size(phasee["v"])
        )  # Not sure why you would replace the
        # list that you created 10 lines prior to this
        # Add a fake point to tptrace and tntrace to avoid ugly if statements
        phasee["tp_trace"] = append(phasee["tp_trace"], phasee["t"][-1])
        phasee["tn_trace"] = append(phasee["tn_trace"], phasee["t"][-1])
        while i < len(phasee["v"]):
            phasee["phase_pol"][i] = cpol
            if phasee["t"][i] == phasee["tp_trace"][itp]:
                cpol = -1
                itp = min((itp + 1), (len(phasee["tp_trace"]) - 1))
            elif phasee["t"][i] == phasee["tn_trace"][inp]:
                cpol = 1
                inp = min((inp + 1), (len(phasee["tn_trace"]) - 1))
            # cpol, inp, itp, i, R
            i += 1
        phasee["tp_trace"] = delete(phasee["tp_trace"], -1)
        phasee["tn_trace"] = delete(phasee["tn_trace"], -1)
        if phasee["show_graphs"] == 1:
            # clf
            plt.plot(phasee["t"], gR, "b")
            ipositive = nonzero(phasee["phase_pol"] > 0)
            ipositive = ipositive[0]
            ipositive_x = []
            for i in ipositive:
                ipositive_x.append(phasee["t"][i])
            ipositive_y = zeros(size(ipositive_x))
            ipositive_y.fill(0.55 * mxamp)
            plt.plot(ipositive_x, ipositive_y, "r.")
            inegative = nonzero(phasee["phase_pol"] < 0)
            inegative = inegative[0]
            inegative_x = []
            for i in inegative:
                inegative_x.append(phasee["t"][i])
            inegative_y = zeros(size(inegative_x))
            inegative_y.fill(0.45 * mxamp)
            plt.plot(inegative_x, inegative_y, "g.")
            plt.show()
        # Now that we have the polarity, without computing sign(dR/dt)
        #   as in Glover et al 2000, calculate the phase per eq. 3 of that paper
        # First the sum in the numerator
        for i, val in enumerate(gR):
            gR[i] = round(val / mxamp * 100) + 1
        gR = clip(gR, 0, 99)
        shb = sum(hb_value)
        hbsum = []
        hbsum.append(float(hb_value[0]) / shb)
        for i in range(1, 100):
            hbsum.append(hbsum[i - 1] + (float(hb_value[i]) / shb))
        for i in range(len(phasee["t"])):
            phasee["phase"].append(pi * hbsum[int(gR[i]) - 1] * phasee["phase_pol"][i])
        phasee["phase"] = array(phasee["phase"])

    # Time series time vector
    phasee["time_series_time"] = arange(
        0, (max(phasee["t"]) - 0.5 * phasee["volume_tr"]), phasee["volume_tr"]
    )
    # Python uses half open ranges, so we need to catch the case when the stop
    # is evenly divisible by the step and add one more to the time series in
    # order to match Matlab, which uses closed ranges  1 Jun 2017 [D Nielson]
    if (max(phasee["t"]) - 0.5 * phasee["volume_tr"]) % phasee["volume_tr"] == 0:
        phasee["time_series_time"] = append(
            phasee["time_series_time"],
            [phasee["time_series_time"][-1] + phasee["volume_tr"]],
        )
    phasee["phase_slice"] = zeros(
        (len(phasee["time_series_time"]), phasee["number_of_slices"])
    )
    phasee["phase_slice_reg"] = zeros(
        (len(phasee["time_series_time"]), 4, phasee["number_of_slices"])
    )
    for i_slice in range(phasee["number_of_slices"]):
        tslc = phasee["time_series_time"] + phasee["slice_offset"][i_slice]
        for i in range(len(phasee["time_series_time"])):
            imin = argmin(abs(tslc[i] - phasee["t"]))
            # mi = abs(tslc[i] - phasee['t']) # probably not needed
            phasee["phase_slice"][i, i_slice] = phasee["phase"][imin]
        # Make regressors for each slice
        phasee["phase_slice_reg"][:, 0, i_slice] = sin(
            phasee["phase_slice"][:, i_slice]
        )
        phasee["phase_slice_reg"][:, 1, i_slice] = cos(
            phasee["phase_slice"][:, i_slice]
        )
        phasee["phase_slice_reg"][:, 2, i_slice] = sin(
            2 * phasee["phase_slice"][:, i_slice]
        )
        phasee["phase_slice_reg"][:, 3, i_slice] = cos(
            2 * phasee["phase_slice"][:, i_slice]
        )

    if phasee["quiet"] == 0 and phasee["show_graphs"] == 1:
        print("--> Calculated phase")
        plt.subplot(413)
        a = divide(divide(phasee["phase"], 2), pi)
        plt.plot(phasee["t"], divide(divide(phasee["phase"], 2), pi), "m")
        if "phase_r" in phasee:
            plt.plot(phasee["tR"], divide(divide(phasee["phase_r"], 2), pi), "m-.")
        plt.subplot(414)
        plt.plot(
            phasee["time_series_time"],
            phasee["phase_slice"][:, 1],
            "ro",
            phasee["time_series_time"],
            phasee["phase_slice"][:, 2],
            "bo",
            phasee["time_series_time"],
            phasee["phase_slice"][:, 2],
            "b-",
        )
        plt.plot(phasee["t"], phasee["phase"], "k")
        # grid on
        # title it
        plt.title(phasee["v_name"])
        plt.show()
        # Need to implement this yet
        # if phasee['Demo']:
        # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
    return phasee


def phase_estimator(amp_phase, phase_info):
    """
    v_name='',
    amp_phase=0,
    t=[],
    x=[],
    iz=[],   # zero crossing (peak) locations
    p_trace=[],
    tp_trace=[],
    n_trace=[],
    tn_trace=[],
    prd=[],
    t_mid_prd=[],
    p_trace_mid_prd=[],
    phase=[],
    rv=[],
    rvt=[],
    var_vector=[],
    phys_fs=(1 / 0.025),
    zero_phase_offset=0.5,
    quiet=0,
    resample_fs=(1 / 0.025),
    f_cutoff=10,
    fir_order=80,
    resample_kernel='linear',
    demo=0,
    as_window_width=0,
    as_percover=0,
    as_fftwin=0,
    sep_dups=0,
    phasee_list=0,
    show_graphs=0
    """
    """
    Example: PhaseEstimator.phase_estimator(amp_phase, info_dictionary)
    or PhaseEstimator.phase_estimator(v) where v is a list
    if v is a matrix, each column is processed separately.
    :param var_vector: column vector--list of list(s)
    :param phys_fs: Sampling frequency
    :param zero_phase_offset: Fraction of the period that corresponds to a phase of 0
                                0.5 means the middle of the period, 0 means the 1st peak
    :param quiet:
    :param resample_fs:
    :param frequency_cutoff:
    :param fir_order: BC ???
    :param resample_kernel:
    :param demo:
    :param as_window_width:
    :param as_percover:
    :param fftwin:
    :param sep_dups:
    :return: *_phased: phase estimation of input signal
    """
    phasee = dict(
        v_name="",
        t=[],
        x=[],
        iz=[],  # zero crossing (peak) locations
        volume_tr=2,
        p_trace=[],
        tp_trace=[],
        n_trace=[],
        tn_trace=[],
        prd=[],
        t_mid_prd=[],
        p_trace_mid_prd=[],
        phase=[],
        rv=[],
        rvt=[],
        var_vector=[],
        phys_fs=(1 / 0.025),
        zero_phase_offset=0.5,
        quiet=0,
        resample_fs=(1 / 0.025),
        frequency_cutoff=10,
        fir_order=80,
        resample_kernel="linear",
        demo=0,
        as_window_width=0,
        as_percover=0,
        as_fftwin=0,
        sep_dups=0,
        phasee_list=0,
        show_graphs=0,
        number_of_slices=0,
    )
    phasee.update(phase_info)
    if isinstance(phasee["phasee_list"], type([])):
        return_phase_list = []
        for phasee_column in phasee["phasee_list"]:
            return_phase.append(phase_base(amp_phase, phasee_column))
        return return_phase_list
    else:
        return_phase = phase_base(amp_phase, phasee)
        return return_phase
