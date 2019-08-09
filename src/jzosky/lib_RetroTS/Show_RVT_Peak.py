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

from matplotlib.pyplot import (
    figure,
    plot,
    subplot,
    text,
    xlabel,
    title,
    legend,
    grid,
    show,
)
from numpy import real, spacing, pi
from .zscale import z_scale


def show_rvt_peak(r, fg):
    """

    :param r:
    :param fg:
    :return:
    """
    rvt_peak_plot = figure(fg)
    rvt_peak_plot.clf()
    # set(fg, 'KeyPressFcn', @afni_fig_interface)  # Appears to be unnecessary to retain from MATLAB code
    subplot(211)
    plot(r["t"], real(r["x"]), "g")

    if r["rvt"].any():
        subplot(211)
        plot(
            r["t_mid_prd"], z_scale(r["rvt"], min(r["p_trace"]), max(r["p_trace"])), "k"
        )
    plot(r["tp_trace"], r["p_trace"], "ro", r["tp_trace"], r["p_trace"], "r")
    plot(r["tn_trace"], r["n_trace"], "bo", r["tn_trace"], r["n_trace"], "b")
    plot(r["t_mid_prd"], r["p_trace_mid_prd"], "kx")
    for i in range(len(r["prd"])):
        text(r["t_mid_prd"][i], r["p_trace_mid_prd"][i], "%.2f" % r["prd"][i])
    if r["tR"]:
        if r["p_trace_r"].any():
            plot(r["tR"], r["p_trace_r"], "m")
            plot(r["tR"], r["n_trace_r"], "y")
        if r["rvtrs"].any():
            plot(
                r["tR"], z_scale(r["rvtrs"], min(r["p_trace"]), max(r["p_trace"])), "k."
            )
    xlabel("time (sec)")
    title(r["v_name"])  # , 'Interpreter', 'None')
    subplot(413)
    vn = real(r["x"]) / (abs(r["x"]) + spacing(1))
    plot(r["t"], vn, "g")
    plot(r["t"], r["phase"] / 2 / pi, "m")
    if "phase_r" in r:
        plot(r["tR"], r["phase_r"] / 2 / pi, "m-.")
    xlabel("time (sec)")
    title("Scaled by magnitude of analytical signal")  # , 'Interpreter', 'None')
    legend(["Scaled signal", "phase"])
    subplot(414)
    plot(
        r["time_series_time"], r["phase_slice"][:, 0], "ro"
    )  # This one isn't any different than the next plot line, check out why values are funky
    plot(r["time_series_time"], r["phase_slice"][:, 1], "bo")
    plot(r["time_series_time"], r["phase_slice"][:, 1], "b-")
    plot(r["t"], r["phase"], "k")
    grid("on")
    xlabel("time (sec)")
    title("Phase sampled at slice acquisition time")
    legend(["slice 0", "slice 1", "slice 1", "original phase"])
    # plotsign2(fg)  # Another AFNI function, not crucial at the moment to making one figure
    show()
