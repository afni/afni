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

from numpy import percentile, size, array


def z_scale(x, lower_bound, upper_bound, perc=[]):
    # ZSCALE (X,UB,LB)
    #
    # This function scales  X into Y such that
    #   its maximum value is UB
    #   and minimum value is LB
    # If perc is specified, then clipping is done
    # at the percentile range specified (e.g. [2, 98])
    # before scaling.
    # If X is all constants, it gets scaled to UB;
    #
    #           Ziad, Oct 30 96 / modified March 18 97

    if type(x) != type(array):
        x = array(x)
    if upper_bound < lower_bound:
        print("Error z_scale: Upper bound < Lower bound")
        return
    if perc:
        lower_clip = percentile(x, perc[0])
        upper_clip = percentile(x, perc[1])
        x = x.clip(lower_clip, upper_clip)

    xmin = min(x)
    xmax = max(x)

    if xmin == xmax:
        # If x is all constants, then scale up to upper_bound value
        y = array(size(x))
        y.fill(upper_bound)
    else:
        # If x is not all constants, then scale to bounds
        y = (((x - xmin) / (xmax - xmin)) * (upper_bound - lower_bound)) + lower_bound
    return y
