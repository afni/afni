#!/usr/bin/env python3

import sys, copy
import numpy as np
import matplotlib.pyplot as plt


class RetroPlobj:
    """An object for holding one time series or set of points for
plotting.

    """

    def __init__(self, x, y,
                 ls      = '-',
                 lw      = None,     # if None -> calc automatically
                 color   = None,     # if None -> calc automatically
                 marker  = None,     # could change to 'o'
                 ms      = None,     # if None -> calc automatically
                 verb=0):
        """Create object holding data to plot.

        """

        self.verb         = verb               # int, verbosity level

        self.x            = copy.deepcopy(x)   # list/arr, x-axis values
        self.y            = copy.deepcopy(y)   # list/arr, y-axis values

        self.color        = color              # str/float, color value
        self.ls           = ls                 # str, linestyle
        self.lw           = lw                 # str, linewidth
        self.marker       = marker             # str, marker type
        self.ms           = ms                 # str, markersize

        # ----------------------------

        check = self.check_inputs()


    def check_inputs(self):
        """check various features of inputs for consistency"""
        NBAD = 0

        if self.n_pts != len(self.y) :
            print("** Error: different number of points:")
            print("   len(x) = {}, len(y) = {}".format(self.n_pts, len(self.y)))
            NBAD+= 1

        return NBAD

    @property
    def n_pts(self):
        """The number of time points in the time series."""
        return len(self.x)

    @property
    def max_xval(self):
        """The maximum value in the x array."""
        return np.max(self.x)

    @property
    def min_xval(self):
        """The minimum value in the x array."""
        return np.min(self.x)

    @property
    def max_yval(self):
        """The maximum value in the y array."""
        return np.max(self.y)

    @property
    def min_yval(self):
        """The minimum value in the y array."""
        return np.min(self.y)


class RetroFig:
    """An object for holding 1 or more fig objects, and then for plotting
them.

    """

    def __init__(self, 
                 figname = 'retro_fig.pdf',
                 title   = 'the plot',
                 xlabel  = 't (s)',
                 ylabel  = 'data',
                 figsize = None,
                 dpi     = 300,
                 max_n_per_sub = 2000,
                 verb=0):
        """Create object holding data to plot.

        """

        self.verb          = verb               # int, verbosity level
        self.figname       = figname            # str, name of output fig
        self.title         = title              # str, title of plot
        self.xlabel        = xlabel             # str, label along x-axis
        self.ylabel        = ylabel             # str, data label

        self.figsize       = figsize            # 2-tuple, fig dims
        self.figsize_use   = None               # 2-tuple, fig dims
        self.dpi           = dpi                # int, res for vector img

        self.list_plobj    = []                 # list, of plot objects
        self.max_n_per_sub = max_n_per_sub      # n, used to split subplots

        # ------------------


    # --------------------------------------------------------------------
    # Comment: the condition for calculating whether we need to use
    # multiplots from the following logic. Consider saying we want to
    # limit each subplot to at most max_n_per_sub=2000 points across
    # one full subplot range.  That establishes a linear density
    # criterion: the max linear density is rho_max = max_n_per_sub/xwidth,
    # where xwidth is our full subplot window width (initially unknown),
    # in units of the x array elements.  We need to solve for xwidth.
    # 
    # But we can calculate the average linear density for each plobj,
    # which is just rho_i = n_i/x0_i, where n_i is the plobj's number
    # of points and x0_i is the plobj's extent along the x-axis (so
    # rho is known).  We want to make sure that no plot is too dense
    # on average, so the condition we want is: max(rho_i) <= rho_max.
    # We can find max(rho_i) easily, and this leads to a constraint on
    # amount of x-axis that can be stored in one subplot: xwidth <=
    # max_n_per_sub/max(rho_i).
    # 
    # Note that 1/rho_i could be viewed as the average x0_i spacing,
    # dx_i, so the condition is also: xwidth <= max_n_per_sub*min(dx_i).
    # This is what we use below.
    # + Find the total X range: (minmax_x[1] - minmax_x[0])
    # + Find the subplot window criterion value, 
    #     max_xwidth = max_n_per_sub*min(dx_i)
    # + Let: m = int(ceil((minmax_x[1] - minmax_x[0]) / max_xwidth))
    # + The number of subplots be:  nsub = max(1, m).
        
    @property
    def n_plobj(self):
        """The number of plot objects (lines, dots, etc.) in the data."""
        return len(self.list_plobj)
        
    @property
    def all_n_pts(self):
        """List of all n_pts across plobjs."""
        return [self.list_plobj[ii].n_pts for ii in range(self.n_plobj)]

    @property
    def max_n_pts(self):
        """maximum number of points."""
        return max(self.all_n_pts)

    @property
    def all_ave_dx(self):
        """List of all densities of points (average delta-x values)."""
        lll_npts  = self.all_n_pts

        lll_xmin  = [self.list_plobj[ii].min_xval \
                     for ii in range(self.n_plobj)]
        lll_xmax  = [self.list_plobj[ii].max_xval \
                     for ii in range(self.n_plobj)]
        lll_avedx = [(lll_xmax[ii] - lll_xmin[ii])/(lll_npts[ii]-1) \
                     for ii in range(self.n_plobj)]

        return lll_avedx

    @property
    def min_ave_dx(self):
        """Min of average delta-x values (max point density)."""
        return min(self.all_ave_dx)

    @property
    def max_xwidth(self):
        """Calculate the max allowed x-axis interval of a subplot, taking into
        account the min_ave_dx and (chosen) n_pts per subplot."""
        return self.max_n_per_sub * self.min_ave_dx

    @property
    def n_subplots(self):
        """The number of subplots in the data, based on the maximum average
        desnity of points in an included plobj, the full x-range
        across all plobj, and the desired max number of points per
        subplot."""

        xrange_full = self.minmax_x[1] - self.minmax_x[0]
        m = int(np.ceil(xrange_full/self.max_xwidth))
        return max([1, m])

    @property
    def is_multiplot(self):
        """Simply an abbreviation (of sorts) for whether we are in a multiplot
        (= multiple subplot) situation, mainly for simpler code
        reading."""
        return (self.n_subplots > 1)
    
    @property
    def all_sub_xwin(self):
        """Make the subplot windows along the x-axis."""
        
        list_win = []
        wmin, max_final = self.minmax_x
        for ii in range(self.n_subplots):
            subwin = [wmin, wmin + self.max_xwidth]
            list_win.append(subwin)
            wmin = subwin[1]          # set for next iteration
        return list_win

    @property
    def all_sub_slice(self):
        """Make the pairs of indices for slicing within the plobj x and y
        arrays for each subplot.
        
        Return a list (n_subplots elements) of lists (each of n_plobj
        elements."""
        
        list_subplot = []

        for ii in range(self.n_subplots):
            list_sli = []
            xwin = self.all_sub_xwin[ii]
            for jj in range(self.n_plobj):
                # get range of [ii]th xwin in terms of indices
                a1 = xwin[0] <= self.list_plobj[jj].x 
                a2 = self.list_plobj[jj].x < xwin[1]
                a3 = a1 * a2
                if np.sum(a3) :
                    # if any elements were found, append as half-open
                    # interval pair: [idx_min, idx_max+1)
                    arr_idx = np.arange(len(a3))[a3]
                    bounds  = [min(arr_idx), max(arr_idx)+1]
                else:
                    # this array has no values here---OK!
                    bounds = [0, 0]
                list_sli.append(bounds)
            list_subplot.append(list_sli)

        return list_subplot

    @property
    def all_color(self):
        """List of all colors across plobjs."""
        return [self.list_plobj[ii].color for ii in range(self.n_plobj)]

    @property
    def minmax_y(self):
        """Tuple of min-minimum and max-maximum across y-values, so all
        subplots have same vertical ranges."""
        minval = np.min([self.list_plobj[ii].min_yval \
                       for ii in range(self.n_plobj)])
        maxval = np.max([self.list_plobj[ii].max_yval \
                       for ii in range(self.n_plobj)])
        return np.array([minval, maxval])

    @property
    def range_ylim(self):
        """Range based on min/max y-values (basically, expand min/max by
        10%)."""
        minval, maxval = self.minmax_y
        delta = 0.1*(maxval - minval)
        return np.array([minval-delta, maxval+delta])

    @property
    def minmax_x(self):
        """Tuple of min-minimum and max-maximum across x-values."""
        minval = np.min([self.list_plobj[ii].min_xval \
                       for ii in range(self.n_plobj)])
        maxval = np.max([self.list_plobj[ii].max_xval \
                       for ii in range(self.n_plobj)])
        return np.array([minval, maxval])

    @property
    def all_lw(self):
        """List of all lw (linewidths) across plobjs."""
        return [self.list_plobj[ii].lw for ii in range(self.n_plobj)]

    @property
    def all_ms(self):
        """List of all ms (markerwidths) across plobjs."""
        return [self.list_plobj[ii].ms for ii in range(self.n_plobj)]

   # --------------------------------------------------------------------

    def add_plobj(self, plobj):
        """Add the specified plobj object to the retrofig object."""

        self.list_plobj.append(plobj)

    def prep_plotvals(self):
        """Go through plobj list and fill in any unspecified values for
        plotting (basically, plobj kwargs that are None by def)"""

        for ii in range(self.n_plobj):
            self.list_plobj[ii]
            if self.list_plobj[ii].ms == None :
                self.list_plobj[ii].ms = 1.5 # !!!!!! come back to this
            if self.list_plobj[ii].lw == None :
                self.list_plobj[ii].lw = 1 # !!!!!! come back to this
            if self.list_plobj[ii].color == None :
                for jj in range(self.n_plobj):
                    color = 'C{}'.format(jj) 
                    if not(color in self.all_color) :
                        self.list_plobj[ii].color = color
                        break
                if self.list_plobj[ii].color == None :
                    self.list_plobj[ii].color = 'black'

        if self.figsize == None :
            self.figsize_use = (7, self.n_plobj*3.5)
        else:
            self.figsize_use = copy.deepcopy(self.figsize)

        
    def make_plot(self, do_show = False, do_save = True):
        """Create the plot"""

        self.prep_plotvals()

        # create figure and subplot array
        fff       = plt.figure( self.title, figsize=self.figsize )
        a, subpl  = plt.subplots( self.n_subplots, 1, 
                                  figsize = self.figsize )

        for ii in range( self.n_subplots ):
            # loop over all plobj and add them
            for jj in range( self.n_plobj ):
                plobj = copy.deepcopy(self.list_plobj[jj])

                # make 'pp' the object for the subplot in question
                if self.is_multiplot :  pp = subpl[ii]
                else:                   pp = subpl

                if 1 :
                    pp.axhline(y=0, c='0.6', ls='-', lw=0.5)

                # treat all plots as slices
                idx = self.all_sub_slice[ii][jj]
                sp = pp.plot( plobj.x[idx[0]:idx[1]], 
                              plobj.y[idx[0]:idx[1]],
                              color = plobj.color,
                              ls    = plobj.ls,
                              lw    = plobj.lw,
                              marker= plobj.marker,
                              ms   = plobj.ms )

                if not(ii) and not(jj) :
                    pp.set_title(self.title)

            if ii == self.n_subplots - 1 :
                pp.set_xlabel(self.xlabel)

            pp.set_ylim(self.range_ylim)

        if 1 :
            plt.tight_layout()

        if do_show :
            plt.ion()
            plt.show()

        if do_save :
            plt.savefig(self.figname, dpi=self.dpi)



# ==========================================================================

if __name__ == "__main__" :

    print("plot")

    b = np.random.random(1000)
    a = np.arange(len(b))

    d = np.random.random(1000)
    c = np.arange(len(d))/2.

    ret_plobj1 = RetroPlobj(a,b)
    ret_plobj2 = RetroPlobj(c,d, ls='None', marker='o')

    fff = RetroFig(max_n_per_sub=500)
    fff.add_plobj(ret_plobj1)
    fff.add_plobj(ret_plobj2)
    fff.make_plot()


    sys.exit(0)
