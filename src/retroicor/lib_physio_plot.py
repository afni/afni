#!/usr/bin/env python3

import sys, copy
import numpy as np
import matplotlib.pyplot      as     plt
from   matplotlib.collections import PatchCollection as MPC
import matplotlib.patches     as     mplp
import matplotlib.cm          as     mplcm

DEF_max_n = 1000                     # def npts per subplot (not used now)
DEF_lw    = 0.75                     # def linewidth in plot
DEF_ms    = 1.50                     # def marker size in plot

# =========================================================================

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
                 mec     = None,     # if None -> calc automatically
                 mew     = None,     #
                 alpha   = 1,        # alpha transparency
                 label   = None,     # legend label
                 add_ibandT = False, # make top colorband about (p/t) intervals?
                 add_ibandB = False, # make bot colorband about (p/t) intervals?
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
        self.mec          = mec                # str, markeredgecolor
        self.mew          = mew                # int/fl, markeredgewidth
        self.alpha        = alpha              # int/fl, alpha value
        self.label        = label              # str, label for legend

        self.add_ibandT   = add_ibandT         # bool, top colorband on/off
        self.xw_ibandT    = []                 # list, iband: (xcoord, width)
        self.col_ibandT   = []                 # list, str of float 0-1
        self.add_ibandB   = add_ibandB         # bool, bot colorband on/off
        self.xw_ibandB    = []                 # list, iband: (xcoord, width)
        self.col_ibandB   = []                 # list, str of float 0-1
        # ----------------------------

        check = self.check_inputs()

        # this will make xcoord and width information for the
        # iband; the ycoord and height info comes from the actual
        # figure, bc there may be many plots
        if self.add_ibandT :
            ok_ibandT = self.make_iband('top')
        if self.add_ibandB :
            ok_ibandB = self.make_iband('bot')

    def check_inputs(self):
        """check various features of inputs for consistency"""
        NBAD = 0

        if self.n_pts != len(self.y) :
            print("** Error: different number of points:")
            print("   len(x) = {}, len(y) = {}".format(self.n_pts, len(self.y)))
            NBAD+= 1

        return NBAD

    def make_iband(self, loc='top'):
        """Make list of (xcoord, width) tuples for iband, using x; ycoord and
        height are const and come in other obj, when plotting.

        Also make color values, which are scaled to be between 0 and
        1, and further (arbitrarily) scaled to make a nice color
        representation with the 'bwr' cmap.

        Return 0 on good exit, 1 on bad exit.

        """

        cmap = mplcm.get_cmap('bwr')  # faded red/wh/blue cmap for bands

        all_ivals = np.array([j-i for i, j in zip(self.x[:-1], self.x[1:])])
        med = np.median(all_ivals)
        std = np.std(all_ivals)
        rat = 0.5 + 0.1*(all_ivals - med)/(std)   # ~Zscore, scaled for cmap

        all_col = [cmap(max(min(1,val),0)) for val in rat]
        all_xw  = [(i, j-i) for i, j in zip(self.x[:-1], self.x[1:])]

        if loc == 'top' :
            self.col_ibandT = all_col  # lazy about not deepcopying here
            self.xw_ibandT  = all_xw
        elif loc == 'bot' :
            self.col_ibandB = all_col  # lazy about not deepcopying here
            self.xw_ibandB  = all_xw
        else :
            return 1

        return 0
        

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

    @property
    def n_ibandT(self):
        """The len of the 'top interval band' list."""
        return len(self.xw_ibandT)

    @property
    def n_ibandB(self):
        """The len of the 'bot interval band' list."""
        return len(self.xw_ibandB)

class RetroFig:
    """An object for holding 1 or more fig objects, and then for plotting
them.

    """

    def __init__(self, 
                 figname = 'retro_fig.pdf',
                 title   = 'the plot',
                 xlabel  = 'time (s)',
                 ylabel  = 'data',
                 figsize = None,
                 dpi     = 300,
                 fontsize= 10,
                 max_n_per_line = DEF_max_n,
                 max_t_per_line = 30,
                 max_l_per_fig  = 6,
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
        self.figsize_rem   = None               # 2-tuple, fig dims for remndr
        self.dpi           = dpi                # int, res for vector img
        self.fontsize      = fontsize           # int/fl, size of most text
        self.list_plobj    = []                 # list, of plot objects
        self.max_n_per_line = max_n_per_line    # n, used to split old subplots
        self.max_t_per_line = max_t_per_line    # int/fl, used to split subpl
        self.max_l_per_fig = max_l_per_fig      # int/fl, used to split figs
        self.list_fig_props = []                # list, start, end and figsize

        self.ypad          = 0.3                # used to space ylims in plot
        # ------------------


    # --------------------------------------------------------------------
    # Comment: 
    # 
    # A) The OLD condition for calculating whether we need to use
    # multiplots comes from the following logic. Consider saying we want to
    # limit each subplot to at most max_n_per_line=1000 points across
    # one full subplot range.  That establishes a linear density
    # criterion: the max linear density is rho_max = max_n_per_line/xwidth,
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
    # max_n_per_line/max(rho_i).
    # 
    # Note that 1/rho_i could be viewed as the average x0_i spacing,
    # dx_i, so the condition is also: xwidth <= max_n_per_line*min(dx_i).
    # This is what we use below.
    # + Find the total X range: (minmax_x[1] - minmax_x[0])
    # + Find the subplot window criterion value, 
    #     max_xwidth = max_n_per_line*min(dx_i)
    # + Let: m = int(ceil((minmax_x[1] - minmax_x[0]) / max_xwidth))
    # + The number of subplots be:  nsub = max(1, m).
    # 
    # B) The NEW logic is to used a fixed amount of time per line,
    # such as 30s, which the user can control.  This is done via
    # max_xwidth_TIME.

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
        account the min_ave_dx and (chosen) n_pts per subplot.
        Returned value is in units of the x-axis array (probably 's').
        **Not currently used**; see max_xwidth_TIME.
        """
        return self.max_n_per_line * self.min_ave_dx

    @property
    def max_xwidth_TIME(self):
        """Calculate the max allowed x-axis interval of a subplot in the 'new'
        manner: based on chosen time per line. If the total duration
        of time points is *less* than the chosen time, use the total
        duration.  Returned value is in units of the x-axis array
        (probably 's')."""

        # total duration of x-axis
        xrange_full = self.minmax_x[1] - self.minmax_x[0]

        return min(xrange_full, self.max_t_per_line)

    @property
    def n_subplots(self):
        """The number of subplots in the data, based on the maximum average
        density of points in an included plobj, the full x-range
        across all plobj, and the desired max number of points per
        subplot."""

        xrange_full = self.minmax_x[1] - self.minmax_x[0]
        m = int(np.ceil(xrange_full/self.max_xwidth_TIME))
        return max([1, m])

    @property
    def is_multiplot(self):
        """Simply an abbreviation (of sorts) for whether we are in a multiplot
        (= multiple subplot) situation, mainly for simpler code
        reading."""
        return (self.n_subplots > 1)
    
    @property
    def n_figs(self):
        """The number of figs to make for the data, based on the amount of
        time per line and the max number of lines per plot."""

        rat = self.n_subplots / self.max_l_per_fig
        return int(np.ceil(rat))

    @property
    def is_multifig(self):
        """Simply an abbreviation (of sorts) for whether we are in a multifig
        (= multiple figures) situation, mainly for simpler code
        reading."""
        return (self.n_figs > 1)

    @property
    def n_subplots_per_fig(self):
        """Return the number of subplots per figure (i.e., per image)."""
        if not(self.is_multiplot) :
            return self.n_subplots
        else:
            return self.max_l_per_fig 

    @property
    def n_subplots_per_fig_rem(self):
        """Return the *remainder* of the number of subplots per figure (i.e.,
        per image)."""
        if not(self.is_multiplot) :
            return 0
        else:
            return self.n_subplots % self.n_subplots_per_fig

    @property
    def all_sub_xwin(self):
        """Make the subplot windows along the x-axis."""
        
        list_win = []
        wmin, max_final = self.minmax_x
        for ii in range(self.n_subplots):
            subwin = [wmin, wmin + self.max_xwidth_TIME]
            list_win.append(subwin)
            wmin = subwin[1]          # set for next iteration
        return list_win

    @property
    def all_range_xlim(self):
        """Range based on min/max x-values for each subplot (basically, expand
        min/max by 1%)."""
        list_win = copy.deepcopy(self.all_sub_xwin)

        list_win = []
        for ii in range(self.n_subplots):
            minval, maxval = self.all_sub_xwin[ii]
            delta = 0.01*(maxval - minval)
            list_win.append([minval-delta, maxval+delta])
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
        delta = self.ypad*(maxval - minval)
        return np.array([minval-delta, maxval+delta])

    @property
    def yh_ibandT(self):
        """Get the ycoord and height for the top iband (interval band), if
        being used."""

        bot, top = self.range_ylim
        delta    = self.ypad * (top - bot) / (1 + 2*self.ypad)
        height   = 0.5 * delta
        return (top - height, height)

    @property
    def yh_ibandB(self):
        """Get the ycoord and height for the bot iband (interval band), if
        being used."""

        bot, top = self.range_ylim
        delta    = self.ypad * (top - bot) / (1 + 2*self.ypad)
        height   = 0.5 * delta
        return (bot, height)

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
                self.list_plobj[ii].ms = DEF_ms # !!!!!! come back to this
            if self.list_plobj[ii].lw == None :
                self.list_plobj[ii].lw = DEF_lw # !!!!!! come back to this
            if self.list_plobj[ii].color == None :
                for jj in range(self.n_plobj):
                    color = 'C{}'.format(jj) 
                    if not(color in self.all_color) :
                        self.list_plobj[ii].color = color
                        break
                if self.list_plobj[ii].color == None :
                    self.list_plobj[ii].color = 'black'
            if self.list_plobj[ii].mec == None : 
                # just mirror main color
                self.list_plobj[ii].mec = self.list_plobj[ii].color

        if self.figsize == None :
            self.figsize_use = (7, 1.0+self.n_subplots_per_fig*1.0)
            # for plot of 'remainder' lines, if applicable
            self.figsize_rem = (7, 1.0+self.n_subplots_per_fig_rem*1.0)
        else:
            self.figsize_use = copy.deepcopy(self.figsize)
            # for plot of 'remainder' lines, if applicable
            tmp = (self.figsize_use[1]-1)
            tmp*= self.n_subplots_per_fig_rem / self.n_subplots_per_fig
            tmp+= 1
            self.figsize_rem = (self.figsize_use[0], tmp)
            if verb > 2 :
                print("   figsize_use:", self.figsize_use)
                print("   figsize_rem:", self.figsize_rem)
      
        if self.is_multifig :
            # make props for *full* figures
            nfull = int(self.n_subplots // self.n_subplots_per_fig )
            for ii in range(nfull):
                self.list_fig_props.append([ii*self.n_subplots_per_fig, 
                                            (ii+1) * self.n_subplots_per_fig,
                                            self.figsize_use])
            # ... and possibly any remainder plot
            nrem = self.n_subplots % self.n_subplots_per_fig
            if nrem :
                self.list_fig_props.append([nfull*self.n_subplots_per_fig,
                                            nfull*self.n_subplots_per_fig+nrem,
                                            self.figsize_rem])
        else:
            self.list_fig_props.append([0, 
                                        self.n_subplots, 
                                        self.figsize_use])

        if len(self.list_fig_props) != self.n_figs :
            print("+* WARN: mismatch, n_figs ({}) != len(list_fig_props ({})"
                  "".format(self.n_figs, len(self.list_fig_props)))

    def make_plot(self, do_show = False, do_save = True):
        """Create the plot"""

        self.prep_plotvals()

        for hh in range(self.n_figs):
            # extract per-figure properties from the prep'ed list:
            # starting and ending subplot (AKA line) selectors, and
            # figsize
            iistart = self.list_fig_props[hh][0]
            iiend   = self.list_fig_props[hh][1]
            iinum   = iiend - iistart
            osize   = self.list_fig_props[hh][2]

            # set output filename, depending on multiplotting or not
            if self.is_multifig :
                obase = ''.join(self.figname.split('.')[:-1])    # basename
                oext  = self.figname.split('.')[-1]              # ext
                oimg  = obase + '_{:03d}'.format(hh+1) + '.' + oext
            else:
                oimg  = self.figname

            # create figure and subplot array
            fff   = plt.figure( oimg, figsize=osize )
            subpl = fff.subplots( iinum, 1)

            list_labels = []

            for ii in range( iistart, iiend ):
                iicount = ii - iistart

                # make 'pp' the object for the subplot in question
                if self.is_multiplot :  pp = subpl[iicount]
                else:                   pp = subpl

                if 1 :
                    pp.axhline(y=0, c='0.8', ls='-', lw=0.5, zorder=0)

                # loop over all plobj and add them
                for jj in range( self.n_plobj ):
                    plobj = copy.deepcopy(self.list_plobj[jj])
                    if not(iicount) :
                        list_labels.append('test_' + str(jj))

                    # treat all plots as slices
                    idx = self.all_sub_slice[ii][jj]
                    sp = pp.plot( plobj.x[idx[0]:idx[1]], 
                                  plobj.y[idx[0]:idx[1]],
                                  color = plobj.color,
                                  ls    = plobj.ls,
                                  lw    = plobj.lw,
                                  marker= plobj.marker,
                                  ms    = plobj.ms,
                                  mec   = plobj.mec,
                                  mew   = plobj.mew,
                                  alpha = plobj.alpha,
                                  label = plobj.label,
                                  fillstyle='full')

                    # obj can have interval bands (ibands)
                    if plobj.add_ibandT :
                        y_iband, h_iband = self.yh_ibandT    # from main obj
                        for bb in range(plobj.n_ibandT):
                            pp.add_patch(mplp.Rectangle(
                                (plobj.xw_ibandT[bb][0], y_iband),
                                plobj.xw_ibandT[bb][1],
                                h_iband,
                                color=plobj.col_ibandT[bb]))

                    if plobj.add_ibandB :
                        y_iband, h_iband = self.yh_ibandB    # from main obj
                        for bb in range(plobj.n_ibandB):
                            pp.add_patch(mplp.Rectangle(
                                (plobj.xw_ibandB[bb][0], y_iband),
                                plobj.xw_ibandB[bb][1],
                                h_iband,
                                color=plobj.col_ibandB[bb]))

                    if not(iicount) and not(jj) :
                        pp.set_title(self.title, fontsize=self.fontsize)

                # make xlabel at bottom of subfig
                if iicount == iinum - 1 :
                    xlbl = pp.set_xlabel(self.xlabel, fontsize=self.fontsize,
                                  loc='left')

                pp.set_xlim(self.all_range_xlim[ii])
                pp.set_ylim(self.range_ylim)

                # thick lines for start/end, to help visualization
                pp.spines['left'].set_linewidth(3)
                pp.spines['right'].set_linewidth(3)

            if 1 :
                plt.legend(ncol=self.n_plobj,
                           loc='upper right', 
                           bbox_to_anchor=(1.0, -0.5 - 0.2*(iinum/6.0)),
                           shadow=True, borderpad=0.4, columnspacing=1.5,
                           borderaxespad=0.1, handletextpad=0.5,
                           handlelength=0.75)
                plt.tight_layout()

            if do_save :
                plt.savefig(oimg, dpi=self.dpi)

            # This block seems to need to come after the save function
            # s.t. it it does not cause the output PDFs to be blank
            if do_show :
                plt.ion()
                plt.show(block=True)
            else:
                plt.ioff()

            # without this, we accumulate a ton of image windows
            # hiding in the Python env:
            plt.close(oimg)

def makefig_phobj_peaks_troughs(phobj, peaks=[], troughs=[],
                                phases = [],
                                upper_env=[], lower_env=[],
                                title='', fname='', retobj=None,
                                add_ibandT = False, add_ibandB = False):
    """A script to plot time series, as well as peaks and/or troughs.  The
idea is to keep the plotting as uniform as possible.

Parameters
----------
phobj : phys_ts_obj
    object with physio time series information
peaks : list
    (opt) 1D list of peak values to include in plot
troughs : list
    (opt) 1D list of trough values to include in plot
phases : np.ndarray
    (opt) 1D array of phase values to include in plot
upper_env : np.ndarray
    (opt) 1D array of upper envelope values to include in plot; has
    same number of time points as the phobj.ts_orig
lower_env : np.ndarray
    (opt) 1D array of lower envelope values to include in plot; has
    same number of time points as the phobj.ts_orig
title : str
    string to include as title for the plot
fname : str
    output file name, which can include path
retobj : retro_obj
    object that contains larger settings information, like fontsize
    and other fine-control things the user can pass along
add_ibandT : bool
    add band of rectangles at the top of plot, reflecting peak intervals
    relative to the median
add_ibandB : bool
    add band of rectangles at the bot of plot, reflecting trough intervals
    relative to the median

Returns
-------
(just creates plots)

    """

    if not(fname) :
        fname = 'physio_calc_plot.pdf'

    if retobj :    fontsize = retobj.font_size
    else:          fontsize = None

    # control alpha of ts and peaks/troughs like this
    if len(phases) :    ts_alpha = 0.5
    else:               ts_alpha = 1.0

    # start figure
    fff = RetroFig(figname=fname,
                   max_n_per_line=5000,
                   title=title,
                   fontsize=fontsize)

    #### !!!!! DOWNSAMPLING TIME SERIES, FOR PURPOSE OF PLOTTING
    #### !!!!! TIME/SPACE!  FIGURE OUT MORE SYSTEMATIC/CONTROLLED WAY
    #### !!!!! OF DOING THIS; done here and below
    # add time series
    ret_plobj1 = RetroPlobj(phobj.tvalues[::10], phobj.ts_orig[::10], 
                            label=phobj.label,
                            alpha=ts_alpha,
                            color='0.5')
    fff.add_plobj(ret_plobj1)

    # a note about plot points: consider matplotlib.markers.TICKUP (2)
    # and matplotlib.markers.TICKDOWN (3) instead of 7 and 6; see
    # https://matplotlib.org/stable/api/markers_api.html
    # ... and this is useful:
    # https://matplotlib.org/stable/gallery/lines_bars_and_markers/marker_reference.html#sphx-glr-gallery-lines-bars-and-markers-marker-reference-py

    # add peaks (maybe)
    if len(peaks) :
        ret_plobj2 = RetroPlobj(phobj.tvalues[peaks],
                                phobj.ts_orig[peaks],
                                label='peaks', 
                                ls='None', marker=7, 
                                ms=5, mec='white', mew=0.02, 
                                color='black',
                                alpha=ts_alpha,
                                add_ibandT=add_ibandT)
        fff.add_plobj(ret_plobj2)

    if len(troughs) :
        ret_plobj3 = RetroPlobj(phobj.tvalues[troughs],
                                phobj.ts_orig[troughs], 
                                label='troughs',
                                ls='None', marker=6, 
                                ms=5, mec='white', mew=0.02,
                                alpha=ts_alpha,
                                color='black',
                                add_ibandB=add_ibandB)
        fff.add_plobj(ret_plobj3)

    # add phases (maybe)
    if len(phases) :
        # scale phase for plotting
        maxts = np.max(phobj.ts_orig)
        mints = np.min(phobj.ts_orig)
        diff  = maxts - mints
        scale_ph = (phases + np.pi)/(2.0*np.pi)*diff + mints
        ret_plobj4 = RetroPlobj(phobj.tvalues[::10], scale_ph[::10], 
                                label='phase (scaled)',
                                alpha=1.0,
                                ls='--',
                                color='black')
        fff.add_plobj(ret_plobj4)

    if len(upper_env) :
        ret_plobj5 = RetroPlobj(phobj.tvalues[::10], upper_env[::10], 
                                label='upper env',
                                alpha=1.0,
                                lw=DEF_lw*2,
                                color='blue')
        fff.add_plobj(ret_plobj5)

    if len(lower_env) :
        ret_plobj5 = RetroPlobj(phobj.tvalues[::10], lower_env[::10], 
                                label='lower env',
                                alpha=1.0,
                                lw=DEF_lw*2,
                                color='red')
        fff.add_plobj(ret_plobj5)



    fff.make_plot()
    

# ==========================================================================

if __name__ == "__main__" :

    print("plot")

    b = np.random.random(2000)
    a = np.arange(len(b))

    d = np.random.random(2000)
    c = np.arange(len(d))/2.

    ret_plobj1 = RetroPlobj(a,b, label='line1')
    ret_plobj2 = RetroPlobj(c,d, ls='None', marker='o', label='dots2')

    fff = RetroFig(max_n_per_line=1000)
    fff.add_plobj(ret_plobj1)
    fff.add_plobj(ret_plobj2)
    fff.make_plot()


    sys.exit(0)
