#!/usr/bin/env python3

import sys, copy
import numpy as np
import matplotlib.pyplot      as     plt
from   matplotlib.collections import PatchCollection as MPC
import matplotlib.patches     as     mplp
import matplotlib.cm          as     mplcm

from   afnipy import afni_base as    BASE

DEF_max_n = 1000                     # def npts per subplot (not used now)
DEF_lw    = 0.75                     # def linewidth in plot
DEF_ms    = 1.50                     # def marker size in plot

PY_VER    = sys.version_info.major   # Python major version

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
                 img_axhline = None, # horizontal line, like 0 or ts median
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

        self.img_axhline  = img_axhline        # flt/str, value or keyword
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
                 ylabel  = '',
                 figsize = [],
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
        self.figsize_use   = []                 # 2-tuple, fig dims
        self.figsize_rem   = []                 # 2-tuple, fig dims for remndr
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
        if not(self.is_multifig) :
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

        if len(self.figsize) == 0 :
            self.figsize_use = (7, 1.0+self.n_subplots_per_fig*1.0)
            # for plot of 'remainder' lines, if applicable
            self.figsize_rem = (7, 0.5+self.n_subplots_per_fig_rem*1.0)
        else:
            self.figsize_use = copy.deepcopy(self.figsize)
            # for plot of 'remainder' lines, if applicable
            tmp = (self.figsize_use[1]-1)
            tmp*= self.n_subplots_per_fig_rem / self.n_subplots_per_fig
            tmp+= 1
            self.figsize_rem = (self.figsize_use[0], tmp)
            if self.verb > 2 :
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

                # loop over all plobj and add them
                for jj in range( self.n_plobj ):
                    plobj = copy.deepcopy(self.list_plobj[jj])
                    if not(iicount) :
                        list_labels.append('test_' + str(jj))

                    # plot a horizontal guide line, e.g., median or 0 line
                    if plobj.img_axhline != None :
                        if plobj.img_axhline == 'MEDIAN' :
                            yval = plobj.med_ts_orig
                            ylab = 'ts med'
                        else:
                            yval = plobj.img_axhline
                            ylab = None
                        pp.axhline(y=yval, c='0.8', ls='-', lw=0.5, zorder=0,
                                   label=ylab)

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
                                  loc='left', labelpad=4)

                # put a plot-wide ylabel at the top
                if PY_VER > 2 and self.ylabel :
                    fff.supylabel(self.ylabel, y=0.9, va='top', 
                                  fontsize=self.fontsize)

                pp.set_xlim(self.all_range_xlim[ii])
                pp.set_ylim(self.range_ylim)

                # thick lines for start/end, to help visualization
                pp.spines['left'].set_linewidth(3)
                pp.spines['right'].set_linewidth(3)

            if 1 :
                # first make layout tight, then place single-row
                # legend, that should now fit nicely
                plt.tight_layout()
                plt.legend(ncol=self.n_plobj,
                           fontsize=self.fontsize,
                           #loc='upper right', 
                           #bbox_to_anchor=(1.0, -0.5 - 0.2*(iinum/6.0)),
                           loc='lower right',
                           bbox_to_anchor=(0.98, 0.01),
                           bbox_transform=plt.gcf().transFigure,
                           shadow=True, borderpad=0.4, columnspacing=1.5,
                           borderaxespad=0.1, handletextpad=0.5,
                           handlelength=0.75)
                #plt.tight_layout()

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
                                add_ibandT = False, add_ibandB = False,
                                img_axhline = 'MEDIAN',
                                verb=0):
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
img_axhline : str or float
    plot a horizontal line in the plot; can be either a number, or a keyword
    like 'MEDIAN', which will get median of phobj time series.

Returns
-------
(just creates plots)

    """

    if not(fname) : fname = 'physio_calc_plot.pdf'

    # functionality for one ylabel to rule them all
    if PY_VER > 2:  fig_ylabel = 'physio signal'
    else:           fig_ylabel = ''

    # start figure, either with retobj options or more simply
    if retobj :    
        fff = RetroFig( figname        = fname,
                        max_n_per_line = 5000,
                        title          = title,
                        figsize        = retobj.img_figsize,
                        fontsize       = retobj.img_fontsize,
                        max_t_per_line = retobj.img_line_time,
                        ylabel         = fig_ylabel,
                        verb           = verb,
        )
    else: 
        # simple fig, all defaults
        fff = RetroFig( figname=fname,
                        max_n_per_line=5000,
                        title=title,
                        verb=verb )

    # control alpha of ts and peaks/troughs like this
    if len(phases) :    ts_alpha = 0.5
    else:               ts_alpha = 1.0

    # can plot lower density of points in lines
    istep = phobj.img_arr_step

    if img_axhline == 'MEDIAN' :
        img_axhline = phobj.stats_med_ts_orig

    ret_plobj1 = RetroPlobj(phobj.tvalues[::istep], phobj.ts_orig[::istep], 
                            label=phobj.label,
                            alpha=ts_alpha,
                            color='0.5',
                            img_axhline =img_axhline)
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
        ret_plobj4 = RetroPlobj(phobj.tvalues[::istep], scale_ph[::istep], 
                                label='phase (scaled)',
                                alpha=1.0,
                                lw=DEF_lw*1.5,
                                color='green')
        fff.add_plobj(ret_plobj4)

        if len(troughs):
            # Bonus for resp phases: vis guide line
            ret_plobj4a = RetroPlobj(phobj.tvalues[::istep], 
                                  np.ones(len(phobj.tvalues[::istep]))*maxts,
                                  #label='$\pm\pi$',
                                  alpha=0.5,
                                  lw=DEF_lw*0.5,
                                  color='green')
            fff.add_plobj(ret_plobj4a)
            ret_plobj4b = RetroPlobj(phobj.tvalues[::istep], 
                                  np.ones(len(phobj.tvalues[::istep]))*mints,
                                  alpha=0.5,
                                  lw=DEF_lw*0.5,
                                  color='green')
            fff.add_plobj(ret_plobj4b)

    if len(upper_env) :
        ret_plobj5 = RetroPlobj(phobj.tvalues[::istep], upper_env[::istep], 
                                label='upper env',
                                alpha=1.0,
                                lw=DEF_lw*2,
                                color='blue')
        fff.add_plobj(ret_plobj5)

    if len(lower_env) :
        ret_plobj5 = RetroPlobj(phobj.tvalues[::istep], lower_env[::istep], 
                                label='lower env',
                                alpha=1.0,
                                lw=DEF_lw*2,
                                color='red')
        fff.add_plobj(ret_plobj5)



    fff.make_plot()
    
# --------------------------------------------------------------------------

def makefig_ft_bandpass_magn(X, Xfilt,
                             delta_f, idx_ny,
                             title='TITLE', fname='FNAME',
                             label='', retobj=None,
                             verb=0):
    """
Parameters
----------
X : np.ndarray
    1D array (probably complex) of unfiltered freq spectrum values;
    the abs value of this is plotted
X : np.ndarray
    1D array (probably complex) of the bandpassed freq spectrum values;
    the abs value of this is plotted
idx_ny : int
    the index value of the Nyquist frequency
delta_f : float
    value of the steps along the abscissa (freq axis); the original
    freq sampling freq divided by N time points of ts_orig.
title : str
    string to include as title for the plot
fname : str
    output file name, which can include path
label : str
    label for the time series, like 'card' or 'resp'
retobj : retro_obj
    object that contains larger settings information, like fontsize
    and other fine-control things the user can pass along

Returns
-------
(just creates plots)

"""

    if not(retobj) :
        print("** ERROR: need retobj in this function (bandpass plot)")
        sys.exit(7)

    if not(fname) :
        fname = 'physio_calc_plot_FT.pdf'

    # functionality for one ylabel to rule them all
    if PY_VER > 2:  fig_ylabel = 'freq spectrum magn'
    else:           fig_ylabel = ''

    N = len(X)
    # make abscissa
    fvalues = np.arange(idx_ny) * delta_f
    max_f   = retobj.img_bp_max_f
    max_idx = int(max_f / delta_f)
    istep   = 1

    # start figure 
    fff = RetroFig( figname        = fname,
                    title          = title,
                    figsize        = retobj.img_figsize,
                    fontsize       = retobj.img_fontsize,
                    max_t_per_line = max_f,
                    xlabel         = 'freq (Hz)',
                    ylabel         = fig_ylabel,
                    verb           = verb,
    )

    # add unfiltered magnitude
    ret_plobj1 = RetroPlobj(fvalues[:max_idx:istep], 
                            np.abs(X[:max_idx:istep]), 
                            label='unfilt',
                            alpha=1,
                            color='0.5')
    fff.add_plobj(ret_plobj1)

    # add bandpassed magnitude
    ret_plobj2 = RetroPlobj(fvalues[:max_idx:istep], 
                            np.abs(Xfilt[:max_idx:istep]), 
                            label='bandpassed',
                            alpha=1,
                            lw=2,
                            color='black')
    fff.add_plobj(ret_plobj2)

    fff.make_plot()

# ---------------------------------------------------------------------------
# dump a temp text file and plot RVT regressors, if being used

def plot_regressors_rvt(retobj, label, ext='svg'):
    """


"""


    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix
    nvol   = retobj.vol_nv
    verb   = retobj.verb
    nrvt   = phobj.n_regress_rvt
    
    # make the filename (final image)
    fname = 'regressors_rvt_' + label + '.{}'.format(ext)
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # make the data file (temporary file)
    ftmp = '__tmp' + label + '_rvt_regressors.dat'
    if prefix  :  ftmp = prefix + '_' + ftmp
    if odir :     ftmp = odir + '/' + ftmp

    title = 'Process {} data: RVT regressors'.format(label)

    # put data+labels into simple forms for writing; initialize objs
    data_shape = (nvol, nrvt)
    data_arr   = np.zeros(data_shape, dtype=float)
    data_lab   = ['LABEL'] * nrvt

    # process any/all RVT regressors
    for ii in range(nrvt):
        key  = phobj.regress_rvt_keys[ii]
        ylab = key + '\\n' + '$\Delta={}$'.format(retobj.rvt_shift_list[ii])

        data_lab[ii] = ylab
        data_arr[:,ii] = phobj.regress_dict_rvt[key]

    # --------------------- write tmp data file ---------------------

    # open the file and write the header/start
    fff = open(ftmp, 'w')
    # write data
    for ii in range(data_shape[0]):
        for jj in range(data_shape[1]):
            fff.write(" {:6.4f} ".format(data_arr[ii,jj]))
        fff.write('\n')
    # le fin: close and finish
    fff.close()

    # --------------------- make image of rvt data -----------------------

    par_dict = {
        'ftmp'    : ftmp,
        'fname'   : fname,
        'title'   : title,
        'all_lab' : ' '.join(['\''+lab+'\'' for lab in data_lab])
    }

    cmd = '''
    1dplot.py                                                            \
        -reverse_order                                                   \
        -infiles        {ftmp}                                           \
        -ylabels        {all_lab}                                        \
        -xlabel         "vol index"                                      \
        -title          "{title}"                                        \
        -prefix         "{fname}"
    '''.format(**par_dict)
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()

    # --------------- clean up tmp file
    cmd    = '''\\rm {ftmp}'''.format(**par_dict)
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()

    print("++ Made plot of {}-based RVT regressors: {}".format(label, fname))


    return 0

# ---------------------------------------------------------------------------
# dump a temp text file and plot phys regressors, if being used

def plot_regressors_phys(retobj, ext='svg'):
    """


"""

    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    odir   = retobj.out_dir
    prefix = retobj.prefix
    nvol   = retobj.vol_nv
    verb   = retobj.verb
    
    # make the filename (final image)
    fname = 'regressors_phys.{}'.format(ext)
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # make the data file (temporary file)
    ftmp = '__tmp__' + 'regressors_phys.dat'
    if prefix  :  ftmp = prefix + '_' + ftmp
    if odir :     ftmp = odir + '/' + ftmp

    # make ylabels and title; NB: here and below, we focus on slice 0
    idx_sli   = 0  
    all_label = [lab for lab in list(retobj.data.keys()) \
                 if retobj.data[lab] != None ]
    nlabel = len(all_label)
    lll    = ', '.join(all_label)
    title  = 'Process ({}) data: '.format(lll)
    title += 'physio regressors, slice {}'.format(idx_sli)

    # build up count of number of regressors
    nreg   = 0
    for hh in range(nlabel):
        label = all_label[hh]
        phobj = retobj.data[label]        # simplify coding below
        nreg += phobj.n_regress_rvt
        nreg += phobj.n_regress_phys

    # put data+labels into simple forms for writing; initialize objs
    data_shape = (nvol, nreg)
    data_arr   = np.zeros(data_shape, dtype=float)
    data_lab   = ['LABEL'] * nreg

    # -------------------- get regressors for [0] slice -------------------

    idx_sli = 0  
    # count number of regressors per slice, as added
    cc = 0 
    for hh in range(nlabel):
        label = all_label[hh]
        phobj = retobj.data[label]        # simplify coding below
        # process any/all phys regressors
        for ii in range(phobj.n_regress_phys):
            keyA = phobj.regress_rvt_phys[ii]
            keyB = phobj.regress_dict_phys[keyA][idx_sli][0]
            data_lab[cc]   = keyB.split('.')[-1] + '\\n' + keyA
            data_arr[:,cc] = phobj.regress_dict_phys[keyA][idx_sli][1]
            cc+= 1

    # --------------------- write tmp data file ---------------------

    # open the file and write the header/start
    fff = open(ftmp, 'w')
    # write data
    for ii in range(data_shape[0]):
        for jj in range(data_shape[1]):
            fff.write(" {:6.4f} ".format(data_arr[ii,jj]))
        fff.write('\n')
    # le fin: close and finish
    fff.close()

    # --------------------- make image of rvt data -----------------------

    par_dict = {
        'ftmp'    : ftmp,
        'fname'   : fname,
        'title'   : title,
        'all_lab' : ' '.join(['\''+lab+'\'' for lab in data_lab])
    }

    cmd = '''
    1dplot.py                                                            \
        -reverse_order                                                   \
        -infiles        {ftmp}                                           \
        -ylabels        {all_lab}                                        \
        -xlabel         "vol index"                                      \
        -title          "{title}"                                        \
        -prefix         "{fname}"
    '''.format(**par_dict)
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()

    # --------------- clean up tmp file
    cmd    = '''\\rm {ftmp}'''.format(**par_dict)
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()

    print("++ Made plot of {}-based RVT regressors: {}".format(label, fname))


    return 0


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
