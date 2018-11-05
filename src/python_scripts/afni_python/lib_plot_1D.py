author    = "PA Taylor (NIMH, NIH)"
#version   = "1.0"; date  = "Oct 24, 2018"
# + birth
#
version   = "1.2345"; date  = "Nov 1, 2018"
# + [PT] now a working beta version, including boxplots
#
# =================================================================

import sys
import os
import matplotlib.pyplot  as plt
from   matplotlib         import rcParams, gridspec
from   matplotlib.ticker  import AutoMinorLocator
import matplotlib.patches as matpat
import numpy              as np
import lib_afni1D         as LAD
import lib_apqc_io        as laio
import afni_util          as au

MAXLEN = 10**7     # can adjust if that is ever necessary!
EPS    = 10**(-14)

# =====================================================================
# =====================================================================

# Note: current size limit of plots is 10**6 values in a column, if
# using create_xarrs via "-xvals ..."

def make_1dplot_figure(bf):
    fff = plt.figure(bf.title, figsize=bf.figsize)

    rcParams['font.family'] = bf.fontfamily
    rcParams['axes.linewidth'] = 0.5         # de-emphasize
    #plt.xkcd() # ha!

    # we can add in one summary boxplot per subj 
    if bf.boxplot_on :
        ncol = 2
        a, subpl = plt.subplots( bf.nsub, ncol, 
                                 gridspec_kw = {'width_ratios':[12, 1],
                                 'wspace':0.05},
                                 sharey='row',
                                 squeeze=True,
                                 figsize=bf.figsize )
    else:
        ncol = 1
        a, subpl = plt.subplots( bf.nsub, ncol, 
                                 figsize=bf.figsize )

    for i in range(bf.nsub):
        ss = bf.all_subs[i]
        ii = i+1

        # just relabel, need to account for different cases of what
        # subpl is and what its shape is (if it even has a shape!
        if bf.boxplot_on :
            if bf.nsub > 1:
                pp = subpl[i,0]
            else:
                pp = subpl[0]
        else:
            if bf.nsub > 1:
                pp = subpl[i]
            else:
                pp = subpl

        # ----------------- Main plot: time series ---------------------

        if bf.ncensor : 
            xoffset = 0.5 * bf.censor_width
            for cc in range(bf.ncensor):
                pp.add_patch(matpat.Rectangle( (bf.censor_arr[cc] - xoffset, 
                                                ss.ylim[0]),
                    width=bf.censor_width,
                    height=(ss.ylim[1] - ss.ylim[0]),
                    facecolor=bf.censor_RGB,
                    lw=0, edgecolor=None, alpha=None) )

        if ss.censor_hline : 
            pp.axhline( y=ss.censor_hline, 
                        c=laio.DEF_censor_hline_RGB, 
                        ls=':', lw=1)

        if bf.see_xax : 
            pp.axhline(y=0, c='0.6', ls='-', lw=0.5)
            #plt.axhline(y=0, c='0.5', ls=':', lw=0.75)

        # the actual plot
        sp = pp.plot( ss.x, ss.y, 
                      color = ss.color,
                      lw=2 )

        pp.set_xlim(ss.xlim)
        pp.set_ylim(ss.ylim)

        pp.set_xlabel(ss.xlabel, fontsize=bf.fontsize)
        pp.set_ylabel(ss.ylabel, fontsize=bf.fontsize)

        # get ylabels aligned horizontally
        pp.get_yaxis().set_label_coords(-0.1, 0.5)
        print("++ Plotting: {}".format(ss.ylabel))

        pp.xaxis.set_minor_locator(AutoMinorLocator(5))
        pp.yaxis.set_minor_locator(AutoMinorLocator(2))
        pp.tick_params( axis='both', which='minor', direction='in', color='0.5',
                        bottom=True, left=True, right=True ) #, top=True )
        pp.tick_params( axis='both', which='major', direction='in', color='0.5',
                        bottom=True, left=True, right=True ) #, top=True )
        pp.spines['bottom'].set_color('0.5')
        pp.spines['top'   ].set_color('0.5')
        pp.spines['left'  ].set_color('0.5')
        pp.spines['right' ].set_color('0.5')

        # only show tick labels at very bottom
        if i < bf.nsub-1 :
            nlabs = len(pp.get_xticklabels())
            pp.set_xticklabels(['']*nlabs)

        if bf.title and not(i):
            # cheating with title because tight layout doesn't know about
            # suptitle
            pp.set_title(bf.title, fontsize=bf.fontsize)

        # ----------------- Optional plot: boxplot ---------------------

        if bf.boxplot_on :
            if bf.nsub > 1:
                qq = subpl[i,1]
            else:
                qq = subpl[1]

            if ss.censor_hline : 
                qq.axhline( y=ss.censor_hline, 
                             c=laio.DEF_censor_hline_RGB, 
                             ls=':', lw=1)

            if bf.see_xax : 
                qq.axhline(y=0, c='0.6', ls='-', lw=0.5)
                #plt.axhline(y=0, c='0.5', ls=':', lw=0.75)

            # actual boxplot
            sq = qq.boxplot( ss.y,
                             widths=0.1,
                             sym='.',
                             notch=0, 
                             patch_artist=True)
            # fun parameter-setting for boxplot
            SETLW = 1.
            MARKSIZE1 = 8
            MARKSIZE2 = 5
            flilines = sq['fliers']
            for line in flilines:
                line.set_color(ss.color)
                line.set_markersize(MARKSIZE1)
            medlines = sq['medians']
            for line in medlines:
                line.set_color('0.8')
                line.set_linewidth(SETLW*1.25 )
            boxlines = sq['boxes']
            for line in boxlines:
                line.set_color(ss.color)
            plt.setp(sq['fliers'],   marker='.', mew=0.3, mec='k', mfc=ss.color, 
                                     color=ss.color, ms=MARKSIZE2)
            plt.setp(sq['whiskers'], color=ss.color, linestyle='-', lw=SETLW )
            plt.setp(sq['caps'],     color=ss.color, linestyle='-', lw=SETLW )

            # no xticks/labels
            if 1:
                qq.set_xticks([])

            # stuff for y-axis ticks (on) and labels (off)
            qq.yaxis.set_minor_locator(AutoMinorLocator(2))
            qq.tick_params( axis='y', which='minor', direction='in', color='0.5',
                            bottom=True, left=True, right=True ) #, top=True )
            qq.tick_params( axis='y', which='major', direction='in', color='0.5',
                            bottom=True, left=True, right=True,
                            labelleft=False) #, top=True )
            pp.spines['bottom'].set_color('0.5')
            pp.spines['top'   ].set_color('0.5')
            qq.spines['left'  ].set_color('0.5')
            qq.spines['right' ].set_color('0.5')


    # finishing touches

    if bf.layout == 'nospace' :
        fff.subplots_adjust( wspace=0.1, hspace=0.1 )
    #elif bf.layout == 'tight':
    #    plt.tight_layout()

    plt.savefig( bf.fname, dpi=bf.dpi, facecolor=bf.bkgd_color,
                 bbox_inches='tight')
    print("++ Done! Figure created:\n\t {}".format(bf.fname))

    return 0

# --------------------------------------------------------------------------------

def populate_1dplot_fig(iopts):

    bigfig = laio.figplobj()
    bigfig.set_dpi( iopts.dpi )
    bigfig.set_layout( iopts.layout )
    bigfig.set_title( iopts.title )
    bigfig.set_fname( iopts.prefix )
    bigfig.set_fontsize( iopts.fontsize )
    bigfig.set_fontfamily( iopts.fontfamily )
    bigfig.set_fontstyles( iopts.fontstyles )
    bigfig.set_censor_arr( iopts.censor_arr )
    bigfig.set_censor_width( iopts.censor_width )
    bigfig.set_censor_RGB( iopts.censor_RGB )
    bigfig.set_censor_hline( iopts.censor_hline )
    bigfig.set_boxplot( iopts.boxplot_on )

    for i in range(iopts.ndsets):

        # can reverse the order of plotting, relative to input
        if iopts.reverse_order : 
            ii = iopts.ndsets - i - 1
        else:
            ii = i

        ss = laio.subplobj()

        ss.set_color(iopts.get_color(i))

        ss.set_x( iopts.all_x )
        ss.set_xlim()
        # only put xlabel on bottom panel
        if i < iopts.ndsets - 1:
            ss.set_xlabel("")
        else:
            ss.set_xlabel( iopts.xlabel )

        ss.set_y( iopts.all_y[ii] )

        # if not a null set, then there is one for each subj
        if iopts.censor_hline : 
            ss.set_censor_hline(iopts.censor_hline[ii] )

        if iopts.onescl :
            delta = iopts.all_ymax - iopts.all_ymin
            bb = iopts.all_ymin - 0.1 * delta
            tt = iopts.all_ymax + 0.1 * delta
            ss.set_ylim([bb, tt])
        else:
            ss.set_ylim()
        ss.set_ylabel( iopts.ylabels[ii] )

        bigfig.add_sub(ss)

    # depends on final number of subjects
    bigfig.set_figsize( iopts.figsize )

    return bigfig

# --------------------------------------------------------------------------------

def populate_1dplot_arrays(iopts):

    iopts.create_all_y()
    ok = iopts.check_dims_all_y() # check with number ylabels

    iopts.create_all_x()
    ok2 = iopts.check_dims_all_x_ys()

    # once arrays are read in, so the total number is known, sort out
    # color table: will either be what user entered or default palette
    # if ndsets>1; otherwise, just a black line.
    iopts.check_color_table()

    # once arrays are read in (particularly x values), make the censor list
    iopts.set_censor_arr()

    # once arrays are read in, check if the number censor hlines is
    # good: it must be either 0 (no lines added), 1 (same line added
    # to all) and/or match the number of input yfiles
    iopts.check_censor_hlines()

    return 0

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

class apqc_1dplot_plopts:

    Nplots  = 0
    ylabels = []
    yscales = []
    xlabel  = ""
    title   = ""
    prefix  = "OUTPUT_1D.jpg"
    dpi     = 100

    def add_ylabel(self, ylabel):
        self.ylabels.append(ylabel)
        self.Nplots+= 1

    def add_yscale(self, bot, top):
        self.yscales.append([bot, top])

    def set_xlabel(self, xlabel):
        self.xlabel = xlabel

    def set_title(self, title):
        self.title = title

    def set_prefix(self, prefix):
        self.prefix = prefix

    def set_dpi(self, dpi):
        self.dpi = dpi

    # check lots of properties for consistency
    def check_all(self):
        MISS = 0
        if Nplots != len(self.ylabels):
            print("mismatched Nplots-len(ylabels)")
            MISS+=1
        if Nplots != len(self.yscales):
            print("mismatched Nplots-len(yscales)")
            MISS+=1
        if not(self.ylabels) :
            print("missing: ylabels-- things to plot!")
            MISS+=1
        return MISS

# --------------------------------------------------------------------------
# -------------------------------------------------------------------------


########################### not used (and broken) #############################
####################### maybe will revisit this, if necessary #################
#
#def make_locations_ticks(N, lims):
#    '''N is number of (major) ticks to have, and lims is an array of upper
#and lower limits.  There will be 4 minor ticks placed throughout.'''
#
#    nice_deltas = [ 1, 2, 2.5, 3, 4, 5, 7.5, 10 ] 
#    Nnice       = len(nice_deltas)
#    
#
#    if N <= 0 :
#        sys.exit("** ERROR: need > 0 tick to set locations!")
#    if lims[0] > lims[1] :
#        sys.exit("** ERROR: lower limit greater than top one?!")
#    elif lims[0] == lims[1] :
#        out = [ -0.1, 0.1 ] # just pick some interval
#
#    fullrange = lims[1] - lims[0]
#    delta0 = float(fullrange)/N
#
#    INCLUDES_ZERO = (np.sign(lims[0])*np.sign(lims[1]) <= 0 )
#
#    if not(INCLUDES_ZERO) :
#        pp = np.round(np.log10(np.abs(delta0)))
#        nd_scal = (10**pp) * np.array(nice_deltas)
#        print("PP and ND_SCALE:  {} and {}".format(pp, nd_scal))
#        mindiff = 10**(pp+5)
#        indmin  = Nnice
#        for i in range(Nnice):
#            diff = nd_scal[i] - delta0
#            print("[{}] {} ---> DIFF: {}".format(i, nd_scal[i], diff))
#            if ( diff > 0 ) and ( diff < mindiff ) :
#                mindiff = diff
#                indmin = i
#        print("delta0 = {} and chose mindiff = {}, delta: {}".format(delta0, mindiff, nd_scal[indmin]))
#
#def adapt_num_to_sigfigs(N, nsigfig=2, rtype='RDOWN'):
#    '''Take a number 'N' (can be int or float) and return it rounded to
#the nearest 'nsigfig' significant figures. The default methodology at
#the moment of rounding is rtype='RDOWN', but this can be adjusted to
#either 'RUP' or 'ROUND', where
#
#    RDOWN : round magnitude down
#    RUP   : round magnitude up
#    ROUND : round in a rounding manner
#
#The output type will float if there are nonzero values to the right of
#the decimal point (to 14 decimals precision); otherwise, int.
#
#    '''
#
#    # when pushing around numbers, how many decimals to keep in the
#    # sci not
#    ndec = nsigfig - 1 
#
#    a     = '{:E}'.format(abs(N))
#    nsign = np.sign(N)
#    b     = a.split('E')
#    npow  = int(b[1]) # will be an integer
#
#    c = float(b[0]) * 10**ndec
#    if rtype == 'RDOWN' :
#        nbase = int(np.floor(c))
#    elif rtype == 'RUP' :
#        nbase = int(np.ceil(c))
#    elif rtype == 'ROUND' :
#        nbase = int(np.round(c, decimals=0))
#
#    nmove = npow - ndec
#    print("NBASE: {}, npow {}, ndec {}, nmove {}".format(nbase, npow, ndec, nmove))
#    sbase = str(nbase)  # string form of integer, need to put decimal
#                        # back in
#    lbase = list(sbase) # list of chars
#    if nmove < 0 :
#        nchar = len(lbase)
#        if abs(nmove) >= nchar:
#            npad = abs(nmove) - nchar
#            out = ''.join(['0.'] + ['0']*npad + lbase)
#        else:
#            out = ''.join(lbase.insert(nmove, '.'))
#
#    elif nmove == 0 :
#        out = sbase
#    elif nmove > 0 :
#        nrhs = len(sbase)
#        stmp = sbase + ( '0'*(nmove - nrhs) )
#        ltmp = [stmp[0]]
#        for i in range(nmove):
#            ltmp.append(stmp[i+1])
#        ltmp.append(stmp[nmove:])
#        out = ''.join(ltmp)
#
#    out = float(out)
#    # check whether it is really an integer or not
#    remaind = out % 1
#    if np.abs(remaind) < EPS:
#        out = int(out)
#        if out:
#            return nsign*out
#        else:
#            return out
#    else:
#        return nsign * out
#
#
#
#
#
#
#
#
#
