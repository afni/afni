author    = "PA Taylor (NIMH, NIH)"
#version   = "1.0"; date  = "Oct 24, 2018"
# + birth
#
#version   = "1.2345"; date  = "Nov 1, 2018"
# + [PT] now a working beta version, including boxplots
#
#version   = "1.3"; date  = "Nov 28, 2018"
# + [PT] fixed censor_hline stuff- now is list of floats, except
#        at single plot level, when is just a float
#
#version   = "1.31"; date  = "Dec 5, 2018"
# + [PT] at the moment, numpy isn't actually used here, so removing
#        that import-- I would expect it to return *someday*, though.
#
#version   = "1.4"; date  = "Jan 14, 2019"
# + [PT] calc 1D-plot LW from Npts
#
#ver = '1.5' ; date = 'Feb 16, 2020' 
# + [PT] add in ability for -censor_hline to contain a 'NONE' argument 
#        ... bc that is useful as a placeholder when some images have
#        censor vals and others don't
#
#ver = '1.9' ; date = 'June 17, 2020' 
# [PT] add in legend, legend_label and legend_loc functionality
#
#ver = '2.0' ; date = 'April 22, 2020'
# [PT] can now wrap y-axis labels, because they might get long
#    + couple ways of trying: purely by length, or with some 'logic'
#
ver = '2.1' ; date = 'May 11, 2020'
# [PT] replace str.isnumeric() with str.isdigit(), for backward
#      compatability with Python v2.7.  Grrr.
#
# =================================================================

import sys
import os
import matplotlib.pyplot  as plt
from   matplotlib         import rcParams, gridspec
from   matplotlib.ticker  import AutoMinorLocator
import matplotlib.patches as matpat
from afnipy import lib_afni1D         as LAD
from afnipy import lib_apqc_io        as laio
from afnipy import afni_util          as au

MAXLEN = 10**7     # can adjust if that is ever necessary!
EPS    = 10**(-14)

MULTIRUN_SHADE = '0.95'

# =====================================================================
# =====================================================================

def long_string_wrap_by_char(x, maxlen = 7):
    '''Sometimes labels are long, and you might want to wrap them to be at
    most a certain length per line (e.g., long labels for regressors
    of interest).  If 'x' has a newline char in that, that is OK; we
    will split with that char, and build based on the rest of the
    pieces.

    This is the more complicated version of wrapping: pay attention to
    contents, chopping into possibly ragged lines.

    This is a helper function for: long_string_wrap().

    Params
    ------

    x      : input str
    maxlen : max number of chars per line (inclusively)

    Return
    ------

    z      : str, version of x with '\n' inserted to obey wrapping len

    '''

    xlen   = len(x)

    # simple cases
    if type(x) != str : return '', 0
    if xlen <= maxlen : return x, 1

    xlist  = x.split('\n')
    zsplit = []
    
    for y in xlist:
    
        ybag = []
        the_str = y
        count = 0
        while len(the_str) :
            full_len = len(the_str)-1
            for i in range(full_len):
                # things that partition at [i]
                if ( the_str[i] == '_' ) :
                    ybag.append(the_str[:i])
                    ybag.append(the_str[i])
                    the_str = the_str[i+1:]
                    break
                # things that split from [i]
                elif ( i>0 and the_str[i].isupper() and \
                       the_str[i+1].islower())              or \
                    ( i>0 and the_str[i] == '#' ) :
                    ybag.append(the_str[:i])
                    the_str = the_str[i:]
                    break
                # things that split from [i+1]
                elif ( the_str[i].isdigit() and \
                       not(the_str[i+1].isdigit()))        or \
                     ( not(the_str[i].isdigit()) and \
                       not(the_str[i] == '#') and      \
                       the_str[i+1].isdigit())  :
                    ybag.append(the_str[:i+1])
                    the_str = the_str[i+1:]
                    break
            if i == full_len-1 :
                ybag.append(the_str)
                the_str = ''
                break
                    
            count+= 1
            if count == 1000:
                print("+* Warn: went into infinite (well, 1000) loop?\n"
                      "   Stopping, and using simple split output")
                return long_string_wrap_by_len(x, maxlen = maxlen)

        # forward pass to glue
        ny   = len(ybag)
        idx  = 0

        while idx < ny :
            word = ''
            lw   = len(word)
            i    = idx
            while i < ny:
                if lw < maxlen:
                    lnew = len(ybag[i])
                    if lw + lnew <= maxlen :
                        word+= ybag[i]
                        i+= 1
                    elif lnew > maxlen :
                        word+= ybag[i][:maxlen-lw]
                        ybag[i] = ybag[i][maxlen-lw:]
                        break
                    else:
                        break
                    lw = len(word)
                else:
                    break
            idx = i

            zsplit.append(word)

    #print("DONE the bag:", ybag)
    #print("DONE the str:", the_str)
    #print("DONE zsplit:", zsplit)

    nlines = len(zsplit)
    z      = '\n'.join(zsplit)

    return z, nlines

def long_string_wrap_by_len(x, maxlen = 7):
    '''Sometimes labels are long, and you might want to wrap them to be at
    most a certain length per line (e.g., long labels for regressors
    of interest).  If 'x' has a newline char in that, that is OK; we
    will split with that char, and build based on the rest of the
    pieces.

    This is the simple version of wrapping: ignore any contents, just
    chop into constant length lines.

    This is a helper function for: long_string_wrap().

    Params
    ------

    x      : input str
    maxlen : max number of chars per line (inclusively)

    Return
    ------

    z      : str, version of x with '\n' inserted to obey wrapping len

    '''

    xlen   = len(x)

    # simple cases
    if type(x) != str : return '', 0
    if xlen <= maxlen : return x, 1

    xlist  = x.split('\n')
    zsplit = []
    
    for y in xlist:
        # basic, split every 'maxlen' chars
        ylen   = len(y)
        npiece = ylen // maxlen

        for nn in range(npiece):
            zsplit.append( y[nn*maxlen:(nn+1)*maxlen] )
        if ylen % maxlen :
            zsplit.append( y[npiece*maxlen:] )
        
    nlines = len(zsplit)
    z      = '\n'.join(zsplit)

    return z, nlines

def long_string_wrap(x, maxlen = 7, verb=0):
    '''Main function wrapping long strings into multiple shorter ones,
    inserting newline chars.  This calls a couple ways of trying to
    shorten, and then picks the "best".
    '''

    x_bylen,  n_bylen  = long_string_wrap_by_len( x, maxlen=maxlen )
    x_bychar, n_bychar = long_string_wrap_by_char( x, maxlen=maxlen )

    if verb > 1 :
        bord = "-"*maxlen
        print("++ String, wrapped by char ({} lines):\n{}\n"
              "{}\n{}".format(n_bychar, bord, x_bychar, bord))
        print("++ String, wrapped by len ({} lines):\n{}\n"
              "{}\n{}".format(n_bylen, bord, x_bylen, bord))

    if n_bychar <= n_bylen :
        if verb:
            print("++ Output: wrapped by char")
        return x_bychar
    else:
        if verb:
            print("++ Output: wrapped by len")
        return x_bylen

# ---------------------------------------------------------------------

# Note: current size limit of plots is 10**6 values in a column, if
# using create_xarrs via "-xvals ..."

# 'bf' stands for 'bigfig' (figplobj); 'ss' for single subject
# (subplobj).
def make_1dplot_figure(bf):
    '''Input 'bf' is an instance of the figplobj object.  It is made up of
lots of individual subject 'ss' instances of the subplobj object.

    '''

    fff = plt.figure(bf.title, figsize=bf.figsize)

    rcParams['font.family'] = bf.fontfamily
    rcParams['axes.linewidth'] = 0.5         # de-emphasize
    #plt.xkcd() # ha!

    # we can add in one summary boxplot per subj 
    # at the moment, can't have one_graph with boxplot
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
        a, subpl = plt.subplots( bf.ngraph, ncol, 
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
            if bf.ngraph > 1:
                pp = subpl[i]
            else:
                pp = subpl

        # ----------------- Main plot: time series ---------------------

        if bf.npatch :
            rsum = ss.x[0]
            for vv in range(0, bf.npatch-1, 2):
                rsum+= bf.patch_arr[vv]
                pp.add_patch(
                    matpat.Rectangle( ( rsum, 
                                        ss.ylim[0] ),
                                      width=bf.patch_arr[vv+1],
                                      height=(ss.ylim[1] - ss.ylim[0]),
                                      facecolor=laio.DEF_patch_alt_RGB,
                                      lw=0, edgecolor=None, alpha=None) ) 
                rsum+= bf.patch_arr[vv+1]

        if bf.ncensor : 
            xoffset = 0.5 * bf.censor_width
            for cc in range(bf.ncensor):
                pp.add_patch(
                    matpat.Rectangle( (bf.censor_arr[cc] - xoffset, 
                                       ss.ylim[0]),
                                      width=bf.censor_width,
                                      height=(ss.ylim[1] - ss.ylim[0]),
                                      facecolor=bf.censor_RGB,
                                      lw=0, edgecolor=None, alpha=None) )

        if ss.censor_hline : 
            if ss.censor_hline != 'NONE' :
                pp.axhline( y=ss.censor_hline, 
                            c=laio.DEF_censor_hline_RGB, 
                            ls=':', lw=1)

        if bf.see_xax : 
            pp.axhline(y=0, c='0.6', ls='-', lw=0.5)

        if not(bf.margin_on) :
            a.subplots_adjust(left=0.0, bottom=0.0, right=1, top=1)

        ## the actual plot. 
        # [PT: Jan 14, 2019] now get line width as a function of the
        # number of points.
        #if not(bf.margin_on) :
        #    pp.axis('off')
        sp = pp.plot( ss.x, ss.y, 
                      color = ss.color,
                      lw=calc_lw_from_npts(ss.npts),
                      label=ss.leglabel )
        
        pp.set_xlim(ss.xlim)
        pp.set_ylim(ss.ylim)

        pp.set_xlabel(ss.xlabel, fontsize=bf.fontsize)

        if bf.ylabels_maxlen :
            ss.ylabel = long_string_wrap(ss.ylabel, maxlen=bf.ylabels_maxlen)

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
        if bf.margin_on :
            pp.spines['bottom'].set_color('0.5')
            pp.spines['top'   ].set_color('0.5')
            pp.spines['left'  ].set_color('0.5')
            pp.spines['right' ].set_color('0.5')

        # only show tick labels at very bottom
        if i < bf.ngraph-1 :
            # [PT] pp.set_xticks() here just to suppress (unnec) warning:
            # "FixedFormatter should only be used together with FixedLocator"
            # ... but it also makes the xaxes vary across a plot, so remove 
            # for now. Maybe come back to this...
            #pp.set_xticks(pp.get_xticks()) 
            nlabs = len(pp.get_xticklabels())
            pp.set_xticklabels(['']*nlabs)

        if bf.title and not(i):
            # cheating with title because tight layout doesn't know about
            # suptitle
            pp.set_title(bf.title, fontsize=bf.fontsize)

        if bf.legend_on :
            pp.legend(loc=ss.legloc)


        # ----------------- Optional plot: boxplot ---------------------

        if bf.boxplot_on :
            if bf.nsub > 1:
                qq = subpl[i,1]
            else:
                qq = subpl[1]

            if ss.censor_hline : 
                if ss.censor_hline != 'NONE' :
                    qq.axhline( y=ss.censor_hline, 
                                c=laio.DEF_censor_hline_RGB, 
                                ls=':', lw=1)

            if bf.see_xax : 
                qq.axhline(y=0, c='0.6', ls='-', lw=0.5)

            # ------------ actual boxplot ------------------

            ## [PT: Jan 15, 2019] Add functionality to see boxplot of
            ## censored data, too, if not turned off *when* censoring
            ## and asking for a boxplot
            if bf.bplot_view == "BC_AC" :
                W = 0.15
                sq = qq.boxplot( [ss.y, ss.ycen],
                                 widths=W,
                                 positions=[0.25, 0.75],
                                 sym='.',
                                 notch=0, 
                                 patch_artist=True )
                ''' # unfortunately, this may not be workable to distinguish
                W3 = 3*W
                qq.add_patch(
                    matpat.Rectangle( ( 0.25 - W3/2.0, 
                                        ss.ylim[0] ),
                                      width=W3,
                                      height=(ss.ylim[1] - ss.ylim[0]),
                                      facecolor=bf.censor_RGB,
                                      lw=0, edgecolor=None, alpha=None) )
                '''
                if not(i):
                    # cheating with title because tight layout doesn't
                    # know about suptitle: BC='before censoring', and
                    # AC='after censoring'
                    qq.set_title( "BC AC", fontsize=bf.fontsize )

            elif bf.bplot_view == "BC_ONLY" :
                sq = qq.boxplot( ss.y,
                                 widths=0.1,
                                 sym='.',
                                 notch=0, 
                                 patch_artist=True )
            elif bf.bplot_view == "AC_ONLY" :
                sq = qq.boxplot( ss.ycen,
                                 widths=0.1,
                                 sym='.',
                                 notch=0, 
                                 patch_artist=True )
                if not(i):
                    qq.set_title( "AC", fontsize=bf.fontsize )
                


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
                line.set_color('0.7')
                line.set_linewidth(SETLW*1.25 )
            boxlines = sq['boxes']
            for line in boxlines:
                line.set_color(ss.color)
            plt.setp(sq['fliers'], marker='.', mew=0.3, mec='k', mfc=ss.color, 
                     color=ss.color, ms=MARKSIZE2)
            plt.setp(sq['whiskers'], color=ss.color, linestyle='-', lw=SETLW )
            plt.setp(sq['caps'],     color=ss.color, linestyle='-', lw=SETLW )

            # no xticks/labels
            if 1:
                qq.set_xticks([])

            # stuff for y-axis ticks (on) and labels (off)
            qq.yaxis.set_minor_locator(AutoMinorLocator(2))
            qq.tick_params( axis='y', which='minor', direction='in', 
                            color='0.5',
                            bottom=True, left=True, right=True ) #, top=True )
            qq.tick_params( axis='y', which='major', direction='in', 
                            color='0.5',
                            bottom=True, left=True, right=True,
                            labelleft=False) #, top=True )
            qq.spines['bottom'].set_color('0.5')
            qq.spines['top'   ].set_color('0.5')
            qq.spines['left'  ].set_color('0.5')
            qq.spines['right' ].set_color('0.5')



    # finishing touches

    if bf.layout == 'nospace' :
        fff.subplots_adjust( wspace=0.1, hspace=0.1 )
    #elif bf.layout == 'tight':
    #    plt.tight_layout()

    if not(bf.margin_on) :
        plt.savefig( bf.fname, dpi=bf.dpi, facecolor=bf.bkgd_color)
    else :
        plt.savefig( bf.fname, dpi=bf.dpi, facecolor=bf.bkgd_color,
                     bbox_inches='tight')
    print("++ Done! Figure created:\n\t {}".format(bf.fname))

    return 0

# ---------------------------------------------------------------------------

def calc_lw_from_npts(N):
    if N < 450:
        return 1.5
    elif N < 1200:
        return 1.0
    elif N < 2550:
        return 0.75
    else:
        return 0.5

# ---------------------------------------------------------------------------

# make the 'bigfig' object (type: figplobj) from lots of individual
# 'ss' subjects (type: subplobj).
def populate_1dplot_fig(iopts):

    # The order of setting things here matters in some cases, so don't
    # rearrange without mega-testing.
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
    bigfig.set_censor_on( iopts.censor_on )
    bigfig.set_censor_hline( iopts.censor_hline )
    bigfig.set_patch_arr( iopts.patch_arr )
    bigfig.set_boxplot( iopts.boxplot_on )
    bigfig.set_margin( iopts.margin_on )
    bigfig.set_bplot_view( iopts.bplot_view )
    bigfig.set_legend_on( iopts.legend_on )
    bigfig.set_ylabels_maxlen( iopts.ylabels_maxlen )

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

        # if not a null set, then there is one for each subj; for
        # plotting in "after censoring" boxplot, if selected at the
        # moment, only a single censor list gets applied to whole
        # plot.  This gets calculated, even if not applied.
        if bigfig.boxplot_ycen : 
            ycen = make_censored_sublist( ss.y, bigfig.censor_arr )
            ss.set_ycen( ycen )

        # if not a null set, then there is one for each subj
        if iopts.censor_hline : 
            ss.set_censor_hline(iopts.censor_hline[ii] )

        if iopts.onescl :
            ss.set_ylim( laio.rangeize_plots( iopts.all_ymin, 
                                              iopts.all_ymax,
                                              firm_lim = iopts.yaxran[ii]) )
        else:
            ss.set_ylim( ext_ylim=iopts.yaxran[ii] )
        ss.set_ylabel( iopts.ylabels[ii] )

        if bigfig.legend_on :
            ss.set_leglabel( iopts.leglabels[ii] )
            ss.set_legloc( iopts.leglocs[ii] )

        bigfig.add_sub(ss)

    # depends on final number of subjects
    bigfig.set_figsize( iopts.figsize )
    bigfig.set_ngraph( iopts.one_graph )

    return bigfig

# ---------------------------------------------------------------------------

# Apply censoring to a time series, to generate a list of censored
# values (for boxplotting, mainly)
def make_censored_sublist( y, cen_arr ):

    Ny    = len(y)
    olist = []

    for i in range(Ny):
        if not(cen_arr.__contains__(i)) :
            olist.append( y[i] )

    return olist

# ---------------------------------------------------------------------------

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
    # ... and check this in the same way: scales for y-axis stretch.
    #     must be done *after* censor hlines are set, and *before* yrange
    #     is assigned; censor hlines will be set to 1 here
    iopts.check_scales()
    if iopts.count_scale() :
        iopts.apply_scales_all_y()
        iopts.apply_scales_censor_hlines()
    # ... and check this in the same way: optional user entry for
    # ylims; this is *not* scaled-- user's value is used directly
    iopts.check_yaxran()

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
#import numpy as np
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
