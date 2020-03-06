#ver = '1.0' ; date = 'Oct 17, 2018' ; auth = 'PA Taylor'
# + birth
#
#ver = '1.2' ; date = 'Nov 1, 2018' 
# + [PT] working beta version
# + now has help file, lots of options
#
#ver = '1.21' ; date = 'Nov 1, 2018' 
# + [PT] file help file (double backslash)
#
#ver = '1.22' ; date = 'Nov 5, 2018' 
# + [PT] trying to get rid of all transparency/alphas, for simple
#   jpg format
#
#ver = '1.3' ; date = 'Dec 5, 2018' 
# + [PT] have removed numpy dependency for 'basic' APQC HTML functionality
#
#ver = '1.3b' ; date = 'Dec 19, 2018' 
# + [PT] make 1dplot.py show correct help
#
#ver = '1.4' ; date = 'Dec 23, 2018' 
# + [PT] change condition for how to check for all args after option
#   flag-- should be more stable for larger range of options; 
# + [PT] have added '-yaxis ...' stuff to pythonic style
#
#ver = '1.5' ; date = 'Jan 15, 2019' 
# + [PT] add in new plotting functionality: when one has both boxplot
#   on *and* censoring, data BC and AC.
# + [PT] ... and have a new option to go back to just having only BC
#   data, via '-bplot_view BC_ONLY'
#
#ver = '1.6' ; date = 'July 23, 2019' 
# + [PT] 1dplot.py can output PDF files now, too
#
#ver = '1.61' ; date = 'July 26, 2019' 
# + [PT] Fix py2 incompatability
#
#ver = '1.7' ; date = 'Jan 29, 2020' 
# + [PT] Fix input with '-xfile ..' opt
#      + Fix initialization of all objs here (with def __init(self))
#
ver = '1.8' ; date = 'Feb 216, 2020' 
# + [PT] add in ability for -censor_hline to contain a 'NONE' argument 
#        ... bc that is useful as a placeholder when some images have
#        censor vals and others don't
#
#########################################################################

# Supplementary stuff and I/O functions for the AP QC tcsh script

import sys, copy
import lib_afni1D as LAD
import lib_afni1D as LAD
import afni_util  as au

# -------------------------------------------------------------------

lvolreg        = [ 'roll\n(deg)', 'pitch\n(deg)', 'yaw\n(deg)', 
                   'dS\n(mm)',  'dL\n(mm)',  'dP\n(mm)' ]
ok_ftypes      = [ '.jpg', '.png', '.tif', '.pdf' ]
ok_ftypes_str  = ', '.join(ok_ftypes)

# these exact names are used in the functions in lib_apqc_tcsh.py to
# determine what kind of images get made
ok_review_styles = ["none", "basic", "pythonic", "java"]

DEF_dpi           = 150
DEF_prefix        = "PREFIX"
DEF_figsize       = [10, 0.75]
DEF_fontsize      = 10
DEF_fontfamily    = "monospace"
DEF_fontstyles    = [] # empty by default, so comp would go through list
DEF_layout        = "tight"  # good for single plots; 'nospace' for multiplot
DEF_censor_RGB    = [1, 0.7, 0.7] #'red' #'green'
DEF_censor_hline_RGB = 'cyan'
DEF_patch_alt_RGB = '0.95'
DEF_bkgd_color    = '0.9'
DEF_boxplot_on    = False
DEF_margin_on     = True
DEF_bplot_view    = ""  # vals: ["BC_ONLY", "BC_AC"]
DEF_boxplot_ycen  = False
DEF_censor_on     = False

# mostly from: plt.cm.get_cmap('Set1') and plt.cm.get_cmap('tab10')
DEF_color_table = [
    [0.215, 0.494, 0.721, 1.0], # blue
    [0.894, 0.102, 0.110, 1.0], # red
    [0.172, 0.627, 0.172, 1.0], # green
    [1.0,   0.498, 0.0,   1.0], # orange
    [0.586, 0.296, 0.629, 1.0], # purple
    [ 0.1,  0.8,   0.8,   1.0], # cyan
    [0.968, 0.506, 0.749, 1.0], # pink
    [0.651, 0.337, 0.157, 1.0], # brown
    [0.6,   0.6,   0.6,   1.0], # gray
    [0.737, 0.741, 0.133, 1.0]] # dark yellow

# -------------------------------------------------------------------
# -------------------------------------------------------------------

# helpfile for the plotting prog

fill_in_help_vals = {                                              
                      'def_dpi':DEF_dpi,                           
                      'def_ext':ok_ftypes[0],                      
                      'def_imwid':DEF_figsize[0],                  
                      'def_imhei':DEF_figsize[1],                  
                      'def_fontsz':DEF_fontsize,                   
                      'def_fontfam':DEF_fontfamily,                
                      'def_cen_RGB':DEF_censor_RGB,                
                      'def_col_bkdg':DEF_bkgd_color,               
                      'def_num_col':len(DEF_color_table),          
                      'ok_ftypes_str':ok_ftypes_str,               
}

help_string_apqc_1dplot = '''

OVERVIEW ~1~

This program is for making images to visualize columns of numbers from
"1D" text files.  It is based heavily on RWCox's 1dplot program, just
using Python (particularly matplotlib).  To use this program, Python
version >=2.7 is required, as well as matplotlib modules (someday numpy 
might be needed, as well).

This program takes very few required options-- mainly, file names and
an output prefix-- but it allows the user to control/add many
features, such as axis labels, titles, colors, adding in censor
information, plotting summary boxplots and more.

++ constructed by PA Taylor (NIMH, NIH, USA).

# =========================================================================

COMMAND OPTIONS ~1~

-help, -h     :see helpfile

-infiles II   :(req) one or more file names of text files.  Each column
               in this file will be treated as a separate time series
               for plotting (i.e., as 'y-values').  One can use
               AFNI-style column '{{ }}' and row '[ ]' selectors.  One
               or more files may be entered, but they must all be of 
               equal length.
-yfiles YY    :exactly the same behavior as "-infiles ..", just another
               option name for it that might be more consistent with
               other options.

-prefix  PP   :output filename or prefix; if no file extension for an
               image is included in 'PP', one will be added from a
               list.  At present, OK file types to output should include:
                  {ok_ftypes_str}
               ... but note that the kinds of image files you may output
               may be limited by packages (or lack thereof) installed on
               your own computer.   Default output image type is {def_ext}

-boxplot_on   :a fun feature to show an small, additional boxplot
               adjacent to each time series.  The plot is a standard
               Python boxplot of that times series's values.  The box
               shows the 25-75%ile range (interquartile range, IQR);
               the median value highlighted by a white line; whiskers
               stretch to 1.5*IQR; circles show outliers.
               When using this option and censoring, by default both a
               boxplot of data "before censoring" (BC) and "after
               censoring (AC) will be added after.  See '-bplot_view ...'
               about current opts to change that, if desired.

-bplot_view BC_ONLY | AC_ONLY
              :when using '-boxplot_on' and censoring, by default the
               plotter will put one boxplot of data "before censoring"
               (BC) and after censoring (AC). To show *only* the
               uncensored one, use this option and keyword.

-margin_off   :use this option to have the plot frame fill the figure
               window completely; thus, no labels, frame, titles or
               other parts of the 'normal' image outside the plot
               window will be visible.  Tick lines will still be
               present, living their best lives.
               This is probably only useful/recommended/tested for
               plots with a single panel.

-scale  SCA1 SCA2 SCA3 ...
              :provide a list of scales to apply to the y-values.
               These will be applied multiplicatively to the y-values;
               there should either be 1 (applied to all time series)
               or the same number as the time series (in the same
               order as those were entered).  The scale values are
               also applied to the censor_hline values, but *not* to
               the "-yaxis ..." range(s). 
               Note that there are a couple keywords that can be used
               instead of SCA* values:
                   SCALE_TO_HLINE: each input time series is
                      vertically scaled so that its censor_hline -> 1.
                      That is, each time point is divided by the
                      censor_hline value.  When using this, a visually
                      pleasing yaxis range might be 0:3.
                   SCALE_TO_MAX: each input time series is
                      vertically scaled so that its max value -> 1.
                      That is, each time point is divided by the
                      max value. When using this, a visually
                      pleasing yaxis range might be 0:1.1.

-xfile   XX   :one way to input x-values explicitly: as a "1D" file XX, a
               containing a single file of numbers.  If no xfile is 
               entered, then a list of integers is created, 0..N-1, based
               on the length of the "-infiles ..".
               
-xvals START STOP STEP
              :an alternative means for entering abscissa values: one
               can provide exactly 3 numbers, the start (inclusive)
               the stop (exclusive) and the steps to take, following
               Python conventions-- that is, numbers are generated
               [START,STOP) in stepsizes of STEP.

-yaxis YMIN1:YMAX1 YMIN2:YMAX2 YMIN3:YMAX3 ...
              :optional range for each "infile" y-axis; note the use
              of a colon to designate the min/max of the range.  One
              can also specify just the min (e.g., "YMIN:") or just
              the max (e.g., ":YMAX"). The final number of y-axis
              values or pairs *must* match the total number of columns
              of data from infiles; a placeholder could just be
              ":". Without specifying a range, one is calculated
              automatically from the min and max of the dsets
              themselves. The order of ylabels should match the order
              of infiles.

-ylabels YL1 YL2 YL3 ...
              :optional text labels for each "infile" column; the
               final number of ylabels *must* match the total number
               of columns of data from infiles.  For 1D files output
               by 3dvolreg, one can automatically provide the 6
               associated ylabels by providing the keyword 'VOLREG'
               (and this counts as 6 labels).  The order of ylabels
               should match the order of infiles.

-xlabel XL    :optional text labels for the abscissa/x-axis.  Only one may  
               be entered, and it will *only* be displayed on the bottom 
               panel of the output plot.  Using labels is good practice!!

 
-title TT     :optional title for the set of plots, placed above the top- 
               most subplot

-reverse_order :optional switch; by default, the entered time series
               are plotted top to bottom according to the order they
               were entered (i.e., first- listed plot at the top).
               This option reverses that order (to first-listed plot
               at the bottom), in order to match with 1dplot's
               behavior.

-sepscl       :make each graph have its own y-range, determined by
               slightly padding its min and max values.  By default,
               the separate plots all have the same y-range, which
               is determined by taking the min-of-mins and max-of-
               maxes, and padding slightly outward.  

-one_graph    :plot multiple infiles in a single subplot (default is to put
               each one in a new subplot).

-dpi DDD      :choose the output image's DPI.  The default value is
               {def_dpi}.

-figsize FX FY :choose the output image's dimensions (units are inches).  
                The default width is {def_imwid}; the default height
                is 0.5 + N*{def_imhei}, where 'N' is the number of 
                infile columns.

-fontsize FS   :change image fontsize; default is {def_fontsz}.

-fontfamily FF :change font-family used; default is the luvly
                {def_fontfam}.

-fontstyles FSS :add in a fontname; should match with chosen
                font-family; default is whatever Python has on your
                system for the given family.  Whether your prescribed
                font gets used depends on what is installed on your
                comp.

-colors C1 C2 C3 ...
               :you can decide what color(s) to cycle through in plots
                (enter one or more); if there are more infile columns
                than entered colors, the program just keeps cycling
                through the list. By default, if only 1 infile column is
                given, the plotline will be black; when more than one
                infile column is given, a default palette of {def_num_col}
                colors, chosen for their mutual-distinguishable-ness,
                will be cycled through.

-patches RL1 RL2 RL3 ...
               :when viewing data from multiple runs that have been
                processing+concatenated, knowing where they start/stop
                can be useful. This option helps with that, by
                alternating patches of the background slightly between
                white and light gray.  The user enters any appropriate
                number of run lengths, and the background patch for
                the duration of the first is white, then light gray,
                etc. (to *start* with light gray, one can have '0' be
                the first RL value).

-censor_trs CS1 CS2 CS3 ...
               :specify time points where censoring has occured (e.g.,
                due to a motion or outlier criterion).  With this
                option, the values are entered using AFNI index
                notation, such as '0..3,8,25,99..$'.  Note that if you
                use special characters like the '$', then the given
                string must be enclosed on quotes.  
                One or more string can be entered, and results are
                simply combined (as well as if censor files are
                entered-- see the '-censor_files ..' opt).
                In order to highlight censored points, a translucent
                background color will be added to all plots of width 1.

-censor_files CF1 CF2 CF3 ...
               :specify time points where censoring has occured (e.g.,
                due to a motion or outlier criterion).  With this
                option, the values are entered as 1D files, columns
                where 0 indicates censoring at that [i]th time point,
                and 1 indicates *no* censoring there.  
                One or more file can be entered, and results are
                simply combined (as well as if censor strings are
                entered-- see the '-censor_str ..' opt).  
                In order to highlight censored points, a translucent
                background color will be added to all plots of width 1.

-censor_hline CH1 CH2 CH3 ...
               :one can add a dotted horizontal line to the plot, with
                the intention that it represents the relevant threshold
                (for example, motion limit or outlier fraction limit).
                One can specify more than one hline: if one line
                is entered, it will be applied to each plot; if more
                than one hline is entered, there must be the same number
                of values as infile columns. 
                Ummm, it is also assumed that all censor hline values 
                are >=0; if negative, it will be a problem-- ask if this
                is a problem!
                A value of 'NONE' can also be input, to be a placeholder
                in a list, when some subplots have censor_hline values
                and others don't.

-censor_RGB COL :choose the color of the censoring background; default
                is: {def_cen_RGB}.

-bkgd_color BC :change the background color outside of the plot
                windows.  Default is the Python color: {def_col_bkdg}.

EXAMPLES ~1~

1) Plot Euclidean norm (enorm) profile, with the censor limit and
   related file of censoring:
1dplot.py                                    \\
    -sepscl                                  \\
    -boxplot_on                              \\
    -infiles      motion_sub-10506_enorm.1D  \\
    -censor_files motion_sub-10506_censor.1D \\
    -censor_hline 0.2                        \\
    -title   "Motion censoring"              \\
    -ylabels enorm                           \\
    -xlabel  "vols"                          \\
    -title   "Motion censoring"              \\
    -prefix  mot_cen_plot.jpg

2) Plot the 6 solid body parameters from 3dvolreg, along with
   the useful composite 'enorm' and outlier time series
1dplot.py                                    \\
    -sepscl                                  \\
    -boxplot_on                              \\
    -reverse_order                           \\
    -infiles      dfile_rall.1D              \\
                  motion_sub-10506_enorm.1D  \\
                  outcount_rall.1D           \\
    -ylabels  VOLREG enorm outliers          \\
    -xlabel  "vols"                          \\
    -title   "Motion and outlier plots"      \\
    -prefix  mot_outlier_plot.png


'''.format(**fill_in_help_vals)




# -------------------------------------------------------------------
# -------------------------------------------------------------------

def ARG_missing_arg(arg):
    print("** ERROR: missing argument after option flag: {}".format(arg))
    sys.exit(1)

# ========================================================================
# ======================== for 1dplotting pythonly =======================

# object for each figure
class figplobj:

    def __init__(self):
        self.title        = ""
        self.dpi          = DEF_dpi
        self.all_subs     = []     # will be a list of subplots
        self.nsub         = 0
        self.ngraph       = 0
        self.fname        = DEF_prefix
        self.figsize      = []
        self.fontsize     = DEF_fontsize
        self.fontfamily   = DEF_fontfamily
        self.fontstyles   = DEF_fontstyles
        self.layout       = DEF_layout
        self.see_xax      = True
        self.color_table  = []
        self.censor_RGB   = DEF_censor_RGB
        self.censor_width = 0
        self.censor_arr   = []              # NB: really stays a list
        self.censor_on    = DEF_censor_on
        self.censor_hline = []
        self.ncensor      = 0
        self.patch_arr    = []
        self.npatch       = 0
        self.bkgd_color   = DEF_bkgd_color
        self.boxplot_on   = DEF_boxplot_on
        self.margin_on    = DEF_margin_on
        self.bplot_view   = ''
        self.boxplot_ycen = DEF_boxplot_ycen

    def set_censor_RGB(self, c):
        self.censor_RGB = c

    def set_censor_on(self, c):
        self.censor_on = c

    def set_boxplot(self, c):
        self.boxplot_on = c

    def set_margin(self, c):
        self.margin_on = c

    def set_boxplot_ycen(self, c):
        self.boxplot_ycen = c

    # assumes self.nsub has been set already; 'c' is T/F value of
    # self.onegraph
    def set_ngraph(self, c):
        if c:
            self.ngraph = 1
        else:
            self.ngraph = self.nsub

    def set_bplot_view(self, c):
        if not(self.censor_on) :
            self.bplot_view = "BC_ONLY"
        else:
            if not(c) or c == "BC_AC":
                self.bplot_view = "BC_AC"
            elif c == 'BC_ONLY' or c == 'AC_ONLY':
                self.bplot_view = c
            else:
                sys.exit("**ERROR: no bplot_view option {}".format(c))

        # And then flag to calc ycen
        if self.bplot_view.__contains__("AC"):
            self.set_boxplot_ycen(True)

    def set_bkgd_color(self, c):
        self.bkgd_color = c

    def set_censor_hline(self, listhh):
        for hh in listhh:
            self.censor_hline.append(hh)

    def set_censor_width(self, w):
        self.censor_width = w

    def set_censor_arr(self, ll):
        self.censor_arr = ll
        self.ncensor = len(self.censor_arr)

    def set_patch_arr(self, ll):
        self.patch_arr = ll
        self.npatch = len(self.patch_arr)

    def set_title(self, title):
        self.title = title

    def set_dpi(self, dpi):
        self.dpi = int(dpi)

    def set_layout(self, kwlay):
        self.layout = kwlay # 'nospace', 'tight', ''

    def no_xaxis(self):
        self.see_xax = False

    def set_fname(self, fname):
        self.fname = fname
        self.check_fname()

    def check_fname(self):
        fff = self.fname
        Nf  = len(self.fname)
        FOUND = 0
        for x in ok_ftypes:
            Nx = len(x)
            if fff[Nf - Nx:] == x:
                FOUND = 1
        if not(FOUND):
            self.fname+= ok_ftypes[0]

    # list of subplots
    def add_sub(self, sub):
        self.all_subs.append(sub)
        self.nsub+=1

    def set_figsize(self, dims=[]):
        if not(dims) :
            # this might be a nice ratio, with respect to page with
            # and html (in inches)
            self.figsize = [ DEF_figsize[0], 
                             2*0.55 + self.nsub*DEF_figsize[1]]
        else:
            self.figsize=dims

    def set_fontsize(self, a):
        self.fontsize = a

    def set_fontfamily(self, a):
        self.fontfamily = a

    # note that here we just replace the list fully, not adding to a
    # starter one
    def set_fontstyles(self, a):
        self.fontstyles = a

# -------------------------------------------------------------------

# A couple simple helper funcs for checking censor hline vals and scaling

def set_valid_censor_hline_val(hh):
    '''Used in a few places: input 'hh' can be either a number or 'NONE'.

    '''

    try:
        out = float(hh)
    except:
        if hh == 'NONE' :
            out = hh
        else:
            sys.exit("** ERROR: censor_hline value '{}' is illegal.\n"
                     "   Must enter a number or 'NONE'".format(hh))
    return out

def set_valid_censor_hline_scale2max( all_y ):
    '''Used in a few places: input list all_y, and this returns the number
to use for scaling in SCALE_TO_MAX scenario.

    '''

    maxy = max(all_y)
    if maxy :
        hh = 1.0/maxy
    else:
        sys.exit("** ERROR: cannot use SCALE_TO_MAX scaling "
                 " when a censor_hline has "
                 "{}-value".format(maxy))

    return hh

# ---------------------------------

# object for each subplots
class subplobj:

    def __init__(self):
        self.x      = []
        self.y      = []
        self.ycen   = []    # will be a list of points after censoring
        self.xlabel = ""
        self.ylabel = ""
        self.xlim   = []
        self.ylim   = []
        self.npts   = -1
        self.color  = ""
        self.ymin   = 0.
        self.ymax   = 0.
        self.censor_hline  = [] # will be list of number(s) and/or 'NONE'

    def set_x(self, x):
        self.x = x

    def set_y(self, y):
        ymin, ymean, ymax, ystdev = au.min_mean_max_stdev(y)
        self.y = y
        #self.ymin = np.min(self.y)
        #self.ymax = np.max(self.y)
        self.ymin = ymin
        self.ymax = ymax
        self.npts = len(y)

    # [PT: Jan 15, 2019] put these properties for each single subj now
    def set_ycen(self, y):
        ymin, ymean, ymax, ystdev = au.min_mean_max_stdev(y)
        self.ycen = y
        #self.ymin = np.min(self.y)
        #self.ymax = np.max(self.y)
        self.ycenmin = ymin
        self.ycenmax = ymax
        self.ycennpts = len(y)

    def set_xlabel(self, xlabel):
        self.xlabel = xlabel

    def set_ylabel(self, ylabel):
        self.ylabel = ylabel

    def set_censor_hline(self, hh): 
        val = set_valid_censor_hline_val(hh) # check number or 'NONE'
        self.censor_hline = val

    def set_xlim(self, xlim=[]):
        if xlim :
            self.xlim = xlim
        else:
            deltax = self.x[1] - self.x[0]
            xmin, xmean, xmax, xstdev = au.min_mean_max_stdev(self.x)
            self.xlim = [ xmin - 0.5*deltax, 
                          xmax + 0.5*deltax ]
            #self.xlim = [ np.min(self.x) - 0.5*deltax, 
            #              np.max(self.x) + 0.5*deltax ]

    # [PT] add in additional check in case user input ranges
    def set_ylim(self, ylim=[], ext_ylim=[]):
        if ylim :
            self.ylim = ylim
        else:
            bb = self.ymin
            tt = self.ymax
            self.ylim = rangeize_plots( bb, tt, firm_lim=ext_ylim )

    def check_len_xy(self):
        MISS = 0
        if len(self.x) != self.npts : 
            print("missing: subplobj has unmatched x-y arr lens")
            MISS+=1
        return MISS

    def set_color(self, color):
        self.color = color

    def set_fontfamily(self, a):
        self.fontfamily = a

    # note that here we just replace the list fully, not adding to a
    # starter one
    def set_fontstyles(self, a):
        self.fontstyles = a


# -------------------------------------------------------------------

# input-reading opts, also partly processed
class apqc_1dplot_opts:

    def __init__(self):

        # req input
        self.infiles  = []
        self.prefix   = DEF_prefix
        self.ninfiles = 0

        # opt input
        self.ylabels  = []
        self.nylabels = 0    
        self.xlabel  = ""
        self.onescl  = True
        self.one_graph  = False
        self.reverse_order = False
        self.xfile   = ""    # 1D file of xaxis vals
        self.xvals   = []    # alt. xaxis def: "start stop step"
        self.yaxran  = []    # yaxis range [MIN, MAX]; maybe later include more

        self.dpi     = DEF_dpi
        self.title   = ""
        self.figsize = []
        self.layout  = ""
        self.fontsize = DEF_fontsize
        self.fontfamily = DEF_fontfamily
        self.fontstyles = DEF_fontstyles
        self.color_table = []
        self.ncolors = 0
        self.bkgd_color = DEF_bkgd_color
        self.boxplot_on = DEF_boxplot_on
        self.margin_on  = DEF_margin_on
        self.bplot_view = DEF_bplot_view

        # input catchers for censoring
        self.censor_in_trs   = []
        self.censor_in_files = []
        self.censor_on       = DEF_censor_on # if user asks for censoring, this
                                        # will go to True, and then if
                                        # boxplotting, the BC_AC will be
                                        # default, EVEN IF no points were
                                        # censored for a subj

        # derived values from censoring
        self.censor_arr   = []     # the final list; this is used
        self.ncensor      = 0
        self.censor_width = 0
        self.censor_RGB   = DEF_censor_RGB
        self.censor_hline = []

        self.scale        = []  # [PT: June 28, 2019] vertical, y-scale possible
        self.scale_type   = ''

        self.patch_arr   = [] # list of run lengths, for alternating patches of plot

        # derived vals
        self.all_x  = []
        self.all_y  = []
        self.npts   = -1
        self.ndsets = -1
        self.all_ymin = 0
        self.all_ymax = 0

    # ----------- req -----------------

    def add_infile(self, fff):
        self.infiles.append(fff)
        self.ninfiles+= 1

    # !!!! also check to put a file extension on end -- jpg by default?
    def set_prefix(self, prefix):
        self.prefix = prefix

    # check requirements
    def check_req(self):
        MISS = 0
        if self.infiles == []:
            print("missing: infiles")
            MISS+=1
        if self.prefix == "":
            print("missing: prefix")
            MISS+=1
        return MISS

    def check_conflicts(self):
        CONFLICTS = 0
        if self.boxplot_on and self.one_graph :
            print("conflict: using both '-boxplot_on' and '-onegraph'")
            CONFLICTS+=1
        return CONFLICTS

    # ----------- opt -----------------

    def set_layout(self, c):
        self.layout = c

    def set_boxplot(self, c):
        self.boxplot_on = c

    def set_margin(self, c):
        self.margin_on = c

    # [PT: Jan 15, 2019] 
    def set_bplot_view(self, c):
        self.bplot_view = c

    def set_bkgd_color(self, c):
        self.bkgd_color = c

    # [PT: Jan 15, 2019] e.g., for multiple dsets, concatenated runs;
    # just a list that must be in correct order
    def add_patch(self, ss):
        if ss.__contains__('.') :
            x = float(ss)
        else : 
            x = int(ss)
        self.patch_arr.append(x)

    def add_censor_in_trs(self, ss):
        self.censor_in_trs.append(ss)
        self.censor_on = True

    def add_censor_in_files(self, ff):
        self.censor_in_files.append(ff)
        self.censor_on = True

    def set_censor_RGB(self, c):
        self.censor_RGB = c

    def add_censor_hline(self, hh):
        val = set_valid_censor_hline_val(hh)
        self.censor_hline.append(val)

    def count_censor_hline(self):
        return len(self.censor_hline)
                      
    def check_censor_hlines(self):
        nhline = self.count_censor_hline()
        
        if nhline == 0:
            pass # fine
        elif nhline == self.ndsets:
            pass # fine
        elif nhline == 1:
            print("++ Will apply same hline {} to "
                  "all dsets".format(self.censor_hline[0]))
            for i in range(1,self.ndsets):
                hh = self.censor_hline[0]
                self.add_censor_hline(hh)
        else:
            sys.exit("** ERROR: number of censor_hlines {} "
                     "must be either 0, 1, or match the "
                     "number of infiles {}".format(nhline, self.ndsets))
  
    # [PT: June 28, 2019] modeled on importing censor values; can
    # scale time series, e.g., changing units (or what will be
    # internal scale)
    def add_scale(self, hh):
        # special case
        if hh == 'SCALE_TO_HLINE':
            self.set_scaletype(hh)
        elif hh == 'SCALE_TO_MAX':
            self.set_scaletype(hh)
        else:
            self.set_scaletype('user_val' )
            self.scale.append(float(hh))

    def set_scaletype(self, hh):
        self.scale_type = hh
        
    def count_scale(self):
        return len(self.scale)

    def check_scales(self):
        n = self.count_scale()
        
        if n == 0:
            # fun special case
            if self.scale_type == 'SCALE_TO_HLINE' :
                print("++ Will scale each time series by censor_hline")
                for i in range(self.ndsets):
                    if self.censor_hline[i] :
                        if self.censor_hline[i] == 'NONE' :
                            hh = set_valid_censor_hline_scale2max(self.all_y[i])
                        else:
                            hh = 1.0/self.censor_hline[i]
                        self.add_scale(hh)
                    else:
                        sys.exit("** ERROR: cannot use SCALE_TO_HLINE scaling "
                                 " when a censor_hline has "
                                 "{}-value".format(self.censor_hline[i]))
            elif self.scale_type == 'SCALE_TO_MAX' :
                print("++ Will scale each time series by time series max")
                for i in range(self.ndsets):
                    hh = set_valid_censor_hline_scale2max(self.all_y[i])
                    self.add_scale(hh)
            else:
                pass # fine
        elif n == self.ndsets:
            pass # fine
        elif n == 1:
            print("++ Will apply same scale {} to "
                  "all dsets".format(self.scale[0]))
            for i in range(1, self.ndsets):
                hh = self.scale[0]
                self.add_scale(hh)
        else:
            sys.exit("** ERROR: number of scale values {} "
                     "must be either 0, 1, or match the "
                     "number of infiles {}".format(n, self.ndsets))

    def apply_scales_all_y(self):
        if not(self.count_scale()):
            pass
        else:
            N = self.ndsets  # also number of scales, by def, here
            for i in range(N):
                print("++ Apply scale: {}".format(self.scale[i]))
                Ly = len(self.all_y[i])
                for j in range(Ly):
                    self.all_y[i][j]*= self.scale[i]
            # and reset min/max
            self.set_y_minmax()

    def apply_scales_censor_hlines(self):
        if not(self.count_scale()) or not(self.count_censor_hline()):
            pass
        else:
            N = self.ndsets  # also number of scales, by def, here
            for i in range(N):
                if self.censor_hline[i] != 'NONE' :
                    self.censor_hline[i]*= self.scale[i]

    # [PT: Dec 23, 2018] 'hh' can be any of the following:
    ###   bot:top --> full range specified
    ###   :top    --> only top val specified, lower filled in 
    ###   bot:    --> only bot val specified, upper filled in
    ###   bot     --> only bot val specified, upper filled in
    ###   :       --> no val specified, just place holder, default up/down
    def add_yaxran(self, hh):
        if type(hh) == str :
            tmp = hh.split(":")
        elif type(hh) == list :
            tmp = copy.deepcopy(hh)
        Ntmp = len(tmp)
        if Ntmp > 2:
            sys.exit("** ERROR: bad format of y-axis range!: {}".format(hh))
        llhh = []
        for i in range(Ntmp):
            try:    vv = float(tmp[i])
            except: vv = ''
            llhh.append(vv)
        self.yaxran.append(llhh)

    def count_yaxran(self):
        return len(self.yaxran)

    def check_yaxran(self):
        nyaxran = self.count_yaxran()
        
        if nyaxran == 0:
            # enter a set of "nulls" to check against
            hh = ":"
            for i in range(self.ndsets):
                self.add_yaxran(hh)
            # pass # fine
        elif nyaxran == self.ndsets:
            pass # fine
        elif nyaxran == 1:
            print("++ Will apply same y-axis range '{}' to "
                  "all dsets".format(self.yaxran[0]))
            for i in range(1,self.ndsets):
                hh = self.yaxran[0]
                self.add_yaxran(hh)
        else:
            sys.exit("** ERROR: number of y-axis range values {} "
                     "must be either 0, 1, or match the "
                     "number of infiles {}".format(nyaxran, self.ndsets))

    # [PT: Dec 5, 2018] Much updated, removing numpy and
    # simultaneously simplifying in parts (Munch-scream emoji!)
    def set_censor_arr(self):
        self.censor_width = self.all_x[1] - self.all_x[0]

        # add to this in different forms, where True values mean
        # censoring is flagged to have occured
        cen_arr = [False] * self.npts

        # combine all strings of censor lists:
        # ... from user-entered strings
        if self.censor_in_trs:
            maxind = self.npts - 1
            for ttt in self.censor_in_trs:
                ll = au.decode_1D_ints(ttt, imax=maxind)
                for ii in ll:
                    cen_arr[ii] = True

        # ... from user-entered files
        if self.censor_in_files:
            for fff in self.censor_in_files:
                x = LAD.Afni1D(fff)
                if x.nt != self.npts :
                    sys.exit("** ERROR: len of censor file {} ({}) does not "
                             "match Npts={}".format(fff, x.nt, self.npts))
                for i in range(self.npts):
                    val = x.mat[0][i]
                    if not(val):
                        cen_arr[i] = True

        # And finally store the list of xvals to censor
        self.ncensor = cen_arr.count(True)
        if self.ncensor :
            for i in range(self.npts):
                if cen_arr[i]:
                    self.censor_arr.append(self.all_x[i])
                
    def add_ylabel(self, ylabel):
        self.ylabels.append(ylabel)
        self.nylabels+= 1

    def set_xlabel(self, xlabel):
        self.xlabel = xlabel

    def set_xfile(self, ss):
        self.xfile = ss

    def set_xvals(self, a, b, c):
        self.xvals = [a, b, c]

    def set_title(self, title):
        self.title = title

    def set_dpi(self, dpi):
        self.dpi = int(dpi)

    def set_figsize(self, a, b):
        self.figsize = [float(a), float(b)]

    def set_fontsize(self, a):
        self.fontsize = a

    def set_fontfamily(self, a):
        self.fontfamily = a

    # note that here we just replace the list fully, not adding to a
    # starter one; 'a' should be a list
    def set_fontstyles(self, a):
        self.fontstyles = a

    def add_tablecolor(self, color):
        self.color_table.append(color)
        self.ncolors+= 1

    def do_sepscl(self):
        self.onescl = False

    def do_onegraph(self):
        self.one_graph = True

    def set_reverse_order(self, tf):
        self.reverse_order = tf

    def create_all_y(self):
        '''Populate ordinate/y-values from one or more filenames of y-values
    entered.
        '''

        ys = []
        for fff in self.infiles:
            x = LAD.Afni1D(fff)
            print("++ FOR: {}: {} arrays with {} pts".format(fff, 
                                                             x.nvec, 
                                                             x.nt))
            # this is done a bit clunkily here at the mo b/c; used to
            # read in as AfniData, which was relatively backwards; can
            # simplify later...
            for i in range(x.nvec):
                p = []
                for j in range(x.nt):
                    p.append(x.mat[i][j])
                ys.append(p)

        self.all_y = ys
        self.set_npts()
        self.set_ndsets()
        self.set_y_minmax()

    def set_y_minmax(self):
        gmin = 10**100
        gmax = -gmin
        for i in range(self.ndsets):
            lmin = min(self.all_y[i])
            lmax = max(self.all_y[i])
            if lmin < gmin :
                gmin = lmin
            if lmax > gmax :
                gmax = lmax
        self.all_ymin = gmin
        self.all_ymax = gmax

    def check_dims_all_y(self):

        N = len(self.all_y)

        if N == self.nylabels :
            print("++ Lists of names and yvars match length")
        elif self.nylabels == 0 :
            print("*+ No ylabels entered")
            for i in range(N):
                self.add_ylabel("")
        else:
            sys.exit("** ERROR: mismatch b/t number of data arrays "
                     "{} and num of ylabels {}".format(N, self.nylabels))

        M = len(self.all_y[0])
        for i in range(1, N):
            if M != len(self.all_y[i]):
                sys.exit("** ERROR: not all files have same num of rows")

        return 0


    def create_all_x(self):
        '''Populate abscissa/x-values depending on what the user entered,
    either a filename containing values or a stop/start/step rule.

        Input
        -----
        iopts : object containing all the info

        Return
        ------
        *nothing returned* : the 'iopts.xvals' array is populated

        '''

        if self.xfile :
            dat = LAD.Afni1D(self.xfile)
            if dat.nvec > 1 :
                sys.exit("** ERROR reading in xvalue file: too many columns.\n"
                         "   Specify just one!")
            x = dat.mat[0]
        elif self.xvals:
            i = 0
            A = self.xvals[0]
            x = [A]
            while A < self.xvals[1] and i < MAXLEN:
                A+= self.xvals[2]
                x.append(A)
                i+=1
        else:
            print("*+ No input x-axis; making values based on length of data: "
                  "[0..{}]".format(self.npts - 1))
            x = []
            for i in range(self.npts):
                x.append(i)

        self.all_x = x

    def check_dims_all_x_ys(self):

        lx = len(self.all_x)
        ly = self.npts

        if lx != ly :
            sys.exit("** ERROR: mismatch length of x "
                     "({}) and y({})".format(lx, ly))

        return 0

    def set_npts(self):

        if len(self.all_y):
            self.npts = len(self.all_y[0])
        else:
            sys.exit("** ERROR: no y-arrays of data entered!")

    def set_ndsets(self):
        
        self.ndsets = len(self.all_y)

        if self.ndsets < 0 :
            sys.exit("** ERROR: no y-arrays of data entered!")

    
    def check_color_table(self):
        '''If user entered a colortable, cool.  

Else: 
    + if only a single y-array was entered, make the colortable just
      a single value, 'black'; 
    + else, add in default one to be cycled through
    '''

        if not(self.color_table) :
            if self.ndsets == 1: 
                self.add_tablecolor('black')
            else:
                for x in DEF_color_table:
                    self.add_tablecolor(x)

    # cycle through color table, return one
    def get_color(self, i):
        m = i % self.ncolors
        return self.color_table[m]


# -------------------------------------------------------------------


# !!! If adding an option to this 1dplot.py program, make sure you
# !!! update the "all_opts' list here!!
def parse_1dplot_args(argv):

    '''Parse arguments for Pythony 1dplotter.

    Input
    -----
    argv : list of args (not including prog name)

    Return
    ------

    iopts : an object with the argument values stored, including a
        self-"check_req()" method, as well.
    '''

    ### [PT: Dec 23, 2018] Important for adding to this program!!! 
    ### One MUST include the option flag in this list, because some
    ### options can take multiple arguments of unspecified length--
    ### therefore, an argv is checked against this list to see if the
    ### list continues or not in some cases.
    all_opts = [ '-ver', '-h', '-help', "-infiles", "-yfiles",
                 "-prefix", "-ylabels", "-xlabel", "-xvals", "-xfile",
                 "-figsize", "-bkgd_color", "-fontsize",
                 "-fontfamily", "-fontstyles", "-censor_trs",
                 "-censor_files", "-censor_RGB", "-censor_hline",
                 "-title", "-dpi", "-colors", "-sepscl",
                 "-reverse_order", "-boxplot_on", "-bplot_view", 
                 "-yaxis", "-patches", "-margin_off", "-scale" ]


    Narg = len(argv)

    if not(Narg):
        print(help_string_apqc_1dplot)
        sys.exit(0)

    # initialize objs
    iopts  = apqc_1dplot_opts()    # input-specific

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_apqc_1dplot)
            sys.exit(0)

        # ---------- req ---------------

        # can have multiple infiles; must all have same len
        elif argv[i] == "-infiles" or argv[i] == "-yfiles":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_infile(argv[i])
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-prefix":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_prefix(argv[i])

        # ---------- opt ---------------

        elif argv[i] == "-ylabels":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                #aaa = argv[i][0]
                if not(all_opts.__contains__(argv[i])): #aaa != "-":
                    if argv[i] == "VOLREG" :
                        for name in lvolreg:
                            iopts.add_ylabel(name)
                    else:
                        iopts.add_ylabel(argv[i])
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-xlabel":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_xlabel(argv[i])

        # takes 3 vals after!
        elif argv[i] == "-xvals":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_xvals( argv[i], argv[i+1], argv[i+2] )
            i+=2

        elif argv[i] == "-xfile":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_xfile(argv[i])

        # takes 2 vals after!
        elif argv[i] == "-figsize":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_figsize( argv[i], argv[i+1] )
            i+=1

#### disabled at moment-- appears unnecessary (and breaks things!)
#        # takes 2 vals after!
#        elif argv[i] == "-layout":
#            if i >= Narg:
#                ARG_missing_arg(argv[i])
#            i+= 1
#            iopts.set_layout( argv[i] )

        elif argv[i] == "-bkgd_color":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_bkgd_color( argv[i] )

        elif argv[i] == "-fontsize":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_fontsize(argv[i])

        elif argv[i] == "-fontfamily":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_fontfamily(argv[i])

        elif argv[i] == "-fontstyles":
            fsty = []
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    fsty.append((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            iopts.set_fontstyles(fsty)
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-patches":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_patch((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-censor_trs":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_censor_in_trs((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-censor_files":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_censor_in_files((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-censor_RGB":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_censor_RGB(argv[i])


        elif argv[i] == "-censor_hline":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_censor_hline((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-scale":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_scale((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-yaxis":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_yaxran((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-title":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_title(argv[i])

        elif argv[i] == "-dpi":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_dpi(argv[i])

        elif argv[i] == "-colors":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if not(all_opts.__contains__(argv[i])): 
                    iopts.add_tablecolor(argv[i])
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-sepscl":
            iopts.do_sepscl()

        elif argv[i] == "-one_graph":
            iopts.do_onegraph()

        elif argv[i] == "-reverse_order":
            iopts.set_reverse_order(True)

        elif argv[i] == "-boxplot_on":
            iopts.set_boxplot(True)

        elif argv[i] == "-margin_off":
            iopts.set_margin(False)

        elif argv[i] == "-bplot_view":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_bplot_view(argv[i])


        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: Missing input arguments")
        sys.exit(1)
    if iopts.check_conflicts():
        print("** ERROR: conflicting/bad opts usage")
        sys.exit(2)

    return iopts


# ========================================================================
# ======================== for apqc_make_tcsh ============================

help_string_apqc_make_tcsh = '''

Help is here.

-uvar_json
-subj_dir
-review_style {{{}}}

'''.format( "|".join(ok_review_styles) )

# -------------------------------------------------------------------

class apqc_tcsh_opts:

    def __init__(self):
        # Each of these is required.  Might have other optional
        # options in the future
        self.json     = ""
        self.subjdir  = ""
        self.revstyle = "basic"

    def set_json(self, json):
        self.json = json

    def set_subjdir(self, subjdir):
        self.subjdir = subjdir

    def set_revstyle(self, revstyle):
        self.revstyle = revstyle

    # check requirements
    def check_req(self):
        MISS = 0
        if self.json == "":
            print("missing: json")
            MISS+=1
        if self.subjdir == "":
            print("missing: subjdir")
            MISS+=1
        if self.revstyle == "":
            print("missing: revstyle")
            MISS+=1
        elif not(ok_review_styles.__contains__(self.revstyle)) :
            print("revstyle '{}' not in allowed list".format(self.revstyle))
            MISS+=1
        return MISS

# -------------------------------------------------------------------

def parse_tcsh_args(argv):
    '''Parse arguments for tcsh scripter.

    Input
    -----
    argv : list of args (not including prog name)

    Return
    ------

    iopts : an object with the argument values stored, including a
        self-"check_req()" method, as well.
    '''

    Narg = len(argv)

    if not(Narg):
        print(help_string_apqc_make_tcsh)
        sys.exit(0)

    # initialize argument array
    iopts = apqc_tcsh_opts()

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_apqc_make_tcsh)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "-uvar_json":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_json(argv[i])

        elif argv[i] == "-subj_dir":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_subjdir(argv[i])

        elif argv[i] == "-review_style":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_revstyle(argv[i])

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: with input arguments (missing or incorrect)")
        sys.exit(1)
        
    return iopts

# ========================================================================
# ======================== for apqc_make_html ============================

help_string_apqc_make_html = '''

Help is here.

-qc_dir

'''

class apqc_html_opts:

    def __init__(self):
        # Each of these is required.  Might have other optional
        # options in the future
        self.qcdir = ""

    def set_qcdir(self, qcdir):
        self.qcdir = qcdir

    # check requirements
    def check_req(self):
        MISS = 0
        if self.qcdir == "":
            print("missing: qcdir")
            MISS+=1
        return MISS

# -------------------------------------------------------------------

def parse_html_args(argv):
    '''Parse arguments for html generator.

    Input
    -----
    argv : list of args (not including prog name)

    Return
    ------

    iopts : an object with the argument values stored, including a
        self-"check_req()" method, as well.
    '''

    Narg = len(argv)

    if not(Narg):
        print(help_string_apqc_make_html)
        sys.exit(0)

    # initialize argument array
    iopts = apqc_html_opts()

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_apqc_make_html)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "-qc_dir":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_qcdir(argv[i])

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: Missing input arguments")
        sys.exit(1)
        
    return iopts

# ----------------------------------------------------------------------------

# Useful function for this process, which occurs many times throughout
# these progs.  Estimate the min/max with a bit of padding, based on
# the difference between the values.  Sets a nice range for plotting
def rangeize_plots(A, B, scale=0.1, firm_lim = ['', '']):

    bot = A
    top = B

    # give precedence to *firm* limits
    if firm_lim[0] != '':
        bot = firm_lim[0]
    if firm_lim[1] != '':
        top = firm_lim[1]

    delta = (top - bot) * scale

    # only use delta if not a *firm* limit
    if firm_lim[0] == '' :
        bot-= delta
    if firm_lim[1] == '' :
        top+= delta

    return [bot, top] 
