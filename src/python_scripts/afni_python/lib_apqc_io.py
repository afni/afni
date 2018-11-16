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
#
#########################################################################

# Supplementary stuff and I/O functions for the AP QC tcsh script

import sys
import lib_afni1D as LAD
import numpy      as np
import afni_util  as au

# -------------------------------------------------------------------

lvolreg        = [ 'roll\n(deg)', 'pitch\n(deg)', 'yaw\n(deg)', 
                   'dS\n(mm)',  'dL\n(mm)',  'dP\n(mm)' ]
ok_ftypes      = [ '.jpg', '.png', '.tif' ]

# these exact names are used in the functions in lib_apqc_tcsh.py to
# determine what kind of images get made
ok_review_styles = ["none", "basic", "pythonic", "java"]

DEF_dpi        = 150
DEF_prefix     = "PREFIX"
DEF_figsize    = [10, 0.75]
DEF_fontsize   = 10
DEF_fontfamily = "monospace"
DEF_fontstyles = [] # empty by default, so comp would go through list
DEF_layout     = "tight"  # good for single plots; 'nospace' good for multiplot
DEF_censor_RGB = [1, 0.7, 0.7] #'red' #'green'
DEF_censor_hline_RGB = 'c'
DEF_bkgd_color = '0.93'
DEF_boxplot_on = False

# mostly from: plt.cm.get_cmap('Set1') and plt.cm.get_cmap('tab10')
DEF_color_table = [
    [0.215, 0.494, 0.721, 1.0],
    [0.894, 0.102, 0.110, 1.0],
    [0.172, 0.627, 0.172, 1.0],
    [1.0,   0.498, 0.0,   1.0],
    [0.586, 0.296, 0.629, 1.0],
    [ 0.1,  0.8,   0.8,   1.0],
    [0.968, 0.506, 0.749, 1.0],
    [0.651, 0.337, 0.157, 1.0],
    [0.6,   0.6,   0.6,   1.0],
    [0.737, 0.741, 0.133, 1.0],
    [0.6,   0.6,   0.6,   1.0]]

# -------------------------------------------------------------------
# -------------------------------------------------------------------

# helpfile for the plotting prog

fill_in_help_vals = {                                              \
                      'def_dpi':DEF_dpi,                           \
                      'def_ext':ok_ftypes[0],                      \
                      'def_imwid':DEF_figsize[0],                  \
                      'def_imhei':DEF_figsize[1],                  \
                      'def_fontsz':DEF_fontsize,                   \
                      'def_fontfam':DEF_fontfamily,                \
                      'def_cen_RGB':DEF_censor_RGB,                \
                      'def_col_bkdg':DEF_bkgd_color,               \
                      'def_num_col':len(DEF_color_table)           \
}

help_string_apqc_1dplot = '''

OVERVIEW ~1~

This program is for making images to visualize columns of numbers from
"1D" text files.  It is based heavily on RWCox's 1dplot program, just
using Python (particularly matplotlib).  To use this program, Python
version >=2.7 is required, with both numpy and matplotlib modules.

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
               list.  The kinds of image files you may output may be
               limited by packages (or lack thereof) installed on your
               computer.  Default output image type is {def_ext}

-boxplot_on :a fun feature to show an small, additional boxplot
               adjacent to each time series.  The plot is a standard
               Python boxplot of that times series's values.  The box
               shows the 25-75%ile range (interquartile range, IQR);
               the median value highlighted by a white line; whiskers
               stretch to 1.5*IQR; circles show outliers.

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

-colors C1 C2 C3
               :you can decide what color(s) to cycle through in plots
                (enter one or more); if there are more infile columns
                than entered colors, the program just keeps cycling
                through the list. By default, if only 1 infile column is
                given, the plotline will be black; when more than one
                infile column is given, a default palette of {def_num_col}
                colors, chosen for their mutual-distinguishable-ness,
                will be cycled through.

-censor_trs CS1 CS2 CS3 
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

-censor_files CF1 CF2 CF3 
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

-censor_hline CH1 CH2 CH3
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

-censor_RGB COL :choose the color of the censoring background; default
                is: {def_cen_RGB}.

-bkgd_color BC :change the background color outside of the plot
                windows.  Default is the Python color: {def_col_bkdg}.

EXAMPLES ~1~

1) Plot Euclidean norm (enorm) profile, with the censor limit and
   related file of censoring:
1dplotpy                                     \\
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
1dplotpy                                     \\
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

    title    = ""
    dpi      = DEF_dpi
    all_subs = []     # will be a list of subplots
    nsub     = 0
    fname    = DEF_prefix
    figsize  = []
    fontsize = DEF_fontsize
    fontfamily = DEF_fontfamily
    fontstyles = DEF_fontstyles
    layout     = DEF_layout
    see_xax  = True
    color_table = []
    censor_RGB = DEF_censor_RGB
    censor_width = 0
    censor_arr = []
    censor_hline = []
    ncensor = 0
    bkgd_color = DEF_bkgd_color
    boxplot_on = DEF_boxplot_on


    def set_censor_RGB(self, c):
        self.censor_RGB = c

    def set_boxplot(self, c):
        self.boxplot_on = c

    def set_bkgd_color(self, c):
        self.bkgd_color = c

    def set_censor_hline(self, hh):
        self.censor_hline = hh

    def set_censor_width(self, w):
        self.censor_width = w

    def set_censor_arr(self, ll):
        self.censor_arr = ll
        self.ncensor = len(self.censor_arr)

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

# object for each subplots
class subplobj:

    x      = []
    y      = []
    xlabel = ""
    ylabel = ""
    xlim   = []
    ylim   = []
    npts   = -1
    color  = ""
    ymin   = 0.
    ymax   = 0.
    censor_hline  = []

    def set_x(self, x):
        self.x = x

    def set_y(self, y):
        self.y = y
        self.ymin = np.min(self.y)
        self.ymax = np.max(self.y)
        self.npts = len(y)

    def set_xlabel(self, xlabel):
        self.xlabel = xlabel

    def set_ylabel(self, ylabel):
        self.ylabel = ylabel

    def set_censor_hline(self, hh):
        self.censor_hline = hh

    def set_xlim(self, xlim=[]):
        if xlim :
            self.xlim = xlim
        else:
            deltax = self.x[1] - self.x[0]
            self.xlim = [ np.min(self.x) - 0.5*deltax, 
                          np.max(self.x) + 0.5*deltax ]
            
    def set_ylim(self, ylim=[]):
        if ylim :
            self.ylim = ylim
        else:
            deltay = self.ymax - self.ymin
            # polish 'em
            bb2 = self.ymin - 0.1*deltay
            tt2 = self.ymax + 0.1*deltay
            self.ylim = [ bb2, 
                          tt2 ]

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

    # req input
    infiles  = []
    prefix   = DEF_prefix
    ninfiles = 0

    # opt input
    ylabels  = []
    nylabels = 0    
    xlabel  = ""
    onescl  = True
    reverse_order = False
    xfile   = ""    # 1D file of xaxis vals
    xvals   = []    # alt. xaxis def: "start stop step"

    dpi     = DEF_dpi
    title   = ""
    figsize = []
    layout  = ""
    fontsize = DEF_fontsize
    fontfamily = DEF_fontfamily
    fontstyles = DEF_fontstyles
    color_table = []
    ncolors = 0
    bkgd_color = DEF_bkgd_color
    boxplot_on = DEF_boxplot_on
    
    # input catchers for censoring
    censor_in_trs   = []
    censor_in_files = []

    # derived values from censoring
    censor_arr   = []     # the final list; this is used
    ncensor      = 0
    censor_width = 0
    censor_RGB   = DEF_censor_RGB
    censor_hline = []

    # derived vals
    all_x  = []
    all_y  = []
    npts   = -1
    ndsets = -1
    all_ymin = 0
    all_ymax = 0

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

    # ----------- opt -----------------

    def set_layout(self, c):
        self.layout = c

    def set_boxplot(self, c):
        self.boxplot_on = c

    def set_bkgd_color(self, c):
        self.bkgd_color = c

    def add_censor_in_trs(self, ss):
        self.censor_in_trs.append(ss)

    def add_censor_in_files(self, ff):
        self.censor_in_files.append(ff)

    def set_censor_RGB(self, c):
        self.censor_RGB = c

    def add_censor_hline(self, hh):
        self.censor_hline.append(hh)

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
                  "all dsets".format(censor_hline[0]))
            for i in range(1,ndsets):
                hh = self.censor_hline[0]
                self.add_censor_hline(hh)
        else:
            sys.exit("** ERROR: number of censor_hlines {} "
                     "must be either 0, 1, or match the "
                     "number of infiles {}".format(nhline, self.ndsets))
  
    def set_censor_arr(self):
        self.censor_width = self.all_x[1] - self.all_x[0]

        # add to this in different forms, where True values mean
        # censoring is flagged to have occured
        cen_arr = np.zeros(self.npts, dtype=bool)

        # combine all strings of censor lists:
        # ... from user-entered strings
        if self.censor_in_trs:
            maxind = self.npts - 1
            for ttt in self.censor_in_trs:
                ll = au.decode_1D_ints(ttt, imax=maxind)
                cen_arr[ll] += True
        # ... from user-entered files
        if self.censor_in_files:
            for fff in self.censor_in_files:
                x = LAD.Afni1D(fff)
                if x.nt != self.npts :
                    sys.exit("** ERROR: len of censor file {} ({}) does not "
                             "match Npts={}".format(fff, x.nt, self.npts))
                q = []
                for i in range(self.npts):
                    val = x.mat[0][i]
                    if not(val):
                        q.append(i)
                q = np.array(q)
                cen_arr[q] += True

        # And finally store
        self.ncensor = np.sum(cen_arr)
        if self.ncensor :
            xarr = np.array(self.all_x)
            self.censor_arr = xarr[cen_arr]
                
    def add_ylabel(self, ylabel):
        self.ylabels.append(ylabel)
        self.nylabels+= 1

    def set_xlabel(self, xlabel):
        self.xlabel = xlabel

    def set_xvals(self, a, b, c):
        self.xvals = [a, b, c]

    def set_title(self, title):
        self.title = title

    def set_dpi(self, dpi):
        self.dpi = int(dpi)

    def set_figsize(self, a, b):
        self.figsize = [a, b]

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
            dat = LAD.Afni1D(fff)
            x = dat.mat[0]
            if x.nvec > 1 :
                sys.exit("** ERROR reading in xvalue file: too many columns.\n"
                         "   Specify just one!")
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

        elif argv[i] == "-help" or argv[i] == "-h" or argv[i] == "-hview":
            print(help_string_apqc_make_tcsh)
            sys.exit(0)

        # ---------- req ---------------

        # can have multiple infiles; must all have same len
        elif argv[i] == "-infiles" or argv[i] == "-yfiles":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if argv[i][0] != "-":
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
                aaa = argv[i][0]
                if aaa != "-":
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
                if argv[i][0] != "-":
                    fsty.append((argv[i]))
                    count+=1
                    i+=1
                else:
                    i-=1
                    break
            iopts.set_fontstyles(fsty)
            if not(count):
                ARG_missing_arg(argv[i])

        elif argv[i] == "-censor_trs":
            count = 0
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if argv[i][0] != "-":
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
                if argv[i][0] != "-":
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
                if argv[i][0] != "-":
                    iopts.add_censor_hline((argv[i]))
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
                if argv[i][0] != "-":
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

        elif argv[i] == "-reverse_order":
            iopts.set_reverse_order(True)

        elif argv[i] == "-boxplot_on":
            iopts.set_boxplot(True)

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: Missing input arguments")
        sys.exit(1)
        
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

    # Each of these is required.  Might have other optional options in
    # the future
    json     = ""
    subjdir  = ""
    revstyle = "basic"

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

        elif argv[i] == "-help" or argv[i] == "-h" or argv[i] == "-hview":
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

    # Each of these is required.  Might have other optional options in
    # the future
    qcdir = ""

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

        elif argv[i] == "-help" or argv[i] == "-h" or argv[i] == "-hview":
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
