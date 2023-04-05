#!/usr/bin/env python

# python3 status: compatible

# Stuff for plotting 2D matrices.
#
# In particular, these funcs are for interacting with 3dNetCorr and
# 3dTrackID outputs.

# --------------------------------------------------------------------------
auth = 'PA Taylor'
#
#ver = '0.0' ; date = 'June 1, 2020'
# [PT] matrix-related things, for plotting, stats and other calcs
#
#ver = '0.1' ; date = 'June 2, 2020'
# [PT] debugged, testing on netcc and grid files
#
#ver = '0.11' ; date = 'June 2, 2020'
# [PT] fix using roi_intvals as labels
#
#ver = '0.12' ; date = 'June 2, 2020'
# [PT] tested in py2 and py3
#    + fixed plotting size stuff with tight layout
#    + put in fix for matplotlib bug for yaxes range
#
#ver = '0.12' ; date = 'June 2, 2020'
# [PT] 
#    + cbar functionality added
#    + funcs for guessing fonts
#
#ver = '0.2' ; date = 'June 2, 2020'
# [PT] 
#    + start to add arg parsing for use
#
#ver = '1.0' ; date = 'June 3, 2020'
# [PT] 
#    + many plotting funcs added
#    + read in user input opts, populate plotting with them
#    + made defaults dict, list input opts and parse all
#
#ver = '1.1' ; date = 'June 4, 2020'
# [PT] 
#    + update cbar link
#    + fix help examples formatting
#    + change def cbar
#
#ver = '1.11' ; date = 'June 4, 2020'
# [PT] 
#    + change def cbar
#    + add a minimum width of figure if we are 'guessing' size
#
#ver = '1.12' ; date = 'June 9, 2020'
# [PT] fix behavior when self.plt_{x,y}ticks_ON is False
#    + that is, when an axis's ticks are turned off, show *nothing*
#
ver = '1.14' ; date = 'July 31, 2020'
# [PT] fix regluing of automatic names (split with '.', and need to
#      rejoin with '.').  done in two places
#
# --------------------------------------------------------------------------

import sys, os, copy, glob

import matplotlib.pyplot       as     plt
import matplotlib.colors       as     clr
from   matplotlib              import rcParams
from   mpl_toolkits.axes_grid1 import make_axes_locatable

from   afnipy import afni_base      as ab
from   afnipy import lib_mat2d_base as lm2b

#cbar_link = 'https://scipy.github.io/old-wiki/pages'
#cbar_link+= '/Cookbook/Matplotlib/Show_colormaps'
cbar_link = 'https://matplotlib.org/stable/tutorials/colors/colormaps.html'

# ---------------------------------------------------------------------------

# defaults
ddefs = {
    'DEF_ver'               : ver,
    'DEF_date'              : date,
    'DEF_auth'              : auth,
    'DEF_ext'               : rcParams["savefig.format"],
    'DEF_dpi'               : 100,
    'DEF_facecolor'         : 'w',
    'DEF_interp'            : 'nearest',
    'DEF_aspect'            : 'equal',
    'DEF_plt_xticks_ON'     : True,
    'DEF_xticks_rot'        : 45,
    'DEF_xticks_ha'         : 'right',
    'DEF_xticks_FS'         : 10,
    'DEF_plt_yticks_ON'     : True,
    'DEF_yticks_rot'        : 0,
    'DEF_yticks_FS'         : 10,
    'DEF_cbar'              : 'RdBu_r', # 'BrBg', 
    'DEF_cbar_link'         : cbar_link,
    'DEF_cbar_width_perc'   : 5,
    'DEF_cbar_pad'          : 0.1,
    'DEF_cbar_loc'          : "right",
    'DEF_cbar_n_interval'   : 4,
    'DEF_do_tight_layout'   : True,
    'DEF_do_hold_image'     : False,
    'DEF_do_show_cbar'      : True,
}

# ----------------------------------------------------------------------------

help_string_mat_plot = '''
  PURPOSE ~1~

  This program plots simple matrices output from 3dNetCorr (*.netcc)
  and 3dTrackID (*.grid). 

  This program has a Python dependency: it requires Python 2.7 or 3.*,
  as well as the 'matplotlib' module.

  Ver  : {DEF_ver}
  Date : {DEF_date}
  Auth : {DEF_auth}

------------------------------------------------------------------------------

INPUTS ~1~

  + A *.netcc or *.grid file output by 3dNetCorr or 3dTrackID,
    respectively.

  + A list of one or more matrices in the file to save.

  Several aspects of the generated image file can be controlled
  (various font sizes, DPI, figure size, etc.), but note that some
  work has gone into trying to "guess" appropriate sizes for the font
  x- and y-tick fonts to be appropriately sizes for column- and
  row-sizes.  So, you might want to first choose your DPI and see how
  things look, and then refine aspects from there.

------------------------------------------------------------------------------

OUTPUTS ~1~

  + Individual image files of matrices; these can contain colorbars,
    as well.

------------------------------------------------------------------------------

RUNNING ~1~

 -input  II     :(req) name of *.netcc or *.grid file with matrices
                 to be plotted

 -pars   PARS   :list of matrices to be plotted, identified by their
                 parameter name.  Can plot one or more.  If no '-pars ..'
                 list is provided, then all matrices in the input file 
                 will be plotted (might plop a lot of plots!)

 -prefix PP     :output basename for image(s).  Note that this can
                 include path information, but both the name of each 
                 matrix (CC, FA, MD, ...) and the file extension (png, 
                 jpg, ...) will be appended to it.  (def: make prefix
                 from the directory+root name of input file)

 -ftype  FT     :filetype, given as extension.  Available filetypes
                 depend slightly on your OS and setup.  (def: chosen
                 by matplotlib.rcParams, which appears to be {DEF_ext})

 -dpi    DPI    :spatial resolution (dots per inch) of output images
                 (def: {DEF_dpi})

 -vmin   MIN    :minimum value of the colorbar (def: min value in matrix)

 -vmax   MAX    :maximum value of the colorbar (def: max value in matrix)

 -fs_xticks FXT :fontsize of ticks along the x-axis (def: {DEF_xticks_FS})

 -fs_yticks FYT :fontsize of ticks along the y-axis (def: {DEF_yticks_FS})

 -fs_title  FT  :fontsize of title (def: let program guess)

 -fs_cbar   FCB :fontsize of colorbar (def: let program guess)

 -cbar_n_intervals NI 
                :number of intervals on colorbars for enumeration purposes.
                 That is, this controls just how many numbers appear along
                 the cbar (which would be NI+1). (def: {DEF_cbar_n_interval})

 -cbar      CB  :name of colorbar to use.  This link contains a name of
                 all available cbars:
                 {DEF_cbar_link}
                 ... and for each, you can add an '_r' as suffix to
                 reverse it. (def: {DEF_cbar})
                 Some perceptually uniform colormaps:
                   viridis, plasma, inferno, magma, cividis
                 Some divergent colormaps:
                   BrBG, RdBu, coolwarm, seismic, bwr
                 
 -cbar_width_perc  CWP 
                :width of cbar as a percentage of the image 
                 (def: {DEF_cbar_width_perc})

 -cbar_off      :colorbar is shown by default; use this opt to turn off 
                 including the colorbar in the image (not recommended)

 -figsize_x FSX :width of created image, in units of inches 
                 (def: guess from size of matrix and x-/y-ticks fontsize)

 -figsize_y FSY :height of created image, in units of inches 
                 (def: guess from size width value, and whether a colorbar
                 is included)

 -hold_image    :in addition to saving an image file, open the image and
                 keep displaying it until a key is pressed in the
                 terminal (def: don't open image immediately)

 -tight_layout  :use matplotlib's "plt.tight_layout()" functionality in 
                 arranging the plot 

 -xticks_off    :don't display labels along the x-axis (def: do display them)

 -yticks_off    :don't display labels along the y-axis (def: do display them)

 -ver           :display version number of program
                 ({DEF_ver})

 -date          :display release/editing date of current version
                 ({DEF_date})

 -help          :display help (in terminal)
 -h             :display help (in terminal)

 -hview         :display help (in separate text editor)

------------------------------------------------------------------------------

EXAMPLES ~1~

0) Plot everything in this netcc file:

   fat_mat2d_plot.py                   \\
       -input REST_corr_rz_003.netcc

1) Plot the CC (correlation coefficient) values between [-1, 1]

   fat_mat2d_plot.py                   \\
       -input REST_corr_rz_003.netcc   \\
       -vmin -1                        \\
       -vmax  1                        \\
       -pars CC

2) Plot the FA (fractional anisotropy) values between [0, 1] using
   the 'cool' colorbar and with a specified prefix:

   fat_mat2d_plot.py                   \\
       -input o.OME_000.grid           \\
       -vmin  0                        \\
       -vmax  1                        \\
       -pars   FA                      \\
       -prefix IMAGE                   \\
       -cbar   cool

3) Plot the MD, L1 and RD values between [0, 3] (-> on a DTI note,
   these values are *probably* in units of x10^-3 mm^2/s, given this
   range) with the reversed Greens colorbar:

   fat_mat2d_plot.py                   \\
       -input o.OME_000.grid           \\
       -vmin  0                        \\
       -vmax  3                        \\
       -pars   MD L1 RD                \\
       -prefix IMAGE2                  \\
       -cbar   Greens_r


'''.format(**ddefs)



# make list and dictionary of all opts, used below in parsing user inputs
opts_list = ab.parse_help_text_for_opts( help_string_mat_plot )
opts_dict  = {} 
for x in opts_list:
    opts_dict[x[1:]] = x

# ---------------------------------------------------------------------------

class iopts_to_plot_mat2d:
    """
    store all the argv-entered things opts to plot
    """

    def __init__( self ) :

        self.full_cmd         = None
        self.input            = None   # input filename
        self.pars_list        = [] 

        self.prefix           = None
        self.ftype            = None
        self.vmin             = None
        self.vmax             = None
        self.fs_xticks        = None
        self.fs_yticks        = None
        self.fs_title         = None
        self.fs_cbar          = None
        self.cbar_n_intervals = None
        self.cbar_width_perc  = None
        self.cbar             = None

        self.figsize_x        = None
        self.figsize_y        = None

        self.tight_layout     = None
        self.hold_image       = None
        self.cbar_off         = None
        self.xticks_off       = None
        self.yticks_off       = None

    # ---------- check ----------------

    def check_req(self):
        ''' Check for and point out any missing inputs.'''
        MISS = 0

        if not(self.input) :
            ab.EP("missing '{input}' dset"
                  "".format(**opts_dict), end_exit=False)
            MISS+=1

        if self.vmin and self.vmax :
            if self.vmax <= self.vmin :
                ab.EP("Bad cbar range: vmax ({}) <= vmin ({})"
                      "".format(self.vmax, self.vmin), end_exit=False)
                MISS+=1

        return MISS

    def finish_defs(self):
        """Check a couple things with input (is exactly 1 file), and sort
        out prefix if necessary.

        """

        lll = glob.glob(self.input)
        if not(lll):
            ab.EP("Could not find file for input: {}"
                  "".format(self.input))
        elif len(lll) > 1 :
            ab.EP("Can only have 1 input file; too many for: {}"
                  "".format(self.input))
        ifile = lll[0]

        if not(self.prefix) :
            # if no prefix is entered, get from name of input
            # [PT: July 31, 2020] fix the regluing
            iroot = '.'.join(ifile.split('.')[:-1])
            if iroot :
                self.prefix = iroot
            else:
                self.prefix = ifile # should never happen

# ------------------------------------------------------------------

def parse_args_mat_plot(full_argv):
    """Go through user-entered options and fill an object with the values.
    These will be used to setup plotting.

    """

    argv = full_argv[1:]
    Narg = len(argv)

    if not(Narg):
        print(help_string_mat_plot)
        sys.exit(0)

    # initialize objs
    iopts          = iopts_to_plot_mat2d()
    iopts.full_cmd = full_argv

    i = 0
    while i < Narg:
        if argv[i] == "{ver}".format(**opts_dict) :
            print(ver)
            sys.exit(0)

        elif argv[i] == "{date}".format(**opts_dict) :
            print(date)
            sys.exit(0)

        elif argv[i] == "{help}".format(**opts_dict) or \
             argv[i] == "{h}".format(**opts_dict) :
            print(help_string_mat_plot)
            sys.exit(0)

        elif argv[i] == "{hview}".format(**opts_dict) :
            prog = os.path.basename(full_argv[0])
            cmd = 'apsearch -view_prog_help {}'.format( prog )
            ab.simple_shell_exec(cmd)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "{input}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.input = argv[i]

        # ---------- opt ---------------

        elif argv[i] == "{prefix}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.prefix = argv[i]

        # can be a list of many
        elif argv[i] == "{pars}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            while i < Narg:
                if opts_list.__contains__(argv[i]) :
                    i-= 1
                    break
                else:
                    iopts.pars_list.append(argv[i])
                    i+= 1

        elif argv[i] == "{ftype}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.ftype = argv[i]

        elif argv[i] == "{dpi}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.dpi = float(argv[i])

        elif argv[i] == "{vmin}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.vmin = float(argv[i])

        elif argv[i] == "{vmax}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.vmax = float(argv[i])

        elif argv[i] == "{fs_xticks}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.fs_xticks = float(argv[i])

        elif argv[i] == "{fs_yticks}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.fs_yticks = float(argv[i])

        elif argv[i] == "{fs_title}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.fs_title = float(argv[i])

        elif argv[i] == "{fs_cbar}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.fs_cbar = float(argv[i])

        elif argv[i] == "{cbar_n_intervals}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.cbar_n_intervals = int(argv[i])

        elif argv[i] == "{cbar}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.cbar = argv[i]

        elif argv[i] == "{cbar_width_perc}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.cbar_width_perc = float(argv[i])

        elif argv[i] == "{figsize_x}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.figsize_x = float(argv[i])

        elif argv[i] == "{figsize_y}".format(**opts_dict) :
            if i >= Narg:
                ab.ARG_missing_arg(argv[i])
            i+= 1
            iopts.figsize_y = float(argv[i])


        elif argv[i] == "{hold_image}".format(**opts_dict) :
            iopts.hold_image = True

        elif argv[i] == "{tight_layout}".format(**opts_dict) :
            iopts.tight_layout = True

        elif argv[i] == "{cbar_off}".format(**opts_dict) :
            iopts.cbar_off = True

        elif argv[i] == "{xticks_off}".format(**opts_dict) :
            iopts.xticks_off = True

        elif argv[i] == "{yticks_off}".format(**opts_dict) :
            iopts.yticks_off = True

        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1


    if iopts.check_req():
        print("   -------------------------------")
        ab.EP("Problem with input arguments. See detailed whining above.")

    iopts.finish_defs()

    return iopts

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

class plot_mat2d:

    """A class for plotting a 2D matrix;  need not be square.  

    For cbars, see:
    https://scipy.github.io/old-wiki/pages/Cookbook/Matplotlib/Show_colormaps
    ... and note that any colorbar there can have a "_r" as a suffix,
    to reverse it.

    """

    def __init__( self, m=None, fout='' ):

        self.m         = None
        
        self.file_out  = 'IMAGE'
        self.fout_base = ''
        #self.fout_ext  = ''

        self.figsize   = None  # (w, h) in inches; now guess good
        self.dpi       = ddefs['DEF_dpi']
        self.facecolor = ddefs['DEF_facecolor']
        self.interp    = ddefs['DEF_interp']
        self.aspect    = ddefs['DEF_aspect']
        self.vmin      = None
        self.vmax      = None

        self.plt_xticks_ON     = ddefs['DEF_plt_xticks_ON']
        self.plt_xticks_ticks  = None
        self.plt_xticks_labels = None
        self.plt_xticks_N      = 0
        self.plt_xticks_rot    = ddefs['DEF_xticks_rot']
        self.plt_xticks_ha     = ddefs['DEF_xticks_ha']
        self.plt_xticks_FS     = ddefs['DEF_xticks_FS']

        self.plt_yticks_ON     = ddefs['DEF_plt_yticks_ON']
        self.plt_yticks_ticks  = None
        self.plt_yticks_labels = None
        self.plt_yticks_N      = 0
        self.plt_yticks_rot    = ddefs['DEF_yticks_rot']
        self.plt_yticks_FS     = ddefs['DEF_yticks_FS']

        self.plt_title_txt     = ''
        self.plt_title_FS      = None

        self.cbar              = ddefs['DEF_cbar']
        self.cbar_width_perc   = ddefs['DEF_cbar_width_perc']
        self.cbar_pad          = ddefs['DEF_cbar_pad' ]
        self.cbar_loc          = ddefs['DEF_cbar_loc']
        self.cbar_n_interval   = ddefs['DEF_cbar_n_interval']
        self.cbar_num_form     = None   # try to guess int/float from vals
        self.cbar_FS           = None   # try to guess 
        self.do_show_cbar      = ddefs['DEF_do_show_cbar']

        self.do_tight_layout   = ddefs['DEF_do_tight_layout']
        self.do_hold_image     = ddefs['DEF_do_hold_image']

        # -------------
        
        if m :
            # can get started a matrix m is added
            self.set_mat2d_m( m )

            # set output filename (find or create ext)
            if fout :
                self.set_file_out( fout )
            else:
                self.set_file_out( self.file_out + '_' + self.m.label )

    # -------------------------------------------------------------

    def apply_iopts(self, iopts):
        """
        Apply all user-entered opts that we know how to.
        """
        #iopts.full_cmd         = None
        #iopts.input            = None   # input filename
        #iopts.pars_list        = [] 

        if iopts.prefix :
            name = iopts.prefix + '_' + self.m.label
            if iopts.ftype :
                name+= '.' + iopts.ftype
            self.set_file_out( name )

        #iopts.full_cmd         = None
        #iopts.input            = None   # input filename
        #iopts.pars_list        = [] 

        if iopts.vmin != None :
            self.vmin = iopts.vmin

        if iopts.vmax != None :
            self.vmax = iopts.vmax

        if iopts.xticks_off != None:
            self.plt_xticks_ON = False
            
        if iopts.yticks_off != None:
            self.plt_yticks_ON = False

        if iopts.fs_xticks != None:
            self.plt_xticks_FS = iopts.fs_xticks

        if iopts.fs_yticks != None:
            self.plt_yticks_FS = iopts.fs_yticks
            
        if iopts.fs_title != None :
            self.plt_title_FS = iopts.fs_title

        if iopts.cbar != None:
            self.cbar = iopts.cbar

        if iopts.fs_cbar != None:
            self.cbar_FS = iopts.fs_cbar

        if iopts.cbar_n_intervals != None:
            self.cbar_n_intervals = iopts.cbar_n_intervals

        if iopts.cbar_width_perc != None:
            self.cbar_width_perc = iopts.cbar_width_perc

        if iopts.tight_layout != None:
            self.do_tight_layout = True

        if iopts.hold_image != None:
            self.do_hold_image = True

        if iopts.cbar_off != None:
            self.do_show_cbar = False

        # this one has to be after we know if the cbar is on/off, for
        # case when only one of (width, height) is shown
        if iopts.figsize_x != None and iopts.figsize_y != None :
            self.figsize = ( iopts.figsize_x, iopts.figsize_y )
        elif iopts.figsize_x != None :
            self.figsize = ( iopts.figsize_x, 
                             self.calc_fig_h_from_w(iopts.figsize_x) )
        elif iopts.figsize_y != None :
            self.figsize = ( self.calc_fig_w_from_h(iopts.figsize_y),
                             iopts.figsize_y )

    def calc_fig_w_from_h(self, H ):
        """Given height, calc appropriate width; basically just depends if
        cbar is shown or not.

        """

        if self.do_show_cbar:  return 1.2*H
        else:                  return H

    def calc_fig_h_from_w(self, W ):
        """Given height, calc appropriate width; basically just depends if
        cbar is shown or not.

        """

        if self.do_show_cbar:  return W/1.2
        else:                  return H

    def set_mat2d_m(self, mm ):
        """Read in the mat2d info, and initialize lots of other attributes
        from it.

        """
        self.read_mat2d( mm )
        self.set_plopts_from_mat2d()

    def set_file_out(self, FF):
        """Set output file name, and parse for ext (or decide on one, if none
        is given.

        """      

        if not(FF) :
            ab.EP("No output file given for me to work with?")

        ff_split = FF.split('.')
        ext      = ff_split[-1]
        
        if len(ff_split) < 2 or \
           not(['bmp', 'emf', 'eps', 'gif', 'jpeg', 'jpg', \
                'pdf', 'pgf', 'png', 'ps', 'raw', 'rgba', \
                'svg', 'svgz', 'tif', 'tiff'].__contains__(ext)) : 
            # fname is just base; need to add our own ext
            self.ext       = rcParams["savefig.format"]
            self.fout_base = FF 
            self.file_out  = '.'.join([self.fout_base, self.ext])
        else:
            # fname is complete 
            # [PT: July 31, 2020] fix joining here, need dot in case 
            #                     path has dots...
            self.ext       = ext
            self.fout_base = '.'.join(ff_split[:-1])
            self.file_out  = '.'.join([self.fout_base, self.ext])
    
    def read_mat2d( self, MM ):
        """Just read in the mat2d obj
        """
    
        self.m = MM

    def set_plopts_from_mat2d(self):
        
        # range of plot
        if self.vmin == None :
            self.vmin = self.m.min_ele
        if self.vmax == None :
            self.vmax = self.m.max_ele

        # yticks/labels
        if self.m.col_strlabs :
            self.plt_yticks_labels = self.m.col_strlabs
        elif self.m.col_intvals :
            self.plt_yticks_labels = [str(w) for w in self.m.col_intvals]
        else: 
            self.plt_yticks_labels = ['']*self.m.ncol # shouldn't happen
        self.plt_yticks_N      = len(self.plt_yticks_labels)
        self.plt_yticks_ticks  = [i for i in range(self.plt_yticks_N) ]

        # xticks/labels
        if self.m.row_strlabs :
            self.plt_xticks_labels = self.m.row_strlabs
        elif self.m.row_intvals :
            self.plt_xticks_labels = [str(w) for w in self.m.row_intvals]
        else: 
            self.plt_xticks_labels = ['']*self.m.nrow # shouldn't happen
        self.plt_xticks_N      = len(self.plt_xticks_labels)
        self.plt_xticks_ticks  = [i for i in range(self.plt_xticks_N) ]
        
        # plot title
        self.plt_title_txt = self.m.label

    def guess_appropriate_figsize(self, min_width=3.5):
        """Use fontsize and number of rows stacked in y-dir to figure out what
        a good overall height.  Basically, we want the fontsize on the
        yticks to be just a bit less than the col width.

        The factor of 0.7 comes from practical experience with about
        the fraction of the height taken up by the plot in the
        vertical direction when things are fine.

        Re. width: make plot approx. square if no cbar is used; add on
        about 20% extra width if a cbar is used.

        We now put a floor on the width; anything less than 3.5 in
        seems pointless.

        """
        
        ### "actual" formula, but bc matplotlib fontsize uses PPI, not
        ### DPI, then the DPI factors cancel!
        #num = self.plt_yticks_N * 1.2*self.plt_yticks_FS * (self.dpi / 72.)
        #den = 0.7 * self.dpi
        #height = num / den

        ### here, the factor of 2 includes the 'fudge factor' on row
        ### height and the denominator factor of 0.7, as well as a
        ### bit extra padding
        height = 2 * self.plt_yticks_N * self.plt_yticks_FS / 72.

        width  = self.calc_fig_w_from_h(height)

        if width < min_width :
            ab.IP("Invoke min width: {}".format(min_width))
            width  = min_width
            height = self.calc_fig_h_from_w(width)

        ab.IP("figsize (h, w) guess (in): {:.2f}, {:.2f}"
              "".format(height, width))

        self.figsize = (width, height)

    def guess_appropriate_cbar_fontsize(self):
        """Similar goal/premise as guess_appropriate_figsize().  This if for
        the cbar values.  Assumes the fig height is known at this
        point (so would have to be calc'ed after
        guess_appropriate_figsize(), potentially.

        """

        ###  some other ideas that didn't end up getting used...
        #n_pix_fig_y = self.dpi * self.figsize[1]
        #n_cbar_vals = self.cbar_n_interval+1
        #n_pix_cbar_yticks = self.plt_yticks_FS * n_cbar_vals 
        #n_pix_cbar_yticks*= (self.dpi / 72.)

        #guess1 = 0.8 * self.plt_yticks_FS 

        good_val = 2 + (0.7 / 32 ) * 100 * self.figsize[1]
        ab.IP("cbar fontsize guess: {:.2f}".format(good_val))

        self.cbar_FS = good_val

    def guess_appropriate_title_fontsize(self):
        """Similar goal/premise as guess_appropriate_figsize().  This if for
        the cbar values.  Assumes the fig height is known at this
        point (so would have to be calc'ed after
        guess_appropriate_figsize(), potentially.

        """

        good_val = 3 + 1.1*(0.7 / 32 ) * 100 * self.figsize[1]
        ab.IP("title fontsize guess: {:.2f}".format(good_val))

        self.plt_title_FS = good_val

    def make_plot(self):
        """
        Construct the plot obj
        """

        if not(self.figsize) :
            # if one isn't entered, we'll try to *guess* a good one,
            # based on fontsize, number of cols, etc. and whether
            # there is a cbar
            self.guess_appropriate_figsize()

        if not(self.cbar_FS) :
            self.guess_appropriate_cbar_fontsize()

        if not(self.plt_title_FS) :
            self.guess_appropriate_title_fontsize()


        fig = plt.figure( figsize=self.figsize, 
                          dpi=self.dpi, 
                          facecolor=self.facecolor)
        subb = plt.subplot(111)

        ax   = plt.gca()
        box  = ax.get_position()

        IM = ax.imshow( self.m.mat,
                        interpolation = self.interp,
                        aspect        = self.aspect,
                        vmin          = self.vmin,
                        vmax          = self.vmax,
                        cmap          = self.cbar )

        if self.plt_xticks_ON :
            plt.xticks( self.plt_xticks_ticks,
                        self.plt_xticks_labels,
                        rotation = self.plt_xticks_rot,
                        ha       = self.plt_xticks_ha,
                        fontsize = self.plt_xticks_FS )
        else:
            plt.tick_params( axis='x',
                             which='both',
                             bottom=False,      
                             top=False,         
                             labelbottom=False ) 

        if self.plt_yticks_ON :
            plt.yticks( self.plt_yticks_ticks,
                        self.plt_yticks_labels,
                        rotation = self.plt_yticks_rot,
                        fontsize = self.plt_yticks_FS )
        else:
            plt.tick_params( axis='y',
                             which='both',
                             left=False,      
                             right=False,         
                             labelleft=False ) 

        # this is do deal with an apparent bug in some versions of
        # matplotlib:
        # https://github.com/matplotlib/matplotlib/issues/14751
        ax.set_ylim(self.plt_yticks_N-0.5, -0.5)

        plt.title( self.plt_title_txt,
                   fontsize=self.plt_title_FS )

        if self.do_show_cbar :
            cbar_wid = "{}%".format(self.cbar_width_perc)
            divider  = make_axes_locatable(ax)
            TheCax   = divider.append_axes(self.cbar_loc, 
                                           size = cbar_wid, 
                                           pad  = self.cbar_pad )
            ntick    = self.cbar_n_interval + 1
            del_cbar = (self.vmax - self.vmin) / self.cbar_n_interval 
            ColBar   = [(self.vmin+del_cbar*ii) for ii in range(ntick) ]

            if self.cbar_num_form :
                cbar_num_form = self.cbar_num_form
            elif ab.list_count_float_not_int(ColBar) :
                # looks like floats
                max_cbar = max(ColBar)
                if max_cbar < 0.01 :
                    cbar_num_form = '%.2e'
                elif max_cbar < 10 :
                    cbar_num_form = '%.3f'
                elif max_cbar < 1000 :
                    cbar_num_form = '%.1f'
                else: 
                    cbar_num_form = '%.2e'
            else:
                # looks like ints
                max_cbar = max(ColBar)
                if max_cbar < 1000 :
                    cbar_num_form = '%d'
                else: 
                    cbar_num_form = '%.2e'

            cbar     = plt.colorbar( IM, 
                                     cax    = TheCax, 
                                     ticks  = ColBar, 
                                     format = cbar_num_form )
            cbar.ax.tick_params(labelsize=self.cbar_FS) 
        
        if self.do_tight_layout :
            plt.tight_layout()
        else:
            subb.set_position([ box.x0+box.width*0.0, 
                                box.y0+box.height*0.05,  
                                box.width*0.95, 
                                box.height*0.95 ])

        # ------------ save -----------
        plt.savefig( self.file_out,
                     dpi = self.dpi )
        ab.IP("Made plot: {}".format(self.file_out))

        # ------------ display -------------
        if self.do_hold_image :
            plt.ion()
            plt.show()

            try:
                input()
            except:
                raw_input()



