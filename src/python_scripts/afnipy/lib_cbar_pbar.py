#!/usr/bin/env python


# A library of functions for taking an existing colorbar (cbar) and
# outputting a new one with enhanced information, like a threshold
# line and Alpha fading.  This was suggested and first demo'ed by Dylan
# Nielson---thanks!
#
# Terminology note: we variously refer to 'pbar' and 'cbar'
# here. These are just the same thing, the 'palette bar' and 'color
# bar', respectively.
#
# This could be expanded at some point to read in a palette file or a
# single array of numbers/colors. We also plan to add in things like
# adding a boundary in various ways, etc.
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.5 : added help docs, tickmark control opts
# ============================================================================

import sys, os, copy
import json
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcol

from   afnipy     import afni_base as ab

# ============================================================================

# allowed values of alpha ('Yes' is equivalent to 'Quadratic')
list_alpha      = ['No', 'Yes', 'Quadratic', 'Linear']
list_alpha_str  = ', '.join(list_alpha)

# Alpha blend color: what do we blend with when Alpha is on?
abc = '#c8c8c8'  ### np.array([200, 200, 200], dtype=np.uint8)   # light gray

# the color(s) of tickmarks and the threshold line
zer = np.array([0, 0, 0], dtype=np.uint8)         # black
rez = np.array([255, 255, 255], dtype=np.uint8)   # white

DO_AUTOROTATE = True

# dict of default cbar props to check for; most/all keys match names
# of some of the keys within the pbar JSON output by @chauffeur_afni
cbar_props = { 
    'pbar_bot'   : None,
    'pbar_top'   : None,
    'vthr'       : None,
    'olay_alpha' : 'No',
    }

# default values for the main PbarCbar obj
DOPTS = {
    'user_opts'     : [],         # command the user ran
    'in_pbar'       : '',
    'prefix'        : '',
    'pbar_min'      : None,
    'pbar_max'      : None,
    'alpha'         : 'No',
    'thr_on'        : True,
    'thr_val'       : None,
    'thr_width'     : 4,
    'thr_num_osc'   : 4,
    'thr_colors'    : [zer, rez],
    'tick_num_int'  : 10,
    'tick_frac'     : 0.07,
    'tick_color'    : zer,
    'orth_on'       : False,
    'orth_frac'     : 1.0,
    'outline_width' : 0,
    'outline_color' : zer,
    'bkgd_color'    : abc,
    'do_autorotate' : True,
    'verb'          : 1,
}


# ============================================================================

class CbarPbar:
    """Object for storing information about a cbar/pbar, and for
controlling behavior of editing it with various thresholding,
outlines, and more.

Parameters
----------
inobj : InOpts object 
    object constructed from running cbar_pbar.py on the command line. At 
    present, the only way to really provide inputs here.

    """

    def __init__(self, user_inobj=None):

        # main quantities to be populated
        self.cbar_arr        = []

        # main input variables
        self.status          = 0                       # not used
        self.user_opts       = DOPTS['user_opts']      # command the user ran
        self.user_inobj      = user_inobj

        # main data variables
        self.in_pbar         = DOPTS['in_pbar']
        self.prefix          = DOPTS['prefix']

        # pbar properties from @chauffeur_afni pbar JSON
        self.pbar_min        = DOPTS['pbar_min']
        self.pbar_max        = DOPTS['pbar_max']
        self.thr_val         = DOPTS['thr_val']
        self.alpha           = DOPTS['alpha']

        # threshold line
        self.thr_on          = DOPTS['thr_on']
        self.thr_width       = DOPTS['thr_width']
        self.thr_num_osc     = DOPTS['thr_num_osc']
        self.thr_colors      = DOPTS['thr_colors']

        # tick properties
        self.tick_num_int    = DOPTS['tick_num_int']
        self.tick_frac       = DOPTS['tick_frac']
        self.tick_color      = DOPTS['tick_color']

        # control orthogonality (e.g., when olay and thr are diff dsets)
        self.orth_on         = DOPTS['orth_on']
        self.orth_frac       = DOPTS['orth_frac']

        # outline properties
        self.outline_width   = DOPTS['outline_width']
        self.outline_color   = DOPTS['outline_color']

        # background properties
        self.bkgd_color      = DOPTS['bkgd_color']

        # control functionality
        self.do_autorotate   = DOPTS['do_autorotate']
        self.DID_autorotate  = False

        # general variables
        self.verb            = DOPTS['verb']

        # ----- do methods
        
        # run through all actions, if user_inobj has been provided as input
        if user_inobj :
            tmp1 = self.load_from_inopts()
            tmp2 = self.check_required_inputs()
            tmp2 = self.check_valid_opts()
            tmp3 = self.check_files_exist()
            tmp4 = self.read_cbar_file()
            tmp5 = self.proc_cbar()
            tmp6 = self.write_cbar()

    # ----------------------------

    def write_cbar(self):
        """not worrying about overwriting yet"""

        # write the new cbar to disk
        if self.prefix :
            plt.imsave(self.prefix, self.cbar_arr)

        return 0

    def proc_cbar(self):
        """Do the main work of thresholding and/or blending the cbar, either
        along the color gradient or orthogonally. Also add any ticks
        or threshold lines."""

        # do blending with cbar background
        if self.orth_on :
            tmp1 = self.blend_cbar_orth()
        else:
            tmp1 = self.blend_cbar_along()

        # add ticks, possibly
        if self.tick_num_int :
            tmp2 = self.add_ticks_to_cbar()

        # add threshold line
        if self.thr_on :
            tmp3 = self.add_thr_to_cbar()

        # add outline
        if self.outline_width :
            tmp4 = self.add_outline_to_cbar()

        # was it autorotated earlier? then unrotate it back here
        if self.DID_autorotate :
            self.cbar_arr = np.rot90(self.cbar_arr, k=1)

        return 0

    def add_outline_to_cbar(self):
        """add outline to cbar, if asked"""

        X = copy.deepcopy(self.cbar_arr)
        W, N, rgb = np.shape(X)             # cbar dimensions

        outwid = self.outline_width

        # ----- init output RGB cbar array
        Y   = np.zeros((W, N, rgb), dtype=np.uint8)

        # ----- init output RGB cbar array
        W2 = W + 2*outwid
        N2 = N + 2*outwid
        Y  = np.zeros((W2, N2, rgb), dtype=np.uint8)
        # ... and initialize the color to be the outline col
        Y[:,:,:] = self.outline_color

        # copy the values of X into the middle in one, Pythonic jump
        Y[outwid:outwid+W,outwid:outwid+N,:] = X

        self.cbar_arr = copy.deepcopy(Y)
        return 0

    def add_thr_to_cbar(self):
        """add threshold lines to the cbar, if present"""

        if not(self.thr_on) :
            return 0

        X = copy.deepcopy(self.cbar_arr)
        W, N, rgb = np.shape(X)             # cbar dimensions

        # actual vals at each cbar loc
        allv = np.linspace(self.pbar_min, self.pbar_max, N)

        # width of the dashes
        wdash = W // (2*self.thr_num_osc) 

        diffs = np.sign(np.abs(allv) - self.thr_val)
        i = 1
        while i < N :
            if diffs[i]*diffs[i-1] < 0 :
                bmin = i - (self.thr_width // 2)
                bmax = bmin + self.thr_width
                X[:, bmin:bmax, :] = self.thr_colors[0]
                for j in range(W):
                    if np.floor(j/wdash) % 2 :
                        X[j, bmin:bmax, :] = self.thr_colors[1]
                i = bmax+1
            else:
                i+= 1

        self.cbar_arr = copy.deepcopy(X)
        return 0

    def add_ticks_to_cbar(self):
        """if number of ticks is nonzero, add them"""

        # nothing to do
        if not(self.tick_num_int) :
            return 0

        X = copy.deepcopy(self.cbar_arr)
        W, N, rgb = np.shape(X)             # cbar dimensions

        # tick properties
        nmark = N // self.tick_num_int      # interval of each tick
        wmark = int(W * self.tick_frac)     # width of each tick

        # add ticks
        for i in range(N):
            if i % nmark == 0 and i and i < (N-0.5*nmark) :
                X[:wmark, i, :]   = self.tick_color
                X[W-wmark:, i, :] = self.tick_color

        self.cbar_arr = copy.deepcopy(X)
        return 0

    def blend_cbar_along(self):
        """Blending of cbar color, along the color grad. This will often
        be used when the olay and thr volumes differ, for example."""

        # make sure 
        if not(self.alpha in [None, 'No']) and \
           self.pbar_min is None and self.pbar_max is None and \
           self.thr_val is None :
            ab.EP("""You said you wanted Alpha on, but didn't provide enough 
            info for it: need pbar_min, pbar_max and thr_val""")

        X = copy.deepcopy(self.cbar_arr)
        W, N, rgb = np.shape(X)             # cbar dimensions

        # ----- init output RGB cbar array
        Y = np.zeros((W, N, rgb), dtype=np.uint8)

        # init weight array along dim of length N: no alpha
        wtN = np.ones(N, dtype=float)
        # actual vals at each cbar loc
        allv = np.linspace(self.pbar_min, self.pbar_max, N)

        # make weight (wtN, vals in [0, 1]) so fading will be along grad
        for i in range(N):
            rat = np.abs(allv[i] / self.thr_val)
            if self.alpha is None :
                # no threshold: wtN[i] remains 1
                continue
            elif self.alpha == 'No' :
                # opaque threshold: wtN[i] is 1 or 0
                wtN[i] = min(max(int(rat), 0.0), 1.0)
            else:
                # transparent thresholding cases (linear or quad fading)
                wtN[i] = min(max(rat, 0.0), 1.0)
                if self.alpha == 'Yes' or self.alpha == 'Quadratic' :
                    # quadratic Alpha situation
                    wtN[i]**= 2

        # apply fading is parallel to cbar gradient
        for i in range(N):
            if wtN[i] < 1.0 :
                new_rgb = wtN[i]*X[W//2, i, :] + (1.0-wtN[i])*self.bkgd_color
            else:
                new_rgb = X[W//2, i, :]
            for j in range(W):
                Y[j, i, :] = new_rgb.astype(np.uint8)

        self.cbar_arr = copy.deepcopy(Y)
        return 0

    def blend_cbar_orth(self):
        """Blending of cbar color, orthogonal to color grad. This will often
        be used when the olay and thr volumes differ, for example."""

        # nothing to do here, if no alpha is used
        if self.alpha == 'No' :
            return 0

        X = copy.deepcopy(self.cbar_arr)
        W, N, rgb = np.shape(X)             # cbar dimensions

        # ----- init output RGB cbar array
        Y = np.zeros((W, N, rgb), dtype=np.uint8)

        # init weight array along dim of length W: no alpha
        wtW = np.ones(W, dtype=float)
        # make weight (wtW, vals in [0, 1]) so fading will be orth to grad
        allvW = np.linspace(1.0, 0.0, W)

        # calculate color-blend weight (wt), restricted to [0,1]
        for j in range(W):
            rat    = np.abs(allvW[j] / self.orth_frac)
            wtW[j] = min(max(rat, 0.0), 1.0)
            if self.alpha == 'Yes' or self.alpha == 'Quadratic' :
                # quadratic Alpha situation
                wtW[j]**= 2

        # when fading is orthogonal to cbar gradient
        for i in range(N):
            # get base color from middle/spine of cbar
            base_rgb = X[W//2, i, :]
            for j in range(W):
                if wtW[j] < 1.0 :
                    new_rgb = wtW[j]*base_rgb + (1.0-wtW[j])*self.bkgd_color
                else:
                    new_rgb = base_rgb
                Y[j, i, :] = new_rgb.astype(np.uint8)

        self.cbar_arr = copy.deepcopy(Y)
        return 0

    def read_cbar_file(self):
        """Read in the cbar from a file to an array of uint8 RGB values."""

        self.cbar_arr = read_cbar(self.in_pbar)

        # do we need to rotate it? record status to unrotate at end, too
        if self.do_autorotate :
            self.DID_autorotate, self.cbar_arr = \
                autorotate_cbar(self.cbar_arr)

        return 0

    def check_valid_opts(self):
        """Check a bunch of inputs/attributes for having appropriate values.
        For example, pretty much every *_color attribute should be
        passed through here."""

        # alpha value must be known keyword
        if not(self.alpha in list_alpha) :
            ttt = "The input alpha value is not recognized: {}. "
            ttt+= "It must be one of: {}".format(self.alpha, list_alpha_str)
            ab.EP(ttt)

        # colors must be in appropriate format

        # threshold color list
        if len(self.thr_colors) :
            self.thr_colors = [set_color_to_rgb(x) for x in self.thr_colors]
            if len(self.thr_colors) == 1 :
                # internally, always want this to have 2 colors
                self.thr_colors.append(self.thr_colors[0])

        # tick color
        if self.tick_color is not None :
            self.tick_color = set_color_to_rgb(self.tick_color)

        # outline color
        if self.outline_color is not None :
            self.outline_color = set_color_to_rgb(self.outline_color)

        # outline width must be an integer
        if self.outline_width :
            width = max(int(self.outline_width), 0)
            if not(width) :
                ttt = "A nonzero width '{}' was ".format(self.outline_width)
                ttt+= "entered, but the positive-int part of that is 0. "
                ttt+= "So, we won't add any outline to the pbar."
                ab.WP(ttt)
            self.outline_width = width

        # background color
        if self.bkgd_color is not None :
            self.bkgd_color = set_color_to_rgb(self.bkgd_color)

        # when the fading is orthogonal, turn threshold off
        if self.orth_on :
            self.thr_on = False

        # *** add more over time ***

        return 0

    def check_required_inputs(self):
        """Make sure that a necessary minimum set of items has been
        provided."""
      
        if self.in_pbar is None :
            ttt = "User is missing input pbar name: see '-in_pbar ..', "
            ttt+= "and please try again."
            ab.EP(ttt)

        if self.prefix is None :
            ttt = "User is missing output pbar name: see '-prefix ..', "
            ttt+= "and please try again."
            ab.EP(ttt)

        if self.pbar_min is None and not(self.orth_on) :
            ttt = "User is missing min pbar value: see '-pbar_min ..' "
            ttt+= "or '-in_json ..', and please try again."
            ab.EP(ttt)

        if self.pbar_max is None  and not(self.orth_on):
            ttt = "User is missing max pbar value: see '-pbar_min ..' "
            ttt+= "or '-in_json ..', and please try again."
            ab.EP(ttt)

        return 0

    def check_files_exist(self):
        """For any file that might be input, check that it exists."""
      
        all_fname = [self.in_pbar]

        for fname in all_fname :
            if not(os.path.isfile(fname)) :
                ttt = "User input file {} does not exist. ".format(fname)
                ttt+= "Please check path and try again."
                ab.EP(ttt)

        return 0

    def load_from_inopts(self):
        """Populate the input values using the command line interface
        input. The user information is provided as the self.user_inobj
        object, which gets parsed and redistributed here.
        """
        
        if not(self.user_inobj) :
            ab.WP("No user_inobj? Nothing to do.")
            return 0

        # shorter name to use, and less confusing with 'self' usage
        io = self.user_inobj

        if io.user_opts is not None :
            self.user_opts = io.user_opts
        if io.in_pbar is not None :
            self.in_pbar = io.in_pbar
        if io.prefix is not None :
            self.prefix = io.prefix

        if io.pbar_min is not None :
            self.pbar_min = io.pbar_min
        if io.pbar_max is not None :
            self.pbar_max = io.pbar_max

        if io.alpha is not None :
            self.alpha = io.alpha

        if io.thr_val is not None :
            self.thr_val = io.thr_val
        if io.thr_on is not None :
            self.thr_on = io.thr_on
        if io.thr_width is not None :
            self.thr_width = io.thr_width
        if io.thr_num_osc is not None :
            self.thr_num_osc = io.thr_num_osc
        if len(io.thr_colors) :
            self.thr_colors = io.thr_colors

        if io.tick_num_int is not None :
            self.tick_num_int = io.tick_num_int
        if io.tick_frac is not None :
            self.tick_frac = io.tick_frac
        if io.tick_color is not None :
            self.tick_color = io.tick_color

        if io.orth_on is not None :
            self.orth_on = io.orth_on
        if io.orth_frac is not None :
            self.orth_frac = io.orth_frac

        if io.outline_width is not None :
            self.outline_width = io.outline_width
        if io.outline_color is not None :
            self.outline_color = io.outline_color

        if io.bkgd_color is not None :
            self.bkgd_color = io.bkgd_color

        if io.verb is not None :
            self.verb = io.verb

        return 0

# -----------------------------------------------------------------------

def autorotate_cbar(X, verb=1):
    """Check if input array X should be rotated for processing.  Will
likely be un-autorotated at the end.  Rotates clockwise by 90deg.

Parameters
----------
X : np.array
    a (width)x(length)x3 RGB array of colors. Each element is 
    in the range [0, 255], and is of type np.uint8.

Returns
-------
DID_ROTATE : bool
    Did we rotate the array?  Recorded and reported here
Y: np.array
    Either a copy of X or a rotated version of it.
"""

    # default scenario
    Y = copy.deepcopy(X)
    DID_ROTATE = False

    R, C, RGB = np.shape(X)
    if R > C :
        if verb :
            ab.IP("Autorotating input colorbar for processing")
        Y = np.rot90(X, k=-1)
        DID_ROTATE = True

    return DID_ROTATE, Y

def read_cbar(fname):

    """Read in colorbar (JPG, PNG, etc.), via filename fname. 

When read in, we assume that the dimensions translate to width x
length x RGB. That seems to be the case with all AFNI-created
colorbars. For future generalizations, we might need to add in a
transpose opt, for example.

We now check whether the RGB values are scaled between [0,1] or
[0,255]. If the former, we will upscale them to the latter and type of
np.uint8.

Parameters
----------
fname : str
    filename of colorbar

Returns
-------
arr : np.array
    a (width)x(length)x3 RGB array of colors. Each element is 
    in the range [0, 255], and is of type np.uint8.

    """

    if not(os.path.exists(fname)) :
        ab.EP("Cannot find cbar file: {}".format(fname))

    arr = plt.imread(fname)

    # at present if input is RGBA, ignore A---might add in to deal
    # with this later. This effectively means A=1 for all inputs.
    try:
        # we expect input to be have 3 dims
        A,B,C = np.shape(arr)
        if C == 3 :
            pass
        elif C == 4 :
            ttt = "Input cbar appears to have RGBA info, not just RGB. \n"
            ttt+= "Just note that we will ignore the 'A', effectively \n"
            ttt+= "reading it in as A=1 throughout.\n"
            ttt+= "-> Not sure these are always dealt with correctly!"
            ab.WP(ttt)
            arr = copy.deepcopy(arr[:,:,:3])
        else:
            ab.EP("""The third dimension of the input cbar is neither\n
            3 (for RGB) or 4 (for RGBA). We don't know what to do with it.""")
    except:
        A = np.shape(arr)
        LA = len(A)
        ab.EP("""Input cbar should have 3 dims (height, width and color),
        but this one has '{}'. Don't know what to do with that.
        """.format(LA))
        
    # check if the RGB values are actually in [0,1] scale format.
    # if so, scale them up to [0,255] range, and fix type to uint8
    if np.max(arr) <= 1 :
        arr = (arr * 255 + 0.1).astype(np.uint8)

    return arr

def read_json(fname):
    """Read in a JSON file and return it as a dictionary/list/etc.

Parameters
----------
fname : str
    filename of JSON file 

Returns
-------
jdict : dict
    the translation of the JSON file into an object in memory; in
    present applications, likely a dictionary

    """

    if not(os.path.exists(fname)) :
        ab.EP("Cannot find cbar json: {}".format(fname))

    with open(fname, 'r') as fff:
        jdict = json.load(fff)    
    return jdict

def set_color_to_rgb(x):
    """Take input color x and translate it to an RGB representation,
specifically an array of length=3, with dtype=uint8 and value in range
[0, 255].  We expect most values of x to be strings, but we allow for
the fact that they could be:
+ a list or an array of 3 or 4 values (NB: for the latter, the index=3
  value will be ignored at present)
+ those values could be in range [0, 1] or [0, 255]

Parameters
----------
x : str, or np.array or list of len 3 or 4
    string name or RGB value of a color

Returns
-------
rgb : np.array of len=3 and dtype=uint8
    our RGB value to return in the correct format
"""

    # if it is a string, pre-convert it to an ordered collection of 4
    # RGB values in range [0,1]
    if type(x) == str :
        x = mcol.to_rgba(x)

    # error string that might get output a couple times
    estring = """The input color type/format is not recognized: '{}'.  
    Please read the help for set_color_to_rgb() and try again.""".format(x)

    # now, x should/must be some ordered collection
    try:
        L = len(x)
    except:
        ab.EP(estring)

    if L in [3, 4] :
        if min(x) >= 0 and max(x) <= 1 :
            # assume this is an RGB value in range [0, 1], so we can 
            # scale it and output a fine type
            y = np.zeros(3, dtype=np.uint8)
            for i in range(3):
                y[i] = min(max(255*x[i], 0), 255)
            return y
        elif min(x) >= 0 and max(x) <= 255 :
            # assume this is an RGB value in range [0, 255], so we
            # can output a fine type
            y = np.zeros(3, dtype=np.uint8)
            for i in range(3):
                y[i] = x[i]
            return y
    else:
        ab.EP(estring)


def blend_cbar(X, min_val=None, max_val=None, thr_val=None, alpha='No',
               thr_wid=2, thr_on=True, thr_nosc=4,
               thr_col = [], 
               tick_nint=10, tick_frac=0.07, tick_col=None,
               orth_on=False, orth_frac = 1.0):

    """Take a 3-dim array X and create a new one with new features. If the
min_val, max_val and thr_val kwargs are provided, then dashed vertical
lines at the threshold value(s) will be placed at |thr_val|. If the
kwarg alpha is then also provided a recognized keyword to activate
that feature, then alpha fading to light gray will be applied within
the output cbar array. 

Parameters
----------
X : np.array
    a (width)x(length)x3 RGB array of colors. Each element is 
    in the range [0, 255], and is of type np.uint8.
min_val : numeric
    scalar value, the minimum of the colorbar
max_val : numeric
    scalar value, the maximum of the colorbar
thr_val : numeric
    scalar value, the threshold value applicable for the colorbar
alpha : str
    provide a keyword for controlling whether Alpha fading should
    appear in the cbar; at present, allowed strings are:
      'No', 'Yes', 'Quadratic', 'Linear'
thr_wid : int
    width (in terms of number of pixels or rows) to make the threshold
    line
thr_on : bool
    True or False, for whether to display a threshold line (if possible)
thr_nosc : int
    number of b/w oscillations to have in the threshold line (if present);
    higher int means shorter dashes, and the number is approximate if the 
    cbar width is not an integer multiple of this value
thr_col : list (of str)
    list of one or 2 strings, defining colors to use for the threshold line
    in the output cbar. If [], will use defaults: if alpha is 'Yes', 'Linear'
    or 'Quadratic', alternate 'black' and 'white'; if alpha is 'No', use
    'black'.
tick_nint : int
    if this value is >0, then short tickmarks will be placed along
    the cbar, dividing it up into the specified number of segments;
    make this 0 or None to turn off tickmarks. Default value is set
    to match AFNI colorbars
tick_frac : float
    the fraction of the cbar width to make the interval ticks
tick_col : str
    string value for tick colors. When None, will use default (likely black)
orth_on : bool
    should the blend be applied orthogonal to colorbar gradation?  if
    this is used, the thr_* opts are basically ignored, as well as the
    *_val ones; the orth_* ones are used
orth_frac : float
    a value between 0 and 1 which determines at what fraction along the 
    cbar width to start the fading+blending

Returns
-------
Y : np.array
    a (width)x(length)x3 RGB array of colors, possibly now updated
    from X with threshold line(s) and possibly Alpha fading. Each
    element is in the range [0, 255], and is of type np.uint8.

    """

    if not(alpha in list_alpha) :
        ab.EP("""The alpha kwarg value is not recognized: {}.
        It must be one of: {}""".format(alpha, list_alpha_str))

    # ----- set colors: defaults
    if tick_col is None :
        tick_col_rgb = zer
    else:
        tick_col_rgb = set_color_to_rgb(tick_col)

    # ----- threshold line coloration
    if type(thr_col) != list :
        ab.EP("""The thr_col kwarg value must be a list; 
        current type is: {}""".format(type(thr_col)))
    elif len(thr_col) == 0 :
        if alpha == 'No' :
            thr_col_rgb = [zer, rez]
        else:
            thr_col_rgb = [zer, rez]
    elif len(thr_col) > 2 :
        ab.EP("""User entered >2 colors for thr_col. Only the first 2 
        will be used""")
    else:
        # then make sure format is appropriate
        thr_col_rgb = [set_color_to_rgb(x) for x in thr_col]
    if len(thr_col_rgb) == 1 :
        # internally, always want this to have 2 colors
        thr_col_rgb.append(thr_col_rgb[0])


    # ----- get parameters
    W, N, rgb = np.shape(X)             # cbar dimensions
    if tick_nint :
        nmark = N // tick_nint          # interval of each tick
    wmark     = int(W * tick_frac)      # width of each tick
    wdash     = W // (2*thr_nosc)       # width of threshold line dash

    # ----- init output RGB cbar array
    Y   = np.zeros((W, N, rgb), dtype=np.uint8)
    # init weight array along dim of length N: no alpha
    wtN = np.ones(N, dtype=float)
    # init weight array along dim of length W: no alpha
    wtW = np.ones(W, dtype=float)

    # can we do quantitative stuff here? NB: if thr_val is None or 0, 
    # treat equivalently (no thr line, and no possible Alpha fade)
    HAVE_ALL_VALS = False
    if min_val is not None and max_val is not None and thr_val :
        HAVE_ALL_VALS = True
        # actual vals at each cbar loc
        allv = np.linspace(min_val, max_val, N)

    # check about blending along direction orthogonal to main one, or
    # along cbar
    if orth_on :
        # make weight (wtW, vals in [0, 1]) so fading will be orth to grad
        allvW = np.linspace(1.0, 0.0, W)
        # calculate color-blend weight (wt), restricted to [0,1]
        for j in range(W):
            rat   = np.abs(allvW[j]/orth_frac)
            wtW[j] = min(max(rat,0.0),1.0)
            if alpha == 'Yes' or alpha == 'Quadratic' :
                # quadratic Alpha situation
                wtW[j]**= 2
    elif HAVE_ALL_VALS :
        # make weight (wtN, vals in [0, 1]) so fading will be along grad
        for i in range(N):
            rat   = np.abs(allv[i]/thr_val)
            if alpha is None :
                # no threshold: wtN[i] remains 1
                continue
            elif alpha == 'No' :
                # opaque threshold: wtN[i] is 1 or 0
                wtN[i] = min(max(int(rat),0.0),1.0)
            else:
                # transparent thresholding cases (linear or quad fading)
                wtN[i] = min(max(rat,0.0),1.0)
                if alpha == 'Yes' or alpha == 'Quadratic' :
                    # quadratic Alpha situation
                    wtN[i]**= 2
    else:
        ab.WP("""You said you wanted Alpha on, but didn't provide enough 
        info for it: need min_val, max_val and thr_val""")

    # fill in colors everywhere in new array
    if orth_on :
        # when fading is orthogonal to cbar gradient
        for i in range(N):
            # get base color from middle/spine of cbar
            base_rgb = X[W//2, i, :]
            for j in range(W):
                if wtW[j] < 1.0 :
                    new_rgb = wtW[j]*base_rgb + (1.0-wtW[j])*self.bkgd_color
                else:
                    new_rgb = base_rgb
                Y[j, i, :] = new_rgb.astype(np.uint8)
    else:
        # when fading is parallel to cbar gradient
        for i in range(N):
            if wtN[i] < 1.0 :
                new_rgb = wtN[i]*X[W//2, i, :] + (1.0-wtN[i])*self.bkgd_color
            else:
                new_rgb = X[W//2, i, :]
            for j in range(W):
                Y[j, i, :] = new_rgb.astype(np.uint8)

    # put in tick marks
    if tick_nint :
        for i in range(N):
            if i % nmark == 0 and i and i < (N-0.5*nmark) :
                Y[:wmark, i, :]   = tick_col_rgb #zer
                Y[W-wmark:, i, :] = tick_col_rgb #zer

    # put threshold line
    if HAVE_ALL_VALS and thr_on :
        diffs = np.sign(np.abs(allv) - thr_val)
        i = 1
        while i < N :
            if diffs[i]*diffs[i-1] < 0 :
                bmin = i - (thr_wid // 2)
                bmax = bmin + thr_wid
                Y[:, bmin:bmax, :] = thr_col_rgb[0] #zer
                for j in range(W):
                    if np.floor(j/wdash) % 2 :
                        Y[j, bmin:bmax, :] = thr_col_rgb[1] #rez
                i = bmax+1
            else:
                i+= 1
    return Y

def get_pbar_info_from_chauffeur_json(D):
    """Get useful colorbar information from keys in the input dictionary
D, assuming that it came from AFNI's @chauffeur_afni. This information
can then be provided to the colorbar updating function.

Parameters
----------
D : dict
    a dictionary that is presumed to accompany a colorbar created by 
    @chauffeur_afni

Returns
-------
min_val : numeric
    scalar value, the minimum of the colorbar
max_val : numeric
    scalar value, the maximum of the colorbar
thr_val : numeric
    scalar value, the threshold value applicable for the colorbar
alpha : str
    a keyword for controlling whether Alpha fading should
    appear in the cbar; at present, allowed strings are:
      'No', 'Yes', 'Quadratic', 'Linear'

    """

    all_keys = D.keys()

    # initialize output values
    min_val = None
    max_val = None
    thr_val = None
    alpha   = 'No'

    # see what can be filled in; several are numeric, so check for
    # that with float() func
    if 'pbar_bot' in all_keys :
        min_val = float(D['pbar_bot'])
    if 'pbar_top' in all_keys :
        max_val = float(D['pbar_top'])
    if 'vthr' in all_keys :
        thr_val = float(D['vthr'])
    if 'olay_alpha' in all_keys :
        alpha   = D['olay_alpha']

    return min_val, max_val, thr_val, alpha

def main_prog(in_cbar_fname, out_cbar_fname=None, in_cbar_json=None,
              alpha='No',
              thr_wid=2, thr_on=True, thr_nosc=4, thr_col=[],
              tick_nint=10, tick_frac=0.07, tick_col=None,
              orth_on=False, orth_frac = 1.0,
              outline_wid=None, outline_col=None):
    """The main parameters of inputs/outputs are described under
Parameters, below.

The additional ones like thr_wid, thr_nosc, etc. provide detailed
control of the output cbar---see the function blend_cbar() for
descriptions on those.  In particular, the orth_* kwargs mean that the
blending/fading will be applied orthogonal to the cbar gradient.

Parameters
----------
in_cbar_fname : str
    filename of input colorbar to read
out_cbar_fname  : str
    filename of output colorbar to write (if provided, the new cbar will
    be saved to disk)
in_cbar_json : str
    a JSON file accompanying the in_cbar_fname, created by @chauffeur_afni
    and which has min_val, max_val, thr_val and alpha information for
    adding information to the new cbar
outline_wid : int
    number of layers to add along each edge as an outline
outline_col : str
    color to make outline (def: black)

Returns
-------
Y : np.array
    updated array representing the new/output cbar

    """

    # read in colorbar image to an array
    X = read_cbar(in_cbar_fname)

    # do we need to rotate it? record status to unrotate at end, too
    DID_ROTATE = False
    if DO_AUTOROTATE :
        DID_ROTATE, X = autorotate_cbar(X)
    
    # do we have cbar ranges, threshold and alpha from a JSON?
    if in_cbar_json :
        # yes, read in @chauffeur_afni-created JSON
        D = read_json(in_cbar_json)
        min_val, max_val, thr_val, alpha = \
            get_pbar_info_from_chauffeur_json(D)
    else:
        # no, so use default/off values
        min_val, max_val, thr_val = None, None, None
        # a synonym for alpha being None
        if alpha is None :   alpha = 'No'

    # create new cbar [PT: now using more opts via kwargs from the top-level]
    Y   =  blend_cbar(X, min_val=min_val, max_val=max_val,
                      thr_val=thr_val, alpha=alpha,
                      thr_wid=thr_wid, thr_on=thr_on, thr_nosc=thr_nosc,
                      thr_col=thr_col,
                      tick_nint=tick_nint, tick_frac=tick_frac, 
                      tick_col=tick_col,
                      orth_on=orth_on, orth_frac=orth_frac)
    
    # potentially add some outline
    if outline_wid :
        Y = add_cbar_outline(Y, outline_wid=outline_wid, 
                             outline_col=outline_col)

    # was it autorotated above? then unrotate it back here
    if DID_ROTATE :
        Y = np.rot90(Y, k=1)

    # write the new cbar to disk
    if out_cbar_fname :
        plt.imsave(out_cbar_fname, Y) 

    return Y
    
def add_cbar_outline(X, outline_wid, outline_col=None):
    """Take a colorbar array X and add an outline of width outline_width
(=number of layers), of color outline_col.

Parameters
----------
X : np.array
    a (width)x(length)x3 RGB array of colors. Each element is 
    in the range [0, 255], and is of type np.uint8.
outline_wid : int
    number of layers to add, >0
outline_col : str
    string value for tick colors. When None, will use default (likely black)

Returns
-------
Y : np.array
    a (width+2*outline_wid)x(length+2*outline_wid)x3 RGB array of colors. 
    Each element is in the range [0, 255], and is of type np.uint8.
    """

    # ----- get parameters
    W, N, rgb = np.shape(X)             # cbar dimensions

    outline_wid = int(outline_wid)
    if outline_wid < 1 :
        ab.WP("""The integer part of the selected outline_wid is <1. 
        So, we won't add any outline to the colorbar""")
        return X

    # ----- set color of outline
    if outline_col is None :
        outline_col_rgb = zer
    else:
        outline_col_rgb = set_color_to_rgb(outline_col)

    # ----- init output RGB cbar array
    W2 = W + 2*outline_wid
    N2 = N + 2*outline_wid
    Y  = np.zeros((W2, N2, rgb), dtype=np.uint8)
    # ... and initialize the color to be the outline col
    Y[:,:,:] = outline_col_rgb

    # copy the values of X into the middle in one, Pythonic jump
    Y[outline_wid:outline_wid+W,outline_wid:outline_wid+N,:] = X

    return Y

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    bname     = 'qc_00_vstat_Full_Fstat.pbar'
    cbar_in   = bname + '.jpg'
    cbar_json = bname + '.json'
    cbar_out  = bname + "_NEW.jpg"

    Y = main_prog(cbar_in, cbar_out, in_cbar_json=cbar_json)
