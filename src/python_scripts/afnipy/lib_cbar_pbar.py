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
#abc = np.array([255, 255, 255], dtype=np.uint8)   # white
abc = np.array([200, 200, 200], dtype=np.uint8)  # light gray

# the color(s) of tickmarks and the threshold line
zer = np.array([0, 0, 0], dtype=np.uint8)       # black
rez = np.array([255,255,255], dtype=np.uint8)   # white

# ============================================================================

def read_cbar(fname):
    """Read in colorbar (JPG, PNG, etc.), via filename fname. 

When read in, we assume that the dimensions translate to width x
length x RGB. That seems to be the case with all AFNI-created
colorbars. For future generalizations, we might need to add in a
transpose opt, for example.

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
    if tick_col == None :
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
        thr_col_rgb.append(thr_col_rgb[1])


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
    if min_val != None and max_val != None and thr_val :
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
            if alpha == None :
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
                    new_rgb = wtW[j]*base_rgb + (1.0-wtW[j])*abc
                else:
                    new_rgb = base_rgb
                Y[j, i, :] = new_rgb.astype(np.uint8)
    else:
        # when fading is parallel to cbar gradient
        for i in range(N):
            if wtN[i] < 1.0 :
                new_rgb = wtN[i]*X[W//2, i, :] + (1.0-wtN[i])*abc
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
        if alpha == None :   alpha = 'No'

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
    if outline_col == None :
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
