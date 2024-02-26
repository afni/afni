#!/usr/bin/env python


# A library of functions for taking an existing colorbar (cbar) and
# outputting a new one with enhanged information, like a threshold
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

from   afnipy     import afni_base as ab

# ============================================================================

# allowed values of alpha ('Yes' is equivalent to 'Quadratic')
list_alpha      = ['No', 'Yes', 'Quadratic', 'Linear']
list_alpha_str  = ', '.join(list_alpha)

# Alpha blend color: what do we blend with when Alpha is on?
#abc = np.array([255, 255, 255], dtype=np.uint8)   # white
abc = np.array([200, 200, 200], dtype=np.uint8)  # light gray

# the color(s) of tickmarks and the threshold line
zer = np.array([0, 0, 0], dtype=np.uint8)
rez = np.array([255,255,255], dtype=np.uint8)

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

def blend_cbar(X, min_val=None, max_val=None, thr_val=None, alpha='No',
               thr_wid=2, thr_on=True, thr_nosc=4,
               tick_nint=10, tick_frac=0.07):
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
tick_nint : int
    if this value is >0, then short tickmarks will be placed along
    the cbar, dividing it up into the specified number of segments;
    make this 0 or None to turn off tickmarks. Default value is set
    to match AFNI colorbars
tick_frac : float
    the fraction of the cbar width to make the interval ticks

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

    # get parameters
    W, N, rgb = np.shape(X)             # cbar dimensions
    if tick_nint :
        nmark = N // tick_nint          # interval of each tick
    wmark     = int(W * tick_frac)      # width of each tick
    wdash     = W // (2*thr_nosc)       # width of threshold line dash

    # init output RGB cbar array
    Y   = np.zeros((W, N, rgb), dtype=np.uint8)
    # init weight array: no alpha
    wt  = np.ones(N, dtype=float)

    # can we do quantitative stuff here?
    HAVE_ALL_VALS = False
    if min_val != None and max_val != None and thr_val != None :
        HAVE_ALL_VALS = True
        # actual vals at each cbar loc
        allv = np.linspace(min_val, max_val, N)

    # Create alpha array (wt), if desired and enough info is available
    if alpha != 'No' :
        if HAVE_ALL_VALS :
            # calculate color-blend weight (wt), restricted to [0,1]
            for i in range(N):
                rat   = np.abs(allv[i]/thr_val)
                wt[i] = min(max(rat,0.0),1.0)
                if alpha == 'Yes' or alpha == 'Quadratic' :
                    # quadratic Alpha situation
                    wt[i]**= 2
        else:
            ab.WP("""You said you wanted Alpha on, but didn't provide enough 
            info for it: need min_val, max_val and thr_val""")

    # fill in colors everywhere in new array
    for i in range(N):
        if wt[i] < 1.0 :
            new_rgb = wt[i]*X[W//2, i, :] + (1.0-wt[i])*abc
        else:
            new_rgb = X[W//2, i, :]
        for j in range(W):
            Y[j, i, :] = new_rgb.astype(np.uint8)

    # put in tick marks
    if tick_nint :
        for i in range(N):
            if i % nmark == 0 and i and i < (N-0.5*nmark) :
                Y[:wmark, i, :]   = zer
                Y[W-wmark:, i, :] = zer

    # put threshold line
    if HAVE_ALL_VALS and thr_on :
        diffs = np.sign(np.abs(allv) - thr_val)
        i = 1
        while i < N :
            if diffs[i]*diffs[i-1] < 0 :
                bmin = i - (thr_wid // 2)
                bmax = bmin + thr_wid
                Y[:, bmin:bmax, :] = zer
                for j in range(W):
                    if np.floor(j/wdash) % 2 :
                        Y[j, bmin:bmax, :] = rez
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

def main_prog(in_cbar_fname, out_cbar_fname=None, in_cbar_json=None):
    """

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
        min_val, max_val, thr_val, alpha = None, None, None, 'No'

    # create new cbar
    Y   =  blend_cbar(X, min_val=min_val, max_val=max_val,
                      thr_val=thr_val, alpha=alpha)
    
    # write the new cbar to disk
    if out_cbar_fname :
        plt.imsave(out_cbar_fname, Y) 

    return Y
    

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    bname     = 'qc_00_vstat_Full_Fstat.pbar'
    cbar_in   = bname + '.jpg'
    cbar_json = bname + '.json'
    cbar_out  = bname + "_NEW.jpg"

    Y = main_prog(cbar_in, cbar_out, in_cbar_json=cbar_json)
