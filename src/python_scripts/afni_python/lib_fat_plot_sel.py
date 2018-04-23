#!/usr/bin/env python
#
# ver 1.0:  Oct, 2014
#
# File of plotting and matrix selection functions for fat_*.py.
#
# Likely:
# << Supported formats: bmp, emf, eps, gif, jpeg, jpg, pdf, pgf, png,
# ps, raw, rgba, svg, svgz, tif, tiff. >>
#
#
#
#############################################################################

import matplotlib.pyplot as plt
import matplotlib.colors as clr
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
import lib_fat_funcs as GR

import sys

np.set_printoptions(linewidth=200)

MatOutPost = "1D.dset"
### Don't need a guard for filetype -> is handled by Python:
LIST_INT_types = [ int , np.int8 , np.int16 , np.int32 , np.int64 ] 




### -----------------------------------------------------------------
### -----------------------------------------------------------------

def Write_Out_Matrix(X, fpref, ftype, OUT_format, ExternLabsOK):
    Ngrid = len(X[1])
    Nroi = np.shape(X[0][0])[0]

    print "NROI:", Nroi
    print "NGRID:", Ngrid

    outname = fpref
    if (Ngrid == 1):
        outname+= '_' + X[1][0]
    else:
        outname = '_SUB'
    if OUT_format == 2:
        outname+= '.' + MatOutPost
    else:
        outname+= '.' + ftype

    f = open(outname, 'w')
    f.write('# %d # %s\n' % (Nroi, GR.HEADER_Nroi))
    f.write('# %d # %s\n' % (Ngrid, GR.HEADER_Ngrid))

    if X[4] : 
        f.write('# %s \n' % GR.HEADER_Labels)  
        if OUT_format == 2:
            f.write("# ")
        for roi in X[4]:
            f.write("%12s\t" % roi)
        f.write('\n')

    if OUT_format == 2:
        f.write("# ")
    for roi in X[3]:
        f.write("%12s\t" % roi)
    f.write('\n')

    for i in range(Ngrid):
        f.write('# %s\n' % X[1][i])
        for j in range(Nroi):
            for k in range(Nroi):
                if X[1][i]=='NT':
                    f.write('%12d\t' % X[0][i][j][k])
                else:
                    f.write('%12e\t' % X[0][i][j][k])
            f.write('\n')

    f.close()

    # for niml dset labels
    if OUT_format == 2:

        outname2 = fpref
        if (Ngrid == 1):
            outname2+= '_' + X[1][0]
        else:
            outname2 = '_SUB'
        outname2+= '.' + 'txt'

        f = open(outname2, 'w')


        if X[4] and ExternLabsOK: 
            WhichNum = 4
        else:
            WhichNum = 3

        for i in range(len(X[WhichNum])):
            roi = X[WhichNum][i]
            f.write("%d \t %12s \t 0 0 0" % (i, roi))
            f.write('\n')

        f.close()

    return outname

### -----------------------------------------------------------------

#  X   is a tuple
# X[0] is list of NxN array of data
# X[1] is list of pars
# X[2] is dict of pars
# X[3] is list of simple int-str labels
# X[4] is (empty or full) list of labels

def Get_Subset_of_inMatrix(X, pars):

    # will be output
    Y = [[],[],{},[],[]]
    
    for i in range(len(pars)):
        P = pars[i]
        # find the index of the par, if it exists
        if X[2].__contains__(P) :
            ind = X[2][P]
            
            Y[0].append(np.array(X[0][ind]))
            Y[1].append(X[1][ind])
            Y[2][P] = i
            if i==0 : # only do on first pass, and replace in
                Y[3] = list(X[3])
                if X[4] :
                    Y[4] = list(X[4])
        else:
            print "*+ Warning! Can't find asked for parameter %s." % (P)
    
    return tuple(Y)

### -----------------------------------------------------------------
### -----------------------------------------------------------------

def DefaultNamingPrefType(list_all):
    '''Take in a list of names, strip off ending and append as part of
    string, forming a prefix for other output. Also output list of
    types, so we can use later if necessary in output files.'''

    out = []
    out_types = []

    for x in list_all:
        if not(x[-5:] == '.grid') and not(x[-6:] == '.netcc'):
            print "ERROR-- doesn't look like this is a *.grid or *netcc file!"
        elif x[-5:] == '.grid' :
            out.append(x[:-5])
            out_types.append('grid')
        else:
            out.append(x[:-6])
            out_types.append('netcc')

    return out, out_types

### -----------------------------------------------------------------

def MakeMyBar(name):

    wind = 0.002
    sfac = 0.999

    if name == 'hot_cold_gap':

        # window/level parameters
        midlev = 0.5

        cdict_00 = {'red': ((0.0, 0.0, 0.0),
                            (midlev-wind     , 0., 0.90),
                            (midlev-wind*sfac, 0.90, 0.0),
                            (midlev+wind*sfac, 0.0, 0.0),
                            (midlev+wind     , 0.0, 0.90),
                            (1.0, 1.0, 1.0)),
                    
                    'green': ((0.0, 0.0, 0.0),
                              (midlev-wind     , 0.90, 0.90),
                              (midlev-wind*sfac, 0.90, 0.0),
                              (midlev+wind*sfac, 0.0, 0.0),
                              (midlev+wind     , 0.0, 0.90),
                              (1.0, 0.0, 0.90)),
                    
                    'blue': ((0.0, 0.50, 0.50),
                             (midlev-wind     , 1.0, 0.00),
                             (midlev-wind*sfac, 0.0, 0.00),
                             (midlev+wind*sfac, 0.0, 0.0),
                             (midlev+wind     , 0.0, 0.00),
                             (1.0, 0.0, 0.0))}

        out = clr.LinearSegmentedColormap('my_colormap',
                                          cdict_00,
                                          512)

    elif name == 'gap_jet':
        cdict_01 = {'red': ((0., 0, 0),
                            (wind*sfac, 0, 0),
                            (wind, 0, 0),
                            (0.11, 0, 0),
                            (0.66, 1, 1),
                            (0.89, 1, 1),
                            (1, 0.5, 0.5)),
                    'green': ((0., 0, 0),
                              (wind*sfac, 0, 0),
                              (wind, 0, 0),
                              (0.11, 0, 0),
                              (0.375, 1, 1),
                              (0.64, 1, 1),
                              (0.91, 0, 0),
                              (1, 0, 0)),
                    'blue': ((0., 0, 0),
                             (wind*sfac, 0, 0),
                             (wind, .5, 0.5),
                             (0.11, 1, 1),
                             (0.34, 1, 1),
                             (0.65, 0, 0),
                             (1, 0, 0))}
        
        out = clr.LinearSegmentedColormap('my_colormap',
                                          cdict_01,
                                          512)
    else:
        print "Unrecognized colormap specialification."
        sys.exit(151)
        
    return out

    
### -----------------------------------------------------------------

def Fat_Mat_Plot( X, 
                  WhichVar, 
                  xpref,
                  xtype,
                  FS,                   # fontsize of whole plot
                  DO_COLORBAR,
                  MAP_of_COL,
                  N_CBAR_INT,
                  WIDTH_CBAR_PERC, 
                  LAB_SIZE_FONT,
                  FTYPE,
                  DO_PLOT,
                  TIGHT_LAY,
                  DO_XTICK_LABS,
                  MATDPI,
                  MAT_X,
                  MAT_Y,
                  MATMIN_str, 
                  MATMAX_str,
                  SPEC_FORM,
                  USE_EXTERN_LABS,
                  HOLD_IMG):
    '''Input the tuple (currently 5 elements) of properties and use
    them for plotting matrix.'''


    filestr = xpref + '_' + xtype + '_' + WhichVar

    # - - - - - - - - - - - - - - - - - - - - - - - - 


    # internal colorbar
    if ( MAP_of_COL == 'hot_cold_gap' ) or   \
       ( MAP_of_COL == 'gap_jet' ):
        print "MAKING COLORMAP:", MAP_of_COL 
        MAP_of_COL = MakeMyBar(MAP_of_COL)
        #print MAP_of_COL, type(MAP_of_COL)

    # get the mat
    WhichMat = X[2][WhichVar]
    MAT = X[0][WhichMat]
 
    # grid
    if MATMIN_str :
        MATMIN = float(MATMIN_str)
    else:
        MATMIN = np.min(MAT)
    if MATMAX_str :
        MATMAX = float(MATMAX_str)
    else:
        MATMAX = np.max(MAT)

    Nroi = np.shape(MAT)[0]
    print "++ Matrix min and max values: ", MATMIN, 'and', MATMAX
    print "++ Matrix dimensionality: ", Nroi, 'by', Nroi

    # whether there are labels or not
    if( X[4] and USE_EXTERN_LABS ):
        MATTICKS = X[4]
    else:
        MATTICKS = X[3]

    # Plotting options
    plt.figure(figsize=(MAT_X, MAT_Y), dpi=MATDPI, facecolor='w')
    subb = plt.subplot(111)
    ax = plt.gca()
    box = ax.get_position()

    IM = ax.imshow( MAT,
                    interpolation='nearest', 
                    aspect='equal',
                    vmin=MATMIN, 
                    vmax=MATMAX,
                    cmap=MAP_of_COL)

    if DO_XTICK_LABS:
        plt.xticks( np.arange(len(MATTICKS)), MATTICKS, 
                    rotation=45, ha='right',
                    fontsize=LAB_SIZE_FONT )
    else:
        plt.xticks( np.arange(len(MATTICKS)), len(MATTICKS)*'',
                    fontsize=LAB_SIZE_FONT )
    plt.yticks( np.arange(len(MATTICKS)), MATTICKS, rotation=0, 
                fontsize=LAB_SIZE_FONT )
    plt.title('%s' % (X[1][WhichMat]), fontsize=FS+1 )


    if DO_COLORBAR:
        cbar_perc = WIDTH_CBAR_PERC+"%"
        divider = make_axes_locatable(ax)
        TheCax = divider.append_axes("right", size=cbar_perc, pad=0.1)
        ColBar = np.linspace(MATMIN, MATMAX, N_CBAR_INT+1, endpoint=True)
        if SPEC_FORM:
            WhatForm = SPEC_FORM
        elif LIST_INT_types.__contains__( type(MAT[0][0]) ):
            WhatForm = '%d'
        else:
            WhatForm = '%.3f'
        cbar = plt.colorbar(IM, 
                            cax=TheCax, 
                            ticks=ColBar, 
                            format=WhatForm)
        cbar.ax.tick_params(labelsize=FS) 

    if DO_PLOT:
        name_out = filestr
        name_out_full = name_out + '.' + FTYPE
        plt.savefig( name_out_full, dpi=MATDPI )
    else:
        name_out_full = ''

    if TIGHT_LAY :
        plt.tight_layout()
    else:
        subb.set_position([box.x0+box.width*0.0, box.y0+box.height*0.05,  
                           box.width*0.95, box.height*0.95])
    plt.ion()
    plt.show()

    if HOLD_IMG :
#        plt.show()
        raw_input()

    return name_out_full
