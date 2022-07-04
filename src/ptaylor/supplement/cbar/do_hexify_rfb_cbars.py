
# Convert RGB cmap files that are given as:
#   0.267004  0.004874  0.329415
#   0.268510  0.009605  0.335427
#   0.269944  0.014625  0.341379
#   0.271305  0.019942  0.347269
#   ...
# into something formatted for pbardefs.h hex/char arrays, like:
#   "Reds_and_Blues_Inv "
#   "#ff0b00 "
#   "ff0e00 "
#   "#ff1100 "
#   "#ff1400 "
#   "#ff1700 "
#   "#ff1a00 "
#   ...
# 
# ======================================================================

import os, sys, glob
from afnipy import afni_base as AB

def read_rgb_cmap( fname ):
    """read in a RGB cmap file, and return it as a list. Also return a
    scale factor for hexifying, depending on whether each RGB channel
    is in range [0, 1] or [0, 255].

    """

    fff = open(fname, 'r')
    X = fff.readlines()
    fff.close()

    N    = len(X)
    data = []
    xmax = -1

    cbar_name = fname.replace('.txt', '')
    cbar_name = cbar_name.replace('cmap_rgb_', '')
    print("++ cbar name:", cbar_name)

    for i in range(N):
        row = X[i].strip()
        if row :
            row_split = row.split()
            if len(row_split) != 3 :
                AB.EP("In file {}, row {}, number of elements is {}, not 3"
                      "".format(fname, i, len(row_split)))
            try:
                row_num = [int(x) for x in row_split]
            except:
                row_num = [float(x) for x in row_split]
            rowmax = max(row_num)
            if rowmax > xmax :
                xmax = rowmax
            data.append(row_num)

    ndata = len(data)
    if xmax > 1 :        scale = 1 
    else:                scale = 255

    AB.IP("Final cbar has '{}' rows".format(ndata))
    AB.IP("The maxvalue in cbar: '{}'".format(xmax))
    AB.IP("... therefore, the scale factor should be {}".format(scale))

    return cbar_name, data, scale

def write_cbar_file(cbar_name, cbar_data, scale=1, wrapby=8):
    '''Take a cbar_data list (N rows by 3 cols) of RGB color values, and
return a string that can be directly put into AFNI's pbardefs.h.

    Use the cbar_name to define the top part of the string.
    
    Use optional scale value to scale the data; for the conversion,
    the numbers used should be in range [0, 255].

    The wrapby kwarg is to specify the number of columns to output (to
    keep final text file entry shorter, vertically).

    '''

    N = len(cbar_data)

    ostr = ''
    tail = '''"\n'''

    # top two lines: start array, and provide cbar name
    ostr+= '''static char HEX_{}_{}_CMD[] = {{\n'''.format(cbar_name, N)
    ostr+= '''    "{} "\n'''.format(cbar_name)

    NEWLINE = 1
    for i in range(N):
        x = cbar_data[i]
        if len(x) != 3 :
            sys.exit("ERROR! line:\n{}\n is not RGB nums!".format(i))

        if NEWLINE: 
            ostr+= '''    "'''
            NEWLINE = 0

        ostr+= '''#{:02x}{:02x}{:02x} '''.format( int(x[0]*scale),
                                                  int(x[1]*scale),
                                                  int(x[2]*scale) )
        if not((i+1) % wrapby) :
            ostr+= tail
            NEWLINE = 1
            
    if ostr[-2:] != tail :
        ostr+= tail        
        
    ostr+= '''};\n'''

    fout  = 'pbardef_{}.txt'.format(cbar_name)
    fff   = open(fout, 'w')
    
    fff.write("{}".format(ostr))
    fff.close()

    return 0

    
# --------------------------------------------------------------------------

if __name__ == "__main__" :

    # get list of cmap files, simply by name (with the parts of the
    # name used for globbing here stripped to findout cbar name)
    all_cmap_files = glob.glob("cmap_rgb_*.txt")
    all_cmap_files.sort()
    
    for cmap_file in all_cmap_files:
        print("++ cmap file:", cmap_file)

        cbar_name, cbar_data, cbar_scale = read_rgb_cmap( cmap_file )
        is_ok = write_cbar_file(cbar_name, cbar_data, scale=cbar_scale)
        

    print("DONE!")
