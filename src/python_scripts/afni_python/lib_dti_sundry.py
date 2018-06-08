import sys as sys
import numpy as np

#np.set_printoptions(linewidth=200)

def read_fsl_eddy_parameters(my_file):
    '''Read in an FSL (eddy-)produced *.eddy_parameters file and output a
    simple text file of 6 columns resembling the output of 3dvolreg: 3
    translations (mm) and 3 rotations (deg).

    Tested/used on FSL v5.0.11; may work on eddy_parameters files from
    different versions of the same software.

    '''

    fff = open( my_file ,'r')
    raw_data = fff.readlines()
    fff.close()

    data_list1 = []
    for line in raw_data:
        aa = np.zeros(6)
        x = w.split()
        Nx = len(x)
        if Nx < 6:
            sys.exit("** Problem in this line:\n %s\n\n" % line)
        for i in range (6):
            aa[i] = float(x[i])

        # convert rads to degs
        aa[3:]*= 180./np.pi

        data_list1.append(aa)

    return np.array(data_list1)

def read_tortoise_transformations(my_file):
    '''Read in a TORTOISE (DIFFPREP-)produced *_transformations.txt file
    and output a simple text file of 6 columns resembling the output
    of 3dvolreg: 3 translations (mm) and 3 rotations (deg).

    Tested/used on TORTOISE v3.1; may work on transformations.txt
    files from different versions of the same software.

    '''

    fff = open( my_file ,'r')
    raw_data = fff.readlines()
    fff.close()

    data_list1 = []
    for line in raw_data:
        aa = np.zeros(6)
        w = line.translate(None,"[]")
        x = w.split(',')
        Nx = len(x)
        if Nx < 6:
            sys.exit("** Problem in this line:\n %s\n\n" % line)
        for i in range (6):
            aa[i] = float(x[i])

        # convert rads to degs
        aa[3:]*= 180./np.pi

        data_list1.append(aa)

    return np.array(data_list1)
 
# ===============================================================

#if __name__ == "__main__":

    
    
