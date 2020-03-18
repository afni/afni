import sys as sys
import numpy as np

#np.set_printoptions(linewidth=200)

def read_fsl_eddy_parameters(my_file):
    '''Read in an FSL (eddy-)produced *.eddy_parameters file and output a
    simple text file of 6 columns resembling the output of 3dvolreg: 3
    translations (mm) and 3 rotations (deg).

    Tested/used on FSL v5.0.11; may work on eddy_parameters files from
    different versions of the same software.

    Note that reordering occurs.  The first 6 columns of FSL/eddy
    *eddy_parameters files are in the following order:

    rot x
    rot y
    rot z
    trans x 
    trans y
    trans z

    3dvolreg order is:

    rot z
    rot x
    rot y
    trans z
    trans x 
    trans y

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

        # convert to 3dvolreg ordering
        bb = Conv_TRxyz_to_RTzyx(aa)

        data_list1.append(bb)

    return np.array(data_list1)

def read_tortoise_transformations(my_file):
    '''Read in a TORTOISE (DIFFPREP-)produced *_transformations.txt file
    and output a simple text file of 6 columns resembling the output
    of 3dvolreg: 3 rotations (deg) and 3 translations (mm).

    Tested/used on TORTOISE v3.1; may work on transformations.txt
    files from different versions of the same software.

    Note that reordering occurs.  The first 6 columns of
    TORTOISE/DIFFPREP transformation.txt files are in the following
    order:

    Column 0 : x (for axial data = RL, in mm)
    Column 1 : y (for axial data = AP, in mm)
    Column 2 : z (for axial data = IS, in mm)
    Column 3 : x (Euler angles, in rads)
    Column 4 : y (Euler angles, in rads)
    Column 5 : z (Euler angles, in rads)

    3dvolreg order is:

    rot z
    rot x
    rot y
    trans z
    trans x 
    trans y

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

        # convert to 3dvolreg ordering
        bb = Conv_TRxyz_to_RTzyx(aa)

        data_list1.append(bb)

    return np.array(data_list1)

# ------------ convert between volreg ordering and trans_xyz rot_xyz -----

def Conv_TRxyz_to_RTzyx(x):
    '''trans (xyz) rot (xyz) ordering to volreg; inverse of
onv_RTzyx_to_TRxyz

    '''
    
    if len(x) - 6:
        sys.exit("** Problem converting matrices! Wrong number of components")

    y = np.zeros(6)

    y[0] = x[5]
    y[1] = x[3]
    y[2] = x[4]
    y[3] = x[2]
    y[4] = x[0]
    y[5] = x[1]

    return y

def Conv_RTzyx_to_TRxyz(x):
    '''volreg ordering to trans (xyz) rot (xyz); inverse of
 Conv_TRxyz_to_RTzyx

    '''

    if len(x) - 6:
        sys.exit("** Problem converting matrices! Wrong number of components")

    y = np.zeros(6)

    y[0] = x[4]
    y[1] = x[5]
    y[2] = x[3]
    y[3] = x[1]
    y[4] = x[2]
    y[5] = x[0]

    return y

# --------------------- for RMS calcs ----------------------

def Rx(x):

    out = np.zeros((3,3))

    out[0,0] = 1
    out[1,1] = np.cos(x)
    out[2,2] = out[1,1]
    out[2,1] = np.sin(x)
    out[1,2] = -out[2,1]

    return out

def Ry(x):

    out = np.zeros((3,3))

    out[1,1] = 1
    out[0,0] = np.cos(x)
    out[2,2] = out[0,0]
    out[0,2] = np.sin(x)
    out[2,0] = -out[0,2]

    return out

def Rz(x):

    out = np.zeros((3,3))

    out[2,2] = 1
    out[0,0] = np.cos(x)
    out[1,1] = out[0,0]
    out[1,0] = np.sin(x)
    out[0,1] = -out[1,0]

    return out

# input angles in order of z-, y- and x- rots.
# calculation would be done as if L-multiplying in order of
# Rx, then Ry, then Rz
# can use optional 4th argument if input angles are in 'rad'
# and not (default) 'deg'
def Rzyx(c, b, a, atype='deg'):

    if atype=='deg':
        fac = np.pi/180.
        a*=fac
        b*=fac
        c*=fac
    elif not(atype =='rad'):
        print "Error! unknown angle specification '%s'!", atype
        sys.exit(5)
        
    YX = np.dot(Ry(b), Rx(a))
    ZYX = np.dot(Rz(c), YX)

    return ZYX

# Following Reuter et al. 2014.  Need 't' and 'R' to have the same
# units (mm). For humans, 'R' here should prob be between 60-75 mm.
def calc_RMS_from_MtR(M, t, R):

    Mid = M - np.identity(3)
    MidTMid = np.dot(np.transpose(Mid), Mid)
    TrMids = np.trace(MidTMid)

    out = R * R * TrMids / 5.
    out+= np.dot(np.transpose(t), t)

    return np.sqrt(out)

def calc_RMS_from_6mot(x, brrad, atype='deg'):
    '''Main function to calculate RMS from six solid body parameters:
three translation (in mm), and 3 rotation (in deg, def; can also put
in 'rad').

INPUT

     x : [req] An array of 6 numbers, which should be 3 translation in mm
         and 3 rotation in either deg (def) or rad (signalled with 3rd
         arg).  Order of components should be in same order as outputs
         of 3dvolreg:

             roll pitch yaw dS  dL  dP

         where "roll" is rotation about I-S axis (shaking head "no"),
         "pitch" is rotation around L-R axis (nodding, which is
         common), and yaw is rotation about A-P axis.

 brrad : [req] Single number, the length scale brain; e.g., approx radius of
         the brain, like r~(3.*V/(4*np.pi))**(1./3). Required.

 atype : [opt] Specify whether rotations are either 'deg' (def) or
         'rad'. 

OUTPUT

    Single floating point number, the RMS.

    '''

    Nx = len(x)

    if not( Nx == 6 ):
        sys.exit("** Error: wrong length %d in input array" % Nx)
    
    # convert volreg ordering to Tx, Ty, Tz, Rx, Ry, Rz
    y = Conv_RTzyx_to_TRxyz(x)

    # NOTE: while rotations and translations can't be mixed, the order
    # of components within rotations isn't so important, since the
    # trace is taken, and likewise for rotations since it is a
    # dot-product.  Also note, annoying in Taylor et al. (2016), the
    # RMS formula is written incorrectly (it should be
    # (r**2)/5*(M-I)... etc.; there is an extra "plus" incorrectly),
    # but the actual calculation was correct.
    MM = LSF.Rzyx(y[5], y[4], y[3], atype=atype) 
    tt = np.array(y[:3])
    RR = brrad
    y  = LSF.calc_RMS(MM,tt,RR)

    return y




# ===============================================================

#if __name__ == "__main__":

    
    
