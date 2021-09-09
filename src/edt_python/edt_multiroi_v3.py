import numpy as np
import math
import matplotlib.pyplot as plt
import sys, copy

BIG = 10**10 # math.inf # 10**10  

# need to transform input image into BIG and zeros...  namely to be
# like 1(q) in F&H2012's expression in the middle of page 416.
def Euclidean_DT(f):
    '''Classical Euclidean Distance Transform (EDT) of Felzenszwalb and
Huttenlocher (2012).
    
    Assumes that: len(f) < sqrt(10**10).

    In this version, all voxels should have equal length, and units
    are "edge length" or "number of voxels."

    Parameters
    ----------

    f : 1D array or list, distance**2 values (or, to start, binarized
    between 0 and BIG).

    '''

    n    = len(f)
    k    = 0
    v    = [0]*n
    z    = [0]*(n+1)
    z[0] = -BIG 
    z[1] =  BIG 

    for q in range(1, n):
        s = ((f[q] + q**2) - (f[v[k]] + v[k]**2))/(2*q - 2*v[k])
        while s <= z[k] : 
            k-= 1
            s = ((f[q] + q**2) - (f[v[k]] + v[k]**2))/(2*q - 2*v[k])
        k+= 1
        v[k]   = q
        z[k]   = s
        z[k+1] = BIG

    k   = 0
    Df  = [0]*n
    for q in range(n):
        while z[k+1] < q :
            k+= 1
        Df[q] = (q - v[k])**2 + f[v[k]]
    
    return Df

def run_EDT_per_line( roi_line, dist2_line, 
                      edges_are_zero_for_nz=True ):
    '''  '''

    Na = len(roi_line)
    idx = 0 

    line_out = np.zeros(Na)

    while idx < Na-1 :
        # get interval of line with current ROI value
        roi = roi_line[idx]
        for n in range(idx+1, Na):
            if roi_line[n] != roi :
                n -= 1
                break
        # n now has the index of last matching element

        # decide if we need to pad with 0 on either side, represents
        # boundary of another ROI (or zeropadded edge)
        ll    = []
        start = 0
        stop  = None    # for pythonic indexing
        # pad at start?
        if idx != 0 or (edges_are_zero_for_nz and roi != 0) :
            ll.append(0)
            start = 1
        # put actual values from dist**2 field...
        for m in range(idx, n+1): 
            ll.append(dist2_line[m]) 
        # pad at end?
        if n != Na-1 or (edges_are_zero_for_nz and roi != 0) :
            ll.append(0)
            stop = -1

        # now do dist calc for this interval of line, and save result
        out_1d            = Euclidean_DT(ll)
        line_out[idx:n+1] = out_1d[start:stop]

        idx = n+1

    return line_out

def Euclidean_DT_delta(f, delta = 1.0):
    '''Classical Euclidean Distance Transform (EDT) of Felzenszwalb and
Huttenlocher (2012), but for given voxel lengths.
    
    Assumes that: len(f) < sqrt(10**10).

    In this version, all voxels should have equal length, and units
    are "edge length" or "number of voxels."

    Parameters
    ----------

    f     : 1D array or list, distance**2 values (or, to start, binarized
    between 0 and BIG).

    delta : voxel edge length size along a particular direction

    '''

    n    = len(f)
    k    = 0
    v    = [0]*n
    z    = [0.]*(n+1)
    z[0] = -BIG * delta
    z[1] =  BIG * delta

    for q in range(1, n):
        s = ((f[q] + (q*delta)**2) - (f[v[k]] + (v[k]*delta)**2))
        s/= 2. * delta * (q - v[k])
        while s <= z[k] : 
            k-= 1
            s = ((f[q] + (q*delta)**2) - (f[v[k]] + (v[k]*delta)**2))
            s/= 2. * delta * (q - v[k])
        k+= 1
        v[k]   = q
        z[k]   = s
        z[k+1] = BIG * delta

    k   = 0
    Df  = [0]*n
    for q in range(n):
        while z[k+1] < q * delta :
            k+= 1
        Df[q] = (delta*(q - v[k]))**2 + f[v[k]]
    
    return Df



def run_EDTD_per_line( roi_line, dist2_line, 
                       delta = 1.0,
                       edges_are_zero_for_nz=True ):
    '''  '''

    Na = len(roi_line)
    idx = 0 

    line_out = np.zeros(Na)

    while idx < Na-1 :
        # get interval of line with current ROI value
        roi = roi_line[idx]
        for n in range(idx+1, Na):
            if roi_line[n] != roi :
                n -= 1
                break
        # n now has the index of last matching element

        # decide if we need to pad with 0 on either side, represents
        # boundary of another ROI (or zeropadded edge)
        ll    = []
        start = 0
        stop  = None    # for pythonic indexing
        # pad at start?
        if idx != 0 or (edges_are_zero_for_nz and roi != 0) :
            ll.append(0)
            start = 1
        # put actual values from dist**2 field...
        for m in range(idx, n+1): 
            ll.append(dist2_line[m]) 
        # pad at end?
        if n != Na-1 or (edges_are_zero_for_nz and roi != 0) :
            ll.append(0)
            stop = -1

        # now do dist calc for this interval of line, and save result
        out_1d            = Euclidean_DT_delta(ll, delta=delta)
        line_out[idx:n+1] = out_1d[start:stop]

        idx = n+1

    return line_out


def img2d_Euclidean_DT(im, 
                       do_sqrt=True, edges_are_zero_for_nz=True,
                       ad3=(1,1)):

    # check inputs for usability, and prep size stuff
    if type(im) != np.ndarray or im.dtype != int :
        print("** ERROR: need input to be an np.array of ints,")
        print("   since this represents a map of ROIs")
        sys.exit(4)

    SH  = np.shape(im)      # dimensions
    ND  = len(SH)           # number of dimensions

    if ND != 2 :
        print("** ERROR: need to be a 2D array at the moment")
        sys.exit(4)

    # initialize the "output" or answer array
    odt = np.ones(SH)*BIG
  
    # first pass: start with all BIGs
    for ii in range(SH[0]) :
        # get a line...
        aa = im[ii,:]
        # ... and then calc with it, and save results
        odt[ii,:] = run_EDTD_per_line( aa, odt[ii,:],
                                       delta = ad3[0],
                                       edges_are_zero_for_nz = edges_are_zero_for_nz )

    # 2nd pass: start from previous; any other dimensions would carry
    # on from here
    for jj in range(SH[1]) :
        # get a line...
        aa = im[:,jj] 
        # ... and then calc with it, and save results
        odt[:,jj] = run_EDTD_per_line( aa, odt[:,jj],
                                       delta = ad3[1],
                                       edges_are_zero_for_nz = edges_are_zero_for_nz )

    if do_sqrt :
        return np.sqrt(odt)
    else :
        return odt

def img3d_Euclidean_DT(im, 
                       do_sqrt=True, edges_are_zero_for_nz=True,
                       ad3=(1,1,1)):

    # check inputs for usability, and prep size stuff
    if type(im) != np.ndarray or im.dtype != int :
        print("** ERROR: need input to be an np.array of ints,")
        print("   since this represents a map of ROIs")
        sys.exit(4)

    SH  = np.shape(im)      # dimensions
    ND  = len(SH)           # number of dimensions

    if ND != 3 :
        print("** ERROR: need to be a 3D array at the moment")
        sys.exit(4)

    # initialize the "output" or answer array
    odt = np.ones(SH)*BIG
  
    # first pass: start with all BIGs
    for ii in range(SH[0]) :
        for jj in range(SH[1]) :
            # get a line...
            aa = im[ii,jj,:]
            # ... and then calc with it, and save results
            odt[ii,jj,:] = run_EDTD_per_line( aa, odt[ii,jj,:],
                                              delta = ad3[2],
                                              edges_are_zero_for_nz = edges_are_zero_for_nz )

    for ii in range(SH[0]) :
        for kk in range(SH[2]) :
            # get a line...
            aa = im[ii,:,kk]
            # ... and then calc with it, and save results
            odt[ii,:,kk] = run_EDTD_per_line( aa, odt[ii,:,kk],
                                              delta = ad3[1],
                                              edges_are_zero_for_nz = edges_are_zero_for_nz )


    # 2nd pass: start from previous; any other dimensions would carry
    # on from here
    for jj in range(SH[1]) :
        for kk in range(SH[2]) :
                # get a line...
                aa = im[:,jj,kk] 
                # ... and then calc with it, and save results
                odt[:,jj,kk] = run_EDTD_per_line( aa, odt[:,jj,kk],
                                                  delta = ad3[0],
                                                  edges_are_zero_for_nz = edges_are_zero_for_nz )

    if do_sqrt :
        return np.sqrt(odt)
    else :
        return odt





if __name__ == "__main__" :

    ''' For earlier testing of 1D; interestingly, 'g' is a pathological
    case.

    N = 30
    a = np.zeros(N)
    b = np.zeros(N)
    c = np.zeros(N)
    d = np.zeros(N)
    e = np.zeros(N)
    f = np.zeros(N)
    g = np.zeros(N)

    b[12] = BIG
    c[13:20] = BIG
    d[:13] = BIG ; d[20:] = BIG
    e[:-1] = BIG
    f[1:] = BIG
    g[:] = BIG
    '''


    # make a test image: a map of various ROIs
    vol   = np.zeros((80, 40, 20), dtype=int)
    AD3   = (0.5, 1, 2)  # pixel dims

    LX, LY, LZ = np.shape(vol)

    vol[4:18, 2:8, 4:8]     = 1
    vol[2:8, 4:18, 4:8]     = 4
    vol[21:30, 0:10, 11:14] = 7
    for i in range(LX):
        for j in range(LY):
            for k in range(LZ):
                if (15-i)**2 + (25-j)**2 + (10-k)**2 < 31:
                    vol[i,j,k] = 2
    vol[17:19, :, 3:6]           = 1
    vol[:, 19:21, 3:6]           = 10
    vol[60:70,28:36,14:19]       = 17
    for i in range(LX):
        for j in range(LY):
            for k in range(LZ):
                if (65-i)**2 + (10-j)**2 + (10-k)**2 < 75:
                    if not((60-i)**2 + (7-j)**2 + (10-k)**2 < 31):
                        vol[i,j,k] = 2

    vol[40:56, 25:33, 4:8]     = 5    # a square depending on vox size


    # run the transform!
    vol_EDT  = img3d_Euclidean_DT(vol, 
                                  edges_are_zero_for_nz=True,
                                  ad3=AD3 )

    vol_EDT_nz = vol_EDT * vol.astype(bool)  # only see depth in nonzero parts

    max_dist    = vol_EDT.max()   # info for plotting
    max_dist_nz = vol_EDT_nz.max()   # info for plotting

    # -------------------- plot the images ------------------------

    #my_cmap = plt.cm.get_cmap('hot', 11)

    # we plot the transpose of each, so that extents and vox dims make
    # sense

    plt.figure("Euclidean_DT, orig", figsize=(20,10))

    ncol = 5
    nrow = int(LZ/ncol) + bool(LZ % ncol)

    for count in range(LZ):
        plt.subplot(nrow,ncol,count+1) 

        plt.imshow(vol[:,:,count].T, 
           interpolation=None, 
           cmap='tab20',   # max ROI index is <20
           origin='lower',
           aspect='equal',
           vmin=0,
           vmax=20,
           extent=(0, LX*AD3[0], 0, LY*AD3[1]))
        plt.colorbar(orientation='horizontal')

    plt.savefig("voxdim_{}x{}x{}_EDT_roi.svg".format(str(AD3[0]), str(AD3[1]), str(AD3[2]) ))

    plt.figure("Euclidean_DT, dist", figsize=(20,10))

    for count in range(LZ):
        plt.subplot(nrow,ncol,count+1) 

        plt.imshow(vol_EDT[:,:,count].T, 
           interpolation=None, 
           cmap='gist_earth_r', #'gist_heat_r',
           origin='lower',
           aspect='equal',
           vmin=np.min(vol_EDT),
           vmax=max_dist,
           extent=(0, LX*AD3[0], 0, LY*AD3[1]))
        plt.colorbar(orientation='horizontal')

    plt.savefig("voxdim_{}x{}x{}_EDT.svg".format(str(AD3[0]), str(AD3[1]), str(AD3[2]) ))

    plt.figure("Euclidean_DT, dist IN ROI only", figsize=(20,10))

    for count in range(LZ):
        plt.subplot(nrow,ncol,count+1) 

        plt.imshow(vol_EDT_nz[:,:,count].T, 
           interpolation=None, 
           cmap='gist_earth_r', #'gist_heat_r',
           origin='lower',
           aspect='equal',
           vmin=np.min(vol_EDT),
           vmax=max_dist_nz,
           extent=(0, LX*AD3[0], 0, LY*AD3[1]))
        plt.colorbar(orientation='horizontal')

    plt.savefig("voxdim_{}x{}x{}_EDT_nz.svg".format(str(AD3[0]), str(AD3[1]), str(AD3[2]) ))

    plt.ion()
    plt.tight_layout()
    plt.show()
