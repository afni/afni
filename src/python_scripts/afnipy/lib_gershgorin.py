#!/usr/bin/env python

# python3 status: compatible

# ----------------------------------------------------------------------
#
# The programs here are build around taking an aff12 matrix and
# addressing the question, "Is the input matrix far from the identity
# matrix + no translation?" We make that 3x4 input into a 4x4 matrix
# M, and calculate D=M-I.  Then, we estimate the size of D, using the
# max eigenvalue of D'D... and THAT is estimated (or bounded) by using
# everyone's favorite Gershgorin theorem.

# Therefore, the focal program outputs a single (scalar) value,
# computed as follows: compute the 4 absolute row sums of D'D and take
# their maximum, and take the square root of THAT value.  This final
# number is our measure of the size of the difference matrix D, which
# can be compared with a tolerance/epsilon value.
# 
# Thanks go to RWC for constructing+explaining this approach!
# 
# There are several related programs here, basically for dealing with
# different cases: 
# + is the input a filename, which could have a MATRIX or ONELINE
#   formatted aff12 mat?
# + is the input a 3x4 list of values?
# + and more generally, is the input a NxN matrix?
#
# ----------------------------------------------------------------------

#ver='1.0' ; date='Aug 28, 2019'
# + [PT] start date for this program.  Thanks, RWC for the math behind
#        it!
#
#ver='1.1' ; date='Aug 29, 2019'
# + [PT] change way we read from file;  use pre-existing AFNI functions
#
ver='1.2' ; date='Oct 3, 2019'
# + [PT] parcellate out behavior of reformatting matrices and doing the
#        actual calc
#      + also, put in a general gershgorin calc: for NxN mat
#
######################################################################

import sys, copy
from afnipy import afni_util  as UTIL
from afnipy import lib_afni1D as LAD

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

def gershgoriny_dist_from_I_aff12_file( fname ):
    '''Input a file of an aff12 matrix (either MATRIX or ONELINE format).
    Must have exactly 12 numbers.

    See help of 'gershgoriny_dist_from_I_aff12()' for what happens
    next.

    Output is one scalar number.

    '''

    # read in, and get into proper shape with a transposition
    x = LAD.Afni1D( fname )
    x.transpose()

    if len(x.mat) == 3 and len(x.mat[0]) == 4 : 
        # then we have a MATRIX-format aff12.1D param already, and we
        # are all set to go
        out = gershgoriny_dist_from_I_aff12( x.mat )

    elif len(x.mat) == 1 and len(x.mat[0]) == 12 : 
        # then we have a ONELINE-format aff12.1D param, and we have to
        # reshape it
        M = [[0.0] * 4 for row in range(3)] 
        for i in range(3):
            M[i][:] = x.mat[0][4*i:4*(i+1)]
        dist_gershgorin = gershgoriny_dist_from_I_aff12( M )
    else:
        print("** ERROR: Input matrix in {} has some problems! Doesn't look\n"
              "   like an aff12.1D format (ONELINE, 1x12; MATRIX, 3x4)\n"
              "".format(fname))
        sys.exit(3)
        
    return dist_gershgorin

# -----------------------------------------------------------------------

def gershgoriny_dist_from_I_aff12( Minp ):
    '''Input a matrix Minp (=3x4 list) of aff12 values.  

To address the question, "Is the input matrix far from the identity
matrix + no translation?" we make it into a 4x4 matrix M, and
calculate D=M-I.  Then, we estimate the size of D, using the max
eigenvalue of D'D... and THAT is estimated (or bounded) by using
everyone's favorite Gershgorin theorem.

Therefore, this program outputs a single (scalar) value, computed as
follows: compute the 4 absolute row sums of D'D and take their
maximum, and take the square root of THAT value.  This final number is
our measure of the size of the difference matrix D, which can be
compared with a tolerance/epsilon value.

Thanks go to RWC for constructing+explaining this approach!

'''

    # Check input format
    Nrow = len(Minp)
    if Nrow != 3 :
        print("** ERROR: Nrow in input matrix is {}, not 3!"
        "".format(Nrow))
        sys.exit(6)
    Ncol = len(Minp[0])
    if Ncol != 4 :
        print("** ERROR: Ncol in input matrix is {}, not 4!"
        "".format(Ncol))
        sys.exit(6)

    # Append a row of [0,0,0,1] to the 3x4 mat to make it 4x4 square
    D    = copy.deepcopy( Minp )
    ZZ   = UTIL.calc_zero_dtype( D[0][0] ) # all zeros bc we subtract I below
    D.append( [ZZ, ZZ, ZZ, (ZZ+1)] ) # won't get bool type right, but OK...
    
    dist_gershgorin = gershgoriny_dist_from_I_general( D )

    return dist_gershgorin

# -----------------------------------------------------------------------

def gershgoriny_dist_from_I_general( mat ):
    '''
    Input: square matrix (Python 2D list)
    
    Output: Gershgorin-approximated distance from identity matrix.

    Thanks go to RWC for constructing+explaining this approach!
'''
    
    # check for badness and fail
    if not(mat) : 
        print("** ERROR: matrix is null")
        sys.exit(5)
    if not(UTIL.is_matrix_square( mat, full_check=True )) : 
        print("** ERROR: Matrix is not square")
        sys.exit(5)

    N = len(mat)

    # Go through these steps to get D: 
    #    Minp -> square form M -> D = M-I
    D    = copy.deepcopy( mat )
    for i in range(N):
        D[i][i]-= 1

    Dtr  = UTIL.transpose(D)                  # Make Dtranspose
    DtrD = UTIL.matrix_multiply_2D( Dtr, D )  # Calculated D'D

    # Calc sum of abs vals of ele in rows (SAVER); then get the max of
    # those, and sqrt it
    SAVER          = UTIL.matrix_sum_abs_val_ele_row( DtrD ) 
    max_SAVER      = max( SAVER )    
    max_SAVER_sqrt = max_SAVER**0.5  

    return max_SAVER_sqrt
