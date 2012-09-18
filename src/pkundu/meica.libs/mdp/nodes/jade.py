__docformat__ = "restructuredtext en"

import mdp
from ica_nodes import ICANode
numx, numx_rand, numx_linalg = mdp.numx, mdp.numx_rand, mdp.numx_linalg

mult = mdp.utils.mult

class JADENode(ICANode):
    """
    Perform Independent Component Analysis using the JADE algorithm.
    Note that JADE is a batch-algorithm. This means that it needs
    all input data before it can start and compute the ICs.
    The algorithm is here given as a Node for convenience, but it
    actually accumulates all inputs it receives. Remember that to avoid
    running out of memory when you have many components and many time samples.

    JADE does not support the telescope mode.

    Main references:
    
      * Cardoso, Jean-Francois and Souloumiac, Antoine (1993).
        Blind beamforming for non Gaussian signals.
        Radar and Signal Processing, IEE Proceedings F, 140(6): 362-370.
      * Cardoso, Jean-Francois (1999).
        High-order contrasts for independent component analysis.
        Neural Computation, 11(1): 157-192.

    Original code contributed by: 
    Gabriel Beckers (2008).

    History:
    
    - May 2005    version 1.8 for MATLAB released by Jean-Francois Cardoso
    - Dec 2007    MATLAB version 1.8 ported to Python/NumPy by Gabriel Beckers
    - Feb 15 2008 Python/NumPy version adapted for MDP by Gabriel Beckers
    """

    def __init__(self, limit = 0.001, max_it=1000, verbose = False,
                 whitened = False, white_comp = None, white_parm = None,
                 input_dim = None, dtype = None):
        """
        Input arguments:

        General:

        whitened -- Set whitened == True if input data are already whitened.
                    Otherwise the node will whiten the data itself

        white_comp -- If whitened == False, you can set 'white_comp' to the
                      number of whitened components to keep during the
                      calculation (i.e., the input dimensions are reduced to
                      white_comp by keeping the components of largest variance).

        white_parm -- a dictionary with additional parameters for whitening.
                      It is passed directly to the WhiteningNode constructor.
                      Ex: white_parm = { 'svd' : True }

        limit -- convergence threshold.

        Specific for JADE:

        max_it -- maximum number of iterations

        """
        super(JADENode, self).__init__(limit, False, verbose, whitened,
                                       white_comp, white_parm, input_dim,
                                       dtype)

        self.max_it = max_it

    def core(self, data):
        # much of the code here is a more or less line by line translation of
        # the original matlab code by Jean-Francois Cardoso.
        append = numx.append
        arange = numx.arange
        arctan2 = numx.arctan2
        array = numx.array
        concatenate = numx.concatenate
        cos = numx.cos
        sin = numx.sin
        sqrt = numx.sqrt
        dtype = self.dtype
        verbose = self.verbose
        max_it = self.max_it
        (T, m) = data.shape
        X = data

        if verbose:
            print "jade -> Estimating cumulant matrices"

        # Dim. of the space of real symm matrices
        dimsymm = (m*(m+1)) // 2
        # number of cumulant matrices
        nbcm = dimsymm
        # Storage for cumulant matrices
        CM = numx.zeros((m, m*nbcm), dtype=dtype)
        R = numx.eye(m, dtype=dtype)
        # Temp for a cum. matrix
        Qij = numx.zeros((m, m), dtype=dtype)
        # Temp
        Xim = numx.zeros(m, dtype=dtype)
        # Temp
        Xijm = numx.zeros(m, dtype=dtype)

        # I am using a symmetry trick to save storage. I should write a short
        # note one of these days explaining what is going on here.
        # will index the columns of CM where to store the cum. mats.
        Range = arange(m)

        for im in xrange(m):
            Xim = X[:, im]
            Xijm = Xim*Xim
            # Note to myself: the -R on next line can be removed: it does not
            # affect the joint diagonalization criterion
            Qij = ( mult(Xijm*X.T, X) / float(T)
                    - R - 2 * numx.outer(R[:,im], R[:,im]) )
            CM[:, Range] = Qij
            Range += m
            for jm in xrange(im):
                Xijm = Xim*X[:, jm]
                Qij = ( sqrt(2) * mult(Xijm*X.T, X) / T
                        - numx.outer(R[:,im], R[:,jm]) - numx.outer(R[:,jm],
                                                                    R[:,im]) )
                CM[:, Range] = Qij
                Range += m

        # Now we have nbcm = m(m+1)/2 cumulants matrices stored in a big
        # m x m*nbcm array.

        # Joint diagonalization of the cumulant matrices
        # ==============================================

        V = numx.eye(m, dtype=dtype)

        Diag = numx.zeros(m, dtype=dtype)
        On = 0.0
        Range = arange(m)
        for im in xrange(nbcm):
            Diag = numx.diag(CM[:, Range])
            On = On + (Diag*Diag).sum(axis=0)
            Range += m

        Off = (CM*CM).sum(axis=0) - On
        # A statistically scaled threshold on `small" angles
        seuil = (self.limit*self.limit) / sqrt(T)
        # sweep number
        encore = True
        sweep = 0
        # Total number of rotations
        updates = 0
        # Number of rotations in a given seep
        upds = 0
        g = numx.zeros((2, nbcm), dtype=dtype)
        gg = numx.zeros((2, 2), dtype=dtype)
        G = numx.zeros((2, 2), dtype=dtype)
        c = 0
        s = 0
        ton = 0
        toff = 0
        theta = 0
        Gain = 0


        # Joint diagonalization proper
        # ============================
        if verbose:
            print "jade -> Contrast optimization by joint diagonalization"

        while encore:
            encore = False
            if verbose:
                print "jade -> Sweep #%3d" % sweep ,
            sweep += 1
            upds  = 0

            for p in xrange(m-1):
                for q in xrange(p+1, m):

                    Ip = arange(p, m*nbcm, m)
                    Iq = arange(q, m*nbcm, m)

                    # computation of Givens angle
                    g = concatenate([numx.atleast_2d(CM[p, Ip] - CM[q, Iq]),
                                     numx.atleast_2d(CM[p, Iq] + CM[q, Ip])])
                    gg = mult(g, g.T)
                    ton = gg[0, 0] - gg[1, 1]
                    toff = gg[0, 1] + gg[1, 0]
                    theta = 0.5 * arctan2(toff, ton + sqrt(ton*ton+toff*toff))
                    Gain = (sqrt(ton * ton + toff * toff) - ton) / 4.0

                    # Givens update
                    if abs(theta) > seuil:
                        encore = True
                        upds = upds + 1
                        c = cos(theta)
                        s = sin(theta)
                        G = array([[c, -s] , [s, c] ])
                        pair = array([p, q])
                        V[:, pair] = mult(V[:, pair], G)
                        CM[pair, :] = mult(G.T, CM[pair, :])
                        CM[:, concatenate([Ip, Iq])]= append(c*CM[:, Ip]+
                                                             s*CM[:, Iq],
                                                             -s*CM[:, Ip]+
                                                             c*CM[:, Iq],
                                                             axis=1)
                        On = On + Gain
                        Off = Off - Gain

            if verbose:
                print "completed in %d rotations" % upds
            updates += upds
            if updates > max_it:
                err_msg = 'No convergence after %d iterations.' % max_it
                raise mdp.NodeException(err_msg)

        if verbose:
            print "jade -> Total of %d Givens rotations" % updates

        # A separating matrix
        # ===================
        # B is whitening matrix

        B = V.T

        # Permute the rows of the separating matrix B to get the most energetic
        # components first. Here the **signals** are normalized to unit
        # variance. Therefore, the sort is according to the norm of the
        # columns of A = pinv(B)

        if verbose:
            print "jade -> Sorting the components"

        A = numx_linalg.pinv(B)
        B =  B[numx.argsort((A*A).sum(axis=0))[::-1], :]

        if verbose:
            print "jade -> Fixing the signs"
        b = B[:, 0]
        # just a trick to deal with sign == 0
        signs = numx.sign(numx.sign(b)+0.1)
        B = mult(numx.diag(signs), B)
        self.filters = B.T
        return theta
