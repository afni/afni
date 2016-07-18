
__docformat__ = "restructuredtext en"

import math
import mdp
from isfa_nodes import ISFANode
numx, numx_rand, numx_linalg = mdp.numx, mdp.numx_rand, mdp.numx_linalg

utils = mdp.utils
mult = utils.mult

class ProjectMatrixMixin(object):
    """Mixin class to be inherited by all ICA-like algorithms"""
    def get_projmatrix(self, transposed=1):
        """Return the projection matrix."""
        self._if_training_stop_training()
        Q = self.filters.T
        if not self.whitened:
            W = self.white.get_projmatrix(transposed=0)
            T = mult(Q, W)
        else:
            T = Q
        if transposed:
            return T.T
        return T

    def get_recmatrix(self, transposed=1):
        """Return the back-projection matrix (i.e. the reconstruction matrix).
        Note that if the unknown sources are white, this is a good
        approximation of the mixing matrix (up to a permutation matrix).
        """
        self._if_training_stop_training()
        Q = self.filters.T
        if not self.whitened:
            W = self.white.get_recmatrix(transposed=1)
            T = mult(Q, W)
        else:
            T = Q
        if transposed:
            return T
        return T.T

class ICANode(mdp.Cumulator, mdp.Node, ProjectMatrixMixin):
    """
    ICANode is a general class to handle different batch-mode algorithm for
    Independent Component Analysis. More information about ICA can be found
    among others in
    Hyvarinen A., Karhunen J., Oja E. (2001). Independent Component Analysis,
    Wiley.
    """

    def __init__(self, limit = 0.001, telescope = False, verbose = False,
                 whitened = False, white_comp = None, white_parm = None,
                 input_dim = None, dtype = None):
        """
        Input arguments:

        whitened -- Set whitened is True if input data are already whitened.
                    Otherwise the node will whiten the data itself.

        white_comp -- If whitened is False, you can set 'white_comp' to the
                      number of whitened components to keep during the
                      calculation (i.e., the input dimensions are reduced to
                      white_comp by keeping the components of largest variance).

        white_parm -- a dictionary with additional parameters for whitening.
                      It is passed directly to the WhiteningNode constructor.
                      Ex: white_parm = { 'svd' : True }

        limit -- convergence threshold.

        telescope -- If telescope == True, use Telescope mode: Instead of
          using all input data in a single batch try larger and larger chunks
          of the input data until convergence is achieved. This should lead to
          significantly faster convergence for stationary statistics. This mode
          has not been thoroughly tested and must be considered beta.
        """
        self.telescope = telescope
        self.verbose = verbose
        self.limit = limit
        self.whitened = whitened
        self.white_comp = white_comp
        if white_parm is None:
            self.white_parm = {}
        else:
            self.white_parm = white_parm
        super(ICANode, self).__init__(input_dim, None, dtype)


    def _set_input_dim(self, n):
        self._input_dim = n
        if self.whitened:
            self.output_dim = n
        elif self.white_comp is None:
            self.output_dim = n

    def _stop_training(self):
        """Whiten data if needed and call the 'core' routine to perform ICA.
           Take care of telescope-mode if needed.
        """
        super(ICANode, self)._stop_training()

        verbose = self.verbose
        core = self.core
        limit = self.limit

        # ?? rewrite as a 2-phases node
        # whiten if needed
        if not self.whitened:
            self.output_dim = self.white_comp
            white = mdp.nodes.WhiteningNode(output_dim = self.white_comp,
                                            dtype=self.dtype,
                                            **self.white_parm)
            white.train(self.data)
            self.data = white.execute(self.data)
            self.white = white

        # if output_dim not set, set it now
        if self.output_dim is None:
            self.output_dim = self.input_dim

        data = self.data

        # call 'core' in telescope mode if needed
        if self.telescope:
            minpow = math.frexp(self.input_dim*10)[1]
            maxpow = int(numx.log(data.shape[0])/numx.log(2))
            for tel in range(minpow, maxpow+1):
                index = 2**tel
                if verbose:
                    print "--\nUsing %d inputs" % index
                convergence = core(data[:index, :])
                if convergence <= limit:
                    break
        else:
            convergence = core(data)
        if verbose:
            print "Convergence criterium: ", convergence
        self.convergence = convergence

    def core(self, data):
        """This is the core routine of the ICANode. Each subclass must
        define this function to return the achieved convergence value.
        This function is also responsible for setting the ICA filters
        matrix self.filters.
        Note that the matrix self.filters is applied to the right of the
        matrix containing input data. This is the transposed of the matrix
        defining the linear transformation."""
        pass

    def _execute(self, x):
        if not self.whitened:
            x = self.white.execute(x)
        # self.filters is applied to the right of the
        # matrix containing input data. This is the transposed of the matrix
        # defining the linear transformation.
        return mult(x, self.filters)

    def _inverse(self, y):
        y = mult(y, self.filters.T)
        if not self.whitened:
            y = self.white.inverse(y)
        return y

class CuBICANode(ICANode):
    """
    Perform Independent Component Analysis using the CuBICA algorithm.
    Note that CuBICA is a batch-algorithm, which means that it needs
    all input data before it can start and compute the ICs.  The
    algorithm is here given as a Node for convenience, but it actually
    accumulates all inputs it receives. Remember that to avoid running
    out of memory when you have many components and many time samples.

    As an alternative to this batch mode you might consider the telescope
    mode (see the docs of the ``__init__`` method).

    Reference:
    Blaschke, T. and Wiskott, L. (2003).
    CuBICA: Independent Component Analysis by Simultaneous Third- and
    Fourth-Order Cumulant Diagonalization.
    IEEE Transactions on Signal Processing, 52(5), pp. 1250-1256.

    **Internal variables of interest**

      ``self.white``
          The whitening node used for preprocessing.

      ``self.filters``
          The ICA filters matrix (this is the transposed of the
          projection matrix after whitening).

      ``self.convergence``
          The value of the convergence threshold.
    """

    def core(self, data):
        # keep track of maximum angle of rotation
        # angles vary in the range [-pi, +pi]
        # put here -2pi < -pi < +pi
        self.maxangle = [-2*numx.pi]
        verbose = self.verbose

        # we need to copy to avoid overwriting during rotation.
        x = data.copy()

        # convergence criterium == maxangle
        limit = self.limit
        comp = x.shape[1]
        tlen = x.shape[0]

        # some constants
        ct_c34 = 0.0625
        ct_s34 = 0.25
        ct_c44 = 1./384
        ct_s44 = 1./96

        # initial transposed rotation matrix == identity matrix
        Qt = numx.identity(comp, dtype=self.dtype)

        # maximum number of sweeps through all possible pairs of signals
        num = int(1+round(numx.sqrt(comp)))

        # start sweeping
        for k in range(num):
            maxangle = 0
            for i in range(comp - 1):
                for j in range(i+1, comp):
                    u1 = x[:, i]
                    u2 = x[:, j]
                    sq1 = x[:, i]*x[:, i]
                    sq2 = x[:, j]*x[:, j]

                    # calculate the cumulants of 3rd and 4th order.
                    C111  = mult(sq1, u1)/tlen
                    C112  = mult(sq1, u2)/tlen
                    C122  = mult(sq2, u1)/tlen
                    C222  = mult(sq2, u2)/tlen
                    C1111 = mult(sq1, sq1)/tlen - 3.
                    C1112 = mult(sq1*u1, u2)/tlen
                    C1122 = mult(sq1, sq2)/tlen - 1.
                    C1222 = mult(sq2*u2, u1)/tlen
                    C2222 = mult(sq2, sq2)/tlen - 3.

                    c_34 = ct_c34 * (    (C111*C111+C222*C222)-
                                      3.*(C112*C112+C122*C122)-
                                      2.*(C111*C122+C112*C222)  )
                    s_34 = ct_s34 * (     C111*C112-C122*C222   )
                    c_44 = ct_c44 *(  7.*(C1111*C1111+C2222*C2222)-
                                     16.*(C1112*C1112+C1222*C1222)-
                                     12.*(C1111*C1122+C1122*C2222)-
                                     36.*(C1122*C1122)-
                                     32.*(C1112*C1222)-
                                      2.*(C1111*C2222)              )
                    s_44 = ct_s44 *(  7.*(C1111*C1112-C1222*C2222)+
                                      6.*(C1112*C1122-C1122*C1222)+
                                                (C1111*C1222-C1112*C2222)  )

                    # rotation angle that maximize the contrast function
                    phi_max = -0.25 * numx.arctan2(s_34+s_44, c_34+c_44)

                    # get the new rotation matrix.
                    # Using the function rotate with angle 'phi' on
                    # a transformation matrix corresponds to the
                    # right-multiplication by a rotation matrix
                    # with angle '-phi'.
                    utils.rotate(Qt, phi_max, [i, j])

                    # rotate input data
                    utils.rotate(x, phi_max, [i, j])

                    # keep track of maximum angle of rotation
                    maxangle = max(maxangle, abs(float(phi_max)))

            self.maxangle.append(maxangle)
            if maxangle <= limit:
                break

        self.iter = k
        if verbose:
            print "\nSweeps: ", k
        self.filters = Qt

        # return the convergence criterium
        return maxangle

class FastICANode(ICANode):
    """
    Perform Independent Component Analysis using the FastICA algorithm.
    Note that FastICA is a batch-algorithm. This means that it needs
    all input data before it can start and compute the ICs.
    The algorithm is here given as a Node for convenience, but it
    actually accumulates all inputs it receives. Remember that to avoid
    running out of memory when you have many components and many time samples.

    FastICA does not support the telescope mode (the convergence
    criterium is not robust in telescope mode).

    Reference:
    Aapo Hyvarinen (1999).
    Fast and Robust Fixed-Point Algorithms for Independent Component Analysis
    IEEE Transactions on Neural Networks, 10(3):626-634.

    **Internal variables of interest**

      ``self.white``
          The whitening node used for preprocessing.

      ``self.filters``
          The ICA filters matrix (this is the transposed of the
          projection matrix after whitening).

      ``self.convergence``
          The value of the convergence threshold.

    History:

    - 1.4.1998 created for Matlab by Jarmo Hurri, Hugo Gavert, Jaakko Sarela,
      and Aapo Hyvarinen
    - 7.3.2003  modified for Python by Thomas Wendler
    - 3.6.2004  rewritten and adapted for scipy and MDP by MDP's authors
    - 25.5.2005 now independent from scipy. Requires Numeric or numarray
    - 26.6.2006 converted to numpy
    - 14.9.2007 updated to Matlab version 2.5
    """

    def __init__(self, approach = 'defl', g = 'pow3', guess = None,
                 fine_g = 'pow3', mu = 1,
                 sample_size = 1, fine_tanh = 1, fine_gaus = 1,
                 max_it = 5000, max_it_fine = 100,
                 failures = 5, primary_limit=0.01, limit = 0.001,  verbose = False,
                 whitened = False, white_comp = None, white_parm = None,
                 input_dim = None, dtype=None):
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

        limit -- final convergence threshold.

	primary_limit -- initial convergence threshold, to switch to fine function, (i.e. linear to non-linear). PK 26-6-12.

        Specific for FastICA:

        approach  -- Approach to use. Possible values are:
                                          'defl' --> deflation
                                          'symm' --> symmetric

               g  -- Nonlinearity to use. Possible values are:
                                          'pow3' --> x^3
                                          'tanh' --> tanh(fine_tanh*x)
                                          'gaus' --> x*exp(-fine_gaus*x^2/2)
                                          'skew' --> x^2 (for skewed signals)

           fine_g -- Nonlinearity for fine tuning. Possible values
                     are the same as for 'g'. Set it to None to disable fine
                     tuning.

               mu -- Step size. If mu != 1, a stabilization procedure is used:
                     the value of mu can momentarily be halved if the algorithm
                     is stuck between two points (this is called a stroke).
                     Also if there is no convergence before half of the maximum
                     number of iterations has been reached then mu will be halved
                     for the rest of the rounds.

      sample_size -- Percentage of samples used in one iteration. If
                     sample_size < 1, samples are chosen in random order.

        fine_tanh -- parameter for 'tanh' nonlinearity
        fine_gaus -- parameter for 'gaus' nonlinearity

            guess -- initial guess for the mixing matrix (ignored if None)

           max_it -- maximum number of iterations

      max_it_fine -- maximum number of iterations for fine tuning

         failures -- maximum number of failures to allow in deflation mode

        """
        super(FastICANode, self).__init__(limit, False, verbose, whitened,
                                          white_comp, white_parm, input_dim,
                                          dtype)

        if approach in ['defl', 'symm']:
            self.approach = approach
        else:
            raise mdp.NodeException('%s approach method not known' % approach)

        if g in ['pow3', 'tanh', 'gaus', 'skew']:
            self.g = g
        else:
            raise mdp.NodeException('%s nonlinearity function not known' % g)

        if fine_g in ['pow3', 'tanh', 'gaus', 'skew', None]:
            self.fine_g = fine_g
        else:
            errmsg = '%s nonlinearity function not known' % fine_g
            raise mdp.NodeException(errmsg)

        if sample_size > 0 and sample_size <= 1:
            self.sample_size = sample_size
        else:
            raise mdp.NodeException('0<sample_size<1, %f given' % sample_size)

        self.mu = mu
        self.stabilization = mu != 1
        self.fine_tanh = fine_tanh
        self.fine_gaus = fine_gaus
        self.max_it = max_it
        self.max_it_fine = max_it_fine
        self.primary_limit = primary_limit
        self.failures = failures
        self.guess = guess

    def _get_rsamples(self, X):
        tlen = X.shape[1]
        mask = numx.where(numx_rand.random(tlen) < self.sample_size)[0]
        return X[:, mask]

    def core(self, data):
        # this is a more or less line per line translation of the original
        # matlab code.
        # Everything could be done better and more efficiently.
        # I just had no time at the moment to do it.
        # The logic behind the used_g hell is beyond my understanding :-)))

        X = data.T

        # casted constants
        comp = X.shape[0]
        tlen = X.shape[1]
        dtype = self.dtype

        # Default values and initial definitions
        fine_tanh = self.fine_tanh
        fine_gaus = self.fine_gaus
        approach = self.approach
        g = self.g
        fine_g = self.fine_g
        stabilization = self.stabilization
        mu = self.mu
        sample_size = self.sample_size
        if self.guess is None:
            # Take random orthonormal initial vectors.
            guess = utils.random_rot(comp, dtype)
        else:
            # Use user supplied mixing matrix
            guess = self._refcast(self.guess)
            if not self.whitened:
                guess = mult(guess, self.white.get_recmatrix(transposed=1))

        limit = self.limit
        primary_limit = self.primary_limit
        max_it = self.max_it
        max_it_fine = self.max_it_fine
        failures = self.failures
        verbose = self.verbose

        # set non linearities. don't blame me for the awful logic: it comes
        # from the matlab program. I didn't dare to understand it and change
        # it.
        if g == 'pow3':
            gOrig = 10
        elif g == 'tanh':
            gOrig = 20
        elif g == 'gaus':
            gOrig = 30
        else:
            gOrig = 40

        if sample_size != 1:
            gOrig += 2
        if mu != 1:
            gOrig += 1

        fine_tuning = True
        if fine_g == 'pow3':
            gFine = 11
        elif fine_g == 'tanh':
            gFine = 21
        elif fine_g == 'gaus':
            gFine = 31
        elif fine_g == 'skew':
            gFine = 41
        else:
            if mu == 1:
                gFine = gOrig + 1
            else:
                stabilization = True
                gFine = gOrig
            fine_tuning = False

        muK = 0.01
        used_g = gOrig
        stroke = 0
        fine_tuned = False
        in_secondary = False
        lng = False

        # SYMMETRIC APPROACH
        if approach == 'symm':
            # create list to store convergence
            convergence = []
            convergence_fine = []
            # orthonormal initial vectors.
            Q = guess
            QOld = numx.zeros(Q.shape, dtype)
            QOldF = numx.zeros(Q.shape, dtype)
            # This is the actual fixed-point iteration loop.
            for round in range(max_it + 1):
                if round == max_it:
                    errstr = 'No convergence after %d steps\n' % max_it
                    raise mdp.NodeException(errstr)


                # Symmetric orthogonalization. Q = Q * real(inv(Q' * Q)^(1/2));
                Q = mult(Q, utils.sqrtm(utils.inv(mult(Q.T, Q))))

                # Test for termination condition. Note that we consider
                # opposite directions here as well.
                v1 = 1.-abs((mult(Q.T, QOld)).diagonal()).min(axis=0)
                convergence.append(v1)
                v2 = 1.-abs((mult(Q.T, QOldF)).diagonal()).min(axis=0)
                convergence_fine.append(v2)

                if self.g!=self.fine_g and convergence[round] < primary_limit and not in_secondary:
                    if verbose:
                        print 'Primary convergence, switching to fine cost...'
                    used_g = gFine
                    in_secondary = True

                if convergence[round] < limit:
                    if fine_tuning and (not fine_tuned):
                        if verbose:
                            print 'Secondary convergence, fine-tuning...'
                        fine_tuned = True
                        used_g = gFine
                        mu = muK * self.mu
                        QOld = numx.zeros(Q.shape, dtype)
                        QoldF = numx.zeros(Q.shape, dtype)
                    else:
                        if verbose:
                            print 'Convergence after %d steps\n' % round
                        break
                if stabilization:
                    if (stroke == 0) and (convergence_fine[round] < limit):
                        if verbose:
                            print 'Stroke!\n'
                        stroke = mu
                        mu = 0.5*mu
                        if used_g % 2 == 0:
                            used_g += 1
                    elif (stroke != 0):
                        mu = stroke
                        stroke = 0
                        if (mu == 1) and (used_g % 2 != 0):
                            used_g -= 1
                    elif (not lng) and (round > max_it//2):
                        if verbose:
                            print 'Taking long (reducing step size)...'
                        lng = True
                        mu = 0.5*mu
                        if used_g % 2 == 0:
                            used_g += 1

                QOldF = QOld
                QOld = Q

                # Show the progress...
                if verbose:
                    msg = ('Step no. %d,'
                           ' convergence: %.7f' % (round+1,convergence[round]))
                    print msg


                # First calculate the independent components (u_i's).
                # u_i = b_i' x = x' b_i. For all x:s simultaneously this is
                # non linearity
                if used_g == 10:
                    u = mult(X.T, Q)
                    Q = mult(X, u*u*u)/tlen - 3.*Q
                elif used_g == 11:
                    u = mult(X.T, Q)
                    Gpow3 = u*u*u
                    Beta = (u*Gpow3).sum(axis=0)
                    D = numx.diag((1/(Beta - 3*tlen)))
                    Q = Q + mu * mult(Q, mult((mult(u.T, Gpow3) -
                                               numx.diag(Beta)), D))
                elif used_g == 12:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    Q = mult(Xsub, u*u*u)/Xsub.shape[1] - 3.*Q
                elif used_g == 13:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    Gpow3 = u*u*u
                    Beta = (u*Gpow3).sum(axis=0)
                    D = numx.diag((1/(Beta - 3*Xsub.shape[1])))
                    Q = Q + mu * mult(Q, mult((mult(u.T, Gpow3) -
                                               numx.diag(Beta)), D))
                elif used_g == 20:
                    u = mult(X.T, Q)
                    tang = numx.tanh(fine_tanh * u)
                    temp = (1.-tang*tang).sum(axis=0)/tlen
                    Q = mult(X, tang)/tlen - temp * Q * fine_tanh
                elif used_g == 21:
                    u = mult(X.T, Q)
                    tang = numx.tanh(fine_tanh * u)
                    Beta = (u*tang).sum(axis=0)
                    D = numx.diag(1/(Beta -
                                     fine_tanh*(1.-tang*tang).sum(axis=0)))
                    Q = Q + mu * mult(Q,
                                 mult((mult(u.T, tang)-
                                       numx.diag(Beta)), D))
                elif used_g == 22:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    tang = numx.tanh(fine_tanh * u)
                    temp = (1.-tang*tang).sum(axis=0)/Xsub.shape[1]
                    Q = mult(Xsub, tang)/Xsub.shape[1] - temp * Q * fine_tanh
                elif used_g == 23:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    tang = numx.tanh(fine_tanh * u)
                    Beta = (u*tang).sum(axis=0)
                    D = numx.diag(1/(Beta -
                                     fine_tanh*(1.-tang*tang).sum(axis=0)))
                    Q = Q + mu * mult(Q,
                                 mult((mult(u.T, tang)-
                                       numx.diag(Beta)), D))
                elif used_g == 30:
                    u = mult(X.T, Q)
                    u2 = u*u
                    ex = numx.exp(-fine_gaus*u2*0.5)
                    gauss =  u*ex
                    dgauss = (1. - fine_gaus*u2)*ex
                    Q = (mult(X, gauss)-dgauss.sum(axis=0)*Q)/tlen
                elif used_g == 31:
                    u = mult(X.T, Q)
                    u2 = u*u
                    ex = numx.exp(-fine_gaus*u2*0.5)
                    gaus =  u*ex
                    Beta = (u*gaus).sum(axis=0)
                    D = numx.diag(1/(Beta -
                                     ((1-fine_gaus*u2)*ex).sum(axis=0)))
                    Q = Q + mu * mult(Q,
                                 mult((mult(u.T, gaus)-
                                       numx.diag(Beta)), D))
                elif used_g == 32:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    u2 = u*u
                    ex = numx.exp(-fine_gaus*u2*0.5)
                    gauss =  u*ex
                    dgauss = (1. - fine_gaus*u2)*ex
                    Q = (mult(Xsub, gauss)-dgauss.sum(axis=0)*Q)/Xsub.shape[1]
                elif used_g == 33:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    u2 = u*u
                    ex = numx.exp(-fine_gaus*u2*0.5)
                    gaus = u*ex
                    Beta = (u*gaus).sum(axis=0)
                    D = numx.diag(1/(Beta -
                                     ((1-fine_gaus*u2)*ex).sum(axis=0)))
                    Q = Q + mu * mult(Q, mult((mult(u.T, gaus)-
                                               numx.diag(Beta)), D))
                elif used_g == 40:
                    u = mult(X.T, Q)
                    Q = mult(X, u*u)/tlen
                elif used_g == 41:
                    u = mult(X.T, Q)
                    Gskew = u*u
                    Beta = (u*Gskew).sum(axis=0)
                    D = numx.diag(1/Beta)
                    Q =  Q + mu * mult(Q, mult((mult(u.T, Gskew)-
                                                numx.diag(Beta)), D))
                elif used_g == 42:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    Q = mult(Xsub, u*u)/Xsub.shape[1]
                elif used_g == 43:
                    Xsub = self._get_rsamples(X)
                    u = mult(Xsub.T, Q)
                    Gskew = u*u
                    Beta = (u*Gskew).sum(axis=0)
                    D = numx.diag(1/Beta)
                    Q =  Q + mu * mult(Q, mult((mult(u.T, Gskew)-
                                                numx.diag(Beta)), D))
                else:
                    errstr = 'Nonlinearity not found: %i' % used_g
                    raise mdp.NodeException(errstr)

            self.convergence = numx.array(convergence)
            self.convergence_fine = numx.array(convergence_fine)
            ret = convergence[-1]
        # DEFLATION APPROACH
        elif approach == 'defl':
            # adjust limit!
            #limit = 1 - limit*limit*0.5
            # create array to store convergence
            convergence = []
            convergence_fine = []
            Q = numx.zeros((comp, comp), dtype=dtype)
            round = 0
            nfail = 0
            while round < comp:
                mu = self.mu
                used_g = gOrig
                stroke = 0
                fine_tuned = False
                lng = False
                end_finetuning = 0

                # Take a random initial vector of lenght 1 and orthogonalize it
                # with respect to the other vectors.
                w  = guess[:, round]
                w -= mult(mult(Q, Q.T), w)
                w /= utils.norm2(w)

                wOld = numx.zeros(w.shape, dtype)
                wOldF = numx.zeros(w.shape, dtype)
                # This is the actual fixed-point iteration loop.
                i = 1
                gabba = 1
                #for i in range(max_it + 1):
                while i <= max_it + gabba:
                    # Project the vector into the space orthogonal to the space
                    # spanned by the earlier found basis vectors. Note that
                    # we can do the projection with matrix Q, since the zero
                    # entries do not contribute to the projection.
                    w -= mult(mult(Q, Q.T), w)
                    w /= utils.norm2(w)

                    if not fine_tuned:
                        if i == max_it + 1:
                            err_msg = ('Component number %d did not'
                                       'converge in %d iterations.' % (round,
                                                                       max_it))
                            if verbose:
                                print err_msg
                            if round == 0:
                                raise mdp.NodeException(err_msg)
                            nfail += 1
                            if nfail > failures:
                                err = ('Too many failures to '
                                       'converge (%d). Giving up.' % nfail)
                                raise mdp.NodeException(err)
                            break
                    else:
                        if i >= end_finetuning:
                            wOld = w

                    # Test for termination condition. Note that the algorithm
                    # has converged if the direction of w and wOld is the same.
                    #conv = float(abs((w*wOld).sum()))
                    conv = min(utils.norm2(w-wOld), utils.norm2(w+wOld))
                    convergence.append(conv)
                    if conv < limit:
                        if fine_tuning and (not fine_tuned):
                            if verbose:
                                print 'Initial convergence, fine-tuning...'
                            fine_tuned = True
                            gabba = max_it_fine
                            wOld = numx.zeros(w.shape, dtype)
                            wOldF = numx.zeros(w.shape, dtype)
                            used_g = gFine
                            mu = muK * self.mu
                            end_finetuning = max_it_fine + i
                        else:
                            nfail = 0
                            convergence[round] = conv
                            # Calculate ICA filter.
                            Q[:, round] = w.copy()
                            # Show the progress...
                            if verbose:
                                print 'IC %d computed ( %d steps )' % (round+1,
                                                                       i+1)
                            break
                    elif stabilization:
                        conv_fine = min(utils.norm2(w-wOldF),
                                        utils.norm2(w+wOldF))
                        convergence_fine.append(conv_fine)
                        if  (stroke == 0) and conv_fine < limit:
                            if verbose:
                                print 'Stroke!'
                            stroke = mu
                            mu = 0.5*mu
                            if used_g % 2 == 0:
                                used_g += 1
                        elif (stroke != 0):
                            mu = stroke
                            stroke = 0
                            if (mu == 1) and (used_g % 2 != 0):
                                used_g -= 1
                        elif (not lng) and (i > max_it//2):
                            if verbose:
                                print 'Taking long (reducing step size)...'
                            lng = True
                            mu = 0.5*mu
                            if used_g % 2 == 0:
                                used_g += 1

                    wOldF = wOld
                    wOld = w
                    if used_g == 10:
                        u = mult(X.T, w)
                        w = mult(X, u*u*u)/tlen - 3.*w
                    elif used_g == 11:
                        u = mult(X.T, w)
                        EXGpow3 = mult(X, u*u*u)/tlen
                        Beta = mult(w.T, EXGpow3)
                        w = w - mu * (EXGpow3 - Beta*w)/(3-Beta)
                    elif used_g == 12:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        w = mult(Xsub, u*u*u)/Xsub.shape[1] - 3.*w
                    elif used_g == 13:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        EXGpow3 = mult(Xsub, u*u*u)/Xsub.shape[1]
                        Beta = mult(w.T, EXGpow3)
                        w = w - mu * (EXGpow3 - Beta*w)/(3-Beta)
                    elif used_g == 20:
                        u = mult(X.T, w)
                        tang = numx.tanh(fine_tanh * u)
                        temp = mult((1. - tang*tang).sum(axis=0), w)
                        w = (mult(X, tang) - fine_tanh*temp)/tlen
                    elif used_g == 21:
                        u = mult(X.T, w)
                        tang = numx.tanh(fine_tanh * u)
                        Beta = mult(u.T, tang)
                        temp = (1. - tang*tang).sum(axis=0)
                        w = w-mu*((mult(X, tang)-Beta*w)/(fine_tanh*temp-Beta))
                    elif used_g == 22:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        tang = numx.tanh(fine_tanh * u)
                        temp = mult((1. - tang*tang).sum(axis=0), w)
                        w = (mult(Xsub, tang) - fine_tanh*temp)/Xsub.shape[1]
                    elif used_g == 23:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        tang = numx.tanh(fine_tanh * u)
                        Beta = mult(u.T, tang)
                        w = w - mu * ((mult(Xsub, tang)-Beta*w) /
                                      (fine_tanh*(1. - tang*tang).sum(axis=0) -
                                       Beta))
                    elif used_g == 30:
                        u = mult(X.T, w)
                        u2 = u*u
                        ex = numx.exp(-fine_gaus*u2*0.5)
                        gauss =  u*ex
                        dgauss = (1. - fine_gaus *u2)*ex
                        w = (mult(X, gauss)-mult(dgauss.sum(axis=0), w))/tlen
                    elif used_g == 31:
                        u = mult(X.T, w)
                        u2 = u*u
                        ex = numx.exp(-fine_gaus*u2*0.5)
                        gauss =  u*ex
                        dgauss = (1. - fine_gaus *u2)*ex
                        Beta = mult(u.T, gauss)
                        w = w - mu*((mult(X, gauss)-Beta*w)/
                                    (dgauss.sum(axis=0)-Beta))
                    elif used_g == 32:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        u2 = u*u
                        ex = numx.exp(-fine_gaus*u2*0.5)
                        gauss =  u*ex
                        dgauss = (1. - fine_gaus *u2)*ex
                        w = (mult(Xsub, gauss)-
                             mult(dgauss.sum(axis=0), w))/Xsub.shape[1]
                    elif used_g == 33:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        u2 = u*u
                        ex = numx.exp(-fine_gaus*u2*0.5)
                        gauss =  u*ex
                        dgauss = (1. - fine_gaus *u2)*ex
                        Beta = mult(u.T, gauss)
                        w = w - mu*((mult(Xsub, gauss)-Beta*w)/
                                    (dgauss.sum(axis=0)-Beta))
                    elif used_g == 40:
                        u = mult(X.T, w)
                        w = mult(X, u*u)/tlen
                    elif used_g == 41:
                        u = mult(X.T, w)
                        EXGskew = mult(X, u*u) / tlen
                        Beta = mult(w.T, EXGskew)
                        w = w - mu * (EXGskew - mult(Beta, w))/(-Beta)
                    elif used_g == 42:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        w = mult(Xsub, u*u)/Xsub.shape[1]
                    elif used_g == 43:
                        Xsub = self._get_rsamples(X)
                        u = mult(Xsub.T, w)
                        EXGskew = mult(Xsub, u*u) / Xsub.shape[1]
                        Beta = mult(w.T, EXGskew)
                        w = w - mu * (EXGskew - Beta*w)/(-Beta)
                    else:
                        errstr = 'Nonlinearity not found: %i' % used_g
                        raise mdp.NodeException(errstr)

                    # Normalize the new w.
                    w /= utils.norm2(w)
                    i += 1

                round += 1
            self.convergence = numx.array(convergence)
            self.convergence_fine = numx.array(convergence_fine)
            ret = convergence[-1]
        self.filters = Q
        return ret


class TDSEPNode(ISFANode, ProjectMatrixMixin):
    """Perform Independent Component Analysis using the TDSEP algorithm.
    Note that TDSEP, as implemented in this Node, is an online algorithm,
    i.e. it is suited to be trained on huge data sets, provided that the
    training is done sending small chunks of data for each time.

    Reference:
    Ziehe, Andreas and Muller, Klaus-Robert (1998).
    TDSEP an efficient algorithm for blind separation using time structure.
    in Niklasson, L, Boden, M, and Ziemke, T (Editors), Proc. 8th Int. Conf.
    Artificial Neural Networks (ICANN 1998).

    **Internal variables of interest**

      ``self.white``
          The whitening node used for preprocessing.

      ``self.filters``
          The ICA filters matrix (this is the transposed of the
          projection matrix after whitening).

      ``self.convergence``
          The value of the convergence threshold.
    """
    def __init__(self, lags=1, limit = 0.00001, max_iter=10000,
                 verbose = False, whitened = False, white_comp = None,
                 white_parm = None, input_dim = None, dtype = None):
        """
        Input arguments:

        lags    -- list of time-lags to generate the time-delayed covariance
                   matrices. If lags is an integer, time-lags 1,2,...,'lags'
                   are used.
                   Note that time-lag == 0 (instantaneous correlation) is
                   always implicitly used.

        whitened -- Set whitened is True if input data are already whitened.
                    Otherwise the node will whiten the data itself.

        white_comp -- If whitened is False, you can set 'white_comp' to the
                      number of whitened components to keep during the
                      calculation (i.e., the input dimensions are reduced to
                      white_comp by keeping the components of largest variance).

        white_parm -- a dictionary with additional parameters for whitening.
                      It is passed directly to the WhiteningNode constructor.
                      Ex: white_parm = { 'svd' : True }

        limit -- convergence threshold.

        max_iter     -- If the algorithms does not achieve convergence within
                        max_iter iterations raise an Exception. Should be
                        larger than 100.
        """
        super(TDSEPNode, self).__init__(lags=lags, sfa_ica_coeff=(0., 1.),
                                        icaweights=None, sfaweights=None,
                                        whitened=whitened,
                                        white_comp=white_comp,
                                        white_parm = None,
                                        eps_contrast=limit,
                                        max_iter=max_iter, RP=None,
                                        verbose=verbose,
                                        input_dim=input_dim,
                                        output_dim=None,
                                        dtype=dtype)

    def _stop_training(self, covs=None):
        super(TDSEPNode, self)._stop_training(covs)
        # set filters
        self.filters = self.RP
        # set convergence
        self.convergence = self.final_contrast
