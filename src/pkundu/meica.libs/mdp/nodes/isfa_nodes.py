__docformat__ = "restructuredtext en"

import sys as _sys
import mdp
from mdp import Node, NodeException, numx, numx_rand
from mdp.nodes import WhiteningNode
from mdp.utils import (DelayCovarianceMatrix, MultipleCovarianceMatrices,
                       rotate, mult)


# TODO: support floats of size different than 64-bit; will need to change SQRT_EPS_D

# rename often used functions
sum, cos, sin, PI = numx.sum, numx.cos, numx.sin, numx.pi
SQRT_EPS_D = numx.sqrt(numx.finfo('d').eps)

def _triu(m, k=0):
    """ returns the elements on and above the k-th diagonal of m.  k=0 is the
        main diagonal, k > 0 is above and k < 0 is below the main diagonal."""
    N = m.shape[0]
    M = m.shape[1]
    x = numx.greater_equal(numx.subtract.outer(numx.arange(N),
                                               numx.arange(M)),1-k)
    out = (1-x)*m
    return out

#############
class ISFANode(Node):
    """
    Perform Independent Slow Feature Analysis on the input data.

    **Internal variables of interest**

      ``self.RP``
          The global rotation-permutation matrix. This is the filter
          applied on input_data to get output_data

      ``self.RPC``
          The *complete* global rotation-permutation matrix. This
          is a matrix of dimension input_dim x input_dim (the 'outer space'
          is retained)

      ``self.covs``
          A `mdp.utils.MultipleCovarianceMatrices` instance containing
          the current time-delayed covariance matrices of the input_data.
          After convergence the uppermost ``output_dim`` x ``output_dim``
          submatrices should be almost diagonal.
          
          ``self.covs[n-1]`` is the covariance matrix relative to the ``n``-th
          time-lag
            
          Note: they are not cleared after convergence. If you need to free
          some memory, you can safely delete them with::
          
              >>> del self.covs

      ``self.initial_contrast``
          A dictionary with the starting contrast and the SFA and ICA parts of
          it.

      ``self.final_contrast``
          Like the above but after convergence.

    Note: If you intend to use this node for large datasets please have
    a look at the ``stop_training`` method documentation for
    speeding things up.

    References:
    Blaschke, T. , Zito, T., and Wiskott, L. (2007).
    Independent Slow Feature Analysis and Nonlinear Blind Source Separation.
    Neural Computation 19(4):994-1021 (2007)
    http://itb.biologie.hu-berlin.de/~wiskott/Publications/BlasZitoWisk2007-ISFA-NeurComp.pdf
    """
    def __init__(self, lags=1, sfa_ica_coeff=(1., 1.), icaweights=None,
                 sfaweights=None, whitened=False, white_comp = None,
                 white_parm = None, eps_contrast=1e-6, max_iter=10000,
                 RP=None, verbose=False, input_dim=None, output_dim=None,
                 dtype=None):
        """
        Perform Independent Slow Feature Analysis.

        The notation is the same used in the paper by Blaschke et al. Please
        refer to the paper for more information.

        :Parameters:
          lags
            list of time-lags to generate the time-delayed covariance
            matrices (in the paper this is the set of \tau). If
            lags is an integer, time-lags 1,2,...,'lags' are used.
            Note that time-lag == 0 (instantaneous correlation) is
            always implicitly used.

          sfa_ica_coeff
            a list of float with two entries, which defines the
            weights of the SFA and ICA part of the objective
            function. They are called b_{SFA} and b_{ICA} in the
            paper.

          sfaweights
            weighting factors for the covariance matrices relative
            to the SFA part of the objective function (called
            \kappa_{SFA}^{\tau} in the paper). Default is
            [1., 0., ..., 0.]
            For possible values see the description of icaweights.

          icaweights
            weighting factors for the cov matrices relative
            to the ICA part of the objective function (called
            \kappa_{ICA}^{\tau} in the paper). Default is 1.
            Possible values are:

            - an integer ``n``: all matrices are weighted the same
              (note that it does not make sense to have ``n != 1``)

            - a list or array of floats of ``len == len(lags)``:
              each element of the list is used for weighting the
              corresponding matrix

            - ``None``: use the default values.

          whitened
            ``True`` if input data is already white, ``False``
            otherwise (the data will be whitened internally).

          white_comp
            If whitened is false, you can set ``white_comp`` to the
            number of whitened components to keep during the
            calculation (i.e., the input dimensions are reduced to
            ``white_comp`` by keeping the components of largest variance).
          white_parm
            a dictionary with additional parameters for whitening.
            It is passed directly to the WhiteningNode constructor.
            Ex: white_parm = { 'svd' : True }

          eps_contrast
            Convergence is achieved when the relative
            improvement in the contrast is below this threshold.
            Values in the range [1E-4, 1E-10] are usually
            reasonable.

          max_iter
            If the algorithms does not achieve convergence within
            max_iter iterations raise an Exception. Should be
            larger than 100.

          RP
            Starting rotation-permutation matrix. It is an
            input_dim x input_dim matrix used to initially rotate the
            input components. If not set, the identity matrix is used.
            In the paper this is used to start the algorithm at the
            SFA solution (which is often quite near to the optimum).

          verbose
            print progress information during convergence. This can
            slow down the algorithm, but it's the only way to see
            the rate of improvement and immediately spot if something
            is going wrong.

          output_dim
            sets the number of independent components that have to
            be extracted. Note that if this is not smaller than
            input_dim, the problem is solved linearly and SFA
            would give the same solution only much faster.
        """
        # check that the "lags" argument has some meaningful value
        if isinstance(lags, (int, long)):
            lags = range(1, lags+1)
        elif isinstance(lags, (list, tuple)):
            lags = numx.array(lags, "i")
        elif isinstance(lags, numx.ndarray):
            if not (lags.dtype.char in ['i', 'l']):
                err_str = "lags must be integer!"
                raise NodeException(err_str)
            else:
                pass
        else:
            err_str = ("Lags must be int, list or array. Found "
                       "%s!" % (type(lags).__name__))
            raise NodeException(err_str)
        self.lags = lags

        # sanity checks for weights
        if icaweights is None:
            self.icaweights = 1.
        else:
            if (len(icaweights) != len(lags)):
                err = ("icaweights vector length is %d, "
                       "should be %d" % (str(len(icaweights)), str(len(lags))))
                raise NodeException(err)
            self.icaweights = icaweights

        if sfaweights is None:
            self.sfaweights = [0]*len(lags)
            self.sfaweights[0] = 1.
        else:
            if (len(sfaweights) != len(lags)):
                err = ("sfaweights vector length is %d, "
                       "should be %d" % (str(len(sfaweights)), str(len(lags))))
                raise NodeException(err)
            self.sfaweights = sfaweights

        # store attributes
        self.sfa_ica_coeff = sfa_ica_coeff
        self.max_iter = max_iter
        self.verbose = verbose
        self.eps_contrast = eps_contrast

        # if input is not white, insert a WhiteningNode
        self.whitened = whitened
        if not whitened:
            if white_parm is None:
                white_parm = {}
            if output_dim is not None:
                white_comp = output_dim
            elif white_comp is not None:
                output_dim = white_comp
            self.white = WhiteningNode(input_dim=input_dim,
                                       output_dim=white_comp,
                                       dtype=dtype, **white_parm)

        # initialize covariance matrices
        self.covs = [ DelayCovarianceMatrix(dt, dtype=dtype) for dt in lags ]

        # initialize the global rotation-permutation matrix
        # if not set that we'll eventually be an identity matrix
        self.RP = RP

        # initialize verbose structure to print nice and useful progress info
        if verbose:
            info = { 'sweep' : max(len(str(self.max_iter)), 5),
                     'perturbe': max(len(str(self.max_iter)), 5),
                     'float' : 5+8,
                     'fmt' : "%.5e",
                     'sep' : " | "}
            f1 = "Sweep".center(info['sweep'])
            f1_2 = "Pertb". center(info['perturbe'])
            f2 = "SFA part".center(info['float'])
            f3 = "ICA part".center(info['float'])
            f4 = "Contrast".center(info['float'])
            header = info['sep'].join([f1, f1_2, f2, f3, f4])
            info['header'] = header+'\n'
            info['line'] = len(header)*"-"
            self._info = info

        # finally call base class constructor
        super(ISFANode, self).__init__(input_dim, output_dim, dtype)

    def _get_supported_dtypes(self):
        """Return the list of dtypes supported by this node.
      
        Support floating point types with size larger or equal than 64 bits.
        """
        return [t for t in mdp.utils.get_dtypes('Float') if t.itemsize>=8]

    def _set_dtype(self, dtype):
        # when typecode is set, we set the whitening node if needed and
        # the SFA and ICA weights
        self._dtype = dtype
        if not self.whitened and self.white.dtype is None:
            self.white.dtype = dtype
        self.icaweights = numx.array(self.icaweights, dtype)
        self.sfaweights = numx.array(self.sfaweights, dtype)

    def _set_input_dim(self, n):
        self._input_dim = n
        if not self.whitened and self.white.output_dim is not None:
            self._effective_input_dim = self.white.output_dim
        else:
            self._effective_input_dim = n

    def _train(self, x):
        # train the whitening node if needed
        if not self.whitened:
            self.white.train(x)
        # update the covariance matrices
        [self.covs[i].update(x) for i in range(len(self.lags))]

    def _execute(self, x):
        # filter through whitening node if needed
        if not self.whitened:
            x = self.white.execute(x)
        # rotate input
        return mult(x, self.RP)

    def _inverse(self, y):
        # counter-rotate input
        x = mult(y, self.RP.T)
        # invert whitening node if needed
        if not self.whitened:
            x = self.white.inverse(x)
        return x

    def _fmt_prog_info(self, sweep, pert, contrast, sfa = None, ica = None):
        # for internal use only!
        # format the progress information
        # don't try to understand this code: it Just Works (TM)
        fmt = self._info
        sweep_str = str(sweep).rjust(fmt['sweep'])
        pert_str = str(pert).rjust(fmt['perturbe'])
        if sfa is None:
            sfa_str = fmt['float']*' '
        else:
            sfa_str = (fmt['fmt']%(sfa)).rjust(fmt['float'])
        if ica is None:
            ica_str = fmt['float']*' '
        else:
            ica_str = (fmt['fmt'] % (ica)).rjust(fmt['float'])
        contrast_str = (fmt['fmt'] % (contrast)).rjust(fmt['float'])
        table_entry = fmt['sep'].join([sweep_str,
                                       pert_str,
                                       sfa_str,
                                       ica_str,
                                       contrast_str])
        return table_entry

    def _get_eye(self):
        # return an identity matrix with the right dimensions and type
        return numx.eye(self._effective_input_dim, dtype=self.dtype)

    def _get_rnd_rotation(self, dim):
        # return a random rot matrix with the right dimensions and type
        return mdp.utils.random_rot(dim, self.dtype)

    def _get_rnd_permutation(self, dim):
        # return a random permut matrix with the right dimensions and type
        zero = numx.zeros((dim, dim), dtype=self.dtype)
        row = numx_rand.permutation(dim)
        for col in range(dim):
            zero[row[col], col] = 1.
        return zero

    def _givens_angle(self, i, j, covs, bica_bsfa=None, complete=0):
        # Return the Givens rotation angle for which the contrast function
        # is minimal
        if bica_bsfa is None:
            bica_bsfa = self._bica_bsfa
        if j < self.output_dim:
            return self._givens_angle_case1(i, j, covs,
                                            bica_bsfa, complete=complete)
        else:
            return self._givens_angle_case2(i, j, covs,
                                            bica_bsfa, complete=complete)


    def _givens_angle_case2(self, m, n, covs, bica_bsfa, complete=0):
        # This function makes use of the constants computed in the paper
        #
        # R -> R
        # m -> \mu
        # n -> \nu
        #
        # Note that the minus sign before the angle phi is there because
        # in the paper the rotation convention is the opposite of ours.

        ncovs = covs.ncovs
        covs = covs.covs
        icaweights = self.icaweights
        sfaweights = self.sfaweights
        R = self.output_dim
        bica, bsfa = bica_bsfa

        Cmm, Cmn, Cnn = covs[m, m, :], covs[m, n, :], covs[n, n, :]
        d0 =   (sfaweights * Cmm*Cmm).sum()
        d1 = 4*(sfaweights * Cmn*Cmm).sum()
        d2 = 2*(sfaweights * (2*Cmn*Cmn + Cmm*Cnn)).sum()
        d3 = 4*(sfaweights * Cmn*Cnn).sum()
        d4 =   (sfaweights * Cnn*Cnn).sum()
        e0 = 2*(icaweights * ((covs[:R, m, :]*covs[:R, m, :]).sum(axis=0)
                              - Cmm*Cmm)).sum()
        e1 = 4*(icaweights * ((covs[:R, m, :]*covs[:R, n, :]).sum(axis=0)
                              - Cmm*Cmn)).sum()
        e2 = 2*(icaweights * ((covs[:R, n, :]*covs[:R, n, :]).sum(axis=0)
                              - Cmn*Cmn)).sum()

        s22 = 0.25 * bsfa*(d1+d3)   + 0.5* bica*(e1)
        c22 = 0.5  * bsfa*(d0-d4)   + 0.5* bica*(e0-e2)
        s24 = 0.125* bsfa*(d1-d3)
        c24 = 0.125* bsfa*(d0-d2+d4)

        # Compute the contrast function in a grid of angles to find a
        # first approximation for the minimum.  Repeat two times
        # (effectively doubling the resolution). Note that we can do
        # that because we know we have a single minimum.
        #
        # npoints should not be too large otherwise the contrast
        # funtion appears to be constant. This is because we hit the
        # maximum resolution for the cosine function (ca. 1e-15)
        npoints = 100
        left = -PI/2 - PI/(npoints+1)
        right = PI/2 + PI/(npoints+1)
        for iter in (1, 2):
            phi = numx.linspace(left, right, npoints+3)
            contrast = c22*cos(-2*phi)+s22*sin(-2*phi)+\
                       c24*cos(-4*phi)+s24*sin(-4*phi)
            minidx = contrast.argmin()
            left = phi[max(minidx-1, 0)]
            right = phi[min(minidx+1, len(phi)-1)]

        # The contrast is almost a parabola around the minimum.
        # To find the minimum we can therefore compute the derivative
        # (which should be a line) and calculate its root.
        # This step helps to overcome the resolution limit of the
        # cosine function and clearly improve the final result.
        der_left = 2*c22*sin(-2*left)- 2*s22*cos(-2*left)+\
                   4*c24*sin(-4*left)- 4*s24*cos(-4*left)
        der_right = 2*c22*sin(-2*right)-2*s22*cos(-2*right)+\
                    4*c24*sin(-4*right)-4*s24*cos(-4*right)
        if abs(der_left - der_right) < SQRT_EPS_D:
            minimum = phi[minidx]
        else:
            minimum = right - der_right*(right-left)/(der_right-der_left)

        dc = numx.zeros((ncovs,), dtype = self.dtype)
        for t in range(ncovs):
            dg = covs[:R, :R, t].diagonal()
            dc[t] = (dg*dg).sum(axis=0)
        dc = ((dc-Cmm*Cmm)*sfaweights).sum()

        ec = numx.zeros((ncovs, ), dtype = self.dtype)
        for t in range(ncovs):
            ec[t] = sum([covs[i, j, t]*covs[i, j, t] for i in range(R-1)
                         for j in range(i+1, R) if i != m and j != m])
        ec = 2*(ec*icaweights).sum()
        a20 = 0.125*bsfa*(3*d0+d2+3*d4+8*dc)+0.5*bica*(e0+e2+2*ec)
        minimum_contrast = a20+c22*cos(-2*minimum)+s22*sin(-2*minimum)+\
                           c24*cos(-4*minimum)+s24*sin(-4*minimum)
        if complete:
            # Compute the contrast between -pi/2 and pi/2
            # (useful for testing purposes)
            npoints = 1000
            phi = numx.linspace(-PI/2, PI/2, npoints+1)
            contrast = a20 + c22*cos(-2*phi) + s22*sin(-2*phi) +\
                       c24*cos(-4*phi) + s24*sin(-4*phi)
            return phi, contrast, minimum, minimum_contrast
        else:
            return minimum, minimum_contrast


    def _givens_angle_case1(self, m, n, covs, bica_bsfa, complete=0):
        # This function makes use of the constants computed in the paper
        #
        # R -> R
        # m -> \mu
        # n -> \nu
        #
        # Note that the minus sign before the angle phi is there because
        # in the paper the rotation convention is the opposite of ours.
        ncovs = covs.ncovs
        covs = covs.covs
        icaweights = self.icaweights
        sfaweights = self.sfaweights
        bica, bsfa = bica_bsfa

        Cmm, Cmn, Cnn = covs[m, m, :], covs[m, n, :], covs[n, n, :]
        d0 =   (sfaweights * (Cmm*Cmm+Cnn*Cnn)).sum()
        d1 = 4*(sfaweights * (Cmm*Cmn-Cmn*Cnn)).sum()
        d2 = 2*(sfaweights * (2*Cmn*Cmn+Cmm*Cnn)).sum()
        e0 = 2*(icaweights * Cmn*Cmn).sum()
        e1 = 4*(icaweights * (Cmn*Cnn-Cmm*Cmn)).sum()
        e2 =   (icaweights * ((Cmm-Cnn)*(Cmm-Cnn)-2*Cmn*Cmn)).sum()

        s24 = 0.25* (bsfa * d1    + bica * e1)
        c24 = 0.25* (bsfa *(d0-d2)+ bica *(e0-e2))

        # compute the exact minimum
        # Note that 'arctan' finds always the first maximum
        # because s24sin(4p)+c24cos(4p)=const*cos(4p-arctan)
        # the minimum lies +pi/4 apart (period = pi/2).
        # In other words we want that: abs(minimum) < pi/4
        phi4 = numx.arctan2(s24, c24)
        # use if-structure until bug in numx.sign is solved
        if  phi4 >= 0:
            minimum = -0.25*(phi4-PI)
        else:
            minimum = -0.25*(phi4+PI)

        # compute all constants:
        R = self.output_dim
        dc = numx.zeros((ncovs, ), dtype = self.dtype)
        for t in range(ncovs):
            dg = covs[:R, :R, t].diagonal()
            dc[t] = (dg*dg).sum(axis=0)
        dc = ((dc-Cnn*Cnn-Cmm*Cmm)*sfaweights).sum()
        ec = numx.zeros((ncovs, ), dtype = self.dtype)
        for t in range(ncovs):
            triu_covs = _triu(covs[:R, :R, t], 1).ravel()
            ec[t] = ((triu_covs*triu_covs).sum() - covs[m, n, t]*covs[m, n, t])
        ec = 2*(icaweights*ec).sum()
        a20 = 0.25*(bsfa*(4*dc+d2+3*d0)+bica*(4*ec+e2+3*e0))
        minimum_contrast = a20+c24*cos(-4*minimum)+s24*sin(-4*minimum)
        npoints = 1000
        if complete == 1:
            # Compute the contrast between -pi/2 and pi/2
            # (useful for testing purposes)
            phi = numx.linspace(-PI/2, PI/2, npoints+1)
            contrast = a20 + c24*cos(-4*phi) + s24*sin(-4*phi)
            return phi, contrast, minimum, minimum_contrast
        elif complete == 2:
            phi = numx.linspace(-PI/4, PI/4, npoints+1)
            contrast = a20 + c24*cos(-4*phi) + s24*sin(-4*phi)
            return phi, contrast, minimum, minimum_contrast
        else:
            return minimum, minimum_contrast


    def _get_contrast(self, covs, bica_bsfa = None):
        if bica_bsfa is None:
            bica_bsfa = self._bica_bsfa
        # return current value of the contrast
        R = self.output_dim
        ncovs = covs.ncovs
        covs = covs.covs
        icaweights = self.icaweights
        sfaweights = self.sfaweights
        # unpack the bsfa and bica coefficients
        bica, bsfa = bica_bsfa
        sfa = numx.zeros((ncovs, ), dtype=self.dtype)
        ica = numx.zeros((ncovs, ), dtype=self.dtype)
        for t in range(ncovs):
            sq_corr =  covs[:R, :R, t]*covs[:R, :R, t]
            sfa[t] = sq_corr.trace()
            ica[t] = 2*_triu(sq_corr, 1).ravel().sum()
        return (bsfa*sfaweights*sfa).sum(), (bica*icaweights*ica).sum()

    def _adjust_ica_sfa_coeff(self):
        # adjust sfa/ica ratio. ica and sfa term are scaled
        # differently because sfa accounts for the diagonal terms
        # whereas ica accounts for the off-diagonal terms
        ncomp = self.output_dim
        if ncomp > 1:
            bica =  self.sfa_ica_coeff[1]/(ncomp*(ncomp-1))
            bsfa = -self.sfa_ica_coeff[0]/ncomp
        else:
            bica =  0.#self.sfa_ica_coeff[1]
            bsfa = -self.sfa_ica_coeff[0]
        self._bica_bsfa = [bica, bsfa]

    def _fix_covs(self, covs=None):
        # fiv covariance matrices
        if covs is None:
            covs = self.covs
            if not self.whitened:
                white = self.white
                white.stop_training()
                proj = white.get_projmatrix(transposed=0)
            else:
                proj = None
            # fix and whiten the covariance matrices
            for i in range(len(self.lags)):
                covs[i], avg, avg_dt, tlen = covs[i].fix(proj)

            # send the matrices to the container class
            covs = MultipleCovarianceMatrices(covs)
            # symmetrize the cov matrices
            covs.symmetrize()
        self.covs = covs

    def _optimize(self):
        # optimize contrast function

        # save initial contrast
        sfa, ica = self._get_contrast(self.covs)
        self.initial_contrast = {'SFA': sfa,
                                 'ICA': ica,
                                 'TOT': sfa + ica}
        # info headers
        if self.verbose:
            print self._info['header']+self._info['line']

        # initialize control variables
        # contrast
        contrast = sfa+ica
        # local rotation matrix
        Q = self._get_eye()
        # local copy of correlation matrices
        covs = self.covs.copy()
        # maximum improvement in the contrast function
        max_increase = self.eps_contrast
        # Number of sweeps
        sweep = 0
        # flag for stopping sweeping
        sweeping = True
        # flag to check if we already perturbed the outer space
        # - negative means that we exit from this routine
        #   because we hit numerical precision or because
        #   there's no outer space to be perturbed (input_dim == outpu_dim)
        # - positive means the number of perturbations done
        #   before finding no further improvement
        perturbed = 0

        # size of the perturbation matrix
        psize = self._effective_input_dim-self.output_dim

        # if there is no outer space don't perturbe
        if self._effective_input_dim == self.output_dim:
            perturbed = -1

        # local eye matrix
        eye = self._get_eye()

        # main loop
        # we'll keep on sweeping until the contrast has improved less
        # then self.eps_contrast
        part_sweep = 0
        while sweeping:
            # update number of sweeps
            sweep += 1

            # perform a single sweep
            max_increase, covs, Q, contrast = self._do_sweep(covs, Q, contrast)

            if max_increase < 0 or contrast == 0:
                # we hit numerical precision, exit!
                sweeping = False
                if perturbed == 0:
                    perturbed = -1
                else:
                    perturbed = -perturbed

            if (max_increase < self.eps_contrast) and (max_increase) >= 0 :
                # rate of change is small for all pairs in a sweep
                if perturbed == 0:
                    # perturbe the outer space one time with a random rotation
                    perturbed = 1
                elif perturbed >= 1 and part_sweep == sweep-1:
                    # after the last pertubation no useful step has
                    # been done. exit!
                    sweeping = False
                elif perturbed < 0:
                    # we can't perturbe anymore
                    sweeping = False
                # keep track of the last sweep we perturbed
                part_sweep = sweep

            # perform perturbation if needed
            if perturbed >= 1 and sweeping is True:
                # generate a random rotation matrix for the external subspace
                PRT = eye.copy()
                rot = self._get_rnd_rotation(psize)
                # generate a random permutation matrix for the ext. subspace
                perm = self._get_rnd_permutation(psize)
                # combine rotation and permutation
                rot_perm = mult(rot, perm)
                # apply rotation+permutation
                PRT[self.output_dim:, self.output_dim:] = rot_perm
                covs.transform(PRT)
                Q = mult(Q, PRT)
                # increment perturbation counter
                perturbed += 1

            # verbose progress information
            if self.verbose:
                table_entry = self._fmt_prog_info(sweep,
                                                  perturbed,
                                                  contrast)
                _sys.stdout.write(table_entry+len(table_entry)*'\b')
                _sys.stdout.flush()

            # if we made too many sweeps exit with error!
            if sweep == self.max_iter:
                err_str = ("Failed to converge, maximum increase= "
                           "%.5e" % (max_increase))
                raise NodeException(err_str)

        # if we land here, we have converged!
        # calculate output contrast

        sfa, ica =  self._get_contrast(covs)
        contrast = sfa+ica
        # print final information
        if self.verbose:
            print self._fmt_prog_info(sweep, perturbed,
                                      contrast, sfa, ica)
            print self._info['line']

        self.final_contrast = {'SFA': sfa,
                               'ICA': ica,
                               'TOT': sfa + ica}

        # finally return optimal rotation matrix
        return Q

    def _do_sweep(self, covs, Q, prev_contrast):
        # perform a single sweep

        # initialize maximal improvement in a single sweep
        max_increase = -1
        # shuffle rotation order
        numx_rand.shuffle(self.rot_axis)
        # sweep through all axes combinations
        for (i, j) in self.rot_axis:
            # get the angle that minimizes the contrast
            # and the contrast value
            angle, contrast = self._givens_angle(i, j, covs)
            if contrast == 0:
                # we hit numerical precision in case when b_sfa == 0
                # we can only break things from here on, better quit!
                max_increase = -1
                break

            # relative improvement in the contrast function
            relative_diff = (prev_contrast-contrast)/abs(prev_contrast)
            if relative_diff < 0:
                # if rate of change is negative we hit numerical precision
                # or we already sit on the optimum for this pair of axis.
                # don't rotate anymore and go to the next pair
                continue

            # update the rotation matrix
            rotate(Q, angle, [i, j])
            # rotate the covariance matrices
            covs.rotate(angle, [i, j])

            # store maximum and previous rate of change
            max_increase = max(max_increase, relative_diff)
            prev_contrast = contrast

        return max_increase, covs, Q, contrast

    def _stop_training(self, covs=None):
        """Stop the training phase.

        If the node is used on large datasets it may be wise to first
        learn the covariance matrices, and then tune the parameters
        until a suitable parameter set has been found (learning the
        covariance matrices is the slowest part in this case).  This
        could be done for example in the following way (assuming the
        data is already white):

        >>> covs=[mdp.utils.DelayCovarianceMatrix(dt, dtype=dtype)
        ...       for dt in lags]
        >>> for block in data:
        ...     [covs[i].update(block) for i in range(len(lags))]

        You can then initialize the ISFANode with the desired parameters,
        do a fake training with some random data to set the internal
        node structure and then call stop_training with the stored covariance
        matrices. For example:

        >>> isfa = ISFANode(lags, .....)
        >>> x = mdp.numx_rand.random((100, input_dim)).astype(dtype)
        >>> isfa.train(x)
        >>> isfa.stop_training(covs=covs)

        This trick has been used in the paper to apply ISFA to surrogate
        matrices, i.e. covariance matrices that were not learnt on a
        real dataset.
        """
        # fix, whiten, symmetrize and weight the covariance matrices
        # the functions sets also the number of input components self.ncomp
        self._fix_covs(covs)
        # if output_dim were not set, set it to be the number of input comps
        if self.output_dim is None:
            self.output_dim = self._effective_input_dim
        # adjust b_sfa and b_ica
        self._adjust_ica_sfa_coeff()
        # initialize all possible rotation axes
        self.rot_axis = [(i, j) for i in range(0, self.output_dim)
                         for j in range(i+1, self._effective_input_dim)]

        # initialize the global rotation-permutation matrix (RP):
        RP = self.RP
        if RP is None:
            RP = self._get_eye()
        else:
            # apply the global rotation matrix
            self.covs.transform(RP)

        # find optimal rotation
        Q = self._optimize()
        RP = mult(RP, Q)
        # rotate and permute the covariance matrices
        # we do it here in one step, to avoid the cumulative errors
        # of multiple rotations in _optimize
        self.covs.transform(Q)

        # keep the complete rotation-permutation matrix
        self.RPC = RP.copy()

        # Reduce dimension to match output_dim#
        RP = RP[:, :self.output_dim]
        # the variance for the derivative of a whitened signal is
        # 0 <= v <= 4, therefore the diagonal elements of the delayed
        # covariance matrice with time lag = 1 (covs[:,:,0]) are
        # -1 <= v' <= +1
        # reorder the components to have them ordered by slowness
        d = (self.covs.covs[:self.output_dim, :self.output_dim, 0]).diagonal()
        idx = d.argsort()[::-1]
        self.RP = RP.take(idx, axis=1)

        # we could in principle clean up self.covs, as we do in SFANode or
        # PCANode, but this algorithm is not stable enough to rule out
        # possible problems. When these occcurs examining the covariance
        # matrices is often the only way to debug.
        #del self.covs
