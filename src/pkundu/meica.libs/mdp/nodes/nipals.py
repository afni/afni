__docformat__ = "restructuredtext en"

from mdp import numx, NodeException, Cumulator
from mdp.utils import mult
from mdp.nodes import PCANode
sqrt = numx.sqrt

class NIPALSNode(Cumulator, PCANode):
    """Perform Principal Component Analysis using the NIPALS algorithm.
    This algorithm is particularyl useful if you have more variable than
    observations, or in general when the number of variables is huge and
    calculating a full covariance matrix may be unfeasable. It's also more
    efficient of the standard PCANode if you expect the number of significant
    principal components to be a small. In this case setting output_dim to be
    a certain fraction of the total variance, say 90%, may be of some help.

    **Internal variables of interest**

      ``self.avg``
          Mean of the input data (available after training).

      ``self.d``
          Variance corresponding to the PCA components.

      ``self.v``
          Transposed of the projection matrix (available after training).
          
      ``self.explained_variance``
          When output_dim has been specified as a fraction of the total
          variance, this is the fraction of the total variance that is actually
          explained.

    Reference for NIPALS (Nonlinear Iterative Partial Least Squares):
    Wold, H.
    Nonlinear estimation by iterative least squares procedures.
    in David, F. (Editor), Research Papers in Statistics, Wiley,
    New York, pp 411-444 (1966).

    More information about Principal Component Analysis, a.k.a. discrete
    Karhunen-Loeve transform can be found among others in
    I.T. Jolliffe, Principal Component Analysis, Springer-Verlag (1986).

    Original code contributed by:
    Michael Schmuker, Susanne Lezius, and Farzad Farkhooi (2008).
    """
    def __init__(self, input_dim=None, output_dim=None, dtype=None,
                 conv = 1e-8, max_it = 100000):
        """
        The number of principal components to be kept can be specified as
        'output_dim' directly (e.g. 'output_dim=10' means 10 components
        are kept) or by the fraction of variance to be explained
        (e.g. 'output_dim=0.95' means that as many components as necessary
        will be kept in order to explain 95% of the input variance).

        Other Arguments:
           conv   - convergence threshold for the residual error.
           max_it - maximum number of iterations

        """
        super(NIPALSNode, self).__init__(input_dim, output_dim, dtype)
        self.conv = conv
        self.max_it = max_it

    def _train(self, x):
        super(NIPALSNode, self)._train(x)

    def _stop_training(self, debug=False):
        # debug argument is ignored but needed by the base class
        super(NIPALSNode, self)._stop_training()
        self._adjust_output_dim()
        if self.desired_variance is not None:
            des_var = True
        else:
            des_var = False

        X = self.data
        conv = self.conv
        dtype = self.dtype
        mean = X.mean(axis=0)
        self.avg = mean
        max_it = self.max_it
        tlen = self.tlen

        # remove mean
        X -= mean
        var = X.var(axis=0).sum()
        self.total_variance = var
        exp_var = 0

        eigenv = numx.zeros((self.input_dim, self.input_dim), dtype=dtype)
        d = numx.zeros((self.input_dim,), dtype = dtype)
        for i in range(self.input_dim):
            it = 0
            # first score vector t is initialized to first column in X
            t = X[:, 0]
            # initialize difference
            diff = conv + 1
            while diff > conv:
                # increase iteration counter
                it += 1
                # Project X onto t to find corresponding loading p
                # and normalize loading vector p to length 1
                p = mult(X.T, t)/mult(t, t)
                p /= sqrt(mult(p, p))

                # project X onto p to find corresponding score vector t_new
                t_new = mult(X, p)
                # difference between new and old score vector
                tdiff = t_new - t
                diff = (tdiff*tdiff).sum()
                t = t_new
                if it > max_it:
                    msg = ('PC#%d: no convergence after'
                           ' %d iterations.'% (i, max_it))
                    raise NodeException(msg)

            # store ith eigenvector in result matrix
            eigenv[i, :] = p
            # remove the estimated principal component from X
            D = numx.outer(t, p)
            X -= D
            D = mult(D, p)
            d[i] = (D*D).sum()/(tlen-1)
            exp_var += d[i]/var
            if des_var and (exp_var >= self.desired_variance):
                self.output_dim = i + 1
                break

        self.d = d[:self.output_dim]
        self.v = eigenv[:self.output_dim, :].T
        self.explained_variance = exp_var
