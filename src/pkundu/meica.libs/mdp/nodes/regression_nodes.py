__docformat__ = "restructuredtext en"

from mdp import numx, numx_linalg, utils, Node, NodeException, TrainingException
from mdp.utils import mult

# ??? For the future: add an optional second phase to compute
# residuals, significance of the slope.

class LinearRegressionNode(Node):
    """Compute least-square, multivariate linear regression on the input
    data, i.e., learn coefficients ``b_j`` so that::

      y_i = b_0 + b_1 x_1 + ... b_N x_N ,

    for ``i = 1 ... M``, minimizes the square error given the training ``x``'s
    and ``y``'s.

    This is a supervised learning node, and requires input data ``x`` and
    target data ``y`` to be supplied during training (see ``train``
    docstring).

    **Internal variables of interest**

      ``self.beta``
          The coefficients of the linear regression
    """

    def __init__(self, with_bias=True, use_pinv=False,
                 input_dim=None, output_dim=None, dtype=None):
        """
        :Arguments:

          with_bias
            If true, the linear model includes a constant term

            - True:  y_i = b_0 + b_1 x_1 + ... b_N x_N
            - False: y_i =       b_1 x_1 + ... b_N x_N

            If present, the constant term is stored in the first
            column of ``self.beta``.

          use_pinv
            If true, uses the pseudo-inverse function to compute
            the linear regression coefficients, which is more robust
            in some cases
        """
        super(LinearRegressionNode, self).__init__(input_dim, output_dim, dtype)

        self.with_bias = with_bias
        self.use_pinv = use_pinv

        # for the linear regression estimator we need two terms
        # the first one is X^T X
        self._xTx = None
        # the second one is X^T Y
        self._xTy = None

        # keep track of how many data points have been sent
        self._tlen = 0

        # final regression coefficients
        # if with_bias=True, beta includes the bias term in the first column
        self.beta = None

    @staticmethod
    def is_invertible():
        return False

    def _check_train_args(self, x, y):
        # set output_dim if necessary
        if self._output_dim is None:
            self._set_output_dim(y.shape[1])
        # check output dimensionality
        self._check_output(y)
        if y.shape[0] != x.shape[0]:
            msg = ("The number of output points should be equal to the "
                   "number of datapoints (%d != %d)" % (y.shape[0], x.shape[0]))
            raise TrainingException(msg)

    def _train(self, x, y):
        """
        **Additional input arguments**

        y
          array of size (x.shape[0], output_dim) that contains the observed
          output to the input x's.
        """
        # initialize internal vars if necessary
        if self._xTx is None:
            if self.with_bias:
                x_size = self._input_dim + 1
            else:
                x_size = self._input_dim
            self._xTx = numx.zeros((x_size, x_size), self._dtype)
            self._xTy = numx.zeros((x_size, self._output_dim), self._dtype)

        if self.with_bias:
            x = self._add_constant(x)

        # update internal variables
        self._xTx += mult(x.T, x)
        self._xTy += mult(x.T, y)
        self._tlen += x.shape[0]

    def _stop_training(self):
        try:
            if self.use_pinv:
                invfun = utils.pinv
            else:
                invfun = utils.inv
            inv_xTx = invfun(self._xTx)
        except numx_linalg.LinAlgError, exception:
            errstr = (str(exception) +
                      "\n Input data may be redundant (i.e., some of the " +
                      "variables may be linearly dependent).")
            raise NodeException(errstr)

        self.beta = mult(inv_xTx, self._xTy)

        # remove junk
        del self._xTx
        del self._xTy

    def _execute(self, x):
        if self.with_bias:
            x = self._add_constant(x)
        return mult(x, self.beta)

    def _add_constant(self, x):
        """Add a constant term to the vector 'x'.
        x -> [1 x]
        """
        return numx.concatenate((numx.ones((x.shape[0], 1),
                                           dtype=self.dtype), x), axis=1)
