__docformat__ = "restructuredtext en"

import mdp
from mdp import numx
from mdp.utils import mult, matmult, invert_exp_funcs2
from mdp.nodes import GrowingNeuralGasNode

def nmonomials(degree, nvariables):
    """Return the number of monomials of a given degree in a given number
    of variables."""
    return int(mdp.utils.comb(nvariables+degree-1, degree))

def expanded_dim(degree, nvariables):
    """Return the size of a vector of dimension ``nvariables`` after
    a polynomial expansion of degree ``degree``."""
    return int(mdp.utils.comb(nvariables+degree, degree))-1

class _ExpansionNode(mdp.Node):

    def __init__(self, input_dim = None, dtype = None):
        super(_ExpansionNode, self).__init__(input_dim, None, dtype)

    def expanded_dim(self, dim):
        return dim

    @staticmethod
    def is_trainable():
        return False

    @staticmethod
    def is_invertible():
        return False

    def _set_input_dim(self, n):
        self._input_dim = n
        self._output_dim = self.expanded_dim(n)

    def _set_output_dim(self, n):
        msg = "Output dim cannot be set explicitly!"
        raise mdp.NodeException(msg)

class PolynomialExpansionNode(_ExpansionNode):
    """Perform expansion in a polynomial space."""

    def __init__(self, degree, input_dim = None, dtype = None):
        """
        Input arguments:
        degree -- degree of the polynomial space where the input is expanded
        """
        self._degree = int(degree)
        super(PolynomialExpansionNode, self).__init__(input_dim, dtype)

    def _get_supported_dtypes(self):
        """Return the list of dtypes supported by this node."""
        return (mdp.utils.get_dtypes('AllFloat') +
                mdp.utils.get_dtypes('AllInteger'))

    def expanded_dim(self, dim):
        """Return the size of a vector of dimension 'dim' after
        a polynomial expansion of degree 'self._degree'."""
        return expanded_dim(self._degree, dim)

    def _execute(self, x):
        degree = self._degree
        dim = self.input_dim
        n = x.shape[1]

        # preallocate memory
        dexp = numx.zeros((self.output_dim, x.shape[0]), dtype=self.dtype)
        # copy monomials of degree 1
        dexp[0:n, :] = x.T

        k = n
        prec_end = 0
        next_lens = numx.ones((dim+1, ))
        next_lens[0] = 0
        for i in range(2, degree+1):
            prec_start = prec_end
            prec_end += nmonomials(i-1, dim)
            prec = dexp[prec_start:prec_end, :]

            lens = next_lens[:-1].cumsum(axis=0)
            next_lens = numx.zeros((dim+1, ))
            for j in range(dim):
                factor = prec[lens[j]:, :]
                len_ = factor.shape[0]
                dexp[k:k+len_, :] = x[:, j] * factor
                next_lens[j+1] = len_
                k = k+len_

        return dexp.T

class QuadraticExpansionNode(PolynomialExpansionNode):
    """Perform expansion in the space formed by all linear and quadratic
    monomials.
    ``QuadraticExpansionNode()`` is equivalent to a
    ``PolynomialExpansionNode(2)``"""

    def __init__(self, input_dim = None, dtype = None):
        super(QuadraticExpansionNode, self).__init__(2, input_dim = input_dim,
                                                     dtype = dtype)

class RBFExpansionNode(mdp.Node):
    """Expand input space with Gaussian Radial Basis Functions (RBFs).

    The input data is filtered through a set of unnormalized Gaussian
    filters, i.e.::

       y_j = exp(-0.5/s_j * ||x - c_j||^2)

    for isotropic RBFs, or more in general::

       y_j = exp(-0.5 * (x-c_j)^T S^-1 (x-c_j))

    for anisotropic RBFs.
    """

    def __init__(self, centers, sizes, dtype = None):
        """
        :Arguments:
          centers
            Centers of the RBFs. The dimensionality
            of the centers determines the input dimensionality;
            the number of centers determines the output
            dimensionalities
          sizes
            Radius of the RBFs.

            ``sizes`` is a list with one element for each RBF, either
            a scalar (the variance of the RBFs for isotropic RBFs)
            or a covariance matrix (for anisotropic RBFs).
            If ``sizes`` is not a list, the same variance/covariance
            is used for all RBFs.
        """
        super(RBFExpansionNode, self).__init__(None, None, dtype)
        self._init_RBF(centers, sizes)

    @staticmethod
    def is_trainable():
        return False

    @staticmethod
    def is_invertible():
        return False

    def _init_RBF(self, centers, sizes):
        # initialize the centers of the RBFs
        centers = numx.array(centers, self.dtype)

        # define input/output dim
        self.set_input_dim(centers.shape[1])
        self.set_output_dim(centers.shape[0])

        # multiply sizes if necessary
        sizes = numx.array(sizes, self.dtype)
        if sizes.ndim==0 or sizes.ndim==2:
            sizes = numx.array([sizes]*self._output_dim)
        else:
            # check number of sizes correct
            if sizes.shape[0] != self._output_dim:
                msg = "There must be as many RBF sizes as centers"
                raise mdp.NodeException, msg

        if numx.isscalar(sizes[0]):
            # isotropic RBFs
            self._isotropic = True
        else:
            # anisotropic RBFs
            self._isotropic = False

            # check size
            if (sizes.shape[1] != self._input_dim or
                sizes.shape[2] != self._input_dim):
                msg = ("Dimensionality of size matrices should be the same " +
                       "as input dimensionality (%d != %d)"
                       % (sizes.shape[1], self._input_dim))
                raise mdp.NodeException, msg

            # compute inverse covariance matrix
            for i in range(sizes.shape[0]):
                sizes[i,:,:] = mdp.utils.inv(sizes[i,:,:])

        self._centers = centers
        self._sizes = sizes

    def _execute(self, x):
        y = numx.zeros((x.shape[0], self._output_dim), dtype = self.dtype)
        c, s = self._centers, self._sizes
        for i in range(self._output_dim):
            dist = x - c[i,:]
            if self._isotropic:
                tmp = (dist**2.).sum(axis=1) / s[i]
            else:
                tmp = (dist*matmult(dist, s[i,:,:])).sum(axis=1)
            y[:,i] = numx.exp(-0.5*tmp)
        return y

class GrowingNeuralGasExpansionNode(GrowingNeuralGasNode):
    """
    Perform a trainable radial basis expansion, where the centers and
    sizes of the basis functions are learned through a growing neural
    gas.

      positions of RBFs
        position of the nodes of the neural gas

      sizes of the RBFs
        mean distance to the neighbouring nodes.

    Important: Adjust the maximum number of nodes to control the
    dimension of the expansion.

    More information on this expansion type can be found in:
    B. Fritzke.
    Growing cell structures-a self-organizing network for unsupervised
    and supervised learning. Neural Networks 7, p. 1441--1460 (1994).
    """

    def __init__(self, start_poss=None, eps_b=0.2, eps_n=0.006, max_age=50,
                 lambda_=100, alpha=0.5, d=0.995, max_nodes=100,
                 input_dim=None, dtype=None):
        """
        For a full list of input arguments please check the documentation
        of GrowingNeuralGasNode.

        max_nodes (default 100) : maximum number of nodes in the
                                  neural gas, therefore an upper bound
                                  to the output dimension of the
                                  expansion.
        """
        # __init__ is overwritten only to reset the default for
        # max_nodes. The default of the GrowingNeuralGasNode is
        # practically unlimited, possibly leading to very
        # high-dimensional expansions.
        super(GrowingNeuralGasExpansionNode, self).__init__(
            start_poss=start_poss, eps_b=eps_b, eps_n=eps_n, max_age=max_age,
            lambda_=lambda_, alpha=alpha, d=d, max_nodes=max_nodes,
            input_dim=input_dim, dtype=dtype)

    def _set_input_dim(self, n):
        # Needs to be overwritten because GrowingNeuralGasNode would
        # fix the output dim to n here.
        self._input_dim = n

    def _set_output_dim(self, n):
        msg = "Output dim cannot be set explicitly!"
        raise mdp.NodeException(msg)

    @staticmethod
    def is_trainable():
        return True

    @staticmethod
    def is_invertible():
        return False

    def _stop_training(self):
        super(GrowingNeuralGasExpansionNode, self)._stop_training()

        # set the output dimension to the number of nodes of the neural gas
        self._output_dim = self.get_nodes_position().shape[0]

        # use the nodes of the learned neural gas as centers for a radial
        # basis function expansion.
        centers = self.get_nodes_position()

        # use the mean distances to the neighbours as size of the RBF expansion
        sizes = []

        for i,node in enumerate(self.graph.nodes):

            # calculate the size of the current RBF
            pos = node.data.pos
            sizes.append(numx.array([((pos-neighbor.data.pos)**2).sum()
                                     for neighbor in node.neighbors() ]).mean())

        # initialize the radial basis function expansion with centers and sizes
        self.rbf_expansion = mdp.nodes.RBFExpansionNode(centers = centers,
                                                        sizes = sizes)

    def _execute(self,x):
        return self.rbf_expansion(x)


class GeneralExpansionNode(_ExpansionNode):
    """Expands the input signal x according to a list [f_0, ... f_k]
    of functions.

    Each function f_i should take the whole two-dimensional array x as input and
    output another two-dimensional array. Moreover the output dimension should
    depend only on the input dimension.
    The output of the node is [f_0[x], ... f_k[x]], that is, the concatenation
    of each one of the outputs f_i[x].

    Original code contributed by Alberto Escalante.
    """
    def __init__(self, funcs, input_dim = None, dtype = None):
        """
        Short argument description:

          ``funcs``
               list of functions f_i that realize the expansion
        """
        self.funcs = funcs
        super(GeneralExpansionNode, self).__init__(input_dim, dtype)

    def expanded_dim(self, n):
        """The expanded dim is computed by directly applying the expansion
        functions f_i to a zero input of dimension n.
        """
        return int(self.output_sizes(n).sum())
    
    def output_sizes(self, n):
        """Return the individual output sizes of each expansion function
        when the input has lenght n"""
        sizes = numx.zeros(len(self.funcs))
        x = numx.zeros((1,n))
        for i, func in enumerate(self.funcs):
            outx = func(x)
            sizes[i] = outx.shape[1]
        return sizes

    @staticmethod
    def is_trainable():
        return False

    @staticmethod
    def is_invertible():
        return False

    def pseudo_inverse(self, x, use_hint=None):
        """Calculate a pseudo inverse of the expansion using
        scipy.optimize.

        ``use_hint``
               when calculating the pseudo inverse of the expansion,
               the hint determines the starting point for the approximation

        This method requires scipy."""

        try:
            app_x_2, app_ex_x_2 = invert_exp_funcs2(x,
                                                    self.input_dim,
                                                    self.funcs,
                                                    use_hint=use_hint,
                                                    k=0.001)
            return app_x_2.astype(self.dtype)
        except NotImplementedError, exc:
            raise mdp.MDPException(exc)

    def _execute(self, x):
        if self.input_dim is None:
            self.set_input_dim(x.shape[1])

        num_samples = x.shape[0]
        sizes = self.output_sizes(self.input_dim)

        out = numx.zeros((num_samples, self.output_dim), dtype=self.dtype)

        current_pos = 0
        for i, func in enumerate(self.funcs):
            out[:,current_pos:current_pos+sizes[i]] = func(x)
            current_pos += sizes[i]
        return out

### old weave inline code to perform a quadratic expansion
# weave C code executed in the function QuadraticExpansionNode.execute
## _EXPANSION_POL2_CCODE = """
##   // first of all, copy the linear part
##   for( int i=0; i<columns; i++ ) {
##     for( int l=0; l<rows; l++ ) {
##       dexp(l,i) = x(l,i);
##     }
##   }

##   // then, compute all monomials of second degree
##   int k=columns;
##   for( int i=0; i<columns; i++ ) {
##     for( int j=i; j<columns; j++ ) {
##       for( int l=0; l<rows; l++ ) {
##         dexp(l,k) = x(l,i)*x(l,j);
##       }
##       k++;
##     }
##   }
## """

# it was called like that:
##     def execute(self, x):
##         mdp.Node.execute(self, x)

##         rows = x.shape[0]
##         columns = self.input_dim
##         # dexp is going to contain the expanded signal
##         dexp = numx.zeros((rows, self.output_dim), dtype=self._dtype)

##         # execute the inline C code
##         weave.inline(_EXPANSION_POL2_CCODE,['rows','columns','dexp','x'],
##                  type_factories = weave.blitz_tools.blitz_type_factories,
##                  compiler='gcc',extra_compile_args=['-O3']);

##         return dexp
