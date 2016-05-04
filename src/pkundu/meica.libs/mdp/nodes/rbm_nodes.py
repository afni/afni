__docformat__ = "restructuredtext en"

import mdp
from mdp import numx
from mdp.utils import mult

random = mdp.numx_rand.random
randn = mdp.numx_rand.randn
exp = mdp.numx.exp

# TODO: does it make sense to define the inverse of RBMNode as sampling
#    from the visible layer given an hidden state?

# this and the other replication functions should go in mdp.utils
def rrep(x, n):
    """Replicate x n-times on a new last dimension"""
    shp = x.shape + (1,)
    return x.reshape(shp).repeat(n, axis=-1)


class RBMNode(mdp.Node):
    """Restricted Boltzmann Machine node. An RBM is an undirected
    probabilistic network with binary variables. The graph is
    bipartite into observed (*visible*) and hidden (*latent*) variables.

    By default, the ``execute`` method returns the *probability* of
    one of the hiden variables being equal to 1 given the input.

    Use the ``sample_v`` method to sample from the observed variables
    given a setting of the hidden variables, and ``sample_h`` to do the
    opposite. The ``energy`` method can be used to compute the energy
    of a given setting of all variables.

    The network is trained by Contrastive Divergence, as described in
    Hinton, G. E. (2002). Training products of experts by minimizing
    contrastive divergence. Neural Computation, 14(8):1711-1800

    **Internal variables of interest**

      ``self.w``
          Generative weights between hidden and observed variables

      ``self.bv``
          bias vector of the observed variables

      ``self.bh``
          bias vector of the hidden variables

    For more information on RBMs, see
    Geoffrey E. Hinton (2007) Boltzmann machine. Scholarpedia, 2(5):1668
    """

    def __init__(self, hidden_dim, visible_dim = None, dtype = None):
        """
        :Parameters:
          hidden_dim
            number of hidden variables
          visible_dim
            number of observed variables
        """
        super(RBMNode, self).__init__(visible_dim, hidden_dim, dtype)
        self._initialized = False

    def _init_weights(self):
        # weights and biases are initialized to small random values to
        # break the simmetry that might lead to degenerate solutions during
        # learning
        self._initialized = True

        # weights
        self.w = self._refcast(randn(self.input_dim, self.output_dim)*0.1)
        # bias on the visibile (input) units
        self.bv = self._refcast(randn(self.input_dim)*0.1)
        # bias on the hidden (output) units
        self.bh = self._refcast(randn(self.output_dim)*0.1)

        # delta w, bv, bh used for momentum term
        self._delta = (0., 0., 0.)

    def _sample_h(self, v):
        # returns P(h=1|v,W,b) and a sample from it
        probs = 1./(1. + exp(-self.bh - mult(v, self.w)))
        h = (probs > random(probs.shape)).astype(self.dtype)
        return probs, h

    def _sample_v(self, h):
        # returns  P(v=1|h,W,b) and a sample from it
        probs = 1./(1. + exp(-self.bv - mult(h, self.w.T)))
        v = (probs > random(probs.shape)).astype(self.dtype)
        return probs, v

    def _train(self, v, n_updates=1, epsilon=0.1, decay=0., momentum=0.,
               update_with_ph=True, verbose=False):
        """Update the internal structures according to the input data `v`.
        The training is performed using Contrastive Divergence (CD).

        :Parameters:
          v
            a binary matrix having different variables on different columns
            and observations on the rows
          n_updates
            number of CD iterations. Default value: 1
          epsilon
            learning rate. Default value: 0.1
          decay
            weight decay term. Default value: 0.
          momentum
            momentum term. Default value: 0.
          update_with_ph
            In his code, G.Hinton updates the hidden biases using the
            probability of the hidden unit activations instead of a
            sample from it. This is in order to speed up sequential
            learning of RBMs. Set this to False to use the samples instead.
        """
        if not self._initialized:
            self._init_weights()

        # useful quantities
        n = v.shape[0]
        w, bv, bh = self.w, self.bv, self.bh

        # old gradients for momentum term
        dw, dbv, dbh = self._delta

        # first update of the hidden units for the data term
        ph_data, h_data = self._sample_h(v)
        # n updates of both v and h for the model term
        h_model = h_data.copy()
        for i in range(n_updates):
            pv_model, v_model = self._sample_v(h_model)
            ph_model, h_model = self._sample_h(v_model)

        # update w
        data_term = mult(v.T, ph_data)
        model_term = mult(v_model.T, ph_model)
        dw = momentum*dw + epsilon*((data_term - model_term)/n - decay*w)
        w += dw

        # update bv
        data_term = v.sum(axis=0)
        model_term = v_model.sum(axis=0)
        dbv = momentum*dbv + epsilon*((data_term - model_term)/n)
        bv += dbv

        # update bh
        if update_with_ph:
            data_term = ph_data.sum(axis=0)
            model_term = ph_model.sum(axis=0)
        else:
            data_term = h_data.sum(axis=0)
            model_term = h_model.sum(axis=0)
        dbh = momentum*dbh + epsilon*((data_term - model_term)/n)
        bh += dbh

        self._delta = (dw, dbv, dbh)
        self._train_err = float(((v-v_model)**2.).sum())

        if verbose:
            print 'training error', self._train_err/v.shape[0]
            ph, h = self._sample_h(v)
            print 'energy', self._energy(v, ph).sum()

    def _stop_training(self):
        #del self._delta
        #del self._train_err
        pass

    # execution methods

    @staticmethod
    def is_invertible():
        return False

    def _pre_inversion_checks(self, y):
        self._if_training_stop_training()
        # control the dimension of y
        self._check_output(y)

    def sample_h(self, v):
        """Sample the hidden variables given observations v.

        :Returns: a tuple ``(prob_h, h)``, where ``prob_h[n,i]`` is the
          probability that variable ``i`` is one given the observations
          ``v[n,:]``, and ``h[n,i]`` is a sample from the posterior probability.
        """
        self._pre_execution_checks(v)
        return self._sample_h(v)

    def sample_v(self, h):
        """Sample the observed variables given hidden variable state h.

        :Returns: a tuple ``(prob_v, v)``, where ``prob_v[n,i]`` is the
          probability that variable ``i`` is one given the hidden
          variables ``h[n,:]``, and ``v[n,i]`` is a sample from that
          conditional probability.
        """
        self._pre_inversion_checks(h)
        return self._sample_v(h)

    def _energy(self, v, h):
        return (-mult(v, self.bv) - mult(h, self.bh) -
                (mult(v, self.w)*h).sum(axis=1))

    def energy(self, v, h):
        """Compute the energy of the RBM given observed variables state `v` and
        hidden variables state `h`.
        """
        return self._energy(v, h)

    def _execute(self, v, return_probs=True):
        """If `return_probs` is True, returns the probability of the
        hidden variables h[n,i] being 1 given the observations v[n,:].
        If `return_probs` is False, return a sample from that probability.
        """
        probs, h = self._sample_h(v)
        if return_probs:
            return probs
        else:
            return h

class RBMWithLabelsNode(RBMNode):
    """Restricted Boltzmann Machine with softmax labels. An RBM is an
    undirected probabilistic network with binary variables. In this
    case, the node is partitioned into a set of observed (*visible*)
    variables, a set of hidden (*latent*) variables, and a set of
    label variables (also observed), only one of which is active at
    any time. The node is able to learn associations between the
    visible variables and the labels.

    By default, the ``execute`` method returns the *probability* of
    one of the hiden variables being equal to 1 given the input.

    Use the ``sample_v`` method to sample from the observed variables
    (visible and labels) given a setting of the hidden variables, and
    ``sample_h`` to do the opposite. The ``energy`` method can be used
    to compute the energy of a given setting of all variables.

    The network is trained by Contrastive Divergence, as described in
    Hinton, G. E. (2002). Training products of experts by minimizing
    contrastive divergence. Neural Computation, 14(8):1711-1800

    Internal variables of interest:

      ``self.w``
          Generative weights between hidden and observed variables

      ``self.bv``
          bias vector of the observed variables

      ``self.bh``
          bias vector of the hidden variables

    For more information on RBMs with labels, see
    
      * Geoffrey E. Hinton (2007) Boltzmann machine. Scholarpedia, 2(5):1668.
      * Hinton, G. E, Osindero, S., and Teh, Y. W. (2006). A fast learning
        algorithm for deep belief nets. Neural Computation, 18:1527-1554.
    """

    def __init__(self, hidden_dim, labels_dim, visible_dim=None, dtype=None):
        super(RBMWithLabelsNode, self).__init__(None, None, dtype)

        self._labels_dim = labels_dim
        if visible_dim is not None:
            self.input_dim = visible_dim+labels_dim

        self.output_dim = hidden_dim
        self._initialized = False

    def _set_input_dim(self, n):
        self._input_dim = n
        self._visible_dim = n - self._labels_dim

    def _sample_v(self, h, sample_l=False, concatenate=True):
        # returns  P(v=1|h,W,b), a sample from it, P(l=1|h,W,b),
        # and a sample from it

        ldim, vdim = self._labels_dim, self._visible_dim

        # activation
        a = self.bv + mult(h, self.w.T)
        av, al = a[:, :vdim], a[:, vdim:]

        # ## visible units: logistic activation
        probs_v = 1./(1. + exp(-av))
        v = (probs_v > random(probs_v.shape)).astype('d')

        # ## label units: softmax activation
        # subtract maximum to regularize exponent
        exponent = al - rrep(al.max(axis=1), ldim)
        probs_l = exp(exponent)
        probs_l /= rrep(probs_l.sum(axis=1), ldim)

        if sample_l:
            # ?? todo: I'm sure this can be optimized
            l = numx.zeros((h.shape[0], ldim))
            for t in range(h.shape[0]):
                l[t, :] = mdp.numx_rand.multinomial(1, probs_l[t, :])
        else:
            l = probs_l.copy()

        if concatenate:
            probs = numx.concatenate((probs_v, probs_l), axis=1)
            x = numx.concatenate((v, l), axis=1)
            return probs, x
        else:
            return probs_v, probs_l, v, l

    # execution methods

    def sample_h(self, v, l):
        """Sample the hidden variables given observations `v` and labels `l`.

        :Returns: a tuple ``(prob_h, h)``, where ``prob_h[n,i]`` is the
          probability that variable ``i`` is one given the observations
          ``v[n,:]`` and the labels ``l[n,:]``, and ``h[n,i]`` is a sample
          from the posterior probability."""

        x = numx.concatenate((v, l), axis=1)
        self._pre_execution_checks(x)
        return self._sample_h(x)

    def sample_v(self, h):
        """Sample the observed variables given hidden variable state `h`.

        :Returns: a tuple ``(prob_v, probs_l, v, l)``, where ``prob_v[n,i]``
          is the probability that the visible variable ``i`` is one given
          the hidden variables ``h[n,:]``, and ``v[n,i]`` is a sample from
          that conditional probability. ``prob_l`` and ``l`` have similar
          interpretations for the label variables. Note that the labels are
          activated using a softmax function, so that only one label can be
          active at any time.
        """
        self._pre_inversion_checks(h)

        probs_v, probs_l, v, l = self._sample_v(h, sample_l=True,
                                                concatenate=False)
        return probs_v, probs_l, v, l

    def energy(self, v, h, l):
        """Compute the energy of the RBM given observed variables state `v`
        and `l`, and hidden variables state `h`."""

        x = numx.concatenate((v, l), axis=1)
        return self._energy(x, h)

    def execute(self, v, l, return_probs = True):
        """If `return_probs` is True, returns the probability of the
        hidden variables h[n,i] being 1 given the observations v[n,:]
        and l[n,:].  If `return_probs` is False, return a sample from
        that probability.
        """
        x = numx.concatenate((v, l), axis=1)
        self._pre_execution_checks(x)

        probs, h = self._sample_h(self._refcast(x))
        if return_probs:
            return probs
        else:
            return h

    @staticmethod
    def is_invertible():
        return False

    def train(self, v, l, n_updates=1, epsilon=0.1, decay=0., momentum=0.,
              verbose=False):
        """Update the internal structures according to the visible data `v`
        and the labels `l`.
        The training is performed using Contrastive Divergence (CD).

        :Parameters:
          v
            a binary matrix having different variables on different columns
            and observations on the rows
          l
            a binary matrix having different variables on different columns
            and observations on the rows. Only one value per row should be 1.
          n_updates
            number of CD iterations. Default value: 1
          epsilon
            learning rate. Default value: 0.1
          decay
            weight decay term. Default value: 0.
          momentum
            momentum term. Default value: 0.
        """

        if not self.is_training():
            errstr = "The training phase has already finished."
            raise mdp.TrainingFinishedException(errstr)

        x = numx.concatenate((v, l), axis=1)
        self._check_input(x)

        self._train_phase_started = True
        self._train_seq[self._train_phase][0](self._refcast(x),
                                              n_updates=n_updates,
                                              epsilon=epsilon,
                                              decay=decay,
                                              momentum=momentum,
                                              verbose=verbose)
