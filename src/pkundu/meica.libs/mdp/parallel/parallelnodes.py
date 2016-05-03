"""
Module for MDP Nodes that support parallel training.

This module contains both the parallel base class and some parallel
implementations of MDP nodes. Note that such ParallelNodes are only needed for
training, parallel execution works with any Node that can be pickled.
"""

# WARNING: There is a problem with unpickled arrays in NumPy < 1.1.x, see
# http://projects.scipy.org/scipy/numpy/ticket/551
# To circumvent this, you can use a copy() of all unpickled arrays.

import inspect

import mdp
from mdp import numx


class NotForkableParallelException(mdp.NodeException):
    """Exception to signal that a fork is not possible.

    This exception is can be safely used and should be caught inside the
    ParallelFlow or the Scheduler.
    """
    pass


class JoinParallelException(mdp.NodeException):
    """Exception for errors when joining parallel nodes."""
    pass


class ParallelExtensionNode(mdp.ExtensionNode, mdp.Node):
    """Base class for parallel trainable MDP nodes.

    With the fork method new node instances are created which can then be
    trained. With the join method the trained instances are then merged back
    into a single node instance.

    This class defines default methods which raise a
    TrainingPhaseNotParallelException exception.
    """

    extension_name = "parallel"

    # TODO: allow that forked nodes are not forkable themselves,
    #    and are not joinable either
    #    this implies that caching does not work for these
    
    def fork(self):
        """Return a new instance of this node class for remote training.

        This is a template method, the actual forking should be implemented in
        _fork.

        The forked node should be a ParallelNode of the same class as well,
        thus allowing recursive forking and joining.
        """
        return self._fork()

    def join(self, forked_node):
        """Absorb the trained node from a fork into this parent node.

        This is a template method, the actual joining should be implemented in
        _join.
        """
        # Warning: Use the properties / setters here. Otherwise we get problems
        #    in certain situations (e.g., for FlowNode).
        if self.dtype is None:
            self.dtype = forked_node.dtype
        if self.input_dim is None:
            self.input_dim = forked_node.input_dim
        if self.output_dim is None:
            self.output_dim = forked_node.output_dim
        if forked_node._train_phase_started and not self._train_phase_started:
            self._train_phase_started = True
        self._join(forked_node)

    ## overwrite these methods ##

    def _fork(self):
        """Hook method for forking with default implementation.

        Overwrite this method for nodes that can be parallelized.
        You can use _default_fork, if that is compatible with your node class,
        typically the hard part is the joining.
        """
        raise NotForkableParallelException("fork is not implemented " +
                                           "by this node (%s)" %
                                           str(self.__class__))

    def _join(self, forked_node):
        """Hook method for joining, to be overridden."""
        raise JoinParallelException("join is not implemented " +
                                    "by this node (%s)" %
                                    str(self.__class__))
    
    @staticmethod
    def use_execute_fork():
        """Return True if node requires a fork / join even during execution.
        
        The default output is False, overwrite this method if required.
        
        Note that the same fork and join methods are used as during training,
        so the distinction must be implemented in the custom _fork and _join
        methods.
        """
        return False
    
    ## helper methods ##

    def _default_fork(self):
        """Default implementation of _fork.

        It uses introspection to determine the init kwargs and tries to fill
        them with attributes. These kwargs are then used to instanciate
        self.__class__ to create the fork instance.

        So you can use this method if all the required keys are also public
        attributes or have a single underscore in front.
        
        There are two reasons why this method does not simply replace _fork
        of ParallelExtensionNode (plus removing Node from the
        inheritance list):
        - If a node is not parallelized _fork raises an exception, as do nodes
            which can not fork due to some other reasons. Without this bahavior
            of _fork we would have to check with hasattr first if fork is
            present, adding more complexity at other places (mostly in
            container nodes).
        - This is a safeguard forcing users to think a little instead of
            relying on the inherited (but possibly incompatible)
            default implementation.
        """
        args, varargs, varkw, defaults = inspect.getargspec(self.__init__)
        args.remove("self")
        if defaults:
            non_default_keys = args[:-len(defaults)]
        else:
            non_default_keys = []
        kwargs = dict((key, getattr(self, key))
                      for key in args if hasattr(self, key))
        # look for the key with an underscore in front
        for key in kwargs:
            args.remove(key)
        under_kwargs = dict((key, getattr(self, '_' + key))
                            for key in args if hasattr(self, '_' + key))
        for key in under_kwargs:
            args.remove(key)
        kwargs.update(under_kwargs)
        # check that all the keys without default arguments are covered
        if non_default_keys:
            missing_defaults = set(non_default_keys) & set(args)
            if missing_defaults:
                err = ("could not find attributes for init arguments %s" %
                       str(missing_defaults))
                raise NotForkableParallelException(err)
        # create new instance
        return self.__class__(**kwargs)
    
    @staticmethod
    def _join_covariance(cov, forked_cov):
        """Helper method to join two CovarianceMatrix instances.
        
        cov -- Instance of CovarianceMatrix, to which the forked_cov instance
            is aded in-place.
        """
        cov._cov_mtx += forked_cov._cov_mtx
        cov._avg += forked_cov._avg
        cov._tlen += forked_cov._tlen
        

## MDP parallel node implementations ##

class ParallelPCANode(ParallelExtensionNode, mdp.nodes.PCANode):
    """Parallel version of MDP PCA node."""

    def _fork(self):
        return self._default_fork()

    def _join(self, forked_node):
        """Combine the covariance matrices."""
        if self._cov_mtx._cov_mtx is None:
            self.set_dtype(self._cov_mtx._dtype)
            self._cov_mtx = forked_node._cov_mtx
        else:
            self._join_covariance(self._cov_mtx, forked_node._cov_mtx)


class ParallelSFANode(ParallelExtensionNode, mdp.nodes.SFANode):
    """Parallel version of MDP SFA node."""

    def _fork(self):
        return self._default_fork()

    def _join(self, forked_node):
        """Combine the covariance matrices."""
        if self._cov_mtx._cov_mtx is None:
            self.set_dtype(forked_node._cov_mtx._dtype)
            self._cov_mtx = forked_node._cov_mtx
            self._dcov_mtx = forked_node._dcov_mtx
        else:
            self._join_covariance(self._cov_mtx, forked_node._cov_mtx)
            self._join_covariance(self._dcov_mtx, forked_node._dcov_mtx)


class ParallelFDANode(ParallelExtensionNode, mdp.nodes.FDANode):

    def _fork(self):
        if self.get_current_train_phase() == 1:
            forked_node = self.copy()
            # reset the variables that might contain data from this train phase
            forked_node._S_W = None
            forked_node._allcov = mdp.utils.CovarianceMatrix(dtype=self.dtype)
        else:
            forked_node = self._default_fork()
        return forked_node

    def _join(self, forked_node):
        if self.get_current_train_phase() == 1:
            if forked_node.get_current_train_phase() != 1:
                msg = ("This node is in training phase 1, but the forked node "
                       "is not.")
                raise NotForkableParallelException(msg)
            if self._S_W is None:
                self.set_dtype(forked_node._allcov._dtype)
                self._allcov = forked_node._allcov
                self._S_W = forked_node._S_W
            else:
                self._join_covariance(self._allcov, forked_node._allcov)
                self._S_W += forked_node._S_W
        else:
            for lbl in forked_node.means:
                if lbl in self.means:
                    self.means[lbl] += forked_node.means[lbl]
                    self.tlens[lbl] += forked_node.tlens[lbl]
                else:
                    self.means[lbl] = forked_node.means[lbl]
                    self.tlens[lbl] = forked_node.tlens[lbl]


class ParallelHistogramNode(ParallelExtensionNode, mdp.nodes.HistogramNode):
    """Parallel version of the HistogramNode."""

    def _fork(self):
        return self._default_fork()

    def _join(self, forked_node):
        if (self.data_hist is not None) and (forked_node.data_hist is not None):
            self.data_hist = numx.concatenate([self.data_hist,
                                            forked_node.data_hist])
        elif forked_node.data_hist != None:
            self.data_hist = forked_node.data_hist
