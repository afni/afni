"""
Module for the FlowNode class.
"""

import mdp
import warnings as _warnings
import copy as _copy


class FlowNode(mdp.Node):
    """FlowNode wraps a Flow of Nodes into a single Node.

    This is handy if you want to use a flow where a Node is required.
    Additional args and kwargs for train and execute are supported.

    Note that for nodes in the internal flow the intermediate training phases
    will generally be closed, e.g. a CheckpointSaveFunction should not expect
    these training  phases to be left open.

    All the read-only container slots are supported and are forwarded to the
    internal flow.
    """

    def __init__(self, flow, input_dim=None, output_dim=None, dtype=None):
        """Wrap the given flow into this node.

        Pretrained nodes are allowed, but the internal flow should not
        be modified after the FlowNode was created (this will cause problems
        if the training phase structure of the internal nodes changes).

        If the node dimensions and dtype are not specified, they will be
        extracted from the internal nodes (late dimension setting is also
        supported).

        flow can have crash recovery enabled, but there is no special support
        for it.
        """
        self._flow = flow
        # set properties if needed:
        if input_dim is None:
            input_dim = self._flow[0].input_dim
        if output_dim is None:
            output_dim = self._flow[-1].output_dim
        if dtype is None:
            dtype = self._flow[-1].dtype
        # store which nodes are pretrained up to what phase
        self._pretrained_phase = [node.get_current_train_phase()
                                  for node in flow]
        # check if all the nodes are already fully trained
        train_len = 0
        for i_node, node in enumerate(self._flow):
            if node.is_trainable():
                train_len += (len(node._get_train_seq())
                              - self._pretrained_phase[i_node])
        if train_len:
            self._is_trainable = True
        else:
            self._is_trainable = False
        # remaining standard node initialisation
        super(FlowNode, self).__init__(input_dim=input_dim,
                                       output_dim=output_dim, dtype=dtype)

    @property
    def flow(self):
        """Read-only internal flow property.

        In general the internal flow should not be modified (see __init__
        for more details).
        """
        return self._flow

    def _set_input_dim(self, n):
        # try setting the input_dim of the first node
        self._flow[0].input_dim = n
        # let a consistency check run
        self._flow._check_nodes_consistency()
        # if we didn't fail here, go on
        self._input_dim = n

    def _set_output_dim(self, n):
        last_node = self._flow[-1]
        if len(self._flow) == 1:
            self._flow[-1].output_dim = n
        elif last_node.output_dim is None:
            self._fix_nodes_dimensions()
            # check if it worked
            if last_node.output_dim is None:
                if last_node.input_dim is None:
                    err = ("FlowNode can't set the dimension of the last "
                           "node, because its input_dim is undefined ("
                           "which could lead to inconsistent dimensions).")
                    raise mdp.InconsistentDimException(err)
                # now we can safely try to set the dimension
                last_node.output_dim = n
        # the last_node dim is now set 
        if n != last_node.output_dim:
            err = (("FlowNode can't be set to output_dim %d" % n) +
                   " because the last internal node already has " +
                   "output_dim %d." % last_node.output_dim)
            raise mdp.InconsistentDimException(err)
        self._output_dim = n
    
    def _fix_nodes_dimensions(self):
        """Try to fix the dimensions of the internal nodes."""
        if len(self._flow) > 1:
            prev_node = self._flow[0]
            for node in self._flow[1:]:
                if node.input_dim is None:
                    node.input_dim = prev_node.output_dim
                prev_node = node
            self._flow._check_nodes_consistency()
        if self._flow[-1].output_dim is not None:
            # additional checks are performed here
            self.output_dim = self._flow[-1].output_dim

    def _set_dtype(self, t):
        # dtype can not be set for sure in arbitrary flows
        # but here we want to be sure that FlowNode *can*
        # offer a dtype that is consistent
        for node in self._flow:
            node.dtype = t
        self._dtype = t

    def _get_supported_dtypes(self):
        # we support the minimal common dtype set
        types = set(mdp.utils.get_dtypes('All'))
        for node in self._flow:
            types = types.intersection(node.get_supported_dtypes())
        return list(types)

    def is_trainable(self):
        return self._is_trainable

    def is_invertible(self):
        return all(node.is_invertible() for node in self._flow)

    def _get_train_seq(self):
        """Return a training sequence containing all training phases."""
        
        def get_train_function(_i_node, _node):
            # This internal function is needed to channel the data through
            # the nodes in front of the current nodes.
            # using nested scopes here instead of default args, see pep-0227
            def _train(x, *args, **kwargs):
                if i_node > 0:
                    _node.train(self._flow.execute(x, nodenr=_i_node-1),
                                *args, **kwargs)
                else:
                    _node.train(x, *args, **kwargs)
            return _train
        
        train_seq = []
        for i_node, node in enumerate(self._flow):
            if node.is_trainable():
                remaining_len = (len(node._get_train_seq())
                                 - self._pretrained_phase[i_node])
                train_seq += ([(get_train_function(i_node, node),
                                node.stop_training)] * remaining_len)

        # try fix the dimension of the internal nodes and the FlowNode
        # after the last node has been trained
        def _get_stop_training_wrapper(self, node, func):
            def _stop_training_wrapper(*args, **kwargs):
                func(*args, **kwargs)
                self._fix_nodes_dimensions()
            return _stop_training_wrapper
        
        if train_seq:
            train_seq[-1] = (train_seq[-1][0],
                             _get_stop_training_wrapper(self, self._flow[-1],
                                                        train_seq[-1][1]))
        return train_seq

    def _execute(self, x, *args, **kwargs):
        return self._flow.execute(x, *args, **kwargs)

    def _inverse(self, x):
        return self._flow.inverse(x)

    def copy(self, protocol=None):
        """Return a copy of this node.

        The copy call is delegated to the internal node, which allows the use
        of custom copy methods for special nodes.

        The protocol parameter should not be used.
        """
        if protocol is not None:
            _warnings.warn("protocol parameter to copy() is ignored",
                           mdp.MDPDeprecationWarning, stacklevel=2)
        # Warning: If we create a new FlowNode with the copied internal
        #    nodes then it will differ from the original one if some nodes
        #    were trained in the meantime. Especially _get_train_seq would
        #    return a shorter list in that case, possibly breaking stuff
        #    outside of this FlowNode (e.g. if it is enclosed by another
        #    FlowNode the _train_phase of this node will no longer fit the
        #    result of _get_train_seq).
        #
        # copy the nodes by delegation
        old_nodes = self._flow[:]
        new_nodes = [node.copy() for node in old_nodes]
        # now copy the rest of this flownode via deepcopy
        self._flow.flow = None
        new_flownode = _copy.deepcopy(self)
        new_flownode._flow.flow = new_nodes
        self._flow.flow = old_nodes
        return new_flownode

    ## container methods ##

    def __len__(self):
        return len(self._flow)

    def __getitem__(self, key):
        return self._flow.__getitem__(key)

    def __contains__(self, item):
        return self._flow.__contains__(item)

    def __iter__(self):
        return self._flow.__iter__()
