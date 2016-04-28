"""
Module for Layers.

Note that additional args and kwargs for train or execute are currently not
supported.
"""

import mdp
from mdp import numx

# TODO: maybe turn self.nodes into a read only property with self._nodes

# TODO: Find a better way to deal with additional args for train/execute?
#    Maybe split them by default, but can be disabled via switch?

class Layer(mdp.Node):
    """Layers are nodes which consist of multiple horizontally parallel nodes.

    The incoming data is split up according to the dimensions of the internal
    nodes. For example if the first node has an input_dim of 50 and the second
    node 100 then the layer will have an input_dim of 150. The first node gets
    x[:,:50], the second one x[:,50:].

    Any additional arguments are forwarded unaltered to each node.
    Warning: This might change in the next release (2.5).

    Since they are nodes themselves layers can be stacked in a flow (e.g. to
    build a layered network). If one would like to use flows instead of nodes
    inside of a layer one can use a FlowNode.
    """

    def __init__(self, nodes, dtype=None):
        """Setup the layer with the given list of nodes.

        The input and output dimensions for the nodes must be already set
        (the output dimensions for simplicity reasons). The training phases for
        the nodes are allowed to differ.

        Keyword arguments:
        nodes -- List of the nodes to be used.
        """
        self.nodes = nodes
        # check nodes properties and get the dtype
        dtype = self._check_props(dtype)
        # calculate the the dimensions
        self.node_input_dims = numx.zeros(len(self.nodes))
        input_dim = 0
        for index, node in enumerate(nodes):
            input_dim += node.input_dim
            self.node_input_dims[index] = node.input_dim
        output_dim = self._get_output_dim_from_nodes()
        super(Layer, self).__init__(input_dim=input_dim,
                                    output_dim=output_dim,
                                    dtype=dtype)

    def _get_output_dim_from_nodes(self):
        """Calculate the output_dim from the nodes and return it.

        If the output_dim of a node is not set the None is returned.
        """
        output_dim = 0
        for node in self.nodes:
            if node.output_dim is not None:
                output_dim += node.output_dim
            else:
                return None
        return output_dim

    def _check_props(self, dtype):
        """Check the compatibility of the properties of the internal nodes.

        Return the found dtype and check the dimensions.

        dtype -- The specified layer dtype.
        """
        dtype_list = []  # the dtypes for all the nodes
        for i, node in enumerate(self.nodes):
            # input_dim for each node must be set
            if node.input_dim is None:
                err = ("input_dim must be set for every node. " +
                       "Node #%d (%s) does not comply." % (i, node))
                raise mdp.NodeException(err)
            if node.dtype is not None:
                dtype_list.append(node.dtype)
        # check that the dtype is None or the same for every node
        nodes_dtype = None
        nodes_dtypes = set(dtype_list)
        nodes_dtypes.discard(None)
        if len(nodes_dtypes) > 1:
            err = ("All nodes must have the same dtype (found: %s)." %
                   nodes_dtypes)
            raise mdp.NodeException(err)
        elif len(nodes_dtypes) == 1:
            nodes_dtype = list(nodes_dtypes)[0]
        # check that the nodes dtype matches the specified dtype
        if nodes_dtype and dtype:
            if not numx.dtype(nodes_dtype) == numx.dtype(dtype):
                err = ("Cannot set dtype to %s: " %
                       numx.dtype(nodes_dtype).name +
                       "an internal node requires %s" % numx.dtype(dtype).name)
                raise mdp.NodeException(err)
        elif nodes_dtype and not dtype:
            dtype = nodes_dtype
        return dtype

    def _set_dtype(self, t):
        for node in self.nodes:
            node.dtype = t
        self._dtype = t

    def _get_supported_dtypes(self):
        # we supported the minimal common dtype set
        types = set(mdp.utils.get_dtypes('All'))
        for node in self.nodes:
            types = types.intersection(node.get_supported_dtypes())
        return list(types)

    def is_trainable(self):
        return any(node.is_trainable() for node in self.nodes)

    def is_invertible(self):
        return all(node.is_invertible() for node in self.nodes)

    def _get_train_seq(self):
        """Return the train sequence.

        The length is set by the node with maximum length.
        """
        max_train_length = 0
        for node in self.nodes:
            node_length = len(node._get_train_seq())
            if node_length > max_train_length:
                max_train_length = node_length
        return ([[self._train, self._stop_training]] * max_train_length)

    def _train(self, x, *args, **kwargs):
        """Perform single training step by training the internal nodes."""
        start_index = 0
        stop_index = 0
        for node in self.nodes:
            start_index = stop_index
            stop_index += node.input_dim
            if node.is_training():
                node.train(x[:, start_index : stop_index], *args, **kwargs)

    def _stop_training(self, *args, **kwargs):
        """Stop training of the internal nodes."""
        for node in self.nodes:
            if node.is_training():
                node.stop_training(*args, **kwargs)
        if self.output_dim is None:
            self.output_dim = self._get_output_dim_from_nodes()

    def _pre_execution_checks(self, x):
        """Make sure that output_dim is set and then perform normal checks."""
        if self.output_dim is None:
            # first make sure that the output_dim is set for all nodes
            in_start = 0
            in_stop = 0
            for node in self.nodes:
                in_start = in_stop
                in_stop += node.input_dim
                node._pre_execution_checks(x[:,in_start:in_stop])
            self.output_dim = self._get_output_dim_from_nodes()
            if self.output_dim is None:
                err = "output_dim must be set at this point for all nodes"
                raise mdp.NodeException(err)
        super(Layer, self)._pre_execution_checks(x)

    def _execute(self, x, *args, **kwargs):
        """Process the data through the internal nodes."""
        in_start = 0
        in_stop = 0
        out_start = 0
        out_stop = 0
        y = None
        for node in self.nodes:
            out_start = out_stop
            out_stop += node.output_dim
            in_start = in_stop
            in_stop += node.input_dim
            if y is None:
                node_y = node.execute(x[:,in_start:in_stop], *args, **kwargs)
                y = numx.zeros([node_y.shape[0], self.output_dim],
                               dtype=node_y.dtype)
                y[:,out_start:out_stop] = node_y
            else:
                y[:,out_start:out_stop] = node.execute(x[:,in_start:in_stop],
                                                        *args, **kwargs)
        return y

    def _inverse(self, x, *args, **kwargs):
        """Combine the inverse of all the internal nodes."""
        in_start = 0
        in_stop = 0
        out_start = 0
        out_stop = 0
        y = None
        for node in self.nodes:
            # compared with execute, input and output are switched
            out_start = out_stop
            out_stop += node.input_dim
            in_start = in_stop
            in_stop += node.output_dim
            if y is None:
                node_y = node.inverse(x[:,in_start:in_stop], *args, **kwargs)
                y = numx.zeros([node_y.shape[0], self.input_dim],
                               dtype=node_y.dtype)
                y[:,out_start:out_stop] = node_y
            else:
                y[:,out_start:out_stop] = node.inverse(x[:,in_start:in_stop],
                                                        *args, **kwargs)
        return y

    ## container methods ##

    def __len__(self):
        return len(self.nodes)

    def __getitem__(self, key):
        return self.nodes.__getitem__(key)

    def __contains__(self, item):
        return self.nodes.__contains__(item)

    def __iter__(self):
        return self.nodes.__iter__()


class CloneLayer(Layer):
    """Layer with a single node instance that is used multiple times.

    The same single node instance is used to build the layer, so
    Clonelayer(node, 3) executes in the same way as Layer([node]*3).
    But Layer([node]*3) would have a problem when closing a training phase,
    so one has to use CloneLayer.

    A CloneLayer can be used for weight sharing in the training phase. It might
    be also useful for reducing the memory footprint use during the execution
    phase (since only a single node instance is needed).
    """

    def __init__(self, node, n_nodes=1, dtype=None):
        """Setup the layer with the given list of nodes.

        Keyword arguments:
        node -- Node to be cloned.
        n_nodes -- Number of repetitions/clones of the given node.
        """
        super(CloneLayer, self).__init__((node,) * n_nodes, dtype=dtype)
        self.node = node  # attribute for convenience

    def _stop_training(self, *args, **kwargs):
        """Stop training of the internal node."""
        if self.node.is_training():
            self.node.stop_training(*args, **kwargs)
        if self.output_dim is None:
            self.output_dim = self._get_output_dim_from_nodes()

class SameInputLayer(Layer):
    """SameInputLayer is a layer were all nodes receive the full input.

    So instead of splitting the input according to node dimensions, all nodes
    receive the complete input data.
    """

    def __init__(self, nodes, dtype=None):
        """Setup the layer with the given list of nodes.

        The input dimensions for the nodes must all be equal, the output
        dimensions can differ (but must be set as well for simplicity reasons).

        Keyword arguments:
        nodes -- List of the nodes to be used.
        """
        self.nodes = nodes
        # check node properties and get the dtype
        dtype = self._check_props(dtype)
        # check that the input dimensions are all the same
        input_dim = self.nodes[0].input_dim
        for node in self.nodes:
            if not node.input_dim == input_dim:
                err = "The nodes have different input dimensions."
                raise mdp.NodeException(err)
        output_dim = self._get_output_dim_from_nodes()
        # intentionally use MRO above Layer, not SameInputLayer
        super(Layer, self).__init__(input_dim=input_dim,
                                    output_dim=output_dim,
                                    dtype=dtype)

    @staticmethod
    def is_invertible():
        return False

    def _train(self, x, *args, **kwargs):
        """Perform single training step by training the internal nodes."""
        for node in self.nodes:
            if node.is_training():
                node.train(x, *args, **kwargs)

    def _pre_execution_checks(self, x):
        """Make sure that output_dim is set and then perform nromal checks."""
        if self.output_dim is None:
            # first make sure that the output_dim is set for all nodes
            for node in self.nodes:
                node._pre_execution_checks(x)
            self.output_dim = self._get_output_dim_from_nodes()
            if self.output_dim is None:
                err = "output_dim must be set at this point for all nodes"
                raise mdp.NodeException(err)
        # intentionally use MRO above Layer, not SameInputLayer
        super(Layer, self)._pre_execution_checks(x)

    def _execute(self, x, *args, **kwargs):
        """Process the data through the internal nodes."""
        out_start = 0
        out_stop = 0
        y = None
        for node in self.nodes:
            out_start = out_stop
            out_stop += node.output_dim
            if y is None:
                node_y = node.execute(x, *args, **kwargs)
                y = numx.zeros([node_y.shape[0], self.output_dim],
                               dtype=node_y.dtype)
                y[:,out_start:out_stop] = node_y
            else:
                y[:,out_start:out_stop] = node.execute(x, *args, **kwargs)
        return y
