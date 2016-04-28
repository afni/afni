# inspired by some code by Nathan Denny (1999)
# see http://www.ece.arizona.edu/~denny/python_nest/graph_lib_1.0.1.html

try:
    # use reduce against BDFL's will even on python > 2.6
    from functools import reduce
except ImportError:
    pass

class GraphException(Exception):
    """Base class for exception in the graph package."""
    pass

class GraphTopologicalException(GraphException):
    """Exception thrown during a topological sort if the graph is cyclical."""
    pass



def is_sequence(x):
    return isinstance(x, (list, tuple))

def recursive_map(func, seq):
    """Apply a function recursively on a sequence and all subsequences."""
    def _func(x):
        if is_sequence(x):
            return recursive_map(func, x)
        else:
            return func(x)
    return map(_func, seq)

def recursive_reduce(func, seq, *argv):
    """Apply reduce(func, seq) recursively to a sequence and all its
    subsequences."""
    def _func(x, y):
        if is_sequence(y):
            return func(x, recursive_reduce(func, y))
        else:
            return func(x, y)
    return reduce(_func, seq, *argv)



class GraphNode(object):
    """Represent a graph node and all information attached to it."""

    def __init__(self, data=None):
        self.data = data
        # edges in
        self.ein = []
        # edges out
        self.eout = []

    def add_edge_in(self, edge):
        self.ein.append(edge)

    def add_edge_out(self, edge):
        self.eout.append(edge)

    def remove_edge_in(self, edge):
        self.ein.remove(edge)

    def remove_edge_out(self, edge):
        self.eout.remove(edge)

    def get_edges_in(self, from_ = None):
        """Return a copy of the list of the entering edges. If from_
        is specified, return only the nodes coming from that node."""
        inedges = self.ein[:]
        if from_:
            inedges = [edge for edge in inedges if edge.head == from_]
        return inedges

    def get_edges_out(self, to_ = None):
        """Return a copy of the list of the outgoing edges. If to_
        is specified, return only the nodes going to that node."""
        outedges = self.eout[:]
        if to_:
            outedges = [edge for edge in outedges if edge.tail == to_]
        return outedges

    def get_edges(self, neighbor = None):
        """Return a copy of all edges. If neighbor is specified, return
        only the edges connected to that node."""
        return ( self.get_edges_in(from_=neighbor) +
                 self.get_edges_out(to_=neighbor) )

    def in_degree(self):
        """Return the number of entering edges."""
        return len(self.ein)

    def out_degree(self):
        """Return the number of outgoing edges."""
        return len(self.eout)

    def degree(self):
        """Return the number of edges."""
        return self.in_degree()+self.out_degree()

    def in_neighbors(self):
        """Return the neighbors down in-edges (i.e. the parents nodes)."""
        return map(lambda x: x.get_head(), self.ein)

    def out_neighbors(self):
        """Return the neighbors down in-edges (i.e. the parents nodes)."""
        return map(lambda x: x.get_tail(), self.eout)

    def neighbors(self):
        return self.in_neighbors() + self.out_neighbors()

class GraphEdge(object):
    """Represent a graph edge and all information attached to it."""

    def __init__(self, head, tail, data=None):
        # head node
        self.head = head
        # neighbors out
        self.tail = tail
        # arbitrary data slot
        self.data = data

    def get_ends(self):
        """Return the tuple (head_id, tail_id)."""
        return (self.head, self.tail)

    def get_tail(self):
        return self.tail

    def get_head(self):
        return self.head

class Graph(object):
    """Represent a directed graph."""

    def __init__(self):
        # list of nodes
        self.nodes = []
        # list of edges
        self.edges = []

    # node functions
    def add_node(self, data=None):
        node = GraphNode(data=data)
        self.nodes.append(node)
        return node

    def remove_node(self, node):
        # the node is not in this graph
        if node not in self.nodes:
            errstr = 'This node is not part of the graph (%s)' % node
            raise GraphException(errstr)

        # remove all edges containing this node
        for edge in node.get_edges():
            self.remove_edge(edge)
        # remove the node
        self.nodes.remove(node)

    # edge functions
    def add_edge(self, head, tail, data=None):
        """Add an edge going from head to tail.
        head : head node
        tail : tail node
        """
        # create edge
        edge = GraphEdge(head, tail, data=data)
        # add edge to head and tail node
        head.add_edge_out(edge)
        tail.add_edge_in(edge)
        # add to the edges dictionary
        self.edges.append(edge)
        return edge

    def remove_edge(self, edge):
        head, tail = edge.get_ends()
        # remove from head
        head.remove_edge_out(edge)
        # remove from tail
        tail.remove_edge_in(edge)
        # remove the edge
        self.edges.remove(edge)

    ### populate functions

    def add_nodes(self, data):
        """Add many nodes at once.

        data -- number of nodes to add or sequence of data values, one for
                each new node"""
        if not is_sequence(data):
            data = [None]*data
        return map(self.add_node, data)

    def add_tree(self, tree):
        """Add a tree to the graph.

        The tree is specified with a nested list of tuple, in a LISP-like
        notation. The values specified in the list become the values of
        the single nodes.

        Return an equivalent nested list with the nodes instead of the values.

        Example:
        >>> a=b=c=d=e=None
        >>> g.add_tree( (a, b, (c, d ,e)) )
        corresponds to this tree structure, with all node values set to None:

                a
               / \
              b   c
                 / \
                d   e
        """

        def _add_edge(root, son):
            self.add_edge(root, son)
            return root

        nodes = recursive_map(self.add_node, tree)
        recursive_reduce(_add_edge, nodes)
        return nodes

    def add_full_connectivity(self, from_nodes, to_nodes):
        """Add full connectivity from a group of nodes to another one.
        Return a list of lists of edges, one for each node in 'from_nodes'.

        Example: create a two-layer graph with full connectivity.
        >>> g = Graph()
        >>> layer1 = g.add_nodes(10)
        >>> layer2 = g.add_nodes(5)
        >>> g.add_full_connectivity(layer1, layer2)
        """
        edges = []
        for from_ in from_nodes:
            edges.append(map(lambda x: self.add_edge(from_, x), to_nodes))
        return edges

    ###### graph algorithms

    def topological_sort(self):
        """Perform a topological sort of the nodes. If the graph has a cycle,
        throw a GraphTopologicalException with the list of successfully
        ordered nodes."""
        # topologically sorted list of the nodes (result)
        topological_list = []
        # queue (fifo list) of the nodes with in_degree 0
        topological_queue = []
        # {node: in_degree} for the remaining nodes (those with in_degree>0)
        remaining_indegree = {}

        # init queues and lists
        for node in self.nodes:
            indegree = node.in_degree()
            if indegree == 0:
                topological_queue.append(node)
            else:
                remaining_indegree[node] = indegree

        # remove nodes with in_degree 0 and decrease the in_degree of their sons
        while len(topological_queue):
            # remove the first node with degree 0
            node = topological_queue.pop(0)
            topological_list.append(node)
            # decrease the in_degree of the sons
            for son in node.out_neighbors():
                remaining_indegree[son] -= 1
                if remaining_indegree[son] == 0:
                    topological_queue.append(son)

        # if not all nodes were covered, the graph must have a cycle
        # raise a GraphTopographicalException
        if len(topological_list)!=len(self.nodes):
            raise GraphTopologicalException(topological_list)

        return topological_list

    ### Depth-First sort

    def _dfs(self, neighbors_fct, root, visit_fct=None):
        # core depth-first sort function
        # changing the neighbors function to return the sons of a node,
        # its parents, or both one gets normal dfs, reverse dfs, or
        # dfs on the equivalent undirected graph, respectively

        # result list containing the nodes in Depth-First order
        dfs_list = []
        # keep track of all already visited nodes
        visited_nodes = { root: None }

        # stack (lifo) list
        dfs_stack = []
        dfs_stack.append(root)

        while len(dfs_stack):
            # consider the next node on the stack
            node = dfs_stack.pop()
            dfs_list.append(node)
            # visit the node
            if visit_fct != None:
                visit_fct(node)
            # add all sons to the stack (if not already visited)
            for son in neighbors_fct(node):
                if son not in visited_nodes:
                    visited_nodes[son] = None
                    dfs_stack.append(son)

        return dfs_list

    def dfs(self, root, visit_fct=None):
        """Return a list of nodes in some Depth First order starting from
        a root node. If defined, visit_fct is applied on each visited node.

        The returned list does not have to contain all nodes in the
        graph, but only the ones reachable from the root.
        """

        neighbors_fct = lambda node: node.out_neighbors()
        return self._dfs(neighbors_fct, root, visit_fct=visit_fct)

    def undirected_dfs(self, root, visit_fct=None):
        """Perform Depth First sort.

        This function is identical to dfs, but the sort is performed on
        the equivalent undirected version of the graph."""
        neighbors_fct = lambda node: node.neighbors()
        return self._dfs(neighbors_fct, root, visit_fct=visit_fct)

    ### Connected components

    def connected_components(self):
        """Return a list of lists containing the nodes of all connected
        components of the graph."""

        visited = {}
        def visit_fct(node, visited=visited):
            visited[node] = None

        components = []
        nodes = self.nodes
        for node in nodes:
            if node in visited:
                continue
            components.append(self.undirected_dfs(node, visit_fct))
        return components

    def is_weakly_connected(self):
        """Return True if the graph is weakly connected."""
        return len(self.undirected_dfs(self.nodes[0]))==len(self.nodes)

    ### Breadth-First Sort
    # BFS and DFS could be generalized to one function. I leave them
    # distinct for clarity.

    def _bfs(self, neighbors_fct, root, visit_fct=None):
        # core breadth-first sort function
        # changing the neighbors function to return the sons of a node,
        # its parents, or both one gets normal bfs, reverse bfs, or
        # bfs on the equivalent undirected graph, respectively

        # result list containing the nodes in Breadth-First order
        bfs_list = []
        # keep track of all already visited nodes
        visited_nodes = { root: None }

        # queue (fifo) list
        bfs_queue = []
        bfs_queue.append(root)

        while len(bfs_queue):
            # consider the next node in the queue
            node = bfs_queue.pop(0)
            bfs_list.append(node)
            # visit the node
            if visit_fct != None:
                visit_fct(node)
            # add all sons to the queue (if not already visited)
            for son in neighbors_fct(node):
                if son not in visited_nodes:
                    visited_nodes[son] = None
                    bfs_queue.append(son)

        return bfs_list

    def bfs(self, root, visit_fct=None):
        """Return a list of nodes in some Breadth First order starting from
        a root node. If defined, visit_fct is applied on each visited node.

        Note the returned list does not have to contain all nodes in the
        graph, but only the ones reachable from the root."""

        neighbors_fct = lambda node: node.out_neighbors()
        return self._bfs(neighbors_fct, root, visit_fct=visit_fct)

    def undirected_bfs(self, root, visit_fct=None):
        """Perform Breadth First sort.

        This function is identical to bfs, but the sort is performed on
        the equivalent undirected version of the graph."""

        neighbors_fct = lambda node: node.neighbors()
        return self._bfs(neighbors_fct, root, visit_fct=visit_fct)
