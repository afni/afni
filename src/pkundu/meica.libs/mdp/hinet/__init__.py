"""Hierarchical Networks Package.

This package makes it possible to construct graph-like Node structures,
especially hierarchical networks.

The most important building block is the new Layer node, which works as an
horizontal version of flow. It encapsulates a list of Nodes, which are trained
and executed in parallel.
For example we can take two Nodes with 100 dimensional input to
construct a layer with a 200 dimensional input. The first half of the input
data is automatically fed into the first Node, the second half into the second
Node.

Since one might also want to use Flows (i.e. vertical stacks of Nodes) in a
Layer, a wrapper class for Nodes is provided.
The FlowNode class wraps any Flow into a Node, which can then be used like any
other Node. Together with the Layer this allows you to combine Nodes both
horizontally and vertically. Thereby one can in principle realize
any feed-forward network topology.

For a hierarchical networks one might want to route the different parts of the
data to different Nodes in a Layer in complicated ways. This is done by a
Switchboard that handles all the routing.
Defining the routing manually can be quite tedious, so one can derive subclasses
for special routing situations. One such subclass for 2d image data is provided.
It maps the data according to rectangular overlapping 2d input areas. One can
then feed the output into a Layer and each Node will get the correct input.
"""

from flownode import FlowNode
from layer import Layer, SameInputLayer, CloneLayer
from switchboard import (
    Switchboard, SwitchboardException, MeanInverseSwitchboard,
    ChannelSwitchboard,
    Rectangular2dSwitchboard, Rectangular2dSwitchboardException,
    DoubleRect2dSwitchboard, DoubleRect2dSwitchboardException,
    DoubleRhomb2dSwitchboard, DoubleRhomb2dSwitchboardException
)
from htmlvisitor import (
    HiNetHTMLVisitor, HiNetXHTMLVisitor, NewlineWriteFile, show_flow
)
from switchboard_factory import (
    get_2d_image_switchboard, FactoryExtensionChannelSwitchboard,
    FactoryRectangular2dSwitchboard, FactoryDoubleRect2dSwitchboard,
    FactoryDoubleRhomb2dSwitchboard
)

__all__ = ['FlowNode', 'Layer', 'SameInputLayer', 'CloneLayer',
           'Switchboard', 'SwitchboardException', 'ChannelSwitchboard',
           'Rectangular2dSwitchboard', 'Rectangular2dSwitchboardException',
           'DoubleRect2dSwitchboard', 'DoubleRect2dSwitchboardException',
           'DoubleRhomb2dSwitchboard', 'DoubleRhomb2dSwitchboardException',
           'HiNetHTMLVisitor', 'HiNetXHTMLVisitor', 'NewlineWriteFile',
           'show_flow', 'get_2d_image_switchboard'
           ]

from mdp.utils import fixup_namespace
fixup_namespace(__name__, __all__,
                ('flownode',
                 'layer',
                 'switchboard',
                 'hinet_Visitor',
                 'switchboard_factory',
                 'utils',
                 'fixup_namespace'
                 ))
