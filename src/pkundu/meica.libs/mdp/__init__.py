# Modular toolkit for Data Processing (MDP)
"""\
**The Modular toolkit for Data Processing (MDP)** package is a library
of widely used data processing algorithms, and the possibility to
combine them together to form pipelines for building more complex
data processing software.

MDP has been designed to be used as-is and as a framework for
scientific data processing development.

From the user's perspective, MDP consists of a collection of *units*,
which process data. For example, these include algorithms for
supervised and unsupervised learning, principal and independent
components analysis and classification.

These units can be chained into data processing flows, to create
pipelines as well as more complex feed-forward network
architectures. Given a set of input data, MDP takes care of training
and executing all nodes in the network in the correct order and
passing intermediate data between the nodes. This allows the user to
specify complex algorithms as a series of simpler data processing
steps.

The number of available algorithms is steadily increasing and includes
signal processing methods (Principal Component Analysis, Independent
Component Analysis, Slow Feature Analysis), manifold learning methods
([Hessian] Locally Linear Embedding), several classifiers,
probabilistic methods (Factor Analysis, RBM), data pre-processing
methods, and many others.

Particular care has been taken to make computations efficient in terms
of speed and memory. To reduce the memory footprint, it is possible to
perform learning using batches of data. For large data-sets, it is
also possible to specify that MDP should use single precision floating
point numbers rather than double precision ones. Finally, calculations
can be parallelised using the ``parallel`` subpackage, which offers a
parallel implementation of the basic nodes and flows.

From the developer's perspective, MDP is a framework that makes the
implementation of new supervised and unsupervised learning algorithms
easy and straightforward. The basic class, ``Node``, takes care of tedious
tasks like numerical type and dimensionality checking, leaving the
developer free to concentrate on the implementation of the learning
and execution phases. Because of the common interface, the node then
automatically integrates with the rest of the library and can be used
in a network together with other nodes.

A node can have multiple training phases and even an undetermined
number of phases. Multiple training phases mean that the training data
is presented multiple times to the same node. This allows the
implementation of algorithms that need to collect some statistics on
the whole input before proceeding with the actual training, and others
that need to iterate over a training phase until a convergence
criterion is satisfied. It is possible to train each phase using
chunks of input data if the chunks are given as an iterable. Moreover,
crash recovery can be optionally enabled, which will save the state of
the flow in case of a failure for later inspection.

MDP is distributed under the open source BSD license. It has been
written in the context of theoretical research in neuroscience, but it
has been designed to be helpful in any context where trainable data
processing algorithms are used. Its simplicity on the user's side, the
variety of readily available algorithms, and the reusability of the
implemented nodes also make it a useful educational tool.

http://mdp-toolkit.sourceforge.net
"""
__docformat__ = "restructuredtext en"

# The descriptions strings below are parsed with a regexp in setup.py.
# Don't do anything fancy, keep strings triple quoted and verify that
# the get_*_description functions continue to work.

# __short_description__ must be one line, 200 characters maximum.
# C.f. http://docs.python.org/distutils/setupscript.html?highlight=description#meta-data
__short_description__ = """\
MDP is a Python library for building complex data processing software \
by combining widely used machine learning algorithms into pipelines \
and networks."""

__medium_description__ ="""\
**Modular toolkit for Data Processing (MDP)** is a Python data processing framework.

From the user's perspective, MDP is a collection of supervised and unsupervised
learning algorithms and other data processing units that can be combined into
data processing sequences and more complex feed-forward network architectures.

From the scientific developer's perspective, MDP is a modular framework, which
can easily be expanded. The implementation of new algorithms is easy and
intuitive. The new implemented units are then automatically integrated with
the rest of the library.

The base of available algorithms is steadily increasing and includes
signal processing methods (Principal Component Analysis,
Independent Component Analysis, Slow Feature Analysis),
manifold learning methods ([Hessian] Locally Linear Embedding),
several classifiers, probabilistic methods (Factor Analysis, RBM),
data pre-processing methods, and many others.
"""

class MDPException(Exception):
    """Base class for exceptions in MDP."""
    pass

class MDPWarning(UserWarning):
    """Base class for warnings in MDP."""
    pass

class MDPDeprecationWarning(DeprecationWarning, MDPWarning):
    """Warn about deprecated MDP API."""
    pass


import configuration

__version__ = '3.3'
__revision__ = configuration.get_git_revision()
__authors__ = 'MDP Developers'
__copyright__ = '(c) 2003-2011 mdp-toolkit-devel@lists.sourceforge.net'
__license__ = 'BSD License, see COPYRIGHT'
__contact__ = 'mdp-toolkit-users@lists.sourceforge.net'
__homepage__ = 'http://mdp-toolkit.sourceforge.net'

configuration.set_configuration()

config = configuration.config
(numx_description, numx, numx_linalg, numx_fft,
 numx_rand, numx_version) = configuration.get_numx()

# import the utils module (used by other modules)
import utils
# set symeig
utils.symeig = configuration.get_symeig(numx_linalg)

# import exceptions from nodes and flows
from signal_node import (NodeException, InconsistentDimException,
                         TrainingException,
                         TrainingFinishedException, IsNotTrainableException,
                         IsNotInvertibleException)
from linear_flows import CrashRecoveryException, FlowException, FlowExceptionCR

# import base nodes and flow classes
from signal_node import (NodeMetaclass, Node, PreserveDimNode,
                         Cumulator, VariadicCumulator)

from linear_flows import (Flow, CheckpointFlow,
                          CheckpointFunction, CheckpointSaveFunction)

# import helper functions:
from helper_funcs import pca, fastica

# import extension mechanism
from extension import (ExtensionException, extension_method,
                       ExtensionNodeMetaclass,
                       ExtensionNode, get_extensions,
                       get_active_extensions, with_extension,
                       activate_extension, deactivate_extension,
                       activate_extensions, deactivate_extensions,
                       extension)

# import classifier node
from classifier_node import (ClassifierNode, ClassifierCumulator)

# import our modules
import nodes
import hinet
import parallel
from test import test

# explicitly set __all__, mainly needed for epydoc
__all__ = ['config',
           'CheckpointFlow',
           'CheckpointFunction',
           'CheckpointSaveFunction',
           'ClassifierCumulator',
           'ClassifierNode',
           'CrashRecoveryException',
           'Cumulator',
           'ExtensionNode',
           'ExtensionNodeMetaclass',
           'Flow',
           'FlowException',
           'FlowExceptionCR',
           'IsNotInvertibleException',
           'IsNotTrainableException',
           'MDPException',
           'MDPWarning',
           'Node',
           'NodeException',
           'TrainingException',
           'TrainingFinishedException',
           'VariadicCumulator',
           'activate_extension',
           'activate_extensions',
           'deactivate_extension',
           'deactivate_extensions',
           'extension',
           'extension_method',
           'get_extensions',
           'graph',
           'hinet',
           'nodes',
           'parallel',
           'pca',
           'fastica',
           'utils',
           'with_extension',
           ]

if config.has_joblib:
    import caching
    __all__ += ['caching']

utils.fixup_namespace(__name__, __all__,
                      ('signal_node',
                       'linear_flows',
                       'helper_funcs',
                       'classifier_node',
                       'configuration',
                       'repo_revision',
                       'extension',
                       ),('extension',
                          'configuration'))
