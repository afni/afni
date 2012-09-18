"""
This is the MDP package for parallel processing.

It is designed to work with nodes for which a large part of the
computation is embaressingly parallel (like in
:class:`~mdp.nodes.PCANode`). The hinet package is also fully
supported, i.e., there are parallel versions of all hinet nodes.

This package consists of two decoupled parts. The first part consists
of parallel versions of the familiar MDP structures (nodes and
flows). At the top there is the :class:`~ParallelFlow`, which
generates tasks that are processed in parallel (this can be done
automatically in the train or execute methods).

The second part consists of the schedulers. They take tasks and
process them in a more or less parallel way (e.g. in multiple
processes). So they are designed to deal with the more technical
aspects of the parallelization, but do not have to know anything about
flows or nodes.
"""


from scheduling import (
    ResultContainer, ListResultContainer, OrderedResultContainer, TaskCallable,
    SqrTestCallable, SleepSqrTestCallable, TaskCallableWrapper, Scheduler,
    cpu_count, MDPVersionCallable
)
from process_schedule import ProcessScheduler
from thread_schedule import ThreadScheduler
from parallelnodes import (
    ParallelExtensionNode, NotForkableParallelException, JoinParallelException,
    ParallelPCANode, ParallelSFANode, ParallelFDANode, ParallelHistogramNode
)
from parallelclassifiers import (
    ParallelGaussianClassifier, ParallelNearestMeanClassifier,
    ParallelKNNClassifier
)
from parallelflows import (
    _purge_flownode, FlowTaskCallable, FlowTrainCallable, FlowExecuteCallable,
    TrainResultContainer, ExecuteResultContainer,
    ParallelFlowException, NoTaskException,
    ParallelFlow, ParallelCheckpointFlow
)
from parallelhinet import (
    ParallelFlowNode, ParallelLayer, ParallelCloneLayer
)

from mdp import config
from mdp.utils import fixup_namespace

if config.has_parallel_python:
    import pp_support

# Note: the modules with the actual extension node classes are still available

__all__ = [
    "ResultContainer", "ListResultContainer",
    "OrderedResultContainer", "TaskCallable", "SqrTestCallable",
    "SleepSqrTestCallable", "TaskCallableWrapper", "Scheduler",
    "ProcessScheduler", "ThreadScheduler",
    "ParallelExtensionNode", "JoinParallelException",
    "NotForkableParallelException",
    "ParallelSFANode", "ParallelSFANode", "ParallelFDANode",
    "ParallelHistogramNode",
    "FlowTaskCallable", "FlowTrainCallable", "FlowExecuteCallable",
    "ExecuteResultContainer", "TrainResultContainer", "ParallelFlowException",
    "NoTaskException",
    "ParallelFlow", "ParallelCheckpointFlow",
    "ParallelFlowNode", "ParallelLayer", "ParallelCloneLayer"]

import sys as _sys
fixup_namespace(__name__, __all__,
                ('scheduling',
                 'process_schedule',
                 'thread_schedule',
                 'parallelnodes',
                 'parallelflows',
                 'parallelhinet',
                 'parallelclassifiers',
                 'config',
                 'fixup_namespace'
                 ))
