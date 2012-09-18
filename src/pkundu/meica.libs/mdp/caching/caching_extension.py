"""MDP extension to cache the execution phase of nodes.

This extension is based on the **joblib** library by Gael Varoquaux,
available at http://packages.python.org/joblib/. At the moment, the
extension is based on joblib v. 0.4.6.
"""
__docformat__ = "restructuredtext en"

import joblib

from ..utils import TemporaryDirectory
from ..extension import ExtensionNode, activate_extension, deactivate_extension
from ..signal_node import Node

# -- global attributes for this extension

_cachedir = None
# If a temporary directory is used, a reference to the
# TemporaryDirectory object is kept here. The directory will be
# deleted when this object is destroyed, so either when this module is
# destroyed, or when a new directory is set and it is temporary
# directory again.
_cacheobj = None
# instance of joblib cache object (set with set_cachedir)
_memory = None

# True is the cache is active for *all* classes
_cache_active_global = True
_cached_classes = []
_cached_instances = []
_cached_methods = {}

def set_cachedir(cachedir=None, verbose=0):
    """Set root directory for the joblib cache.

    :Parameters:
     cachedir
         the cache directory name; if ``None``, a temporary directory
         is created using `TemporaryDirectory`
     verbose
         an integer number, controls the verbosity of the cache
         (default is 0, i.e., not verbose)
    """

    global _cachedir
    global _cacheobj
    global _cached_methods
    global _memory

    if cachedir is None:
        _cacheobj = TemporaryDirectory(prefix='mdp-joblib-cache.')
        cachedir = _cacheobj.name

    # only reset if the directory changes
    if cachedir != _cachedir:
        _cachedir = cachedir
        _memory = joblib.Memory(cachedir, verbose=verbose)
        # reset cached methods
        _cached_methods.clear()

# initialize cache with temporary directory
#set_cachedir()

class CacheExecuteExtensionNode(ExtensionNode, Node):
    """MDP extension for caching execution results.

    The return value of the 'execute' methods are cached if:
    1) the extension is activated in global mode
    2) the Node subclass is registered to be cached
    or
    3) the instance is registered to be cached

    *Warning: this extension might break the algorithms if nodes rely
    on side effects.*

    See `activate_caching`, `deactivate_caching`, and the `cache` context
    manager to learn about how to activate the caching mechanism and its
    options.
    """

    extension_name = 'cache_execute'

    def is_cached(self):
        """Return True if the node is cached."""
        global _cache_active_global
        global _cached_classes
        global _cached_instances
        return (_cache_active_global
                or self.__class__ in _cached_classes
                or self in _cached_instances)

    def set_instance_cache(self, active=True):
        """Add or remove this instance from caching.

        The global caching and class caching options still have priority over
        the instance caching option.
        """
        # add to global dictionary
        global _cached_instances
        if active:
            _cached_instances.append(self)
        else:
            if self in _cached_instances:
                _cached_instances.remove(self)

    def execute(self, x, *args, **kwargs):
        global _cached_methods

        # cache is not active for globally, for this class or instance:
        # call original execute method
        if not self.is_cached():
            return self._non_extension_execute(x, *args, **kwargs)

        if self not in _cached_methods:
            global _memory
            _cached_methods[self] = _memory.cache(
                self._non_extension_execute.im_func)
            # execute pre-execution checks once so that all automatic
            # settings of things like dtype and input_dim are done, and
            # caching begins from first execution, not the second
            self._pre_execution_checks(x)

        return _cached_methods[self](self, x, *args, **kwargs)


# ------- helper functions and context manager

# TODO: check that classes and instances are Nodes

def activate_caching(cachedir=None,
                     cache_classes=None, cache_instances=None,
                     verbose=0):
    """Activate caching extension.

    By default, the cache is activated globally (i.e., for all instances
    of Node). If cache_classes or cache instances are specified, the cache
    is activated only for those classes and instances.

    :Parameters:
     cachedir
      The root of the joblib cache, or a temporary directory if None
     cache_classes
      A list of Node subclasses for which caching is activated.
      Default value: None
     cache_classes
      A list of Node instances for which caching is activated.
      Default value: None
    """
    global _cache_active_global
    global _cached_classes
    global _cached_instances

    set_cachedir(cachedir=cachedir, verbose=verbose)
    _cache_active_global = (cache_classes is None and cache_instances is None)

    # active cache for specific classes and instances
    if cache_classes is not None:
        _cached_classes = list(cache_classes)
    if cache_instances is not None:
        _cached_instances = list(cache_instances)

    activate_extension('cache_execute')

def deactivate_caching(cachedir=None):
    """De-activate caching extension."""
    deactivate_extension('cache_execute')

    # reset global variables
    global _cache_active_global
    global _cached_classes
    global _cached_instances
    global _cached_methods
    _cache_active_global = True
    _cached_classes = []
    _cached_instances = []
    _cached_methods = {}

class cache(object):
    """Context manager for the 'cache_execute' extension.

    This allows using the caching extension using a 'with'
    statement, as in:

    >>> with mdp.caching.cache(CACHEDIR):                    # doctest: +SKIP
    ...     # 'node' is executed caching the results in CACHEDIR
    ...     node.execute(x)

    If the argument to the context manager is not specified, caching is
    done in a temporary directory.
    """

    def __init__(self, cachedir=None, cache_classes=None, cache_instances=None,
                 verbose=0):
        """Activate caching extension.

        By default, the cache is activated globally (i.e., for all instances
        of Node). If cache_classes or cache instances are specified, the cache
        is activated only for those classes and instances.

        :Parameters:
         cachedir
          The root of the joblib cache, or a temporary directory if None
         cache_classes
          A list of Node subclasses for which caching is activated.
          Default value: None
         cache_classes
          A list of Node instances for which caching is activated.
          Default value: None
        """
        self.cachedir = cachedir
        self.cache_classes = cache_classes
        self.cache_instances = cache_instances
        self.verbose = verbose

    def __enter__(self):
        activate_caching(self.cachedir, self.cache_classes,
                         self.cache_instances, self.verbose)

    def __exit__(self, type, value, traceback):
        deactivate_caching()
