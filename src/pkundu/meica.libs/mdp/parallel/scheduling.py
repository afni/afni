"""
This module contains the basic classes for task processing via a scheduler.
"""

import threading
import time
import os
try:
    import multiprocessing
except ImportError:
    # Python version < 2.6, have to use fallbacks
    pass


class ResultContainer(object):
    """Abstract base class for result containers."""

    def add_result(self, result_data, task_index):
        """Store a result in the container."""
        pass

    def get_results(self):
        """Return results and reset container."""
        pass


class ListResultContainer(ResultContainer):
    """Basic result container using simply a list."""

    def __init__(self):
        super(ListResultContainer, self).__init__()
        self._results = []

    def add_result(self, result, task_index):
        """Store a result in the container."""
        self._results.append(result)

    def get_results(self):
        """Return the list of results and reset this container.

        Note that the results are stored in the order that they come in, which
        can be different from the orginal task order.
        """
        results = self._results
        self._results = []
        return results


class OrderedResultContainer(ListResultContainer):
    """Default result container with automatic restoring of the result order.

    In general the order of the incoming results in the scheduler can be
    different from the order of the tasks, since some tasks may finish quicker
    than other tasks. This result container restores the original order.
    """

    def __init__(self):
        super(OrderedResultContainer, self).__init__()

    def add_result(self, result, task_index):
        """Store a result in the container.

        The task index is also stored and later used to reconstruct the
        original task order.
        """
        self._results.append((result, task_index))

    def get_results(self):
        """Sort the results into the original order and return them in list."""
        results = self._results
        self._results = []
        results.sort(key=lambda x: x[1])
        return list(zip(*results))[0]


class TaskCallable(object):
    """Abstract base class for task callables.

    This class encapsulates the task behavior and the related fixed data
    (data which stays constant over multiple tasks).
    """

    def setup_environment(self):
        """This hook method is only called when the callable is first called
        in a different Python process / environment.

        It can be used for modifications in the Python environment that are
        required by this callable.
        """
        pass

    def __call__(self, data):
        """Perform the computation and return the result.

        Override this method with a concrete implementation.
        """
        return data

    # TODO: is 'fork' really a good name?
    #    As an alternative one could have a separate CallableFactory class,
    #    but this would make things more complicated for simple callables
    #    (similar to why iterators implement the iterable interface).
    def fork(self):
        """Return a fork of this callable, e.g. by making a copy.

        This method is always called exactly once before a callable is called,
        so instead of the original callable a fresh fork is called. This
        ensures that the original callable is preserved when caching is used.
        If the callable is not modified by the call then it can simply return
        itself.
        """
        return self


class SqrTestCallable(TaskCallable):
    """Callable for testing."""

    def __call__(self, data):
        """Return the squared data."""
        return data**2


class SleepSqrTestCallable(TaskCallable):
    """Callable for testing."""

    def __call__(self, data):
        """Return the squared data[0] after sleeping for data[1] seconds."""
        time.sleep(data[1])
        return data[0]**2


class MDPVersionCallable(TaskCallable):
    """Callable For testing MDP version.

    Should return a unique comparable object which includes version information
    and installed/used modules.
    """

    def __call__(self, data):
        """Ignore input data and return mdp.info()"""
        import mdp
        return mdp.config.info()


class TaskCallableWrapper(TaskCallable):
    """Wrapper to provide a fork method for simple callables like a function.

    This wrapper is applied internally in Scheduler.
    """

    def __init__(self, task_callable):
        """Store and wrap the callable."""
        self._callable = task_callable

    def __call__(self, data):
        """Call the internal callable with the data and return the result."""
        return self._callable(data)


# helper function
def cpu_count():
    """Return the number of CPU cores."""
    try:
        return multiprocessing.cpu_count()
    # TODO: remove except clause once we support only python >= 2.6
    except NameError:
        ## This code part is taken from parallel python.
        # Linux, Unix and MacOS
        if hasattr(os, "sysconf"):
            if "SC_NPROCESSORS_ONLN" in os.sysconf_names:
                # Linux & Unix
                n_cpus = os.sysconf("SC_NPROCESSORS_ONLN")
                if isinstance(n_cpus, int) and n_cpus > 0:
                    return n_cpus
            else:
                # OSX
                return int(os.popen2("sysctl -n hw.ncpu")[1].read())
        # Windows
        if "NUMBER_OF_PROCESSORS" in os.environ:
            n_cpus = int(os.environ["NUMBER_OF_PROCESSORS"])
            if n_cpus > 0:
                return n_cpus
        # Default
        return 1


class Scheduler(object):
    """Base class and trivial implementation for schedulers.

    New tasks are added with add_task(data, callable).
    get_results then returns the results (and locks if tasks are
    pending).

    In this simple scheduler implementation the tasks are simply executed in the
    add_task method.
    """

    def __init__(self, result_container=None, verbose=False):
        """Initialize the scheduler.

        result_container -- Instance of ResultContainer that is used to store
            the results (default is None, in which case a ListResultContainer
            is used).
        verbose -- If True then status messages will be printed to sys.stdout.
        """
        if result_container is None:
            result_container = OrderedResultContainer()
        self.result_container = result_container
        self.verbose = verbose
        self._n_open_tasks = 0  # number of tasks that are currently running
        # count the number of submitted tasks, also used for the task index
        self._task_counter = 0
        self._lock = threading.Lock()
        self._last_callable = None  # last callable is stored
        # task index of the _last_callable, can be *.5 if updated between tasks
        self._last_callable_index = -1.0

    ## public read only properties ##

    @property
    def task_counter(self):
        """This property counts the number of submitted tasks."""
        return self._task_counter

    @property
    def n_open_tasks(self):
        """This property counts of submitted but unfinished tasks."""
        return self._n_open_tasks

    ## main methods ##

    def add_task(self, data, task_callable=None):
        """Add a task to be executed.

        data -- Data for the task.
        task_callable -- A callable, which is called with the data. If it is
            None (default value) then the last provided callable is used.
            If task_callable is not an instance of TaskCallable then a
            TaskCallableWrapper is used.

        The callable together with the data constitutes the task. This method
        blocks if there are no free recources to store or process the task
        (e.g. if no free worker processes are available).
        """
        self._lock.acquire()
        if task_callable is None:
            if self._last_callable is None:
                raise Exception("No task_callable specified and " +
                                "no previous callable available.")
        self._n_open_tasks += 1
        self._task_counter += 1
        task_index = self.task_counter
        if task_callable is None:
            # use the _last_callable_index in _process_task to
            # decide if a cached callable can be used
            task_callable = self._last_callable
        else:
            if not hasattr(task_callable, "fork"):
                # not a TaskCallable (probably a function), so wrap it
                task_callable = TaskCallableWrapper(task_callable)
            self._last_callable = task_callable
            self._last_callable_index = self.task_counter
        self._process_task(data, task_callable, task_index)

    def set_task_callable(self, task_callable):
        """Set the callable that will be used if no task_callable is given.

        Normally the callables are provided via add_task, in which case there
        is no need for this method.

        task_callable -- Callable that will be used unless a new task_callable
            is given.
        """
        self._lock.acquire()
        self._last_callable = task_callable
        # set _last_callable_index to half value since the callable is newer
        # than the last task, but not newer than the next incoming task
        self._last_callable_index = self.task_counter + 0.5
        self._lock.release()

    def _store_result(self, result, task_index):
        """Store a result in the internal result container.

        result -- Result data
        task_index -- Task index. Can be None if an error occured.

        This function blocks to avoid any problems during result storage.
        """
        self._lock.acquire()
        self.result_container.add_result(result, task_index)
        if self.verbose:
            if task_index is not None:
                print "    finished task no. %d" % task_index
            else:
                print "    task failed"
        self._n_open_tasks -= 1
        self._lock.release()

    def get_results(self):
        """Get the accumulated results from the result container.

        This method blocks if there are open tasks.
        """
        while True:
            self._lock.acquire()
            if self._n_open_tasks == 0:
                results = self.result_container.get_results()
                self._lock.release()
                return results
            else:
                self._lock.release()
                time.sleep(1)

    def shutdown(self):
        """Controlled shutdown of the scheduler.

        This method should always be called when the scheduler is no longer
        needed and before the program shuts down! Otherwise one might get
        error messages.
        """
        self._shutdown()

    ## Context Manager interface ##

    def __enter__(self):
        """Return self."""
        return self

    def __exit__(self, type, value, traceback):
        """Shutdown the scheduler.

        It is important that all the calculations have finished
        when this is called, otherwise the shutdown might fail.
        """
        self.shutdown()

    ## override these methods in custom schedulers ##

    def _process_task(self, data, task_callable, task_index):
        """Process the task and store the result.

        You can override this method for custom schedulers.

        Warning: When this method is entered is has the lock, the lock must be
        released here.

        Warning: Note that fork has not been called yet, so the provided
        task_callable must not be called. Only a forked version can be called.
        """
        # IMPORTANT: always call fork, since it must be called at least once!
        task_callable = task_callable.fork()
        result = task_callable(data)
        # release lock before store_result
        self._lock.release()
        self._store_result(result, task_index)

    def _shutdown(self):
        """Hook method for shutdown to be used in custom schedulers."""
        pass
