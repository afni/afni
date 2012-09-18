"""
Process based scheduler for distribution across multiple CPU cores.
"""

# TODO: use a queue instead of sleep?
#    http://docs.python.org/library/queue.html

# TODO: use shared memory for data numpy arrays, but this also requires the
#    use of multiprocessing since the ctype objects can't be pickled

# TODO: only return result when get_results is called,
#    this sends a special request to the processes to send their data,
#    we would have to add support for this to the callable,
#    might get too complicated

# TODO: leverage process forks on unix systems,
#    might be very efficient due to copy-on-write, see
#    http://gael-varoquaux.info/blog/?p=119
#    http://www.ibm.com/developerworks/aix/library/au-multiprocessing/

import sys
import os
import cPickle as pickle
import threading
import subprocess
import time
import traceback
import warnings

if __name__ == "__main__":
    # try to make sure that mdp can be imported by adding it to sys.path
    mdp_path = os.path.realpath(__file__)
    mdp_index = mdp_path.rfind("mdp")
    if mdp_index:
        mdp_path = mdp_path[:mdp_index-1]
        # mdp path goes after sys.path
        sys.path.append(mdp_path)
    # shut off warnings of any kinds
    warnings.filterwarnings("ignore", ".*")

import mdp
from mdp.parallel import Scheduler, cpu_count

SLEEP_TIME = 0.1  # time spend sleeping when waiting for a free process


class ProcessScheduler(Scheduler):
    """Scheduler that distributes the task to multiple processes.

    The subprocess module is used to start the requested number of processes.
    The execution of each task is internally managed by dedicated thread.

    This scheduler should work on all platforms (at least on Linux,
    Windows XP and Vista).
    """

    def __init__(self, result_container=None, verbose=False, n_processes=1,
                 source_paths=None, python_executable=None,
                 cache_callable=True):
        """Initialize the scheduler and start the slave processes.

        result_container -- ResultContainer used to store the results.
        verbose -- Set to True to get progress reports from the scheduler
            (default value is False).
        n_processes -- Number of processes used in parallel. If None (default)
            then the number of detected CPU cores is used.
        source_paths -- List of paths that are added to sys.path in
            the processes to make the task unpickling work. A single path
            instead of a list is also accepted.
            If None (default value) then source_paths is set to sys.path.
            To prevent this you can specify an empty list.
        python_executable -- Python executable that is used for the processes.
            The default value is None, in which case sys.executable will be
            used.
        cache_callable -- Cache the task objects in the processes (default
            is True). Disabling caching can reduce the memory usage, but will
            generally be less efficient since the task_callable has to be
            pickled each time.
        """
        super(ProcessScheduler, self).__init__(
                                        result_container=result_container,
                                        verbose=verbose)
        if n_processes:
            self._n_processes = n_processes
        else:
            self._n_processes = cpu_count()
        self._cache_callable = cache_callable
        if python_executable is None:
            python_executable = sys.executable
        # get the location of this module to start the processes
        module_path = os.path.dirname(mdp.__file__)
        module_file = os.path.join(module_path, "parallel",
                                   "process_schedule.py")
        # Note: -u argument is important on Windows to set stdout to binary
        #    mode. Otherwise you might get a strange error message for
        #    copy_reg.
        process_args = [python_executable, "-u", module_file]
        process_args.append(str(self._cache_callable))
        if isinstance(source_paths, str):
            source_paths = [source_paths]
        if source_paths is None:
            source_paths = sys.path
        process_args += source_paths
        # list of processes not in use, start the processes now
        self._free_processes = [subprocess.Popen(args=process_args,
                                                 stdout=subprocess.PIPE,
                                                 stdin=subprocess.PIPE)
                                for _ in range(self._n_processes)]
        # tag each process with its cached callable task_index,
        # this is compared with _last_callable_index to check if the cached
        # task_callable is still up to date
        for process in self._free_processes:
            process._callable_index = -1
        if self.verbose:
            print ("scheduler initialized with %d processes" %
                   self._n_processes)

    def _shutdown(self):
        """Shut down the slave processes.

        If a process is still running a task then an exception is raised.
        """
        self._lock.acquire()
        if len(self._free_processes) < self._n_processes:
            raise Exception("some slave process is still working")
        for process in self._free_processes:
            pickle.dump("EXIT", process.stdin)
            process.stdin.flush()
        self._lock.release()
        if self.verbose:
            print "scheduler shutdown"

    def _process_task(self, data, task_callable, task_index):
        """Add a task, if possible without blocking.

        It blocks when the system is not able to start a new thread
        or when the processes are all in use.
        """
        task_started = False
        while not task_started:
            if not len(self._free_processes):
                # release lock for other threads and wait
                self._lock.release()
                time.sleep(SLEEP_TIME)
                self._lock.acquire()
            else:
                try:
                    process = self._free_processes.pop()
                    self._lock.release()
                    thread = threading.Thread(target=self._task_thread,
                                              args=(process, data,
                                                    task_callable, task_index))
                    thread.start()
                    task_started = True
                except thread.error:
                    if self.verbose:
                        print ("unable to create new task thread,"
                               " waiting 2 seconds...")
                    time.sleep(2)

    def _task_thread(self, process, data, task_callable, task_index):
        """Thread function which cares for a single task.

        The task is pushed to the process via stdin, then we wait for the
        result on stdout, pass the result to the result container, free
        the process and exit.
        """
        try:
            if self._cache_callable:
                # check if the cached callable is up to date
                if process._callable_index < self._last_callable_index:
                    process._callable_index = self._last_callable_index
                else:
                    task_callable = None
            # push the task to the process
            pickle.dump((data, task_callable, task_index),
                        process.stdin, protocol=-1)
            process.stdin.flush()
            # wait for result to arrive
            result = pickle.load(process.stdout)
        except:
            traceback.print_exc()
            self._free_processes.append(process)
            sys.exit("failed to execute task %d in process:" % task_index)
        # store the result and clean up
        self._store_result(result, task_index)
        self._free_processes.append(process)


def _process_run(cache_callable=True):
    """Run this function in a worker process to receive and run tasks.

    It waits for tasks on stdin, and sends the results back via stdout.
    """
    # use sys.stdout only for pickled objects, everything else goes to stderr
    # NOTE: .buffer is the binary mode interface for stdin and out in py3k
    try:
        pickle_out = sys.stdout.buffer
    except AttributeError:
        pickle_out = sys.stdout
    try:
        pickle_in = sys.stdin.buffer
    except AttributeError:
        pickle_in = sys.stdin

    sys.stdout = sys.stderr
    exit_loop = False
    last_callable = None  # cached callable
    while not exit_loop:
        task = None
        try:
            # wait for task to arrive
            task = pickle.load(pickle_in)
            if task == "EXIT":
                exit_loop = True
            else:
                data, task_callable, task_index = task
                if task_callable is None:
                    if last_callable is None:
                        err = ("No callable was provided and no cached "
                               "callable is available.")
                        raise Exception(err)
                    task_callable = last_callable.fork()
                elif cache_callable:
                    # store callable in cache
                    last_callable = task_callable
                    task_callable.setup_environment()
                    task_callable = task_callable.fork()
                else:
                    task_callable.setup_environment()
                result = task_callable(data)
                del task_callable  # free memory
                pickle.dump(result, pickle_out, protocol=-1)
                pickle_out.flush()
        except Exception, exception:
            # return the exception instead of the result
            if task is None:
                print "unpickling a task caused an exception in a process:"
            else:
                print "task %d caused exception in process:" % task[2]
            print exception
            traceback.print_exc()
            sys.stdout.flush()
            sys.exit()

if __name__ == "__main__":
    # first argument is cache_callable flag
    cache_callable = sys.argv[1] == "True"

    if len(sys.argv) > 2:
        # remaining arguments are code paths,
        # put them in front so that they take precedence over PYTHONPATH
        new_paths = [sys_arg for sys_arg in sys.argv[2:]
                     if sys_arg not in sys.path]
        sys.path = new_paths + sys.path
    _process_run(cache_callable=cache_callable)
