"""
Adapters for the Parallel Python library (http://www.parallelpython.com).

The PPScheduler class uses an existing pp scheduler and is a simple adapter.

LocalPPScheduler includes the creation of a local pp scheduler.
NetworkPPScheduler includes the management of the remote slaves via SSH.
"""

from __future__ import with_statement

import sys
import os

import time
import subprocess
import signal
import traceback
import tempfile

import scheduling
import pp
import mdp

TEMPDIR_PREFIX='pp4mdp-monkeypatch.'

def _monkeypatch_pp(container_dir):
    """Apply a hack for http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=620551.

    Importing numpy fails because the parent directory of the slave
    script (/usr/share/pyshared) is added to the begging of sys.path.
    This is a temporary fix until parallel python or the way it is
    packaged in debian is changed.

    This function monkey-patches the ppworker module and changes the
    path to the slave script. A temporary directory is created and the
    worker script is copied there.

    The temporary directory should be automatically removed when this
    module is destroyed.

    XXX: remove this when parallel python or the way it is packaged in debian is changed.
    """
    import os.path, shutil

    # this part copied from pp.py, should give the same result hopefully
    ppworker = os.path.join(os.path.dirname(os.path.abspath(pp.__file__)),
                            'ppworker.py')

    global _ppworker_dir
    _ppworker_dir = mdp.utils.TemporaryDirectory(prefix=TEMPDIR_PREFIX, dir=container_dir)
    ppworker3 = os.path.join(_ppworker_dir.name, 'ppworker.py')
    shutil.copy(ppworker, ppworker3)

    mdp._pp_worker_command = pp._Worker.command[:]
    try:
        pp._Worker.command[pp._Worker.command.index(ppworker)] = ppworker3
    except TypeError:
        # pp 1.6.0 compatibility
        pp._Worker.command = pp._Worker.command.replace(ppworker, ppworker3)

if hasattr(mdp.config, 'pp_monkeypatch_dirname'):
    _monkeypatch_pp(mdp.config.pp_monkeypatch_dirname)

class PPScheduler(scheduling.Scheduler):
    """Adaptor scheduler for the parallel python scheduler.

    This scheduler is a simple wrapper for a pp server. A pp server instance
    has to be provided.
    """

    def __init__(self, ppserver, max_queue_length=1,
                 result_container=None, verbose=False):
        """Initialize the scheduler.

        ppserver -- Parallel Python Server instance.
        max_queue_length -- How long the queue can get before add_task blocks.
        result_container -- ResultContainer used to store the results.
            ListResultContainer by default.
        verbose -- If True to get progress reports from the scheduler.
        """
        if result_container is None:
            result_container = scheduling.ListResultContainer()
        super(PPScheduler, self).__init__(result_container=result_container,
                                          verbose=verbose)
        self.ppserver = ppserver
        self.max_queue_length = max_queue_length

    def _process_task(self, data, task_callable, task_index):
        """Non-blocking processing of tasks.

        Depending on the scheduler state this function is non-blocking or
        blocking. One reason for blocking can be a full task-queue.
        """
        task = (data, task_callable.fork(), task_index)
        def execute_task(task):
            """Call the first args entry and return the return value."""
            data, task_callable, task_index = task
            task_callable.setup_environment()
            return task_callable(data), task_index
        while True:
            if len(self.ppserver._Server__queue) > self.max_queue_length:
                # release lock for other threads and wait
                self._lock.release()
                time.sleep(0.5)
                self._lock.acquire()
            else:
                # release lock to enable result storage
                self._lock.release()
                # the inner tuple is a trick to prevent introspection by pp
                # this forces pp to simply pickle the object
                self.ppserver.submit(execute_task, args=(task,),
                                     callback=self._pp_result_callback)
                break

    def _pp_result_callback(self, result):
        """Calback method for pp to unpack the result and the task id.

        This method then calls the normal _store_result method.
        """
        if result is None:
            result = (None, None)
        self._store_result(*result)

    def _shutdown(self):
        """Call destroy on the ppserver."""
        self.ppserver.destroy()


class LocalPPScheduler(PPScheduler):
    """Uses a local pp server to distribute the work across cpu cores.

    The pp server is created automatically instead of being provided by the
    user (in contrast to PPScheduler).
    """

    def __init__(self, ncpus="autodetect", max_queue_length=1,
                 result_container=None, verbose=False):
        """Create an internal pp server and initialize the scheduler.

        ncpus -- Integer or 'autodetect', specifies the number of processes
            used.
        max_queue_length -- How long the queue can get before add_task blocks.
        result_container -- ResultContainer used to store the results.
            ListResultContainer by default.
        verbose -- If True to get progress reports from the scheduler.
        """
        ppserver = pp.Server(ncpus=ncpus)
        super(LocalPPScheduler, self).__init__(ppserver=ppserver,
                                          max_queue_length=max_queue_length,
                                          result_container=result_container,
                                          verbose=verbose)


# default secret
SECRET = "rosebud"

class NetworkPPScheduler(PPScheduler):
    """Scheduler which can manage pp remote servers (requires SSH).

    The remote slave servers are automatically started and killed at the end.

    Since the slaves are started via SSH this schduler does not work on normal
    Windows systems. On such systems you can start the pp slaves
    manually and then use the standard PPScheduler.
    """

    def __init__(self, max_queue_length=1,
                 result_container=None,
                 verbose=False,
                 remote_slaves=None,
                 source_paths=None,
                 port=50017,
                 secret=SECRET,
                 nice=-19,
                 timeout=3600,
                 n_local_workers=0,
                 slave_kill_filename=None,
                 remote_python_executable=None):
        """Initialize the remote slaves and create the internal pp scheduler.

        result_container -- ResultContainer used to store the results.
            ListResultContainer by default.
        verbose -- If True to get progress reports from the scheduler.
        remote_slaves -- List of tuples, the first tuple entry is a string
            containing the name or IP adress of the slave, the second entry
            contains the number of processes (i.e. the pp ncpus parameter).
            The second entry can be None to use 'autodetect'.
        source_paths -- List of paths that will be appended to sys.path in the
        slaves.
        n_local_workers -- Value of ncpus for this machine.
        secret -- Secret password to secure the remote slaves.
        slave_kill_filename -- Filename (including path) where a list of the
            remote slave processes should be stored. Together with the
            'kill_slaves' function this makes it possible to quickly all
            remote slave processes in case something goes wrong.
            If None, a tempfile is created.
        """
        self._remote_slaves = remote_slaves
        self._running_remote_slaves = None  # list of strings 'address:port'
        # list with processes for the ssh connections to the slaves
        self._ssh_procs = None
        self._remote_pids = None  # list of the pids of the remote servers
        self._port = port
        if slave_kill_filename is None:
            slave_kill_file = tempfile.mkstemp(prefix='MDPtmp-')[1]
        self.slave_kill_file = slave_kill_file
        self._secret = secret
        self._slave_nice = nice
        self._timeout = timeout
        if not source_paths:
            self._source_paths = []
        else:
            self._source_paths = source_paths
        if remote_python_executable is None:
            remote_python_executable = sys.executable
        self._python_executable = remote_python_executable
        module_file = os.path.abspath(__file__)
        self._script_path = os.path.dirname(module_file)
        self.verbose = verbose
        # start ppserver
        self._start_slaves()
        ppslaves = tuple(["%s:%d" % (address, self._port)
                          for address in self._running_remote_slaves])
        ppserver = pp.Server(ppservers=ppslaves,
                             ncpus=n_local_workers,
                             secret=self._secret)
        super(NetworkPPScheduler, self).__init__(ppserver=ppserver,
                                          max_queue_length=max_queue_length,
                                          result_container=result_container,
                                          verbose=verbose)

    def _shutdown(self):
        """Shutdown all slaves."""
        for ssh_proc in self._ssh_procs:
            os.kill(ssh_proc.pid, signal.SIGQUIT)
        super(NetworkPPScheduler, self)._shutdown()
        if self.verbose:
            print "All slaves shut down."

    def start_slave(self, address, ncpus="autodetect"):
        """Start a single remote slave.

        The return value is a tuple of the ssh process handle and
        the remote pid.
        """
        try:
            print "starting slave " + address + " ..."
            proc = subprocess.Popen(["ssh","-T", "%s" % address],
                                    stdin=subprocess.PIPE,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
            proc.stdin.write("cd %s\n" % self._script_path)
            cmd = (self._python_executable +
                   " pp_slave_script.py  %d %d %d %s %d" %
                   (self._slave_nice, self._port, self._timeout, self._secret,
                    ncpus))
            proc.stdin.write(cmd + "\n")
            # send additional information to the remote process
            proc.stdin.write(self._python_executable + "\n")
            for sys_path in self._source_paths:
                proc.stdin.write(sys_path + "\n")
            proc.stdin.write("_done_" + "\n")
            # print status message from slave
            sys.stdout.write(address + ": " + proc.stdout.readline())
            # get PID for remote slave process
            pid = None
            if self.verbose:
                print "*** output from slave %s ***" % address
            while pid is None:
                # the slave process might first output some hello message
                try:
                    value = proc.stdout.readline()
                    if self.verbose:
                        print value
                    pid = int(value)
                except ValueError:
                    pass
            if self.verbose:
                print "*** output end ***"
            return (proc, pid)
        except:
            print "Initialization of slave %s has failed." % address
            traceback.print_exc()
            return None

    def _start_slaves(self):
        """Start remote slaves.

        The slaves that could be started are stored in a textfile, in the form
        name:port:pid
        """
        with open(self.slave_kill_file, 'w') as slave_kill_file:
            self._running_remote_slaves = []
            self._remote_pids = []
            self._ssh_procs = []
            for (address, ncpus) in self._remote_slaves:
                ssh_proc, pid = self.start_slave(address, ncpus=ncpus)
                if pid is not None:
                    slave_kill_file.write("%s:%d:%d\n" %
                                          (address, pid, ssh_proc.pid))
                self._running_remote_slaves.append(address)
                self._remote_pids.append(pid)
                self._ssh_procs.append(ssh_proc)

def kill_slaves(slave_kill_filename):
    """Kill all remote slaves which are stored in the given file.

    This functions is only meant for emergency situations, when something
    went wrong and the slaves have to be killed manually.
    """
    with open(slave_kill_filename) as tempfile:
        for line in tempfile:
            address, pid, ssh_pid = line.split(":")
            pid = int(pid)
            ssh_pid = int(ssh_pid)
            # open ssh connection to to kill remote slave
            proc = subprocess.Popen(["ssh","-T", address],
                                    stdin=subprocess.PIPE,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
            proc.stdin.write("kill %d\n" % pid)
            proc.stdin.flush()
            # kill old ssh connection
            try:
                os.kill(ssh_pid, signal.SIGKILL)
            except:
                pass
            # a kill might prevent the kill command transmission
            # os.kill(proc.pid, signal.SIGQUIT)
            print "killed slave " + address + " (pid %d)" % pid
        print "all slaves killed."


if __name__ == "__main__":
    if len(sys.argv) == 2:
        kill_slaves(sys.argv[1])
    else:
        sys.stderr.write("usage: %s slave_list.txt\n" % __file__)
