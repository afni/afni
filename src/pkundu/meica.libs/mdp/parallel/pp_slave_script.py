#!/usr/bin/env python

"""
Script to be called on a remote machine for starting a pp network server.

This script calls pp_slave_wrapper in a new process and returns the pid.
The ssh connection stays open and can be used to kill the server process.

The python_executable and the paths are send via stdin.
The first sys.argv argument ist the nice value.
The other arguments and the paths are then used as arguments for the
wrapper script.
"""

import sys
import subprocess

def main():
    try:
        # receive sys_paths via stdin to be used in the wrapper
        python_executable = sys.stdin.readline()[:-1] # remove newline character
        sys_paths = []
        while True:
            sys_path = sys.stdin.readline()[:-1] # remove newline character
            if sys_path == "_done_":
                break
            sys_paths.append(sys_path)
        # assemble the command line for the wrapper by forwarding the arguments and
        cmd = ("nice %s %s pp_slave_wrapper.py" %
               (sys.argv[1], python_executable))
        for arg in sys.argv[2:]:
            cmd += " " + arg
        for sys_path in sys_paths:
            cmd += " " + sys_path
        # start the subprocess in which the slave process runs
        proc = subprocess.Popen(cmd, shell=True,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        # print status message from slave process
        sys.stdout.write(proc.stdout.readline())
        sys.stdout.flush()
        # return the pid via stdout
        print proc.pid
        sys.stdout.flush()
    except Exception, e:
        print "Error while starting the server process."
        print e
        print -1
        sys.stdout.flush()

if __name__ == "__main__":
    main()
