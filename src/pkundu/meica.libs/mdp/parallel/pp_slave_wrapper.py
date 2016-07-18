#!/usr/bin/env python

"""
Script to be called to start a network server.

It differs from calling ppserver.py mainly in that it allows to add paths to
sys.path. So it acts like a wrapper for the server initialization.
The paths are passed via sys.argv[5:]. The first four arguments are
port, timeout, secret, n_workers.
"""

import sys

def main():
    port, timeout, secret, n_workers = sys.argv[1:5]
    port = int(port)
    timeout = int(timeout)
    n_workers = int(n_workers)
    sys_paths = sys.argv[5:]
    for sys_path in sys_paths:
        sys.path.append(sys_path)
    import ppserver
    ## initialization code as in ppserver.py
    server = ppserver._NetworkServer(ncpus=n_workers,
                                     port=port,
                                     secret=secret,
                                     timeout=timeout)
    print "Server is ready."
    sys.stdout.flush()
    server.listen()

if __name__ == "__main__":
    main()
