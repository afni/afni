#!/usr/bin/env python3
"""Check real stderr TTY detection and pipe behavior of test_progress --demo."""
import os
import pty
import select
import subprocess
import sys


def main():
    binary = os.path.abspath(sys.argv[1])
    for mode in ('auto', 'line', 'bar', 'counter', 'dots', 'digits', 'ticks', 'index', 'off'):
        result = subprocess.run([binary, '--demo', mode], capture_output=True, timeout=10, check=True)
        assert not result.stdout, (mode, 'progress leaked to stdout')
        assert b'\x1b' not in result.stderr, (mode, 'terminal escapes in a pipe')
        if mode == 'off':
            assert not result.stderr
        else:
            assert result.stderr.endswith(b'\n'), mode
        if mode in ('auto', 'line'):
            assert b'\r' not in result.stderr
            assert result.stderr.count(b'10/10') == 1
        if mode == 'bar':
            assert b'[####################]' in result.stderr

    for mode in ('auto', 'line', 'off'):
        master, slave = pty.openpty()
        try:
            # The demo's two short reports fit in the PTY buffer. Keep stdout
            # separate to prove terminal mode does not contaminate data output.
            result = subprocess.run([binary, '--demo', mode], stdout=subprocess.PIPE,
                                    stderr=slave, timeout=10, check=True)
            output = bytearray()
            # The child has exited, so all output is available. On macOS,
            # closing the last slave first can discard its buffered output.
            while select.select([master], [], [], 0)[0]:
                output.extend(os.read(master, 4096))
            assert not result.stdout
            if mode == 'auto':
                assert b'[####################]' in output
                assert b'\x1b[K' in output
                assert output.count(b'10/10') == 1
            elif mode == 'line':
                assert b'\x1b' not in output and b'[####' not in output
            else:
                assert not output
        finally:
            if slave is not None:
                os.close(slave)
            os.close(master)
    print('thd_progress: terminal and pipe tests passed')


if __name__ == '__main__':
    main()
