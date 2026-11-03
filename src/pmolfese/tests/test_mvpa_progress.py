#!/usr/bin/env python3
"""Progress modes preserve predictions and finish monotonically under OpenMP."""
import errno
import os
from pathlib import Path
import pty
import re
import struct
import subprocess
import sys
import tempfile


def nifti(path, rows):
    h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    struct.pack_into('<8h', h, 40, 4, len(rows[0]), 1, 1, len(rows), 1, 1, 1)
    struct.pack_into('<2h', h, 70, 16, 32)
    struct.pack_into('<8f', h, 76, *([1] * 8))
    struct.pack_into('<f', h, 108, 352)
    struct.pack_into('<h', h, 254, 1)
    for i in range(3):
        row = [0.] * 4
        row[i] = 1
        struct.pack_into('<4f', h, 280 + 16*i, *row)
    h[344:348] = b'n+1\0'
    values = [v for row in rows for v in row]
    path.write_bytes(h + struct.pack('<%df' % len(values), *values))


binary = str(Path(sys.argv[1]).resolve())
env = dict(os.environ, AFNI_NIFTI_TYPE_WARN='NO', AFNI_NIFTI_VIEW='orig',
           OPENBLAS_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1')
with tempfile.TemporaryDirectory(prefix='mvpa_progress_') as temp:
    d = Path(temp)
    rows = [[(i % 4 - 1.5) * (j+1) + .1*(i//4) for j in range(8)]
            for i in range(12)]
    nifti(d/'data.nii', rows)
    nifti(d/'mask.nii', [[1, 1, 2, 2, 3, 3, 4, 4]])
    (d/'table.1D').write_text('Subj Run Condition Rating InputFile\n' + ''.join(
        f's1 r{i//4} {"A" if i%4<2 else "B"} {i%4-1.5} {d}/data.nii[{i}]\n'
        for i in range(12)))
    for mode, label in [('classify', 'Classification'), ('decode', 'Decoding'),
                        ('encode', 'Encoding')]:
        for searchlight in (False, True):
            reference = None
            count = 8 if searchlight else 4
            for display, quiet, workers, terminal in [
                    ('off', False, 1, False), ('line', False, 4, False),
                    ('auto', False, 4, False), ('bar', False, 4, False),
                    ('bar', True, 4, False), ('auto', False, 4, True)]:
                name = f'{mode}_{searchlight}_{display}_{quiet}_{terminal}'
                pred = d/(name+'.pred.1D')
                args = [binary, '-mode', mode, '-dataTable', '@'+str(d/'table.1D'),
                        '-mask', str(d/'mask.nii'), '-cv_column', 'Run',
                        '-progress', display, '-prefix', str(d/name),
                        '-save_predictions', str(pred)]
                if mode == 'classify':
                    args += ['-method', 'centroid', '-target_column', 'Condition']
                elif mode == 'decode':
                    args += ['-method', 'ridge', '-target_column', 'Rating']
                else:
                    args += ['-method', 'ridge', '-design_columns', 'Rating',
                             '-score', 'pattern']
                if searchlight:
                    args += ['-searchlight', 'SPHERE(1.1)']
                if quiet:
                    args += ['-quiet']
                run_env = dict(env, OMP_NUM_THREADS=str(workers))
                if terminal:
                    master, slave = pty.openpty()
                    chunks = []
                    try:
                        process = subprocess.Popen(args, stdout=subprocess.DEVNULL,
                                                   stderr=slave, env=run_env)
                        os.close(slave)
                        slave = None
                        while True:
                            try:
                                chunk = os.read(master, 65536)
                            except OSError as error:
                                if error.errno == errno.EIO:
                                    break
                                raise
                            if not chunk:
                                break
                            chunks.append(chunk)
                        result = subprocess.CompletedProcess(args, process.wait(),
                                                             b'', b''.join(chunks))
                    finally:
                        os.close(master)
                        if slave is not None:
                            os.close(slave)
                else:
                    result = subprocess.run(args, capture_output=True, env=run_env)
                assert result.returncode == 0, result.stderr.decode()
                stderr = result.stderr.decode()
                # Progress goes to stderr; stdout remains available to callers.
                assert f'3dMVPA {label}' not in result.stdout.decode()
                lines = [s for s in re.split(r'[\r\n]', stderr)
                         if f'3dMVPA {label}' in s]
                if quiet or display == 'off':
                    assert not lines, stderr
                else:
                    counts = [tuple(map(int, re.search(r'(\d+)/(\d+)', s).groups()))
                              for s in lines]
                    assert counts[0] == (0, count), counts
                    assert counts[-1] == (count, count), counts
                    assert [a for a, b in counts] == sorted(set(a for a, b in counts))
                    assert all(b == count for a, b in counts)
                    assert '100.0%' in lines[-1] and 'elapsed' in lines[-1]
                    assert ('[####################]' in lines[-1]) == (display == 'bar' or terminal)
                    if display != 'bar' and not terminal:
                        assert '\r' not in stderr
                values = '\n'.join(s for s in pred.read_text().splitlines()
                                   if not s.startswith('#'))
                if reference is None:
                    reference = values
                else:
                    assert values == reference, f'Progress changed predictions: {name}'
    for args in [['-progress'], ['-progress', 'invalid']]:
        result = subprocess.run([binary]+args, capture_output=True, env=env)
        assert result.returncode != 0 and b'auto, bar, line, or off' in result.stderr
print('PASS: progress modes, quiet override, parallel counts and unchanged predictions')
