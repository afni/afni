#!/usr/bin/env python3
"""Compare SUMA control-flow refactors to same-toolchain baseline executables.

Generate all inputs locally. Require byte-identical numeric outputs, fitted ACF
reports and return codes; no numerical tolerance is used. Command provenance and
build banners are not numerical output. Requires NumPy and nibabel.
"""
import argparse
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

import nibabel as nib
import numpy as np


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for name in ('sim', 'fwhm', 'baseline-sim', 'baseline-fwhm', 'ico'):
        p.add_argument('--' + name, required=True)
    p.add_argument('--workdir')
    p.add_argument('--seed-library', required=True,
                   help='test-only library compiled from surf_fixed_seed.c')
    args = p.parse_args()
    programs = {name: str(Path(getattr(args, name.replace('-', '_'))).resolve())
                for name in ('sim', 'fwhm', 'baseline-sim', 'baseline-fwhm', 'ico')}
    temp = tempfile.TemporaryDirectory(prefix='surf-goto-') if not args.workdir else None
    root = Path(args.workdir or temp.name).resolve()
    root.mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix='case-', dir=root))
    env = dict(os.environ, OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1',
               AFNI_NOSPLASH='YES', AFNI_VERSION_CHECK='NO', AFNI_RANDOM_SEEDVAL='713')

    def run(program, options):
        return subprocess.run([program, *map(str, options)], cwd=work, env=env,
                              text=True, capture_output=True, timeout=600)

    for name, rd in [('ico', 3), ('fine', 4)]:
        result = run(programs['ico'], ['-rad', '100', '-rd', rd, '-prefix', name])
        assert result.returncode == 0, result.stderr
    xyz = np.loadtxt(work/'ico.asc', skiprows=2, max_rows=642)[:, :3]
    inner = (work/'ico.asc').read_text().splitlines()
    for i, coord in enumerate(xyz):
        inner[i+2] = ' '.join(map(str, .9*coord)) + ' 0'
    (work/'inner.asc').write_text('\n'.join(inner) + '\n')
    with (work/'ico.spec').open('a') as spec:
        spec.write('\nStateDef = inner\nNewSurface\n SurfaceFormat = ASCII\n SurfaceType = FreeSurfer\n'
                   ' SurfaceName = inner.asc\n LocalDomainParent = ./SAME\n'
                   ' SurfaceState = inner\n EmbedDimension = 3\n Anatomical = N\n')
    rng = np.random.default_rng(713)
    field = np.column_stack([np.sin(xyz[:, 0]/30) + np.cos(xyz[:, 1]/40)
                             + rng.normal(0, .3, len(xyz)) for _ in range(5)])
    for name, data in [('field', field), ('constant', np.ones((len(xyz), 2))),
                       ('mixed', np.column_stack([np.ones(len(xyz)), field])),
                       ('mask', (xyz[:, 2] > -30).astype(int)),
                       ('empty', np.zeros(len(xyz)))]:
        np.savetxt(work/f'{name}.1D', data, fmt='%.9g')
    # A volume enclosing the generated sphere exercises volume-to-surface mapping.
    affine = np.diag([10., 10., 10., 1.]); affine[:3, 3] = -120
    nib.save(nib.Nifti1Image(np.ones((25, 25, 25), dtype=np.uint8), affine),
             work/'volume.nii.gz')

    # Powell overrides AFNI_RANDOM_SEEDVAL with time()+getpid(). A test-only
    # srand48 interposer fixes that seed without modifying production behavior.
    env['DYLD_INSERT_LIBRARIES' if sys.platform == 'darwin' else 'LD_PRELOAD'] = str(Path(args.seed_library).resolve())
    count = 0

    def compare(kind, options, prefix, expected=0):
        nonlocal count
        records = []
        for key in ('baseline-' + kind, kind):
            result = run(programs[key], options)
            if expected is not None:
                assert result.returncode == expected, (key, options, result.stderr)
            files = {path.name: path.read_bytes() for path in work.glob(prefix + '*')
                     if path.is_file()}
            # Both programs use a stable command name in output provenance,
            # so numeric metadata, tables and ACF curves can be compared as bytes.
            normalized = files
            generated = re.findall(r'generated a=[^\r\n]*', result.stdout + result.stderr)
            records.append((result.returncode, result.stdout, normalized, generated))
            (work/f'{count:02d}-{key}.log').write_text(result.stdout + result.stderr)
            for name in files:
                (work/name).unlink()
        assert records[0] == records[1], (kind, options, 'baseline mismatch', work)
        count += 1
        print(f'PASS {count}: {kind} {prefix}', flush=True)
        return records[1]

    common = ['-spec', 'ico.spec', '-surf_A', 'ico', '-on_surface',
              '-niter', '60', '-pthr', '.05', '.01', '-athr', '.1', '.05',
              '-1sided', '-2sided', '-bisided', '-legacy_sided', '-maxarea_1D',
              '-selfcheck']
    fixed = ['-fixed', '-sigma', '5', '-Niter', '4']
    for radius in ['-1', '-2', '30']:
        compare('sim', common + fixed + ['-rmm', radius, '-multithresh',
                '-prefix', 'fixed', '-nthreads', '2'], 'fixed')
    masked = []
    for threads in ['1', '4']:
        record = compare('sim', common + fixed + ['-surf_mask', 'mask.1D', '-itersize', '7',
                '-prefix', 'masked', '-nthreads', threads], 'masked')
        masked.append({name: value for name, value in record[2].items()
                       if '.max.area.' in name})
    assert masked[0] == masked[1], 'thread count changed raw maxima'
    for block in ['2', '12', '60']:
        compare('sim', common + ['-compat', '-target_fwhm', '40', '-itersize', block,
                '-prefix', 'compat', '-nthreads', '2'], 'compat')
    compare('sim', ['-spec', 'ico.spec', '-surf_A', 'ico', '-surf_B', 'inner',
            '-sv', 'volume.nii.gz', '-vol_mask', 'volume.nii.gz', *fixed,
            '-niter', '12', '-pthr', '.05', '-1sided', '-maxarea_1D',
            '-prefix', 'volume_out'], 'volume_out')
    acf = compare('sim', ['-spec', 'fine.spec', '-surf_A', 'fine', '-on_surface',
            '-acf', '.5', '8', '24', '-acf_nbasis', '6', '-niter', '20',
            '-pthr', '.01', '-1sided', '-maxarea_1D', '-prefix', 'acf_out',
            '-nthreads', '4', '-verb'], 'acf_out')
    assert acf[3], 'ACF calibration did not report generated parameters'
    achieved = float(re.search(r'generated a=([0-9.eE+-]+)', acf[3][0])[1])
    assert abs(achieved - .5) < .15, achieved  # existing scientific acceptance gate

    for name, extra in [('ordinary', []), ('acf', ['-acf', 'curve.1D', '80', '-acf_binwidth', '10']),
                        ('masked_acf', ['-acf', 'curve.1D', '80', '-acf_binwidth', '10', '-b_mask', 'mask.1D']),
                        ('detrended_acf', ['-acf', 'curve.1D', '80', '-acf_binwidth', '10', '-detpoly', '1'])]:
        compare('fwhm', ['-i_fs', 'ico.asc', '-input', 'field.1D', '-clean', *extra],
                'curve')
    for data in ['constant', 'mixed']:
        compare('fwhm', ['-i_fs', 'ico.asc', '-input', data+'.1D', '-clean',
                '-acf', 'curve.1D', '80', '-acf_binwidth', '10'], 'curve', expected=None)
    compare('fwhm', ['-i_fs', 'ico.asc', '-input', 'field.1D', '-clean',
            '-acf', 'curve.1D', '20', '-acf_binwidth', '10'], 'curve', expected=None)
    for mask in ['empty', 'mask']:
        compare('fwhm', ['-i_fs', 'ico.asc', '-input', 'mixed.1D', '-clean',
                '-acf', 'curve.1D', '80', '-acf_binwidth', '10',
                '-b_mask', mask+'.1D'], 'curve', expected=None)
    print(f'{count} baseline comparisons passed; artifacts: {work}')
    if temp:
        temp.cleanup()


if __name__ == '__main__':
    main()
