#!/usr/bin/env python3
"""Deterministic M0 fixtures. Standard library only; no AFNI execution.

Creates small float32 NIfTI-1 volumes, full/censored synthetic Xmats, explicit
intervals, censor vectors, and analytic schedule expectations. Xmat support is
synthetic (not claimed to be an HRF fit). Paths in tables are relative to the
fixture directory. Never overwrite an existing fixture directory.
"""
import argparse
import json
import math
import random
import struct
from pathlib import Path

SEED = 20261104
SHAPE = (4, 3, 2)
NT = 48
NS = 6
TR = 2.0


def nifti(path, values, nt=1):
    """Minimal little-endian float NIfTI, x fastest, followed by y,z,time."""
    assert len(values) == math.prod(SHAPE) * nt
    header = bytearray(352)
    struct.pack_into('<i', header, 0, 348)
    struct.pack_into('<8h', header, 40, 4 if nt > 1 else 3, *SHAPE, nt, 1, 1, 1)
    struct.pack_into('<hh', header, 70, 16, 32)
    struct.pack_into('<8f', header, 76, 1, 2, 2, 2, TR, 0, 0, 0)
    struct.pack_into('<ff', header, 108, 352, 1)
    header[123] = 10  # mm and seconds
    struct.pack_into('<h', header, 254, 1)  # scanner anatomical sform
    for axis, offset in enumerate((280, 296, 312)):
        row = [0.0] * 4
        row[axis] = 2.0
        struct.pack_into('<4f', header, offset, *row)
    header[344:348] = b'n+1\0'
    path.write_bytes(header + struct.pack('<%df' % len(values), *values))


def support(t):
    t %= 24
    return int(2 <= t <= 9 or 14 <= t <= 21), int(7 <= t <= 12 or 18 <= t <= 23)


def xmat(path, keep):
    attrs = {'ni_type': '4*double', 'ni_dimen': str(sum(keep)),
             'ColumnLabels': 'Run#1Pol#0 ; Run#2Pol#0 ; Vis#0 ; Aud#0',
             'ColumnGroups': '-1,-1,1,2', 'RowTR': str(TR),
             'NRowFull': str(NT), 'RunStart': '0,24',
             'GoodList': ','.join(str(t) for t, k in enumerate(keep) if k),
             'Nstim': '2', 'StimBots': '2,3', 'StimTops': '2,3',
             'StimLabels': 'Vis ; Aud'}
    rows = ['# <matrix'] + [f'# {k} = "{v}"' for k, v in attrs.items()] + ['# >']
    rows += [f'{int(t < 24)} {int(t >= 24)} {support(t)[0]} {support(t)[1]}'
             for t in range(NT) if keep[t]]
    path.write_text('\n'.join(rows + ['# </matrix>']) + '\n')


def write_fixture(root):
    root = Path(root)
    root.mkdir(parents=True, exist_ok=False)
    rng = random.Random(SEED)
    nv = math.prod(SHAPE)
    clean = []
    for sub in range(NS):
        series = []
        for t in range(NT):
            series.append([math.sin(t * .31 + v * .09) + .4 * math.cos(t * .13) +
                           .15 * (sub // 3) * math.sin(t * .61) + .35 * rng.gauss(0, 1)
                           for v in range(nv)])
        # Tie-rich, nonconstant signals for Spearman checks.
        for t in range(NT):
            series[t][1] = float((t + sub) % 5)
        clean.append(series)
        nifti(root / f's{sub+1:02d}.nii', [x for row in series for x in row], NT)
        second = [[.8*x + .2*math.cos(t*.23 + sub*.19) for x in row]
                  for t, row in enumerate(series)]
        nifti(root / f's{sub+1:02d}_C2.nii', [x for row in second for x in row], NT)
        keep = [int(t not in (4+sub, 28+sub)) for t in range(NT)]
        (root / f's{sub+1:02d}.censor.1D').write_text('\n'.join(map(str, keep))+'\n')
        xmat(root / f's{sub+1:02d}.X.xmat.1D', keep)
    xmat(root/'X.full.xmat.1D', [1]*NT)
    common = [int(t not in list(range(4, 10)) + list(range(28, 34))) for t in range(NT)]
    (root/'common.censor.1D').write_text('\n'.join(map(str, common))+'\n')
    nifti(root/'atlas.nii', [1 if v % 4 < 2 else 2 for v in range(nv)])
    nifti(root/'mask.nii', [int(v % 4 < 2) for v in range(nv)])
    for name, extra in [('subjects.txt', ''), ('censor_subjects.txt', 'Censor '),
                        ('xmat_subjects.txt', 'Xmat XmatFull Censor ')]:
        rows = ['Subj Group Age '+extra+'InputFile']
        for sub in range(NS):
            sid=f's{sub+1:02d}'
            vals = (f'{sid}.censor.1D ' if name=='censor_subjects.txt' else
                    f'{sid}.X.xmat.1D X.full.xmat.1D {sid}.censor.1D ' if name=='xmat_subjects.txt' else '')
            rows.append(f'{sid} {"A" if sub<3 else "B"} {20+sub} {vals}{sid}.nii')
        (root/name).write_text('\n'.join(rows)+'\n')
    rows=['Subj Group Condition Age InputFile']
    intervals=['Subj Group Selection Segment Run StartTR StopTR InputFile']
    for sub in range(NS):
        sid=f's{sub+1:02d}'; group='A' if sub<3 else 'B'
        for cond, suffix in [('C1',''),('C2','_C2')]:
            rows.append(f'{sid} {group} {cond} {20+sub} {sid}{suffix}.nii')
        for run in (1,2):
            for block,(start,stop) in enumerate(((2,9),(14,21)),1):
                for selection in (f'R{run}_Vis_{block}', 'Vis_all'):
                    intervals.append(f'{sid} {group} {selection} b{run}_{block} {run} {start} {stop} {sid}.nii')
    (root/'conditions.txt').write_text('\n'.join(rows)+'\n')
    (root/'intervals.txt').write_text('\n'.join(intervals)+'\n')
    # Separate adversarial inputs: never contaminate the clean regression set.
    constant=[list(row) for row in clean[0]]
    for row in constant: row[0]=7.0
    nifti(root/'constant_s01.nii', [x for row in constant for x in row], NT)
    bad=[list(row) for row in clean[0]]; bad[6][3]=float('nan')
    nifti(root/'nonfinite_s01.nii', [x for row in bad for x in row], NT)
    (root/'nonfinite_s01.1D').write_text('\n'.join(' '.join(str(bad[t][v]) for t in range(NT)) for v in range(nv))+'\n')
    expected={'schema':1,'seed':SEED,'shape':SHAPE,'ntime':NT,'tr':TR,
              'run_starts':[0,24], 'support_threshold':.5,
              'blocks':{'Vis':[[2,9],[14,21],[26,33],[38,45]],
                        'Aud':[[7,12],[18,23],[31,36],[42,47]]},
              'overlap_trs_per_run':7,'common_kept':sum(common),
              'common_vis_block_kept':[2,8,2,8],
              'windows_w8_s4':[[start,start+7] for run in (0,24) for start in range(run,run+17,4)],
              'notes':['No HRF claim: support values are synthetic.',
                       'NIfTI readers may sanitize NaNs; test nonfinite .1D separately.',
                       'Future Censor/Xmat columns are fixture inputs, not implemented options.']}
    (root/'expected_schedule.json').write_text(json.dumps(expected,indent=2)+'\n')
    return root


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory',type=Path)
    args=parser.parse_args()
    print(write_fixture(args.directory))
