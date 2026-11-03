#!/usr/bin/env python3
"""Exercise LSS ingestion with real 3dDeconvolve/3dLSS output and shuffled events."""
import argparse
import os
from pathlib import Path
import subprocess
import numpy as np
import nibabel as nib

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--prog', required=True)
p.add_argument('--deconvolve', required=True)
p.add_argument('--lss', required=True)
p.add_argument('--workdir', required=True)
a = p.parse_args()
w = Path(a.workdir).resolve()
w.mkdir(parents=True, exist_ok=True)
env = dict(os.environ, OMP_NUM_THREADS='2', AFNI_DECONFLICT='OVERWRITE')

def run(args, error=None):
    result = subprocess.run(list(map(str, args)), cwd=w, env=env,
                            text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    with (w / 'commands.log').open('a') as f:
        f.write(' '.join(map(str, args)) + '\n' + result.stdout + result.stderr)
    if error is None:
        assert result.returncode == 0, result.stderr
    else:
        assert result.returncode != 0 and error in result.stderr, result.stderr
    return result.stdout

rng = np.random.default_rng(42)
nib.save(nib.Nifti1Image(np.ones((3, 3, 2), dtype='uint8'), np.eye(4)), w / 'mask.nii.gz')
subject_rows, explicit_rows = [], []
reference_betas = rng.normal(size=(18, 6))
for i in range(3):
    subj = f's{i+1}'
    (w / f'{subj}.times.1D').write_text('5 13 21 29 37 45\n5 13 21 29 37 45\n')
    # Include censoring so run inference must use GoodList, not row position.
    censor = np.ones(120, dtype=int)
    censor[[0, 1, 17, 59, 60, 61, 89]] = 0
    np.savetxt(w / 'censor.1D', censor, fmt='%d')
    run([a.deconvolve, '-nodata', '120', '1', '-polort', '1', '-concat', '1D: 0 60',
         '-censor', 'censor.1D', '-num_stimts', '1', '-stim_times_IM', '1',
         f'{subj}.times.1D', 'BLOCK(1,1)', '-stim_label', '1', 'event',
         '-x1D', f'{subj}.X.xmat.1D', '-x1D_stop'])
    design = np.loadtxt(w / f'{subj}.X.xmat.1D')
    order = np.r_[rng.permutation(6), rng.permutation(6)]
    raw = rng.normal(scale=.1, size=(18, 120)) + 100
    raw[:, censor.astype(bool)] += reference_betas[:, order] @ design[:, -12:].T
    nib.save(nib.Nifti1Image(raw.reshape(3, 3, 2, 120).astype('float32'), np.eye(4)),
             w / f'{subj}.bold.nii.gz')
    run([a.lss, '-input', f'{subj}.bold.nii.gz', '-matrix', f'{subj}.X.xmat.1D',
         '-prefix', f'{subj}.LSS.nii.gz'])
    columns = ['Trial', 'Item', 'Condition', 'Repeat', 'Fold']
    if i == 1:
        columns = columns[::-1]
    events = [' '.join(columns)]
    # Scramble event rows independently of volume order.
    for trial in rng.permutation(12):
        item = int(order[trial])
        row = dict(Trial=str(trial), Item=f'image_{item}', Condition=f'cat{item%2}',
                   Repeat=str(trial//6+1), Fold=f'category{item%2}')
        events.append(' '.join(row[c] for c in columns))
        explicit_rows.append(f'{subj} control {row["Item"]} {row["Condition"]} '
                             f'{row["Repeat"]} {row["Fold"]} {trial//6+1} '
                             f'{subj}.LSS.nii.gz[{trial}]')
    (w / f'{subj}.events.txt').write_text('\n'.join(events)+'\n')
    subject_rows.append(f'{subj} control {subj}.X.xmat.1D {subj}.events.txt {subj}.LSS.nii.gz')
subject_text = 'Subj Group Xmat Events InputFile\n'+'\n'.join(subject_rows)+'\n'
(w / 'subjects.txt').write_text(subject_text)
(w / 'explicit.txt').write_text('Subj Group Item Condition Repeat Fold Run InputFile\n'+
                                '\n'.join(explicit_rows)+'\n')
base = [a.prog, '-axis', 'item', '-lss', '-dataTableFile', 'subjects.txt']
shown = run(base + ['-show_table'])
(w / 'expanded.txt').write_text(shown)
# Parse the printed table independently and check each selector and inferred run.
lines = [line.split() for line in shown.splitlines() if line.strip() and not line.startswith('#')]
head, *rows = lines
assert len(rows) == 36
for cells in rows:
    row = dict(zip(head, cells))
    trial = int(row['Trial'])
    assert row['InputFile'] == f'{row["Subj"]}.LSS.nii.gz[{trial}]'
    assert int(row['Run']) == trial//6+1
    assert row['Group'] == 'control' and float(row['Xmat.ncensored']) == 7
assert 'Xmat' not in head and 'Events' not in head

# Numerical equality of automatic, independently written explicit, and round-trip
# tables. Exercise both algorithms, fit and held-out run scoring.
for algorithm in ['det', 'prob']:
    for cv in ['none', 'run']:
        common = ['-algorithm', algorithm, '-nfeatures', '2', '-niter', '12',
                  '-seed', '123', '-cv', cv, '-save_W', '-mask', 'mask.nii.gz']
        prefixes = []
        for name, table, opts in [('auto', 'subjects.txt', ['-lss']),
                                  ('manual', 'explicit.txt', []),
                                  ('shown', 'expanded.txt', [])]:
            prefix = f'{algorithm}_{cv}_{name}'
            prefixes.append(prefix)
            run([a.prog, '-axis', 'item', '-dataTableFile', table, '-prefix', prefix] + common + opts)
        arrays = [np.loadtxt(w / f'{pre}.S.1D') for pre in prefixes]
        for value in arrays[1:]:
            np.testing.assert_array_equal(value, arrays[0])
        for subject in ['s1', 's2', 's3']:
            for kind in ['W', 'proj']:
                arrays = [nib.load(w / f'{pre}.{kind}.{subject}+orig.HEAD').get_fdata()
                          for pre in prefixes]
                for value in arrays[1:]:
                    np.testing.assert_array_equal(value, arrays[0])
        if cv == 'none':
            continue
        # Compare shared score fields; automatic tables add Xmat covariates.
        def scores(prefix):
            lines = [line.split() for line in (w / f'{prefix}.scores.txt').read_text().splitlines()
                     if line.strip() and not line.startswith('#')]
            return [dict(zip(lines[0], row)) for row in lines[1:]]
        expected = scores(prefixes[1])
        for prefix in [prefixes[0], prefixes[2]]:
            actual = scores(prefix)
            assert len(actual) == len(expected)
            for x, y in zip(actual, expected):
                for key, value in y.items():
                    assert x[key] == value, (prefix, key, x[key], value)

# Explicit run labels, custom item/repeat/run columns, and metadata survive.
original_events = [(w / f's{i+1}.events.txt').read_text() for i in range(3)]
for i, text in enumerate(original_events):
    rows = [line.split() for line in text.splitlines()]
    head = rows[0]
    ti = head.index('Trial')
    head[head.index('Item')] = 'Stimulus'
    head[head.index('Repeat')] = 'Presentation'
    head.append('Session')
    for row in rows[1:]:
        row.append('early' if int(row[ti]) < 6 else 'late')
    (w / f's{i+1}.events.txt').write_text('\n'.join(' '.join(row) for row in rows)+'\n')
custom = run(base + ['-item_column', 'Stimulus', '-repeat_column', 'Presentation',
                     '-run_column', 'Session', '-show_table'])
assert 'Stimulus' in custom and 'Presentation' in custom and 'late' in custom
for i, text in enumerate(original_events):
    (w / f's{i+1}.events.txt').write_text(text)
# Subject-level run labels are useful for separate per-run beta datasets.
(w / 'subjects.txt').write_text(subject_text.replace('Subj Group', 'Subj Run Group')
                              .replace(' control ', ' sessionA control '))
assert 'sessionA' in run(base + ['-show_table'])
(w / 'subjects.txt').write_text(subject_text)
# If a design column crosses runs, do not guess a run or silently make a fold.
xpath = w / 's1.X.xmat.1D'
original_xmat = xpath.read_text()
lines = original_xmat.splitlines()
numeric = [j for j, line in enumerate(lines) if line.strip() and not line.startswith('#')]
last = lines[numeric[-1]].split()
last[-12] = '1'
lines[numeric[-1]] = ' '.join(last)
xpath.write_text('\n'.join(lines)+'\n')
ambiguous = run(base + ['-show_table'])
assert 'Run' not in next(line.split() for line in ambiguous.splitlines()
                         if line.strip() and not line.startswith('#'))
xpath.write_text(original_xmat)

# Malformed mappings must fail before fitting, even in inspection mode.
original = (w / 's1.events.txt').read_text()
def bad_events(text, error):
    (w / 's1.events.txt').write_text(text)
    run(base + ['-show_table'], error)
    (w / 's1.events.txt').write_text(original)
lines = original.splitlines()
bad_events('\n'.join(lines[:-1])+'\n', 'must map all')
parts = lines[2].split(); parts[0] = lines[1].split()[0]
bad_events('\n'.join(lines[:2]+[' '.join(parts)]+lines[3:])+'\n', 'duplicate Trial')
for bad in ['12', '-1', '0.5', 'oops']:
    parts = lines[1].split(); parts[0] = bad
    bad_events('\n'.join([lines[0], ' '.join(parts)]+lines[2:])+'\n', 'zero-based integer')
bad_events(original.replace('Trial Item', 'Event Item'), 'Events needs Trial')
bad_events(original.replace('Fold', 'Group'), 'conflicts')
bad_events(original.replace('Fold', 'NewColumn'), "missing column")
run([a.prog, '-lss', '-dataTableFile', 'subjects.txt', '-show_table'], 'needs -axis item')
run(base + ['-xmat_stims', 'event', '-show_table'], 'cannot be combined')
run([a.prog, '-axis', 'item', '-dataTableFile', 'subjects.txt', '-show_table'], 'requires -lss')
# A selected dataset, ordinary stats bucket, or unrelated design is not LSS.
(w / 'subjects.txt').write_text(subject_text.replace('s1.LSS.nii.gz', 's1.LSS.nii.gz[0..11]'))
run(base + ['-show_table'], 'whole LSS dataset')
(w / 'subjects.txt').write_text(subject_text)
xpath = w / 's1.X.xmat.1D'
original_xmat = xpath.read_text()
xpath.write_text(original_xmat.replace('-stim_times_IM', '-stim_times'))
run(base + ['-show_table'], 'exactly one -stim_times_IM')
xpath.write_text(original_xmat)
# A plain NIfTI with the right count but no LSS labels must be rejected.
nib.save(nib.Nifti1Image(np.ones((3,3,2,12), dtype='float32'), np.eye(4)), w / 'unlabelled.nii.gz')
(w / 'subjects.txt').write_text(subject_text.replace('s1.LSS.nii.gz', 'unlabelled.nii.gz'))
run(base + ['-show_table'], 'must be labelled')
nib.save(nib.Nifti1Image(np.ones((3,3,2,11), dtype='float32'), np.eye(4)), w / 'short.nii.gz')
(w / 'subjects.txt').write_text(subject_text.replace('s1.LSS.nii.gz', 'short.nii.gz'))
run(base + ['-show_table'], '12 IM trials')
(w / 'subjects.txt').write_text(subject_text)
print('PASS: real LSS ingestion, shuffled identities/repeats, censored run mapping, both SRM algorithms, CV, round-trip tables, invalid inputs')
