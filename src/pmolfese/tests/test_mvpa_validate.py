#!/usr/bin/env python3
"""Exercise validation through the public CLI; tiny NIfTI fixtures, stdlib only."""
import csv
import os
from pathlib import Path
import struct
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())
env = dict(os.environ, OMP_NUM_THREADS='2', AFNI_FLOATSCAN='NO')


def nifti(path, volumes, nx=4):
    header = bytearray(352)
    struct.pack_into('<i', header, 0, 348)
    struct.pack_into('<8h', header, 40, 4, nx, 2, 1, len(volumes), 1, 1, 1)
    struct.pack_into('<2h', header, 70, 16, 32)
    struct.pack_into('<8f', header, 76, 1, 1, 1, 1, 1, 1, 1, 1)
    struct.pack_into('<f', header, 108, 352)
    struct.pack_into('<h', header, 254, 1)
    for i in range(3):
        row = [0., 0., 0., 0.]
        row[i] = 1.
        struct.pack_into('<4f', header, 280 + 16*i, *row)
    header[344:348] = b'n+1\0'
    values = [v for volume in volumes for v in volume]
    assert len(values) == nx * 2 * len(volumes)
    path.write_bytes(header + struct.pack('<%df' % len(values), *values))


with tempfile.TemporaryDirectory(prefix='mvpa_validate_') as folder:
    root = Path(folder)
    nifti(root/'mask.nii', [[1, 1, 2, 2, 0, 0, 0, 0]])
    nifti(root/'betas.nii', [[v, v, 5, 5, 0, 0, 0, 0] for v in (0, 10, 1, 9, 2, 8)])
    rows = [dict(Subj='s1', Run=str(i//2+1), Sample=f's{i}', Condition='AB'[i % 2],
                 Target=str(i), Volume=str(i), InputFile=str(root/'betas.nii')) for i in range(6)]

    def table(name, records):
        path = root/name
        with path.open('w') as f:
            w = csv.DictWriter(f, list(records[0]), delimiter='\t')
            w.writeheader(); w.writerows(records)
        return path

    manifest = table('manifest.1D', rows)
    direct = table('direct.1D', [dict(r, InputFile=r['InputFile']+'['+r['Volume']+']') for r in rows])
    common = ['-mode', 'classify', '-target_column', 'Condition', '-cv_column', 'Run',
              '-mask', str(root/'mask.nii')]

    def run(args, failure=None):
        result = subprocess.run([binary, *map(str, args)], cwd=root, env=env,
                                capture_output=True, text=True, timeout=60)
        text = result.stdout + result.stderr
        if failure is None:
            assert result.returncode == 0, text
        else:
            assert result.returncode != 0 and failure in text, text
            assert '# Validation PASS' not in text
        return result.stdout

    before = set(root.iterdir())
    report = run(common + ['-trial_manifest', manifest, '-validate_only',
                 '-save_predictions', 'never.pred.1D', '-save_fold_scores', 'never.fold.1D',
                 '-save_confusion', 'never.conf.1D', '-save_summary_scores', 'never.summary.1D',
                 '-prefix', 'never', '-nperm', '100000000', '-permute_within', 'Run'])
    assert set(root.iterdir()) == before, 'validation wrote analysis outputs'
    for expected in ('# Samples\t6', '# Classes\t2', '# Folds\t3', '1\tAll\t4\t2',
                     '# SelectedFeatures\t4', '# FiniteSelectedValues\t24',
                     '# PermutationBlockLevels\t3', '# Validation PASS\t0 warning(s)'):
        assert expected in report, (expected, report)
    assert '# samples from ' in report and 'BrickLabel' in report and '# ClassCounts' in report
    direct_report = run(common + ['-dataTableFile', direct, '-validate_only'])
    assert direct_report[direct_report.index('# 3dMVPA input validation'):] == report[report.index('# 3dMVPA input validation'):].replace('# PermutationBlockLevels\t3\n', '')
    # Inline tables must stop correctly at the new option.
    tokens = ['Subj', 'Run', 'Sample', 'Condition', 'InputFile']
    for r in rows:
        tokens += [r[k] for k in tokens[:4]] + [r['InputFile']+'['+r['Volume']+']']
    assert '# Validation PASS' in run(common + ['-dataTable', *tokens, '-validate_only'])
    assert '# SpatialUnits\t4' in run(common + ['-dataTable', '@'+str(direct), '-searchlight', 'SPHERE(1.1)', '-validate_only'])
    assert '# SelectedFeatures\t2' in run(common + ['-trial_manifest', manifest, '-roi_sel', '1', '-validate_only'])

    xmat = root/'X.xmat.1D'
    xmat.write_text('# <matrix\n# ni_type = "6*double"\n# ni_dimen = "6"\n'
                    '# ColumnLabels = "blocks#0 ; blocks#1 ; blocks#2 ; blocks#3 ; blocks#4 ; blocks#5"\n'
                    '# RunStart = "0"\n# StimBots = "0"\n# StimTops = "5"\n'
                    '# StimLabels = "blocks"\n# >\n' +
                    '\n'.join(' '.join(str(int(i == j)) for j in range(6)) for i in range(6)) + '\n# </matrix>\n')
    provenance = table('provenance.1D', [dict(r, Xmat=str(xmat), XColumn=r['Volume']) for r in rows])
    assert '# Validation PASS' in run(common + ['-trial_manifest', provenance, '-validate_only'])
    bad_column = table('bad_column.1D', [dict(r, Xmat=str(xmat), XColumn='99') for r in rows])
    run(common + ['-trial_manifest', bad_column, '-validate_only'], "XColumn '99'")
    unpaired = table('unpaired.1D', [dict(r, Xmat=str(xmat)) for r in rows])
    run(common + ['-trial_manifest', unpaired, '-validate_only'], 'must occur together')

    # Finite numeric metadata and actual grouped inner-fold validation.
    numeric = ['-cv_column', 'Run', '-mask', root/'mask.nii', '-trial_manifest', manifest, '-validate_only']
    assert '# FiniteNumericColumns\t1' in run(['-mode', 'decode', '-target_column', 'Target', *numeric])
    assert '# FiniteNumericColumns\t1' in run(['-mode', 'encode', '-design_columns', 'Target', *numeric])
    assert '# FiniteNumericColumns\t2' in run(['-mode', 'decode', '-target_columns', 'Volume,target', *numeric])
    assert '# FiniteNumericColumns\t2' in run(['-mode', 'encode', '-design_columns', 'Volume,target', *numeric])
    for option, mode in (('-target_columns', 'decode'), ('-design_columns', 'encode')):
        for columns, message in [('Target,target', 'duplicate numeric column'),
                                 ('Missing', 'finite numeric'), ('Condition', 'finite numeric'),
                                 ('Target,', 'is empty'), (',Target', 'is empty'),
                                 ('Target,,Volume', 'is empty')]:
            run(['-mode', mode, option, columns, *numeric], message)
    run(common + ['-trial_manifest', manifest, '-method', 'ridge', '-lambda', 'auto', '-validate_only'])
    run(common + ['-trial_manifest', manifest, '-method', 'svm', '-svm_c', 'auto', '-validate_only'])
    run(common + ['-trial_manifest', manifest, '-scale', 'run_center', '-validate_only'], 'absent from training')
    short = table('short.1D', rows[:4])
    run(common + ['-trial_manifest', short, '-method', 'lda_diag', '-validate_only'], 'more training rows than classes')
    run(common + ['-trial_manifest', manifest, '-nperm', '10', '-permute_within', 'Missing', '-validate_only'], "column 'Missing' is missing")

    def bad(name, records, message):
        run(common + ['-trial_manifest', table(name, records), '-validate_only'], message)

    bad('bad_volume.1D', [dict(r, Volume='99') if i == 0 else r for i, r in enumerate(rows)], 'zero-based integer')
    bad('duplicate_id.1D', [dict(r, Sample='same') for r in rows], 'duplicate sample label')
    bad('missing_file.1D', [dict(r, InputFile=str(root/'absent.nii')) for r in rows], "Can't open dataset")
    bad('missing_class.1D', [dict(r, Condition='A' if r['Run'] == '1' else 'B') for r in rows], 'training set')
    bad('missing_target.1D', [{k:v for k,v in r.items() if k != 'Condition'} for r in rows], "column 'Condition' is missing")
    nifti(root/'wrong_grid.nii', [[1]*10], nx=5)
    bad('wrong_grid.1D', [dict(r, InputFile=str(root/'wrong_grid.nii'), Volume='0') if i == 0 else r for i,r in enumerate(rows)], 'not on the grid')
    bad_numeric = table('bad_numeric.1D', [dict(r, Target='nan') for r in rows])
    run(['-mode', 'decode', '-target_column', 'Target', '-cv_column', 'Run', '-mask', root/'mask.nii',
         '-trial_manifest', bad_numeric, '-validate_only'], 'not finite')
    for value in ('inf', '1e100'):
        nonfinite = table('nonfinite_'+value+'.1D', [dict(r, Target=value) if i == 2 else r
                                                   for i, r in enumerate(rows)])
        run(['-mode', 'encode', '-design_columns', 'Target', '-cv_column', 'Run', '-mask', root/'mask.nii',
             '-trial_manifest', nonfinite, '-validate_only'], 'output row 3 (table row 3) is not finite')
    # Each outer training set has both classes, but an inner training set does not.
    inner = table('inner.1D', [dict(r, Condition='A' if r['Run'] == '1' else 'B') if r['Run'] != '3' else r for r in rows])
    run(common + ['-trial_manifest', inner, '-method', 'ridge', '-lambda', 'auto', '-validate_only'], 'inner folds of outer')

    # A repeated selection is visible even when spelled via a symlink.
    (root/'alias.nii').symlink_to(root/'betas.nii')
    duplicate = table('duplicate_beta.1D', [dict(r, InputFile=str(root/'alias.nii'), Volume='0') if i == 2 else r for i,r in enumerate(rows)])
    duplicate_report = run(common + ['-trial_manifest', duplicate, '-validate_only'])
    assert 'repeated beta selection in rows 1 (s0) and 3 (s2)' in duplicate_report
    assert '# Validation PASS\t1 warning(s)' in duplicate_report
    # Multiple repeats each name the earliest representative, once per row.
    triple = table('triple_beta.1D', [dict(r, InputFile=str(root/'alias.nii'), Volume='0')
                                   if i in (2, 4) else r for i, r in enumerate(rows)])
    triple_report = run(common + ['-trial_manifest', triple, '-validate_only'])
    assert 'repeated beta selection in rows 1 (s0) and 3 (s2)' in triple_report
    assert 'repeated beta selection in rows 1 (s0) and 5 (s4)' in triple_report
    assert triple_report.count('WARNING: repeated beta selection') == 2
    assert '# Validation PASS\t2 warning(s)' in triple_report
    # Missing test classes are reported, not rejected when training coverage is valid.
    sparse_test = table('sparse_test.1D', [dict(r, Condition='A') if r['Run'] == '1' else r for r in rows])
    assert 'has no test observations of class B' in run(common + ['-trial_manifest', sparse_test, '-validate_only'])

    # NIfTI readers can repair raw NaNs. Overflow on applying the brick factor
    # tests the actual values passed to modeling, after loading/conversion.
    nifti(root/'overflow.nii', [[2e38, 0, 0, 0, 0, 0, 0, 0]])
    data = bytearray((root/'overflow.nii').read_bytes())
    struct.pack_into('<f', data, 112, 2.)
    (root/'overflow.nii').write_bytes(data)
    bad('nonfinite.1D', [dict(r, InputFile=str(root/'overflow.nii'), Volume='0') if i == 0 else r for i,r in enumerate(rows)], 'is not finite')
    # Nonfinite features outside the selected ROI do not invalidate that analysis.
    outside = table('outside.1D', [dict(r, InputFile=str(root/'overflow.nii'), Volume='0') if i == 0 else r for i,r in enumerate(rows)])
    run(common + ['-trial_manifest', outside, '-roi_sel', '2', '-validate_only'])

    # Normal fitting still writes results; both input adapters yield identical predictions.
    for source, option, prefix in ((direct, '-dataTableFile', 'direct'), (manifest, '-trial_manifest', 'manifest')):
        run(common + [option, source, '-prefix', prefix, '-save_predictions', prefix+'.pred.1D'])
        assert (root/(prefix+'.roi.1D')).exists()
    def predictions(name):
        with (root/name).open() as f:
            return list(csv.DictReader((s[11:] if s.startswith('# Columns: ') else s for s in f if not s.startswith('#') or s.startswith('# Columns: ')), delimiter='\t'))
    assert predictions('direct.pred.1D') == predictions('manifest.pred.1D')
print('PASS: 3dMVPA validation-only input, fold, data, no-output and fitting regression checks')
