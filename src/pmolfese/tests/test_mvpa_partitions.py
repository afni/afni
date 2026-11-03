#!/usr/bin/env python3
"""Fixed-split CLI contracts and independent training-only oracles; stdlib only."""
import csv
import itertools
import math
import os
from pathlib import Path
import struct
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())
env = dict(os.environ, OMP_NUM_THREADS='2', AFNI_FLOATSCAN='NO')


def nifti(path, volumes):
    header = bytearray(352)
    struct.pack_into('<i', header, 0, 348)
    struct.pack_into('<8h', header, 40, 4, 4, 2, 1, len(volumes), 1, 1, 1)
    struct.pack_into('<2h', header, 70, 16, 32)
    struct.pack_into('<8f', header, 76, 1, 1, 1, 1, 1, 1, 1, 1)
    struct.pack_into('<f', header, 108, 352)
    struct.pack_into('<h', header, 254, 1)
    for i in range(3):
        row = [0.] * 4
        row[i] = 1.
        struct.pack_into('<4f', header, 280 + 16*i, *row)
    header[344:348] = b'n+1\0'
    values = [v for volume in volumes for v in volume]
    assert len(values) == 8 * len(volumes)
    path.write_bytes(header + struct.pack('<%df' % len(values), *values))


def read(path):
    with path.open() as f:
        return list(csv.DictReader((s[11:] if s.startswith('# Columns: ') else s
                    for s in f if not s.startswith('#') or s.startswith('# Columns: ')), delimiter='\t'))


def near(actual, expected):
    assert math.isclose(float(actual), expected, rel_tol=2e-5, abs_tol=2e-5), (actual, expected)


def ridge(train_x, train_y, test_x, penalty=1.):
    """Small independent normal-equation solve, centered with training scaling."""
    n, p = len(train_x), len(train_x[0])
    mean = [sum(r[j] for r in train_x)/n for j in range(p)]
    sd = [math.sqrt(sum((r[j]-mean[j])**2 for r in train_x)/n) or 1 for j in range(p)]
    x = [[(r[j]-mean[j])/sd[j] for j in range(p)] for r in train_x]
    ym = sum(train_y)/n
    a = [[sum(r[j]*r[k] for r in x) + (penalty if j == k else 0)
          for k in range(p)] + [sum(r[j]*(y-ym) for r, y in zip(x, train_y))]
         for j in range(p)]
    for j in range(p):
        pivot = a[j][j]
        a[j] = [v/pivot for v in a[j]]
        for k in range(p):
            if k != j:
                factor = a[k][j]
                a[k] = [v-factor*w for v, w in zip(a[k], a[j])]
    return [ym + sum((r[j]-mean[j])/sd[j]*a[j][-1] for j in range(p)) for r in test_x], ym


with tempfile.TemporaryDirectory(prefix='mvpa_partitions_') as folder:
    root = Path(folder)
    # Test rows come first and start with B. Training order must still define A first.
    xy = [(-2, 0), (2, 0), (0, 0), (4, 3), (-3, 1), (3, -1),
          (-2, -1), (2, 1), (-1, 2), (1, -2)]
    labels = list('BAABABABAB')
    nifti(root/'mask.nii', [[1, 1, 2, 2, 0, 0, 0, 0]])
    volumes = [[x, y, 5, 5, 0, 0, 0, 0] for x, y in xy]
    nifti(root/'betas.nii', volumes)
    rows = [dict(Subj='heldout' if i < 4 else 'training',
                 Run='test' if i < 4 else f'r{(i-4)//2}', Sample=f's{i}',
                 Split='test' if i < 4 else 'train', Condition=labels[i],
                 Target=str(7+2*x-.5*y), X=str(x), Volume=str(i),
                 InputFile=str(root/'betas.nii')) for i, (x, y) in enumerate(xy)]

    def table(name, records):
        path = root/name
        with path.open('w') as f:
            w = csv.DictWriter(f, list(records[0]), delimiter='\t')
            w.writeheader(); w.writerows(records)
        return path

    manifest = table('manifest.1D', rows)
    direct = table('direct.1D', [dict(r, InputFile=r['InputFile']+'['+r['Volume']+']') for r in rows])
    common = ['-trial_manifest', manifest, '-mask', root/'mask.nii', '-partition_column', 'Split']

    def run(args, failure=None, workers=2):
        result = subprocess.run([binary, *map(str, args)], cwd=root,
                                env=dict(env, OMP_NUM_THREADS=str(workers)),
                                capture_output=True, text=True, timeout=60)
        output = result.stdout + result.stderr
        if failure is None:
            assert result.returncode == 0, output
        else:
            assert result.returncode != 0 and failure in output, output
        return output

    def analyze(name, mode='classify', extra=(), source=None, workers=2, input_args=None):
        args = common.copy() if input_args is None else list(input_args) + ['-mask', root/'mask.nii']
        if source is not None:
            args[1] = source
        args += ['-mode', mode, '-prefix', name, '-save_predictions', name+'.pred.1D',
                 '-save_fold_scores', name+'.fold.1D']
        if mode == 'classify':
            args += ['-target_column', 'Condition', '-save_confusion', name+'.conf.1D',
                     '-save_summary_scores', name+'.summary.1D']
        elif mode == 'decode':
            args += ['-target_column', 'Target']
        else:
            args += ['-design_columns', 'X']
        run(args + list(extra), workers=workers)
        predictions = read(root/(name+'.pred.1D'))
        for r in predictions:
            if int(r['Sample'][1:]) >= 4:
                assert r['Predicted'] == 'NA' and r['CVFold'] == 'NA', r
                if mode == 'classify':
                    assert r['Correct'] == r['DecisionClass0'] == r['DecisionClass1'] == 'NA', r
                else:
                    assert r['TrainingMean'] == 'NA', r
            else:
                assert r['CVFold'] == 'test' and r['Predicted'] != 'NA', r
        for r in read(root/(name+'.fold.1D')):
            assert int(r['NTrain']) == 6 and int(r['NTest']) == 4, r
            if mode != 'classify':
                held = [p for p in predictions if p['Unit'] == r['Unit'] and
                        p['Response'] == r['Response'] and p['Predicted'] != 'NA']
                sse = sum((float(p['Observed'])-float(p['Predicted']))**2 for p in held)
                base = sum((float(p['Observed'])-float(p['TrainingMean']))**2 for p in held)
                near(r['RMSE'], math.sqrt(sse/4))
                if base:
                    near(r['CVR2'], 1-sse/base)
        if mode == 'classify':
            for r in read(root/(name+'.summary.1D')):
                if r['Scope'] == 'Subject' and r['Group'] == 'training':
                    assert r['NTest'] == '0' and r['ACC'] == r['BACC'] == 'NA', r
                elif r['Scope'] == 'Pooled':
                    tested = [p for p in predictions if p['UnitID'] == r['UnitID'] and p['Predicted'] != 'NA']
                    near(r['ACC'], sum(p['Observed'] == p['Predicted'] for p in tested)/4)
                    assert r['NTest'] == '4', r
        return predictions

    before = set(root.iterdir())
    report = run(common + ['-mode', 'classify', '-target_column', 'Condition', '-validate_only',
                          '-method', 'ridge', '-lambda', 'auto', '-inner_cv_column', 'Run'])
    assert set(root.iterdir()) == before
    assert '# Folds\t1' in report and 'test\tAll\t6\t4' in report
    inner_report = report.split('OuterFold\tInnerFold\tStratum\tSet\tRow\tSample\n')[1].split('\n#')[0]
    for line in inner_report.strip().splitlines():
        assert int(line.split('\t')[-1][1:]) >= 4, line

    # Independent centroid decisions use training-only population scaling.
    pred = analyze('centroid')
    for r in pred:
        if r['Predicted'] == 'NA' or r['UnitID'] != '1':
            continue
        i = int(r['Sample'][1:])
        expected = []
        for label in 'AB':
            value = 0.
            for j in range(2):
                mean = sum(xy[k][j] for k in range(4, 10))/6
                var = sum((xy[k][j]-mean)**2 for k in range(4, 10))/6
                cent = sum(xy[k][j] for k in range(4, 10) if labels[k] == label)/3
                value -= (xy[i][j]-cent)**2/var
            expected.append(value)
        for c in range(2):
            near(r[f'DecisionClass{c}'], expected[c])
        assert r['Predicted'] == 'AB'[int(expected[1] > expected[0])]
    assert all(r['Predicted'] == 'A' for r in pred if r['Sample'] == 's2')

    # Perturb one test pattern and all test outcomes; other test predictions and
    # every tuning candidate loss must remain unchanged, for all classifiers.
    changed = [v[:] for v in volumes]
    changed[0][:2] = [1000, -500]
    nifti(root/'changed.nii', changed)
    perturbed = table('perturbed.1D', [dict(r, InputFile=str(root/'changed.nii'),
                      Condition='AB'[i % 2], Target=str(10000+i), X='900') if i < 4
                      else r for i, r in enumerate(rows)])
    configs = [('centroid', []), ('ridge', ['-method', 'ridge']),
               ('lda_diag', ['-method', 'lda_diag']), ('lda_shrinkage', ['-method', 'lda_shrinkage']),
               ('svm', ['-method', 'svm']),
               ('tuned_ridge', ['-method', 'ridge', '-lambda_grid', '.1,1,10', '-inner_cv_column', 'Run']),
               ('tuned_svm', ['-method', 'svm', '-svm_c_grid', '.1,1,10', '-inner_cv_column', 'Run'])]
    for name, options in configs:
        original = pred if name == 'centroid' else analyze(name, extra=options)
        altered = analyze(name+'_changed', extra=options, source=perturbed)
        for a, b in zip(original, altered):
            if a['Sample'] != 's0':
                for key in ['Predicted', 'DecisionClass0', 'DecisionClass1']:
                    assert a[key] == b[key], (name, a, b)
        if name.startswith('tuned'):
            assert (root/(name+'.tuning.1D')).read_bytes() == (root/(name+'_changed.tuning.1D')).read_bytes()

    for mode, options in [('decode', []), ('encode', ['-score', 'pattern']), ('encode', ['-score', 'voxel'])]:
        name = mode + ('_'+options[-1] if options else '')
        original = analyze(name, mode, options)
        # Analytic ridge predictions and training-mean baselines, all responses.
        for r in original:
            if r['Predicted'] == 'NA':
                continue
            i = int(r['Sample'][1:])
            if mode == 'decode':
                tx = [xy[k] if r['Unit'] == '1' else (5, 5) for k in range(4, 10)]
                test = [xy[i] if r['Unit'] == '1' else (5, 5)]
                ty = [float(rows[k]['Target']) for k in range(4, 10)]
            else:
                voxel = int(r['Response'].split('_')[1])
                tx = [[float(rows[k]['X'])] for k in range(4, 10)]
                test = [[float(rows[i]['X'])]]
                ty = [volumes[k][voxel] for k in range(4, 10)]
            expected, baseline = ridge(tx, ty, test)
            near(r['Predicted'], expected[0]); near(r['TrainingMean'], baseline)
        tune = options + ['-lambda_grid', '.1,1,10', '-inner_cv_column', 'Run', '-inner_cv_k', '2']
        a = analyze(name+'_tuned', mode, tune)
        # Keep test predictors unchanged for encoding; change test responses only.
        continuous_rows = [dict(r, InputFile=str(root/'changed.nii'), Target=str(10000+i)) if i < 4 else r
                           for i, r in enumerate(rows)]
        cp = table(name+'_changed.1D', continuous_rows)
        b = analyze(name+'_tuned_changed', mode, tune, source=cp)
        assert (root/(name+'_tuned.tuning.1D')).read_bytes() == (root/(name+'_tuned_changed.tuning.1D')).read_bytes()
        for ra, rb in zip(a, b):
            if mode == 'encode' or ra['Sample'] != 's0':
                assert ra['Predicted'] == rb['Predicted'] and ra['TrainingMean'] == rb['TrainingMean']

    # Input adapters, inline option termination, and customized labels.
    class_args = ['-mode', 'classify', '-target_column', 'Condition', '-mask', root/'mask.nii']
    tokens = direct.read_text().split()
    for extra in [['-dataTableFile', direct], ['-dataTable', '@'+str(direct)], ['-dataTable', *tokens]]:
        run(class_args + extra + ['-partition_column', 'Split', '-validate_only'])
    custom = table('custom.1D', [dict(r, Split='FIT' if r['Split'] == 'train' else 'EVAL') for r in rows])
    run(class_args + ['-trial_manifest', custom, '-partition_column', 'Split', '-train_value', 'FIT',
                     '-test_value', 'EVAL', '-validate_only'])

    # Spatial runners and permutations retain observed audits, including NA rows.
    for mode in ['classify', 'decode', 'encode']:
        options = ['-searchlight', 'SPHERE(1.1)'] + (['-score', 'pattern'] if mode == 'encode' else [])
        a = analyze(mode+'_spatial', mode, options, workers=1)
        b = analyze(mode+'_spatial_perm', mode, options + ['-nperm', '5', '-permute_within', 'Run'], workers=2)
        assert a == b, mode
        assert (root/(mode+'_spatial.fold.1D')).read_bytes() == (root/(mode+'_spatial_perm.fold.1D')).read_bytes()

    # Exhaustive binary null: 2^3 training label swaps times 6 test arrangements.
    # Independent centroid oracle also checks tie handling and test-only scoring.
    analyze('exact', extra=['-nperm', '999', '-permute_within', 'Run'])
    def accuracy(truth):
        correct = 0
        for i in range(4):
            ds = []
            for label in 'AB':
                distance = 0.
                for j in range(2):
                    var = sum(xy[k][j]**2 for k in range(4, 10))/6
                    cent = sum(xy[k][j] for k in range(4, 10) if truth[k] == label)/3
                    distance += (xy[i][j]-cent)**2/var
                ds.append(distance)
            correct += 'AB'[int(ds[1] < ds[0])] == truth[i]
        return correct/4
    observed = accuracy(labels)
    null = [accuracy(list(test) + list(itertools.chain.from_iterable(train)))
            for test in sorted(set(itertools.permutations(labels[:4])))
            for train in itertools.product(*[[(labels[k], labels[k+1]), (labels[k+1], labels[k])]
                                             for k in [4, 6, 8]])]
    roi = next(r for r in read(root/'exact.roi.1D') if r['ROI'] == '1')
    near(roi['PVAL'], sum(s >= observed for s in null)/len(null))

    # Incompatible options/data fail before producing any analysis outputs.
    def bad(extra, message, base=None):
        before = set(root.iterdir())
        run((common + class_args if base is None else base) + list(extra), message)
        assert set(root.iterdir()) == before

    for extra in [['-cv', 'run'], ['-cv_column', 'Run'], ['-cv_k', '2'], ['-within_column', 'Subj']]:
        bad(extra, 'cannot accompany')
    for option in ['-train_value', '-test_value']:
        bad([option, 'x'], 'require -partition_column', class_args + ['-trial_manifest', manifest, '-cv', 'run'])
    bad(['-train_value', 'test'], 'distinct train/test')
    bad(['-test_value', ''], 'must be nonempty')
    bad(['-partition_column', 'Split'], 'specified twice')
    bad(['-train_value', 'train', '-train_value', 'train'], 'specified twice')
    bad(['-method', 'ridge', '-lambda', 'auto'], 'explicit -inner_cv_column')
    bad(['-method', 'svm', '-svm_c', 'auto'], 'explicit -inner_cv_column')
    bad(['-method', 'ridge', '-lambda', 'auto', '-inner_cv_column', 'Split'], '1 inner groups')
    bad(['-method', 'ridge', '-save_coefficients', 'forbidden'], 'refits all rows')
    for validate in [[], ['-validate_only']]:
        bad(['-nperm', '2', '-permute_within', 'none', *validate], 'crosses the split')
        bad(['-nperm', '2', '-permute_within', 'Condition', *validate], 'crosses the train/test')
    for name, records, error in [
        ('unknown', [dict(r, Split='unused') if i == 0 else r for i, r in enumerate(rows)], 'accepts only'),
        ('no_test', [dict(r, Split='train') for r in rows], '0 test samples'),
        ('no_train', [dict(r, Split='test') for r in rows], '0 training'),
        ('test_class', [dict(r, Condition='C') if i == 0 else r for i, r in enumerate(rows)], 'training set'),
        ('missing', [{k:v for k,v in r.items() if k != 'Split'} for r in rows], "column 'Split' is missing"),
    ]:
        path = table(name+'.1D', records)
        bad(['-validate_only'], error, class_args + ['-trial_manifest', path, '-partition_column', 'Split'])
    bad(['-scale', 'run_center', '-validate_only'], 'absent from training')
    inner_bad = table('inner_missing_class.1D', [dict(r, Condition='A' if r['Run'] == 'r0' else 'B')
                                              if i >= 4 else r for i, r in enumerate(rows)])
    bad(['-method', 'ridge', '-lambda', 'auto', '-inner_cv_column', 'Run', '-validate_only'],
        'inner folds of outer', class_args + ['-trial_manifest', inner_bad, '-partition_column', 'Split'])

    # Separate tables must reproduce a single fixed partition in every mode,
    # including tuning and null inference. Test schema order/case may differ.
    direct_rows = [dict(r, InputFile=r['InputFile']+'['+r['Volume']+']') for r in rows]
    train_rows, test_rows = direct_rows[4:], direct_rows[:4]
    train_file = table('training data.1D', train_rows)
    test_file = table('test data.1D', [{k.lower():v for k, v in reversed(list(r.items()))} for r in test_rows])
    inputs = ['-dataTableFile', train_file, '-testTable', '@'+str(test_file)]
    def keyed(predictions):
        return {(r.get('UnitID', r.get('Unit')), r.get('Response'), r['Sample']): r for r in predictions}

    dual_configs = [('classify', name, opts) for name, opts in configs]
    dual_configs += [('decode', 'decode_tuned', ['-lambda_grid', '.1,1,10', '-inner_cv_column', 'Run', '-inner_cv_k', '2']),
                     ('encode', 'encode_pattern_tuned', ['-score', 'pattern', '-lambda_grid', '.1,1,10', '-inner_cv_column', 'Run', '-inner_cv_k', '2']),
                     ('encode', 'encode_voxel_tuned', ['-score', 'voxel', '-lambda_grid', '.1,1,10', '-inner_cv_column', 'Run', '-inner_cv_k', '2']),
                     ('classify', 'exact', ['-nperm', '999', '-permute_within', 'Run'])]
    for mode, name, options in dual_configs:
        dual = analyze('dual_'+name, mode, options, input_args=inputs)
        single = keyed(read(root/(name+'.pred.1D')))
        assert keyed(dual).keys() == single.keys()
        for r in dual:
            key = (r.get('UnitID', r.get('Unit')), r.get('Response'), r['Sample'])
            for col, value in single[key].items():
                if col != 'TableRow':
                    assert r[col] == value, (name, col, r, single[key])
            i = int(r['Sample'][1:])
            assert r['Partition'] == ('train' if i >= 4 else 'test')
            assert r['SourceTable'] == str(train_file if i >= 4 else test_file)
            assert int(r['SourceRow']) == (i-3 if i >= 4 else i+1)
        for suffix in ['.fold.1D', '.tuning.1D', '.roi.1D', '.continuous.1D']:
            if (root/(name+suffix)).exists():
                assert (root/(name+suffix)).read_bytes() == (root/('dual_'+name+suffix)).read_bytes(), (name, suffix)

    # Test-outcome/pattern perturbation cannot alter tuning or other predictions.
    changed_rows = [dict(r, InputFile=str(root/'changed.nii')+'['+r['Volume']+']',
                         Condition='AB'[i % 2], Target=str(10000+i)) for i, r in enumerate(test_rows)]
    changed_file = table('dual_changed.1D', changed_rows)
    for mode, name, opts in [c for c in dual_configs if 'tuned' in c[1]]:
        altered = analyze('dual_changed_'+name, mode, opts,
                          input_args=['-dataTableFile', train_file, '-testTableFile', changed_file])
        original = keyed(read(root/('dual_'+name+'.pred.1D')))
        for key, r in keyed(altered).items():
            if mode == 'encode' or r['Sample'] != 's0':
                assert r['Predicted'] == original[key]['Predicted'], (mode, name, r)
        assert (root/('dual_'+name+'.tuning.1D')).read_bytes() == (root/('dual_changed_'+name+'.tuning.1D')).read_bytes()

    before = set(root.iterdir())
    report = run(class_args + inputs + ['-validate_only'])
    assert set(root.iterdir()) == before
    assert '# Two-table sample provenance' in report and 'test\tAll\t6\t4' in report
    assert str(train_file) in report and str(test_file) in report
    # Repeats confined to one side are warnings, not cross-table overlap.
    repeat_train = table('repeat_train.1D', [dict(r, InputFile=train_rows[0]['InputFile'])
                                          if i == 2 else r for i, r in enumerate(train_rows)])
    repeat_test = table('repeat_test.1D', [dict(r, InputFile=test_rows[1]['InputFile'])
                                        if i == 2 else r for i, r in enumerate(test_rows)])
    repeat_inputs = ['-dataTableFile', repeat_train, '-testTableFile', repeat_test]
    repeat_report = run(class_args + repeat_inputs + ['-validate_only'])
    assert 'repeated beta selection in rows 1 (s4) and 3 (s6)' in repeat_report
    assert 'repeated beta selection in rows 8 (s1) and 9 (s2)' in repeat_report
    assert '# Validation PASS\t2 warning(s)' in repeat_report
    run(class_args + repeat_inputs + ['-prefix', 'within_side_repeats'])
    # Inline tables on either side stop at the next input option.
    def inline(records):
        keys = [k for k in records[0] if k != 'InputFile'] + ['InputFile']
        return keys + [r[k] for r in records for k in keys]
    run(class_args + ['-dataTable', *inline(train_rows), '-testTable', *inline(test_rows), '-validate_only'])
    run(class_args + ['-testTable', *inline(test_rows), '-dataTable', '@'+str(train_file), '-validate_only'])
    # No explicit sample column: generated sample IDs remain unique globally.
    noid_train = table('noid_train.1D', [{k:v for k,v in r.items() if k != 'Sample'} for r in train_rows])
    noid_test = table('noid_test.1D', [{k:v for k,v in r.items() if k != 'Sample'} for r in test_rows])
    run(class_args + ['-dataTableFile', noid_train, '-testTableFile', noid_test,
                     '-prefix', 'noid', '-save_predictions', 'noid.pred.1D'])
    noid = read(root/'noid.pred.1D')
    assert len({r['Sample'] for r in noid}) == 10
    assert all(r['Predicted'] == 'NA' for r in noid if r['Partition'] == 'train')

    def dual_bad(extra, message, replacement=None):
        args = inputs if replacement is None else ['-dataTableFile', train_file, '-testTableFile', replacement]
        bad(extra, message, class_args + args)
    for extra in [['-partition_column', 'Split'], ['-train_value', 'train'], ['-test_value', 'test'],
                  ['-cv', 'run'], ['-cv_column', 'Run'], ['-cv_k', '2'], ['-within_column', 'Subj']]:
        dual_bad(extra, 'cannot accompany')
    dual_bad(['-testTableFile', test_file], 'test table was given twice')
    dual_bad(['-method', 'ridge', '-lambda', 'auto'], 'explicit -inner_cv_column')
    dual_bad(['-method', 'ridge', '-save_coefficients', 'no_coeff'], 'refits all rows')
    bad(['-testTableFile', test_file], 'cannot accompany', class_args + ['-trial_manifest', manifest])
    bad(['-testTableFile', test_file], '-dataTable or -dataTableFile is required', class_args)
    for name, records, error in [
        ('schema_missing', [{k:v for k,v in r.items() if k != 'Target'} for r in test_rows], 'same column names'),
        ('schema_rename', [{('Other' if k == 'Condition' else k):v for k,v in r.items()} for r in test_rows], "missing column 'Condition'"),
        ('reserved', [dict(r, MVPA_SourceRow='1') for r in test_rows], 'reserved'),
        ('sample_overlap', [dict(r, Sample='s4') if i == 0 else r for i,r in enumerate(test_rows)], 'duplicate sample label'),
        ('unknown_class', [dict(r, Condition='C') if i == 0 else r for i,r in enumerate(test_rows)], 'training set'),
        ('beta_overlap', [dict(r, InputFile=train_rows[0]['InputFile']) if i == 0 else r for i,r in enumerate(test_rows)], 'same dataset/brick'),
    ]:
        invalid = table('dual_'+name+'.1D', records)
        dual_bad(['-validate_only'], error, invalid)
    (root/'alias_betas.nii').symlink_to(root/'betas.nii')
    alias = table('alias_test.1D', [dict(r, InputFile=str(root/'alias_betas.nii')+'[4]') if i == 0 else r
                                  for i, r in enumerate(test_rows)])
    for extra in [[], ['-validate_only']]:
        dual_bad(extra, 'same dataset/brick', alias)
    wrong = bytearray((root/'betas.nii').read_bytes())
    struct.pack_into('<f', wrong, 80, 2.)
    struct.pack_into('<f', wrong, 280, 2.)
    (root/'wrong_grid.nii').write_bytes(wrong)
    wrong_table = table('dual_grid.1D', [dict(r, InputFile=str(root/'wrong_grid.nii')+'['+r['Volume']+']') for r in test_rows])
    dual_bad(['-validate_only'], 'not on the grid', wrong_table)

print('PASS: single/two-table fixed partitions, training-only oracles/tuning, provenance, spatial nulls and rejection contracts')
