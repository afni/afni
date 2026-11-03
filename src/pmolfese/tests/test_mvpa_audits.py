#!/usr/bin/env python3
"""Independent reconstruction of descriptive classification audits (stdlib only)."""
import csv
import math
import os
from pathlib import Path
import subprocess
import sys
import tempfile

mvpa, undump = sys.argv[1:]
env = dict(os.environ, AFNI_DECONFLICT='OVERWRITE', OMP_NUM_THREADS='2')


def run(args, fail=None):
    result = subprocess.run([str(x) for x in args], env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if fail is None:
        assert result.returncode == 0, result.stdout
    else:
        assert result.returncode != 0 and fail in result.stdout, result.stdout
    return result


def read(path):
    with open(path) as f:
        return list(csv.DictReader((line[11:] if line.startswith('# Columns: ') else line for line in f if not line.startswith('#') or line.startswith('# Columns: ')), delimiter='\t'))


def near(actual, expected):
    if expected is None:
        assert actual == 'NA', actual
    else:
        assert math.isclose(float(actual), expected, rel_tol=1e-12, abs_tol=1e-12), (actual, expected)


def verify(prefix):
    predictions = read(str(prefix) + '.pred.1D')
    summaries = read(str(prefix) + '.summary.1D')
    confusion = read(str(prefix) + '.conf.1D')
    labels = {}
    with open(str(prefix) + '.conf.1D') as f:
        for line in f:
            if line.startswith('# Class\t'):
                _, idx, label = line.rstrip('\n').split('\t')
                labels[int(idx)] = label
    assert len(labels) == 3
    by_unit = {}
    for row in predictions:
        by_unit.setdefault(row['UnitID'], []).append(row)
    # A display name may collide, but fold indexes and raw strata must not.
    for table in [summaries, confusion]:
        identities = {}
        for row in table:
            if row['Scope'] == 'Fold':
                key = (row['UnitID'], int(row['CVFoldID']))
                pair = (row['Group'], row['CVStratum'])
                assert identities.setdefault(key, pair) == pair
            else:
                assert row['CVFoldID'] == 'NA'
                assert row['CVStratum'] == (row['Group'] if row['Scope'] == 'Stratum' else 'NA')
        for unit in by_unit:
            ids = sorted(k[1] for k in identities if k[0] == unit)
            assert ids == list(range(len(ids)))
    expected_summary, expected_conf = set(), set()
    for unit, rows in by_unit.items():
        grouped = {('Pooled', 'All', 'NA'): rows}
        for scope, col in [('Fold', 'CVFold'), ('Subject', 'Subj'), ('Stratum', 'CVStratum')]:
            for label, stratum in dict.fromkeys((r[col], r['CVStratum'] if scope == 'Fold' else
                                                  r[col] if scope == 'Stratum' else 'NA') for r in rows):
                grouped[scope, label, stratum] = [r for r in rows if r[col] == label and
                                                  (scope != 'Fold' or r['CVStratum'] == stratum)]
        metrics = {}
        for (scope, group, stratum), subset in grouped.items():
            tested = [r for r in subset if r['Predicted'] != 'NA']
            matrix = [[sum(r['Observed'] == labels[t] and r['Predicted'] == labels[p]
                           for r in tested) for p in labels] for t in labels]
            total = len(tested)
            correct = sum(matrix[c][c] for c in labels)
            recall = [matrix[c][c] / sum(matrix[c]) if sum(matrix[c]) else None for c in labels]
            present = [r for r in recall if r is not None]
            acc, bacc = correct / total if total else None, sum(present) / len(present) if present else None
            metrics[scope, group, stratum] = (total, acc, bacc)
            matches = [r for r in summaries if (r['UnitID'], r['Scope'], r['Group'], r['CVStratum']) == (unit, scope, group, stratum)]
            assert len(matches) == 1, (unit, scope, group, matches)
            s = matches[0]
            assert (int(s['NTest']), int(s['NCorrect']), int(s['NClassPresent'])) == (total, correct, len(present))
            near(s['ACC'], acc); near(s['BACC'], bacc)
            expected_summary.add((unit, scope, group, stratum))
            for c in labels:
                matches = [r for r in confusion if (r['UnitID'], r['Scope'], r['Group'], r['CVStratum'], int(r['TrueClassID'])) == (unit, scope, group, stratum, c)]
                assert len(matches) == 1
                r = matches[0]
                assert r['TrueClass'] == labels[c] and int(r['NTrue']) == sum(matrix[c])
                assert [int(r[f'CountClass{p}']) for p in labels] == matrix[c]
                near(r['Recall'], recall[c])
                expected_conf.add((unit, scope, group, stratum, c))
        for scope, mean_scope in [('Subject', 'EqualSubject'), ('Stratum', 'EqualStratum')]:
            groups = [v for k, v in metrics.items() if k[0] == scope and v[0]]
            s, = [r for r in summaries if (r['UnitID'], r['Scope']) == (unit, mean_scope)]
            assert int(s['NTest']) == sum(v[0] for v in groups)
            assert int(s['NGroups']) == len(groups)
            assert s['NCorrect'] == s['NClassPresent'] == 'NA'
            near(s['ACC'], sum(v[1] for v in groups) / len(groups))
            near(s['BACC'], sum(v[2] for v in groups) / len(groups))
            expected_summary.add((unit, mean_scope, 'All', 'NA'))
    assert len(summaries) == len(expected_summary)
    assert len(confusion) == len(expected_conf)
    return summaries


with tempfile.TemporaryDirectory(prefix='thd_mvpa_audits.') as temp:
    work = Path(temp)
    for name, values in [('mask', [1, 1]), ('a', [0, 0]), ('b', [5, 5]), ('c', [10, 10])]:
        points = work / (name + '.txt')
        points.write_text(''.join(f'{i} 0 0 {v}\n' for i, v in enumerate(values)))
        run([undump, '-dimen', 2, 2, 1, '-ijk', '-datum', 'float', '-prefix', work / name, points])
    header = 'Subj Run Block Sample Condition InputFile\n'
    records = []
    # Unequal subject counts and a fold with only A; every training split has all classes.
    for subject, nruns in [('s01', 3), ('s02', 2)]:
        for r in range(1, nruns + 1):
            for label in 'ABC':
                dset = label.lower() if subject == 's01' else 'a'
                records.append(f'{subject} r{r} {subject}_r{r} {subject}_r{r}_{label} {label} {work / dset}+orig\n')
    records.append(f's02 r3 s02_r3 s02_r3_A A {work / "a"}+orig\n')
    table = work / 'samples.1D'
    table.write_text(header + ''.join(records))
    base = [mvpa, '-mode', 'classify', '-dataTableFile', table, '-target_column', 'Condition',
            '-cv_column', 'Run', '-within_column', 'Subj', '-mask', str(work / 'mask') + '+orig', '-quiet']

    def analyze(name, extra=(), args=None):
        prefix = work / name
        run((base if args is None else args) + ['-prefix', prefix, '-save_predictions', str(prefix) + '.pred.1D',
            '-save_confusion', str(prefix) + '.conf.1D', '-save_summary_scores', str(prefix) + '.summary.1D'] + list(extra))
        return prefix, verify(prefix)

    prefix, scores = analyze('centroid')
    pooled, = [s for s in scores if s['Scope'] == 'Pooled']
    equal, = [s for s in scores if s['Scope'] == 'EqualSubject']
    near(pooled['ACC'], 12/16); near(pooled['BACC'], (1 + 3/5 + 3/5)/3)
    near(equal['ACC'], (1 + 3/7)/2); near(equal['BACC'], (1 + 1/3)/2)
    # Each subject summary agrees with the independently fitted within-subject run.
    for subject in ['s01', 's02']:
        subtable = work / (subject + '.1D')
        subtable.write_text(header + ''.join(r for r in records if r.startswith(subject + ' ')))
        subbase = base.copy(); subbase[subbase.index('-dataTableFile') + 1] = subtable
        _, subscores = analyze(subject, args=subbase)
        own, = [s for s in scores if s['Scope'] == 'Subject' and s['Group'] == subject]
        independent, = [s for s in subscores if s['Scope'] == 'Pooled']
        assert (own['NTest'], own['ACC'], own['BACC']) == (independent['NTest'], independent['ACC'], independent['BACC'])
    for method in ['ridge', 'lda_diag', 'lda_shrinkage', 'svm']:
        analyze(method, ['-method', method])
    # Pooled model CV also reports subject subsets independently of the single All stratum.
    unstratified = base.copy()
    idx = unstratified.index('-within_column'); del unstratified[idx:idx+2]
    analyze('all_stratum', args=unstratified)
    # Audits must describe restored observed predictions after blocked null fitting.
    perm, _ = analyze('permuted', ['-nperm', '5', '-permute_within', 'Block', '-seed', '42'])
    for suffix in ['.conf.1D', '.summary.1D']:
        assert Path(str(prefix) + suffix).read_bytes() == Path(str(perm) + suffix).read_bytes()
    # Opting in cannot change the existing held-out result.
    run(base + ['-prefix', work / 'plain'])
    assert (work / 'plain.roi.1D').read_bytes() == Path(str(prefix) + '.roi.1D').read_bytes()
    # Each audit can be requested independently.
    for option, suffix in [('-save_confusion', '.conf.1D'), ('-save_summary_scores', '.summary.1D')]:
        output = work / ('only' + suffix)
        run(base + ['-prefix', work / ('only' + suffix + '.result'), option, output])
        assert output.read_bytes() == Path(str(prefix) + suffix).read_bytes()
    # Slash-containing labels produce identical display names for distinct folds.
    collision = work / 'fold_names.1D'
    collision.write_text('Subj Run Sample Condition InputFile\n' + ''.join(
        f'{subject} {run_name} sample{i}_{label} {label} {work / label.lower()}+orig\n'
        for i, (subject, run_name) in enumerate([
            ('s/x', 'r1'), ('s/x', 'r2'), ('s', 'x/r1'), ('s', 'x/r2')])
        for label in 'ABC'))
    collision_base = base.copy()
    collision_base[collision_base.index('-dataTableFile') + 1] = collision
    collision_prefix, collision_scores = analyze('fold_names', args=collision_base)
    fold_rows = [r for r in collision_scores if r['Scope'] == 'Fold']
    assert len(fold_rows) == 4 and len({r['Group'] for r in fold_rows}) == 2
    assert len({r['CVFoldID'] for r in fold_rows}) == 4
    assert len({(r['Group'], r['CVStratum']) for r in fold_rows}) == 4
    fold_conf = [r for r in read(str(collision_prefix) + '.conf.1D') if r['Scope'] == 'Fold']
    assert len({(r['CVFoldID'], r['TrueClassID']) for r in fold_conf}) == 12

    # Exact enumeration must accept nonascending, repeated labels in each block.
    unsorted = work / 'unsorted.1D'
    unsorted.write_text('Subj Run Sample Condition InputFile\n' + ''.join(
        f's1 r{run_id} item{run_id}_{j} {label} {work / label.lower()}+orig\n'
        for run_id, order in enumerate(['ABAC', 'CBAC']) for j, label in enumerate(order)))
    unsorted_base = base.copy()
    unsorted_base[unsorted_base.index('-dataTableFile') + 1] = unsorted
    for method in ['centroid', 'ridge', 'lda_diag', 'lda_shrinkage', 'svm']:
        ref = work / ('unsorted_' + method)
        perm = work / ('unsorted_perm_' + method)
        run(unsorted_base + ['-method', method, '-prefix', ref,
                            '-save_predictions', str(ref) + '.pred.1D'])
        run(unsorted_base + ['-method', method, '-prefix', perm, '-nperm', '999',
                            '-permute_within', 'Run', '-save_predictions', str(perm) + '.pred.1D'])
        assert Path(str(ref) + '.pred.1D').read_bytes() == Path(str(perm) + '.pred.1D').read_bytes()
        assert '# Label permutations: 144;' in Path(str(perm) + '.roi.1D').read_text()
        for row in read(str(perm) + '.roi.1D'):
            assert 0 < float(row['PVAL']) <= float(row['PFWE']) <= 1

    # A malformed X-matrix must fail in the adapter before any mode expands items.
    bad_xmat = work / 'overlap.xmat.1D'
    bad_xmat.write_text('# <matrix\n# ni_type = "2*double"\n# ni_dimen = "2"\n'
                        '# StimLabels = "A ; B"\n# StimBots = "0,0"\n# StimTops = "1,1"\n'
                        '# >\n1 0\n0 1\n# </matrix>\n')
    bad_table = work / 'overlap.1D'
    bad_table.write_text(f'Subj Xmat InputFile\ns1 {bad_xmat} {work / "a"}+orig\n')
    for mode, role, column in [('classify', '-target_column', 'Condition'),
                               ('decode', '-target_column', 'Y'), ('encode', '-design_columns', 'X')]:
        run([mvpa, '-mode', mode, '-dataTableFile', bad_table, role, column,
             '-cv_column', 'Item', '-mask', str(work / 'mask') + '+orig'], fail='ranges overlap')

    # Reject collisions before truncating the requested files, even with overwrite enabled.
    sentinel = work / 'sentinel.1D'; sentinel.write_text('keep me\n')
    run(base + ['-prefix', sentinel, '-save_confusion', sentinel], fail='cannot replace score output prefix')
    assert sentinel.read_text() == 'keep me\n'
    for option in ['-save_predictions', '-save_fold_scores', '-save_summary_scores']:
        run(base + ['-save_confusion', sentinel, option, sentinel], fail='different files')
        assert sentinel.read_text() == 'keep me\n'
    run(base + ['-prefix', work / 'roi_collision', '-save_summary_scores', work / 'roi_collision.roi.1D'], fail='cannot replace ROI')
    run(base + ['-method', 'ridge', '-lambda', 'auto', '-prefix', work / 'tuned',
                '-save_confusion', work / 'tuned.tuning.1D'], fail='filenames must differ')
    run(base + ['-method', 'ridge', '-save_coefficients', work / 'coef',
                '-save_confusion', work / 'coef.coefficients.1D'], fail='filenames must differ')
    continuous = base.copy(); continuous[continuous.index('-mode') + 1] = 'decode'
    run(continuous + ['-save_summary_scores', work / 'invalid.1D'], fail='require -mode classify')
    run(base + ['-save_confusion', ''], fail='nonempty filenames')
    print('PASS confusion/recall, unequal subject weighting, independent subject runs, all classifiers, and null restoration')
