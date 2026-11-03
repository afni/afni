#!/usr/bin/env python3
"""Labeled RDM inputs/models: alignment, classic/IS equivalence, and rejection contracts."""
import argparse
import math
import os
from pathlib import Path
import random
import struct
import subprocess
import tempfile


def token(value):
    return '"' + value.replace('"', '""') + '"'


def square(path, labels, matrix, columns=None, rows=None, kind='dissimilarity', axis='Condition'):
    columns = list(range(len(labels))) if columns is None else columns
    rows = list(range(len(labels))) if rows is None else rows
    path.write_text('# matrix_format labeled\n# matrix_kind ' + kind + '\n# Columns: ' + axis + ' ' +
                    ' '.join(token(labels[i]) for i in columns) + '\n' +
                    ''.join(token(labels[i]) + ' ' + ' '.join(format(matrix[i][j], '.9g') for j in columns) + '\n'
                            for i in rows))


def numeric_rows(path):
    return [[float(x) for x in line.split()] for line in path.read_text().splitlines()
            if line.strip() and not line.startswith('#')]


def result(path):
    lines = path.read_text().splitlines()
    header = next(line[1:].split() for line in lines if line.startswith('#ROI'))
    rows = [line.split() for line in lines if line.strip() and not line.startswith('#')]
    assert len(rows) == 1
    return dict(zip(header, rows[0]))


def results(path):
    lines = path.read_text().splitlines()
    header = next(line[1:].split() for line in lines if line.startswith('#ROI'))
    return [dict(zip(header, line.split())) for line in lines
            if line.strip() and not line.startswith('#')]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--bin', required=True)
    parser.add_argument('--trdm-bin')
    args = parser.parse_args()
    binary = str(Path(args.bin).resolve())
    trdm = str(Path(args.trdm_bin).resolve()) if args.trdm_bin else None
    with tempfile.TemporaryDirectory(prefix='rsa-labeled-') as directory:
        work = Path(directory)
        env = {**os.environ, 'OMP_NUM_THREADS': '2'}
        rng = random.Random(1306)
        labels = ['place one', '#hash', 'cat "A"', "O'Brien", 'face', 'Face']
        matrices = []
        native = ['Subj engagement InputFile']
        packed = ['Subj engagement InputFile']
        for s in range(6):
            matrix = [[0.0]*6 for _ in range(6)]
            for i in range(6):
                for j in range(i+1, 6):
                    matrix[i][j] = matrix[j][i] = rng.uniform(-1, 2)
            matrices.append(matrix)
            columns, rows = list(range(6)), list(range(6))
            rng.shuffle(columns)
            rng.shuffle(rows)
            square(work/f's{s}.1D', labels, matrix, columns, rows)
            (work/f'packed{s}.1D').write_text(' '.join(format(matrix[i][j], '.9g')
                                                       for i in range(6) for j in range(i+1, 6))+'\n')
            native.append(f's{s} {s*s+1} s{s}.1D')
            packed.append(f"s{s} {s*s+1} packed{s}.1D'")
        (work/'native.txt').write_text('\n'.join(native)+'\n')
        (work/'packed.txt').write_text('\n'.join(packed)+'\n')
        (work/'mask.1D').write_text(' '.join(['1']*15)+'\n')

        def run(name, flags, failure=None):
            command = [binary, '-mode', 'IS-RSA', '-model', 'engagement', 'engagement:nn',
                       '-nperm', '127', '-seed', '902', '-quiet', '-prefix', name] + flags
            proc = subprocess.run(command, cwd=work, env=env, text=True, capture_output=True)
            if failure:
                assert proc.returncode != 0 and failure in proc.stderr, (name, proc.stdout, proc.stderr)
                assert not (work/(name+'.rsa.1D')).exists(), name
                return
            assert proc.returncode == 0, (name, proc.stdout, proc.stderr)
            return result(work/(name+'.rsa.1D'))

        for metric in ['corr', 'scorr', 'euclid']:
            baseline = run('packed_'+metric, ['-mask', "mask.1D'", '-featuretype', 'pattern', '-no_dset',
                            '-neural_metric', metric, '-save_rdm', 'saved_packed_'+metric,
                            '-dataTableFile', 'packed.txt'])
            actual = run('native_'+metric, ['-input_rdm', '-neural_metric', metric,
                         '-save_rdm', 'saved_native_'+metric, '-dataTableFile', 'native.txt'])
            assert actual.keys() == baseline.keys()
            for key in actual:
                if key == 'label':
                    continue
                assert math.isclose(float(actual[key]), float(baseline[key]), rel_tol=2e-6, abs_tol=2e-6), (metric, key, actual, baseline)
            a = numeric_rows(work/f'saved_native_{metric}_roi0001.1D')
            b = numeric_rows(work/f'saved_packed_{metric}_roi0001.1D')
            assert all(math.isclose(x, y, abs_tol=2e-6, rel_tol=2e-6) for ar, br in zip(a, b) for x, y in zip(ar, br))
            output = (work/f'native_{metric}.rsa.1D').read_text()
            assert 'nvox denotes unique condition pairs' in output and 'condition order:' in output
            assert not list(work.glob(f'native_{metric}*.HEAD'))

            unmasked = run('unmasked_'+metric, ['-featuretype', 'pattern', '-neural_metric', metric,
                           '-dataTableFile', 'packed.txt'])
            assert unmasked == baseline, (metric, unmasked, baseline)
            assert not list(work.glob(f'unmasked_{metric}*.HEAD'))

        # Direct MEG: condition rows x sensor columns, transposed by AFNI.
        sensor_rng = random.Random(2217)
        sensors = ['Subj engagement InputFile']
        for s in range(6):
            (work/f'sensors{s}.1D').write_text(''.join(
                ' '.join(str(sensor_rng.gauss(0, 1)) for _ in range(3))+' 0\n'
                for _ in range(6)))
            sensors.append(f"s{s} {s*s+1} sensors{s}.1D'")
        (work/'sensors.txt').write_text('\n'.join(sensors)+'\n')
        (work/'sensor_mask.1D').write_text('1 1 1 1\n')
        for feature in ['pattern', 'rdm', 'mean']:
            common = ['-featuretype', feature, '-dataTableFile', 'sensors.txt']
            baseline = run('sensors_mask_'+feature, common+['-mask', "sensor_mask.1D'", '-no_dset'])
            actual = run('sensors_all_'+feature, common)
            assert actual == baseline, (feature, actual, baseline)
            assert actual['nvox'] == '4'  # Include the zero-valued sensor too.
        assert 'nvox denotes features' in (work/'sensors_all_pattern.rsa.1D').read_text()
        run('sensors_inline', ['-dataTable']+' '.join(sensors).split())
        for flag in [['-searchlight', 'SPHERE(2)'], ['-roi_sel', '1']]:
            run('sensors_spatial_'+flag[0][1:], ['-dataTableFile', 'sensors.txt']+flag,
                '-mask is required')
        saved = (work/'sensors1.1D').read_text()
        (work/'sensors1.1D').write_text('\n'.join(' '.join(line.split()[:3]) for line in saved.splitlines())+'\n')
        run('sensors_mismatch', ['-dataTableFile', 'sensors.txt'], 'first InputFile')
        (work/'sensors1.1D').write_text(saved)

        # A singleton-axis NIfTI is still an image, not a numeric 1D table.
        header = bytearray(352)
        struct.pack_into('<i', header, 0, 348)
        struct.pack_into('<8h', header, 40, 4, 4, 1, 1, 6, 1, 1, 1)
        struct.pack_into('<2h', header, 70, 16, 32)
        struct.pack_into('<8f', header, 76, 1, 1, 1, 1, 1, 1, 1, 1)
        struct.pack_into('<f', header, 108, 352)
        header[344:348] = b'n+1\0'
        (work/'image.nii').write_bytes(header+struct.pack('<24f', *range(24)))
        for s, error in [(0, '-mask is required for image input'),
                         (1, 'requires every InputFile to be .1D')]:
            (work/'mixed.txt').write_text('\n'.join(sensors).replace(f"sensors{s}.1D'", 'image.nii')+'\n')
            run('mixed_'+str(s), ['-dataTableFile', 'mixed.txt'], error)

        # Inline tables use the same native path, with no synthetic mask supplied.
        run('inline', ['-input_rdm', '-dataTable'] + ' '.join(native).split())
        original = (work/'s0.1D').read_text()
        mutations = [
            ('duplicate_row', original.replace(token('face')+' ', token('Face')+' ', 1), 'label'),
            ('missing_label', original.replace(token('face'), token('unexpected')), 'label'),
            ('nonfinite', original.replace(' 0 ', ' nan ', 1), 'nonfinite'),
            ('kind', original.replace('matrix_kind dissimilarity', 'matrix_kind similarity'), 'expects dissimilarity'),
            ('axis', original.replace('Columns: Condition', 'Columns: Time'), "first column must be 'Condition'"),
        ]
        # Mutation at subject 1 ensures expected label identities come from subject 0.
        s1_original = (work/'s1.1D').read_text()
        for name, content, error in mutations:
            (work/'s1.1D').write_text(content)
            run('bad_'+name, ['-input_rdm', '-dataTableFile', 'native.txt'], error)
        (work/'s1.1D').write_text(s1_original)
        for name, i, j, error in [('asymmetric', 0, 1, 'not symmetric'), ('diagonal', 0, 0, 'nonzero diagonal')]:
            matrix = [row[:] for row in matrices[0]]
            matrix[i][j] += 1
            square(work/'s0.1D', labels, matrix)
            run('bad_'+name, ['-input_rdm', '-dataTableFile', 'native.txt'], error)
        (work/'s0.1D').write_text(original)
        for name, flags, error in [
            ('mask', ['-mask', "mask.1D'"], 'nonspatial'),
            ('classic', ['-mode', 'RSA'], 'requires -mode IS-RSA'),
            ('rdm', ['-featuretype', 'rdm'], 'precomputed RDM pairs'),
            ('timeshift', ['-null', 'timeshift'], 'precomputed RDM pairs'),
            ('centering', ['-center_conditions', 'subject'], 'precomputed RDM pairs'),
            ('noise', ['-noise_norm', 'diag'], 'precomputed RDM pairs'),
        ]:
            run('bad_'+name, ['-input_rdm', '-dataTableFile', 'native.txt']+flags, error)
        (work/'duplicates.txt').write_text('\n'.join(native).replace('s1 ', 's0 ')+'\n')
        run('bad_subjects', ['-input_rdm', '-dataTableFile', 'duplicates.txt'], 'duplicate Subj')
        (work/'nosubjects.txt').write_text('\n'.join(native).replace('Subj ', 'ID ')+'\n')
        run('bad_missing_subjects', ['-input_rdm', '-dataTableFile', 'nosubjects.txt'], 'explicit Subj')

        # Model-column files live beside their table, independently of the CWD.
        # Reserved observation-column names must be ordinary condition labels.
        model_dir = work/'models'
        model_dir.mkdir()
        model_labels = ['InputFile', 'Subj', 'cat "A"', "O'Brien", 'face', 'Face']
        fusion = ['Subj MEG_RDM InputFile']
        series = ['Subj Time ModelFile']
        patterns = []
        for s in range(6):
            values = [[rng.gauss(0, 1) for _ in range(6)] for _ in range(8)]
            patterns.append(values)
            (work/f'fmri{s}.1D').write_text('\n'.join(' '.join(map(str, row)) for row in values)+'\n')
            columns, rows = list(range(6)), list(range(6))
            rng.shuffle(columns)
            rng.shuffle(rows)
            square(model_dir/f'rdm {s}.1D', model_labels, matrices[s], columns, rows)
            (work/f'numeric{s}.1D').write_text('\n'.join(' '.join(format(x, '.9g') for x in row)
                                                        for row in matrices[s])+'\n')
            fusion.append(f's{s} "rdm {s}.1D" {work}/fmri{s}.1D')
            for t in range(2):
                series.append(f's{s} time{t} numeric{s}.1D')
        (model_dir/'fusion.txt').write_text('\n'.join(fusion)+'\n')
        (work/'series.txt').write_text('\n'.join(series)+'\n')
        (work/'atlas.1D').write_text('1\n'*4+'2\n'*4)
        order = ','.join(model_labels)

        def fusion_run(name, flags, failure=None, table='models/fusion.txt', masked=True):
            command = [binary] + (['-mask', 'atlas.1D'] if masked else []) + ['-no_dset', '-nperm', '127',
                       '-seed', '902', '-quiet', '-prefix', name,
                       '-dataTableFile', table] + flags
            proc = subprocess.run(command, cwd=work, env=env, text=True, capture_output=True)
            if failure:
                assert proc.returncode != 0 and failure in proc.stderr, (name, proc.stdout, proc.stderr)
                return
            assert proc.returncode == 0, (name, proc.stdout, proc.stderr)
            return results(work/(name+'.rsa.1D'))

        for metric in ['corr', 'scorr', 'euclid']:
            for null in ['subjects', 'conditions']:
                name = metric+'_'+null
                common = ['-mode', 'RSA', '-neural_metric', metric, '-metric', 'spearman',
                          '-classic_null', null]
                actual = fusion_run('column_'+name, common+['-condition_order', order,
                                    '-model_rdm', 'MEG', 'MEG_RDM'])
                baseline = fusion_run('series_'+name, common+['-model_series_subjects', 'paired', 'series.txt'])
                baseline = [r for r in baseline if r['time_index'] == '0']
                assert len(actual) == len(baseline) == 2
                for a, b in zip(actual, baseline):
                    for left, right in [('MEG_r', 'effect'), ('MEG_p', 'p'),
                                        ('MEG_q', 'q'), ('MEG_pfwe', 'pfwe')]:
                        assert math.isclose(float(a[left]), float(b[right]), abs_tol=2e-6), (name, left, a, b)
            # The independently packed native input above produces the fixed
            # subject geometry that the model-column path must reconstruct.
            common = ['-mode', 'IS-RSA', '-featuretype', 'rdm', '-condition_metric', 'corr',
                      '-neural_metric', metric, '-metric', 'pearson']
            actual = fusion_run('column_is_'+metric, common+['-condition_order', order,
                                '-model_rdm', 'MEG', 'MEG_RDM'])
            baseline = fusion_run('matrix_is_'+metric, common+['-model_mat', 'MEG',
                                  'saved_packed_'+metric+'_roi0001.1D'])
            assert len(actual) == len(baseline) == 2
            for a, b in zip(actual, baseline):
                for key in ['MEG_r', 'MEG_p', 'MEG_q', 'MEG_pfwe']:
                    # -save_rdm serializes fewer digits than the input fixtures.
                    assert math.isclose(float(a[key]), float(b[key]), abs_tol=5e-6), (metric, key, a, b)

        paired = ['-mode', 'RSA', '-condition_order', order, '-model_rdm', 'MEG', 'MEG_RDM']
        (work/'all_features.1D').write_text('1\n'*8)
        classic_masked = fusion_run('classic_all_mask', paired+['-mask', 'all_features.1D'])
        classic_unmasked = fusion_run('classic_all_nomask', paired, masked=False)
        assert classic_unmasked == classic_masked, (classic_unmasked, classic_masked)
        for option, column in [('-bootstrap', 'boot'), ('-cond_bootstrap', 'cboot')]:
            actual = fusion_run('column_'+column, paired+[option, '40'])
            baseline = fusion_run('series_'+column, ['-mode', 'RSA', '-model_series_subjects',
                                  'paired', 'series.txt', option, '40'])
            baseline = [r for r in baseline if r['time_index'] == '0']
            for a, b in zip(actual, baseline):
                for suffix in ['Lo', 'Hi']:
                    assert math.isclose(float(a['MEG_'+column+suffix]), float(b[column+suffix]),
                                        abs_tol=2e-6), (option, a, b)
        fusion_run('column_searchlight', paired+['-searchlight', 'SPHERE(2)'])
        fusion_run('column_multi', paired+['-model_rdm', 'MEG2', 'MEG_RDM'])
        fusion_run('column_no_order', ['-mode', 'RSA', '-model_rdm', 'MEG', 'MEG_RDM'], 'needs -condition_order')
        fusion_run('column_missing', ['-mode', 'RSA', '-condition_order', order,
                                     '-model_rdm', 'MEG', 'missing'], 'separate RDM-file column')
        fusion_run('column_wrong_labels', ['-mode', 'RSA', '-condition_order', ','.join(labels),
                                          '-model_rdm', 'MEG', 'MEG_RDM'], 'label')
        fusion_run('column_joint', paired+['-model_joint'], 'separate paired RDM fits')
        fusion_run('column_pattern', paired+['-mode', 'IS-RSA', '-featuretype', 'pattern'], 'requires -featuretype rdm')
        fusion_run('column_dualboot', paired+['-bootstrap', '40', '-cond_bootstrap', '40'], 'separate paired RDM fits')
        (model_dir/'duplicate.txt').write_text('\n'.join(fusion).replace('s1 ', 's0 ')+'\n')
        fusion_run('column_duplicate', paired, 'duplicate Subj', 'models/duplicate.txt')

        # Long-table collapse retains the original table directory for RDMs.
        long_rows = ['Subj Condition MEG_RDM InputFile']
        for s in range(6):
            for c in reversed(range(6)):
                fn = work/f'fmri{s}_condition{c}.1D'
                fn.write_text(''.join(str(row[c])+'\n' for row in patterns[s]))
                long_rows.append(f's{s} {token(model_labels[c])} "rdm {s}.1D" {fn}')
        (model_dir/'long.txt').write_text('\n'.join(long_rows)+'\n')
        compact_result = fusion_run('column_compact', paired)
        long_result = fusion_run('column_long', paired+['-condition_column', 'Condition'], table='models/long.txt')
        assert compact_result == long_result, (compact_result, long_result)
        inferred = paired[:]
        ix = inferred.index('-condition_order'); del inferred[ix:ix+2]
        inferred_result = fusion_run('column_long_inferred', inferred+['-condition_column', 'Condition'], table='models/long.txt')
        for actual, expected in zip(inferred_result, long_result):
            for key in ['MEG_r', 'MEG_p', 'MEG_q', 'MEG_pfwe']:
                assert math.isclose(float(actual[key]), float(expected[key]), abs_tol=2e-6), (key, actual, expected)

        if trdm:
            # Real 1dTrdm export -> native 3dRSA, without a conversion script.
            observations = ['Subj Observation Condition InputFile']
            for s in range(6):
                for c in range(6):
                    fn = f'trial_{s}_{c}.txt'
                    (work/fn).write_text('time_value time_unit A B C D\n'+''.join(
                        f'{t} ms '+' '.join(str(rng.gauss(0, 1)) for _ in range(4))+'\n' for t in range(3)))
                    observations.append(f's{s} trial{c} cond{c} {fn}')
            (work/'trials.txt').write_text('\n'.join(observations)+'\n')
            proc = subprocess.run([trdm, '-dataTable', 'trials.txt', '-input_format', 'labeled',
                                   '-window_width', '3', '-metric', 'corr', '-matrix_format', 'labeled',
                                   '-prefix', 'export'], cwd=work, env=env, text=True, capture_output=True)
            assert proc.returncode == 0, proc.stderr
            (work/'export.txt').write_text('Subj engagement InputFile\n'+''.join(
                f's{s} {s*s+1} export_s{s:04d}_s{s}_t0000.1D\n' for s in range(6)))
            run('exported', ['-input_rdm', '-dataTableFile', 'export.txt'])
            (work/'export_fusion.txt').write_text('Subj MEG_RDM InputFile\n'+''.join(
                f's{s} export_s{s:04d}_s{s}_t0000.1D fmri{s}.1D\n' for s in range(6)))
            for mode in ['RSA', 'IS-RSA']:
                fusion_run('export_fusion_'+mode, ['-mode', mode, '-condition_order',
                           ','.join(f'cond{c}' for c in range(6)), '-model_rdm', 'MEG', 'MEG_RDM'],
                           table='export_fusion.txt')
    print('PASS: labeled input/model RDMs align labels, match classic paired-series and IS subject-matrix effects/nulls, reject invalid inputs, and read 1dTrdm exports in both modes.')


if __name__ == '__main__':
    main()
