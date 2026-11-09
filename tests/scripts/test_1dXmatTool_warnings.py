"""Synthetic warning/exit-code checks; also runnable directly with --tool.

Set AFNI_XMAT_TOOL to test a specific build under pytest. Direct execution
needs only the standard library, so diagnostics can be checked independently
of the imaging dependencies used by the broader integration scripts.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile


def check_warnings(tool, directory):
    """Check censored/full pairs against hand-counted support and run counts."""
    directory = Path(directory)
    tool = str(Path(tool).resolve())
    matrix = [[float(r < 12), float(r >= 12), 0., 0., 0.] for r in range(24)]
    for r, value in [(2, 1.), (3, .5), (4, .2), (5, .015)]:
        matrix[r][2] = value
    for r in [14, 15]:
        matrix[r][3] = 1.
    for r in [18, 19]:
        matrix[r][4] = 1.

    def write(name, rows, values=matrix):
        # Explicit metadata keeps each support/ownership test independent of
        # guesses based on column labels or omitted run/censor attributes.
        attrs = dict(ColumnLabels='base1;base2;trial#0;trial#1;trial#2',
                     ColumnGroups='-1,-1,1,1,1', NRowFull='24',
                     GoodList=','.join(map(str, rows)), RunStart='0,12', RowTR='2',
                     Nstim='1', StimLabels='trial', StimBots='2', StimTops='4',
                     BasisNstim='1', BasisOption_000001='-stim_times_IM',
                     BasisColumns_000001='2:4', BasisFormula_000001='GAM')
        text = '\n'.join(f'# {key} = "{value}"' for key, value in attrs.items())
        text = f'# <matrix\n# ni_type = "5*double"\n# ni_dimen = "{len(rows)}"\n' + text + '\n# >'
        text += '\n' + '\n'.join(' '.join(map(str, values[r])) for r in rows) + '\n'
        (directory / name).write_text(text + '# </matrix>\n')

    def run(*args, code=0):
        result = subprocess.run([tool, *args], cwd=directory, text=True,
                                capture_output=True,
                                env=dict(os.environ, AFNI_VERSION_CHECK='NO', OMP_NUM_THREADS='1'))
        assert result.returncode == code, (args, result.stdout, result.stderr)
        return result

    write('full.1D', range(24))
    write('partial.1D', [r for r in range(24) if r not in [2, 3, 4]])
    write('lost.1D', [r for r in range(24) if r not in [2, 3, 4, 5]])
    pair = ['-infile', 'partial.1D', '-uncensored', 'full.1D']
    result = run(*pair, '-validate', '-json', 'partial.json')
    assert '# WARNING' not in result.stderr
    assert 'original counts differ across runs' in result.stdout
    data = json.loads((directory / 'partial.json').read_text())
    support = data['im_support'][0]
    assert support['full_support_rows'] == 4 and support['retained_support_rows'] == 1
    assert support['support_fraction'] == .25 and abs(support['peak_fraction'] - .015) < 1e-8
    assert [v['original'] for v in data['im_counts']] == [1, 2]
    result = run(*pair, '-validate', '-warn_support_frac', '.5')
    assert 'support fraction 0.25, peak fraction 0.015' in result.stderr
    assert 'Validation PASS' in result.stdout
    assert 'support fraction' not in run(*pair, '-describe', '-warn_support_frac', '.25').stderr
    for bad in ['nan', '-.1', '1.1', 'junk']:
        run(*pair, '-describe', '-warn_support_frac', bad, code=1)
    run(*pair, '-split_runs', '-prefix', 'bad-option', '-warn_support_frac', '.5', code=1)

    result = run('-infile', 'lost.1D', '-describe', '-json', 'unknown.json')
    message = ("IM column 'trial#0' has no nonzero value in any retained row; "
               "the trial may be fully censored. Re-run with -uncensored "
               "<X.nocensor.xmat.1D> to identify the run and confirm")
    assert message in result.stdout
    assert '2 failing run(s)' in result.stderr and '-validate returns 2' in result.stderr
    unknown = json.loads((directory / 'unknown.json').read_text())
    assert [v['retained'] for v in unknown['runs']] == [8, 12]
    assert all(v['columns'] == -1 and v['rank'] == -1 for v in unknown['runs'])
    assert unknown['im_support'][0]['support_fraction'] is None
    assert all(v['original'] is None for v in unknown['im_counts'])
    run('-infile', 'lost.1D', '-validate', code=2)
    run('-infile', 'lost.1D', '-split_runs', '-prefix', 'lost-split', code=1)
    result = run('-infile', 'lost.1D', '-uncensored', 'full.1D', '-validate',
                 '-json', 'lost.json', code=2)
    assert "IM column 'trial#0' loses its support after censoring" in result.stdout
    assert 'run 1: 0 of 1 assigned IM columns retained after censoring' in result.stderr
    data = json.loads((directory / 'lost.json').read_text())
    assert data['runs'][0]['retained'] == 8 and data['runs'][1]['valid']
    assert data['im_support'][0]['support_fraction'] == 0
    run('-infile', 'lost.1D', '-uncensored', 'full.1D', '-split_runs', '-prefix', 'lost-ref', code=1)

    zero = [row[:] for row in matrix]
    for row in zero:
        row[2] = 0
    write('zero.1D', range(24), zero)
    result = run('-infile', 'zero.1D', '-describe')
    assert 'no nonzero value in the uncensored design; check onsets and basis specification' in result.stdout
    cross = [row[:] for row in matrix]
    cross[15][2] = .5
    write('cross.1D', range(24), cross)
    assert 'has cross-run support; run ownership is ambiguous' in run('-infile', 'cross.1D', '-describe').stdout
    result = run('-infile', 'partial.1D', '-uncensored', 'zero.1D', '-validate', code=2)
    assert 'reference mismatch' in result.stderr

    # No reference means unknown pre-censor fractions, even if support remains.
    result = run('-infile', 'partial.1D', '-describe', '-warn_support_frac', '.5',
                 '-json', 'partial-unknown.json')
    assert 'support fraction' not in result.stderr
    assert all(v['peak_fraction'] is None for v in
               json.loads((directory / 'partial-unknown.json').read_text())['im_support'])
    write('empty-run.1D', range(12, 24))
    result = run('-infile', 'empty-run.1D', '-uncensored', 'full.1D',
                 '-validate', '-json', 'empty.json', code=2)
    assert 'selected run has no retained rows' in result.stdout
    assert json.loads((directory / 'empty.json').read_text())['runs'][0]['retained'] == 0

    events = 'Run Volume Condition\n1 0 a\n2 0 b\n2 1 c\n'
    (directory / 'events.1D').write_text(events + '2 2 extra\n')
    manifest = ['-infile', 'full.1D', '-export_manifest', '-events', 'events.1D',
                '-subject', 's01', '-beta_pattern', 'beta.r{run}.nii']
    result = run(*manifest, '-prefix', 'extra', code=1)
    assert 'events contain unmatched rows: row 4, run 2 volume 2' in result.stderr
    assert not (directory / 'extra.trials.1D').exists()
    (directory / 'events.1D').write_text('Run Volume Condition\n2 0 b\n2 1 c\n')
    result = run(*manifest, '-prefix', 'missing', code=1)
    assert 'missing event for run 1 volume 0; check run/volume indexing' in result.stderr

    # Missing timeline metadata must not silently become one run/no censoring.
    full_text = (directory / 'full.1D').read_text()
    for attribute in ['RunStart', 'GoodList', 'NRowFull']:
        name = 'missing-' + attribute + '.1D'
        (directory / name).write_text('\n'.join(
            line for line in full_text.splitlines()
            if not line.startswith('# ' + attribute + ' =')) + '\n')
        for force in [False, True]:
            suffix = attribute + ('-forced' if force else '-default')
            options = ['-force_inferred_metadata'] if force else []
            result = run('-infile', name, '-describe', *options, '-json', suffix + '.json')
            report = json.loads((directory / (suffix + '.json')).read_text())
            assert not report['timeline_metadata_explicit']
            assert report['forced_inferred_metadata'] == force
            assert not report['valid']
            if force:
                assert 'INFERRED METADATA' in result.stdout
                assert 'run/censor assignments may be wrong' in result.stderr
                assert report['im_counts'] is not None
                if attribute == 'RunStart':
                    assert report['im_counts'][0]['original'] == 3
            else:
                assert 'timeline-dependent diagnostics unavailable' in result.stderr
                assert report['im_support'] is None and report['im_counts'] is None
                assert report['runs'] == []
                assert '# IMCounts Run=' not in result.stdout
            run('-infile', name, '-validate', *options, code=2)
        run('-infile', name, '-split_runs', '-prefix', 'no-metadata', code=1)
        run('-infile', name, '-split_runs', '-prefix', 'no-metadata',
            '-force_inferred_metadata', code=1)
    # Reference metadata is checked too; force permits defaults only when the
    # reference still matches. It never overrides a mismatched reference.
    result = run('-infile', 'partial.1D', '-uncensored', 'missing-GoodList.1D',
                 '-describe', '-json', 'missing-reference.json')
    report = json.loads((directory / 'missing-reference.json').read_text())
    assert report['im_support'] is None
    assert 'uncensored reference lacks explicit' in result.stderr
    run('-infile', 'partial.1D', '-uncensored', 'missing-GoodList.1D',
        '-validate', '-force_inferred_metadata')
    run('-infile', 'full.1D', '-uncensored', 'zero.1D', '-validate',
        '-force_inferred_metadata', '-json', 'force-mismatch.json', code=2)
    assert json.loads((directory / 'force-mismatch.json').read_text())['im_support'] is None
    run('-infile', 'full.1D', '-show_trs', '-runs', '1', '-select_columns', '2',
        '-tr_above', '.01', '-force_inferred_metadata', code=1)

    # Query stdout is byte-for-byte selector text; fractional queries without
    # full support retain their operational-error status.
    query = ['-infile', 'partial.1D', '-show_trs', '-runs', '1', '-select_columns', '2']
    result = run(*query, '-tr_above', '.01')
    assert result.stdout == '5\n', result.stdout
    run(*query, '-tr_above_frac', '.5', code=1)
    run('-infile', 'full.1D', '-compare', 'full.1D')
    run('-infile', 'full.1D', '-compare', 'partial.1D', code=2)
    run('-infile', 'missing.1D', '-describe', code=1)


def test_xmat_warnings(tmp_path):
    tool = os.environ.get('AFNI_XMAT_TOOL', str(Path(__file__).resolve().parents[2] / 'src' / '1dXmatTool'))
    check_warnings(tool, tmp_path)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--tool', required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='xmat-warnings-') as directory:
        check_warnings(args.tool, directory)
    print('1dXmatTool warning checks passed')
