#!/usr/bin/env python3
"""Run extraction, metadata/provenance, failure policy and independent LSS fits."""
import argparse
import csv
import html
import os
from pathlib import Path
import re
import subprocess
import tempfile

import numpy as np
import nibabel as nib


def header(path):
    return {k: html.unescape(v) for k, v in re.findall(
        r'^#\s+(\w+)\s*=\s*"(.*)"\s*$', path.read_text(), re.M)}


def ints(value):
    values = []
    for item in value.split(','):
        if '..' in item:
            a, b = map(int, item.split('..'))
            values.extend(range(a, b + 1))
        elif '@' in item:
            n, v = map(int, item.split('@'))
            values.extend([v] * n)
        elif item.strip():
            values.append(int(item))
    return values


def table(path):
    lines=path.read_text().splitlines()
    columns=next(s[len('# Columns: '):].split() for s in lines if s.startswith('# Columns: '))
    return [dict(zip(columns,s.split(),strict=True)) for s in lines if s.strip() and not s.startswith('#')]


def write(path, data, attrs):
    with path.open('w') as f:
        f.write('# <matrix\n')
        f.write(f'# ni_type = "{data.shape[1]}*double"\n# ni_dimen = "{len(data)}"\n')
        for k, v in attrs.items():
            f.write(f'# {k} = "{html.escape(str(v), quote=True)}"\n')
        f.write('# >\n')
        np.savetxt(f, data, fmt='%.17g')
        f.write('# </matrix>\n')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--tool', required=True)
    p.add_argument('--deconvolve', required=True)
    p.add_argument('--lss', required=True)
    p.add_argument('--workdir')
    a = p.parse_args()
    tools = {k: str(Path(getattr(a, k)).resolve()) for k in ('tool', 'deconvolve', 'lss')}
    temporary = tempfile.TemporaryDirectory(prefix='xmat-check-') if not a.workdir else None
    root = Path(a.workdir or temporary.name).resolve()
    root.mkdir(parents=True, exist_ok=True)
    # A new subdirectory makes repeated CTest invocations safe without removing files.
    w = Path(tempfile.mkdtemp(prefix='case-', dir=root))
    env = dict(os.environ, OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1',
               VECLIB_MAXIMUM_THREADS='1', AFNI_DECONFLICT='OVERWRITE')

    def run(args, error=None):
        result = subprocess.run(list(map(str, args)), cwd=w, env=env,
                                capture_output=True, text=True)
        with (w / 'commands.log').open('a') as f:
            f.write(' '.join(map(str, args))+'\n'+result.stdout+result.stderr)
        if error is None:
            assert result.returncode == 0, result.stderr
        else:
            assert result.returncode != 0 and error in result.stderr, result.stderr
        return result

    def split(src='X.xmat.1D', prefix='split', extra=(), error=None):
        return run([tools['tool'], '-infile', src, '-split_runs', '-prefix', prefix, *extra], error)

    # -nodmbase prevents separate/global nuisance demeaning from changing the
    # controls: extraction intentionally preserves the input values.
    # Two unequal runs, censoring at run boundaries, one shared nuisance, one
    # run-local nuisance, and an IM family. No preprocessing is needed here.
    lengths = [80, 100]
    times = ['5 18 31 50', '6 21 40 62 76']
    (w / 'times.1D').write_text('\n'.join(times)+'\n')
    t = np.arange(sum(lengths))
    shared = np.sin(t * .31)
    local = np.zeros(len(t)); local[:80] = np.cos(np.arange(80) * .47)
    np.savetxt(w / 'shared.1D', shared)
    np.savetxt(w / 'local.1D', local)
    censor = np.ones(len(t), dtype=int)
    censor[[0, 79, 80, 88, 178]] = 0
    np.savetxt(w / 'censor.1D', censor, fmt='%d')
    run([tools['deconvolve'], '-nodata', len(t), 1, '-polort', 1, '-nodmbase',
         '-concat', '1D: 0 80', '-local_times', '-censor', 'censor.1D',
         '-ortvec', 'shared.1D', 'shared', '-ortvec', 'local.1D', 'local',
         '-num_stimts', 1, '-stim_times_IM', 1, 'times.1D', 'BLOCK(1,1)',
         '-stim_label', 1, 'events', '-x1D', 'X.xmat.1D',
         '-x1D_uncensored', 'U.xmat.1D', '-x1D_stop'])
    split(extra=['-uncensored', 'U.xmat.1D'])
    src = np.loadtxt(w / 'X.xmat.1D').astype('float32')
    sh = header(w / 'X.xmat.1D')
    source_good = ints(sh['GoodList'])
    summaries = table(w / 'split.runs.1D')
    assert len(summaries) == 2
    rng = np.random.default_rng(734)
    worst = 0.
    for i, summary in enumerate(summaries):
        base = f'split.r{i+1:02d}'
        path = w / (base+'.xmat.1D')
        x = np.loadtxt(path)
        h = header(path)
        rows = table(w / (base+'.rows.1D'))
        cols = table(w / (base+'.columns.1D'))
        assert len(cols) == src.shape[1]
        selected = [int(c['SourceColumn']) for c in cols if c['Kept'] == '1']
        selected_rows = [int(r['SourceRow']) for r in rows]
        np.testing.assert_array_equal(x.astype('float32'), src[np.ix_(selected_rows, selected)])
        assert Path(h['XmatSourceFile']) == (w / 'X.xmat.1D').resolve()
        assert Path(h['XmatSupportFile']) == (w / 'U.xmat.1D').resolve()
        assert ints(h['XmatSourceRows']) == selected_rows
        assert ints(h['XmatSourceColumns']) == selected
        assert ints(h['RunStart']) == [0] and int(h['NRowFull']) == lengths[i]
        assert [int(r['SourceTR']) for r in rows] == [source_good[r] for r in selected_rows]
        assert ints(h['GoodList']) == [int(r['RunTR']) for r in rows]
        assert int(summary['Start']) == (0 if i == 0 else 80)
        assert int(summary['NColumns']) == x.shape[1]
        im = [c for c in cols if int(c['IMVolume']) >= 0]
        assert [int(c['IMVolume']) for c in im] == list(range(4 if i == 0 else 5))
        assert [int(c['SourceIMVolume']) for c in im] == list(range(0, 4) if i == 0 else range(4, 9))
        assert int(h['BasisNstim']) == 1
        assert h['BasisColumns_000001'] == f"2:{5 if i == 0 else 6}"
        assert len(ints(h['ColumnGroups'])) == x.shape[1]

        # Independent single-run 3dDeconvolve controls: retain exactly the
        # applicable nuisance regressors, generate polorts and IMs afresh.
        start = int(summary['Start']); stop = start+lengths[i]
        (w / f'times{i}.1D').write_text(times[i]+'\n')
        np.savetxt(w / f'censor{i}.1D', censor[start:stop], fmt='%d')
        np.savetxt(w / f'shared{i}.1D', shared[start:stop])
        np.savetxt(w / f'local{i}.1D', local[start:stop])
        cmd = [tools['deconvolve'], '-nodata', lengths[i], 1, '-polort', 1, '-nodmbase',
               '-local_times', '-censor', f'censor{i}.1D',
               '-ortvec', f'shared{i}.1D', 'shared']
        if i == 0:
            cmd += ['-ortvec', f'local{i}.1D', 'local']
        run(cmd + ['-num_stimts', 1, '-stim_times_IM', 1, f'times{i}.1D', 'BLOCK(1,1)',
                   '-stim_label', 1, 'events', '-x1D', f'control{i}.xmat.1D', '-x1D_stop'])
        control = np.loadtxt(w / f'control{i}.xmat.1D')
        np.testing.assert_allclose(x, control, atol=2e-7, rtol=2e-6)
        run([tools['lss'], '-nodata', '-matrix', path, '-save1D', f'est{i}.1D'])
        run([tools['lss'], '-nodata', '-matrix', f'control{i}.xmat.1D', '-save1D', f'control_est{i}.1D'])
        est = np.loadtxt(w / f'est{i}.1D')
        np.testing.assert_allclose(est, np.loadtxt(w / f'control_est{i}.1D'), atol=2e-6, rtol=2e-5)
        # LSS estimators against an independent least-squares calculation.
        b, e = map(int, h['BasisColumns_000001'].split(':'))
        nuisance = np.delete(x, np.s_[b:e+1], axis=1)
        trial = x[:, b:e+1]
        ref = np.column_stack([
            np.linalg.pinv(np.column_stack([nuisance, trial.sum(axis=1)-trial[:, j], trial[:, j]]))[-1]
            for j in range(trial.shape[1])])
        np.testing.assert_allclose(est, ref, atol=2e-6, rtol=2e-5)
        # Full-length synthetic EPI tests 3dLSS's application of local GoodList.
        data = rng.normal(size=(2, 2, 2, lengths[i])).astype('float32')
        nib.save(nib.Nifti1Image(data, np.eye(4)), w / f'epi{i}.nii.gz')
        run([tools['lss'], '-input', f'epi{i}.nii.gz', '-matrix', path,
             '-prefix', f'beta{i}.nii.gz'])
        actual = nib.load(w / f'beta{i}.nii.gz').get_fdata().reshape(8, -1)
        expected = data.reshape(8, lengths[i])[:, ints(h['GoodList'])] @ ref
        np.testing.assert_allclose(actual, expected, atol=3e-5, rtol=2e-5)
        worst = max(worst, np.max(np.abs(actual-expected)))

    # No overwrite (including dangling symlinks), strict selections, all-run
    # preflight, missing directory, and read/write/read on a one-run matrix.
    before = (w / 'split.r01.xmat.1D').read_bytes()
    split(error='already exists')
    assert (w / 'split.r01.xmat.1D').read_bytes() == before
    (w / 'link.r01.xmat.1D').symlink_to('nonexistent')
    split(prefix='link', error='already exists')
    assert (w / 'link.r01.xmat.1D').is_symlink()
    assert not (w / 'link.runs.1D').exists()
    for spec in ['0', '3', '1,1', '1junk', '1..9999999999', '1,', '']:
        split(prefix='badselect', extra=['-runs', spec], error='invalid -runs')
    split(prefix='selected', extra=['-runs', '2..1'])
    assert [r['Run'] for r in table(w / 'selected.runs.1D')] == ['2', '1']
    split(src='split.r02.xmat.1D', prefix='again')
    np.testing.assert_array_equal(np.loadtxt(w / 'again.r01.xmat.1D'), np.loadtxt(w / 'split.r02.xmat.1D'))
    split(prefix='missing/sub/X', error='cannot create summary')
    for tol in ['-1', '1', 'nan', 'oops']:
        split(prefix='tol', extra=['-zero_tol', tol], error='-zero_tol')

    # Hand-built families exercise dropping/renumbering whole stimuli, GLTs,
    # and positive ColumnGroups, independent of the single-IM generated case.
    data = np.zeros((10, 5)); data[:, 0] = 1
    data[1, 1] = 1; data[2, 2] = .123456789; data[6, 3] = 1; data[7, 4] = .7
    attrs = dict(ColumnLabels='base ; A#0 ; A#1 ; B#0 ; B#1',
                 ColumnGroups='-1,2@1,2@2', RowTR='1', NRowFull='10',
                 GoodList='0..9', RunStart='0,5', Nstim='2', StimLabels='A ; B',
                 StimBots='1,3', StimTops='2,4', BasisNstim='2',
                 BasisOption_000001='-stim_times_IM', BasisOption_000002='-stim_times_IM',
                 BasisColumns_000001='1:2', BasisColumns_000002='3:4',
                 BasisFormula_000001='GAM', BasisFormula_000002='BLOCK(1,1)',
                 Nglt='1', GltLabels='contrast', GltMatrix_000000='1; 1 0 0 0 0',
                 ColumnMasks='ignored', CustomAttribute='keep & <this> "quote"', CommandLine='original')
    write(w / 'families.1D', data, attrs)
    split(src='families.1D', prefix='families')
    h = header(w / 'families.r02.xmat.1D')
    assert h['Nstim'] == h['BasisNstim'] == '1'
    assert h['StimLabels'] == 'B' and h['BasisFormula_000001'] == 'BLOCK(1,1)'
    assert h['BasisColumns_000001'] == '1:2' and ints(h['ColumnGroups']) == [-1, 1, 1]
    assert h['CustomAttribute'] == attrs['CustomAttribute'] and h['XmatSourceCommandLine'] == 'original'
    assert 'GltLabels' not in h and 'Nglt' not in h and 'ColumnMasks' not in h
    assert 'XmatDroppedAttributes' in h and 'BasisOption_000002' not in h

    def bad(data2, attrs2, message, extra=()):
        write(w / 'bad.1D', data2, attrs2)
        split(src='bad.1D', prefix='rejected', error=message, extra=extra)
        assert not list(w.glob('rejected.*')), 'failed validation left output files'

    crossed = data.copy(); crossed[6, 1] = .2
    bad(crossed, attrs, 'cross-run support')
    zero = data.copy(); zero[:, 1] = 0
    bad(zero, attrs, 'zero or cross-run support')
    broken = data.copy(); broken[0, 0] = np.nan
    bad(broken, attrs, 'non-finite')
    bad(data[:5], dict(attrs, GoodList='0..4'), 'no retained rows',
        ['-uncensored', 'families.1D'])
    # Global support exists before censoring, but an entire IM trial was lost.
    keep = [i for i in range(10) if i != 1]
    bad(data[keep], dict(attrs, GoodList=','.join(map(str, keep))), 'loses its support',
        ['-uncensored', 'families.1D'])
    changed = data.copy(); changed[0, 0] = 2
    bad(changed, attrs, 'differs at a retained matrix value', ['-uncensored', 'families.1D'])
    bad(data, dict(attrs, BasisColumns_000001='0:2'), 'BasisColumns disagrees')
    bad(data, dict(attrs, BasisOption_000003='-stim_times'), 'unknown stimulus')
    bad(data, dict(attrs, Nstim='3'), 'Nstim disagrees')
    bad(data, dict(attrs, ColumnGroups='0'), 'ColumnGroups length')
    bad(data, {k: v for k, v in attrs.items() if k != 'RunStart'}, 'requires explicit')
    # Shared non-IM signals are allowed across runs; zero nuisance columns drop.
    shared_attrs = dict(attrs, BasisOption_000001='-stim_times')
    write(w / 'shared.1D', crossed, shared_attrs)
    split(src='shared.1D', prefix='shared')
    assert 'A#0' in header(w / 'shared.r02.xmat.1D')['ColumnLabels']
    # A generic nuisance-only design, including zero columns, needs no IM or
    # stimulus attributes. A surviving run can also have no stimulus families.
    nuisance_only = np.column_stack([np.ones(10), np.r_[np.arange(5)+1, np.zeros(5)], np.zeros(10)])
    no_stim = dict(ColumnLabels='shared ; local ; zero', ColumnGroups='-1,0,0',
                   RowTR='1', NRowFull='10', GoodList='0..9', RunStart='0,5')
    write(w / 'nuisance.1D', nuisance_only, no_stim)
    split(src='nuisance.1D', prefix='nuisance')
    assert np.loadtxt(w / 'nuisance.r02.xmat.1D').shape == (5,)
    assert header(w / 'nuisance.r02.xmat.1D')['Nstim'] == '0'
    split(src='nuisance.r02.xmat.1D', prefix='nuisance_again')
    no_second_family = {k: v for k, v in attrs.items() if not k.endswith('_000002')}
    no_second_family.update(Nstim='1', BasisNstim='1', StimLabels='A', StimBots='1',
                            StimTops='2', ColumnGroups='-1,2@1,2@0')
    write(w / 'empty_family.1D', data, no_second_family)
    split(src='empty_family.1D', prefix='empty_family')
    assert header(w / 'empty_family.r02.xmat.1D')['Nstim'] == '0'
    # Tiny numerical padding does not assign IM trials to the wrong run.
    tiny = data.copy(); tiny[6, 1] = 1.e-9
    write(w / 'tiny.1D', tiny, attrs)
    split(src='tiny.1D', prefix='tiny')
    split(src='tiny.1D', prefix='exact', extra=['-zero_tol', '0'], error='cross-run support')
    print(f'1dXmatTool checks passed; max synthetic beta error {worst:.3g}; artifacts: {w}')
    if temporary:
        temporary.cleanup()


if __name__ == '__main__':
    main()
