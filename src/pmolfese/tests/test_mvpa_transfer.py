#!/usr/bin/env python3
"""Directional transfer: independent fixed-fit references, isolation and domain checks."""
import csv
import math
import os
from pathlib import Path
import random
import struct
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())
env = dict(os.environ, OMP_NUM_THREADS='2', AFNI_FLOATSCAN='NO')
env.pop('AFNI_DECONFLICT', None)


def nifti(path, volumes):
    h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    struct.pack_into('<8h', h, 40, 4, 2, 2, 1, len(volumes), 1, 1, 1)
    struct.pack_into('<2h', h, 70, 16, 32)
    struct.pack_into('<8f', h, 76, *([1]*8))
    struct.pack_into('<f', h, 108, 352)
    struct.pack_into('<h', h, 254, 1)
    for i in range(3):
        row = [0.]*4; row[i] = 1
        struct.pack_into('<4f', h, 280+16*i, *row)
    h[344:348] = b'n+1\0'
    values = [v for row in volumes for v in row]
    path.write_bytes(h+struct.pack('<%df' % len(values), *values))


def read(path):
    with path.open() as f:
        return list(csv.DictReader((s[11:] if s.startswith('# Columns: ') else s
                    for s in f if not s.startswith('#') or s.startswith('# Columns: ')), delimiter='\t'))


with tempfile.TemporaryDirectory(prefix='mvpa_transfer_') as tmp:
    root = Path(tmp); rng = random.Random(138)
    rows = []; volumes = []
    for sub in ['s1', 's2']:
        for i in range(24):
            domain = 'visual' if i < 18 else 'auditory'
            x = (-1 if i % 2 == 0 else 1)*(1+.2*rng.random())
            y = rng.gauss(0, 1); idx = len(rows)
            volumes.append([x, y, 2*x, 4.])
            rows.append(dict(Subj=sub, Sample=f'{sub}_{i}', Item=f'{sub}_{domain}_{i}',
                             Task=domain, Session='pre' if i < 18 else 'post',
                             Run=f'{sub}_{domain}_{i//6}', Condition='A' if i % 2 == 0 else 'B',
                             Y=str(4+2*x-y), Y2=str(y), X=str(x), X2=str(y),
                             Z=str(i % 3-1), InputFile=str(root/'betas.nii')+f'[{idx}]'))
    nifti(root/'betas.nii', volumes); nifti(root/'mask.nii', [[1, 1, 2, 2]])

    def table(name, records):
        path = root/name
        with path.open('w') as f:
            w = csv.DictWriter(f, list(records[0]), delimiter='\t', lineterminator='\n')
            w.writeheader(); w.writerows(records)
        return path

    source = table('samples', rows)

    def run(args, fail=None, workers=1, overwrite=False):
        e = dict(env, OMP_NUM_THREADS=str(workers))
        if overwrite: e['AFNI_DECONFLICT'] = 'OVERWRITE'
        p = subprocess.run([binary, *map(str, args)], cwd=root, env=e,
                           capture_output=True, text=True, timeout=90)
        output = p.stdout+p.stderr
        assert (p.returncode == 0) if fail is None else (p.returncode != 0 and fail in output), (args, output)
        return output

    def command(name, mode='decode', method='ridge', src=source, transfer=True, extra=()):
        args = ['-mode', mode, '-dataTableFile', src, '-mask', root/'mask.nii',
                '-method', method, '-scale', 'feature_zscore', '-prefix', name,
                '-save_predictions', name+'.pred.1D', '-save_fold_scores', name+'.fold.1D']
        if mode == 'classify': args += ['-target_column', 'Condition']
        elif mode == 'decode': args += ['-target_columns', 'Y,Y2']
        else: args += ['-design_columns', 'X,X2', '-score', 'pattern']
        if transfer: args += ['-transfer_column', 'Task', '-train_domain', 'visual',
                              '-test_domain', 'auditory', '-transfer_subjects', 'within']
        else: args += ['-partition_column', 'Task', '-train_value', 'visual', '-test_value', 'auditory']
        return args+list(extra)

    def predictions(name):
        data = read(root/(name+'.pred.1D'))
        fields = ['Predicted', 'TrainingMean', 'DecisionClass0', 'DecisionClass1']
        return {(r['Sample'], r.get('Unit', r.get('UnitID')), r.get('Response', 'Class')):
                tuple(r[f] for f in fields if f in r) for r in data}

    # Multisubject directional fits must equal separate original fixed partitions.
    modes = [('classify', m) for m in ['centroid', 'ridge', 'svm', 'lda_diag', 'lda_shrinkage']]
    modes += [('decode', 'ridge'), ('encode', 'ridge')]
    for mode, method in modes:
        name = mode+'_'+method; run(command(name, mode, method))
        merged = {}
        for sub in ['s1', 's2']:
            ref = name+'_'+sub
            run(command(ref, mode, method, table(ref, [r for r in rows if r['Subj'] == sub]), False))
            merged.update(predictions(ref))
        assert predictions(name) == merged, name
        for r in read(root/(name+'.fold.1D')):
            assert r['NTrain'] == '18' and r['NTest'] == '6', r
        for r in read(root/(name+'.pred.1D')):
            if int(r['Sample'].split('_')[1]) < 18: assert r['Predicted'] == 'NA'
            else: assert r['CVFold'] == r['Sample'].split('_')[0]+'/auditory', r
    assert all(v[0] == ('A' if int(k[0].split('_')[1]) % 2 == 0 else 'B')
               for k, v in predictions('classify_centroid').items() if v[0] != 'NA')
    # Independent one-varying-feature ridge oracle for ROI 2 (the other input is constant).
    for r in read(root/'decode_ridge.pred.1D'):
        if r['Unit'] != '2' or r['Predicted'] == 'NA': continue
        subject = r['Sample'].split('_')[0]
        train = [i for i,v in enumerate(rows) if v['Subj']==subject and v['Task']=='visual']
        test = next(i for i,v in enumerate(rows) if v['Sample']==r['Sample'])
        x = [volumes[i][2] for i in train]; y = [float(rows[i][r['Response']]) for i in train]
        xm = sum(x)/len(x); ym = sum(y)/len(y)
        variance = sum((v-xm)**2 for v in x)
        slope = sum((a-xm)*(b-ym) for a,b in zip(x,y))/variance * len(x)/(len(x)+1)
        expected = ym+slope*(volumes[test][2]-xm)
        assert math.isclose(float(r['Predicted']),expected,abs_tol=2e-5,rel_tol=2e-5),r
    audit = read(root/'decode_ridge.transfer.1D')
    assert len(audit) == 48 and {r['Stratum'] for r in audit} == {'s1', 's2'}
    assert all(r['Domain'] == ('visual' if r['Role'] == 'train' else 'auditory') for r in audit)
    before = set(root.iterdir())
    report = run(command('validate', extra=['-validate_only']))
    assert '# SubjectDesign\twithin' in report and '# SharedSubjects\t2' in report
    assert set(root.iterdir()) == before

    # Joint selection/PCA/per-response tuning uses only this subject's source rows.
    tune = ['-lambda_grid', '.1,10', '-lambda_per_response', '-inner_cv_column', 'Run',
            '-select_features_grid', '1,2', '-pca_components_grid', '1,2', '-nuisance_columns', 'Z']
    run(command('tuned', extra=tune))
    altered = [dict(r) for r in rows]; changed = [list(v) for v in volumes]
    for i, r in enumerate(altered):
        if r['Task'] == 'auditory':
            r.update(Y=str(100+i), Y2=str(-i), Z=str(70+i)); changed[i] = [v+100 for v in changed[i]]
    nifti(root/'altered.nii', changed)
    for i, r in enumerate(altered): r['InputFile'] = str(root/'altered.nii')+f'[{i}]'
    run(command('altered', src=table('altered', altered), extra=tune))
    for suffix in ['.tuning.1D', '.feature_tuning.1D', '.pca_tuning.1D']:
        assert (root/('tuned'+suffix)).read_bytes() == (root/('altered'+suffix)).read_bytes(), suffix
    other = [dict(r) for r in rows]
    for r in other:
        if r['Subj'] == 's1': r['Y'] = str(float(r['Y'])+100)
    run(command('other', src=table('other', other)))
    assert {k:v for k,v in predictions('other').items() if k[0].startswith('s2_')} == {
        k:v for k,v in predictions('decode_ridge').items() if k[0].startswith('s2_')}

    # Two-table and disjoint-subject workflows preserve explicit directions.
    tr = table('train', [r for r in rows if r['Task'] == 'visual'])
    te = table('test', [r for r in rows if r['Task'] == 'auditory'])
    run(command('two', src=tr, extra=['-testTableFile', te]))
    assert predictions('two') == predictions('decode_ridge')
    assert all(r['Partition'] == r['Role'] for r in read(root/'two.transfer.1D'))
    disjoint = [dict(r, Subj=r['Subj']+('_train' if r['Task'] == 'visual' else '_test')) for r in rows]
    ds = table('disjoint', disjoint); args = command('disjoint', src=ds)
    args[args.index('-transfer_subjects')+1] = 'disjoint'; run(args)
    run(command('disjoint_ref', src=ds, transfer=False))
    assert predictions('disjoint') == predictions('disjoint_ref')
    # Sessions are subject-qualified; equal local names across different people are not shared sessions.
    dss = table('session_scope', [dict(r, Session='visit1') for r in disjoint])
    args = command('scope', src=dss); args[args.index('-transfer_subjects')+1] = 'disjoint'; run(args)

    # Restored observed predictions and deterministic volume searchlights.
    for mode in ['classify', 'decode', 'encode']:
        for worker in [1, 2]:
            run(command(mode+'_sl'+str(worker), mode, extra=['-searchlight', '2']), workers=worker)
        assert predictions(mode+'_sl1') == predictions(mode+'_sl2')
        run(command(mode+'_null', mode, extra=['-nperm', '5', '-permute_within', 'Run']))
        assert predictions(mode+'_null') == predictions(mode+'_ridge')
    # Single-fit saved-model support; multisubject bundles remain explicitly unsupported.
    one = table('one', rows[:24]); run(command('model', src=one, extra=['-save_model', 'model.mvpa']))
    run(command('model_ref', src=one, transfer=False, extra=['-save_model', 'ref.mvpa']))
    assert (root/'model.mvpa').read_bytes() == (root/'ref.mvpa').read_bytes()
    run(command('bad', extra=['-save_model', 'bad.mvpa']), fail='one explicit fixed')

    # Identity, overlap and target-domain failures are actionable, before outputs.
    args = command('bad'); args[args.index('-transfer_subjects')+1] = 'disjoint'
    run(args, fail='disjoint subjects')
    item = [dict(r) for r in rows]; item[18]['Item'] = item[24]['Item']
    it = table('items', item); run(command('bad', src=it), fail='disjoint items')
    run(command('allow_items', src=it, extra=['-transfer_items', 'allow']))
    session = table('sessions', [dict(r, Session='same') for r in rows])
    run(command('bad', src=session), fail='disjoint sessions')
    run(command('allow_sessions', src=session, extra=['-transfer_sessions', 'allow']))
    duplicate = [dict(r) for r in rows]; duplicate[18]['InputFile'] = duplicate[0]['InputFile']
    run(command('bad', src=table('duplicates', duplicate)), fail='same dataset/brick')
    unknown = [dict(r) for r in rows]; unknown[18]['Condition'] = 'unknown'
    run(command('bad', 'classify', src=table('unknown', unknown)), fail='class')
    absent = [dict(r, Condition='A') if r['Task'] == 'auditory' else dict(r) for r in rows]
    run(command('bad', 'classify', src=table('absent', absent)), fail='class')
    wrong = [dict(r) for r in rows]; wrong[0]['Task'] = 'other'
    run(command('bad', src=table('wrong', wrong)), fail='accepts only')
    missing = table('missing', [r for r in rows if not (r['Subj']=='s2' and r['Task']=='auditory')])
    run(command('bad', src=missing), fail='both domains')
    run(command('bad', src=te, extra=['-testTableFile', tr]), fail='table/domain mismatch')
    no_session = table('no_session', [{k:v for k,v in r.items() if k!='Session'} for r in rows])
    run(command('bad', src=no_session), fail='Session')
    no_item = table('no_item', [{k:v for k,v in r.items() if k!='Item'} for r in rows])
    run(command('bad', src=no_item), fail='explicit subject and item')
    # Block labels must preserve both transfer direction and subject strata.
    shared_run = table('shared_run', [dict(r, Run=r['Task']) for r in rows])
    run(command('bad', src=shared_run, extra=['-nperm','5','-permute_within','Run']), fail='crosses subject strata')
    run(command('bad', extra=['-nperm','5','-permute_within','Subj']), fail='crosses the train/test')
    for extra, message in [(['-cv_column','Run'],'replaces outer'), (['-partition_column','Task'],'replaces outer'),
                           (['-transfer_items','maybe'],'disjoint or allow'), (['-transfer_column','Task'],'specified twice'),
                           (['-lambda','auto'],'explicit -inner_cv_column'), (['-save_coefficients','coef'],'refits all rows')]:
        run(command('bad',extra=extra),fail=message)
    # Duplicate spellings in the schema and changed affine are rejected.
    schema = [dict(r, task=r['Task']) for r in rows]
    run(command('bad',src=table('schema',schema)),fail='duplicate transfer column')
    raw = bytearray((root/'betas.nii').read_bytes());struct.pack_into('<f',raw,292,3.);(root/'shift.nii').write_bytes(raw)
    shifted = [dict(r, InputFile=str(root/'shift.nii')+f'[{i}]') for i,r in enumerate(rows)]
    run(command('bad',src=table('shifted',shifted)),fail='grid')
    args = command('bad');args[args.index('-save_predictions')+1]='bad.transfer.1D'
    run(args,fail='transfer audit must differ')
    # Reordered two-table columns and custom identity-role names remain explicit.
    reorder = table('test_reordered',[dict(reversed(list(r.items()))) for r in rows if r['Task']=='auditory'])
    run(command('reordered',src=tr,extra=['-testTableFile',reorder]))
    assert predictions('reordered') == predictions('decode_ridge')
    custom = table('custom',[{({'Subj':'Person','Item':'Stimulus','Session':'Visit'}.get(k,k)):v for k,v in r.items()} for r in rows])
    run(command('custom_run',src=custom,extra=['-subject_column','Person','-item_column','Stimulus','-session_column','Visit']))
    assert predictions('custom_run') == predictions('decode_ridge')
    # Aliased automatic output/input files must survive even with overwrite enabled.
    victim = root/'protect.continuous.1D';victim.write_text('keep this output')
    (root/'protect.transfer.1D').symlink_to(victim)
    run(command('protect'),fail='cannot replace automatic outputs',overwrite=True)
    assert victim.read_text()=='keep this output'
    old_source=source.read_bytes();(root/'protect_input.transfer.1D').symlink_to(source)
    run(command('protect_input'),fail='transfer audit must differ',overwrite=True)
    assert source.read_bytes()==old_source
    assert not list(root.glob('bad*')), list(root.glob('bad*'))

print('PASS transfer: within/disjoint subjects, separate-fit equivalence, all estimators/modes, training isolation, tuning, workers, null restoration, model reuse, explicit identity/domain/overlap checks and audit protection')
