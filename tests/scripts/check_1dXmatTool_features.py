#!/usr/bin/env python3
"""Independent checks of 1dXmatTool diagnostics, selection, comparison and export."""
import argparse
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import numpy as np
from check_1dXmatTool import write, header, ints, table

p=argparse.ArgumentParser()
p.add_argument('--tool',required=True)
p.add_argument('--cat',required=True)
p.add_argument('--mvpa',required=True)
a=p.parse_args()
programs={k:str(Path(getattr(a,k)).resolve()) for k in ['tool','cat','mvpa']}
env=dict(os.environ,OMP_NUM_THREADS='2',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1')
with tempfile.TemporaryDirectory(prefix='xmat-features-') as tmp:
    w=Path(tmp)
    def run(opts,code=0,contains=None,prog='tool'):
        r=subprocess.run([programs[prog],*map(str,opts)],cwd=w,env=env,text=True,capture_output=True)
        assert r.returncode==code,(opts,r.stdout,r.stderr)
        if contains:assert contains in r.stdout+r.stderr,(contains,r.stdout,r.stderr)
        return r
    # Two unequal runs, shared nuisance, run-specific baselines, and 2+3 IMs.
    rng=np.random.default_rng(913)
    x=np.zeros((35,8));x[:15,0]=1;x[15:,1]=1
    x[[2,6],2]=[1,.2];x[[8,11],3]=[1,.4]
    x[[18,20],4]=[1,.3];x[[24,26],5]=[1,.2];x[[29,31],6]=[1,.5]
    x[:,7]=rng.normal(size=35)
    attrs=dict(ColumnLabels='base1 ; base2 ; event#0 ; event#1 ; event#2 ; event#3 ; event#4 ; shared',
               ColumnGroups='2@-1,5@1,0',NRowFull='35',GoodList='0..34',RunStart='0,15',RowTR='2',
               Nstim='1',StimLabels='event',StimBots='2',StimTops='6',BasisNstim='1',
               BasisOption_000001='-stim_times_IM',BasisColumns_000001='2:6',BasisFormula_000001='GAM')
    write(w/'X.1D',x,attrs)
    run(['-infile','X.1D','-describe','-json','describe.json'])
    d=json.loads((w/'describe.json').read_text())
    xn=x/np.linalg.norm(x,axis=0)
    sv=np.linalg.svd(xn,compute_uv=False)
    assert d['rank']==np.sum(sv>sv[0]*1e-7)==8
    assert abs(d['condition']-sv[0]/sv[-1])<1e-6
    assert [r['retained'] for r in d['runs']]==[15,20]
    assert [r['columns'] for r in d['runs']]==[4,5]
    assert d['residual_df']==27 and d['valid']
    run(['-infile','X.1D','-validate','-for','lss'],contains='Validation PASS')
    run(['-infile','X.1D','-validate','-for','reml'],contains='Validation PASS')
    run(['-infile','X.1D','-describe','-json','describe.json'],code=1)
    # Scaled and sign-flipped duplicate columns, rank-deficient and wide matrices.
    bad=x.copy();bad[:,7]=-3*x[:,0]
    write(w/'duplicate.1D',bad,attrs)
    run(['-infile','duplicate.1D','-validate','-json','bad.json'],code=2)
    d=json.loads((w/'bad.json').read_text())
    assert d['rank']==7 and d['duplicate_pairs']==1 and d['condition'] is None
    zero=x.copy();zero[:,7]=0
    write(w/'zero.1D',zero,attrs)
    run(['-infile','zero.1D','-validate','-json','zero.json'],code=2)
    assert json.loads((w/'zero.json').read_text())['zero_columns']==1
    wide=rng.normal(size=(3,5))
    write(w/'wide.1D',wide,dict(ColumnLabels='a ; b ; c ; d ; e',NRowFull='3',GoodList='0..2',RunStart='0'))
    run(['-infile','wide.1D','-validate','-json','wide.json'],code=2)
    assert json.loads((w/'wide.json').read_text())['rank']==3
    # Numeric provenance files are accepted by AFNI's numeric 1D reader.
    run(['-infile','X.1D','-split_runs','-prefix','split'])
    for name in ['split.r01.rows.1D','split.r01.columns.1D','split.runs.1D']:
        r=run([name],prog='cat')
        got=np.loadtxt(r.stdout.splitlines(),ndmin=2)
        np.testing.assert_array_equal(got,np.loadtxt(w/name,ndmin=2))
        assert (w/name).read_text().startswith('# Columns:')
    assert '# ColumnLabel 2 event#0' in (w/'split.r01.columns.1D').read_text()
    # Selection: union, source order, retained rows, label/group metadata, GLTs.
    run(['-infile','X.1D','-select_columns','7,3','-select_groups','baseline','-prefix','subset'])
    y=np.loadtxt(w/'subset.xmat.1D');np.testing.assert_array_equal(y.astype('float32'),x[:,[0,1,3,7]].astype('float32'))
    h=header(w/'subset.xmat.1D');assert ints(h['RunStart'])==[0,15]
    assert h['BasisColumns_000001']=='2:2' and h['ColumnLabels'].split(' ; ')[2]=='event#1'
    run(['-infile','X.1D','-select_stimuli','event','-prefix','stim'])
    np.testing.assert_array_equal(np.loadtxt(w/'stim.xmat.1D').astype('float32'),x[:,2:7].astype('float32'))
    run(['-infile','X.1D','-select_labels','shared,base2','-prefix','names'])
    np.testing.assert_array_equal(np.loadtxt(w/'names.xmat.1D').astype('float32'),x[:,[1,7]].astype('float32'))
    run(['-infile','X.1D','-select_groups','nuisance','-prefix','nuisance'])
    for badsel in ['', '0,', '0..', '-1', '8','0junk','0,0']:
        run(['-infile','X.1D','-select_columns',badsel,'-prefix','badselect'],code=1)
        assert not list(w.glob('badselect.*'))
    run(['-infile','X.1D','-select_labels','no_such','-prefix','missing'],code=1)
    write(w/'glt.1D',x,dict(attrs,Nglt='1',GltLabels='example',GltMatrix_000000='1; 0 0 1 -1 0 0 0 0'))
    run(['-infile','glt.1D','-select_groups','stimuli','-prefix','glt'],code=1,contains='-drop_glts')
    run(['-infile','glt.1D','-select_groups','stimuli','-prefix','glt','-drop_glts'])
    assert 'GltLabels' not in header(w/'glt.xmat.1D')
    before=(w/'subset.xmat.1D').read_bytes()
    run(['-infile','X.1D','-select_columns','0','-prefix','subset'],code=1)
    assert (w/'subset.xmat.1D').read_bytes()==before
    # Compare identical and reordered designs, then meaningful changes.
    run(['-infile','X.1D','-compare','X.1D'])
    order=[1,0,2,3,4,5,6,7]
    labs=attrs['ColumnLabels'].split(' ; ')
    write(w/'reordered.1D',x[:,order],dict(attrs,ColumnLabels=' ; '.join(labs[i] for i in order)))
    run(['-infile','X.1D','-compare','reordered.1D'],contains='ColumnOrderChanged 1')
    changed=x.copy();changed[1,0]+=.01
    write(w/'changed.1D',changed,attrs)
    run(['-infile','X.1D','-compare','changed.1D'],code=2,contains='ChangedValues 1')
    run(['-infile','X.1D','-compare','changed.1D','-atol','.02'])
    write(w/'censored.1D',x[1:],dict(attrs,GoodList='1..34'))
    run(['-infile','X.1D','-compare','censored.1D'],code=2,contains='SameCensor 0')
    write(w/'changed_tr.1D',x,dict(attrs,RowTR='3'))
    run(['-infile','X.1D','-compare','changed_tr.1D'],code=2,contains='SameRunsAndTR 0')
    write(w/'changed_groups.1D',x,dict(attrs,ColumnGroups='-1,0,5@1,0'))
    run(['-infile','X.1D','-compare','changed_groups.1D'],code=2,contains='SameColumnGroups 0')
    write(w/'bad_basis.1D',x,dict(attrs,BasisOption_000002='-stim_times'))
    run(['-infile','bad_basis.1D','-select_columns','0','-prefix','bad_basis'],code=1,contains='unknown stimulus')
    run(['-infile','X.1D','-select_columns','0','-prefix',''],code=1,contains='cannot be empty')
    run(['-infile','X.1D','-compare','X.1D','-for','lss'],code=1)
    write(w/'duplicate_labels.1D',x,dict(attrs,ColumnLabels=attrs['ColumnLabels'].replace('base2','base1')))
    run(['-infile','X.1D','-compare','duplicate_labels.1D'],code=1,contains='unique ColumnLabels')
    # Event labels are explicit. Shuffle rows; export both whole-bucket manifest
    # and selected-brick tables. Quoting and commented schemas round-trip.
    events=['2 2 A item4','1 1 B item1','2 0 A item2','1 0 A item0','2 1 B item3']
    (w/'events.1D').write_text('# Conditions supplied by experimenter\n# Columns: Run Volume Condition Item\n'+'\n'.join(events)+'\n')
    common=['-infile','X.1D','-export_manifest','-events','events.1D','-subject','subject with spaces','-beta_pattern','run-{run}.nii.gz']
    run(common+['-prefix','manifest'])
    text=(w/'manifest.trials.1D').read_text().splitlines()
    fields=next(line[11:].split() for line in text if line.startswith('# Columns: '))
    rows=[dict(zip(fields,shlex.split(line),strict=True)) for line in text if not line.startswith('#')]
    assert len(rows)==5 and [int(r['XColumn']) for r in rows]==[2,3,4,5,6]
    assert [int(r['Volume']) for r in rows]==[0,1,0,1,2]
    assert all(r['Subj']=='subject with spaces' for r in rows)
    assert [r['Condition'] for r in rows]==['A','B','A','B','A']
    run(common+['-prefix','selected','-manifest_format','selected'])
    # Validate selected-brick exports independently of downstream executables.
    selected_text=(w/'selected.trials.1D').read_text().splitlines()
    selected_fields=next(line[11:].split() for line in selected_text if line.startswith('# Columns: '))
    selected_rows=[dict(zip(selected_fields,shlex.split(line),strict=True))
                   for line in selected_text if not line.startswith('#')]
    assert len(selected_rows)==5
    assert [row['InputFile'] for row in selected_rows]==[
        'run-01.nii.gz[0]','run-01.nii.gz[1]',
        'run-02.nii.gz[0]','run-02.nii.gz[1]','run-02.nii.gz[2]']
    assert [int(row['SourceColumn']) for row in selected_rows]==[2,3,4,5,6]
    assert all(row['SourceXmat']=='X.1D' for row in selected_rows)
    assert [row['Subj'] for row in selected_rows]==[row['Subj'] for row in rows]
    assert [row['Condition'] for row in selected_rows]==[row['Condition'] for row in rows]
    for field in ['Run','Item','Volume']:
        assert [row[field] for row in selected_rows]==[row[field] for row in rows]
    assert 'Xmat' not in selected_fields and 'XColumn' not in selected_fields
    # Create beta fixtures so the exported files can be read by 3dMVPA.
    import nibabel as nib
    nib.save(nib.Nifti1Image(np.ones((2,2,2),dtype='uint8'),np.eye(4)),w/'mask.nii.gz')
    for i,n in [(1,2),(2,3)]:
        nib.save(nib.Nifti1Image(rng.normal(size=(2,2,2,n)).astype('float32'),np.eye(4)),w/f'run-{i:02d}.nii.gz')
    base=['-mode','classify','-target_column','Condition','-cv_column','Run','-mask','mask.nii.gz','-validate_only']
    run(base+['-trial_manifest','manifest.trials.1D'],prog='mvpa',contains='Validation PASS')
    run(base+['-dataTableFile','selected.trials.1D'],prog='mvpa',contains='Validation PASS')
    (w/'events.1D').write_text('# Columns: Run Volume Condition Item\n'+'\n'.join(events+events[:1])+'\n')
    run(common+['-prefix','bad_manifest'],code=1,contains='duplicate')
    assert not (w/'bad_manifest.trials.1D').exists()
    (w/'events.1D').write_text('Run Volume Condition\n1 0 A\n')
    run(common+['-prefix','bad_manifest'],code=1,contains='missing event')
    assert not (w/'bad_manifest.trials.1D').exists()
    print('1dXmatTool features passed: SVD, numeric 1D readers, selection, compare, manifests, MVPA ingestion')
