#!/usr/bin/env python3
"""Saved ridge model round-trip, target-free application and format/domain rejection checks."""
import csv
import difflib
import itertools
import math
import os
import random
from pathlib import Path
import struct
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())


def nifti(path, volumes):
    p = len(volumes[0]); h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    assert p == 4
    struct.pack_into('<8h', h, 40, 4, 2, 2, 1, len(volumes), 1, 1, 1)
    struct.pack_into('<2h', h, 70, 16, 32)
    struct.pack_into('<8f', h, 76, *([1]*8))
    struct.pack_into('<f', h, 108, 352); struct.pack_into('<h', h, 254, 1)
    for i in range(3):
        row = [0.]*4; row[i] = 1
        struct.pack_into('<4f', h, 280+16*i, *row)
    h[344:348] = b'n+1\0'
    v = [x for row in volumes for x in row]
    path.write_bytes(h+struct.pack('<%df' % len(v), *v))


def read(path):
    with path.open() as f:
        return list(csv.DictReader((s[11:] if s.startswith('# Columns: ') else s
                                   for s in f if not s.startswith('#') or s.startswith('# Columns: ')), delimiter='\t'))


with tempfile.TemporaryDirectory(prefix='mvpa_models_') as folder:
    root=Path(folder);rng=random.Random(183);n=24
    x=[[rng.gauss(0,1),rng.gauss(0,1),rng.gauss(0,1),5.] for _ in range(n)]
    y=[[2*r[0]-.5*r[1],rng.gauss(0,1),7] for r in x]
    nifti(root/'mask.nii',[[1,1,2,2]]);nifti(root/'betas.nii',x)
    rows=[dict(Subj='s1',Sample=f'sample {i}',Fold=str(i//6),Run='all',Split='train' if i<18 else 'test',
               Class=['face "A"','house#B'][i%2],Z=str(i%3-1),Z2=str((i*3)%5-2),
               Y=str(y[i][0]),Y2=str(y[i][1]),Y3=str(y[i][2]),InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name,records):
        path=root/name
        with path.open('w') as f:
            w=csv.DictWriter(f,list(records[0]),delimiter='\t',quoting=csv.QUOTE_ALL,lineterminator='\n');w.writeheader();w.writerows(records)
        return path
    source=table('samples',rows)
    new=table('new',rows[18:])
    no_targets=table('targetfree',[{k:v for k,v in r.items() if k not in ['Y','Y2','Y3','Class','Split','Fold']} for r in rows[18:]])
    def run(args,fail=None,workers=1,overwrite=False,exe=binary):
        env=dict(os.environ,OMP_NUM_THREADS=str(workers));env.pop('AFNI_DECONFLICT',None)
        if overwrite:env['AFNI_DECONFLICT']='OVERWRITE'
        p=subprocess.run([exe,*map(str,args)],cwd=root,capture_output=True,text=True,env=env,timeout=90)
        output=p.stdout+p.stderr
        assert (p.returncode==0) if fail is None else (p.returncode!=0 and fail in output),(args,output)
        return output
    def command(name,mode='decode',extra=(),save=True,src=source):
        return ['-mode',mode,'-method','ridge','-dataTableFile',src,'-partition_column','Split',
                '-mask',root/'mask.nii','-sample_column','Sample','-prefix',name,
                '-save_predictions',name+'.pred.1D','-save_fold_scores',name+'.fold.1D',
                '-target_columns','Class' if mode=='classify' else 'Y,Y2,Y3',
                *(['-save_model',name+'.model'] if save else []),*extra]
    def apply(model,name='applied',src=no_targets,extra=(),**kw):
        run(['-apply_model',model,'-dataTableFile',src,'-prefix',name,'-sample_column','Sample',*extra],**kw)
        if 'fail' not in kw:return read(root/(name+'.applied.1D'))
    def verify(name,mode='decode',result=None):
        result=apply(name+'.model',name+'_apply') if result is None else result
        original=read(root/(name+'.pred.1D'))
        truth={(r.get('Unit',r.get('UnitID')),r['Sample'],r.get('Response','Class')):r for r in original if r['CVFold']!='NA'}
        labels=[]
        if mode=='classify':
            for line in (root/(name+'.pred.1D')).read_text().splitlines():
                if line.startswith('# Class\t'):
                    # AFNI tokens double quote characters; csv handles the field.
                    labels.append(next(csv.reader([line.split('\t',2)[2]],delimiter='\t'))[0])
        f32=lambda v:struct.unpack('f',struct.pack('f',float(v)))[0]
        for r in result:
            assert r['Observed']=='NA'
            old=truth[r['Unit'],r['Sample'],'Class' if mode=='classify' else r['Response']]
            value=old['DecisionClass'+str(labels.index(r['Response']))] if mode=='classify' else old['Predicted']
            assert f32(r['Prediction'])==f32(value),(name,r,value)
            if mode=='classify':assert r['PredictedClass']==old['Predicted']
        assert len(result)==len(truth)*(len(labels) if mode=='classify' else 1)
        return result
    for mode in ['decode','classify']:
        for scale in ['none','feature_zscore','sample_center']:
            name=mode+'_'+scale;run(command(name,mode,['-scale',scale]));verify(name,mode)
        pipeline=['-nuisance_columns','Z,Z2','-select_features_grid','1,2','-pca_components_grid','1,2','-lambda_grid','.1,10','-inner_cv_column','Fold']
        name=mode+'_pipeline';run(command(name,mode,pipeline));res=verify(name,mode)
        # Saving is opt-in and does not change any observed analysis result.
        run(command(name+'_plain',mode,pipeline,save=False))
        for suffix in ['pred','fold','tuning','features','pca']:
            assert (root/(name+'.'+suffix+'.1D')).read_bytes()==(root/(name+'_plain.'+suffix+'.1D')).read_bytes()
        # A batch is just independent applications of frozen training parameters.
        single=table('single',[{k:v for k,v in rows[18].items() if k not in ['Y','Y2','Y3','Class','Split','Fold']}])
        one=apply(name+'.model',name+'_single',single)
        assert one==[r for r in res if r['Sample']=='sample 18']
        with_targets=apply(name+'.model',name+'_observed',new)
        assert [{k:v for k,v in r.items() if k!='Observed'} for r in with_targets]==[{k:v for k,v in r.items() if k!='Observed'} for r in res]
        # Covariate columns may be reordered, never silently reinterpreted.
        reordered=table('reordered',[dict(reversed(list({k:v for k,v in row.items() if k not in ['Y','Y2','Y3','Class','Split','Fold']}.items()))) for row in rows[18:]])
        assert apply(name+'.model',name+'_reordered',reordered)==res
        missing=table('missing',[{k:v for k,v in rows[18].items() if k!='Z2'}])
        apply(name+'.model','bad',missing,fail='saved nuisance columns')
        wrong=table('wrong',[dict(rows[18],Class='unknown label')])
        if mode=='classify':apply(name+'.model','bad',wrong,fail='unknown observed class')
    run(command('response',extra=['-lambda_per_response','-inner_cv_column','Fold','-lambda_grid','.01,1,100','-pca_components','2']))
    verify('response')
    run(command('response_perm',extra=['-lambda_per_response','-inner_cv_column','Fold','-lambda_grid','.01,1,100','-pca_components','2','-nperm','3','-permute_within','Fold']))
    assert (root/'response.model').read_bytes()==(root/'response_perm.model').read_bytes()
    # Mutating all held-out values leaves serialized training state byte-identical.
    changed_x=[r[:] for r in x];changed=[r.copy() for r in rows]
    for i in range(18,n):changed_x[i]=[v+100 for v in changed_x[i]];changed[i].update(Y='100',Y2='-100',Y3='-99',Z='40',Z2='30')
    nifti(root/'changed.nii',changed_x)
    for i in range(n):changed[i]['InputFile']=str(root/'changed.nii')+f'[{i}]'
    run(command('changed',extra=pipeline,src=table('changed',changed)))
    assert (root/'changed.model').read_bytes()==(root/'decode_pipeline.model').read_bytes()
    # Export final observed state after permutations; all units/workers retain it.
    for mode in ['decode','classify']:
        for workers in [1,2]:
            run(command(mode+'_sl'+str(workers),mode,['-searchlight','2','-lambda_grid','.1,10','-inner_cv_column','Fold','-pca_components','1']),workers=workers)
        assert (root/(mode+'_sl1.model')).read_bytes()==(root/(mode+'_sl2.model')).read_bytes()
        verify(mode+'_sl1',mode)
        for perm in [False,True]:
            run(command(mode+'_perm'+str(perm),mode,['-lambda_grid','.1,10','-inner_cv_column','Fold']+(['-nperm','3','-permute_within','Fold'] if perm else [])))
        assert (root/(mode+'_permFalse.model')).read_bytes()==(root/(mode+'_permTrue.model')).read_bytes()
        verify(mode+'_permTrue',mode)
    # Two-table provenance does not alter the frozen training pipeline.
    tr=table('train',rows[:18]);te=table('test',rows[18:]);args=command('two',src=tr)
    pos=args.index('-partition_column');args[pos:pos+2]=['-testTableFile',te];run(args);verify('two')
    assert (root/'two.model').read_bytes()==(root/'decode_feature_zscore.model').read_bytes()
    # Rank-zero PCA and constant inputs survive serialization/application.
    nifti(root/'constant.nii',[[2]*4 for _ in rows]);constant=[dict(r,InputFile=str(root/'constant.nii')+f'[{i}]') for i,r in enumerate(rows)]
    run(command('rankzero',src=table('constant',constant),extra=['-pca_components','2']))
    cres=apply('rankzero.model','rankzero_apply',table('constantnew',[{k:v for k,v in r.items() if k not in ['Y','Y2','Y3','Class']} for r in constant[18:]]));verify('rankzero',result=cres)
    # Model variants must reject incompatible fields before producing output.
    good=(root/'decode_pipeline.model').read_text()
    variants=[good.replace('AFNI_MVPA_MODEL 1','AFNI_MVPA_MODEL 999',1),good[:len(good)//2],good+'extra\n',
              good.replace('META 0 0 2 3 2 18','META 0 0 2147483647 3 2 18',1),
              good.replace('1 59\n2 5932','1 59\n1 59',1),
              good.replace('GRID 2 2 1','GRID 1 1 1',1),good.replace('INDEX\n','INDEX\n999999 ',1),
              good.replace('COEFFICIENTS\n','COEFFICIENTS\nnan ',1),good.replace('LAMBDA\n','LAMBDA\n0 ',1)]
    assert all(v!=good for v in variants)
    for i,v in enumerate(variants):
        (root/'bad.model').write_text(v);apply('bad.model','bad',fail='saved')
        assert not (root/'bad.applied.1D').exists()
    # Geometry shifts, dimension changes and surfaces are not interchangeable features.
    raw=bytearray((root/'betas.nii').read_bytes());struct.pack_into('<f',raw,292,3.);(root/'shift.nii').write_bytes(raw)
    shifted=table('shifted',[dict(rows[18],InputFile=str(root/'shift.nii')+'[18]')]);apply('decode_none.model','bad',shifted,fail='affine/feature grid')
    shift_train=table('shifttrain',[dict(r,InputFile=str(root/'shift.nii')+f'[{i}]') for i,r in enumerate(rows)])
    run(command('badgrid',src=shift_train),fail='grid');assert not (root/'badgrid.model').exists()
    raw=bytearray((root/'betas.nii').read_bytes());struct.pack_into('<3h',raw,42,4,1,1);(root/'shape.nii').write_bytes(raw)
    shape=table('shape',[dict(rows[18],InputFile=str(root/'shape.nii')+'[18]')]);apply('decode_none.model','bad',shape,fail='dimensions/view/space')
    for extra,message in [(['-scale','run_center'],'does not support run_center'),(['-method','svm'],'ridge decoding/classification only'),
                          (['-save_coefficients','coef'],'cannot accompany'),(['-surf','unused.gii','-searchlight','100'],'volume ROIs/searchlights only'),
                          (['-save_model','again'],'specified twice')]:run(command('bad',extra=extra),fail=message)
    args=command('bad');pos=args.index('-partition_column');args[pos:pos+2]=['-cv_column','Fold'];run(args,fail='explicit fixed')
    args=command('bad');args[args.index('-mode')+1]='encode';run(args,fail='ridge decoding/classification only')
    apply('decode_none.model','bad',extra=['-scale','none'],fail='standalone')
    run(command('validate',extra=['-validate_only']));assert not list(root.glob('validate*'))
    # Saving a fold pattern alongside the model does not change the frozen fit.
    run(command('patterns',extra=['-save_patterns','patterns_out']))
    assert (root/'patterns.model').read_bytes()==(root/'decode_feature_zscore.model').read_bytes()
    # Publication is atomic and input/output aliases cannot destroy source data.
    saved=(root/'decode_none.model').read_bytes();run(command('decode_none',extra=['-scale','none']),fail='already exists');assert saved==(root/'decode_none.model').read_bytes()
    for destination in [source,root/'betas.nii','bad.pred.1D','bad.continuous.1D']:
        args=command('bad');args[args.index('-save_model')+1]=destination;run(args,fail='model',overwrite=True)
    alias=root/'alias.applied.1D';alias.symlink_to(root/'decode_none.model');apply('decode_none.model','alias',fail='cannot replace',overwrite=True)
    (root/'bad.model').write_text(good);apply('bad.model','kept');saved=(root/'kept.applied.1D').read_bytes()
    (root/'bad.model').write_text(good[:10]);apply('bad.model','kept',fail='format/version',overwrite=True);assert (root/'kept.applied.1D').read_bytes()==saved
    assert not list(root.glob('*.tmp.*'))
    # Training-only fits must reproduce the same fixed-partition training fit.
    training=table('training_only',rows[:18])
    def train_command(name,mode='classify',extra=()):
        return ['-train_only','-mode',mode,'-method','ridge','-dataTableFile',training,
                '-mask',root/'mask.nii','-target_columns','Class' if mode=='classify' else 'Y,Y2,Y3',
                '-save_model',name+'.model','-prefix',name,*extra]
    for mode in ['classify','decode']:
        for scale in ['none','feature_zscore','sample_center']:
            name='train_'+mode+'_'+scale
            run(train_command(name,mode,['-scale',scale]))
            assert (root/(name+'.model')).read_bytes()==(root/(mode+'_'+scale+'.model')).read_bytes()
            assert not (root/(name+'.roi.1D')).exists() and not (root/(name+'.continuous.1D')).exists()
            assert len(read(root/(name+'.training.1D')))==2*(2 if mode=='classify' else 3)
        run(train_command('train_'+mode+'_pipeline',mode,pipeline))
        assert (root/('train_'+mode+'_pipeline.model')).read_bytes()==(root/(mode+'_pipeline.model')).read_bytes()
    run(train_command('train_response','decode',['-lambda_per_response','-inner_cv_column','Fold','-lambda_grid','.01,1,100','-pca_components','2']))
    assert (root/'train_response.model').read_bytes()==(root/'response.model').read_bytes()
    run(train_command('train_validate',extra=['-validate_only']))
    assert not list(root.glob('train_validate*'))
    for extra,message in [(['-cv_column','Fold'],'outer CV'),(['-testTableFile',te],'test tables'),
                          (['-save_predictions','no.1D'],'does not compute'),(['-nperm','2'],'does not compute'),
                          (['-lambda','auto'],'explicit -inner_cv_column')]:
        run(train_command('train_bad',extra=extra),fail=message)
    args=train_command('train_bad');i=args.index('-save_model');del args[i:i+2];run(args,fail='requires -save_model')
    # Logistic export preserves fixed-partition logits and probabilities, also after tuning.
    logistic_pipeline=['-select_features_grid','1,2','-pca_components_grid','1,2',
                       '-logistic_lambda_grid','.1,1','-inner_cv_column','Fold','-nuisance_columns','Z,Z2']
    for extra,suffix in [([],''),(logistic_pipeline,'_pipeline')]:
        name='logistic_export'+suffix
        run(command(name,'classify',['-method','logistic',*extra]))
        applied=apply(name+'.model',name+'_apply')
        original=read(root/(name+'.pred.1D'))
        truth={(r['UnitID'],r['Sample']):r for r in original if r['CVFold']!='NA'}
        for row in applied:
            cls=0 if row['Response']==rows[0]['Class'] else 1
            old=truth[row['Unit'],row['Sample']]
            assert math.isclose(float(row['Prediction']),float(old['DecisionClass'+str(cls)]),rel_tol=2e-6,abs_tol=2e-7)
            assert math.isclose(float(row['Probability']),float(old['ProbabilityClass'+str(cls)]),rel_tol=2e-6,abs_tol=2e-7)
        run(train_command('train_'+name,extra=['-method','logistic',*extra]))
        assert (root/('train_'+name+'.model')).read_bytes()==(root/(name+'.model')).read_bytes()
    # Dataset adapter: all TRs, reordered/noncontiguous ranges, source indexing and ROI x class x TR.
    for model in ['classify_feature_zscore.model','decode_none.model','logistic_export.model']:
        name='dataset_'+model.split('.')[0]
        run(['-apply_model',model,'-apply_dset',root/'betas.nii','-prefix',name])
        allrows=read(root/(name+'.applied.1D'))
        assert len(allrows)==n*2*(3 if model.startswith('decode') else 2)
        selected=table('selected',[{k:v for k,v in rows[i].items() if k not in ['Class','Y','Y2','Y3']} for i in [23,18,20]])
        expected=apply(model,name+'_table',selected)
        run(['-apply_model',model,'-apply_dset',str(root/'betas.nii')+'[23,18,20]','-prefix',name+'_selected'])
        actual=read(root/(name+'_selected.applied.1D'))
        assert list(dict.fromkeys(r['SourceBrick'] for r in actual))==['23','18','20']
        for a,b in zip(actual,expected):
            assert a['Prediction']==b['Prediction'] and a['PredictedClass']==b['PredictedClass']
            if 'Probability' in a:assert a['Probability']==b['Probability']
        if 'Probability' in allrows[0]:
            groups={}
            for r in allrows:groups.setdefault((r['Unit'],r['SourceBrick']),[]).append(float(r['Probability']))
            assert all(abs(sum(v)-1)<1e-7 and all(0<=p<=1 for p in v) for v in groups.values())
        run(['-apply_model',model,'-apply_dset',str(root/'betas.nii')+'[999]','-prefix','bad_dset'],fail='selector')
    run(['-apply_model','decode_pipeline.model','-apply_dset',root/'betas.nii'],fail='nuisance columns require')
    run(['-apply_model','decode_none.model','-apply_dset',root/'betas.nii','-dataTableFile',new],fail='exactly one')
    # More than two classes, source time metadata, and original brick selectors.
    three=table('three_classes',[dict(r,Class=['A','B','C'][i%3]) for i,r in enumerate(rows[:18])])
    args=train_command('three_model',extra=['-method','logistic','-logistic_lambda','auto','-inner_cv_column','Fold',
                                             '-save_tuning','three_tuning.1D'])
    args[args.index('-dataTableFile')+1]=three;run(args)
    raw=bytearray((root/'betas.nii').read_bytes());raw[123]=10  # mm and seconds
    struct.pack_into('<f',raw,92,2.5);struct.pack_into('<f',raw,136,1.25)
    (root/'time_series.nii').write_bytes(raw)
    run(['-apply_model','three_model.model','-apply_dset',str(root/'time_series.nii')+'[23,18,20]',
         '-prefix','three_applied'])
    result=read(root/'three_applied.applied.1D')
    assert len(result)==2*3*3
    for i in range(0,len(result),3):
        assert abs(sum(float(r['Probability']) for r in result[i:i+3])-1)<1e-7
        assert float(result[i]['TimeSeconds'])==1.25+2.5*int(result[i]['SourceBrick'])
    # Test AFNI HEAD/BRIK input when the companion converter is available.
    copier=Path(binary).parent/'macos_13_ARM'/'3dcopy'
    if copier.is_file():
        p=subprocess.run([str(copier),str(root/'time_series.nii'),str(root/'native')],capture_output=True,text=True)
        assert p.returncode==0,p.stderr
        run(['-apply_model','three_model.model','-apply_dset',str(root/'native+orig')+'[23,18,20]','-prefix','native_applied'])
        native=read(root/'native_applied.applied.1D')
        assert [(r['Prediction'],r['Probability'],r['SourceBrick'],r['TimeSeconds']) for r in native]==[(r['Prediction'],r['Probability'],r['SourceBrick'],r['TimeSeconds']) for r in result]
    # Logistic v2 estimator tags are strict; old v1 ridge files remain readable.
    goodlog=(root/'three_model.model').read_text()
    (root/'badlog.model').write_text(goodlog.replace('ESTIMATOR 1','ESTIMATOR 99'))
    apply('badlog.model','badlog',fail='malformed')
print('PASS saved models: training-only ridge/logistic, dataset application and ROI/class/TR probabilities, exact float round trips, target-free and batch-independent application, full pipeline and per-response lambdas, classes, partitions, workers, null restoration, version/schema/grid rejection and atomic output protection')
