#!/usr/bin/env python3
"""Independent selection/ridge/centroid oracles, leakage and exact-null checks."""
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


def nifti(path, volumes):
    p = len(volumes[0]); h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    struct.pack_into('<8h', h, 40, 4, p, 1, 1, len(volumes), 1, 1, 1)
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


def solve(a, b):
    a = [list(row)+[v] for row, v in zip(a, b)]
    for j in range(len(a)):
        pivot = max(range(j, len(a)), key=lambda i: abs(a[i][j]))
        a[j], a[pivot] = a[pivot], a[j]
        d = a[j][j]; assert abs(d) > 1e-12
        a[j] = [v/d for v in a[j]]
        for i in range(len(a)):
            if i != j:
                d = a[i][j]; a[i] = [v-d*w for v, w in zip(a[i], a[j])]
    return [row[-1] for row in a]


def preprocess(x, train, z=None, scale='feature_zscore'):
    x = [list(row) for row in x]; p = len(x[0]); n = len(train)
    if z is not None:
        d = [[1, v] for v in z]
        gram = [[sum(d[i][j]*d[i][k] for i in train) for k in range(2)] for j in range(2)]
        beta = [solve(gram, [sum(d[i][j]*x[i][f] for i in train) for j in range(2)]) for f in range(p)]
        x = [[v-sum(a*b for a,b in zip(d[i], beta[f])) for f,v in enumerate(row)] for i,row in enumerate(x)]
    if scale == 'feature_zscore':
        mean = [sum(x[i][j] for i in train)/n for j in range(p)]
        sd = [math.sqrt(sum((x[i][j]-mean[j])**2 for i in train)/n) for j in range(p)]
        x = [[(v-mean[j])/sd[j] if sd[j] > 1e-10 else 0 for j,v in enumerate(row)] for row in x]
    elif scale == 'sample_center':
        x = [[v-sum(row)/p for v in row] for row in x]
    elif scale == 'run_center':
        mean = [sum(x[i][j] for i in train)/n for j in range(p)]
        x = [[v-mean[j] for j,v in enumerate(row)] for row in x]
    return x


def ranking(x, y, train, classes=False):
    scores = []
    for j in range(len(x[0])):
        v = [x[i][j] for i in train]; mean = sum(v)/len(v)
        v = [a-mean for a in v]; ss = sum(a*a for a in v)
        score = 0
        if ss > 1e-20 and classes:
            labels = [y[i] for i in train]
            score = sum(sum(a for a,c in zip(v, labels) if c == label)**2/labels.count(label)
                        for label in set(labels))/ss
        elif ss > 1e-20:
            valid = 0
            for o in range(len(y[0])):
                u = [y[i][o] for i in train]; m = sum(u)/len(u); u = [a-m for a in u]
                ys = sum(a*a for a in u)
                if ys > 1e-20:
                    score += sum(a*b for a,b in zip(u,v))**2/ys/ss; valid += 1
            score = score/valid if valid else 0
        scores.append(max(0, min(1, score)))
    return sorted(range(len(scores)), key=lambda j: (-scores[j], j)), scores


def ridge(x, y, train, test, k, lam=1, z=None, scale='feature_zscore', labels=None):
    x = preprocess(x, train, z, scale)
    ids, scores = ranking(x, labels if labels is not None else y, train, labels is not None)
    ids = ids[:k]; x = [[r[j] for j in ids] for r in x]; p = len(ids); n = len(train)
    mean = [sum(x[i][j] for i in train)/n for j in range(p)]
    x = [[v-mean[j] for j,v in enumerate(row)] for row in x]
    ym = [sum(y[i][o] for i in train)/n for o in range(len(y[0]))]
    gram = [[sum(x[i][j]*x[i][l] for i in train)+(lam if j == l else 0) for l in range(p)] for j in range(p)]
    beta = [solve(gram, [sum(x[i][j]*(y[i][o]-ym[o]) for i in train) for j in range(p)]) for o in range(len(ym))]
    return [[ym[o]+sum(v*w for v,w in zip(x[i], beta[o])) for o in range(len(ym))] for i in test], ym, ids, scores


def centroid(x, labels, train, test, k, z=None):
    x = preprocess(x, train, z)
    ids, scores = ranking(x, labels, train, True); ids = ids[:k]
    levels = sorted(set(labels[i] for i in train))
    centers = {c: [sum(x[i][j] for i in train if labels[i] == c)/sum(labels[i] == c for i in train) for j in ids] for c in levels}
    return [min(levels, key=lambda c: sum((x[i][j]-v)**2 for j,v in zip(ids, centers[c]))) for i in test], ids, scores


def close(a, b):
    assert math.isclose(float(a), float(b), abs_tol=5e-5, rel_tol=5e-5), (a,b)


with tempfile.TemporaryDirectory(prefix='mvpa_selection_') as folder:
    root = Path(folder); n = 16; train = list(range(12)); test = list(range(12,16))
    labels = [i%2 for i in range(n)]
    z = [-1 if i%4 < 2 else 1 for i in range(n)]
    # Ranking changes across inner folds, and the selected subset is not a prefix.
    x = [[math.sin(i*1.7), 2*(i%2)+.8*math.cos(i*.9),
          (i%2)*(.1 if i//4==1 else 2.7)+math.sin(i*.4), 7.] for i in range(n)]
    y = [[1.3*r[2]-.4*r[0]+.3*math.cos(i), .7*r[1]+.2*math.sin(i*.8)] for i,r in enumerate(x)]
    design = [[r[1], r[0], r[2]] for r in x]
    nifti(root/'betas.nii', x); nifti(root/'mask.nii', [[1]*4])
    rows = [dict(Sample=f's{i}', Run='all', Fold=str(i//4), Split='train' if i<12 else 'test',
                 Block=str(i//2), Condition='A' if labels[i]==0 else 'B', Z=str(z[i]),
                 Y=str(y[i][0]), Y2=str(y[i][1]), D0=str(design[i][0]), D1=str(design[i][1]), D2=str(design[i][2]),
                 InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name, records):
        path = root/name
        with path.open('w') as f:
            w = csv.DictWriter(f, list(records[0]), delimiter='\t'); w.writeheader(); w.writerows(records)
        return path
    source = table('samples.1D', rows)
    def run(args, failure=None, workers=2):
        r = subprocess.run([binary, *map(str,args)], cwd=root, capture_output=True, text=True,
                           env=dict(os.environ, OMP_NUM_THREADS=str(workers)), timeout=90)
        text = r.stdout+r.stderr
        assert (r.returncode == 0) if failure is None else (r.returncode != 0 and failure in text), text
        return text
    def command(name, mode='classify', extra=(), src=source, k='2'):
        return ['-mode',mode,'-dataTableFile',src,'-partition_column','Split','-mask',root/'mask.nii',
                '-design_columns' if mode=='encode' else '-target_columns',
                'D0,D1,D2' if mode=='encode' else 'Y,Y2' if mode=='decode' else 'Condition',
                '-prefix',name,'-save_predictions',name+'.pred.1D','-save_fold_scores',name+'.fold.1D',
                *(['-score','pattern'] if mode=='encode' else []),
                *(['-select_features',k] if k is not None else []), *extra]
    def check_ids(name, ids, scores, encoding=False):
        audit=read(root/(name+'.features.1D'))
        assert [int(r['FeatureIndex']) for r in audit] == ids, (name,audit,ids)
        for rank,r in enumerate(audit):
            assert r['Feature'] == ('D'+str(ids[rank]) if encoding else 'voxel_'+str(ids[rank]))
            assert int(r['Rank']) == rank+1
            close(r['SelectionScore'],scores[ids[rank]])
    # Independent multivariate continuous oracle, all scaling options and nuisance composition.
    for mode in ['decode','encode']:
        for scale in ['none','feature_zscore','sample_center','run_center']:
            for adjust in [False,True]:
                name=f'{mode}_{scale}_{adjust}'
                run(command(name,mode,['-scale',scale,*(['-nuisance_columns','Z'] if adjust else [])]))
                xx,yy=(x,y) if mode=='decode' else (design,x)
                expected,means,ids,scores=ridge(xx,yy,train,test,2,z=z if adjust else None,scale=scale)
                check_ids(name,ids,scores,mode=='encode')
                for r in read(root/(name+'.pred.1D')):
                    i=int(r['Sample'][1:])
                    if i in test:
                        o=['Y','Y2'].index(r['Response']) if mode=='decode' else int(r['Response'].split('_')[1])
                        close(r['Predicted'],expected[test.index(i)][o]);close(r['TrainingMean'],means[o])
    # Every classifier consumes the selected subset; equivalent external masking agrees.
    ids,scores=ranking(preprocess(x,train),labels,train,True)
    mask=[int(j in ids[:2]) for j in range(4)];nifti(root/'selected_mask.nii',[mask])
    for method in ['centroid','ridge','svm','lda_diag','lda_shrinkage']:
        run(command(method,extra=['-method',method]));check_ids(method,ids[:2],scores)
        args=command(method+'_mask',extra=['-method',method],k=None);args[args.index(root/'mask.nii')]=root/'selected_mask.nii';run(args)
        a,b=read(root/(method+'.pred.1D')),read(root/(method+'_mask.pred.1D'))
        for r,t in zip(a,b):
            assert r['Predicted']==t['Predicted']
            if r['Predicted']!='NA':
                for col in ['DecisionClass0','DecisionClass1']:close(r[col],t[col])
    # Joint grouped count/lambda oracle, with nuisance adjustment separately refitted per split.
    counts=[4,1,2];penalties=[.1,1,10];loss={};inner_rank=[]
    for k,lam in itertools.product(counts,penalties):
        sse=[0.,0.];base=[0.,0.]
        for group in range(3):
            tr=[i for i in train if i//4!=group];te=[i for i in train if i//4==group]
            pred,mean,ids,scores=ridge(x,y,tr,te,k,lam,z)
            if k==1 and lam==.1:inner_rank.append(ids)
            for i,v in zip(te,pred):
                for o in range(2):sse[o]+=(y[i][o]-v[o])**2;base[o]+=(y[i][o]-mean[o])**2
        loss[k,lam]=sum(a/b for a,b in zip(sse,base))/2
    assert len({tuple(r) for r in inner_rank})>1,inner_rank
    winner=min(loss,key=lambda pair:(loss[pair],pair[0],-pair[1]))
    opts=['-select_features_grid','4,1,2','-lambda_grid','.1,1,10','-inner_cv_column','Fold','-nuisance_columns','Z']
    run(command('joint','decode',opts,k=None))
    audit=read(root/'joint.feature_tuning.1D');assert len(audit)==9
    for r in audit:
        pair=(int(r['RequestedCount']),float(r['Penalty']));close(r['InnerLoss'],loss[pair]);assert int(r['Selected'])==int(pair==winner)
    expected,mean,ids,scores=ridge(x,y,train,test,*winner,z=z);check_ids('joint',ids,scores)
    for r in read(root/'joint.pred.1D'):
        i=int(r['Sample'][1:]);o=['Y','Y2'].index(r['Response'])
        if i in test:close(r['Predicted'],expected[test.index(i)][o])
    # Independent centroid nested-count objective; fixed-count SVM/lambda tuning also run selection.
    opts_class=['-select_features_grid','4,1,2','-inner_cv_column','Fold']
    run(command('centroid_grid',extra=opts_class,k=None))
    closs={}
    for k in counts:
        truth=[];pred=[]
        for group in range(3):
            tr=[i for i in train if i//4!=group];te=[i for i in train if i//4==group]
            pp,_,_=centroid(x,labels,tr,te,k);pred+=pp;truth += [labels[i] for i in te]
        closs[k]=1-sum(a==b for a,b in zip(truth,pred))/len(pred)
    chosen=min(counts,key=lambda k:(closs[k],k))
    for r in read(root/'centroid_grid.feature_tuning.1D'):
        k=int(r['RequestedCount']);close(r['InnerLoss'],closs[k]);assert int(r['Selected'])==int(k==chosen)
    for method in ['ridge','svm','lda_diag','lda_shrinkage']:
        run(command(method+'_grid',extra=['-method',method,*opts_class],k=None))
        assert sum(int(r['Selected']) for r in read(root/(method+'_grid.feature_tuning.1D')))==1
    run(command('svm_joint',extra=['-method','svm',*opts_class,'-svm_c_grid','.1,1'],k=None))
    assert len(read(root/'svm_joint.feature_tuning.1D'))==6
    for method,extra in [('svm',['-svm_c_grid','.1,1']),('ridge',['-lambda_grid','.1,1'])]:
        run(command(method+'_fixed_tune',extra=['-method',method,*extra,'-inner_cv_column','Fold']))
        assert len(read(root/(method+'_fixed_tune.feature_tuning.1D')))==2
    # Held-out neural values/outcomes/covariates must not change ranking, tuning or other predictions.
    changed_x=[list(r) for r in x];changed_x[-1]=[8000,-3000,2000,7];nifti(root/'changed.nii',changed_x)
    changed=table('changed.1D',[dict(r,InputFile=str(root/'changed.nii')+f'[{i}]',Z='700' if i==15 else r['Z'],Y='-9000' if i==15 else r['Y'],Condition='B' if i==14 else r['Condition']) for i,r in enumerate(rows)])
    for name,mode,extra in [('joint','decode',opts),('svm_joint','classify',['-method','svm',*opts_class,'-svm_c_grid','.1,1'])]:
        run(command(name+'_changed',mode,extra,src=changed,k=None))
        for suffix in ['features.1D','feature_tuning.1D','tuning.1D']:
            assert (root/(name+'.'+suffix)).read_bytes()==(root/(name+'_changed.'+suffix)).read_bytes(),(name,suffix)
        for r,t in zip(read(root/(name+'.pred.1D')),read(root/(name+'_changed.pred.1D'))):
            if r['Sample']!='s15':assert r['Predicted']==t['Predicted']
    # Multiclass, imbalanced groups distinguish eta-squared from averaging one-hot r^2.
    multi_labels=[0,0,1,2]*4
    multi=table('multi.1D',[dict(r,Condition='ABC'[multi_labels[i]]) for i,r in enumerate(rows)])
    run(command('multi_ridge',extra=['-method','ridge'],src=multi))
    onehot=[[float(c==label) for c in range(3)] for label in multi_labels]
    pred,_,ids,scores=ridge(x,onehot,train,test,2,labels=multi_labels)
    check_ids('multi_ridge',ids,scores)
    for r in read(root/'multi_ridge.pred.1D'):
        i=int(r['Sample'][1:])
        if i in test:
            for c in range(3):close(r['DecisionClass'+str(c)],pred[test.index(i)][c])
    for metric in ['accuracy','balanced_accuracy']:
        name='multi_'+metric
        run(command(name,extra=[*opts_class,'-metric',metric],src=multi,k=None))
        losses={}
        for k in counts:
            truth=[];pred=[]
            for group in range(3):
                tr=[i for i in train if i//4!=group];te=[i for i in train if i//4==group]
                pp,_,_=centroid(x,multi_labels,tr,te,k);pred+=pp;truth += [multi_labels[i] for i in te]
            if metric=='accuracy':score=sum(a==b for a,b in zip(truth,pred))/len(pred)
            else:score=sum(sum(a==c and b==c for a,b in zip(truth,pred))/truth.count(c) for c in range(3))/3
            losses[k]=1-score
        chosen=min(counts,key=lambda k:(losses[k],k))
        for r in read(root/(name+'.feature_tuning.1D')):
            k=int(r['RequestedCount']);close(r['InnerLoss'],losses[k]);assert int(r['Selected'])==int(k==chosen)
    # Independently pool inner predictions to verify the joint SVM candidate losses.
    svm_losses={}
    for k,cost in itertools.product(counts,[.1,1.]):
        correct=0
        for group in range(3):
            inner_rows=[dict(r,Split='test' if i//4==group else 'train') for i,r in enumerate(rows[:12])]
            source_inner=table(f'inner_{k}_{cost}_{group}.1D',inner_rows)
            name=f'inner_{k}_{cost}_{group}'
            run(command(name,extra=['-method','svm','-svm_c',str(cost)],src=source_inner,k=str(k)))
            correct+=sum(r['Observed']==r['Predicted'] for r in read(root/(name+'.pred.1D')) if r['Predicted']!='NA')
        svm_losses[k,cost]=1-correct/12
    chosen=min(svm_losses,key=lambda pair:(svm_losses[pair],pair[0],pair[1]))
    for r in read(root/'svm_joint.feature_tuning.1D'):
        pair=(int(r['RequestedCount']),float(r['Penalty']));close(r['InnerLoss'],svm_losses[pair]);assert int(r['Selected'])==int(pair==chosen)
    # Exact 2^8 blocked null: selection is recomputed for each relabeling, including nested counts.
    run(command('null',extra=['-nperm','999','-permute_within','Block',*opts_class],k=None))
    null=[];rankings=set()
    for swaps in itertools.product([0,1],repeat=8):
        perm_labels=[labels[i^swaps[i//2]] for i in range(n)]
        losses={}
        for k in counts:
            correct=0
            for group in range(3):
                tr=[i for i in train if i//4!=group];te=[i for i in train if i//4==group]
                pp,_,_=centroid(x,perm_labels,tr,te,k);correct+=sum(v==perm_labels[i] for i,v in zip(te,pp))
            losses[k]=1-correct/len(train)
        k=min(counts,key=lambda k:(losses[k],k))
        pred,ids,_=centroid(x,perm_labels,train,test,k);rankings.add(tuple(ids))
        null.append(sum(v==perm_labels[i] for i,v in zip(test,pred))/len(test))
    assert len(rankings)>1
    r=read(root/'null.roi.1D')[0];close(r['PVAL'],sum(v>=null[0] for v in null)/len(null))
    for suffix in ['features.1D','feature_tuning.1D']:
        assert (root/('null.'+suffix)).read_bytes()==(root/('centroid_grid.'+suffix)).read_bytes()
    # Continuous permutation refits, including encoding predictor-row permutations.
    for mode in ['decode','encode']:
        run(command('null_'+mode,mode,['-nperm','999','-permute_within','Block','-metric','RMSE']))
        allscores=[]
        for swaps in itertools.product([0,1],repeat=8):
            perm=[i^swaps[i//2] for i in range(n)]
            xx,yy=(x,[y[i] for i in perm]) if mode=='decode' else ([design[i] for i in perm],x)
            pp,_,_,_=ridge(xx,yy,train,test,2)
            ss=[math.sqrt(sum((yy[i][o]-v[o])**2 for i,v in zip(test,pp))/len(test)) for o in range(len(yy[0]))]
            allscores.append(ss if mode=='decode' else [sum(ss)/len(ss)])
        for o,r in enumerate(read(root/('null_'+mode+'.inference.1D'))):
            scores=[ss[o] for ss in allscores];close(r['SCORE'],scores[0]);close(r['NULL_MEAN'],sum(scores)/len(scores))
            close(r['PVAL'],sum(v<=scores[0]+1e-8 for v in scores)/len(scores))
    # Edge-width clipping, deterministic workers and original domain mapping.
    for workers in [1,2]:run(command('spatial'+str(workers),extra=['-searchlight','SPHERE(1)',*opts_class],k=None),workers=workers)
    for suffix in ['features.1D','feature_tuning.1D','pred.1D']:
        assert (root/('spatial1.'+suffix)).read_bytes()==(root/('spatial2.'+suffix)).read_bytes()
    for r in read(root/'spatial1.features.1D'):
        assert int(r['EffectiveCount'])<=int(r['InputNFeat'])
        assert abs(int(r['Feature'].split('_')[1])-int(r['Unit']))<=1
    for mode in ['decode','encode']:
        for workers in [1,2]:
            run(command(mode+'_spatial'+str(workers),mode,['-searchlight','SPHERE(1)','-select_features_grid','1,2','-inner_cv_column','Fold'],k=None),workers=workers)
        for suffix in ['features.1D','feature_tuning.1D','pred.1D']:
            assert (root/(mode+'_spatial1.'+suffix)).read_bytes()==(root/(mode+'_spatial2.'+suffix)).read_bytes()
    run(command('voxel','encode',['-score','voxel'],k='1'))
    for unit in range(4):
        yy=[[r[unit]] for r in x]
        expected,means,ids,scores=ridge(design,yy,train,test,1)
        audit=[r for r in read(root/'voxel.features.1D') if int(r['Unit'])==unit]
        assert len(audit)==1 and audit[0]['Feature']=='D'+str(ids[0]);close(audit[0]['SelectionScore'],scores[ids[0]])
        for r in read(root/'voxel.pred.1D'):
            i=int(r['Sample'][1:])
            if int(r['Unit'])==unit and i in test:close(r['Predicted'],expected[test.index(i)][0])
    # Single/two-table and group-CV paths.
    tr=table('train.1D',rows[:12]);te=table('test.1D',rows[12:])
    args=command('dual');at=args.index('-partition_column');del args[at:at+2];args[args.index(source)]=tr;args+=['-testTableFile',te];run(args)
    assert (root/'dual.features.1D').read_bytes()==(root/'centroid.features.1D').read_bytes()
    args=command('grouped',extra=opts_class,k=None);at=args.index('-partition_column');args[at:at+2]=['-cv_column','Fold'];run(args)
    assert len({r['CVFold'] for r in read(root/'grouped.features.1D')})==4
    # Deterministic ties/constant features and more predictors than training samples.
    constant=[[1.]*4 for _ in range(n)];nifti(root/'constant.nii',constant)
    const=table('constant.1D',[dict(r,InputFile=str(root/'constant.nii')+f'[{i}]',Y='3',Y2='4') for i,r in enumerate(rows)])
    run(command('ties','decode',['-select_features_grid','4,2,1','-lambda_grid','1,10','-inner_cv_column','Fold'],src=const,k=None))
    r=read(root/'ties.features.1D');assert len(r)==1 and r[0]['FeatureIndex']=='0' and float(r[0]['SelectionScore'])==0
    selected=[r for r in read(root/'ties.feature_tuning.1D') if r['Selected']=='1'];assert len(selected)==1 and selected[0]['RequestedCount']=='1' and float(selected[0]['Penalty'])==10
    wide=[[r[j%3] if j<60 else 9 for j in range(64)] for r in x];nifti(root/'wide.nii',wide);nifti(root/'wide_mask.nii',[[1]*64])
    wide_src=table('wide.1D',[dict(r,InputFile=str(root/'wide.nii')+f'[{i}]') for i,r in enumerate(rows)])
    args=command('wide','decode',src=wide_src,k='24');args[args.index(root/'mask.nii')]=root/'wide_mask.nii';run(args)
    assert len(read(root/'wide.features.1D'))==24
    # Validation is read-only and rejects malformed/incompatible requests.
    report=run(command('validate',extra=[*opts_class,'-validate_only'],k=None));assert '# FeatureSelectionFitted\tno_validation_only' in report
    assert not (root/'validate.features.1D').exists()
    failures=[(['-select_features_grid','1'],'at least two'),(['-select_features_grid','1,1'],'duplicate'),
              (['-select_features','0'],'feature count'),(['-select_features','1.5'],'feature count'),
              (['-select_features_grid','1,2'],'explicit -inner_cv_column'),
              (['-select_features','1','-select_features_grid','1,2'],'choose only one'),
              (['-select_features','1','-save_coefficients','coef'],'cannot yet export'),
              (['-select_features','1','-save_predictions','bad.features.1D'],'filenames must differ')]
    for extra,msg in failures:run(command('bad',extra=[*extra,'-validate_only'],k=None),msg)
print('PASS feature selection: independent rankings/predictions, nested grids, held-out isolation, exact nulls, clipping, high-dimensional ties and input contracts')
