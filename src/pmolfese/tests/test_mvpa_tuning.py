#!/usr/bin/env python3
"""Independent inner-CV objective/candidate oracles and default compatibility."""
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


def objective(truth, pred, baseline, name):
    if name in ['accuracy','balanced_accuracy']:
        actual=[max(range(len(r)),key=lambda j:r[j]) for r in truth]
        predicted=[max(range(len(r)),key=lambda j:r[j]) for r in pred]
        if name=='accuracy':return 1-sum(a==b for a,b in zip(actual,predicted))/len(actual)
        return 1-sum(sum(a==c and b==c for a,b in zip(actual,predicted))/actual.count(c) for c in set(actual))/len(set(actual))
    losses=[]
    for o in range(len(truth[0])):
        errors=[a[o]-b[o] for a,b in zip(truth,pred)]
        if name=='normalized_mse':
            base=sum((a[o]-b[o])**2 for a,b in zip(truth,baseline))
            if not base:continue
            loss=sum(e*e for e in errors)/base
        elif name=='MAE':loss=sum(abs(e) for e in errors)/len(errors)
        else:
            loss=sum(e*e for e in errors)/len(errors)
            if name=='RMSE':loss=math.sqrt(loss)
        losses.append(loss)
    return sum(losses)/len(losses) if losses else float('nan')


def candidates(x,y,train,groups,counts,penalties,metric,z=None,labels=None):
    losses={}
    for k,lam in itertools.product(counts,penalties):
        truth=[];pred=[];baseline=[]
        for group in sorted(set(groups[i] for i in train)):
            tr=[i for i in train if groups[i]!=group];te=[i for i in train if groups[i]==group]
            pp,means,_,_=ridge(x,y,tr,te,k,lam,z=z,labels=labels)
            truth.extend(y[i] for i in te);pred.extend(pp);baseline.extend([means]*len(te))
        losses[k,lam]=objective(truth,pred,baseline,metric)
    return losses


with tempfile.TemporaryDirectory(prefix='mvpa_tuning_') as folder:
    root=Path(folder);n=24;train=list(range(18));test=list(range(18,24))
    rng=random.Random(17);labels=[int(i%3==2) for i in range(n)]
    groups=[0 if i<4 else 1 if i<10 else 2 if i<18 else 3 for i in range(n)]
    z=[-1 if i%4<2 else 1 for i in range(n)]
    x=[[rng.gauss(0,1),.8*labels[i]+rng.gauss(0,1),rng.gauss(0,1),5.] for i in range(n)]
    y=[[r[0]+.2*r[1]+rng.gauss(0,.4),20*r[1]+rng.gauss(0,30)] for r in x]
    y[3][1]+=150
    design=[[r[2],r[0],r[1]] for r in x]
    nifti(root/'betas.nii',x);nifti(root/'mask.nii',[[1]*4])
    rows=[dict(Sample=f's{i}',Run='all',Fold=str(groups[i]),Split='train' if i<18 else 'test',
               Condition='AB'[labels[i]],Z=str(z[i]),Y=str(y[i][0]),Y2=str(y[i][1]),
               D0=str(design[i][0]),D1=str(design[i][1]),D2=str(design[i][2]),
               InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name,records):
        path=root/name
        with path.open('w') as f:
            w=csv.DictWriter(f,list(records[0]),delimiter='\t');w.writeheader();w.writerows(records)
        return path
    source=table('samples.1D',rows)
    def run(args,failure=None,workers=2,exe=binary):
        result=subprocess.run([exe,*map(str,args)],cwd=root,capture_output=True,text=True,
                              env=dict(os.environ,OMP_NUM_THREADS=str(workers)),timeout=90)
        output=result.stdout+result.stderr
        assert (result.returncode==0) if failure is None else (result.returncode!=0 and failure in output),output
        return output
    def command(name,mode='decode',method='ridge',extra=(),src=source,tuning=True):
        return ['-mode',mode,'-method',method,'-dataTableFile',src,'-partition_column','Split',
                '-mask',root/'mask.nii','-prefix',name,'-save_predictions',name+'.pred.1D',
                '-save_fold_scores',name+'.fold.1D',
                '-design_columns' if mode=='encode' else '-target_columns',
                'D0,D1,D2' if mode=='encode' else 'Condition' if mode=='classify' else 'Y,Y2',
                *(['-score','pattern'] if mode=='encode' else []),
                *(['-svm_max_iter','100000'] if method=='svm' else []),
                *(['-inner_cv_column','Fold','-select_features_grid','1,2,4',
                   *(['-lambda_grid','.01,1,100'] if method=='ridge' else ['-svm_c_grid','.01,1,100'] if method=='svm' else [])] if tuning else []),*extra]
    def audit(name):return read(root/(name+'.candidates.1D'))
    def check_audit(name,losses,metric):
        aa=audit(name);winner=min(losses,key=lambda pair:(losses[pair],pair[0],-pair[1]))
        assert len(aa)==len(losses)
        for r in aa:
            pair=int(r['RequestedCount']),float(r['Penalty'])
            close(r['InnerLoss'],losses[pair]);assert r['Objective']==metric
            assert int(r['InnerNTest'])==len(train) and int(r['Selected'])==int(pair==winner)
        return winner
    counts=[1,2,4];penalties=[.01,1,100];winners={}
    # All continuous objectives, including multi-response weighting and nuisance fits.
    for metric in ['normalized_mse','MSE','RMSE','MAE']:
        name='decode_'+metric
        run(command(name,extra=['-tune_metric',metric,'-nuisance_columns','Z','-save_tuning',name+'.candidates.1D']))
        losses=candidates(x,y,train,groups,counts,penalties,metric,z=z)
        winners[metric]=winner=check_audit(name,losses,metric)
        expected,_,_,_=ridge(x,y,train,test,*winner,z=z)
        for r in read(root/(name+'.pred.1D')):
            i=int(r['Sample'][1:])
            if i in test:close(r['Predicted'],expected[test.index(i)][['Y','Y2'].index(r['Response'])])
    assert len(set(winners.values()))>1,winners
    # Encoding uses equally weighted per-response errors, including constant outputs.
    run(command('encode','encode',extra=['-tune_metric','MAE','-save_tuning','encode.candidates.1D']))
    check_audit('encode',candidates(design,x,train,groups,counts,penalties,'MAE'),'MAE')
    # Classifying ridge now selects by ACC/BACC rather than always using one-hot squared error.
    onehot=[[float(c==label) for c in range(2)] for label in labels]
    class_winners={}
    for metric in ['accuracy','balanced_accuracy','normalized_mse']:
        name='ridge_'+metric
        run(command(name,'classify',extra=['-metric','accuracy','-tune_metric',metric,'-save_tuning',name+'.candidates.1D']))
        losses=candidates(x,onehot,train,groups,counts,penalties,metric,labels=labels)
        class_winners[metric]=check_audit(name,losses,metric)
    # Tune objective can differ from the outer metric for every classifier.
    for method in ['centroid','svm','lda_diag','lda_shrinkage']:
        name=method+'_bacc'
        run(command(name,'classify',method,extra=['-metric','accuracy','-tune_metric','BACC','-save_tuning',name+'.candidates.1D']))
        for r in audit(name):assert r['Objective']=='balanced_accuracy'
        assert sum(int(r['Selected']) for r in audit(name))==1
        # Reference run sets the preexisting -metric BACC tuning behavior explicitly.
        ref=method+'_reference'
        run(command(ref,'classify',method,extra=['-metric','balanced_accuracy','-save_tuning',ref+'.candidates.1D']))
        assert audit(name)==audit(ref)
        for a,b in zip(read(root/(name+'.pred.1D')),read(root/(ref+'.pred.1D'))):
            assert a['Predicted']==b['Predicted']
    # Default audits/predictions remain byte-identical to the pre-#33 executable.
    baseline=sys.argv[2] if len(sys.argv)>2 else None
    for mode,method in [('decode','ridge'),('encode','ridge'),('classify','ridge'),('classify','svm'),('classify','centroid')]:
        name='default_'+mode+'_'+method;run(command(name,mode,method))
        explicit=name+'_explicit';run(command(explicit,mode,method,['-tune_metric','default']))
        suffixes=['pred.1D','fold.1D','features.1D','feature_tuning.1D']+(['tuning.1D'] if method in ['ridge','svm'] else [])
        for suffix in suffixes:assert (root/(name+'.'+suffix)).read_bytes()==(root/(explicit+'.'+suffix)).read_bytes(),(name,suffix)
        if baseline:
            old=name+'_old';run(command(old,mode,method),exe=baseline)
            for suffix in suffixes:
                aa=(root/(name+'.'+suffix)).read_text();bb=(root/(old+'.'+suffix)).read_text()
                assert aa==bb,(name,suffix,''.join(difflib.unified_diff(bb.splitlines(True),aa.splitlines(True))))
    # Penalty-only searches produce the same common schema and all-input feature counts.
    args=command('penalty',extra=['-tune_metric','MAE','-save_tuning','penalty.candidates.1D'])
    at=args.index('-select_features_grid');del args[at:at+2];run(args)
    check_audit('penalty',candidates(x,y,train,groups,[4],penalties,'MAE'),'MAE')
    assert all(r['FeatureSelection']=='0' for r in audit('penalty'))
    # Held-out outcomes, covariates and neural patterns cannot change candidates or other predictions.
    changed_x=[list(r) for r in x];changed_x[-1]=[10000,-9000,5000,5];nifti(root/'changed.nii',changed_x)
    changed=table('changed.1D',[dict(r,InputFile=str(root/'changed.nii')+f'[{i}]',Y='9999' if i==23 else r['Y'],Z='900' if i==23 else r['Z']) for i,r in enumerate(rows)])
    run(command('changed',src=changed,extra=['-tune_metric','MAE','-nuisance_columns','Z','-save_tuning','changed.candidates.1D']))
    assert audit('changed')==audit('decode_MAE')
    assert (root/'changed.features.1D').read_bytes()==(root/'decode_MAE.features.1D').read_bytes()
    for a,b in zip(read(root/'changed.pred.1D'),read(root/'decode_MAE.pred.1D')):
        if a['Sample']!='s23':assert a['Predicted']==b['Predicted']
    # Constant response contracts: normalized loss undefined; raw errors defined.
    constant=table('constant.1D',[dict(r,Y='3',Y2='5') for r in rows])
    for metric in ['normalized_mse','MSE','RMSE','MAE']:
        name='constant_'+metric;run(command(name,src=constant,extra=['-tune_metric',metric,'-save_tuning',name+'.candidates.1D']))
        selected=[r for r in audit(name) if r['Selected']=='1'];assert len(selected)==1
        assert selected[0]['RequestedCount']=='1' and float(selected[0]['Penalty'])==100
        for r in audit(name):
            assert int(r['InnerNValid'])==(0 if metric=='normalized_mse' else 2)
            assert math.isnan(float(r['InnerLoss'])) if metric=='normalized_mse' else float(r['InnerLoss'])==0
    # Spatial workers run the identical candidate evaluation without shared mutable scratch.
    for mode,method,metric in [('decode','ridge','MAE'),('encode','ridge','RMSE'),('classify','ridge','BACC'),('classify','svm','ACC')]:
        names=[]
        for workers in [1,2]:
            name=f'spatial_{mode}_{method}_{workers}';names.append(name)
            run(command(name,mode,method,['-searchlight','SPHERE(1)','-tune_metric',metric,'-save_tuning',name+'.candidates.1D']),workers=workers)
        for suffix in ['candidates.1D','pred.1D','features.1D']:
            assert (root/(names[0]+'.'+suffix)).read_bytes()==(root/(names[1]+'.'+suffix)).read_bytes()
    # Explicit partitions and two-table input use the same training-only objective.
    tr=table('train.1D',rows[:18]);te=table('test.1D',rows[18:])
    args=command('dual',extra=['-tune_metric','MAE','-nuisance_columns','Z','-save_tuning','dual.candidates.1D'])
    at=args.index('-partition_column');del args[at:at+2];args[args.index(source)]=tr;args+=['-testTableFile',te];run(args)
    assert audit('dual')==audit('decode_MAE')
    # Exact blocked nulls refit the explicitly selected objective and restore its audit.
    pn=12;ptrain=list(range(8));ptest=list(range(8,12));pgroups=[i//2 for i in range(pn)]
    plabels=[i%2 for i in range(pn)];pz=[-1 if i//2%2==0 else 1 for i in range(pn)]
    px=[[math.sin(i*1.3),1.2*(i%2)+.3*math.cos(i),math.cos(i*.7),3.] for i in range(pn)]
    py=[[r[0]+.4*r[1],2*r[2]+.2*r[1]] for r in px];pd=[[r[1],r[0],r[2]] for r in px]
    nifti(root/'null_betas.nii',px)
    prows=[dict(rows[i],Sample=f's{i}',Fold=str(pgroups[i]),Split='train' if i<8 else 'test',Block=str(i//2),
                Z=str(pz[i]),Condition='AB'[plabels[i]],Y=str(py[i][0]),Y2=str(py[i][1]),
                D0=str(pd[i][0]),D1=str(pd[i][1]),D2=str(pd[i][2]),InputFile=str(root/'null_betas.nii')+f'[{i}]') for i in range(pn)]
    psource=table('null.1D',prows)
    for mode,inner,outer in [('classify','accuracy','balanced_accuracy'),('decode','MAE','RMSE'),('encode','RMSE','MAE')]:
        name='null_'+mode
        def null_command(prefix,permute):
            args=command(prefix,mode,src=psource,extra=['-tune_metric',inner,'-metric',outer,
                         '-nuisance_columns','Z','-save_tuning',prefix+'.candidates.1D',
                         *(['-nperm','999','-permute_within','Block'] if permute else [])])
            args[args.index('-lambda_grid')+1]='.1,10';args[args.index('-select_features_grid')+1]='1,2'
            return args
        run(null_command(name,True));run(null_command(name+'_observed',False))
        assert audit(name)==audit(name+'_observed')
        scores=[]
        for swaps in itertools.product([0,1],repeat=6):
            perm=[i^swaps[i//2] for i in range(pn)]
            ll=[plabels[i] for i in perm] if mode=='classify' else None
            xx=[pd[i] for i in perm] if mode=='encode' else px
            yy=[[float(c==label) for c in range(2)] for label in ll] if ll is not None else px if mode=='encode' else [py[i] for i in perm]
            losses=candidates(xx,yy,ptrain,pgroups,[1,2],[.1,10],inner,z=pz,labels=ll)
            winner=min(losses,key=lambda pair:(losses[pair],pair[0],-pair[1]))
            pp,_,_,_=ridge(xx,yy,ptrain,ptest,*winner,z=pz,labels=ll)
            if mode=='classify':
                predicted=[max(range(2),key=lambda c:r[c]) for r in pp]
                scores.append([sum(a==ll[i] for a,i in zip(predicted,ptest))/len(ptest)])
            else:
                values=[]
                for o in range(len(yy[0])):
                    errors=[yy[i][o]-v[o] for i,v in zip(ptest,pp)]
                    values.append(sum(abs(e) for e in errors)/len(errors) if outer=='MAE' else math.sqrt(sum(e*e for e in errors)/len(errors)))
                scores.append([sum(values)/len(values)] if mode=='encode' else values)
        result=read(root/(name+('.roi.1D' if mode=='classify' else '.inference.1D')))
        for o,r in enumerate(result):
            values=[v[o] for v in scores];observed=values[0]
            close(r['PVAL'],sum(v>=observed-1e-8 if mode=='classify' else v<=observed+1e-8 for v in values)/len(values))
            if mode!='classify':close(r['NULL_MEAN'],sum(values)/len(values));close(r['SCORE'],observed)
    # Read-only validation and rejected unsupported or incoherent objectives.
    report=run(command('validate',extra=['-tune_metric','RMSE','-save_tuning','validate.candidates.1D','-validate_only']))
    assert '# InnerCVObjective\tRMSE' in report and not (root/'validate.candidates.1D').exists()
    for args,message in [(command('bad',extra=['-tune_metric','ACC']),'continuous tuning needs'),
                         (command('bad','classify',extra=['-tune_metric','MAE']),'classification tuning needs'),
                         (command('bad','classify','svm',['-tune_metric','normalized_mse']),'classification tuning needs'),
                         (command('bad','classify',extra=['-tune_metric','log_loss']),'probability losses'),
                         (command('bad',extra=['-tune_metric','MAE'],tuning=False),'require a lambda/C'),
                         (command('bad',extra=['-save_tuning','bad.tuning.1D']),'automatic result/audit'),
                         (command('bad',extra=['-save_tuning','bad.pred.1D']),'different files')]:
        run(args+['-validate_only'],message)
print('PASS generalized tuning: independent objectives/grids, default compatibility, held-out isolation, constant targets, common audits and spatial workers')
