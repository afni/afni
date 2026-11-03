#!/usr/bin/env python3
"""Independent per-response ridge selection, prediction and audit checks."""
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


def eigen(a):
    """Independent maximum-off-diagonal rotation reference for small matrices."""
    d=len(a);a=[r[:] for r in a];v=[[float(i==j) for j in range(d)] for i in range(d)]
    for _ in range(100*d*d):
        if d<2:break
        i,j=max(((i,j) for i in range(d) for j in range(i+1,d)),key=lambda ij:abs(a[ij[0]][ij[1]]))
        if abs(a[i][j])<1e-13*max(1,max(abs(a[k][k]) for k in range(d))):break
        angle=.5*math.atan2(2*a[i][j],a[j][j]-a[i][i]);c=math.cos(angle);s=math.sin(angle)
        # Explicit left/right matrix products, distinct from the native cyclic implementation.
        rot=[[float(i==j) for j in range(d)] for i in range(d)]
        rot[i][i]=rot[j][j]=c;rot[i][j]=s;rot[j][i]=-s
        ar=[[sum(a[r][k]*rot[k][col] for k in range(d)) for col in range(d)] for r in range(d)]
        a=[[sum(rot[k][r]*ar[k][col] for k in range(d)) for col in range(d)] for r in range(d)]
        v=[[sum(v[r][k]*rot[k][col] for k in range(d)) for col in range(d)] for r in range(d)]
    else:raise AssertionError('reference eigen solver did not converge')
    order=sorted(range(d),key=lambda i:-a[i][i])
    return [a[i][i] for i in order],[[v[j][i] for j in range(d)] for i in order]


def project(x,y,train,features,components,z=None,scale='feature_zscore',labels=None):
    x=preprocess(x,train,z,scale);p=len(x[0]);ids=list(range(p))
    if features is not None:ids=ranking(x,labels if labels is not None else y,train,labels is not None)[0][:features]
    x=[[r[j] for j in ids] for r in x];p=len(ids);n=len(train)
    mean=[sum(x[i][j] for i in train)/n for j in range(p)]
    x=[[v-mean[j] for j,v in enumerate(r)] for r in x]
    if components is None:return x,ids,mean,[],[],p
    gram=[[sum(x[i][j]*x[i][k] for i in train) for k in range(p)] for j in range(p)]
    values,vectors=eigen(gram);threshold=max(0,values[0])*64*sys.float_info.epsilon*max(n,p)
    rank=min(n-1,sum(v>threshold for v in values));count=min(components,rank)
    vectors=vectors[:count]
    for v in vectors:
        pivot=max(range(p),key=lambda j:abs(v[j]))
        if v[pivot]<0:v[:]=[-a for a in v]
    projected=[[sum(a*b for a,b in zip(row,v)) for v in vectors] if count else [0.] for row in x]
    return projected,ids,mean,vectors,[v/(n-1) for v in values[:count]],rank


def fit(x,y,train,test,features,components,penalty=1,z=None,scale='feature_zscore',labels=None,method='ridge'):
    xp,ids,mean,vectors,values,rank=project(x,y,train,features,components,z,scale,labels)
    p=len(xp[0]);n=len(train);ym=[sum(y[i][o] for i in train)/n for o in range(len(y[0]))]
    if method=='centroid':
        centers=[[sum(xp[i][j] for i in train if labels[i]==c)/sum(labels[i]==c for i in train) for j in range(p)] for c in range(2)]
        result=[[float(c==min(range(2),key=lambda a:sum((v-w)**2 for v,w in zip(xp[i],centers[a])))) for c in range(2)] for i in test]
    else:
        xm=[sum(xp[i][j] for i in train)/n for j in range(p)];xp=[[v-xm[j] for j,v in enumerate(row)] for row in xp]
        gram=[[sum(xp[i][j]*xp[i][k] for i in train)+(penalty if j==k else 0) for k in range(p)] for j in range(p)]
        beta=[solve(gram,[sum(xp[i][j]*(y[i][o]-ym[o]) for i in train) for j in range(p)]) for o in range(len(y[0]))]
        result=[[ym[o]+sum(a*b for a,b in zip(xp[i],beta[o])) for o in range(len(ym))] for i in test]
    return result,ym,(ids,mean,vectors,values,rank)


def search(x,y,train,groups,counts,components,penalties,metric,z=None,labels=None,method='ridge'):
    losses={};ranges={}
    for k,pc,lam in itertools.product(counts,components,penalties):
        truth=[];pred=[];base=[];effective=[]
        for group in sorted(set(groups[i] for i in train)):
            tr=[i for i in train if groups[i]!=group];te=[i for i in train if groups[i]==group]
            pp,means,details=fit(x,y,tr,te,k,pc,lam,z=z,labels=labels,method=method)
            truth.extend(y[i] for i in te);pred.extend(pp);base.extend([means]*len(te));effective.append(len(details[2]))
        losses[k,pc,lam]=objective(truth,pred,base,metric);ranges[k,pc,lam]=min(effective),max(effective)
    winner=min(losses,key=lambda a:(losses[a],a[0] or len(x[0]),a[1],-a[2]))
    return losses,ranges,winner


def patterns(xx,scores):
    n=len(xx);p=len(xx[0]);q=len(scores[0])
    xm=[sum(row[j] for row in xx)/n for j in range(p)];sm=[sum(row[o] for row in scores)/n for o in range(q)]
    x=[[v-xm[j] for j,v in enumerate(row)] for row in xx];s=[[v-sm[o] for o,v in enumerate(row)] for row in scores]
    cov=[[sum(row[j]*row[k] for row in s)/(n-1) for k in range(q)] for j in range(q)]
    ev,vec=eigen(cov);cutoff=max(0,ev[0])*64*sys.float_info.epsilon*max(n,q);rank=sum(v>cutoff for v in ev)
    inv=[[sum(v[j]*v[k]/e for v,e in zip(vec,ev) if e>cutoff) for k in range(q)] for j in range(q)]
    cross=[[sum(x[i][j]*s[i][o] for i in range(n))/(n-1) for o in range(q)] for j in range(p)]
    a=[[sum(cross[j][k]*inv[k][o] for k in range(q)) for o in range(q)] for j in range(p)]
    return a,xm,sm,cov,inv,rank


def response_search(x,y,train,groups,counts,components,penalties,metric,z=None,scale='feature_zscore'):
    losses={};best={};aggregate={}
    for k,pc in itertools.product(counts,components):
        for lam in penalties:
            truth=[];pred=[];base=[]
            for group in sorted(set(groups[i] for i in train)):
                tr=[i for i in train if groups[i]!=group];te=[i for i in train if groups[i]==group]
                pp,means,_=fit(x,y,tr,te,k,pc,lam,z=z,scale=scale)
                truth.extend(y[i] for i in te);pred.extend(pp);base.extend([means]*len(te))
            for o in range(len(y[0])):
                losses[k,pc,lam,o]=objective([[r[o]] for r in truth],[[r[o]] for r in pred],[[r[o]] for r in base],metric)
        for o in range(len(y[0])):
            best[k,pc,o]=min(penalties,key=lambda l:(losses[k,pc,l,o] if math.isfinite(losses[k,pc,l,o]) else math.inf,-l))
        valid=[losses[k,pc,best[k,pc,o],o] for o in range(len(y[0])) if math.isfinite(losses[k,pc,best[k,pc,o],o])]
        aggregate[k,pc]=sum(valid)/len(valid) if valid else math.nan
    chosen=min(aggregate,key=lambda a:(aggregate[a] if math.isfinite(aggregate[a]) else math.inf,a[0] or len(x[0]),a[1] or 0))
    return losses,best,aggregate,chosen


def near(a,b):
    if not math.isfinite(b):assert not math.isfinite(float(a)),(a,b)
    else:close(a,b)


with tempfile.TemporaryDirectory(prefix='mvpa_response_ridge_') as folder:
    root=Path(folder);rng=random.Random(91);n=24;train=list(range(18));test=list(range(18,24));groups=[i//6 for i in range(n)]
    z=[float(i%3-1) for i in range(n)]
    x=[[rng.gauss(0,1),rng.gauss(0,1),rng.gauss(0,1),5.] for i in range(n)]
    for row in x:row[1]+=.5*row[0]
    y=[[3*r[0]-.2*r[1],rng.gauss(0,2),7.] for r in x]
    # Match the float input representation used by AFNI, not unlimited decimal data.
    f32=lambda a:struct.unpack('f',struct.pack('f',a))[0]
    x=[[f32(a) for a in row] for row in x];y=[[f32(a) for a in row] for row in y]
    nifti(root/'betas.nii',x);nifti(root/'mask.nii',[[1]*4])
    rows=[dict(Sample=f's{i}',Run='all',Fold=str(groups[i]),Split='train' if i<18 else 'test',Condition='AB'[i%2],
               Z=str(z[i]),Y=str(y[i][0]),Y2=str(y[i][1]),Y3=str(y[i][2]),InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name,records):
        path=root/name
        with path.open('w') as f:
            w=csv.DictWriter(f,list(records[0]),delimiter='\t');w.writeheader();w.writerows(records)
        return path
    source=table('samples.1D',rows)
    def run(args,failure=None,workers=1,exe=binary):
        r=subprocess.run([exe,*map(str,args)],cwd=root,capture_output=True,text=True,
                         env=dict(os.environ,OMP_NUM_THREADS=str(workers)),timeout=120)
        out=r.stdout+r.stderr
        assert (r.returncode==0) if failure is None else (r.returncode!=0 and failure in out),(args,out)
        return out
    def command(name,extra=(),mode='decode',src=source,partition=True,per=True):
        return ['-mode',mode,'-method','ridge','-dataTableFile',src,
                *(['-partition_column','Split','-inner_cv_column','Fold'] if partition else ['-cv_column','Fold']),
                '-mask',root/'mask.nii','-prefix',name,'-sample_column','Sample',
                '-save_predictions',name+'.pred.1D','-save_fold_scores',name+'.fold.1D','-save_tuning',name+'.candidates.1D',
                *(['-target_columns','Y,Y2,Y3'] if mode=='decode' else ['-design_columns','Y,Y2','-score','pattern']),
                '-lambda_grid','100,.01,1',*(['-lambda_per_response'] if per else []),*extra]
    def audit(name,suffix):return read(root/(name+'.'+suffix+'.1D'))
    def check(name,xx=x,yy=y,counts=[None],pcs=[None],metric='normalized_mse',zz=None,scale='feature_zscore',encoding=False,trs=None):
        names=['voxel_'+str(j) for j in range(len(yy[0]))] if encoding else ['Y','Y2','Y3']
        candidates=audit(name,'tuning');folds=sorted(set(r['CVFold'] for r in candidates));pred=audit(name,'pred');foldaudit=audit(name,'fold')
        assert candidates==audit(name,'candidates')
        assert len(candidates)==len(folds)*len(counts)*len(pcs)*3*len(yy[0])
        for fold in folds:
            tr=train if trs is None else trs[fold];te=[int(r['Sample'][1:]) for r in pred if r['CVFold']==fold and r['Response']==names[0]]
            losses,best,aggregate,chosen=response_search(xx,yy,tr,groups,counts,pcs,[100,.01,1],metric,zz,scale)
            for row in [r for r in candidates if r['CVFold']==fold]:
                k=int(row['RequestedCount']) if counts!=[None] else None;pc=int(row['PCARequested']) or None
                lam=float(row['Penalty']);o=names.index(row['Response']);cfg=(k,pc);winner=cfg==chosen
                near(row['InnerLoss'],losses[k,pc,lam,o]);near(row['ConfigurationLoss'],aggregate[cfg])
                assert int(row['InnerNValid'])==math.isfinite(losses[k,pc,lam,o])
                assert int(row['ConfigurationSelected'])==winner
                assert int(row['ResponseSelected'])==(lam==best[k,pc,o])
                assert int(row['Selected'])==(winner and lam==best[k,pc,o])
            predictions={lam:fit(xx,yy,tr,te,*chosen,lam,z=zz,scale=scale)[0] for lam in set(best[chosen+(o,)] for o in range(len(yy[0])))}
            for o,response in enumerate(names):
                selected=best[chosen+(o,)]
                frow,=[r for r in foldaudit if r['CVFold']==fold and r['Response']==response]
                close(frow['Lambda'],selected);near(frow['InnerLoss'],losses[chosen+(selected,o)])
                for row in [r for r in pred if r['CVFold']==fold and r['Response']==response]:
                    pos=te.index(int(row['Sample'][1:]));close(row['Predicted'],predictions[selected][pos][o])
        return candidates
    run(command('basic'));base=check('basic')
    assert len({r['Penalty'] for r in base if r['Selected']=='1'})>=2
    for metric in ['MSE','RMSE','MAE']:
        run(command(metric,extra=['-tune_metric',metric]));check(metric,metric=metric)
    for scale in ['none','sample_center','run_center']:
        run(command(scale,extra=['-scale',scale]));check(scale,scale=scale)
    joint=['-select_features_grid','3,1','-pca_components_grid','2,1','-nuisance_columns','Z']
    run(command('joint',extra=joint));check('joint',counts=[3,1],pcs=[2,1],zz=z)
    assert audit('joint','tuning')==audit('joint','feature_tuning')==audit('joint','pca_tuning')
    # Joint selection is not independent univariate feature selection: keep shared
    # rankings/projections when comparing each response's independent ridge fit.
    for suffix in ['tuning','fold','pred','features','pca']:
        assert (root/('joint.'+suffix+'.1D')).exists()
    run(command('outer',partition=False,extra=['-pca_components','2']))
    trmap={r['CVFold']:[i for i in range(n) if str(groups[i])!=r['CVFold']] for r in audit('outer','fold')}
    check('outer',pcs=[2],trs=trmap)
    # Held-out X, Y and nuisance perturbations cannot affect fitted selections.
    changed=[r.copy() for r in rows];changed_x=[r[:] for r in x]
    for i in test:
        changed[i].update(Y='999',Y2='-500',Y3='0',Z='100')
        changed_x[i]=[v+50 for v in changed_x[i]]
    nifti(root/'changed.nii',changed_x)
    for i in range(n):changed[i]['InputFile']=str(root/'changed.nii')+f'[{i}]'
    run(command('changed',src=table('changed.1D',changed),extra=joint))
    for suffix in ['tuning','candidates','features','pca']:
        assert audit('changed',suffix)==audit('joint',suffix),suffix
    # Separate tables retain the same conditional response selection.
    trainfile=table('train.1D',rows[:18]);testfile=table('test.1D',rows[18:])
    args=command('two',src=trainfile);at=args.index('-partition_column');args[at:at+2]=['-testTableFile',testfile]
    run(args);assert audit('two','tuning')==base
    # Constant responses and rank-zero predictors: finite error objectives retain
    # constants; normalized MSE excludes them; all-undefined ties are deterministic.
    constant=[dict(r,Y='7',Y2='7',Y3='7') for r in rows]
    run(command('constant',src=table('constant.1D',constant),extra=['-select_features_grid','3,1','-pca_components_grid','2,1']))
    selected=[r for r in audit('constant','tuning') if r['Selected']=='1']
    assert len(selected)==3 and all(r['Penalty']=='100' and r['RequestedCount']=='1' and r['PCARequested']=='1' and r['InnerNValid']=='0' for r in selected)
    nifti(root/'zeros.nii',[[0]*4 for _ in range(n)])
    zero=[dict(r,InputFile=str(root/'zeros.nii')+f'[{i}]') for i,r in enumerate(rows)]
    run(command('rankzero',src=table('zeros.1D',zero),extra=['-pca_components','2']))
    assert all(r['Penalty']=='100' for r in audit('rankzero','tuning') if r['Selected']=='1')
    # Patterns report the corresponding response lambda and use the actual mixed
    # ridge fitted scores in their covariance transform.
    run(command('patterns',extra=['-save_patterns','pat']))
    pat=read(root/'pat.patterns.1D');pen={r['Response']:float(r['Penalty']) for r in base if r['Selected']=='1'}
    scores={l:fit(x,y,train,train,None,None,l)[0] for l in set(pen.values())}
    actual_scores=[[scores[pen[names]][i][o] for o,names in enumerate(['Y','Y2','Y3'])] for i in range(len(train))]
    expected=patterns([preprocess(x,train)[i] for i in train],actual_scores)[0]
    for r in pat:
        o=['Y','Y2','Y3'].index(r['Response']);close(r['Lambda'],pen[r['Response']]);close(r['Pattern'],expected[int(r['FeatureIndex'])][o])
    # Encoding fits multiple voxel responses together; penalties vary within a unit.
    run(command('encoding',mode='encode'));check('encoding',xx=[r[:2] for r in y],yy=x,encoding=True)
    # Single-response voxel encoding still works and names each real voxel.
    args=command('voxel',mode='encode');args[args.index('-score')+1]='voxel';run(args)
    assert {r['Response'] for r in audit('voxel','tuning')}=={'voxel_0','voxel_1','voxel_2','voxel_3'}
    for mode in ['decode','encode']:
        for workers in [1,2]:run(command(mode+str(workers),mode=mode,partition=False,extra=['-searchlight','2','-pca_components','2']),workers=workers)
        for suffix in ['pred','fold','tuning','pca']:assert audit(mode+'1',suffix)==audit(mode+'2',suffix)
        for perm in [False,True]:run(command(mode+str(perm),mode=mode,partition=False,extra=['-select_features','1']+(['-nperm','3','-permute_within','Fold'] if perm else [])))
        for suffix in ['pred','fold','tuning','features']:assert audit(mode+'True',suffix)==audit(mode+'False',suffix)
    # -validate_only exposes the objective/cost without fitting or writing output.
    out=run(command('validate',extra=['-validate_only']))
    assert '# RidgePenaltyScope\tper_response' in out and not list(root.glob('validate*'))
    run(command('bad',extra=['-lambda_per_response']),'specified twice')
    run(command('bad',extra=['-lambda','1']),'cannot accompany numeric')
    args=command('bad');args[args.index('-mode')+1]='classify';args[args.index('-target_columns')+1]='Condition'
    run(args,'requires continuous')
    # Default option alone enables the documented grid; legacy output is unchanged.
    args=command('auto');at=args.index('-lambda_grid');del args[at:at+2];run(args)
    assert {float(r['Penalty']) for r in audit('auto','tuning')}=={.01,.1,1,10,100}
    if len(sys.argv)>2:
        run(command('legacy',per=False));saved={s:(root/('legacy.'+s+'.1D')).read_bytes() for s in ['pred','fold','tuning','candidates']}
        run(command('reference',per=False),exe=str(Path(sys.argv[2]).resolve()))
        for s,data in saved.items():assert data==(root/('reference.'+s+'.1D')).read_bytes(),s
print('PASS per-response ridge: independent candidate losses, shared configuration choice, predictions, analytic penalties, held-out isolation, pattern composition, decoding/encoding, partitions, spatial workers, permutations and default compatibility')
