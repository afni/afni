#!/usr/bin/env python3
"""Independent fold-local activation-pattern and covariance normalization checks."""
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


with tempfile.TemporaryDirectory(prefix='mvpa_patterns_') as folder:
    root=Path(folder);n=24;train=list(range(18));test=list(range(18,24));rng=random.Random(23)
    labels=[i%3 for i in range(n)];groups=[i//6 for i in range(n)];z=[(-1 if i//3%2 else 1) for i in range(n)]
    x=[[rng.gauss(0,1),rng.gauss(0,1),rng.gauss(0,1),5.] for i in range(n)]
    for row in x:row[1]+=.7*row[0]
    y=[[row[0]-.8*row[1]+.1*rng.gauss(0,1),row[2]+.3*row[1]] for row in x]
    y=[r+[2*r[0]] for r in y]
    onehot=[[float(c==label) for c in range(3)] for label in labels]
    nifti(root/'betas.nii',x);nifti(root/'mask.nii',[[1]*4])
    rows=[dict(Sample=f's{i}',Run='all',Fold=str(groups[i]),Split='train' if i<18 else 'test',Condition='ABC'[labels[i]],
               Z=str(z[i]),Y=str(y[i][0]),Y2=str(y[i][1]),Y3=str(y[i][2]),InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name,records):
        path=root/name
        with path.open('w') as f:
            w=csv.DictWriter(f,list(records[0]),delimiter='\t');w.writeheader();w.writerows(records)
        return path
    source=table('samples.1D',rows)
    def run(args,failure=None,workers=1,exe=binary):
        r=subprocess.run([exe,*map(str,args)],cwd=root,capture_output=True,text=True,env=dict(os.environ,OMP_NUM_THREADS=str(workers)),timeout=120)
        out=r.stdout+r.stderr
        assert (r.returncode==0) if failure is None else (r.returncode!=0 and failure in out),(args,out)
        return out
    def command(name,mode='decode',extra=(),src=source,export=True,partition=True):
        return ['-mode',mode,'-method','ridge','-dataTableFile',src,
                *(['-partition_column','Split'] if partition else ['-cv_column','Fold']),
                '-mask',root/'mask.nii','-prefix',name,'-save_predictions',name+'.pred.1D',
                '-save_fold_scores',name+'.fold.1D','-target_columns','Condition' if mode=='classify' else 'Y,Y2,Y3',
                *(['-save_patterns',name+'.pat'] if export else []),*extra]
    def audit(name,suffix='patterns'):return read(root/(name+'.pat.'+suffix+'.1D'))
    def normalized(records):return [{k:v for k,v in r.items() if k!='MapPrefix'} for r in records]
    def check(name,mode='decode',scale='feature_zscore',counts=[None],components=[99],penalties=[1],zz=None,trs=None):
        yy=onehot if mode=='classify' else y;ll=labels if mode=='classify' else None
        names=['A','B','C'] if mode=='classify' else ['Y','Y2','Y3']
        result=audit(name);ca=audit(name,'pattern_covariance')
        folds=sorted(set(r['CVFold'] for r in result));assert len(result)==len(folds)*4*3
        for fold in folds:
            tr=train if trs is None else trs[fold]
            if len(counts)*len(components)*len(penalties)>1:
                _,_,winner=search(x,yy,tr,groups,counts,components,penalties,'normalized_mse',z=zz,labels=ll)
            else:winner=counts[0],components[0],penalties[0]
            scores,_,_=fit(x,yy,tr,tr,*winner,z=zz,scale=scale,labels=ll)
            transformed=preprocess(x,tr,zz,scale)
            expected,xm,sm,cov,inv,rank=patterns([transformed[i] for i in tr],scores)
            for r in result:
                if r['CVFold']!=fold:continue
                j=int(r['FeatureIndex']);o=names.index(r['Response'])
                close(r['Pattern'],expected[j][o]);close(r['InputMean'],xm[j]);close(r['ScoreMean'],sm[o])
                close(r['Lambda'],winner[2]);assert int(r['ScoreRank'])==rank
                assert r['Feature']==f'voxel_{j}' and int(r['MapBrick'])==o
            for r in ca:
                if r['CVFold']!=fold:continue
                j=names.index(r['ScoreRow']);o=names.index(r['ScoreColumn'])
                close(r['Covariance'],cov[j][o]);close(r['Pseudoinverse'],inv[j][o]);assert int(r['ScoreRank'])==rank
    # Direct, scaled, centered, and nuisance-adjusted coordinates; rank-deficient targets.
    for scale in ['none','feature_zscore','sample_center','run_center']:
        name='scale_'+scale;run(command(name,extra=['-scale',scale]));check(name,scale=scale)
    for mode in ['decode','classify']:
        name='joint_'+mode
        extra=['-nuisance_columns','Z','-select_features_grid','2,3','-pca_components_grid','1,2',
               '-lambda_grid','.1,10','-inner_cv_column','Fold','-save_pca_loadings',name+'.load.1D']
        run(command(name,mode,extra));check(name,mode,counts=[2,3],components=[1,2],penalties=[.1,10],zz=z)
        # Enabling patterns never changes predictions, tuning, PCA or selected features.
        plain_extra=extra[:-2]  # separate loading file is irrelevant to compatibility here
        run(command(name+'_plain',mode,plain_extra,export=False))
        for suffix in ['pred.1D','fold.1D','tuning.1D','features.1D','feature_tuning.1D','pca.1D','pca_tuning.1D']:
            assert (root/(name+'.'+suffix)).read_bytes()==(root/(name+'_plain.'+suffix)).read_bytes(),(mode,suffix)
    # Fixed single-feature/PCA projection: omitted correlated input still has a pattern.
    run(command('one',extra=['-select_features','1','-pca_components','1']))
    check('one',counts=[1],components=[1])
    assert any(r['SelectedFeature']=='0' and abs(float(r['Pattern']))>1e-4 for r in audit('one'))
    # Perturb all held-out predictor/outcome/covariate values; training patterns stay identical.
    changed=[dict(r) for r in rows];xx=[r[:] for r in x]
    for i in test:
        xx[i]=[v+100*(j+1) for j,v in enumerate(xx[i])];changed[i]['Y']='999';changed[i]['Z']='20'
    nifti(root/'changed.nii',xx)
    for i,r in enumerate(changed):r['InputFile']=str(root/'changed.nii')+f'[{i}]'
    extra=['-nuisance_columns','Z','-select_features_grid','2,3','-pca_components_grid','1,2','-lambda_grid','.1,10','-inner_cv_column','Fold']
    run(command('changed',extra=extra,src=table('changed.1D',changed)))
    assert normalized(audit('changed'))==normalized(audit('joint_decode'))
    assert audit('changed','pattern_covariance')==audit('joint_decode','pattern_covariance')
    # Outer CV keeps patterns separate and fits each covariance only on that training set.
    run(command('cv',partition=False,extra=['-pca_components','2','-lambda','.1']))
    folds={r['CVFold'] for r in audit('cv')};assert len(folds)==4,folds
    trs={fold:[i for i in range(n) if str(groups[i])!=fold] for fold in folds}
    check('cv',components=[2],penalties=[.1],trs=trs)
    # More neural inputs than training rows: covariance need not be invertible.
    small=[dict(r,Split='train' if i<3 else 'test') for i,r in enumerate(rows[:6])]
    run(command('small',src=table('small.1D',small),extra=['-pca_components','9']))
    check('small',components=[9],trs={'test':[0,1,2]})
    # Two-table holdout keeps identical learned patterns and covariance.
    args=command('dual',extra=extra);at=args.index('-dataTableFile')
    args[at:at+2]=['-dataTable','@'+str(table('train.1D',rows[:18])),'-testTable','@'+str(table('test.1D',rows[18:]))]
    at=args.index('-partition_column');del args[at:at+2];run(args)
    assert normalized(audit('dual'))==normalized(audit('joint_decode'))
    # ROI/volume maps occupy each original feature, not just searchlight center; worker invariance.
    for workers in [1,2]:
        run(command(f'spatial{workers}',extra=['-searchlight','1.01','-pca_components','1']),workers=workers)
    assert normalized(audit('spatial1'))==normalized(audit('spatial2'))
    assert audit('spatial1','pattern_covariance')==audit('spatial2','pattern_covariance')
    if len(sys.argv)>2:
        dump=str(Path(sys.argv[2]).resolve())
        for name in ['scale_none','joint_decode','spatial1']:
            aa=audit(name);maps=sorted(set((r['MapPrefix'],r['MapBrick']) for r in aa))
            for mp,brick in maps:
                headers=list(root.glob(mp+'+*.HEAD')) or list(root.glob(mp+'.1D'));assert len(headers)==1,(mp,headers)
                text=run(['-noijk',str(headers[0])+'['+brick+']'],exe=dump)
                values=[]
                for line in text.splitlines():
                    try:values.append(float(line.strip()))
                    except ValueError:pass
                assert len(values)==4
                expected={int(r['FeatureIndex']) if name!='spatial1' else int(r['Feature'].split('_')[1]):float(r['Pattern']) for r in aa if r['MapPrefix']==mp and r['MapBrick']==brick}
                for j,v in enumerate(values):close(v,expected.get(j,0))
    # Constant training scores report rank zero, without misleading nonzero patterns.
    constant=[dict(r,Y='3',Y2='4',Y3='6') for r in rows]
    run(command('constant',src=table('constant.1D',constant)))
    assert all(r['ScoreRank']=='0' and float(r['Pattern'])==0 for r in audit('constant'))
    # Class indicator covariance is singular: two-class maps are opposite half-contrasts.
    binary_rows=[dict(r,Condition='AB'[i%2]) for i,r in enumerate(rows)]
    run(command('binary','classify',src=table('binary.1D',binary_rows)))
    aa=audit('binary');assert all(r['ScoreRank']=='1' for r in aa)
    for j in range(4):close(aa[2*j]['Pattern'],-float(aa[2*j+1]['Pattern']))
    # Performed permutations do not turn patterns into inferential maps or leave null fits exported.
    for mode in ['decode','classify']:
        name='perm_'+mode
        opts=['-pca_components_grid','1,2','-inner_cv_column','Fold','-lambda_grid','.1,10']
        run(command(name,mode,opts+['-nperm','11','-permute_within','Fold','-seed','42']))
        run(command(name+'_observed',mode,opts))
        assert normalized(audit(name))==normalized(audit(name+'_observed'))
        assert audit(name,'pattern_covariance')==audit(name+'_observed','pattern_covariance')
    # Full-data coefficient export remains independent and may coexist for its supported pipelines.
    run(command('both',partition=False,extra=['-save_coefficients','coeff']))
    assert (root/'coeff.coefficients.1D').exists()
    report=run(command('validate',extra=['-validate_only','-pca_components','2']))
    assert '# PatternFit\tno_validation_only' in report and not list(root.glob('validate*'))
    for extra,message in [(['-save_patterns','again'],'specified twice'),
                           (['-method','svm'],'requires ridge decoding or ridge classification'),
                           (['-method','centroid'],'requires ridge decoding or ridge classification'),
                           (['-save_predictions','bad.pat.patterns.1D'],'filenames must differ'),
                           (['-save_predictions',root/'bad.pat.patterns.1D'],'filenames must differ'),
                           (['-save_pca_loadings','bad.pat.pattern_covariance.1D','-pca_components','1'],'filenames must differ'),
                           (['-save_predictions','bad.pat.unit_1.fold_1'+(str(headers[0])[len(str(root/mp)):] if len(sys.argv)>2 else '+orig.HEAD')],'filenames must differ')]:
        run(command('bad','classify',extra+['-validate_only']),message)
    args=command('bad',extra=['-validate_only']);args[args.index('-mode')+1]='encode';at=args.index('-target_columns');args[at:at+2]=['-design_columns','Y']
    run(args,'requires ridge decoding or ridge classification')
    saved=(root/'scale_none.pat.patterns.1D').read_bytes()
    run(command('scale_none'), 'already exists')
    assert (root/'scale_none.pat.patterns.1D').read_bytes()==saved
print('PASS activation patterns: correlated-feature oracle, joint singular normalization, fold-local pipelines, held-out isolation, map identities, constant scores, permutation restoration, worker determinism and collision validation')
