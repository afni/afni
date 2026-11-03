#!/usr/bin/env python3
"""PCA projection, nested-grid, held-out isolation, and exact-null oracles (stdlib only)."""
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


with tempfile.TemporaryDirectory(prefix='mvpa_pca_') as folder:
    root=Path(folder);n=24;train=list(range(18));test=list(range(18,24));rng=random.Random(114)
    labels=[i%2 for i in range(n)];groups=[0 if i<4 else 1 if i<10 else 2 if i<18 else 3 for i in range(n)]
    z=[-1 if i%4<2 else 1 for i in range(n)]
    x=[[rng.gauss(0,1)+z[i],.8*labels[i]+rng.gauss(0,1),rng.gauss(0,1),5.] for i in range(n)]
    y=[[r[0]+.2*r[1]+rng.gauss(0,.4),2*r[1]+rng.gauss(0,1)] for r in x]
    design=[[r[2],r[0],r[1]] for r in x];onehot=[[float(c==label) for c in range(2)] for label in labels]
    nifti(root/'betas.nii',x);nifti(root/'mask.nii',[[1]*4])
    rows=[dict(Sample=f's{i}',Run='all',Fold=str(groups[i]),Split='train' if i<18 else 'test',Condition='AB'[labels[i]],
               Z=str(z[i]),Y=str(y[i][0]),Y2=str(y[i][1]),D0=str(design[i][0]),D1=str(design[i][1]),D2=str(design[i][2]),InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name,records):
        path=root/name
        with path.open('w') as f:
            w=csv.DictWriter(f,list(records[0]),delimiter='\t');w.writeheader();w.writerows(records)
        return path
    source=table('samples.1D',rows)
    def run(args,failure=None,workers=2):
        result=subprocess.run([binary,*map(str,args)],cwd=root,capture_output=True,text=True,env=dict(os.environ,OMP_NUM_THREADS=str(workers)),timeout=120)
        output=result.stdout+result.stderr
        assert (result.returncode==0) if failure is None else (result.returncode!=0 and failure in output),output
        return output
    def command(name,mode='decode',method='ridge',extra=(),src=source,tuning=True):
        return ['-mode',mode,'-method',method,'-dataTableFile',src,'-partition_column','Split',
                '-mask',root/'mask.nii','-prefix',name,'-save_predictions',name+'.pred.1D','-save_fold_scores',name+'.fold.1D',
                '-design_columns' if mode=='encode' else '-target_columns','D0,D1,D2' if mode=='encode' else 'Condition' if mode=='classify' else 'Y,Y2',
                *(['-score','pattern'] if mode=='encode' else []),*(['-svm_max_iter','100000'] if method=='svm' else []),
                *(['-inner_cv_column','Fold','-select_features_grid','1,3,4','-pca_components_grid','1,2,9',
                   *(['-lambda_grid','.1,10'] if method=='ridge' else ['-svm_c_grid','.01,1'] if method=='svm' else [])] if tuning else []),*extra]
    def audit(name,suffix='pca_tuning'):return read(root/(name+'.'+suffix+'.1D'))
    def check(name,xx,yy,counts=[1,3,4],components=[1,2,9],penalties=[.1,10],metric='MAE',zz=None,ll=None,method='ridge'):
        losses,ranges,winner=search(xx,yy,train,groups,counts,components,penalties,metric,z=zz,labels=ll,method=method)
        aa=audit(name);assert len(aa)==len(losses),(name,len(aa),len(losses))
        for r in aa:
            key=(int(r['RequestedCount']) if counts!=[None] else None,int(r['PCARequested']),float(r['Penalty']) if r['Penalty']!='NA' else 1)
            close(r['InnerLoss'],losses[key]);assert int(r['Selected'])==int(key==winner),(name,key,winner,r)
            assert (int(r['InnerPCAMin']),int(r['InnerPCAMax']))==ranges[key]
        pp,_,details=fit(xx,yy,train,test,*winner,z=zz,labels=ll,method=method)
        for r in audit(name,'pred'):
            i=int(r['Sample'][1:])
            if i not in test:continue
            if ll is not None:
                predicted='AB'[max(range(2),key=lambda c:pp[test.index(i)][c])]
                assert r['Predicted']==predicted,(name,r,predicted)
            elif 'Response' in r:
                o=['Y','Y2'].index(r['Response']) if xx is x else int(r['Response'].split('_')[-1])
                close(r['Predicted'],pp[test.index(i)][o])
        return winner,details
    for mode,xx,yy,ll,metric in [('decode',x,y,None,'MAE'),('encode',design,x,None,'RMSE'),('classify',x,onehot,labels,'normalized_mse')]:
        name=mode
        run(command(name,mode,extra=['-tune_metric',metric,'-nuisance_columns','Z','-save_pca_loadings',name+'.load.1D','-save_tuning',name+'.common.1D']))
        winner,details=check(name,xx,yy,metric=metric,zz=z,ll=ll)
        assert audit(name)==audit(name,'common')
        ids,means,vectors,values,rank=details
        for r in audit(name,'pca'):
            k=int(r['Component'])-1;assert int(r['NumericalRank'])==rank
            close(r['Variance'],values[k]);assert int(r['EffectiveComponents'])==len(values)
        for r in audit(name,'load'):
            k=int(r['Component'])-1;j=ids.index(int(r['FeatureIndex']))
            close(r['Mean'],means[j]);close(r['Loading'],vectors[k][j])
    run(command('centroid','classify','centroid',['-tune_metric','BACC']))
    check('centroid',x,onehot,penalties=[1],metric='balanced_accuracy',ll=labels,method='centroid')
    # Other classifier paths and component-only tuning (no supervised selection).
    for method in ['ridge','centroid','svm','lda_diag','lda_shrinkage']:
        args=command('only_'+method,'classify',method,['-pca_components_grid','1,2,9','-inner_cv_column','Fold','-tune_metric','ACC'],tuning=False)
        run(args);aa=audit('only_'+method);assert len(aa)==3 and sum(int(r['Selected']) for r in aa)==1
        if method in ['ridge','centroid']:check('only_'+method,x,onehot,counts=[None],penalties=[1],metric='accuracy',ll=labels,method=method)
    # Fixed PCA composed with penalty-only tuning retains the complete candidate audit.
    run(command('fixed_penalties',extra=['-pca_components','2','-lambda_grid','.1,10',
                '-inner_cv_column','Fold','-tune_metric','MAE'],tuning=False))
    check('fixed_penalties',x,y,counts=[None],components=[2])
    # Fixed PCA is independently fitted even without a nested search; all scaling modes.
    for scale in ['none','feature_zscore','sample_center','run_center']:
        name='fixed_'+scale;run(command(name,extra=['-scale',scale,'-pca_components','2','-save_pca_loadings',name+'.load.1D'],tuning=False))
        pp,_,_=fit(x,y,train,test,None,2,scale=scale)
        for r in audit(name,'pred'):
            i=int(r['Sample'][1:])
            if i in test:close(r['Predicted'],pp[test.index(i)][['Y','Y2'].index(r['Response'])])
        assert not (root/(name+'.pca_tuning.1D')).exists()
    # Perturb only held-out inputs/outcomes/covariates: training projections/search unchanged.
    changed=[dict(r) for r in rows];xx=[r[:] for r in x]
    for i in test:
        xx[i]=[a+100*(j+1) for j,a in enumerate(xx[i])];changed[i]['Y']=str(500+i);changed[i]['Z']='7'
    nifti(root/'changed.nii',xx)
    for i,r in enumerate(changed):r['InputFile']=str(root/'changed.nii')+f'[{i}]'
    run(command('changed',src=table('changed.1D',changed),extra=['-tune_metric','MAE','-nuisance_columns','Z','-save_pca_loadings','changed.load.1D']))
    for suffix in ['pca','pca_tuning','load']:assert audit('changed',suffix)==audit('decode',suffix),suffix
    # Two-table fixed partition parity.
    args=command('dual',extra=['-tune_metric','MAE','-nuisance_columns','Z','-save_pca_loadings','dual.load.1D'])
    at=args.index('-dataTableFile');args[at:at+2]=['-dataTable','@'+str(table('train.1D',rows[:18])),'-testTable','@'+str(table('test.1D',rows[18:]))]
    at=args.index('-partition_column');del args[at:at+2];run(args)
    for suffix in ['pca','pca_tuning','load']:assert audit('dual',suffix)==audit('decode',suffix)
    # Spatial workers: active widths change at edges; audit original voxel IDs and count ranges.
    for mode in ['decode','classify','encode']:
        for workers in [1,2]:run(command(f'spatial_{mode}_{workers}',mode,extra=['-searchlight','1.01','-save_pca_loadings',f'spatial_{mode}_{workers}.load.1D']),workers=workers)
        for suffix in ['pca','pca_tuning','load','pred','fold']:
            assert audit(f'spatial_{mode}_1',suffix)==audit(f'spatial_{mode}_2',suffix),(mode,suffix)
    # Constant and collinear input: clip rank, no fabricated directions, finite predictions.
    nifti(root/'constant.nii',[[7.]*4 for _ in range(n)])
    crows=[dict(r,InputFile=str(root/'constant.nii')+f'[{i}]',Y='3',Y2='3') for i,r in enumerate(rows)]
    run(command('constant',src=table('constant.1D',crows),extra=['-save_pca_loadings','constant.load.1D']))
    assert all(int(r['NumericalRank'])==0 and int(r['EffectiveComponents'])==0 for r in audit('constant','pca'))
    for r in audit('constant','pred'):
        if r['Predicted']!='NA':close(r['Predicted'],3)
    selected=[r for r in audit('constant') if r['Selected']=='1'];assert len(selected)==1
    assert (selected[0]['RequestedCount'],selected[0]['PCARequested'],selected[0]['Penalty'])==('1','1','10')
    # Validation-only and failure contracts.
    report=run(command('validate',extra=['-validate_only','-save_pca_loadings','validate.load.1D']))
    assert '# PCAFit\tno_validation_only' in report and not list(root.glob('validate.*'))
    for extra,message in [(['-pca_components','0'],'PCA count'),(['-pca_components_grid','1,1'],'duplicate PCA count'),
                           (['-pca_components','1,2'],'needs one count'),(['-pca_components_grid','2'],'at least two'),
                           (['-save_pca_loadings','bad.load.1D'],'requires PCA'),
                           (['-pca_components','2','-save_coefficients','coef'],'cannot yet export PCA'),
                           (['-pca_components','2','-save_pca_loadings','bad.pca.1D'],'filenames must differ'),
                           (['-pca_components','2','-save_pca_loadings','bad.pred.1D'],'filenames must differ')]:
        run(command('bad',extra=extra+['-validate_only'],tuning=False),message)
    # Full-rank unwhitened PCR matches ridge; test both smaller-Gram paths.
    for dim in [4,27]:
        hx=[[math.sin(i*(j+1)*.31)+.1*math.cos(i+j) for j in range(dim)] for i in range(n)]
        nifti(root/f'wide{dim}.nii',hx);nifti(root/f'mask{dim}.nii',[[1]*dim])
        hr=[dict(r,InputFile=str(root/f'wide{dim}.nii')+f'[{i}]') for i,r in enumerate(rows)]
        hs=table(f'wide{dim}.1D',hr)
        for use_pca in [False,True]:
            name=f'full{dim}_{use_pca}'
            args=command(name,src=hs,tuning=False,extra=['-scale','none',*(['-pca_components','99','-save_pca_loadings',name+'.load.1D'] if use_pca else [])])
            args[args.index('-mask')+1]=root/f'mask{dim}.nii';run(args)
        for a,b in zip(audit(f'full{dim}_False','pred'),audit(f'full{dim}_True','pred')):
            if a['Predicted']!='NA':close(a['Predicted'],b['Predicted'])
        # Full projection reconstructs centered training rows in original feature coordinates.
        load=audit(f'full{dim}_True','load');count=max(int(r['Component']) for r in load)
        means=[float(r['Mean']) for r in load[:dim]]
        vectors=[[float(r['Loading']) for r in load[k*dim:(k+1)*dim]] for k in range(count)]
        for i in train:
            centered=[v-m for v,m in zip(hx[i],means)]
            scores=[sum(a*b for a,b in zip(centered,v)) for v in vectors]
            for j in range(dim):close(sum(scores[k]*vectors[k][j] for k in range(count)),centered[j])
    # Enumerate the 64 allowed blocked permutations independently, including PCA refits.
    pn=12;ptrain=list(range(8));ptest=list(range(8,12));pgroups=[i//2 for i in range(pn)]
    plabels=[i%2 for i in range(pn)];pz=[-1 if i//2%2==0 else 1 for i in range(pn)]
    px=[[math.sin(i*1.3),1.2*(i%2)+.3*math.cos(i),math.cos(i*.7),3.] for i in range(pn)]
    py=[[r[0]+.4*r[1],2*r[2]+.2*r[1]] for r in px];pd=[[r[1],r[0],r[2]] for r in px]
    nifti(root/'null_betas.nii',px)
    prows=[dict(rows[i],Sample=f's{i}',Fold=str(pgroups[i]),Split='train' if i<8 else 'test',Block=str(i//2),
                Z=str(pz[i]),Condition='AB'[plabels[i]],Y=str(py[i][0]),Y2=str(py[i][1]),
                D0=str(pd[i][0]),D1=str(pd[i][1]),D2=str(pd[i][2]),InputFile=str(root/'null_betas.nii')+f'[{i}]') for i in range(pn)]
    psource=table('null.1D',prows)
    for mode,inner,outer,method in [('classify','normalized_mse','balanced_accuracy','ridge'),('classify','accuracy','accuracy','centroid'),('decode','MAE','RMSE','ridge'),('encode','RMSE','MAE','ridge')]:
        name='null_'+mode+'_'+method
        def null_command(prefix,permute):
            args=command(prefix,mode,method,src=psource,extra=['-tune_metric',inner,'-metric',outer,
                         '-nuisance_columns','Z','-save_pca_loadings',prefix+'.load.1D',
                         *(['-nperm','999','-permute_within','Block'] if permute else [])])
            args[args.index('-select_features_grid')+1]='2,3';args[args.index('-pca_components_grid')+1]='1,2'
            return args
        run(null_command(name,True));run(null_command(name+'_observed',False))
        for suffix in ['pca','pca_tuning','load','features','feature_tuning','pred','fold']:
            assert audit(name,suffix)==audit(name+'_observed',suffix),(name,suffix)
        scores=[]
        for swaps in itertools.product([0,1],repeat=6):
            perm=[i^swaps[i//2] for i in range(pn)]
            ll=[plabels[i] for i in perm] if mode=='classify' else None
            xx=[pd[i] for i in perm] if mode=='encode' else px
            yy=[[float(c==label) for c in range(2)] for label in ll] if ll is not None else px if mode=='encode' else [py[i] for i in perm]
            _,_,winner=search(xx,yy,ptrain,pgroups,[2,3],[1,2],[.1,10] if method=='ridge' else [1],inner,z=pz,labels=ll,method=method)
            pp,_,_=fit(xx,yy,ptrain,ptest,*winner,z=pz,labels=ll,method=method)
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
            close(r['PVAL'],sum(v>=observed-1e-7 if mode=='classify' else v<=observed+1e-7 for v in values)/len(values))
            if mode!='classify':close(r['NULL_MEAN'],sum(values)/len(values));close(r['SCORE'],observed)
print('PASS PCA: independent projections/reconstruction and joint grids, all classifiers, decode/encode, held-out isolation, rank clipping, exact blocked nulls, partition parity and spatial workers')
