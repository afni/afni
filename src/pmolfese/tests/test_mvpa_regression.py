#!/usr/bin/env python3
"""Continuous estimator acceptance tests; stdlib independent numerical oracles.

EN enumerates active sets; SVR uses primal Newton; PLS uses Jacobi SVD and
an explicit loading inverse; banded ridge uses a dense normal-equation solve.
Production uses coordinate descent, NIPALS, and Cholesky/Woodbury respectively.
"""
import csv
import itertools
import math
import os
from pathlib import Path
import random
import struct
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())
env = dict(os.environ, AFNI_NIFTI_TYPE_WARN='NO', AFNI_NIFTI_VIEW='orig',
           OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1',
           MKL_NUM_THREADS='1', NUMEXPR_NUM_THREADS='1')

def f32(v):
    return struct.unpack('f', struct.pack('f', v))[0]

def nifti(path, rows):
    p = len(rows[0]); h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    struct.pack_into('<8h', h, 40, 4, p, 1, 1, len(rows), 1, 1, 1)
    struct.pack_into('<2h', h, 70, 16, 32)
    struct.pack_into('<8f', h, 76, *([1]*8))
    struct.pack_into('<f', h, 108, 352); struct.pack_into('<h', h, 254, 1)
    for i in range(3):
        row = [0.]*4; row[i] = 1
        struct.pack_into('<4f', h, 280+16*i, *row)
    h[344:348] = b'n+1\0'
    v = [v for row in rows for v in row]
    Path(path).write_bytes(h+struct.pack('<%df' % len(v), *v))

def read(path):
    with Path(path).open() as f:
        return list(csv.DictReader((s[11:] if s.startswith('# Columns: ') else s
                    for s in f if not s.startswith('#') or s.startswith('# Columns: ')), delimiter='\t'))

def run(args, ok=True, workers=1):
    r = subprocess.run([binary]+list(map(str,args)), env=dict(env, OMP_NUM_THREADS=str(workers)),
                       text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    assert (r.returncode == 0) == ok, (args,r.stdout+r.stderr)
    return r.stdout+r.stderr

def close(a,b,tol=2e-5):
    assert math.isclose(float(a),float(b),abs_tol=tol,rel_tol=tol),(a,b)

def dot(a,b):return sum(v*w for v,w in zip(a,b))
def transpose(a):return list(map(list,zip(*a)))
def mm(a,b):return [[dot(row,col) for col in zip(*b)] for row in a]
def mean(a):return [sum(col)/len(a) for col in zip(*a)]
def centered(a):
    m=mean(a);return [[v-w for v,w in zip(row,m)] for row in a],m

def solve(a,b):
    n=len(a);a=[list(row)+[v] for row,v in zip(a,b)]
    for j in range(n):
        pivot=max(range(j,n),key=lambda i:abs(a[i][j]));a[j],a[pivot]=a[pivot],a[j]
        d=a[j][j]
        if abs(d)<1e-14:raise ArithmeticError('singular oracle system')
        a[j]=[v/d for v in a[j]]
        for i in range(n):
            if i!=j:
                d=a[i][j];a[i]=[v-d*w for v,w in zip(a[i],a[j])]
    return [row[-1] for row in a]

def leading_eigen(a):
    a=[list(row) for row in a];n=len(a);v=[[float(i==j) for j in range(n)] for i in range(n)]
    for _ in range(100*n*n):
        if n==1:break
        i,j=max(((i,j) for i in range(n) for j in range(i+1,n)),key=lambda ij:abs(a[ij[0]][ij[1]]))
        if abs(a[i][j])<1e-13*max(1,max(abs(a[k][k]) for k in range(n))):break
        angle=.5*math.atan2(2*a[i][j],a[j][j]-a[i][i]);c=math.cos(angle);s=math.sin(angle)
        rot=[[float(r==t) for t in range(n)] for r in range(n)];rot[i][i]=rot[j][j]=c;rot[i][j]=s;rot[j][i]=-s
        a=mm(transpose(rot),mm(a,rot));v=mm(v,rot)
    else:raise AssertionError('Jacobi oracle failed')
    k=max(range(n),key=lambda k:a[k][k]);return [row[k] for row in v],a[k][k]

def fit(method,x,y,param):
    n=len(x);p=len(x[0]);q=len(y[0]);xx,xm=centered(x);yy,ym=centered(y)
    coef=[[0.]*q for _ in range(p)];intercept=ym[:]
    if method=='elastic_net':
        alpha,rho=param;gram=mm(transpose(xx),xx);cross=mm(transpose(xx),yy)
        for o in range(q):
            found=False
            for signs in itertools.product((-1,0,1),repeat=p):
                active=[j for j in range(p) if signs[j]];beta=[0.]*p
                try:
                    b=solve([[gram[j][k]/n+(alpha*(1-rho) if j==k else 0) for k in active] for j in active],
                            [cross[j][o]/n-alpha*rho*signs[j] for j in active])
                except ArithmeticError:continue
                for j,v in zip(active,b):beta[j]=v
                gradient=[(dot(gram[j],beta)-cross[j][o])/n+alpha*(1-rho)*beta[j] for j in range(p)]
                if any(beta[j]*signs[j]<=0 for j in active):continue
                if any(abs(gradient[j])>alpha*rho+1e-8 for j in range(p) if not signs[j]):continue
                for j in range(p):coef[j][o]=beta[j]
                found=True;break
            assert found,'no EN active set met KKT'
    elif method=='banded_ridge':
        gram=mm(transpose(xx),xx);cross=mm(transpose(xx),yy)
        for j in range(p):gram[j][j]+=param[j%2]
        coef=transpose([solve(gram,[row[o] for row in cross]) for o in range(q)])
    elif method=='pls':
        weights=[];loadings=[];responses=[]
        for _ in range(min(param,p,n-1)):
            cross=mm(transpose(xx),yy);w,eig=leading_eigen(mm(cross,transpose(cross)))
            if eig<1e-20:break
            t=[dot(row,w) for row in xx];tt=dot(t,t)
            if tt<1e-14:break
            pl=[dot(col,t)/tt for col in zip(*xx)];cl=[dot(col,t)/tt for col in zip(*yy)]
            weights.append(w);loadings.append(pl);responses.append(cl)
            xx=[[v-ti*pj for v,pj in zip(row,pl)] for row,ti in zip(xx,t)]
            yy=[[v-ti*cj for v,cj in zip(row,cl)] for row,ti in zip(yy,t)]
        if weights:
            pw=mm(loadings,transpose(weights));rot=mm(transpose(weights),transpose([solve(pw,[float(i==j) for i in range(len(pw))]) for j in range(len(pw))]))
            coef=mm(rot,responses)
    elif method=='svr':
        c,eps=param;aug=[row+[1.] for row in x];d=p+1
        for o in range(q):
            b=[0.]*d
            def objective(b):return dot(b,b)/2+c*sum(max(0,abs(y[i][o]-dot(row,b))-eps)**2 for i,row in enumerate(aug))
            for _ in range(100):
                g=b[:];h=[[float(i==j) for j in range(d)] for i in range(d)]
                for i,row in enumerate(aug):
                    r=dot(row,b)-y[i][o]
                    if abs(r)<=eps:continue
                    e=r-math.copysign(eps,r)
                    for j in range(d):
                        g[j]+=2*c*e*row[j]
                        for k in range(d):h[j][k]+=2*c*row[j]*row[k]
                if max(map(abs,g))<1e-9:break
                step=solve(h,g);rate=1;old=objective(b)
                while rate>1e-10:
                    trial=[v-rate*w for v,w in zip(b,step)]
                    if objective(trial)<=old-1e-4*rate*dot(g,step)+1e-14:break
                    rate/=2
                b=trial
            else:raise AssertionError('SVR primal oracle failed')
            for j in range(p):coef[j][o]=b[j]
            intercept[o]=b[-1]
    else:raise ValueError(method)
    if method!='svr':intercept=[ym[o]-sum(xm[j]*coef[j][o] for j in range(p)) for o in range(q)]
    return lambda rows:[[f32(intercept[o]+sum(row[j]*coef[j][o] for j in range(p))) for o in range(q)] for row in rows],coef

def preprocess(x,train,z=None,scale='feature_zscore',y=None,count=None):
    a=[row[:] for row in x];p=len(a[0])
    if z is not None:
        design=[[1.,v] for v in z];gram=mm(transpose([design[i] for i in train]),[design[i] for i in train])
        beta=[solve(gram,[sum(design[i][j]*a[i][k] for i in train) for j in range(2)]) for k in range(p)]
        a=[[v-dot(design[i],beta[j]) for j,v in enumerate(row)] for i,row in enumerate(a)]
    if scale=='feature_zscore':
        m=mean([a[i] for i in train]);sd=[math.sqrt(sum((a[i][j]-m[j])**2 for i in train)/len(train)) for j in range(p)]
        a=[[f32((v-m[j])/sd[j]) if sd[j]>1e-10 else 0 for j,v in enumerate(row)] for row in a]
    else:a=[[f32(v) for v in row] for row in a]
    if count:
        ranks=[];yy,_=centered([y[i] for i in train])
        for j in range(p):
            v=[a[i][j] for i in train];mu=sum(v)/len(v);v=[w-mu for w in v];vx=dot(v,v)
            ranks.append(sum(dot(v,col)**2/(vx*dot(col,col)) if vx*dot(col,col)>1e-20 else 0 for col in zip(*yy)))
        ids=sorted(range(p),key=lambda j:(-ranks[j],j))[:count];a=[[row[j] for j in ids] for row in a]
    return a

def pca_rows(x,train,count):
    centered_train,m=centered([x[i] for i in train]);gram=mm(transpose(centered_train),centered_train)
    loadings=[];largest=0
    for k in range(min(count,len(m),len(train)-1)):
        vector,value=leading_eigen(gram)
        if k==0:largest=value
        if value<=largest*64*sys.float_info.epsilon*max(len(train),len(m)):break
        loadings.append(vector)
        gram=[[v-value*vector[i]*vector[j] for j,v in enumerate(row)] for i,row in enumerate(gram)]
    return [[f32(dot([v-w for v,w in zip(row,m)],axis)) for axis in loadings] or [0.] for row in x]

def loss(truth,pred,base,metric):
    values=[]
    for y,p,b in zip(zip(*truth),zip(*pred),zip(*base)):
        e=[v-w for v,w in zip(y,p)];sse=dot(e,e)
        if metric=='normalized_mse':
            den=sum((v-w)**2 for v,w in zip(y,b))
            if den:values.append(sse/den)
        elif metric=='MAE':values.append(sum(map(abs,e))/len(e))
        elif metric=='RMSE':values.append(math.sqrt(sse/len(e)))
        else:values.append(sse/len(e))
    return sum(values)/len(values) if values else math.nan

METHODS={
    'elastic_net':(['-enet_alpha_grid','.03,.4','-enet_l1_ratio_grid','.25,.8'],[(.03,.25),(.03,.8),(.4,.25),(.4,.8)]),
    'banded_ridge':(['-feature_groups','groups.1D','-banded_penalties','penalties.1D'],[(.2,3),(4,.1),(1,1)]),
    'pls':(['-pls_components_grid','1,2,3'],[1,2,3]),
    'svr':(['-svr_c_grid','.1,1','-svr_epsilon_grid','0,.3'],[(.1,0),(.1,.3),(1,0),(1,.3)])}

def main():
    with tempfile.TemporaryDirectory(prefix='mvpa-regression-') as folder:
        os.chdir(folder);rng=random.Random(28353031);n=24;p=4;q=2
        x=[[f32(rng.gauss(0,1)) for j in range(p)] for i in range(n)]
        y=[[f32(1+2*row[0]-.5*row[1]+rng.gauss(0,.2)),f32(-2+row[2]+.6*row[0]+rng.gauss(0,.2))] for row in x]
        groups=[i//6 for i in range(n)];z=[f32(rng.gauss(0,1)) for _ in range(n)]
        names=[f'X{j}' for j in range(p)];nifti('x.nii',x);nifti('y.nii',y);nifti('mask.nii',[[1]*p]);nifti('ymask.nii',[[1]*q]);nifti('roi.nii',[[1,1,2,2]])
        Path('groups.1D').write_text('Group Column\n'+'\n'.join(f'g{j%2} X{j}' for j in range(p))+'\n')
        Path('penalties.1D').write_text('g0 g1\n.2 3\n4 .1\n1 1\n')
        def table(name='samples.1D',xx=x,yy=y,xf='x.nii',yf='y.nii',encoding=False):
            Path(name).write_text('Subj Run Sample Split Motion Y0 Y1 '+' '.join(names)+' InputFile\n'+''.join(
                f's0 r{groups[i]} t{i} '+('test' if groups[i]==3 else 'train')+f' {z[i]:.17g} '+' '.join(f'{v:.17g}' for v in yy[i]+xx[i])+f' {yf if encoding else xf}[{i}]\n' for i in range(n)))
        table();table('encoding.1D',encoding=True)
        def base(method,fixed=False):
            encoding=method=='banded_ridge'
            return ['-mode','encode' if encoding else 'decode','-method',method,'-dataTable','@encoding.1D' if encoding else '@samples.1D',
                    '-design_columns' if encoding else '-target_columns',','.join(names) if encoding else 'Y0,Y1',
                    '-mask','ymask.nii' if encoding else 'mask.nii','-reg_tol','1e-9','-reg_max_iter','50000']+(
                    ['-partition_column','Split','-inner_cv_column','Run'] if fixed else ['-cv_column','Run'])+(['-score','pattern'] if encoding else [])
        # Every nested candidate loss and selected held-out prediction matches an independent solver.
        for method,(options,params) in METHODS.items():
            for metric in ['normalized_mse','MAE','RMSE','MSE']:
                tag=f'{method}_{metric}'
                run(base(method)+options+['-tune_metric',metric,'-prefix',tag,'-save_predictions',tag+'.pred','-save_fold_scores',tag+'.fold','-save_tuning',tag+'.all'])
                actual=read(tag+'.regression_candidates.1D');pred=read(tag+'.pred');diag=read(tag+'.regression.1D')
                for group in range(4):
                    test=[i for i,g in enumerate(groups) if g==group];train=[i for i,g in enumerate(groups) if g!=group]
                    fold=next(r['CVFold'] for r in pred if r['Sample']==f't{test[0]}')
                    expected=[]
                    for param in params:
                        truths=[];preds=[];baselines=[]
                        for inner in sorted(set(groups[i] for i in train)):
                            tr=[i for i in train if groups[i]!=inner];te=[i for i in train if groups[i]==inner]
                            xx=preprocess(x,tr);model,_=fit(method,[xx[i] for i in tr],[y[i] for i in tr],param)
                            truths += [y[i] for i in te];preds += model([xx[i] for i in te]);baselines += [mean([y[i] for i in tr])]*len(te)
                        expected.append(loss(truths,preds,baselines,metric))
                    rows=[r for r in actual if r['CVFold']==fold];assert len(rows)==len(params)
                    for r,v in zip(rows,expected):close(r['InnerLoss'],v)
                    chosen=min(range(len(params)),key=lambda i:expected[i]);assert int(next(r['Candidate'] for r in rows if r['Selected']=='1'))==chosen+1
                    xx=preprocess(x,train);model,beta=fit(method,[xx[i] for i in train],[y[i] for i in train],params[chosen]);expected_pred=model([xx[i] for i in test])
                    for o in range(q):
                        response=f'voxel_{o}' if method=='banded_ridge' else f'Y{o}'
                        for i,values in zip(test,expected_pred):close(next(r['Predicted'] for r in pred if r['Response']==response and r['Sample']==f't{i}'),values[o])
                        d=next(r for r in diag if r['Response']==response and r['CVFold']==fold)
                        if method!='banded_ridge':assert float(d['Residual'])<=1e-9
                        close(d['MAE'],sum(abs(y[i][o]-pr[o]) for i,pr in zip(test,expected_pred))/len(test))
                        close(d['RMSE'],math.sqrt(sum((y[i][o]-pr[o])**2 for i,pr in zip(test,expected_pred))/len(test)))
                        if method=='elastic_net':
                            sparse=[r for r in read(tag+'.sparse_features.1D') if r['Response']==response and r['CVFold']==fold]
                            assert len(sparse)==p
                            for j,r in enumerate(sparse):assert r['Feature']==f'voxel_{j}';close(r['Coefficient'],beta[j][o]);assert int(r['Selected'])==(beta[j][o]!=0)
                        if method=='banded_ridge':
                            contrib=[r for r in read(tag+'.group_contributions.1D') if r['Response']==response and r['CVFold']==fold]
                            trainpred=model([xx[i] for i in train]);m=sum(row[o] for row in trainpred)/len(trainpred)
                            close(sum(float(r['CovarianceWithTotal']) for r in contrib),sum((row[o]-m)**2 for row in trainpred)/(len(trainpred)-1))
                # Pooled metrics preserve each response even for pattern encoding.
                for pooled in read(tag+'.regression_scores.1D'):
                    response=pooled['Response'];o=int(response.split('_')[1]) if method=='banded_ridge' else int(response[1:])
                    rows=[r for r in pred if r['Response']==response]
                    errors=[float(r['Observed'])-float(r['Predicted']) for r in rows]
                    denominator=sum((float(r['Observed'])-float(r['TrainingMean']))**2 for r in rows)
                    close(pooled['CVR2'],1-dot(errors,errors)/denominator)
                    close(pooled['MAE'],sum(map(abs,errors))/len(errors))
                    close(pooled['RMSE'],math.sqrt(dot(errors,errors)/len(errors)))
                # Changing outer held-out outcomes and images cannot change candidate losses or trained audits.
                if metric=='normalized_mse':
                    run(base(method,True)+options+['-prefix',tag+'_fixed','-save_predictions',tag+'_fixed.pred'])
                    mutx=[row[:] for row in x];muty=[row[:] for row in y]
                    for i in range(18,n):mutx[i]=[f32(v*40+300) for v in mutx[i]];muty[i]=[f32(v*80-500) for v in muty[i]]
                    nifti('mutx.nii',mutx);nifti('muty.nii',muty)
                    table('mut.1D',mutx,muty,'mutx.nii','muty.nii',method=='banded_ridge')
                    args=base(method,True);args[args.index('-dataTable')+1]='@mut.1D'
                    run(args+options+['-prefix',tag+'_mut'])
                    assert read(tag+'_fixed.regression_candidates.1D')==read(tag+'_mut.regression_candidates.1D')
                    fields=['Candidate','Iterations','Residual','Objective','Components','Nonzero']
                    assert [[r[k] for k in fields] for r in read(tag+'_fixed.regression.1D')]==[[r[k] for k in fields] for r in read(tag+'_mut.regression.1D')]
                    for suffix in ['sparse_features'] if method=='elastic_net' else ['group_contributions'] if method=='banded_ridge' else []:
                        assert read(tag+'_fixed.'+suffix+'.1D')==read(tag+'_mut.'+suffix+'.1D')
            # Worker determinism and permutation restoration in volume searchlights.
            for workers in (1,2):
                tag=f'{method}_sl{workers}';args=base(method)+options+['-searchlight','SPHERE(1.1)','-prefix',tag,'-save_predictions',tag+'.pred','-nperm','5','-permute_within','Run','-seed','183']
                run(args,workers=workers)
            for suffix in ['continuous.1D','regression.1D','regression_candidates.1D','inference.1D','pred']:
                assert read(f'{method}_sl1.'+suffix)==read(f'{method}_sl2.'+suffix)
            tag=f'{method}_plain';run(base(method)+options+['-searchlight','SPHERE(1.1)','-prefix',tag,'-save_predictions',tag+'.pred'])
            for suffix in ['regression.1D','regression_candidates.1D','pred']:
                assert read(tag+'.'+suffix)==read(f'{method}_sl1.'+suffix)
            # Forced nonconvergence must be a hard failure, not a score/map.
            if method!='banded_ridge':
                args=base(method);args[args.index('-reg_max_iter')+1]='1';args[args.index('-reg_tol')+1]='1e-12'
                msg=run(args+options+['-prefix',method+'_fail'],ok=False);assert 'did not converge' in msg
            print('PASS nested oracles, outer isolation, volume workers/permutations:',method)
        for method,(options,_) in METHODS.items():
            for suffix in ['regression','regression_scores','regression_candidates']:
                run(base(method)+options+['-prefix','collision','-save_predictions','collision.'+suffix+'.1D'],ok=False)
            text=run(base(method)+options+['-prefix','validate_'+method,'-validate_only'])
            assert '# Estimator='+method in text
            assert not list(Path('.').glob('validate_'+method+'*'))
        # Unsupported scientific contracts and malformed options fail before fitting.
        for extra in [['-lambda','1'],['-lambda_per_response'],['-pca_components','2'],['-save_model','no.model'],['-save_coefficients','no.coef'],['-save_patterns','no.pattern']]:
            run(base('elastic_net')+METHODS['elastic_net'][0]+extra+['-prefix','invalid'],ok=False)
        run(base('banded_ridge')+METHODS['banded_ridge'][0]+['-select_features','2','-prefix','invalid'],ok=False)
        run(base('svr')+['-mode','encode','-design_columns','X0,X1','-score','pattern','-prefix','invalid'],ok=False)
        run(base('pls')+['-pls_components','1.5','-prefix','invalid'],ok=False)
        run(base('elastic_net')+['-enet_l1_ratio','1.1','-prefix','invalid'],ok=False)
        run(base('svr')+['-svr_epsilon','-1','-prefix','invalid'],ok=False)
        original=Path('groups.1D').read_text();Path('groups.1D').write_text(original.replace('g0 X0','g0 X1'))
        run(base('banded_ridge')+METHODS['banded_ridge'][0]+['-prefix','invalid'],ok=False);Path('groups.1D').write_text(original)
        # Fold-local nuisance and count selection compose with all compatible estimators.
        for method in ['elastic_net','pls','svr']:
            tag=method+'_selection';options,params=METHODS[method]
            run(base(method,True)+options+['-nuisance_columns','Motion','-select_features_grid','2,3','-prefix',tag,'-save_predictions',tag+'.pred'])
            actual=read(tag+'.regression_candidates.1D');train=list(range(18));expected=[]
            for count in [2,3]:
                for param in params:
                    truth=[];pred=[];baseline=[]
                    for group in range(3):
                        tr=[i for i in train if groups[i]!=group];te=[i for i in train if groups[i]==group]
                        xx=preprocess(x,tr,z=z,y=y,count=count);model,_=fit(method,[xx[i] for i in tr],[y[i] for i in tr],param)
                        truth += [y[i] for i in te];pred += model([xx[i] for i in te]);baseline += [mean([y[i] for i in tr])]*len(te)
                    expected.append(loss(truth,pred,baseline,'normalized_mse'))
            for r,v in zip(actual,expected):close(r['InnerLoss'],v,5e-5)
            assert len(actual)==len(expected)
        # Compatible estimators compose selection/PCA/model grids inside each training split.
        for method in ['pls','svr']:
            tag=method+'_pca';options,params=METHODS[method]
            run(base(method,True)+options+['-nuisance_columns','Motion','-select_features_grid','2,3','-pca_components_grid','1,2','-prefix',tag])
            actual=read(tag+'.regression_candidates.1D');train=list(range(18));expected=[]
            for count in [2,3]:
                for components in [1,2]:
                    for param in params:
                        truth=[];pred=[];baseline=[]
                        for group in range(3):
                            tr=[i for i in train if groups[i]!=group];te=[i for i in train if groups[i]==group]
                            xx=preprocess(x,tr,z=z,y=y,count=count);xx=pca_rows(xx,tr,components)
                            model,_=fit(method,[xx[i] for i in tr],[y[i] for i in tr],param)
                            truth += [y[i] for i in te];pred += model([xx[i] for i in te]);baseline += [mean([y[i] for i in tr])]*len(te)
                        expected.append(loss(truth,pred,baseline,'normalized_mse'))
            assert len(actual)==len(expected)
            for r,v in zip(actual,expected):close(r['InnerLoss'],v,8e-5)
        # PLS encoding: joint pattern response domain differs from independent voxel fits.
        for score in ['pattern','voxel']:
            args=base('banded_ridge');args[args.index('-method')+1]='pls';args[args.index('-score')+1]=score
            tag='pls_encode_'+score;run(args+METHODS['pls'][0]+['-prefix',tag,'-save_predictions',tag+'.pred'])
            pred=read(tag+'.pred')
            for group in range(4):
                tr=[i for i in range(n) if groups[i]!=group];te=[i for i in range(n) if groups[i]==group]
                fold=next(r['CVFold'] for r in pred if r['Sample']==f't{te[0]}');xx=preprocess(x,tr)
                for o in range(q):
                    diag=next(r for r in read(tag+'.regression.1D') if r['CVFold']==fold and r['Response']==f'voxel_{o}')
                    chosen=METHODS['pls'][1][int(diag['Candidate'])-1];response=y if score=='pattern' else [[row[o]] for row in y]
                    model,_=fit('pls',[xx[i] for i in tr],[response[i] for i in tr],chosen);expected=model([xx[i] for i in te])
                    for i,values in zip(te,expected):close(next(r['Predicted'] for r in pred if r['Sample']==f't{i}' and r['Response']==f'voxel_{o}'),values[o if score=='pattern' else 0])
        print('PASS input rejection, nuisance/selection composition, joint/voxel encoding domains')
    print('PASS continuous estimator integration and independent numeric oracles')

if __name__ == '__main__':
    main()
