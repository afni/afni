#!/usr/bin/env python3
"""Logistic numerical/integration checks; independent dense Newton oracle, stdlib only.

Oracle fixes the final intercept to zero and fits all K weight vectors using
an explicit Hessian. Production uses centered intercepts and L-BFGS.
"""
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
env = dict(os.environ, AFNI_NIFTI_TYPE_WARN='NO', AFNI_NIFTI_VIEW='orig',
           OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1')


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
    v = [x for row in rows for x in row]
    path.write_bytes(h+struct.pack('<%df' % len(v), *v))


def read(path):
    with path.open() as f:
        return list(csv.DictReader((s[11:] if s.startswith('# Columns: ') else s
                    for s in f if not s.startswith('#') or s.startswith('# Columns: ')), delimiter='\t'))


def solve(a, b):
    a = [list(row)+[v] for row,v in zip(a,b)]
    for j in range(len(a)):
        pivot = max(range(j,len(a)), key=lambda i:abs(a[i][j]))
        a[j],a[pivot] = a[pivot],a[j]
        d=a[j][j]; assert abs(d)>1e-14
        a[j]=[v/d for v in a[j]]
        for i in range(len(a)):
            if i!=j:
                d=a[i][j];a[i]=[v-d*w for v,w in zip(a[i],a[j])]
    return [row[-1] for row in a]


def softmax(z):
    p = [math.exp(v-max(z)) for v in z]; s=sum(p)
    return [v/s for v in p]


def losses(prob, y):
    return -math.log(prob[y]), sum((v-(c==y))**2 for c,v in enumerate(prob))


def fit(x,y,k,penalty):
    n=len(x);p=len(x[0]); terms=[(j,c) for j in range(p) for c in range(k)]+[(p,c) for c in range(k-1)]
    d=len(terms);b=[0.]*d; xx=[row+[1.] for row in x]
    def eval_at(b, hessian=True):
        value=0;g=[0.]*d;h=[[0.]*d for _ in range(d)]
        for row,truth in zip(xx,y):
            z=[sum(b[a]*row[j] for a,(j,c) in enumerate(terms) if c==cl) for cl in range(k)]
            prob=softmax(z);value+=(max(z)-z[truth]+math.log(sum(math.exp(v-max(z)) for v in z)))/n
            for a,(j,c) in enumerate(terms):
                g[a]+=(prob[c]-(truth==c))*row[j]/n
                if hessian:
                    for t,(q,cl) in enumerate(terms):
                        h[a][t]+=prob[c]*((c==cl)-prob[cl])*row[j]*row[q]/n
        for a,(j,c) in enumerate(terms):
            if j<p:
                value+=penalty*b[a]*b[a]/2;g[a]+=penalty*b[a];h[a][a]+=penalty
        return value,g,h
    for it in range(80):
        v,g,h=eval_at(b)
        if max(map(abs,g))<2e-10: break
        delta=solve(h,g);step=1
        while step>1e-10:
            trial=[v-step*w for v,w in zip(b,delta)]
            if eval_at(trial,False)[0]<=v+1e-14:break
            step/=2
        b=trial
    else:raise AssertionError('independent Newton oracle failed')
    def predict(rows):
        return [softmax([sum(b[a]*(row+[1])[j] for a,(j,c) in enumerate(terms) if c==cl) for cl in range(k)]) for row in rows]
    return predict


def preprocess(x, train, scale):
    if scale=='none':return x
    mean=[sum(x[i][j] for i in train)/len(train) for j in range(len(x[0]))]
    sd=[math.sqrt(sum((x[i][j]-mean[j])**2 for i in train)/len(train)) for j in range(len(mean))]
    # Production stores transformed rows as floats.
    f32=lambda v:struct.unpack('f',struct.pack('f',v))[0]
    return [[f32((v-mean[j])/sd[j]) if sd[j]>1e-10 else 0 for j,v in enumerate(row)] for row in x]


def run(args, ok=True, workers=1):
    result=subprocess.run([binary]+list(map(str,args)),env=dict(env,OMP_NUM_THREADS=str(workers)),text=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    assert (result.returncode==0)==ok, result.stdout+result.stderr
    return result.stdout+result.stderr


def close(a,b,tol=3e-7):
    assert abs(float(a)-float(b))<tol,(a,b)


with tempfile.TemporaryDirectory(prefix='mvpa-logistic-') as td:
    td=Path(td);os.chdir(td)
    rng=random.Random(29);workroot=td
    for k in (2,3):
        td=workroot/str(k);td.mkdir();os.chdir(td)
        x=[];y=[];runs=[];subjects=[]
        for sub in range(2):
            for r in range(4):
                for c in range(k):
                    for rep in range(2):
                        x.append([round(rng.gauss(c*.35,1)+sub*.2,4),round(rng.gauss(c*.7,1),4),round(rng.gauss(0,1),4),1.])
                        y.append(c);runs.append(sub*4+r);subjects.append(sub)
        x=[[struct.unpack('f',struct.pack('f',v))[0] for v in row] for row in x]
        nifti(td/'x.nii',x);nifti(td/'mask.nii',[[1]*4]);nifti(td/'roi2.nii',[[1,1,2,2]])
        def table(name, labels=y, data='x.nii'):
            text='Subj Run Sample Class Motion Split Item Session Task InputFile\n'
            for i,c in enumerate(labels):
                text+=f's{subjects[i]} r{runs[i]} t{i} c{c} {((i*7)%13)/13:.8f} '+('test' if runs[i]%4==3 else 'train')+f' item{i} '+('post target' if runs[i]%4==3 else 'pre source')+f' {data}[{i}]\n'
            Path(name).write_text(text)
        table('samples.1D')
        base=['-mode','classify','-method','logistic','-dataTable','@samples.1D','-target_column','Class','-mask','mask.nii','-cv_column','Run','-within_column','Subj','-logistic_tol','1e-9']
        grid=[.05,.5,5.]
        for objective in ('log_loss','brier','ACC','BACC'):
            prefix=f'k{k}_{objective}'
            run(base+['-scale','feature_zscore','-logistic_lambda_grid','.05,.5,5','-tune_metric',objective,'-save_tuning',prefix+'.candidates','-save_predictions',prefix+'.pred','-save_fold_scores',prefix+'.fold','-save_probability_scores',prefix+'.scores','-save_calibration',prefix+'.cal','-prefix',prefix])
            pred=read(Path(prefix+'.pred'));candidates=read(Path(prefix+'.candidates'));folds=read(Path(prefix+'.fold'))
            assert len(pred)==len(x) and len(candidates)==8*3
            for fold in folds:
                test=[int(r['Sample'][1:]) for r in pred if r['CVFold']==fold['CVFold']]
                train=[i for i in range(len(x)) if subjects[i]==subjects[test[0]] and i not in test]
                expected=[]
                for pen in grid:
                    probs=[];truth=[]
                    for group in sorted(set(runs[i] for i in train)):
                        itrain=[i for i in train if runs[i]!=group];itest=[i for i in train if runs[i]==group]
                        xx=preprocess(x,itrain,'feature_zscore');model=fit([xx[i] for i in itrain],[y[i] for i in itrain],k,pen)
                        probs+=model([xx[i] for i in itest]);truth+=[y[i] for i in itest]
                    if objective in ('ACC','BACC'):
                        # Each test group is class-balanced, so ACC == BACC.
                        loss=sum(max(range(k),key=lambda c:pr[c])!=c for pr,c in zip(probs,truth))/len(truth)
                    else:loss=sum(losses(pr,c)[objective=='brier'] for pr,c in zip(probs,truth))/len(truth)
                    expected.append(loss)
                actual=[r for r in candidates if r['CVFold']==fold['CVFold'] and r['CVStratum']==fold['Stratum']]
                for r,v in zip(actual,expected):close(r['InnerLoss'],v)
                best=min(range(3),key=lambda j:(expected[j],-grid[j]))
                close(fold['Lambda'],grid[best]);assert float(fold['SolverResidual'])<=1e-9
                xx=preprocess(x,train,'feature_zscore');model=fit([xx[i] for i in train],[y[i] for i in train],k,grid[best]);probs=model([xx[i] for i in test])
                for i,pr in zip(test,probs):
                    actualpr=[float(pred[i][f'ProbabilityClass{c}']) for c in range(k)]
                    close(sum(actualpr),1,1e-12)
                    for a,b in zip(actualpr,pr):close(a,b)
                close(fold['LogLoss'],sum(losses(pr,y[i])[0] for i,pr in zip(test,probs))/len(test))
                close(fold['Brier'],sum(losses(pr,y[i])[1] for i,pr in zip(test,probs))/len(test))
            probs=[[float(r[f'ProbabilityClass{c}']) for c in range(k)] for r in pred]
            for score in read(Path(prefix+'.scores')):
                if score['Scope']=='Pooled':
                    close(score['LogLoss'],sum(losses(pr,c)[0] for pr,c in zip(probs,y))/len(y))
                    close(score['Brier'],sum(losses(pr,c)[1] for pr,c in zip(probs,y))/len(y))
            for row in read(Path(prefix+'.cal')):
                c=int(row['ClassID']);b=int(row['Bin']);idx=[i for i,pr in enumerate(probs) if min(9,int(pr[c]*10))==b]
                assert int(row['NTest'])==len(idx)
                if idx:
                    close(row['MeanProbability'],sum(probs[i][c] for i in idx)/len(idx),1e-12)
                    close(row['ObservedFrequency'],sum(y[i]==c for i in idx)/len(idx),1e-12)
                else:assert row['MeanProbability']==row['ObservedFrequency']=='NA'
        # Fixed holdout with joint feature/PCA/penalty search and nuisance fits.
        fixed=[v for v in base] # remove paired outer CV options
        for opt in ('-cv_column','-within_column'):
            at=fixed.index(opt);del fixed[at:at+2]
        joint=['-partition_column','Split','-inner_cv_column','Run','-logistic_lambda','auto','-nuisance_columns','Motion','-select_features_grid','2,4','-pca_components_grid','1,2']
        run(fixed+joint+['-prefix','joint','-save_predictions','joint.pred','-save_tuning','joint.tune','-save_probability_scores','joint.scores'])
        jointpred=read(Path('joint.pred'));assert all(r['ProbabilityClass0']=='NA' for i,r in enumerate(jointpred) if runs[i]%4!=3)
        altered=[(c+1)%k if runs[i]%4==3 else c for i,c in enumerate(y)];table('altered.1D',altered)
        alt=fixed.copy();alt[alt.index('@samples.1D')]='@altered.1D'
        run(alt+joint+['-prefix','altered','-save_predictions','altered.pred','-save_tuning','altered.tune'])
        assert Path('joint.tune').read_bytes()==Path('altered.tune').read_bytes(),'test labels leaked into tuning'
        for a,b in zip(jointpred,read(Path('altered.pred'))):
            for c in range(k):assert a[f'ProbabilityClass{c}']==b[f'ProbabilityClass{c}']
        # Two input tables reproduce the same pooled fixed-partition fit.
        header,*rows=Path('samples.1D').read_text().splitlines()
        Path('train.1D').write_text(header+'\n'+'\n'.join(r for r in rows if ' train ' in r)+'\n')
        Path('test.1D').write_text(header+'\n'+'\n'.join(r for r in rows if ' test ' in r)+'\n')
        two=fixed.copy();two[two.index('@samples.1D')]='@train.1D'
        run(two+['-testTable','@test.1D']+joint[2:]+['-prefix','two','-save_predictions','two.pred'])
        lookup={r['Sample']:r for r in read(Path('two.pred'))}
        for r in jointpred:
            for c in range(k):assert r[f'ProbabilityClass{c}']==lookup[r['Sample']][f'ProbabilityClass{c}']
        run(fixed+['-transfer_column','Task','-train_domain','source','-test_domain','target','-transfer_subjects','within','-inner_cv_column','Run','-logistic_lambda','auto','-prefix','transfer','-save_predictions','transfer.pred','-save_probability_scores','transfer.scores'])
        for row in read(Path('transfer.scores')):
            if row['Scope']=='Subject':assert int(row['NTest'])==2*k
        # Exact ties for all-constant balanced patterns choose larger lambda.
        nifti(td/'constant.nii',[[1]*4 for _ in x]);table('constant.1D',data='constant.nii')
        const=base.copy();const[const.index('@samples.1D')]='@constant.1D'
        run(const+['-logistic_lambda_grid','5,.05,.5','-prefix','constant','-save_fold_scores','constant.fold'])
        assert all(float(row['Lambda'])==5 for row in read(Path('constant.fold')))
        # Probability output remains byte-identical after permutation inference.
        for suffix,perm in [('obs',[]),('perm',['-nperm','99','-permute_within','Run','-seed','29'])]:
            run(base+['-prefix',suffix,'-save_predictions',suffix+'.pred','-save_fold_scores',suffix+'.fold','-logistic_lambda','.5']+perm)
        assert Path('obs.pred').read_bytes()==Path('perm.pred').read_bytes()
        assert Path('obs.fold').read_bytes()==Path('perm.fold').read_bytes()
        run(['-compare_predictions','obs.pred','perm.pred','-prefix','paired'])
        assert all(float(r['DeltaBminusA'])==0 for r in read(Path('paired.comparison.1D')) if r['DeltaBminusA']!='NA')
        # Parallel volume searchlight outputs are deterministic.
        for workers in (1,2):
            run(base+['-searchlight','SPHERE(1.1)','-prefix',f'search{workers}','-save_predictions',f'search{workers}.pred','-save_calibration',f'search{workers}.cal'],workers=workers)
        assert Path('search1.pred').read_bytes()==Path('search2.pred').read_bytes()
        assert Path('search1.cal').read_bytes()==Path('search2.cal').read_bytes()
        # Invalid requests must not silently select a different estimator or objective.
        for opts in (['-logistic_lambda','0'],['-logistic_lambda','1','-logistic_lambda_grid','1,2'],['-logistic_lambda_grid','1,1'],['-logistic_lambda','auto','-tune_metric','MSE'],['-save_model','bad.model'],['-save_patterns','bad.pattern'],['-save_coefficients','bad.coef'],['-metric','log_loss'],['-distance','cosine'],['-svm_c','1'],['-logistic_max_iter','1','-logistic_tol','1e-12']):
            run(base+['-prefix','bad']+opts,False)
        for path in ('samples.1D','x.nii','mask.nii','bad.roi.1D'):
            run(base+['-prefix','bad','-save_calibration',path],False)
        run(base+['-prefix','bad','-save_calibration','same','-save_predictions','same'],False)
        centroid=base.copy();centroid[centroid.index('logistic')]='centroid';at=centroid.index('-logistic_tol');del centroid[at:at+2]
        run(centroid+['-select_features_grid','1,2','-tune_metric','brier','-prefix','bad'],False)
        run(base+['-prefix','validate','-logistic_lambda','auto','-validate_only'])
        assert not list(td.glob('validate*'))
    print('PASS logistic binary/multinomial Newton oracle, nested objectives, probabilities/calibration, leakage, permutations, parallel searchlights and rejection checks')
