#!/usr/bin/env python3
"""Independent OLS/ridge and permutation oracles for fold-local nuisance adjustment."""
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
    h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    struct.pack_into('<8h', h, 40, 4, 2, 1, 1, len(volumes), 1, 1, 1)
    struct.pack_into('<2h', h, 70, 16, 32)
    struct.pack_into('<8f', h, 76, *([1]*8))
    struct.pack_into('<f', h, 108, 352)
    struct.pack_into('<h', h, 254, 1)
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
        d = a[j][j]; assert abs(d) > 1e-10
        a[j] = [v/d for v in a[j]]
        for i in range(len(a)):
            if i != j:
                d = a[i][j]; a[i] = [v-d*w for v, w in zip(a[i], a[j])]
    return [row[-1] for row in a]

def residual(x, z, train):
    d = [[1, v] for v in z]
    gram = [[sum(d[i][j]*d[i][k] for i in train) for k in range(2)] for j in range(2)]
    coef = [solve(gram, [sum(d[i][j]*x[i][f] for i in train) for j in range(2)]) for f in range(len(x[0]))]
    return [[v-sum(a*b for a, b in zip(d[i], coef[f])) for f, v in enumerate(row)] for i, row in enumerate(x)]

def ridge(x, y, z, train, test, lam=1):
    x = residual(x, z, train); p = len(x[0]); n = len(train)
    mean = [sum(x[i][j] for i in train)/n for j in range(p)]
    sd = [math.sqrt(sum((x[i][j]-mean[j])**2 for i in train)/n) for j in range(p)]
    x = [[(v-mean[j])/sd[j] if sd[j] > 1e-10 else 0 for j, v in enumerate(row)] for row in x]
    ym = sum(y[i] for i in train)/n
    a = [[sum(x[i][j]*x[i][k] for i in train)+(lam if j == k else 0) for k in range(p)] for j in range(p)]
    b = [sum(x[i][j]*(y[i]-ym) for i in train) for j in range(p)]
    coef = solve(a, b)
    return [ym+sum(v*w for v, w in zip(x[i], coef)) for i in test], ym

with tempfile.TemporaryDirectory(prefix='mvpa_nuisance_') as folder:
    root = Path(folder); n = 16; train = list(range(12)); test = list(range(12,16))
    z = [-1 if i % 4 < 2 else 1 for i in range(n)]
    signal = [-1 if i % 2 == 0 else 1 for i in range(n)]
    x = [[10+4*z[i]+2*signal[i]+.1*(i//4)+.037*math.sin(i+1), 3-2*z[i]+.5*signal[i]+.043*math.cos(i+1)] for i in range(n)]
    y = [3+1.5*signal[i]+.2*z[i]+.3*(i//4)+.029*math.sin(2*i+1) for i in range(n)]
    design = [[signal[i]+2*z[i]+.2*(i//4)] for i in range(n)]
    nifti(root/'betas.nii', x); nifti(root/'mask.nii', [[1,1]])
    rows = [dict(Sample=f's{i}', Run='all', Fold=str(i//4), Split='train' if i<12 else 'test',
                 Block=str(i//2), Condition='A' if signal[i]<0 else 'B', Z=str(z[i]), Y=str(y[i]), X=str(design[i][0]),
                 InputFile=str(root/'betas.nii')+f'[{i}]') for i in range(n)]
    def table(name, records):
        path = root/name
        with path.open('w') as f:
            w = csv.DictWriter(f, list(records[0]), delimiter='\t'); w.writeheader(); w.writerows(records)
        return path
    source = table('samples.1D', rows)
    def run(args, failure=None, workers=2):
        r = subprocess.run([binary, *map(str, args)], cwd=root, capture_output=True, text=True,
                           env=dict(os.environ, OMP_NUM_THREADS=str(workers)), timeout=90)
        text = r.stdout+r.stderr
        assert (r.returncode == 0) if failure is None else (r.returncode != 0 and failure in text), text
        return text
    def command(name, mode='classify', extra=(), src=source, adjust=True):
        return ['-mode',mode,'-dataTableFile',src,'-partition_column','Split','-mask',root/'mask.nii',
                '-design_columns' if mode=='encode' else '-target_column', 'X' if mode=='encode' else 'Y' if mode=='decode' else 'Condition',
                '-prefix',name,'-save_predictions',name+'.pred.1D','-save_fold_scores',name+'.fold.1D',
                *(['-nuisance_columns','Z'] if adjust else []), *(['-score','pattern'] if mode=='encode' else []), *extra]
    def prediction(name):
        return {(r.get('UnitID',r.get('Unit')),r.get('Response'),r['Sample']):r for r in read(root/(name+'.pred.1D'))}
    def same(a,b):
        aa,bb=prediction(a),prediction(b);assert aa.keys()==bb.keys()
        for key,r in aa.items():
            for col in ['Predicted']+[c for c in r if c.startswith('DecisionClass')]:
                v,w=r[col],bb[key][col]
                if v in ('NA','A','B'):assert v==w,(key,col,v,w)
                else:assert math.isclose(float(v),float(w),abs_tol=3e-5,rel_tol=3e-5),(key,col,v,w)
    # Every classifier must match a separate run using independently adjusted inputs.
    nifti(root/'residual.nii', residual(x,z,train))
    adjusted = table('adjusted.1D',[dict(r,InputFile=str(root/'residual.nii')+f'[{i}]') for i,r in enumerate(rows)])
    for method in ['centroid','ridge','svm','lda_diag','lda_shrinkage']:
        run(command(method,extra=['-method',method]))
        run(command(method+'_oracle',extra=['-method',method],src=adjusted,adjust=False))
        same(method,method+'_oracle')
    nifti(root/'confound.nii', [[5+4*v,3-2*v] for v in z])
    confound=table('confound.1D',[dict(r,Condition='A' if z[i]<0 else 'B',InputFile=str(root/'confound.nii')+f'[{i}]') for i,r in enumerate(rows)])
    run(command('confound_raw',src=confound,adjust=False));run(command('confound_adjusted',src=confound))
    assert float(read(root/'confound_raw.roi.1D')[0]['ACC'])==1
    assert float(read(root/'confound_adjusted.roi.1D')[0]['ACC'])==.5
    for scale in ['none','sample_center','run_center']:
        run(command(scale,extra=['-scale',scale]));run(command(scale+'_oracle',extra=['-scale',scale],src=adjusted,adjust=False));same(scale,scale+'_oracle')
    # Continuous predictions are in original response units with original training-mean baselines.
    for mode in ['decode','encode']:
        run(command(mode,mode))
        expected,baseline = ridge(x if mode=='decode' else design,y if mode=='decode' else [r[0] for r in x],z,train,test)
        checked=0
        for r in read(root/(mode+'.pred.1D')):
            i=int(r['Sample'][1:])
            if i<12:assert r['Predicted']=='NA';continue
            if mode=='encode' and r['Response'] not in ('0','voxel_0'):continue
            assert math.isclose(float(r['Predicted']),expected[i-12],rel_tol=3e-5,abs_tol=3e-5),(mode,r,expected)
            assert math.isclose(float(r['TrainingMean']),baseline,abs_tol=3e-5)
            checked+=1
        assert checked==4
    run(command('encode_voxel','encode',['-score','voxel']))
    for r in read(root/'encode_voxel.pred.1D'):
        i=int(r['Sample'][1:]);j=int(r['Response'].split('_')[1])
        if i in test:
            expected,_=ridge(design,[v[j] for v in x],z,train,test)
            assert math.isclose(float(r['Predicted']),expected[i-12],rel_tol=3e-5,abs_tol=3e-5)
    # Independent nested lambda oracle: nuisance OLS is fitted separately inside each inner fold.
    grid=[.1,1,10];loss=[]
    for lam in grid:
        sse=base=0
        for group in range(3):
            tr=[i for i in train if i//4!=group];te=[i for i in train if i//4==group]
            pred,mean=ridge(x,y,z,tr,te,lam)
            sse+=sum((y[i]-v)**2 for i,v in zip(te,pred));base+=sum((y[i]-mean)**2 for i in te)
        loss.append(sse/base)
    chosen=min(range(3),key=lambda i:(loss[i],-grid[i]))
    run(command('nested','decode',['-lambda_grid','.1,1,10','-inner_cv_column','Fold']))
    audit=read(root/'nested.tuning.1D')
    for r in audit:
        lam=float(r['Lambda']);assert math.isclose(float(r['InnerLoss']),loss[grid.index(lam)],rel_tol=3e-5,abs_tol=3e-5),r
    assert float(read(root/'nested.fold.1D')[0]['Lambda'])==grid[chosen]
    # Held-out patterns, covariates and outcomes cannot alter other predictions or tuning.
    changed_x=[list(r) for r in x];changed_x[-1]=[5000,-2000];nifti(root/'changed.nii',changed_x)
    changed=table('changed.1D',[dict(r,InputFile=str(root/'changed.nii')+f'[{i}]',Z='1000' if i==15 else r['Z'],Y='-9000' if i==15 else r['Y']) for i,r in enumerate(rows)])
    for mode,opts in [('decode',['-lambda_grid','.1,1,10','-inner_cv_column','Fold']),('classify',['-method','svm','-svm_c_grid','.1,1','-inner_cv_column','Fold'])]:
        a='stable_'+mode;b='changed_'+mode
        run(command(a,mode,opts));run(command(b,mode,opts,src=changed))
        aa,bb=prediction(a),prediction(b)
        for key,r in aa.items():
            if r['Sample']!='s15':assert r['Predicted']==bb[key]['Predicted'],(r,bb[key])
        assert (root/(a+'.tuning.1D')).read_bytes()==(root/(b+'.tuning.1D')).read_bytes()
    # Exact blocked classification null: enumerate all 2^8 allowed label swaps.
    run(command('null',extra=['-nperm','999','-permute_within','Block']))
    rx=residual(x,z,train)
    mean=[sum(rx[i][j] for i in train)/12 for j in range(2)]
    sd=[math.sqrt(sum((rx[i][j]-mean[j])**2 for i in train)/12) for j in range(2)]
    rx=[[(v-mean[j])/sd[j] for j,v in enumerate(r)] for r in rx]
    null=[]
    for swaps in itertools.product([0,1],repeat=8):
        labels=[(i%2)^swaps[i//2] for i in range(n)]
        centers=[[sum(rx[i][j] for i in train if labels[i]==c)/sum(labels[i]==c for i in train) for j in range(2)] for c in range(2)]
        pred=[min(range(2),key=lambda c:sum((v-w)**2 for v,w in zip(rx[i],centers[c]))) for i in test]
        null.append(sum(a==labels[i] for a,i in zip(pred,test))/4)
    r=read(root/'null.roi.1D')[0]
    assert math.isclose(float(r['PVAL']),sum(v>=null[0] for v in null)/len(null),abs_tol=1e-7),(r,null)
    for mode in ['decode','encode']:
        run(command('null_'+mode,mode,['-nperm','999','-permute_within','Block','-metric','RMSE']))
        scores=[]
        for swaps in itertools.product([0,1],repeat=8):
            perm=[i^swaps[i//2] for i in range(n)]
            if mode=='decode':
                yp=[y[i] for i in perm];pred,_=ridge(x,yp,z,train,test)
                score=math.sqrt(sum((yp[i]-v)**2 for i,v in zip(test,pred))/4)
            else:
                xp=[design[i] for i in perm];score=0
                for j in range(2):
                    yp=[r[j] for r in x];pred,_=ridge(xp,yp,z,train,test)
                    score+=math.sqrt(sum((yp[i]-v)**2 for i,v in zip(test,pred))/4)/2
            scores.append(score)
        r=read(root/('null_'+mode+'.inference.1D'))[0]
        assert math.isclose(float(r['SCORE']),scores[0],rel_tol=3e-5,abs_tol=3e-5),(mode,r,scores[0])
        assert math.isclose(float(r['NULL_MEAN']),sum(scores)/len(scores),rel_tol=3e-5,abs_tol=3e-5)
        assert math.isclose(float(r['PVAL']),sum(v<=scores[0]+1e-8 for v in scores)/len(scores),abs_tol=1e-7),(mode,r,scores)
    # Volume searchlights use identical worker-local preprocessing.
    for workers in [1,2]:run(command('spatial'+str(workers),extra=['-searchlight','SPHERE(2)']),workers=workers)
    assert (root/'spatial1.pred.1D').read_bytes()==(root/'spatial2.pred.1D').read_bytes()
    # Single/two-table input and validation share the same path.
    trainfile=table('train.1D',rows[:12]);testfile=table('test.1D',rows[12:])
    dual=command('dual');idx=dual.index('-partition_column');del dual[idx:idx+2]
    dual[dual.index(source)]=trainfile;dual+=['-testTableFile',testfile];run(dual);same('centroid','dual')
    report=run(command('validate',extra=['-validate_only']))
    assert '# NuisanceOuterAndInnerRanks\tPASS' in report and not (root/'validate.pred.1D').exists()
    for args,msg in [(['-nperm','9','-permute_within','Split'],'nuisance values vary'),
                     (['-save_coefficients','bad'],'cannot yet export'),
                     (['-nuisance_columns','Z'],'specified twice')]:
        run(command('bad',extra=args+['-validate_only']),msg)
    for name,records,msg in [('constant',[dict(r,Z='1') for r in rows],'constant'),
                             ('nan',[dict(r,Z='nan') for r in rows],'not finite'),
                             ('short',[dict(r,Split='train' if i<2 else 'test') for i,r in enumerate(rows)],'more training rows'),
                             ('inner_rank',[dict(r,Z='1' if i<8 else r['Z']) for i,r in enumerate(rows)],'inner fold')]:
        src=table(name+'.1D',records)
        opts=['-validate_only']+(['-method','ridge','-lambda','auto','-inner_cv_column','Fold'] if name=='inner_rank' else [])
        run(command('bad_'+name,extra=opts,src=src),msg)
    redundant=table('redundant.1D',[dict(r,Z2=str(2*z[i])) for i,r in enumerate(rows)])
    run(command('bad_redundant',src=redundant,adjust=False,extra=['-nuisance_columns','Z,Z2','-validate_only']),'rank deficient')
    run(command('bad_role','decode',adjust=False,extra=['-nuisance_columns','Y','-validate_only']),'also appears')
print('PASS nuisance OLS, classifier/continuous oracles, nested tuning, leakage, exact null and spatial/input contracts')
