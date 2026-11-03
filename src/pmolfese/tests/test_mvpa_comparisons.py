#!/usr/bin/env python3
"""Independent paired-prediction metrics, identity failures, and real-run CLI checks."""
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
env = dict(os.environ, OMP_NUM_THREADS='2', AFNI_FLOATSCAN='NO')
env.pop('AFNI_DECONFLICT', None)


def read(path):
    return list(csv.DictReader((line[11:] if line.startswith('# Columns: ') else line
                for line in Path(path).read_text().splitlines()
                if not line.startswith('#') or line.startswith('# Columns: ')), delimiter='\t'))


def write(path, rows, comments=''):
    with Path(path).open('w') as f:
        f.write(comments)
        w = csv.DictWriter(f, list(rows[0]), delimiter='\t', lineterminator='\n', quoting=csv.QUOTE_ALL)
        w.writeheader(); w.writerows(rows)


def f32(s):
    return struct.unpack('f', struct.pack('f', float(s)))[0]


def corr(a, b):
    ma, mb = sum(a)/len(a), sum(b)/len(b)
    a, b = [v-ma for v in a], [v-mb for v in b]
    denom = sum(v*v for v in a)*sum(v*v for v in b)
    return sum(x*y for x, y in zip(a, b))/math.sqrt(denom) if denom else math.nan


def rank(a):
    return [sum(x<v for x in a)+(sum(x==v for x in a)+1)/2 for v in a]


def metrics(rows, classification):
    y = [r['Observed'] for r in rows]; p = [r['Predicted'] for r in rows]
    if classification:
        return dict(accuracy=sum(a==b for a,b in zip(y,p))/len(y),
                    balanced_accuracy=sum(sum(a==c and b==c for a,b in zip(y,p))/y.count(c)
                                          for c in set(y))/len(set(y)))
    y, p = list(map(f32,y)), list(map(f32,p))
    base = [f32(r['TrainingMean']) for r in rows]
    sse = sum((a-b)**2 for a,b in zip(y,p)); bse=sum((a-b)**2 for a,b in zip(y,base))
    return dict(pearson=corr(y,p),spearman=corr(rank(y),rank(p)),
                cross_validated_r2=1-sse/bse if bse else math.nan,
                mae=sum(abs(a-b) for a,b in zip(y,p))/len(y),rmse=math.sqrt(sse/len(y)))


def near(value, expected):
    if not math.isfinite(expected): assert value == 'NA', (value, expected)
    else: assert math.isclose(float(value), expected, abs_tol=3e-12, rel_tol=3e-12), (value, expected)


def verify(a, b, prefix):
    aa, bb = read(a), read(b); classification = 'Correct' in aa[0]
    def key(r):
        return (r['UnitType'] if classification else 'Continuous',
                r['UnitID'] if classification else r['Unit'],
                'Class' if classification else r['Response'],r['Sample'])
    aidx, bidx = {key(r):r for r in aa}, {key(r):r for r in bb}
    paired=read(str(prefix)+'.paired_predictions.1D');scores=read(str(prefix)+'.comparison.1D')
    assert len(paired)==len(aa)==len(bb)
    for r in paired:
        k=tuple(r[c] for c in ['UnitType','Unit','Response','Sample'])
        x,y=aidx[k],bidx[k]
        assert aa[int(r['AuditRowA'])-1] == x and bb[int(r['AuditRowB'])-1] == y
        assert r['TableRowA']==x['TableRow'] and r['TableRowB']==y['TableRow']
        for side,source in [('A',x),('B',y)]:
            for field in ['Partition','SourceTable','SourceRow']:
                assert r[field+side]==source.get(field,'NA')
        tested=x['Correct' if classification else 'Predicted']!='NA'
        assert int(r['Tested'])==tested
        if classification: assert (r['PredictedA'],r['PredictedB'])==(x['Predicted'],y['Predicted'])
        else:
            near(r['Observed'],f32(x['Observed']))
            near(r['PredictedA'],f32(x['Predicted']) if tested else math.nan)
            near(r['PredictedB'],f32(y['Predicted']) if tested else math.nan)
    expected={}
    for k,x in aidx.items():
        if x['Correct' if classification else 'Predicted']=='NA':continue
        for scope,group,stratum in [('Pooled','All','NA'),('Fold',x['CVFold'],x['CVStratum']),
                                    ('Subject',x['Subj' if classification else 'Subject'],'NA'),
                                    ('Stratum',x['CVStratum'],x['CVStratum'])]:
            expected.setdefault((*k[:3],scope,group,stratum),[]).append(k)
    seen=set()
    for r in scores:
        group=tuple(r[c] for c in ['UnitType','Unit','Response','Scope','Group','CVStratum'])
        keys=expected[group];m=r['Metric'];ma=metrics([aidx[k] for k in keys],classification)[m];mb=metrics([bidx[k] for k in keys],classification)[m]
        assert int(r['NPaired'])==len(keys)
        near(r['ScoreA'],ma);near(r['ScoreB'],mb);near(r['DeltaBminusA'],mb-ma)
        near(r['ImprovementB'],ma-mb if m in ['mae','rmse'] else mb-ma)
        assert (*group,m) not in seen;seen.add((*group,m))
    assert len(seen)==len(expected)*(2 if classification else 5)
    return scores


def nifti(path, volumes):
    h=bytearray(352);struct.pack_into('<i',h,0,348)
    struct.pack_into('<8h',h,40,4,2,2,1,len(volumes),1,1,1);struct.pack_into('<2h',h,70,16,32)
    struct.pack_into('<8f',h,76,1,1,1,1,1,1,1,1);struct.pack_into('<f',h,108,352)
    struct.pack_into('<h',h,254,1)
    for i in range(3):
        row=[0.]*4;row[i]=1.;struct.pack_into('<4f',h,280+16*i,*row)
    h[344:348]=b'n+1\0';flat=[v for row in volumes for v in row]
    path.write_bytes(h+struct.pack('<%df'%len(flat),*flat))


with tempfile.TemporaryDirectory(prefix='mvpa_comparisons_') as folder:
    root=Path(folder)
    def run(args, failure=None, workers=2, overwrite=False):
        e=dict(env,OMP_NUM_THREADS=str(workers))
        if overwrite:e['AFNI_DECONFLICT']='OVERWRITE'
        r=subprocess.run([binary,*map(str,args)],cwd=root,env=e,text=True,capture_output=True,timeout=90)
        out=r.stdout+r.stderr
        if failure is None:assert r.returncode==0,(args,out)
        else:assert r.returncode!=0 and failure in out,(args,out)
        return out
    def compare(a,b,name,**kw):
        run(['-compare_predictions',a,b,'-prefix',name],**kw)
        if 'failure' not in kw:return verify(root/a,root/b,root/name)

    # Hand-constructed unequal folds, tied ranks, absent fold classes, quoted
    # labels and a training-only row. B is reordered and TableRow differs.
    for classify in [True,False]:
        a=[];b=[]
        for unit in ['2','9']:
            for i in range(8):
                x=dict(UnitType='ROI',UnitID=unit,UnitLabel='area '+unit,I='NA',J='NA',K='NA',NFeat='3') if classify else dict(Unit=unit,Response='rating')
                x.update(TableRow=str(i+1),Sample=f'sample {i}',**{'Subj' if classify else 'Subject':'subject '+str(i//4)},
                         Run='r'+str(i//2),Item='item#'+str(i),InputFold='NA',CVFold='f'+str(i//3) if i<7 else 'NA',
                         CVStratum='stratum '+str(i//4) if i<7 else 'NA')
                if classify:
                    truth='class "A"' if i<5 else 'B'
                    x.update(Observed=truth,Predicted=truth if i<7 else 'NA',Correct='1' if i<7 else 'NA')
                else:x.update(Observed=str(i//2),Predicted=str(i//2+1) if i<7 else 'NA',TrainingMean='0' if i<7 else 'NA')
                y=x.copy();y['TableRow']=str(100+i)
                if i<7:
                    if classify:y.update(Predicted='B',Correct=str(int(truth=='B')))
                    else:y['Predicted']=str(i//2)
                a.append(x);b.append(y)
        random.Random(42).shuffle(b)
        comments='# Target\t"condition name"\n' if classify else ''
        name='class' if classify else 'continuous';write(root/(name+'A'),a,comments);write(root/(name+'B'),b,comments)
        scores=compare(name+'A',name+'B',name)
        reverse=compare(name+'B',name+'A',name+'_reverse')
        for x,y in zip(scores,reverse):
            near(y['DeltaBminusA'],-float(x['DeltaBminusA']) if x['DeltaBminusA']!='NA' else math.nan)
        compare(name+'A',name+'A',name+'_self')
        # No overwrite without opt-in, including preservation of existing output.
        saved=(root/(name+'.comparison.1D')).read_bytes()
        compare(name+'A',name+'B',name,failure='already exists')
        assert saved==(root/(name+'.comparison.1D')).read_bytes()
        compare(name+'A',name+'B',name,overwrite=True)
        # Each malformed input must fail before either output is created.
        edits=[('Sample','missing','domains differ'),('Run','different','run mismatch'),
               ('Item','different','item mismatch'),('InputFold','different','input_fold mismatch'),
               ('CVFold','different','fold mismatch'),('CVStratum','different','stratum mismatch'),
               ('Subj' if classify else 'Subject','different','subject mismatch')]
        edits += [('I','1','I mismatch')] if classify else [('Observed','99','outcome mismatch'),
                  ('TrainingMean','99','baseline mismatch'),('Predicted','nan','finite numeric'),('Predicted','1e300','finite numeric')]
        for col,val,msg in edits:
            bad=[r.copy() for r in b];idx=next(i for i,r in enumerate(bad) if r['CVFold']!='NA');bad[idx][col]=val
            write(root/'bad',bad,comments);compare(name+'A','bad','badout',failure=msg)
            assert not list(root.glob('badout.*'))
        bad=[r.copy() for r in b];bad[-1]=bad[0].copy();write(root/'bad',bad,comments)
        run(['-compare_predictions',name+'A','bad','-prefix','badout'],failure='3dMVPA')
        write(root/'bad',b[:-1],comments);compare(name+'A','bad','badout',failure='row counts')
        if classify:
            write(root/'bad',b,'# Target\tdifferent\n');compare(name+'A','bad','badout',failure='Target headers differ')
        else:
            const=[dict(r,Observed='0',Predicted='0' if r['CVFold']!='NA' else 'NA') for r in a]
            write(root/'const',const);out=compare('const','const','constant')
            assert all(r['ScoreA']=='NA' for r in out if r['Metric'] in ['pearson','spearman','cross_validated_r2'])
    # A class literally named NA is scored using Correct/Tested, not mistaken
    # for a missing prediction. Training-only comparisons have no score rows.
    for side in ['A','B']:
        records=read(root/('class'+side))
        for row in records:
            for field in ['Observed','Predicted']:
                if row[field]=='B':row[field]='NA'
        write(root/('na'+side),records,'# Target\tCondition\n')
    compare('naA','naB','literal_na_class')
    train_only=[r for r in read(root/'continuousA') if r['Predicted']=='NA']
    write(root/'train_only',train_only)
    compare('train_only','train_only','badout',failure='no held-out predictions')
    (root/'exact.comparison.1D').write_bytes((root/'classA').read_bytes())
    run(['-compare_predictions',root/'exact.comparison.1D','classB','-prefix','./exact'],
        failure='cannot replace',overwrite=True)
    compare('classA','continuousA','badout',failure='classification and continuous')
    run(['-compare_predictions','classA','classB','-nperm','10'],failure='standalone')
    run(['-compare_predictions','classA'],failure='needs model-A')
    # Alias protections must apply even with overwrite explicitly enabled.
    alias=root/'alias.comparison.1D';alias.symlink_to(root/'classA')
    compare('classA','classB','alias',failure='cannot replace',overwrite=True)
    alias.unlink();os.link(root/'classA',alias)
    compare('classA','classB','alias',failure='cannot replace',overwrite=True)
    alias.unlink();(root/'alias.comparison.1D').write_text('untouched')
    (root/'alias.paired_predictions.1D').symlink_to(root/'alias.comparison.1D')
    compare('classA','classB','alias',failure='alias each other',overwrite=True)
    assert (root/'alias.comparison.1D').read_text()=='untouched'

    # Real outputs: different classifiers, nested preprocessing, encoding
    # designs, CV/fixed/two-table partitions and volume searchlights.
    nifti(root/'mask.nii',[[1,1,2,2]])
    rows=[];volumes=[]
    for i in range(18):
        label=i%3;x=(label-1)*2+(i//3)*.3;z=(i*7%11)-5
        volumes.append([x,z,.5*x+z,x*x-.1*z])
        rows.append(dict(Subj='sub'+str(i//9),Run='run'+str((i//3)%3),Sample='s'+str(i),
                         Class='ABC'[label],Y=str(2*x-z),Y2=str(x*x),X=str(x),Z=str(z),
                         Split='train' if i<12 else 'test',InputFile=str(root/'betas.nii')+f'[{i}]'))
    nifti(root/'betas.nii',volumes);write(root/'samples',rows)
    write(root/'train',rows[:12]);write(root/'test',rows[12:])
    write(root/'train_reordered',list(reversed(rows[:12])));write(root/'test_reordered',list(reversed(rows[12:])))
    def fit(name,mode='classify',extra=(),source=None,workers=2):
        args=['-mode',mode,'-mask','mask.nii','-sample_column','Sample','-prefix',name,'-save_predictions',name+'.pred']
        args+=list(source or ['-dataTableFile','samples','-cv_column','Run'])
        if mode=='classify':args+=['-target_column','Class']
        elif mode=='decode':args+=['-target_columns','Y,Y2']
        else:args+=['-design_columns','X','-score','pattern']
        run(args+list(extra),workers=workers)
        return name+'.pred'
    a=fit('centroid')
    for method in ['ridge','lda_diag','svm']:
        b=fit(method,extra=['-method',method]);compare(a,b,'compare_'+method)
    a=fit('decode_a','decode',extra=['-lambda','1'])
    b=fit('decode_b','decode',extra=['-lambda_grid','.1,10','-nuisance_columns','Z','-pca_components','1'])
    compare(a,b,'compare_decode')
    a=fit('encode_a','encode')
    # Replace the default design field through an explicitly constructed command.
    run(['-mode','encode','-mask','mask.nii','-dataTableFile','samples','-cv_column','Run','-sample_column','Sample',
         '-design_columns','X,Z','-score','pattern','-prefix','encode_b','-save_predictions','encode_b.pred'])
    compare(a,'encode_b.pred','compare_encode')
    for mode in ['classify','decode','encode']:
        for split,source in [('fixed',['-dataTableFile','samples','-partition_column','Split']),
                             ('two',['-dataTableFile','train','-testTableFile','test'])]:
            a=fit(mode+split+'a',mode,source=source,extra=['-method','ridge','-lambda','1'])
            b=fit(mode+split+'b',mode,source=source,extra=['-method','ridge','-lambda','10'])
            compare(a,b,'compare_'+mode+split)
    # Source filenames and row numbers can change with explicit sample identity.
    for mode in ['classify','decode','encode']:
        a=mode+'twoa.pred'
        b=fit(mode+'_reordered',mode,source=['-dataTableFile','train_reordered','-testTableFile','test_reordered'],
              extra=['-method','ridge','-lambda','1'])
        compare(a,b,'compare_reordered_'+mode)
        compare(mode+'fixeda.pred',a,'compare_single_two_'+mode)
    for mode in ['classify','decode','encode']:
        a=fit('sl_'+mode+'a',mode,extra=['-method','ridge','-searchlight','2'],workers=1)
        b=fit('sl_'+mode+'b',mode,extra=['-method','ridge','-searchlight','2'],workers=2)
        scores=compare(a,b,'compare_sl_'+mode)
        assert all(r['DeltaBminusA'] in ['0','NA'] for r in scores)
    # Comparing saved predictions does not invoke a model/permutation runner:
    # observed audits restored after permutations compare exactly to plain fits.
    a=fit('observed',extra=['-method','ridge'])
    b=fit('permuted',extra=['-method','ridge','-nperm','3','-permute_within','Run'])
    scores=compare(a,b,'compare_permutation')
    assert all(r['DeltaBminusA']=='0' for r in scores)

print('PASS paired comparisons: independent metrics, complete alignment, grouped scopes, training exclusion, undefined scores, reordered rows, real classifiers/decode/encode, partitions, spatial workers and output protection')
