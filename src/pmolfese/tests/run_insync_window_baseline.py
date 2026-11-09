#!/usr/bin/env python3
"""Capture/verify static ISC before window refactoring, using M0 fixtures.

--capture is an explicit baseline-writing operation; normal runs never update
expected output. Numeric snapshots exclude histories/timestamps. Independently
checks all eight core estimator/metric/summary combinations against scalar math.
Requires 3dInSync and 3dinfo on PATH (or --info); no third-party Python modules.
"""
import argparse
import hashlib
import itertools
import json
import math
import os
import shutil
import statistics
import struct
import subprocess
import tempfile
from pathlib import Path
from insync_window_fixtures import write_fixture, NS

DEFAULT = Path(__file__).with_name('insync_static_baseline.json')


def read_nifti(path):
    raw=path.read_bytes()
    endian='<' if struct.unpack_from('<i',raw)[0]==348 else '>'
    assert struct.unpack_from(endian+'i',raw)[0]==348, path
    dims=struct.unpack_from(endian+'8h',raw,40)
    shape=list(dims[1:dims[0]+1]); count=math.prod(shape)
    dtype=struct.unpack_from(endian+'h',raw,70)[0]
    code={2:'B',4:'h',8:'i',16:'f',64:'d'}[dtype]
    offset=int(struct.unpack_from(endian+'f',raw,108)[0])
    slope,inter=struct.unpack_from(endian+'2f',raw,112)
    slope=slope or 1.0
    data=[v*slope+inter for v in struct.unpack_from(endian+str(count)+code,raw,offset)]
    assert all(math.isfinite(v) for v in data), path
    return shape,data


def ranks(x):
    result=[0.0]*len(x)
    order=sorted(range(len(x)),key=x.__getitem__)
    start=0
    while start<len(x):
        end=start+1
        while end<len(x) and x[order[end]]==x[order[start]]: end+=1
        for i in order[start:end]: result[i]=(start+1+end)/2
        start=end
    return result


def corr(x,y,metric):
    if metric=='spearman': x,y=ranks(x),ranks(y)
    mx,my=statistics.mean(x),statistics.mean(y)
    num=sum((a-mx)*(b-my) for a,b in zip(x,y))
    den=math.sqrt(sum((a-mx)**2 for a in x)*sum((b-my)**2 for b in y))
    return max(-1.0,min(1.0,num/den))


def summary(values,kind):
    if kind=='median': return statistics.median(values)
    return math.tanh(statistics.mean(math.atanh(max(-.999999,min(.999999,v))) for v in values))


def oracle(root,method,metric,kind):
    allseries=[]
    for subject in range(NS):
        shape,flat=read_nifti(root/f's{subject+1:02d}.nii')
        nv=math.prod(shape[:3]); nt=shape[3]
        allseries.append([[flat[t*nv+v] for t in range(nt)] for v in range(nv)])
    expected=[]
    for group in (range(3),range(3,6)):
        vals=[]
        for v in range(nv):
            if method=='pairwise':
                rs=[corr(allseries[a][v],allseries[b][v],metric) for a,b in itertools.combinations(group,2)]
            else:
                rs=[]
                for a in group:
                    others=[b for b in group if b!=a]
                    # The production reference is a float buffer, not double.
                    ref=[struct.unpack('<f',struct.pack('<f',statistics.mean(allseries[b][v][t] for b in others)))[0] for t in range(nt)]
                    rs.append(corr(allseries[a][v],ref,metric))
            vals.append(summary(rs,kind))
        expected.extend([vals,[3.0]*nv])
    expected.append([a-b for a,b in zip(expected[0],expected[2])])
    return [v for brick in expected for v in brick]


def compare(got,expected,tol,context):
    if len(got)!=len(expected): raise AssertionError(f'{context}: length mismatch')
    error=max((abs(a-b) for a,b in zip(got,expected)),default=0)
    if error>tol: raise AssertionError(f'{context}: max absolute error {error} > {tol}')


def cases():
    for method,metric,kind in itertools.product(('pairwise','loo'),('pearson','spearman'),('median','fisher_mean')):
        yield f'{method}_{metric}_{kind}', ['-isc_method',method,'-correlation',metric,'-summary',kind], 'subjects.txt'
    yield 'bootstrap_group',['-nboot','100','-nperm','20','-exact'],'subjects.txt'
    yield 'timeshift',['-temporal_null','timeshift','-nnull','20','-min_shift','3'],'subjects.txt'
    yield 'phase',['-temporal_null','phase','-nnull','20'],'subjects.txt'
    yield 'conditions',['-condition_contrast','C1','C2','-condition_exact','-save_pairwise','pairs.nii'],'conditions.txt'
    yield 'roi',['-atlas','atlas.nii','-save_matrix','matrix.1D'],'subjects.txt'
    yield 'roi_only',['-atlas','atlas.nii','-roi_only'],'subjects.txt'
    yield 'compact',['-mask','mask.nii'],'subjects.txt'
    yield 'half',['-mask','mask.nii','-float16'],'subjects.txt'
    yield 'common_censor',['-censor','common.censor.1D'],'subjects.txt'


def run(args):
    program=Path(args.program).resolve(); info=shutil.which(args.info)
    if not info: raise RuntimeError('3dinfo not found')
    env=dict(os.environ,OMP_NUM_THREADS=str(args.threads),AFNI_NOMMAP='YES',AFNI_DECONFLICT='NO')
    parent=Path(args.work).resolve() if args.work else Path(tempfile.mkdtemp(prefix='insync-m0-baseline-'))
    parent.mkdir(parents=True,exist_ok=True)
    root=write_fixture(parent/'fixture')
    records={}
    commands=[]
    for name,extra,table in cases():
        output=name+'.nii'
        cmd=[str(program),'-quiet','-progress','off','-seed','20261104','-prefix',output,*extra,'-dataTableFile',table]
        commands.append(cmd)
        with (root/(name+'.log')).open('w') as log:
            subprocess.run(cmd,cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
        shape,values=read_nifti(root/output)
        labels=subprocess.check_output([info,'-label',output],cwd=root,env=env,text=True).strip()
        records[name]={'shape':shape,'labels':labels,'values':values}
        if name.startswith(('pairwise_','loo_')):
            method,metric,kind=name.split('_',2)
            compare(values,oracle(root,method,metric,kind),1e-6,name+' scalar oracle')
        # Snapshot descriptive tables too; contents have no transient paths.
        for suffix in ('.roi.1D',):
            path=root/(output+suffix)
            if path.exists(): records[name][suffix]=path.read_text()
        print('PASS:',name,flush=True)
    shape,values=read_nifti(root/'pairs.nii')
    labels=subprocess.check_output([info,'-label','pairs.nii'],cwd=root,env=env,text=True).strip()
    records['pair_maps']={'shape':shape,'labels':labels,'values':values}
    # Validate tables against the actual emitted dataset selectors.
    for table in sorted(root.glob('pairs*.txt')):
        rows=table.read_text().splitlines()
        for row in rows[1:]:
            selector=row.split()[-1]
            subprocess.run([info,'-nv',selector],cwd=root,env=env,check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
        records[table.name]={'text':table.read_text().replace(str(root),'<FIXTURE>')}
    path=root/'matrix.1D'
    records['roi_matrix']={'text':path.read_text()}
    payload={'schema':1,'cases':records}
    if args.capture:
        args.baseline.write_text(json.dumps(payload,sort_keys=True,separators=(',',':'))+'\n')
        print('CAPTURED:',args.baseline)
    else:
        expected=json.loads(args.baseline.read_text())
        assert records.keys()==expected['cases'].keys(), 'Case set mismatch'
        for name,got in records.items():
            wanted=expected['cases'][name]
            assert got.keys()==wanted.keys(), name
            for key,value in got.items():
                if key=='values': compare(value,wanted[key],1e-6,name)
                else: assert value==wanted[key], (name,key)
        print('PASS: static snapshots match')
    report={'program':str(program),'sha256':hashlib.sha256(program.read_bytes()).hexdigest(),
            'threads':args.threads,'tolerance':1e-6,'commands':commands,'cases':len(records),
            'fixture':str(root),'capture':args.capture}
    (parent/'run.json').write_text(json.dumps(report,indent=2)+'\n')
    print('Evidence:',parent/'run.json')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--program',required=True)
    p.add_argument('--info',default='3dinfo')
    p.add_argument('--baseline',type=Path,default=DEFAULT)
    p.add_argument('--capture',action='store_true')
    p.add_argument('--threads',type=int,default=1)
    p.add_argument('--work',help='Evidence directory; fixture child must not already exist')
    run(p.parse_args())
