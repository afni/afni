#!/usr/bin/env python3
"""M6 independent pair-map oracles, model bundle validation and shell execution."""
import argparse
import csv
import itertools
import json
import math
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image,correlation,f32
from run_insync_block_summary import read_table
from run_insync_window_baseline import summary


def fisher(r):
    r=f32(r)
    return f32(math.atanh(max(-f32(.999999),min(f32(.999999),r)))) if abs(r)>=1 else f32(math.atanh(r))


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--program',required=True); parser.add_argument('--work')
    args=parser.parse_args(); program=str(Path(args.program).resolve()); count=0
    tmp=None
    if args.work: root=write_fixture(Path(args.work)/'fixture')
    else:
        tmp=tempfile.TemporaryDirectory(prefix='insync-m6-'); root=write_fixture(Path(tmp.name)/'fixture')
    data=[]
    for ci in range(2):
        subjects=[]
        for si in range(6):
            _,flat=image(root/(f's{si+1:02d}'+('_C2' if ci else '')+'.nii'))
            subjects.append([[flat[t*24+v] for t in range(48)] for v in range(24)])
        data.append(subjects)
    def run(opts=(),table='subjects.txt',error=None,partial=False,export=True,threads=2,pairprefix=None):
        nonlocal count
        count+=1; prefix=f'm{count:03d}'; pp=pairprefix or prefix+'_pairs'
        cmd=[program,'-quiet','-prefix',prefix]+(['-save_pairwise',pp] if export else [])+list(opts)+['-dataTableFile',table]
        result=subprocess.run(cmd,cwd=root,env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_DECONFLICT='NO',AFNI_NOMMAP='YES'),capture_output=True,text=True)
        bundle=root/(pp+'.isc')
        if error:
            assert result.returncode and error.lower() in result.stderr.lower(),(cmd,error,result.stderr)
            assert not (root/(prefix+'.results.tsv')).exists() or not (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
            assert not (bundle/'jobs.tsv').exists() or not (bundle/'jobs.tsv').read_text().endswith('# Status: complete\n')
            if not partial: assert not list(root.glob(prefix+'.sel*.nii*'))
            return prefix,bundle,result
        assert result.returncode==0,(cmd,result.stdout,result.stderr)
        assert (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
        assert (bundle/'jobs.tsv').read_text().endswith('# Status: complete\n')
        return prefix,bundle,result
    def table(bundle,job):
        return list(csv.DictReader((bundle/job['Table']).open(),delimiter=' '))
    def oracle(bundle,segments,metric='pearson',bs='median',series=data,aggregate=False,drop=False,min_blocks=1,keep=None):
        jobs=read_table(bundle/'jobs.tsv')
        for job in jobs:
            ci=0 if job['Condition1'] in ('All','C1') else 1
            cj=0 if job['Condition2'] in ('All','C1') else 1
            idx=int(job['SelectionID'][3:])-1
            blocks=segments if aggregate and idx==0 else [segments[idx-1] if aggregate else segments[idx]]
            _,maps=image(bundle/job['Pairs']); _,mask=image(bundle/job['Mask']); _,support=image(bundle/job['Support'])
            rows=table(bundle,job); assert len(rows)==15 and len(maps)==24*15
            assert all(r['InputFile']==job['Pairs']+f'[{i}]' for i,r in enumerate(rows))
            for v in range(24):
                admitted=[]; bv=[]
                for bi,ts in enumerate(blocks):
                    ts=[t for t in ts if keep is None or keep[t]]
                    vals=[correlation([series[ci][a][v][t] for t in ts],[series[cj][b][v][t] for t in ts],metric) for a,b in itertools.combinations(range(6),2)] if len(ts)>=3 else [float('nan')]*15
                    good=all(math.isfinite(x) for x in vals)
                    if good: admitted.append(bi)
                    bv.append(vals)
                    if aggregate and idx==0:
                        assert support[(3*bi)*24+v]==len(ts)
                        assert support[(3*bi+1)*24+v]==int(good)
                valid=bool(admitted) and len(admitted)>=(min_blocks if aggregate and idx==0 else 1) and (drop or len(admitted)==len(blocks))
                assert mask[v]==int(valid),(job,v,mask[v],valid)
                for pi in range(15):
                    expected=fisher(bv[admitted[0]][pi] if len(admitted)==1 else summary([bv[b][pi] for b in admitted],bs)) if valid else 0
                    assert abs(maps[pi*24+v]-expected)<5e-6,(job,v,pi,maps[pi*24+v],expected)
                if aggregate and idx==0:
                    for bi in range(len(blocks)):
                        expected=1/len(admitted) if valid and bi in admitted else 0
                        assert abs(support[(3*bi+2)*24+v]-expected)<1e-7
    windows=[list(range(12)),list(range(24,36))]
    for metric,method in itertools.product(('pearson','spearman'),('pairwise','loo')):
        p,b,_=run(['-window','12','-step','24','-correlation',metric,'-isc_method',method],table='conditions.txt')
        oracle(b,windows,metric)
        assert len(read_table(b/'jobs.tsv'))==8
    blocks=[list(range(6)),list(range(8,20)),list(range(24,48))]
    lines=['Subj Group Age Selection Segment Run StartTR StopTR InputFile']
    for si in range(6):
        for i,ts in enumerate(blocks): lines.append(f's{si+1:02d} {"A" if si<3 else "B"} {20+si} Combined b{i} 1 {ts[0]} {ts[-1]} s{si+1:02d}.nii')
    (root/'blocks.txt').write_text('\n'.join(lines)+'\n')
    for metric,bs in itertools.product(('pearson','spearman'),('median','fisher_mean')):
        _,b,_=run(['-correlation',metric,'-block_summary',bs],table='blocks.txt')
        oracle(b,blocks,metric,bs,aggregate=True)
    _,b,_=run(['-combine_blocks','pool','-segment_center','mean'],table='blocks.txt')
    # Pooling's scalar oracle, with per-subject block centering before joining.
    _,flat=image(b/read_table(b/'jobs.tsv')[0]['Pairs'])
    for pi,(a,c) in enumerate(itertools.combinations(range(6),2)):
        for v in range(24):
            vectors=[]
            for si in (a,c):
                vec=[]
                for ts in blocks:
                    center=f32(statistics.mean(data[0][si][v][t] for t in ts))
                    vec.extend(f32(data[0][si][v][t]-center) for t in ts)
                vectors.append(vec)
            assert abs(flat[pi*24+v]-fisher(correlation(*vectors,'pearson')))<5e-6
    # The model path always exports pairs, even when LOO references cancel.
    cancel=[[list(v) for v in si] for si in data[0]]
    for v in range(24): cancel[2][v]=[-x for x in cancel[1][v]]
    for si in range(6): nifti(root/f'cancel{si}.nii',[cancel[si][v][t] for t in range(48) for v in range(24)],48)
    (root/'cancel.txt').write_text('Subj Group InputFile\n'+''.join(f's{si} {"A" if si<3 else "B"} cancel{si}.nii\n' for si in range(6)))
    _,b,_=run(['-window','48','-isc_method','loo'],table='cancel.txt')
    oracle(b,[list(range(48))],series=[cancel])
    # Partially invalid graph: one subject constant during one block, one voxel.
    changed=[[list(v) for v in si] for si in data[0]]
    for t in blocks[0]: changed[0][0][t]=3
    nifti(root/'changed.nii',[changed[0][v][t] for t in range(48) for v in range(24)],48)
    (root/'changed.txt').write_text((root/'blocks.txt').read_text().replace('s01.nii','changed.nii'))
    for drop in (False,True):
        _,b,_=run(['-block_missing','drop_common'] if drop else [],table='changed.txt')
        oracle(b,blocks,series=[changed],aggregate=True,drop=drop)
    _,b,_=run(['-block_missing','drop_common','-min_blocks','3'],table='changed.txt')
    oracle(b,blocks,series=[changed],aggregate=True,drop=True,min_blocks=3)
    # ROI-backed export means are calculated before ranking/correlation.
    _,labels=image(root/'atlas.nii'); parcels={int(r):[i for i,x in enumerate(labels) if x==r] for r in set(labels) if r>0}
    means=[]
    for si in range(6): means.append([[f32(statistics.mean(data[0][si][v][t] for v in parcels[int(labels[w])])) for t in range(48)] for w in range(24)])
    _,b,_=run(['-atlas','atlas.nii','-roi_only','-window','12','-step','24','-correlation','spearman'])
    oracle(b,windows,'spearman',series=[means])
    # Compact-mask indexing and atlas template space reach all model outputs.
    _,b,_=run(['-window','12','-step','24','-mask','mask.nii'])
    _,analysis_mask=image(root/'mask.nii')
    for job in read_table(b/'jobs.tsv'):
        _,modelmask=image(b/job['Mask']); _,pairvalues=image(b/job['Pairs'])
        assert modelmask==analysis_mask
        assert all(pairvalues[p*24+v]==0 for p in range(15) for v in range(24) if not analysis_mask[v])
    copy=shutil.which('3dcopy'); refit=shutil.which('3drefit'); info=shutil.which('3dinfo')
    assert copy and refit and info
    subprocess.run([copy,'atlas.nii','template'],cwd=root,check=True,capture_output=True)
    subprocess.run([refit,'-view','tlrc','-space','MNI','template+orig'],cwd=root,check=True,capture_output=True)
    _,b,_=run(['-window','48','-atlas','template+tlrc','-roi_only'])
    job=read_table(b/'jobs.tsv')[0]
    for key in ('Pairs','Mask','Support'):
        out=subprocess.run([info,'-space','-av_space',str(b/job[key])],text=True,capture_output=True,check=True)
        assert 'MNI' in out.stdout and '+tlrc' in out.stdout,out.stdout
    out=subprocess.run([info,'-label',str(b/job['Pairs'])],text=True,capture_output=True,check=True)
    assert out.stdout.strip().split('|')==[f'Z_pair{p+1:06d}' for p in range(15)]
    # Censor support is original-time; Xmat controls never become covariates.
    keep=[int(t%7!=0) for t in range(48)]; (root/'keep.1D').write_text('\n'.join(map(str,keep))+'\n')
    _,b,_=run(['-window','12','-step','24','-censor','keep.1D']); oracle(b,windows,keep=keep)
    _,b,_=run(['-xmat_blocks','each','-xmat_stims','Aud','-xmat_threshold','.5'],table='xmat_subjects.txt')
    assert all(not any(x in table(b,j)[0] for x in ('Xmat1','XmatFull1','Censor1','Selection1','Run1')) for j in read_table(b/'jobs.tsv'))
    # Repeated-condition block aggregation preserves ordered cross-condition dyads.
    lines=(root/'blocks.txt').read_text().splitlines(); long=[lines[0].replace('Age ','Age Condition ')]
    for line in lines[1:]:
        tokens=line.split()
        for ci in range(2):
            vals=tokens[:3]+[f'C{ci+1}']+tokens[3:]
            if ci: vals[-1]=vals[-1].replace('.nii','_C2.nii')
            long.append(' '.join(vals))
    (root/'longblocks.txt').write_text('\n'.join(long)+'\n')
    _,b,_=run(['-block_summary','fisher_mean'],table='longblocks.txt')
    oracle(b,blocks,bs='fisher_mean',aggregate=True)
    # Nonfinite .1D input exercises common finite support and NIfTI export.
    rows=['Subj Group InputFile']; good=[]
    for si in range(6):
        vectors=[]
        for v in range(2):
            vectors.append(['nan' if si==0 and v==0 and t==6 else str(data[0][si][v][t]) for t in range(12)])
        filename=f'bad{si}.1D'
        (root/filename).write_text('# <AFNI_3D_dataset\n# ni_type = "12*float"\n# ni_dimen = "2,1,1"\n# ni_delta = "1,1,1"\n# ni_origin = "0,0,0"\n# ni_axes = "R-L,A-P,I-S"\n# >\n'+'\n'.join(' '.join(v) for v in vectors)+'\n# </AFNI_3D_dataset>\n')
        rows.append(f's{si} '+('A' if si<3 else 'B')+' '+filename)
    (root/'bad.txt').write_text('\n'.join(rows)+'\n')
    run(['-window','12'],table='bad.txt',error='nonfinite selected input')
    _,b,_=run(['-window','12','-missing','common'],table='bad.txt')
    job=read_table(b/'jobs.tsv')[0]
    assert image(b/job['Support'])[1]==[11.,12.]
    _,maps=image(b/job['Pairs'])
    for pi,(a,c) in enumerate(itertools.combinations(range(6),2)):
        for v in range(2):
            times=[t for t in range(12) if v or t!=6]
            expected=fisher(correlation([data[0][a][v][t] for t in times],[data[0][c][v][t] for t in times],'pearson'))
            assert abs(maps[pi*2+v]-expected)<5e-6
    # Shell paths with spaces/apostrophes and structured model arguments.
    (root/"a folder's").mkdir()
    options=['-window','48','-isc_model','I(Age1+Age2)+(1|Subj1)+(1|Subj2)', '-isc_qVars','Age1,Age2','-isc_qVarCenters','22,22','-isc_gltCode','intercept','1 0','-isc_gltCode','age_sum','0 1','-isc_jobs','2']
    _,custom,_=run(options,pairprefix="a folder's/models")
    mock=root/'mockbin';mock.mkdir(); log=root/'argv.jsonl'
    (mock/'3dISC').write_text('#!'+sys.executable+'\nimport os,json,sys\nwith open(os.environ["MODEL_ARGV_LOG"],"a") as f: f.write(json.dumps(dict(cwd=os.getcwd(),argv=sys.argv[1:]))+"\\n")\nsys.exit(int(os.environ.get("MODEL_FAIL","0")))\n'); (mock/'3dISC').chmod(0o755)
    env=dict(os.environ,PATH=str(mock)+os.pathsep+os.environ['PATH'],MODEL_ARGV_LOG=str(log))
    subprocess.run(['sh',str(custom/'run_all.sh')],cwd=root.parent,env=env,check=True)
    assert os.access(custom/'run_all.sh',os.X_OK)
    invocation=json.loads(log.read_text().splitlines()[0]); argv=invocation['argv']
    assert invocation['cwd']==str(custom.resolve()) and argv[argv.index('-model')+1]==options[options.index('-isc_model')+1]
    assert argv[argv.index('-qVarCenters')+1]=='22,22' and argv.count('-gltCode')==2
    # R's actual table reader sees exactly the rectangular intended tokens.
    rscript=shutil.which('Rscript')
    if rscript:
        tablefile=custom/read_table(custom/'jobs.tsv')[0]['Table']
        read=subprocess.run([rscript,'-e','x<-scan(commandArgs(TRUE)[1],what="character",quiet=TRUE,na.strings=""); stopifnot(length(x)==16*11); cat("R table scan passed\\n")',str(tablefile)],capture_output=True,text=True)
        assert read.returncode==0,read.stderr
    _,b,_=run(['-window','12','-step','24'])
    log.write_text('')
    stopped=subprocess.run(['sh',str(b/'run_all.sh')],cwd=root.parent,env=dict(env,MODEL_FAIL='7'),capture_output=True)
    assert stopped.returncode==7 and len(log.read_text().splitlines())==1
    # Unsupported table syntax, duplicate columns/options and bad model metadata.
    for opts,error in [(['-isc_qVars','Age'],'not an exported'),(['-isc_qVars','Age1,Age1'],'duplicate qVars'),(['-isc_qVarCenters','0'],'requires qVars'),(['-isc_qVars','Age1,Age2','-isc_qVarCenters','0'],'count'),(['-isc_gltCode','same','1','-isc_gltCode','same','1'],'duplicate GLT'),(['-isc_gltCode','bad','nan'],'finite numeric'),(['-isc_jobs','0'],'integer'),(['-isc_model','1','-isc_model','1'],'given twice')]:
        run(['-window','12']+opts,error=error)
    run(['-window','12','-isc_model','1'],export=False,error='require a selection request and -save_pairwise')
    original=(root/'subjects.txt').read_text()
    (root/'collision.txt').write_text(original.replace('Age InputFile','Age Age1 InputFile').replace('20 s01','20 20 s01').replace('21 s02','21 21 s02').replace('22 s03','22 22 s03').replace('23 s04','23 23 s04').replace('24 s05','24 24 s05').replace('25 s06','25 25 s06'))
    run(['-window','12'],table='collision.txt',error='collision')
    (root/'nonnumeric.txt').write_text(original.replace('20 s01','young s01'))
    run(['-window','12','-isc_qVars','Age1'],table='nonnumeric.txt',error='nonnumeric')
    (root/'badtoken.txt').write_text(original.replace('s01 A','s:01 A'))
    run(['-window','12'],table='badtoken.txt',error='unrepresentable')
    mismatched=['Subj Group Condition Selection Segment Run StartTR StopTR InputFile']
    for si in range(6):
        for ci in range(2): mismatched.append(f's{si} {"A" if si<3 else "B"} C{ci+1} Same b 1 {ci} {ci+11} s{si+1:02d}.nii')
    (root/'unmatched.txt').write_text('\n'.join(mismatched)+'\n')
    run(table='unmatched.txt',error='unmatched original coordinates')
    run(['-window','12','-memory_limit','.000001'],error='memory estimate exceeds')
    # All-invalid graph fails clearly with no completion marker and no runnable partial job.
    nifti(root/'constant.nii',[2.]*24*48,48)
    (root/'invalid.txt').write_text(original.replace('s01.nii','constant.nii'))
    _,incomplete,_=run(['-window','12'],table='invalid.txt',error='zero valid model support',partial=True)
    result=subprocess.run(['sh',str(incomplete/'run_all.sh')],cwd=root.parent,capture_output=True,text=True)
    assert result.returncode and 'Incomplete' in result.stderr
    (root/'reserved.isc').mkdir()
    run(['-window','12','-atlas','atlas.nii','-save_matrix','reserved.isc/jobs.tsv'],pairprefix='reserved',error='collides')
    # Existing output collision is caught before any descriptive output appears.
    (root/'occupied.isc').mkdir(); (root/'occupied.isc/sel000001.c001_c001.mask.nii.gz').write_text('preserve')
    run(['-window','12'],pairprefix='occupied',error='already exists')
    assert (root/'occupied.isc/sel000001.c001_c001.mask.nii.gz').read_text()=='preserve'
    # Optional actual fits are intentionally separate from generation, invoked
    # only by this explicit test harness. Preserve their bundles with --work.
    print(f'PASS: {count} M6 export cases; scalar pairs, support, ROI, metadata and shell tests')
    print('Custom model bundle:',custom)
    if tmp: tmp.cleanup()

if __name__=='__main__': main()
