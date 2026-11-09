#!/usr/bin/env python3
"""M3 descriptive selection estimates against independent two-pass/rank math."""
import argparse
import csv
import gzip
import io
import itertools
import math
import os
from pathlib import Path
import statistics
import struct
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_window_baseline import ranks, summary


def image(path):
    if str(path).endswith('.1D'):
        rows=[[float(x) for x in line.split()] for line in Path(path).read_text().splitlines() if line.strip() and not line.startswith('#')]
        return (len(rows),1,1,len(rows[0])),[row[b] for b in range(len(rows[0])) for row in rows]
    raw=gzip.open(path,'rb').read() if str(path).endswith('.gz') else Path(path).read_bytes()
    dims=struct.unpack_from('<8h',raw,40); shape=dims[1:dims[0]+1]
    offset=int(struct.unpack_from('<f',raw,108)[0]); count=math.prod(shape)
    code={2:'B',4:'h',8:'i',16:'f',64:'d'}[struct.unpack_from('<h',raw,70)[0]]
    slope,inter=struct.unpack_from('<ff',raw,112); slope=slope or 1
    return shape,[x*slope+inter for x in struct.unpack_from('<'+str(count)+code,raw,offset)]


def f32(value):
    return struct.unpack('<f',struct.pack('<f',value))[0]


def correlation(x,y,metric):
    if metric=='spearman': x,y=ranks(x),ranks(y)
    mx,my=statistics.mean(x),statistics.mean(y)
    xx=sum((a-mx)**2 for a in x); yy=sum((a-my)**2 for a in y)
    if not xx or not yy: return float('nan')
    return f32(max(-1,min(1,sum((a-mx)*(b-my) for a,b in zip(x,y))/math.sqrt(xx*yy))))


def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--program',required=True)
    args=p.parse_args(); program=str(Path(args.program).resolve()); count=0
    with tempfile.TemporaryDirectory(prefix='insync-m3-') as tmp:
        root=write_fixture(Path(tmp)/'fixture')
        data=[]
        for i in range(1,7):
            _,flat=image(root/f's{i:02d}.nii')
            data.append([[flat[t*24+v] for t in range(48)] for v in range(24)])
        def run(options=(),table='subjects.txt',error=None,threads=1):
            nonlocal count
            count+=1; prefix=f'run{count:03d}'
            env=dict(os.environ,AFNI_DECONFLICT='NO',AFNI_NOMMAP='YES',OMP_NUM_THREADS=str(threads))
            cmd=[program,'-quiet','-prefix',prefix]+list(options)+['-dataTableFile',table]
            r=subprocess.run(cmd,cwd=root,env=env,capture_output=True,text=True)
            if error:
                assert r.returncode and error.lower() in r.stderr.lower(),(cmd,error,r.stderr)
                assert not list(root.glob(prefix+'*.nii*')),(cmd,list(root.glob(prefix+'*')))
                return r
            assert not r.returncode,(cmd,r.stdout,r.stderr)
            return prefix
        def manifest(prefix):
            lines=(root/(prefix+'.selections.tsv')).read_text().splitlines()
            return list(csv.DictReader(io.StringIO('\n'.join(l for l in lines if not l.startswith('#'))),delimiter='\t'))
        def results(prefix):
            text=(root/(prefix+'.results.tsv')).read_text()
            assert text.endswith('# Status: complete\n'),text
            rows=list(csv.DictReader(io.StringIO('\n'.join(l for l in text.splitlines() if not l.startswith('#'))),delimiter='\t'))
            return {r['SelectionID']:image(root/r['Bucket'])[1] for r in rows}
        def close(a,b,tol=1e-6):
            assert len(a)==len(b)
            worst=max(abs(x-y) for x,y in zip(a,b))
            assert worst<=tol,worst
        def oracle(selected,method,metric,kind,source=data,center=False):
            # selected is a list of disjoint lists of retained original TRs.
            nt=sum(map(len,selected)); maps=[[0.]*24 for _ in range(11)]
            for v in range(24):
                series=[]
                for si in range(6):
                    row=[]
                    for segment in selected:
                        values=[source[si][v][t] for t in segment]
                        if center and values:
                            mean=f32(statistics.mean(values)); values=[f32(x-mean) for x in values]
                        row+=values
                    series.append(row)
                maps[5][v]=nt
                if nt<3: continue
                for gi,group in enumerate((range(3),range(3,6))):
                    edges=[correlation(series[a],series[b],metric) for a,b in itertools.combinations(group,2)]
                    values=edges
                    if method=='loo':
                        values=[]
                        for a in group:
                            others=[b for b in group if b!=a]
                            ref=[f32(statistics.mean(series[b][t] for b in others)) for t in range(nt)]
                            values.append(correlation(series[a],ref,metric))
                    valid=[x for x in values if math.isfinite(x)]
                    if valid:
                        maps[gi*2][v]=f32(summary(valid,kind)); maps[6+gi][v]=1
                    maps[gi*2+1][v]=len(valid); maps[8+gi][v]=int(all(math.isfinite(x) for x in edges))
                if maps[6][v] and maps[7][v]: maps[4][v]=f32(maps[0][v]-maps[2][v]); maps[10][v]=1
            return [x for m in maps for x in m]
        def table(name,lines): (root/name).write_text('\n'.join(lines)+'\n'); return name
        def manual(name,segments,files=None):
            lines=['Subj Group Selection Segment Run StartTR StopTR InputFile']
            for i in range(6):
                for label,segment,start,stop in segments:
                    lines.append(f's{i+1:02d} {"A" if i<3 else "B"} {label} {segment} 1 {start} {stop} '+(files[i] if files else f's{i+1:02d}.nii'))
            return table(name,lines)
        # Every window, all eight combinations; compare a full-run selection
        # directly with the untouched static driver, including counts.
        for method,metric,kind in itertools.product(('pairwise','loo'),('pearson','spearman'),('median','fisher_mean')):
            opts=['-isc_method',method,'-correlation',metric,'-summary',kind]
            prefix=run(opts+['-window','8','-step','4','-run_starts','0,24'])
            rows=manifest(prefix); output=results(prefix)
            for sid,values in output.items():
                r=next(r for r in rows if r['SelectionID']==sid and r['Subject']=='s01')
                selected=[list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1))]
                close(values,oracle(selected,method,metric,kind))
            full=run(opts+['-window','48']); static=run(opts)
            values=next(iter(results(full).values()))
            # Ask the static command for NIfTI explicitly.
            subprocess.run([program,'-quiet','-prefix',static+'.nii']+opts+['-dataTableFile','subjects.txt'],cwd=root,env=dict(os.environ,OMP_NUM_THREADS='1',AFNI_NOMMAP='YES'),check=True,capture_output=True)
            close(values[:5*24],image(root/(static+'.nii'))[1])
        # Common censoring, boundary stability and subject masks.
        opts=['-window','8','-step','4','-run_starts','0,24']
        censored=run(opts,table='censor_subjects.txt')
        rows=manifest(censored); out=results(censored)
        common=[int(v) for v in (root/'common.censor.1D').read_text().split()]
        for sid,values in out.items():
            r=next(r for r in rows if r['SelectionID']==sid and r['Subject']=='s01')
            selected=[t for t in range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1) if common[t]]
            close(values,oracle([selected],'pairwise','pearson','median'))
        direct=run(opts+['-censor','common.censor.1D'])
        for a,b in zip(out.values(),results(direct).values()): close(a,b)
        multi=run(opts,table='censor_subjects.txt',threads=2)
        for a,b in zip(out.values(),results(multi).values()): close(a,b,0)
        # Both retained-count/fraction gates; an unusable window writes an
        # explicit invalid result with its retained count, never shifts edges.
        gated=run(opts+['-min_valid','5','-min_valid_frac','.75'],table='censor_subjects.txt')
        for sid,values in results(gated).items():
            nt=int(values[5*24]); baseline=out[sid]
            if nt<6:
                assert not any(values[:5*24]+values[6*24:])
            else: close(values,baseline)
        zeros=table('zero.1D',['0']*48)
        zero=run(opts+['-censor',zeros]); assert all(not any(v) for v in results(zero).values())
        # Xmat blocks and explicit pooling preserve child results.
        x=run(['-xmat_blocks','each','-xmat_threshold','.5'],table='xmat_subjects.txt')
        for sid,values in results(x).items():
            r=next(r for r in manifest(x) if r['SelectionID']==sid and r['Subject']=='s01')
            times=[t for t in range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1) if common[t]]
            close(values,oracle([times],'pairwise','pearson','median'))
        segments=[('pool','a',2,9),('pool','b',30,37)]
        manual('pool.txt',segments)
        for center in (False,True):
            opts2=['-combine_blocks','pool']+(['-segment_center','mean'] if center else [])
            pooled=run(opts2,table='pool.txt'); outp=results(pooled); rows=manifest(pooled)
            assert len(outp)==3 and sum(r['ParentSelectionID']!='NA' for r in rows)==12
            for sid,values in outp.items():
                pieces=[list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1)) for r in rows if r['Subject']=='s01' and r['SelectionID']==sid]
                close(values,oracle(pieces,'pairwise','pearson','median',center=center))
        xb=run(['-xmat_blocks','by_stim','-xmat_across_runs','-xmat_threshold','.5','-combine_blocks','pool'],table='xmat_subjects.txt')
        assert len(results(xb))==10
        for sid,values in results(xb).items():
            pieces=[[t for t in range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1) if common[t]] for r in manifest(xb) if r['Subject']=='s01' and r['SelectionID']==sid]
            close(values,oracle(pieces,'pairwise','pearson','median'))
        # Constants do not become artificial zero edges. A partial pair graph
        # can still yield a descriptive result, with PairComplete=0.
        files=['constant_s01.nii']+[f's{i:02d}.nii' for i in range(2,7)]
        manual('constant.txt',[('one','a',0,47)],files)
        constant=run(table='constant.txt'); v=next(iter(results(constant).values()))
        assert v[24]==1 and v[6*24]==1 and v[8*24]==0
        constant=run(['-isc_method','loo','-correlation','spearman'],table='constant.txt'); v=next(iter(results(constant).values()))
        assert v[24]==2 and v[6*24]==1 and v[8*24]==0
        altered=[[row[:] for row in subject] for subject in data]
        for i in range(3):
            altered[i][0]=[7.]*48
            nifti(root/f'constant{i}.nii',[altered[i][v][t] for t in range(48) for v in range(24)],48)
        files=[f'constant{i}.nii' for i in range(3)]+[f's{i:02d}.nii' for i in range(4,7)]
        manual('allconstant.txt',[('one','a',0,47)],files)
        invalid=run(table='allconstant.txt'); v=next(iter(results(invalid).values()))
        assert v[0]==v[24]==v[6*24]==v[8*24]==v[10*24]==0
        # Valid median zero versus invalid zero; endpoint correlations and ties.
        sequences=([1,-1,1,-1],[1,1,-1,-1],[1,0,-1,0])
        files=[]
        for i in range(6):
            filename=f'orth{i}.nii'; files.append(filename)
            nifti(root/filename,[float(sequences[i%3][t%4]) for t in range(48) for v in range(24)],48)
        manual('orth.txt',[('one','a',0,3)],files)
        orth=run(table='orth.txt'); v=next(iter(results(orth).values()))
        assert v[0]==0 and v[24]==3 and v[6*24]==v[8*24]==1
        # Strict missingness ignores unselected samples, but checks the union
        # before emitting any outputs. .1D preserves nonfinite source values.
        files=[]
        for i in range(6):
            filename=f'bad{i}.1D'; files.append(filename)
            lines=[]
            for voxel in range(2):
                values=[str(data[i][voxel][t]) for t in range(48)]
                if i==0 and voxel==0: values[6]='nan'
                lines.append(' '.join(values))
            (root/filename).write_text('# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "2,1,1"\n# ni_delta = "1,1,1"\n# ni_origin = "0,0,0"\n# ni_axes = "R-L,A-P,I-S"\n# >\n'+'\n'.join(lines)+'\n# </AFNI_3D_dataset>\n')
        manual('outside.txt',[('one','a',0,3)],files)
        run(table='outside.txt')
        manual('inside.txt',[('one','a',4,9)],files)
        run(table='inside.txt',error='nonfinite selected input')
        finite=run(['-missing','common'],table='inside.txt'); v=next(iter(results(finite).values()))
        assert v[5*2]==5 and v[5*2+1]==6
        censor=[1]*48; censor[6]=0; table('nanmask.1D',list(map(str,censor)))
        run(['-censor','nanmask.1D'],table='inside.txt')
        # Mask and compressed storage preserve estimates and geometry.
        masked=run(['-window','48','-mask','mask.nii']); half=run(['-window','48','-mask','mask.nii','-float16'])
        a=next(iter(results(masked).values())); b=next(iter(results(half).values())); close(a,b,.001)
        for k in range(11): assert all(a[k*24+v]==0 for v in range(24) if v%4>=2)
        # A globally zero raw volume is removed before checking selected data.
        files=[]
        for i in range(6):
            filename=f'zero{i}.nii'; files.append(filename)
            nifti(root/filename,[0. if t==3 and i==0 else data[i][v][t] for t in range(48) for v in range(24)],48)
        manual('zc.txt',[('one','a',0,7)],files)
        zc=run(['-zcensor'],table='zc.txt'); close(next(iter(results(zc).values())),oracle([[0,1,2,4,5,6,7]],'pairwise','pearson','median'))
        # First input group determines difference sign even when subject IDs
        # sort differently internally. Conditions stay separate.
        original=(root/'subjects.txt').read_text().splitlines(); table('reverse.txt',[original[0]]+original[4:]+original[1:4])
        rev=run(['-window','48'],table='reverse.txt'); v=next(iter(results(rev).values()))
        expected=oracle([list(range(48))],'pairwise','pearson','median')
        close(v[:2*24],expected[2*24:4*24]); close(v[4*24:5*24],[-x for x in expected[4*24:5*24]])
        conditions=run(['-window','48'],table='conditions.txt'); shape,flat=image(root/(conditions+'.sel000001.nii.gz'))
        assert shape[-1]==22
        control=run(table='conditions.txt')
        subprocess.run([program,'-quiet','-prefix',control+'.nii','-dataTableFile','conditions.txt'],cwd=root,env=dict(os.environ,OMP_NUM_THREADS='1',AFNI_NOMMAP='YES'),check=True,capture_output=True)
        close(flat[:10*24],image(root/(control+'.nii'))[1])
        # Conditions share complete-case filtering even when one group's
        # source is the only nonfinite value at a retained time point.
        raw=(root/'conditions.txt').read_text().splitlines()
        table('allgroup.txt',['Subj InputFile']+[f's{i:02d} s{i:02d}.nii' for i in range(1,7)])
        allgroup=run(['-window','48'],table='allgroup.txt')
        assert len(next(iter(results(allgroup).values())))==5*24
        table('threegroup.txt',['Subj Group InputFile']+[f's{i:02d} G{(i-1)//3} s{(i-1)%6+1:02d}.nii' for i in range(1,10)])
        threegroup=run(['-window','48'],table='threegroup.txt')
        assert len(next(iter(results(threegroup).values())))==13*24
        # Later collisions must be caught before the first bucket is written.
        nextprefix=f'run{count+1:03d}'
        (root/(nextprefix+'.sel000002.nii.gz')).write_text('existing')
        before=count
        count+=1
        r=subprocess.run([program,'-quiet','-prefix',nextprefix,'-window','8','-step','4','-dataTableFile','subjects.txt'],cwd=root,env=dict(os.environ,AFNI_DECONFLICT='NO',OMP_NUM_THREADS='1'),capture_output=True,text=True)
        assert r.returncode and 'already exists' in r.stderr
        assert not (root/(nextprefix+'.sel000001.nii.gz')).exists()
        assert (root/(nextprefix+'.sel000002.nii.gz')).read_text()=='existing'
        # Invalid options / deferred estimands / memory checks fail before outputs.
        for opts2,table2,error in [(['-block_summary','fisher_mean','-combine_blocks','pool'], 'pool.txt','cannot be combined'),
            (['-xmat_blocks','by_stim','-xmat_threshold','.5','-block_weight','retained_n'],'xmat_subjects.txt','M4a'),
            (['-window','8','-edges'],'subjects.txt','deferred'),
            (['-window','8','-roi_only'],'subjects.txt','require -atlas'),
            (['-window','8','-segment_center','mean'],'subjects.txt','requires'),
            (['-window','8','-min_valid','2'],'subjects.txt','integer'),
            (['-window','8','-min_valid_frac','1.1'],'subjects.txt','[0,1]'),
            (['-window','8','-min_valid_frac','nan'],'subjects.txt','[0,1]'),
            (['-window','8','-memory_limit','.000001'],'subjects.txt','exceeds limit')]: run(opts2,table2,error)
        print(f'PASS: {count} M3 estimation cases; every window checked against scalar math')


if __name__=='__main__': main()
