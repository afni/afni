#!/usr/bin/env python3
"""M4 black-box block summaries: component-first scalar oracles and support maps."""
import argparse
import csv
import io
import itertools
import math
import os
from pathlib import Path
import statistics
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image, f32, correlation
from run_insync_window_baseline import summary


def read_table(path):
    return list(csv.DictReader(io.StringIO('\n'.join(l for l in path.read_text().splitlines() if not l.startswith('#'))),delimiter='\t'))


def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--program',required=True)
    program=str(Path(p.parse_args().program).resolve()); count=0
    with tempfile.TemporaryDirectory(prefix='insync-m4-') as temp:
        root=write_fixture(Path(temp)/'fixture')
        segments=[list(range(0,6)),list(range(8,20)),list(range(24,48))]
        source=[]; second=[]; plainfiles=[]
        for si in range(6):
            _,flat=image(root/f's{si+1:02d}.nii')
            original=[[flat[t*24+v] for t in range(48)] for v in range(24)]
            plainfiles.append(f'plain{si}.nii')
            nifti(root/plainfiles[-1],flat,48)
            for b,segment in enumerate(segments):
                for v in range(24):
                    for t in segment: original[v][t]=f32(original[v][t]*(1+.3*si*b)+30*si*(b-1))
            source.append(original)
            nifti(root/f's{si+1:02d}.nii',[original[v][t] for t in range(48) for v in range(24)],48)
            _,flat=image(root/f's{si+1:02d}_C2.nii')
            second.append([[flat[t*24+v] for t in range(48)] for v in range(24)])
        def table(name,blocks=segments,files=None,censors=None,conditions=False):
            lines=['Subj Group '+('Condition ' if conditions else '')+'Selection Segment Run StartTR StopTR '+('Censor ' if censors else '')+'InputFile']
            for si in range(6):
                for ci in range(2 if conditions else 1):
                    for b,block in enumerate(blocks):
                        filename=files[si] if files else f's{si+1:02d}'+('_C2' if ci else '')+'.nii'
                        lines.append(f's{si+1:02d} {"A" if si<3 else "B"} '+(f'C{ci+1} ' if conditions else '')+f'Combined b{b+1} 1 {block[0]} {block[-1]} '+(censors[si]+' ' if censors else '')+filename)
            (root/name).write_text('\n'.join(lines)+'\n'); return name
        table('blocks.txt')
        def run(options=(),table='blocks.txt',error=None,threads=1,check=False):
            nonlocal count
            count+=1; prefix=f'b{count:03d}'
            env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_NOMMAP='YES',AFNI_DECONFLICT='NO')
            r=subprocess.run([program,'-quiet','-prefix',prefix]+(['-check_table'] if check else [])+list(options)+['-dataTableFile',table],cwd=root,env=env,text=True,capture_output=True)
            if error:
                assert r.returncode and error.lower() in r.stderr.lower(),(options,error,r.stderr)
                assert not list(root.glob(prefix+'.sel*')),options
                return r
            assert not r.returncode,(options,r.stdout,r.stderr)
            if not check: assert (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
            return prefix
        def outputs(prefix): return read_table(root/(prefix+'.results.tsv'))
        def result(prefix,id='sel000001'): return image(root/next(r['Bucket'] for r in outputs(prefix) if r['SelectionID']==id))[1]
        def support(prefix,id='sel000001'): return image(root/next(r['BlockSupportBucket'] for r in outputs(prefix) if r['SelectionID']==id))[1]
        def close(a,b,tol=1e-6):
            assert len(a)==len(b),(len(a),len(b))
            worst=max(abs(x-y) for x,y in zip(a,b))
            assert worst<=tol,worst
        def oracle(method='pairwise',metric='pearson',bs='median',gs='median',data=source,
                   blocks=segments,keep=None,drop=False,min_blocks=1,min_frac=0,min_valid=3,min_valid_frac=0):
            maps=[[0.]*24 for _ in range(14)]; supportmaps=[[0.]*24 for _ in range(3*len(blocks))]
            for v in range(24):
                blockvalues=[]; admitted=[]; lengths=[]
                for b,block in enumerate(blocks):
                    ts=[t for t in block if (keep is None or keep[t]) and all(math.isfinite(data[si][v][t]) for si in range(6))]
                    supportmaps[3*b][v]=len(ts); lengths.append(len(ts))
                    gv=[[],[]]
                    if len(ts)>=min_valid and len(ts)/len(block)>=min_valid_frac:
                        for gi,group in enumerate((range(3),range(3,6))):
                            if method=='pairwise':
                                gv[gi]=[correlation([data[a][v][t] for t in ts],[data[c][v][t] for t in ts],metric) for a,c in itertools.combinations(group,2)]
                            else:
                                for a in group:
                                    ref=[f32(statistics.mean(data[c][v][t] for c in group if c!=a)) for t in ts]
                                    gv[gi].append(correlation([data[a][v][t] for t in ts],ref,metric))
                    good=all(len(row)==3 and all(math.isfinite(x) for x in row) for row in gv)
                    if good: admitted.append(b); supportmaps[3*b+1][v]=1
                    blockvalues.append(gv)
                n=len(admitted); maps[11][v]=len(blocks); maps[12][v]=n
                if n<min_blocks or n/len(blocks)<min_frac or (not drop and n!=len(blocks)): continue
                maps[13][v]=n; maps[5][v]=sum(lengths[b] for b in admitted)
                for b in admitted: supportmaps[3*b+2][v]=f32(1/n)
                for gi in range(2):
                    combined=[]
                    for component in range(3):
                        values=[blockvalues[b][gi][component] for b in admitted]
                        combined.append(values[0] if n==1 else f32(summary(values,bs)))
                    maps[gi*2][v]=f32(summary(combined,gs)); maps[gi*2+1][v]=3
                    maps[6+gi][v]=maps[8+gi][v]=1
                maps[4][v]=f32(maps[0][v]-maps[2][v]); maps[10][v]=1
            return [x for row in maps for x in row],[x for row in supportmaps for x in row]
        # All 16 method/metric/block-summary/group-summary combinations.
        reference=None
        for method,metric,bs,gs in itertools.product(('pairwise','loo'),('pearson','spearman'),('median','fisher_mean'),('median','fisher_mean')):
            prefix=run(['-isc_method',method,'-correlation',metric,'-block_summary',bs,'-summary',gs])
            expected,weights=oracle(method,metric,bs,gs)
            close(result(prefix),expected); close(support(prefix),weights)
            assert len(outputs(prefix))==4 # aggregate plus three children
            meta=read_table(root/(prefix+'.block_support.tsv'))
            assert [(int(r['OriginalStartTR']),int(r['OriginalStopTR'])) for r in meta]==[(b[0],b[-1]) for b in segments]
            assert [int(r['WeightBrick']) for r in meta]==[2,5,8]
            manifest=read_table(root/(prefix+'.selections.tsv'))
            assert sum(r['ParentSelectionID']=='sel000001' for r in manifest)==18
            if (method,metric,bs,gs)==('pairwise','pearson','median','median'): reference=prefix
        # Defaults, explicit equal weights, and thread independence.
        default=run(); close(result(default),result(reference),0)
        eq=run(['-block_weight','equal','-block_missing','require_all']); close(result(eq),result(default),0)
        threaded=run(threads=2); close(result(threaded),result(default),0); close(support(threaded),support(default),0)
        # Group-first averaging is a different answer; pooling and centered
        # pooling differ from the component-first block statistic as well.
        pool=run(['-combine_blocks','pool']); centered=run(['-combine_blocks','pool','-segment_center','mean'])
        assert max(abs(a-b) for a,b in zip(result(default)[:24],result(pool)[:24]))>.01
        assert max(abs(a-b) for a,b in zip(result(default)[:24],result(centered)[:24]))>.001
        assert max(abs(a-b) for a,b in zip(result(pool)[:24],result(centered)[:24]))>.01
        child=[result(default,f'sel{n:06d}') for n in (2,3,4)]
        wrong=[statistics.median([v[voxel] for v in child]) for voxel in range(24)]
        assert max(abs(a-b) for a,b in zip(wrong,result(default)[:24]))>1e-4
        # A censored first block has two retained TRs; drops are common to both groups.
        keep=[int(t not in (0,1,2,3)) for t in range(48)]
        (root/'keep.1D').write_text('\n'.join(map(str,keep))+'\n')
        table('censored.txt',censors=['keep.1D']*6)
        for method in ('pairwise','loo'):
            for drop in (False,True):
                opts=['-isc_method',method]+(['-block_missing','drop_common'] if drop else [])
                prefix=run(opts,'censored.txt'); expected,weights=oracle(method=method,keep=keep,drop=drop)
                close(result(prefix),expected); close(support(prefix),weights)
                if drop: assert result(prefix)[5*24]==36 and result(prefix)[13*24]==2
                else: assert not any(result(prefix)[:11*24]) and result(prefix)[12*24]==2
        (root/'zero.1D').write_text('0\n'*48)
        zero=run(['-censor','zero.1D','-block_missing','drop_common'])
        expected,weights=oracle(keep=[0]*48,drop=True); close(result(zero),expected); close(support(zero),weights)
        for n,frac in ((3,0),(1,.67),(2,2/3)):
            prefix=run(['-block_missing','drop_common','-min_blocks',str(n),'-min_block_frac',str(frac)],'censored.txt')
            expected,weights=oracle(keep=keep,drop=True,min_blocks=n,min_frac=frac)
            close(result(prefix),expected); close(support(prefix),weights)
        for n,frac in ((7,0),(3,1)):
            prefix=run(['-block_missing','drop_common','-min_valid',str(n),'-min_valid_frac',str(frac)],'censored.txt')
            expected,weights=oracle(keep=keep,drop=True,min_valid=n,min_valid_frac=frac)
            close(result(prefix),expected); close(support(prefix),weights)
        # One subject is constant only in one voxel/block. This must remove
        # the block for group B too, without removing that subject anywhere.
        changed=[[row[:] for row in subj] for subj in source]
        for t in segments[0]: changed[0][0][t]=7.
        nifti(root/'constant.nii',[changed[0][v][t] for t in range(48) for v in range(24)],48)
        files=['constant.nii']+[f's{i:02d}.nii' for i in range(2,7)]; table('constant.txt',files=files)
        for method in ('pairwise','loo'):
            for drop in (False,True):
                prefix=run(['-isc_method',method]+(['-block_missing','drop_common'] if drop else []),'constant.txt')
                expected,weights=oracle(method=method,data=changed,drop=drop)
                close(result(prefix),expected); close(support(prefix),weights)
                assert result(prefix)[12*24]==2 and result(prefix)[12*24+1]==3
        threaded=run(['-block_missing','drop_common'],'constant.txt',threads=2)
        expected,weights=oracle(data=changed,drop=True); close(result(threaded),expected); close(support(threaded),weights)
        # A constant LOO reference can invalidate a block even when all pair
        # correlations are valid. Test admission using the chosen estimator.
        cancel=[[row[:] for row in subj] for subj in source]; files=[]
        for t in segments[0]: cancel[1][0][t]=float(t); cancel[2][0][t]=-float(t)
        for si in range(6):
            filename=f'cancel{si}.nii'; files.append(filename)
            nifti(root/filename,[cancel[si][v][t] for t in range(48) for v in range(24)],48)
        table('cancel.txt',files=files)
        for method in ('pairwise','loo'):
            prefix=run(['-isc_method',method,'-block_missing','drop_common'],'cancel.txt')
            expected,weights=oracle(method=method,data=cancel,drop=True); close(result(prefix),expected); close(support(prefix),weights)
            assert result(prefix)[12*24]==(3 if method=='pairwise' else 2)
        # Local missing-common changes retained samples before correlation and
        # eligibility. NIML 1D input preserves NaN rather than sanitizing it.
        missing=[[row[:] for row in subj] for subj in source]; missing[0][0][0]=float('nan'); files=[]
        for si in range(6):
            filename=f'missing{si}.1D'; files.append(filename)
            header='# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "24,1,1"\n# ni_delta = "1,1,1"\n# ni_origin = "0,0,0"\n# ni_axes = "R-L,A-P,I-S"\n# >\n'
            (root/filename).write_text(header+'\n'.join(' '.join(map(str,row)) for row in missing[si])+'\n# </AFNI_3D_dataset>\n')
        table('missing.txt',files=files)
        run(table='missing.txt',error='nonfinite selected input')
        for n in (3,6):
            prefix=run(['-missing','common','-block_missing','drop_common','-min_valid',str(n)],'missing.txt')
            expected,weights=oracle(data=missing,drop=True,min_valid=n); close(result(prefix),expected); close(support(prefix),weights)
        # Xmat aggregation retains all child blocks and uses identical rules.
        prefix=run(['-xmat_blocks','by_stim','-xmat_across_runs','-xmat_threshold','.5','-block_missing','drop_common'],'xmat_subjects.txt')
        assert len(outputs(prefix))==10
        manifest=read_table(root/(prefix+'.selections.tsv'))
        common=[int(v) for v in (root/'common.censor.1D').read_text().split()]
        for out in outputs(prefix):
            if out['BlockSupportBucket']=='NA': continue
            blocks=[list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1)) for r in manifest if r['SelectionID']==out['SelectionID'] and r['Subject']=='s01']
            expected,weights=oracle(blocks=blocks,keep=common,drop=True)
            close(result(prefix,out['SelectionID']),expected); close(support(prefix,out['SelectionID']),weights)
        # One-block summary is an identity on per-edge/LOO values, including
        # perfect correlations before the group Fisher summary.
        table('single.txt',blocks=[segments[0]])
        single=run(table='single.txt')
        text=(root/'X.full.xmat.1D').read_text().splitlines(); ti=0
        for i,line in enumerate(text):
            if line.startswith('#'): continue
            if 14<=ti<=21:
                vals=line.split(); vals[2]='0'; text[i]=' '.join(vals)
            ti+=1
        (root/'oneblock.xmat').write_text('\n'.join(text)+'\n')
        (root/'oneblock.txt').write_text('Subj Group Xmat InputFile\n'+'\n'.join(f's{i:02d} {"A" if i<=3 else "B"} oneblock.xmat s{i:02d}.nii' for i in range(1,7))+'\n')
        for bs in ('median','fisher_mean'):
            for gs in ('median','fisher_mean'):
                prefix=run(['-xmat_blocks','by_stim','-xmat_threshold','.5','-xmat_stims','Vis','-xmat_runs','1','-block_summary',bs,'-summary',gs],'oneblock.txt')
                rows=outputs(prefix); assert len(rows)==2
                aggregate=next(r for r in rows if r['BlockSupportBucket']!='NA'); childrow=next(r for r in rows if r['BlockSupportBucket']=='NA')
                close(result(prefix,aggregate['SelectionID'])[:11*24],result(prefix,childrow['SelectionID']),0)
        # Two conditions retain separate, explicitly reported support sets;
        # no paired-condition contrast/inference is enabled by this feature.
        table('conditions.txt',conditions=True)
        prefix=run(table='conditions.txt'); a,_=oracle(); b,_=oracle(data=second)
        aa=[a[i:i+24] for i in range(0,len(a),24)]; bb=[b[i:i+24] for i in range(0,len(b),24)]
        combined=aa[:4]+bb[:4]+[aa[4],bb[4],aa[5],bb[5]]+aa[6:8]+bb[6:8]+aa[8:10]+bb[8:10]+[aa[10],bb[10],aa[11],bb[11],aa[12],bb[12],aa[13],bb[13]]
        close(result(prefix),[x for row in combined for x in row]); assert len(support(prefix))==18*24
        # Masked voxels have no invented counts or weights; half storage
        # changes neither block membership nor normalized weights here.
        table('plain.txt',files=plainfiles)
        masked=run(['-mask','mask.nii'],'plain.txt'); half=run(['-mask','mask.nii','-float16'],'plain.txt')
        close(result(masked),result(half),.001); close(support(masked),support(half),0)
        for brick in range(14): assert all(result(masked)[brick*24+v]==0 for v in range(24) if v%4>=2)
        # Invalid options and deferred inference/weighting fail before outputs.
        for opts,error in [(['-min_blocks','0'],'integer'),(['-min_block_frac','1.1'],'[0,1]'),
            (['-min_block_frac','nan'],'[0,1]'),(['-block_summary','mean'],'median or fisher_mean'),
            (['-block_missing','drop_pair'],'require_all or drop_common'),(['-block_weight','retained_n'],'M4a'),
            (['-block_summary','median','-block_summary','median'],'twice'),
            (['-combine_blocks','pool','-min_blocks','2'],'cannot be combined'),
            (['-segment_center','mean'],'requires'),(['-edges'],'deferred'),
            (['-edges'],'deferred'),(['-memory_limit','.000001'],'exceeds limit')]: run(opts,error=error)
        run(['-block_summary','median'],table='single.txt',error='require an aggregate')
        run(['-min_blocks','2'],table='single.txt',check=True,error='require an aggregate')
        run(['-block_summary','fisher_mean','-block_missing','drop_common'],check=True)
        # Collision in a support file is found before any result bucket.
        nextprefix=f'b{count+1:03d}'; (root/(nextprefix+'.sel000001.blocks.nii.gz')).write_text('existing'); count+=1
        r=subprocess.run([program,'-quiet','-prefix',nextprefix,'-dataTableFile','blocks.txt'],cwd=root,env=dict(os.environ,AFNI_DECONFLICT='NO',OMP_NUM_THREADS='1'),capture_output=True,text=True)
        assert r.returncode and 'already exists' in r.stderr and not (root/(nextprefix+'.sel000001.nii.gz')).exists()
        print(f'PASS: {count} M4 block-summary cases; scalar component-first estimates and voxel support verified')


if __name__=='__main__': main()
