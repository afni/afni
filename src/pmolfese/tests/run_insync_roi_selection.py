#!/usr/bin/env python3
"""M5 ROI selection outputs against explicit parcel-mean data and scalar pairs."""
import argparse
import itertools
import math
import os
from pathlib import Path
import re
import statistics
import shutil
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image, correlation, f32
from run_insync_block_summary import read_table
from run_insync_window_baseline import summary


def main():
    ap=argparse.ArgumentParser(description=__doc__); ap.add_argument('--program',required=True)
    program=str(Path(ap.parse_args().program).resolve()); count=0
    with tempfile.TemporaryDirectory(prefix='insync-m5-') as tmp:
        root=write_fixture(Path(tmp)/'fixture')
        parcels=[list(range(1,8)),list(range(9,16)),list(range(17,23))]
        labels=[0]*24
        for r,vs in enumerate(parcels):
            for v in vs: labels[v]=r+1
        nifti(root/'parcel.nii',labels)
        mask=[int(v not in (2,10,20)) for v in range(24)]
        nifti(root/'r_mask.nii',mask)
        parcels=[[v for v in vs if mask[v]] for vs in parcels]
        means=[]
        for si in range(6):
            _,flat=image(root/f's{si+1:02d}.nii')
            means.append([[f32(statistics.mean(flat[t*24+v] for v in vs)) for t in range(48)] for vs in parcels])
            nifti(root/f'mean{si}.nii',[means[si][labels[v]-1][t] if labels[v] else 0 for t in range(48) for v in range(24)],48)
        lines=['Subj Group Selection Segment Run StartTR StopTR InputFile']
        for si in range(6):
            for b,(lo,hi) in enumerate(((0,11),(24,47))):
                lines.append(f's{si+1:02d} {"A" if si<3 else "B"} Combined b{b} 1 {lo} {hi} s{si+1:02d}.nii')
        (root/'blocks.txt').write_text('\n'.join(lines)+'\n')
        (root/'means.txt').write_text('\n'.join(re.sub(r's(\d\d)\.nii',lambda m:f'mean{int(m[1])-1}.nii',l) for l in lines)+'\n')
        def run(opts=(),table='blocks.txt',err=None,quiet=True,threads=2):
            nonlocal count
            count+=1; prefix=f'r{count:03d}'
            command=[program,'-prefix',prefix]+(['-quiet'] if quiet else [])+list(opts)+['-dataTableFile',table]
            result=subprocess.run(command,cwd=root,env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_NOMMAP='YES',AFNI_DECONFLICT='NO'),text=True,capture_output=True)
            if err:
                assert result.returncode and err.lower() in result.stderr.lower(),(command,err,result.stderr)
                assert not list(root.glob(prefix+'.sel*.nii*'))
                return result
            assert result.returncode==0,(command,result.stderr)
            assert (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
            return prefix,result
        atlas=['-atlas','parcel.nii','-mask','r_mask.nii']
        def rows(p): return read_table(root/(p+'.roi.tsv'))
        def values(p): return {(r['SelectionID'],r['ROI'],r['Kind'],r['Brick']):float(r['Value']) for r in rows(p)}
        def check_means(p,q,ids=(1,2,3)):
            rr=values(p)
            index=read_table(root/(q+'.results.tsv'))
            for entry in index:
                for kind,key in (('result','Bucket'),('block_support','BlockSupportBucket')):
                    if entry[key]=='NA': continue
                    _,data=image(root/entry[key]); nb=len(data)//24
                    for roi in ids:
                        for b in range(nb):
                            actual=rr[entry['SelectionID'],str(roi),kind,str(b)]
                            expected=data[b*24+parcels[roi-1][0]]
                            assert abs(actual-expected)<2e-6,(p,q,kind,roi,b,actual,expected)
        def check_bucket(p):
            rr=values(p)
            for entry in read_table(root/(p+'.results.tsv')):
                for kind,key in (('result','Bucket'),('block_support','BlockSupportBucket')):
                    if entry[key]=='NA': continue
                    _,data=image(root/entry[key]); nb=len(data)//24
                    for b in range(nb):
                        for v in range(24):
                            expected=rr[entry['SelectionID'],str(labels[v]),kind,str(b)] if labels[v] and mask[v] else 0
                            assert abs(data[b*24+v]-expected)<2e-6,(p,kind,b,v)
        def check_matrix(p,metric,bs,pooled=False):
            table=read_table(root/'pairs.tsv')
            assert len(table)==3*3*15
            for row in table:
                a,b=int(row['Subj1'][1:])-1,int(row['Subj2'][1:])-1
                roi=int(row['ROI'])-1; selection=row['SelectionID']
                segments=[range(12),range(24,48)]
                if selection!='sel000001': segments=[segments[int(selection[-1])-2]]
                if pooled: segments=[list(itertools.chain.from_iterable(segments))]
                corr=[correlation([means[a][roi][t] for t in ts],[means[b][roi][t] for t in ts],metric) for ts in segments]
                expected=corr[0] if len(corr)==1 else summary(corr,bs)
                assert row['Valid']=='1' and abs(float(row['Correlation'])-expected)<2e-6,(row,expected)
        for method,metric,bs in itertools.product(('pairwise','loo'),('pearson','spearman'),('median','fisher_mean')):
            opts=['-isc_method',method,'-correlation',metric,'-block_summary',bs]
            q,_=run(opts,table='means.txt')
            p,_=run(atlas+opts+['-save_matrix','pairs.tsv'])
            check_means(p,q); check_matrix(p,metric,bs)
            (root/'pairs.tsv').unlink()
            only,_=run(atlas+opts+['-roi_only'])
            assert values(p)==values(only)
            check_bucket(only)
            # Atlas mode's spatial bucket remains voxelwise, not parcel-expanded.
            plain,_=run(opts+['-mask','r_mask.nii'])
            for e,f in zip(read_table(root/(p+'.results.tsv')),read_table(root/(plain+'.results.tsv'))):
                assert image(root/e['Bucket'])[1]==image(root/f['Bucket'])[1]
        q,_=run(['-combine_blocks','pool'],table='means.txt')
        p,_=run(atlas+['-combine_blocks','pool','-save_matrix','pairs.tsv','-roi_only'])
        check_means(p,q); check_matrix(p,'pearson','median',True); check_bucket(p)
        (root/'pairs.tsv').unlink()
        p,_=run(atlas+['-roi_sel','2','-roi_only'])
        assert {r['ROI'] for r in rows(p)}=={'2'}
        # Xmat selections preserve stimulus labels and multi-run identity.
        p,_=run(atlas+['-roi_only','-xmat_blocks','by_stim','-xmat_across_runs','-xmat_threshold','.5','-block_missing','drop_common'],table='xmat_subjects.txt')
        assert {r['Stimulus'] for r in rows(p)}=={'Vis','Aud'}
        assert any(r['Run']=='mixed' for r in rows(p))
        # Every brick appears exactly once in the manifest and resolves in AFNI.
        info=shutil.which('3dinfo')
        assert info, '3dinfo is required for output-selector verification'
        grouped={}
        for entry in read_table(root/(p+'.bricks.tsv')): grouped.setdefault(entry['File'],[]).append(entry)
        for file,entries in grouped.items():
            out=subprocess.run([info,'-label',file],cwd=root,text=True,capture_output=True)
            assert not out.returncode,(file,out.stderr)
            assert out.stdout.strip().split('|')==[e['Label'] for e in entries]
        p,_=run(atlas+['-roi_only','-float16'])
        q,_=run(atlas+['-roi_only'])
        assert values(p)==values(q)
        # Repeated conditions: each ROI value carries the corresponding condition.
        for old,new in (('blocks.txt','conditions.txt'),('means.txt','meanconditions.txt')):
            content=(root/old).read_text().splitlines(); expanded=[content[0].replace('Group ','Group Condition ')]
            for line in content[1:]:
                words=line.split()
                for c in ('C1','C2'): expanded.append(' '.join(words[:2]+[c]+words[2:]))
            (root/new).write_text('\n'.join(expanded)+'\n')
        p,_=run(atlas+['-roi_only'],table='conditions.txt'); q,_=run(table='meanconditions.txt')
        check_means(p,q); check_bucket(p)
        assert {r['Condition'] for r in rows(p)}=={'C1','C2'}
        for row in rows(p):
            if row['Kind']=='result': assert row['Condition'] in row['Label']
        # A subject-specific censor is applied identically to voxel/ROI engines.
        (root/'censor_test.1D').write_text('\n'.join('0' if t%7==0 else '1' for t in range(48))+'\n')
        for old,new in (('blocks.txt','censored.txt'),('means.txt','meancensored.txt')):
            lines=(root/old).read_text().splitlines(); converted=[lines[0].replace('InputFile','Censor InputFile')]
            for line in lines[1:]:
                words=line.split(); converted.append(' '.join(words[:-1]+['censor_test.1D',words[-1]]))
            (root/new).write_text('\n'.join(converted)+'\n')
        p,_=run(atlas+['-roi_only'],table='censored.txt'); q,_=run(table='meancensored.txt'); check_means(p,q)
        # Atlas view overrides scanner-space inputs when no mask is supplied.
        copy=shutil.which('3dcopy'); refit=shutil.which('3drefit')
        assert copy and refit
        subprocess.run([copy,'parcel.nii','template'],cwd=root,check=True,capture_output=True)
        subprocess.run([refit,'-view','tlrc','-space','MNI','template+orig'],cwd=root,check=True,capture_output=True)
        p,_=run(['-atlas','template+tlrc','-roi_only'])
        for row in read_table(root/(p+'.results.tsv')):
            for key in ('Bucket','BlockSupportBucket'):
                if row[key]=='NA': continue
                out=subprocess.run([info,'-space','-av_space',row[key]],cwd=root,capture_output=True,text=True,check=True)
                assert 'MNI' in out.stdout and '+tlrc' in out.stdout,out.stdout
        # Constant parcel mean invalidates one block for both groups; support
        # follows the same require/drop rules as explicitly averaged input.
        _,flat=image(root/'s01.nii')
        for t in range(12):
            for v in parcels[0]: flat[t*24+v]=4
        nifti(root/'s01.nii',flat,48)
        _,flat=image(root/'mean0.nii')
        for t in range(12):
            for v in range(24):
                if labels[v]==1: flat[t*24+v]=4
        nifti(root/'mean0.nii',flat,48)
        for flags in ([],['-block_missing','drop_common'],['-block_missing','drop_common','-min_blocks','2']):
            p,_=run(atlas+flags+['-roi_only','-save_matrix','pairs.tsv']); q,_=run(flags,table='means.txt')
            check_means(p,q); check_bucket(p)
            invalid=[r for r in read_table(root/'pairs.tsv') if r['SelectionID']=='sel000001' and r['ROI']=='1']
            assert all(r['Valid']==('1' if len(flags)==2 else '0') for r in invalid)
            (root/'pairs.tsv').unlink()
        # Censoring, sliding runs, and deterministic threaded ROI output.
        opts=atlas+['-window','8','-step','8','-run_starts','0,24','-roi_only']
        p,_=run(opts,table='subjects.txt',threads=1); q,_=run(opts,table='subjects.txt',threads=2)
        assert values(p)==values(q)
        # Explicit whole-input output spans every run and has its own role.
        p,_=run(atlas+['-roi_only','-whole_summary','-window','8','-step','8','-run_starts','0,24'],table='subjects.txt')
        q,_=run(atlas+['-roi_only','-window','48'],table='subjects.txt')
        whole={r['SelectionID'] for r in read_table(root/(p+'.selections.tsv')) if r['Role']=='whole_input'}
        assert len(whole)==1
        whole=whole.pop()
        a={(r['ROI'],r['Kind'],r['Brick']):r['Value'] for r in rows(p) if r['SelectionID']==whole}
        b={(r['ROI'],r['Kind'],r['Brick']):r['Value'] for r in rows(q)}
        assert a==b and all(r['Run']=='all' for r in rows(p) if r['SelectionID']==whole)
        # Identical constituent blocks used by different aggregates are written
        # once, with a forward component link from each aggregate's rows.
        lines=(root/'blocks.txt').read_text().splitlines()
        (root/'shared.txt').write_text('\n'.join(lines+[l.replace('Combined','Other') for l in lines[1:]])+'\n')
        p,_=run(atlas+['-roi_only'],table='shared.txt')
        entries=read_table(root/(p+'.results.tsv'))
        assert len(entries)==4 # 2 aggregates + 2 shared children
        manifest=read_table(root/(p+'.selections.tsv'))
        agg=[r for r in manifest if r['Role']=='block_aggregate_component']
        assert len({r['ComponentSelectionID'] for r in agg})==2 and all(r['ComponentSelectionID']!='NA' for r in agg)
        # .1D preserves nonfinite values; one missing parcel voxel invalidates
        # its mean at that TR, then common finite support is selected.
        (root/'smallatlas.1D').write_text('1\n1\n')
        lines=['Subj Group Selection Segment Run StartTR StopTR InputFile']
        for si in range(6):
            data=[]
            for v in range(2):
                vals=[str(means[si][v][t]) for t in range(48)]
                if si==0 and v==0: vals[6]='nan'
                data.append(' '.join(vals))
            filename=f'bad{si}.1D'
            (root/filename).write_text('# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "2,1,1"\n# ni_delta = "1,1,1"\n# ni_origin = "0,0,0"\n# ni_axes = "R-L,A-P,I-S"\n# >\n'+'\n'.join(data)+'\n# </AFNI_3D_dataset>\n')
            lines.append(f's{si} {"A" if si<3 else "B"} One b 1 4 9 {filename}')
        (root/'badtable.txt').write_text('\n'.join(lines)+'\n')
        small=['-atlas','smallatlas.1D','-roi_only']
        run(small,table='badtable.txt',err='nonfinite selected input: ROI')
        p,_=run(small+['-missing','common'],table='badtable.txt')
        assert all(float(r['Value'])==5 for r in rows(p) if r['Label']=='ValidTR')
        (root/'badcensor.1D').write_text('\n'.join('0' if t==6 else '1' for t in range(48))+'\n')
        q,_=run(small+['-censor','badcensor.1D'],table='badtable.txt')
        assert values(p)==values(q)
        p,_=run(small+['-missing','common','-min_valid','6','-save_matrix','pairs.tsv'],table='badtable.txt')
        assert all(r['Valid']=='0' and math.isnan(float(r['Correlation'])) for r in read_table(root/'pairs.tsv'))
        (root/'pairs.tsv').unlink()
        run(['-window','8','-save_matrix','bad.tsv'],table='subjects.txt',err='require -atlas')
        run(atlas+['-save_matrix','missing/path.tsv'],err='not writable')
        upcoming=f'r{count+1:03d}'
        run(atlas+['-save_matrix','./'+upcoming+'.roi.tsv'],err='collides')
        upcoming=f'r{count+1:03d}'
        (root/(upcoming+'.bricks.tsv')).write_text('occupied')
        run(atlas,err='already exists')
        (root/'collision.txt').write_text((root/'blocks.txt').read_text().replace(' A ',' a+b ').replace(' B ',' a_b '))
        run(atlas,table='collision.txt',err='label collision')
        run(atlas+['-memory_limit','.0000001'],err='memory estimate exceeds')
        # Fixed batch size of one: source/result ledger is invariant as window
        # count rises; scheduling bytes are reported separately.
        logs=[]
        for step in ('24','1'):
            _,r=run(atlas+['-roi_only','-window','24','-step',step],table='subjects.txt',quiet=False)
            logs.append(r.stderr)
        assert all('one result bucket at a time' in log for log in logs)
        assert all('work ' in log for log in logs)
        ledger=[re.search(r'sources (\d+) bytes; result/support (\d+) bytes',log).groups() for log in logs]
        assert ledger[0]==ledger[1],ledger
    print(f'PASS: {count} M5 cases; ROI mean oracles, matrices, support, labels, preflight, threads and bounded outputs')

if __name__=='__main__': main()
