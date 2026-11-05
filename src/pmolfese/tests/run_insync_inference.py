#!/usr/bin/env python3
"""M7: independent scalar exact-null, global correction and bootstrap oracles."""
import argparse, csv, io, itertools, math, os, statistics, subprocess, tempfile
from pathlib import Path
from functools import lru_cache
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image, f32, correlation
from run_insync_window_baseline import summary

def rows(path):
    return list(csv.DictReader(io.StringIO('\n'.join(x for x in path.read_text().splitlines() if not x.startswith('#'))), delimiter='\t'))

def main():
    parser=argparse.ArgumentParser(description=__doc__); parser.add_argument('--program',required=True)
    program=str(Path(parser.parse_args().program).resolve()); runs=0; checked=0
    with tempfile.TemporaryDirectory(prefix='insync-m7-') as tmp:
        root=write_fixture(Path(tmp)/'fixture'); nifti(root/'small.nii',[int(v in (0,2,3)) for v in range(24)])
        source=[]
        for c in ('','_C2'):
            for s in range(1,7):
                flat=image(root/f's{s:02d}{c}.nii')[1]
                source.append([[flat[t*24+v] for t in range(48)] for v in range(24)])
        def run(opts=(),table='conditions.txt',threads=1,error=None):
            nonlocal runs
            runs+=1; prefix=f'r{runs:03d}'
            cmd=[program,'-quiet','-mask','small.nii','-prefix',prefix]+[str(x).replace('{prefix}',prefix) for x in opts]+['-dataTableFile',table]
            proc=subprocess.run(cmd,cwd=root,env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_DECONFLICT='NO'),capture_output=True,text=True)
            if error:
                assert proc.returncode and error.lower() in proc.stderr.lower(),(cmd,proc.stderr)
                assert not (root/(prefix+'.results.tsv')).exists() or not (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
                return prefix
            assert not proc.returncode,(cmd,proc.stderr)
            assert (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
            return prefix
        def outputs(prefix):
            result={}; cache={}
            for row in rows(root/(prefix+'.inference.tsv')):
                fn=row['File']
                if fn not in cache: cache[fn]=image(root/fn)[1]
                result[row['SelectionID'],row['Label']]=cache[fn][int(row['Brick'])*24:(int(row['Brick'])+1)*24]
            return result
        def draws(prefix,kind):
            out={}
            for row in rows(root/(prefix+'.inference_draws.tsv')):
                if row['Kind']==kind: out.setdefault(int(row['Draw']),[]).append(int(row['Sign']) if kind=='condition' else int(row['SourceSubject'][1:])-1)
            return list(out.values())
        def close(a,b,tol=2e-6):
            nonlocal checked
            checked+=1
            assert abs(a-b)<=tol,(a,b,tol)
        def check(prefix,method='pairwise',metric='pearson',blocks=None,blockkind='median',tail='two',censor=False):
            out=outputs(prefix); selections=sorted({k[0] for k in out}); windows=blocks or [[[t for t in range(start,start+12)]] for start in (0,24)]
            if censor:
                keep=[int(x) for x in (root/'common.censor.1D').read_text().split()]
                windows=[[[t for t in block if keep[t]] for block in selection] for selection in windows]
            gd,cd,bd=[draws(prefix,k) for k in ('group','condition','bootstrap')]
            if gd:
                assert len(gd)==20 and len({tuple(sorted(d[:3])) for d in gd})==20 and gd[0]==list(range(6))
            if cd: assert len(cd)==64 and len({tuple(d) for d in cd})==64 and cd[0]==[1]*6
            @lru_cache(None)
            def estimate(si,v,members):
                if len(set(members))<2: return math.nan
                components=[]
                for block in windows[si]:
                    series=[[source[s][v][t] for t in block] for s in members]
                    if method=='pairwise':
                        vals=[correlation(series[a],series[b],metric) for a,b in itertools.combinations(range(len(members)),2) if members[a]!=members[b]]
                    else:
                        vals=[correlation(series[a],[f32(statistics.mean(series[b][t] for b in range(len(members)) if b!=a)) for t in range(len(block))],metric) for a in range(len(members))]
                    components.append(vals)
                return f32(summary([f32(summary(list(vs),blockkind)) for vs in zip(*components)],'median'))
            families=[{},{}]
            for si,sid in enumerate(selections):
                for v in (0,2,3):
                    for c in range(2):
                        if gd:
                            null=[f32(estimate(si,v,tuple(s+6*c for s in d[:3]))-estimate(si,v,tuple(s+6*c for s in d[3:]))) for d in gd]
                            families[0][sid,f'C{c+1}_A-B',v]=null
                        if bd:
                            for g in range(2):
                                vals=sorted(x for d in bd if math.isfinite(x:=estimate(si,v,tuple(s+6*c for s in d[g*3:g*3+3]))))
                                stem=f'C{c+1}_{"AB"[g]}'
                                close(out[sid,'BootN_'+stem][v],len(vals),0); close(out[sid,'BootValid_'+stem][v],1,0)
                                for label,p in (('BootLo',f32(.025)),('BootHi',f32(1-f32(.025)))):
                                    pos=(len(vals)-1)*p; lo=int(pos); hi=math.ceil(pos)
                                    close(out[sid,label+'_'+stem][v],f32(vals[lo]*(hi-pos)+vals[hi]*(pos-lo)) if hi!=lo else vals[lo])
                    if cd:
                        null=[[],[],[]]
                        for signs in cd:
                            effects=[]
                            for g in range(2):
                                a=tuple(s+(6 if signs[s]<0 else 0) for s in range(g*3,g*3+3)); b=tuple((s+6)%12 for s in a)
                                effects.append(f32(estimate(si,v,a)-estimate(si,v,b)))
                            for e,val in enumerate(effects+[f32(effects[0]-effects[1])]): null[e].append(val)
                        for e,label in enumerate(('A','B','GxCond')): families[1][sid,'C1-C2_'+label,v]=null[e]
            for ci,family in enumerate(families):
                if not family: continue
                twosided=ci or tail=='two'; bisided=not ci and tail=='bisided'
                maxima=[max(abs(n[d]) if twosided else n[d] for n in family.values()) for d in range(len(next(iter(family.values()))))]
                negative=[max(-n[d] for n in family.values()) for d in range(len(maxima))]
                pvals={}
                for key,null in family.items():
                    sid,stem,v=key; obs=null[0]; nd=len(null)
                    sign=-1 if bisided and obs<0 else 1
                    cmp=lambda a: abs(a) if twosided else sign*a
                    p=min(1,(2 if bisided else 1)*sum(cmp(a)>=cmp(obs) for a in null)/nd)
                    mx=negative if bisided and obs<0 else maxima
                    pf=min(1,(2 if bisided else 1)*sum(a>=cmp(obs) for a in mx)/nd)
                    prefix='C' if ci else ''
                    close(out[sid,('Cond_' if ci else 'GroupDiff_')+stem][v],obs)
                    
                    if abs(out[sid,prefix+'P_'+stem][v]-p)>2e-6: print('mismatch',method,metric,key,obs,null,out[sid,prefix+'P_'+stem][v],p)
                    close(out[sid,prefix+'P_'+stem][v],p); close(out[sid,prefix+'PFWE_'+stem][v],pf)
                    assert out[sid,prefix+'P_'+stem][v]>0
                    pvals[key]=p
                ordered=sorted(pvals,key=pvals.get); q=1
                for rank in range(len(ordered),0,-1):
                    key=ordered[rank-1]; q=min(q,pvals[key]*len(ordered)/rank); sid,stem,v=key
                    close(out[sid,('CQ_' if ci else 'Q_')+stem][v],q)
            return out
        base=['-window','12','-step','24','-exact','-nboot','100','-condition_contrast','C1','C2','-condition_exact']
        for method,metric in itertools.product(('pairwise','loo'),('pearson','spearman')):
            opts=base+['-isc_method',method,'-correlation',metric]
            p=run(opts); a=check(p,method,metric)
            q=run(opts,threads=2); assert a==outputs(q)
            assert (root/(p+'.inference_draws.tsv')).read_bytes()==(root/(q+'.inference_draws.tsv')).read_bytes()
        check(run(base+['-censor','common.censor.1D']),censor=True)
        for tail in ('one','bisided'): check(run(base+['-tail',tail]),tail=tail)
        # Component-first block aggregation, including bootstrap and swaps.
        lines=['Subj Group Condition Selection Segment Run StartTR StopTR InputFile']
        for s in range(6):
            for c in range(2):
                for b,start in enumerate((0,24)):
                    lines.append(f's{s+1:02d} {"AB"[s//3]} C{c+1} combined b{b} 1 {start} {start+11} s{s+1:02d}{"_C2" if c else ""}.nii')
        (root/'blocks.txt').write_text('\n'.join(lines)+'\n')
        for method,kind in itertools.product(('pairwise','loo'),('median','fisher_mean')):
            opts=base[4:]+['-combine_blocks','block_summary','-block_summary',kind,'-isc_method',method]
            p=run(opts,table='blocks.txt')
            # The schedule includes component children: obtain actual ordered intervals.
            sch=rows(root/(p+'.selections.tsv')); windows=[]
            for sid in sorted({k[0] for k in outputs(p)}):
                windows.append([list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1)) for r in sch if r['SelectionID']==sid and r['Subject']=='s01' and r['Condition']=='C1'])
            check(p,method,blocks=windows,blockkind=kind)
        # Adding another selection cannot lower max-FWE for unchanged cells.
        single=outputs(run(['-window','48','-exact']))
        both=outputs(run(['-window','12','-step','24','-whole_summary','-exact']))
        target=next(s for s,l in both if l=='GroupDiff_C1_A-B' and both[s,l]==single['sel000001',l])
        for (sid,label),vals in single.items():
            if label.startswith('PFWE_'): assert all(b>=a for a,b in zip(vals,both[target,label]))
        # Identical conditions: all contrasts zero, all p/q one in mask.
        (root/'same.txt').write_text((root/'conditions.txt').read_text().replace('_C2.nii','.nii'))
        same=outputs(run(base,table='same.txt'))
        for (sid,label),vals in same.items():
            if label.startswith('Cond_'): assert not any(vals)
            if label.startswith(('CP_','CPFWE_','CQ_')): assert all(vals[v]==1 for v in (0,2,3))
        mis=[line.replace(' 0 11 ',' 1 12 ') if ' C2 ' in line else line for line in lines]
        (root/'mis.txt').write_text('\n'.join(mis)+'\n')
        run(base[4:]+['-combine_blocks','block_summary'],table='mis.txt',error='matched')
        run(['-window','12','-edges'],error='deferred')
        # Sampled paired swaps and sampled group assignments also share draws
        # across selections and threads (8 subjects => 70 possible assignments).
        import shutil
        eight=['Subj Group Condition InputFile']
        for si in range(8):
            for ci in range(2):
                fn=f'e{si}_{ci}.nii'; shutil.copyfile(root/f's{si%6+1:02d}{"_C2" if ci else ""}.nii',root/fn)
                eight.append(f'e{si:02d} {"AB"[si//4]} C{ci+1} {fn}')
        (root/'eight.txt').write_text('\n'.join(eight)+'\n')
        sampled=['-window','12','-step','24','-nperm','20','-condition_contrast','C1','C2','-ncondperm','20']
        a=run(sampled,table='eight.txt'); b=run(sampled,table='eight.txt',threads=2)
        assert outputs(a)==outputs(b)
        assert (root/(a+'.inference_draws.tsv')).read_bytes()==(root/(b+'.inference_draws.tsv')).read_bytes()
        assert 'group assignments sampled; condition swaps sampled' in (root/(a+'.inference.tsv')).read_text()
        # Parcel estimates use mean BOLD; correcting painted voxels repeatedly
        # would be wrong. An independent scalar parcel oracle checks the maps.
        nifti(root/'unity.nii',[1]*24)
        original=source
        source=[[[f32(statistics.mean(subject[v][t] for v in (0,2,3))) for t in range(48)] for _ in range(24)] for subject in original]
        parcel=run(base+['-atlas','unity.nii','-roi_only'])
        check(parcel)
        assert len(rows(root/(parcel+'.inference_roi.tsv')))==102
        source=original
        # Export and matrix coexistence; refuse aliases before truncating files.
        run(base+['-save_pairwise','{prefix}_pairs','-atlas','unity.nii','-save_matrix','{prefix}_matrix.1D'])
        run(base+['-atlas','unity.nii','-save_matrix','{prefix}.inference.tsv'],error='collides')
        run(base+['-memory_limit','.000001'],error='exceeds limit')
        # Coverage-failed blocks: require_all invalidates aggregate; drop_common
        # keeps exactly the observed surviving block on every resample.
        (root/'half.1D').write_text('\n'.join(str(int(t>=24)) for t in range(48))+'\n')
        for policy in ('require_all','drop_common'):
            p=run(base[4:]+['-censor','half.1D','-block_missing',policy],table='blocks.txt')
            out=outputs(p); sch=rows(root/(p+'.selections.tsv'))
            # Identify the aggregate by two segment rows rather than ordinal.
            counts={sid:sum(r['SelectionID']==sid and r['Subject']=='s01' and r['Condition']=='C1' for r in sch) for sid,_ in out}
            agg=next(sid for sid,n in counts.items() if n==2)
            child=next(r['SelectionID'] for r in sch if counts[r['SelectionID']]==1 and r['OriginalStartTR']=='24')
            for label in ('InferValid_C1_A-B','CInferValid_C1-C2_A'):
                assert all(out[agg,label][v]==(policy=='drop_common') for v in (0,2,3))
            if policy=='drop_common':
                for (sid,label),value in out.items():
                    if sid==agg: assert value==out[child,label],label
        # No valid null family must not produce a completed result marker.
        (root/'zero.1D').write_text('0\n'*48)
        run(base+['-censor','zero.1D'],error='no complete finite-null')
        # Static exact group results match a full-timeline selection after the
        # label-to-subject inversion fix (statistical labels differ).
        p=run(['-window','48','-exact']); expected=outputs(p)
        static='static.nii'; subprocess.run([program,'-quiet','-mask','small.nii','-prefix',static,'-exact','-dataTableFile','conditions.txt'],cwd=root,env=dict(os.environ,OMP_NUM_THREADS='1'),check=True,capture_output=True)
        labeltext=subprocess.check_output(['3dinfo','-label',str(root/static)],text=True).strip().split('|')
        flat=image(root/static)[1]; ncomp=0
        for j,label in enumerate(labeltext):
            if ('sel000001',label) in expected and label.startswith(('P_','Q_','PFWE_')):
                for v in (0,2,3): close(flat[j*24+v],expected['sel000001',label][v]); ncomp+=1
        assert ncomp==18,(ncomp,labeltext)
        print(f'PASS: {runs} M7 executions; {checked} independent numeric comparisons')
if __name__=='__main__': main()
