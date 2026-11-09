#!/usr/bin/env python3
"""M8 independent full-run shift/DFT, window and block inference oracles."""
import argparse, cmath, itertools, math, os, statistics, subprocess, tempfile, time, json
from pathlib import Path
from functools import lru_cache
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image, f32, correlation
from run_insync_window_baseline import summary
from run_insync_inference import rows

MASK=(0,2,3)

def factor(seed,draw,sub,freq,nfreq):
    if not draw: return 1+0j
    mask=(1<<64)-1
    flat=((draw-1)*6+sub)*nfreq+freq-1
    z=((seed^0xe7037ed1a0b428db)+0x9e3779b97f4a7c15*(flat+1))&mask
    z=((z^(z>>30))*0xbf58476d1ce4e5b9)&mask
    z=((z^(z>>27))*0x94d049bb133111eb)&mask
    z^=z>>31
    angle=2*math.pi*(z>>11)*2**-53
    return complex(f32(math.cos(angle)),f32(math.sin(angle)))

def main():
    parser=argparse.ArgumentParser(description=__doc__); parser.add_argument('--program',required=True); parser.add_argument('--evidence')
    args=parser.parse_args(); program=str(Path(args.program).resolve()); count=0; checks=0; began=time.monotonic()
    temp=tempfile.TemporaryDirectory(prefix='insync-m8-'); root=write_fixture(Path(temp.name)/'fixture')
    nifti(root/'small.nii',[int(v in MASK) for v in range(24)])
    source=[]
    for c in ('','_C2'):
        for sub in range(1,7):
            flat=image(root/f's{sub:02d}{c}.nii')[1]
            source.append([[flat[t*24+v] for t in range(48)] for v in range(24)])
    def run(opts,table='conditions.txt',threads=1,error=None,mask=True):
        nonlocal count
        count+=1; prefix=f't{count:03d}'
        cmd=[program,'-quiet','-prefix',prefix]+(['-mask','small.nii'] if mask else [])+[str(x).replace('{prefix}',prefix) for x in opts]+['-dataTableFile',table]
        proc=subprocess.run(cmd,cwd=root,env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_DECONFLICT='NO'),capture_output=True,text=True)
        if error:
            assert proc.returncode and error.lower() in proc.stderr.lower(),(cmd,proc.stderr,error)
            assert not list(root.glob(prefix+'.sel*.nii*')),cmd
            return prefix
        assert not proc.returncode,(cmd,proc.stderr)
        assert (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
        return prefix
    def outputs(prefix):
        cache={}; result={}
        for row in rows(root/(prefix+'.inference.tsv')):
            fn=row['File']
            if fn not in cache: cache[fn]=image(root/fn)[1]
            result[row['SelectionID'],row['Label']]=cache[fn][int(row['Brick'])*24:(int(row['Brick'])+1)*24]
        return result
    def close(a,b,tol=3e-6):
        nonlocal checks
        checks+=1; assert abs(a-b)<=tol,(a,b,tol)
    def oracle(prefix,mode,method='pairwise',metric='pearson',kind='median',pool=False,center=False,tail='one',input_data=None):
        data=input_data or source; out=outputs(prefix)
        provenance=rows(root/(prefix+'.temporal_draws.tsv')); nd=1+max(int(r['Draw']) for r in provenance)
        meta={int(r['Run']):(int(r['StartTR']),int(r['NTR']),int(r['RunSeed'])) for r in provenance}
        offsets={(int(r['Run']),int(r['Draw']),int(r['Subject'][1:])-1):int(r['Offset']) for r in provenance if r['Offset']!='NA'}
        for r,(start,n,seed) in meta.items():
            assert seed==(1234567+1000003*r)%2147483647
            if mode=='timeshift':
                assert all(offsets[r,0,s]==0 for s in range(6))
                assert all(0<offsets[r,d,s]<n for d in range(1,nd) for s in range(6))
        sch=rows(root/(prefix+'.selections.tsv')); sids=sorted({sid for sid,_ in out})
        @lru_cache(None)
        def spectrum(c,sub,v,r):
            start,n,_=meta[r]; values=data[c*6+sub][v][start:start+n]
            return tuple(sum(value*cmath.exp(-2j*math.pi*f*t/n) for t,value in enumerate(values)) for f in range(n))
        @lru_cache(None)
        def full(c,sub,v,r,draw):
            start,n,seed=meta[r]; values=data[c*6+sub][v][start:start+n]
            if not draw: return tuple(values)
            if mode=='timeshift': return tuple(values[(t+offsets[r,draw,sub])%n] for t in range(n))
            spec=list(spectrum(c,sub,v,r)); work=spec.copy()
            for freq in range(1,(n-1)//2+1):
                work[freq]=spec[freq]*factor(seed,draw,sub,freq,(n-1)//2); work[n-freq]=work[freq].conjugate()
            rebuilt=[f32(sum(work[f]*cmath.exp(2j*math.pi*f*t/n) for f in range(n)).real/n) for t in range(n)]
            # Independent reconstruction preserves run mean and energy.
            close(statistics.mean(rebuilt),statistics.mean(values),2e-6)
            close(sum(x*x for x in rebuilt),sum(x*x for x in values),1e-4)
            return tuple(rebuilt)
        def sample(c,sub,v,t,draw):
            r=next(r for r,(start,n,_) in meta.items() if start<=t<start+n)
            return full(c,sub,v,r,draw)[t-meta[r][0]]
        family={}
        for sid in sids:
            for c in range(2):
                selected=[list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1)) for r in sch if r['SelectionID']==sid and r['Subject']=='s01' and r['Condition']==f'C{c+1}']
                assert selected
                for g in range(2):
                    for v in MASK:
                        null=[]
                        for draw in range(nd):
                            blocks=[]
                            for block in selected:
                                series=[[sample(c,sub,v,t,draw) for t in block] for sub in range(g*3,g*3+3)]
                                if center:
                                    series=[[f32(x-f32(statistics.mean(row))) for x in row] for row in series]
                                blocks.append(series)
                            if pool: blocks=[[[x for b in blocks for x in b[sub]] for sub in range(3)]]
                            comps=[]
                            for block in blocks:
                                if method=='pairwise': values=[correlation(block[a],block[b],metric) for a,b in itertools.combinations(range(3),2)]
                                else: values=[correlation(block[a],[f32(statistics.mean(block[b][t] for b in range(3) if b!=a)) for t in range(len(block[0]))],metric) for a in range(3)]
                                comps.append(values)
                            null.append(f32(summary([f32(summary(list(vals),kind)) for vals in zip(*comps)],'median')))
                        family[sid,f'C{c+1}_{"AB"[g]}',v]=null
        maxima=[max(abs(null[d]) if tail=='two' else null[d] for null in family.values()) for d in range(nd)]
        pvals={}
        for key,null in family.items():
            sid,stem,v=key; cmp=lambda x:abs(x) if tail=='two' else x
            p=sum(cmp(x)>=cmp(null[0]) for x in null)/nd; pf=sum(x>=cmp(null[0]) for x in maxima)/nd
            close(out[sid,'TemporalISC_'+stem][v],null[0]); close(out[sid,'TP_'+stem][v],p); close(out[sid,'TPFWE_'+stem][v],pf)
            close(out[sid,'TInferValid_'+stem][v],1,0); assert p>0; pvals[key]=p
        keys=sorted(pvals,key=pvals.get); q=1
        for rank in range(len(keys),0,-1):
            key=keys[rank-1]; q=min(q,pvals[key]*len(keys)/rank); sid,stem,v=key
            close(out[sid,'TQ_'+stem][v],q)
        # Identity estimate agrees with the descriptive bucket for all selections.
        cache={}
        for row in rows(root/(prefix+'.bricks.tsv')):
            if row.get('Kind')=='inference': continue
            label=row.get('Label','')
            if label.startswith('ISC_') and (row['SelectionID'],'TemporalISC_'+label[4:]) in out:
                fn=row['File']
                if fn not in cache: cache[fn]=image(root/fn)[1]
                for v in MASK: close(cache[fn][int(row['Brick'])*24+v],out[row['SelectionID'],'TemporalISC_'+label[4:]][v])
        return out
    base=['-window','12','-step','6','-run_starts','0,24','-nnull','20']
    for mode,method,metric in itertools.product(('timeshift','phase'),('pairwise','loo'),('pearson','spearman')):
        opts=base+['-temporal_null',mode,'-isc_method',method,'-correlation',metric]
        p=run(opts); expected=oracle(p,mode,method,metric)
        q=run(opts,threads=2); assert expected==outputs(q)
        assert (root/(p+'.temporal_draws.tsv')).read_bytes()==(root/(q+'.temporal_draws.tsv')).read_bytes()
    # Odd run lengths, two-sided tails, and whole-input summaries across run joins.
    for mode in ('timeshift','phase'):
        opts=['-window','12','-step','8','-run_starts','0,19','-nnull','20','-temporal_null',mode,'-whole_summary','-temporal_tail','two']
        oracle(run(opts),mode,tail='two')
    # Multi-run block aggregation and explicit pooling/within-segment centering.
    lines=['Subj Group Condition Selection Segment Run StartTR StopTR InputFile']
    for sub in range(6):
        for c in range(2):
            for b,(runid,lo,hi) in enumerate(((1,2,9),(1,14,21),(2,3,14))):
                lines.append(f's{sub+1:02d} {"AB"[sub//3]} C{c+1} combined b{b} {runid} {lo} {hi} s{sub+1:02d}{"_C2" if c else ""}.nii')
    (root/'blocks.txt').write_text('\n'.join(lines)+'\n')
    for mode,method,kind in itertools.product(('timeshift','phase'),('pairwise','loo'),('median','fisher_mean')):
        opts=['-run_starts','0,24','-nnull','20','-temporal_null',mode,'-isc_method',method,'-block_summary',kind]
        oracle(run(opts,table='blocks.txt'),mode,method,kind=kind)
    for mode in ('timeshift','phase'):
        for center in (False,True):
            opts=['-run_starts','0,24','-nnull','20','-temporal_null',mode,'-combine_blocks','pool']+(['-segment_center','mean'] if center else [])
            oracle(run(opts,table='blocks.txt'),mode,pool=True,center=center)
    # Independent parcel mean BOLD oracle.
    nifti(root/'unity.nii',[1]*24)
    parcel=[[[f32(statistics.mean(subject[v][t] for v in MASK)) for t in range(48)] for _ in range(24)] for subject in source]
    for mode in ('timeshift','phase'): oracle(run(base+['-temporal_null',mode,'-atlas','unity.nii','-roi_only']),mode,input_data=parcel)
    # Adding whole_input cannot reduce FWE for any existing temporal test.
    p=run(base+['-temporal_null','timeshift']); q=run(base+['-temporal_null','timeshift','-whole_summary'])
    a,b=outputs(p),outputs(q)
    for key,val in a.items():
        if key[1].startswith('TPFWE_'): assert all(y>=x for x,y in zip(val,b[key]))
    # Other inference families and export coexist, without changing temporal results.
    q=run(base+['-temporal_null','timeshift','-nboot','100','-exact','-condition_contrast','C1','C2','-condition_exact','-save_pairwise','{prefix}_pairs'])
    b=outputs(q)
    for key,val in a.items(): assert val==b[key],key
    # Deterministic seed changes really change the synchronized temporal draws.
    q=run(base+['-temporal_null','timeshift','-seed','42']); assert (root/(p+'.temporal_draws.tsv')).read_bytes()!=(root/(q+'.temporal_draws.tsv')).read_bytes()
    # Header-only validation: censoring outside a window still invalidates its run.
    for opts,error in ((['-nnull','20'],'require'),(['-temporal_null','timeshift'],'requires'),(['-temporal_null','phase','-nnull','20','-min_shift','2'],'only'),(['-temporal_null','timeshift','-nnull','20','-min_shift','13'],'too short'),(['-temporal_null','timeshift','-nnull','20','-missing','common'],'M8a'),(['-temporal_null','phase','-nnull','20','-zcensor'],'M8a')):
        run(['-window','8','-run_starts','0,24','-check_table']+opts,error=error)
    for mode in ('timeshift','phase'):
        run(base+['-temporal_null',mode],table='censor_subjects.txt',error='intact')
        run(base+['-temporal_null',mode,'-censor','common.censor.1D'],error='intact')
    (root/'ones.1D').write_text('1\n'*48)
    q=run(base+['-temporal_null','timeshift','-censor','ones.1D']); assert outputs(q)==a
    run(base+['-temporal_null','timeshift','-atlas','unity.nii','-save_matrix','{prefix}.temporal_draws.tsv'],error='collides')
    run(base+['-temporal_null','phase','-memory_limit','.000001'],error='exceeds limit')
    # Equal relative lags need not produce equal WINDOW correlations: both
    # absolute shifts matter. Ensure this fixture exercises that distinction.
    provenance=rows(root/(p+'.temporal_draws.tsv'))
    shifts={(int(r['Run']),int(r['Draw']),int(r['Subject'][1:])-1):int(r['Offset']) for r in provenance}
    lagseen={}; witnessed=False
    for d in range(1,20):
        a0,b0=shifts[1,d,0],shifts[1,d,1]; lag=(b0-a0)%24
        val=correlation([source[0][0][(t+a0)%24] for t in range(12)],[source[1][0][(t+b0)%24] for t in range(12)],'pearson')
        if lag in lagseen and abs(val-lagseen[lag])>.001: witnessed=True
        lagseen[lag]=val
    assert witnessed
    # Static coverage failures hold the same admitted block set on every draw.
    for mode,policy in itertools.product(('timeshift','phase'),('require_all','drop_common')):
        prefix=run(['-run_starts','0,24','-temporal_null',mode,'-nnull','20','-min_valid','9','-block_missing',policy],table='blocks.txt')
        out=outputs(prefix); sch=rows(root/(prefix+'.selections.tsv'))
        counts={sid:sum(r['SelectionID']==sid and r['Subject']=='s01' and r['Condition']=='C1' for r in sch) for sid,_ in out}
        aggregate=next(sid for sid,n in counts.items() if n==3)
        child=next(r['SelectionID'] for r in sch if counts[r['SelectionID']]==1 and r['OriginalStartTR']=='27')
        for label in ('TInferValid_C1_A','TInferValid_C2_B'):
            assert all(out[aggregate,label][v]==(policy=='drop_common') for v in MASK)
        if policy=='drop_common':
            for (sid,label),val in out.items():
                if sid==aggregate: assert val==out[child,label],label
    # Nonfinite input outside a window but inside its run must fail before
    # output. A nonfinite value in an entirely unselected run is irrelevant.
    manual=['Subj Group Selection Segment Run StartTR StopTR InputFile']
    for sub in range(6):
        manual.append(f's{sub+1:02d} {"AB"[sub//3]} one one 1 0 7 raw{sub}.1D')
    (root/'raw.txt').write_text('\n'.join(manual)+'\n')
    for badtime in (18,30):
        for sub in range(6):
            values=[[str(source[sub][v][t]) for t in range(48)] for v in (0,2)]
            if sub==0: values[0][badtime]='nan'
            (root/f'raw{sub}.1D').write_text('# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "2,1,1"\n# ni_delta = "1,1,1"\n# ni_origin = "0,0,0"\n# ni_axes = "R-L,A-P,I-S"\n# >\n'+'\n'.join(' '.join(row) for row in values)+'\n# </AFNI_3D_dataset>\n')
        run(['-run_starts','0,24'],table='raw.txt',mask=False)
        for mode in ('timeshift','phase'):
            run(['-run_starts','0,24','-temporal_null',mode,'-nnull','20'],table='raw.txt',mask=False,error='nonfinite selected input' if badtime==18 else None)
    report={'executions':count,'numeric_comparisons':checks,'seconds':time.monotonic()-began}
    if args.evidence: Path(args.evidence).write_text(json.dumps(report,indent=2)+'\n')
    print('PASS:',json.dumps(report)); temp.cleanup()
if __name__=='__main__': main()
