#!/usr/bin/env python3
"""M8a black-box joint signal/mask shifts against a scalar original-time oracle."""
import argparse,itertools,math,os,statistics,subprocess,tempfile
from pathlib import Path
from insync_window_fixtures import write_fixture,nifti
from run_insync_estimation import image,f32,correlation
from run_insync_inference import rows
from run_insync_window_baseline import summary

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--program',required=True);p.set_defaults(bounds=True);args=p.parse_args();program=str(Path(args.program).resolve()); count=0;checks=0
    with tempfile.TemporaryDirectory(prefix='insync-m8a-') as tmp:
        root=write_fixture(Path(tmp)/'fixture'); nifti(root/'small.nii',[int(v in (0,2)) for v in range(24)])
        source=[];masks=[]
        for c in ('','_C2'):
            for sub in range(6):
                flat=image(root/f's{sub+1:02d}{c}.nii')[1];source.append([[flat[t*24+v] for t in range(48)] for v in range(24)])
                masks.append([int(x) for x in (root/f's{sub+1:02d}.censor.1D').read_text().split()])
        lines=['Subj Group Condition Censor InputFile']
        for sub in range(6):
            for c in range(2): lines.append(f's{sub+1:02d} {"AB"[sub//3]} C{c+1} s{sub+1:02d}.censor.1D s{sub+1:02d}{"_C2" if c else ""}.nii')
        (root/'censored.txt').write_text('\n'.join(lines)+'\n')
        def run(opts=(),table='censored.txt',threads=1,error=None,mask=True):
            nonlocal count
            count+=1; prefix=f'a{count:03d}'
            cmd=[program,'-quiet','-prefix',prefix]+(['-mask','small.nii'] if mask else [])+['-run_starts','0,24','-temporal_null','timeshift','-temporal_censor','joint_shift','-nnull','20']+[str(x).replace('{prefix}',prefix) for x in opts]+['-dataTableFile',table]
            r=subprocess.run(cmd,cwd=root,env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_DECONFLICT='NO'),capture_output=True,text=True)
            if error:
                assert r.returncode and error.lower() in r.stderr.lower(),(cmd,r.stderr,error)
                return prefix
            assert not r.returncode,(cmd,r.stderr)
            assert (root/(prefix+'.results.tsv')).read_text().endswith('# Status: complete\n')
            return prefix
        def outputs(prefix):
            out={}; cache={}
            for row in rows(root/(prefix+'.inference.tsv')):
                fn=row['File']
                if fn not in cache:cache[fn]=image(root/fn)[1]
                out[row['SelectionID'],row['Label']]=cache[fn][int(row['Brick'])*24:(int(row['Brick'])+1)*24]
            return out
        def close(a,b):
            nonlocal checks
            checks+=1; assert abs(a-b)<3e-6,(a,b)
        def oracle(prefix,method='pairwise',metric='pearson',kind='median',pool=False,center=False,drop=False,min_n=3,min_frac=0,min_blocks=1):
            out=outputs(prefix);sch=rows(root/(prefix+'.selections.tsv'));prov=rows(root/(prefix+'.temporal_draws.tsv'))
            offsets={(int(r['Run'])-1,int(r['Draw']),int(r['Subject'][1:])-1):int(r['Offset']) for r in prov}
            common=[]
            for d in range(20):
                common.append([all(masks[s][(t//24)*24+(t%24+offsets[t//24,d,s%6])%24] for s in range(12)) for t in range(48)])
            for row in rows(root/(prefix+'.temporal_support.tsv')):
                lo,hi=int(row['StartTR']),int(row['StopTR']);n=sum(common[int(row['Draw'])][lo:hi+1]);close(int(row['RetainedN']),n);close(float(row['RetainedFrac']),n/(hi-lo+1))
            def components(series,nominal):
                if len(series[0])<min_n or len(series[0])/nominal<min_frac: return [math.nan]*3
                if method=='pairwise':return [correlation(series[a],series[b],metric) for a,b in itertools.combinations(range(3),2)]
                return [correlation(series[a],[f32(statistics.mean(series[b][t] for b in range(3) if b!=a)) for t in range(len(series[0]))],metric) for a in range(3)]
            family={};all_nulls={};pvalues={}
            for sid in sorted({sid for sid,_ in out}):
                for c in range(2):
                    segments=[range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1) for r in sch if r['SelectionID']==sid and r['Subject']=='s01' and r['Condition']==f'C{c+1}']
                    for v in (0,2):
                        null=[[],[]]
                        for d in range(20):
                            blocks=[]
                            for segment in segments:
                                good=[t for t in segment if common[d][t]]
                                series=[[source[c*6+s][v][t//24*24+(t%24+offsets[t//24,d,s])%24] for t in good] for s in range(6)]
                                if center and good:series=[[f32(x-f32(statistics.mean(row))) for x in row] for row in series]
                                blocks.append(series)
                            if pool:blocks=[[[x for block in blocks for x in block[s]] for s in range(6)]]
                            nominal=[sum(map(len,segments))] if pool else list(map(len,segments))
                            comps=[[components(block[:3],n),components(block[3:],n)] for block,n in zip(blocks,nominal)]
                            good=[b for b in comps if all(math.isfinite(x) for group in b for x in group)]
                            if not good or (not drop and len(good)!=len(comps)) or (len(comps)>1 and len(good)<min_blocks):
                                for g in range(2):null[g].append(math.nan)
                            else:
                                for g in range(2):null[g].append(f32(summary([f32(summary([b[g][edge] for b in good],kind)) for edge in range(3)],'median')))
                        for g in range(2):
                            stem=f'C{c+1}_{"AB"[g]}';values=null[g];all_nulls[sid,stem,v]=[x if math.isfinite(x) else math.inf for x in values];finite=sum(map(math.isfinite,values));close(out[sid,'TNullN_'+stem][v],finite)
                            valid=math.isfinite(values[0]) and (args.bounds or finite==20)
                            close(out[sid,'TInferValid_'+stem][v],valid)
                            if valid:family[sid,stem,v]=[x if math.isfinite(x) else math.inf for x in values]
                            else:
                                if args.bounds:pvalues[sid,stem,v]=1
                                for label in ('TP','TPFWE','TQ'):close(out[sid,label+'_'+stem][v],1)
            maxima=[max(x[d] for x in (all_nulls if args.bounds else family).values()) for d in range(20)]
            for key,values in family.items():
                sid,stem,v=key;p=sum(x>=values[0] for x in values)/20;pf=sum(x>=values[0] for x in maxima)/20
                close(out[sid,'TemporalISC_'+stem][v],values[0]);close(out[sid,'TP_'+stem][v],p);close(out[sid,'TPFWE_'+stem][v],pf);pvalues[key]=p
            ordered=sorted(pvalues,key=pvalues.get);q=1
            for rank in range(len(ordered),0,-1):
                key=ordered[rank-1];q=min(q,pvalues[key]*len(ordered)/rank);sid,stem,v=key;close(out[sid,'TQ_'+stem][v],q)
            return out
        for method,metric in itertools.product(('pairwise','loo'),('pearson','spearman')):
            opts=['-window','12','-step','6','-isc_method',method,'-correlation',metric]
            a=run(opts);expected=oracle(a,method,metric);b=run(opts,threads=2);assert expected==outputs(b)
            assert (root/(a+'.temporal_support.tsv')).read_bytes()==(root/(b+'.temporal_support.tsv')).read_bytes()
        # Manual multi-run blocks, including lengths that fail min_valid on
        # some draws: support and admitted blocks must change with the masks.
        table=['Subj Group Condition Censor Selection Segment Run StartTR StopTR InputFile']
        for sub in range(6):
            for c in range(2):
                for b,(r,lo,hi) in enumerate(((1,0,7),(1,12,23),(2,4,19))):table.append(f's{sub+1:02d} {"AB"[sub//3]} C{c+1} s{sub+1:02d}.censor.1D combined b{b} {r} {lo} {hi} s{sub+1:02d}{"_C2" if c else ""}.nii')
        (root/'blocks.txt').write_text('\n'.join(table)+'\n')
        for method,kind,drop in itertools.product(('pairwise','loo'),('median','fisher_mean'),(False,True)):
            opts=['-isc_method',method,'-block_summary',kind,'-block_missing','drop_common' if drop else 'require_all','-min_valid','7']
            oracle(run(opts,table='blocks.txt'),method,kind=kind,drop=drop,min_n=7)
        oracle(run(['-window','12','-step','6','-min_valid_frac','.7']),min_frac=.7)
        oracle(run(['-block_missing','drop_common','-min_valid','7','-min_blocks','2'],table='blocks.txt'),drop=True,min_n=7,min_blocks=2)
        for center in (False,True):oracle(run(['-combine_blocks','pool']+(['-segment_center','mean'] if center else []),table='blocks.txt'),pool=True,center=center)
        # Whole-input summary, no window option, and combined M7 inference.
        oracle(run(['-whole_summary','-nboot','100','-exact','-condition_contrast','C1','C2','-condition_exact']))
        # Atlas mean-BOLD path agrees with a scalar mean of its masked voxels.
        original=source
        source=[[[f32(statistics.mean(subject[v][t] for v in (0,2))) for t in range(48)] for _ in range(24)] for subject in original]
        nifti(root/'unity.nii',[1]*24)
        oracle(run(['-window','12','-step','6','-atlas','unity.nii','-roi_only']))
        source=original
        # With intact masks, the new policy reproduces the existing temporal
        # statistics exactly; the extra brick reports all 20 usable draws.
        intact=outputs(run(['-window','12','-step','6'],table='conditions.txt'))
        subprocess.run([program,'-quiet','-mask','small.nii','-prefix','intact_old','-window','12','-step','6','-run_starts','0,24','-temporal_null','timeshift','-nnull','20','-dataTableFile','conditions.txt'],cwd=root,env=dict(os.environ,OMP_NUM_THREADS='1'),check=True,capture_output=True)
        count+=1
        for key,val in outputs('intact_old').items(): assert intact[key]==val,key
        baseline=run(['-window','12','-step','6']);base=outputs(baseline)
        # Excluded artifacts cannot change any temporal statistic or p/q.
        for c in range(2):
            for sub in range(6):
                data=source[c*6+sub];mask=masks[c*6+sub]
                nifti(root/f's{sub+1:02d}{"_C2" if c else ""}.nii',[data[v][t] if mask[t] else 1e20*(sub+1) for t in range(48) for v in range(24)],48)
        assert outputs(run(['-window','12','-step','6']))==base
        # Censored nonfinite samples are permitted; own-retained samples must
        # be finite even outside the original common mask / requested window.
        raw=['Subj Group Censor InputFile']
        for sub in range(6):
            raw.append(f's{sub+1:02d} {"AB"[sub//3]} s{sub+1:02d}.censor.1D raw{sub}.1D')
            lines=[' '.join(str(source[sub][v][t]) if masks[sub][t] else 'nan' for t in range(48)) for v in (0,2)]
            (root/f'raw{sub}.1D').write_text('# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "2,1,1"\n# ni_delta = "1,1,1"\n# ni_origin = "0,0,0"\n# ni_axes = "R-L,A-P,I-S"\n# >\n'+'\n'.join(lines)+'\n# </AFNI_3D_dataset>\n')
        (root/'raw.txt').write_text('\n'.join(raw)+'\n');run(['-window','12'],table='raw.txt',mask=False)
        text=(root/'raw0.1D').read_text().replace(str(source[0][0][5]),'nan');(root/'raw0.1D').write_text(text)
        run(['-window','12'],table='raw.txt',mask=False,error='nonfinite selected input')
        for opts,error in ((['-temporal_null','phase'],'requires'),(['-float16'],'float32'),(['-missing','common'],'intact'),(['-zcensor'],'intact'),(['-memory_limit','.000001'],'exceeds limit')):run(['-window','12']+opts,error=error)
        nifti(root/'unity.nii',[1]*24)
        run(['-window','12','-atlas','unity.nii','-save_matrix','{prefix}.temporal_support.tsv'],error='collides')
        print(f'PASS: {count} M8a executions; {checks} scalar comparisons')
if __name__=='__main__':main()
