#!/usr/bin/env python3
"""Independent subject/bootstrap/swap/shift oracles for pairwise missingness.

Read immutable draw descriptors, recompute masked correlations from input data,
then verify pointwise and synchronized family probabilities. Failed null draws
remain in denominators and family maxima. Bootstrap intervals require all draws.
"""
import argparse, itertools, math, os, statistics, subprocess, tempfile
from functools import lru_cache
from pathlib import Path
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image, f32, correlation
from run_insync_pairwise_missing import table, pearson, weighted, interpolate
from run_insync_window_baseline import summary


def main():
    ap=argparse.ArgumentParser(description=__doc__); ap.add_argument('--program',required=True)
    ap.add_argument('--interp',type=int,default=0)
    args=ap.parse_args(); program=str(Path(args.program).resolve()); runs=checks=0
    missing=['-missing','interp','-interp_maxgap',str(args.interp)] if args.interp else ['-missing','pairwise']
    with tempfile.TemporaryDirectory(prefix='insync-missing-infer-') as temp:
        root=write_fixture(Path(temp)/'fixture'); vox=(0,2,3)
        nifti(root/'small.nii',[int(v in vox) for v in range(24)])
        source=[]; masks=[]
        for c in range(2):
            for sub in range(6):
                data=image(root/f's{sub+1:02d}{"_C2" if c else ""}.nii')[1]
                source.append([[data[t*24+v] for t in range(48)] for v in range(24)])
                mask=[int((t+sub*3+c)%13 not in (0,1)) for t in range(48)]
                masks.append(mask); (root/f'keep{c}_{sub}.1D').write_text('\n'.join(map(str,mask))+'\n')
        lines=['Subj Group Condition Censor InputFile']
        for sub in range(6):
            for c in range(2):lines.append(f's{sub+1:02d} {"AB"[sub//3]} C{c+1} keep{c}_{sub}.1D s{sub+1:02d}{"_C2" if c else ""}.nii')
        (root/'missing.txt').write_text('\n'.join(lines)+'\n')
        blocks=[lines[0]+' Selection Segment Run StartTR StopTR']
        for line in lines[1:]:
            for i,start in enumerate((0,24)):blocks.append(f'{line} joined b{i} 1 {start} {start+11}')
        (root/'blocks_missing.txt').write_text('\n'.join(blocks)+'\n')

        def run(opts,filename='missing.txt',threads=1):
            nonlocal runs
            runs+=1; p=f'r{runs:03d}'
            r=subprocess.run([program,'-quiet',*missing,'-mask','small.nii','-prefix',p,
                *opts,'-dataTableFile',filename],cwd=root,text=True,capture_output=True,
                env=dict(os.environ,OMP_NUM_THREADS=str(threads),AFNI_DECONFLICT='NO'))
            assert r.returncode==0,(r.args,r.stderr)
            assert (root/(p+'.results.tsv')).read_text().endswith('# Status: complete\n')
            return p

        def output(p):
            cache={};out={}
            for row in table(root/(p+'.inference.tsv')):
                fn=row['File']; cache.setdefault(fn,image(root/fn)[1])
                out[row['SelectionID'],row['Label']]=cache[fn][int(row['Brick'])*24:(int(row['Brick'])+1)*24]
            return out

        def close(a,b,context):
            nonlocal checks
            checks+=1
            assert abs(a-b)<4e-6,(context,a,b)

        def check(p,method='pairwise',metric='pearson',kind='median',weight=False,minimum=3,block=False,policy='drop_component',tail='two',taper=False):
            out=output(p); sch=table(root/(p+'.selections.tsv')); selections=sorted({sid for sid,_ in out})
            parts={sid:[list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1)) for r in sch if r['SelectionID']==sid and r['Subject']=='s01' and r['Condition']=='C1'] for sid in selections}
            aggregate={sid:block and any(r['Role']=='block_aggregate_component' for r in sch if r['SelectionID']==sid) for sid in selections}
            dd={name:{} for name in ('bootstrap','group','condition')}
            for row in table(root/(p+'.inference_draws.tsv')):
                dd[row['Kind']].setdefault(int(row['Draw']),[]).append(int(row['Sign']) if row['Kind']=='condition' else int(row['SourceSubject'][1:])-1)
            shifts={}
            if (root/(p+'.temporal_draws.tsv')).exists():
                for r in table(root/(p+'.temporal_draws.tsv')):shifts[int(r['Draw']),int(r['Run'])-1,int(r['Subject'][1:])-1]=int(r['Offset'])
            def summ(vals,weights):return f32(weighted(vals,weights,kind) if weight else summary([x for x in vals if math.isfinite(x)],kind)) if any(math.isfinite(x) for x in vals) else math.nan
            @lru_cache(None)
            def estimate(sid,v,groups,shift=-1):
                series={}; observed={}
                for sub in set(itertools.chain.from_iterable(groups)):
                    values=[]
                    for t in range(48):
                        offset=shifts[shift,t//24,sub%6] if shift>=0 else 0
                        ot=t//24*24+(t%24+offset)%24
                        values.append(source[sub][v][ot] if masks[sub][ot] else math.nan)
                    observed[sub]=list(map(math.isfinite,values))
                    series[sub]=interpolate(values,[0],args.interp) if args.interp and shift<0 else values
                components=[]
                for members in groups:
                    components.append([(a,b) for a,b in itertools.combinations(members,2) if a!=b] if method=='pairwise' else [(a,None) for a in members])
                perblock=[]
                for part in parts[sid] if aggregate[sid] else [sum(parts[sid],[])]:
                    groupvals=[]
                    for members,comps in zip(groups,components):
                        vals=[]
                        for a,b in comps:
                            xs=[];ys=[];real=0;weights=[]
                            for t in part:
                                x=series[a][t]
                                if not math.isfinite(x):continue
                                if b is not None:
                                    y=series[b][t]; genuine=observed[a][t] and observed[b][t]
                                else:
                                    others=[series[sub][t] for sub in members if sub!=a and math.isfinite(series[sub][t])]
                                    if not others:continue
                                    y=f32(statistics.mean(others))
                                    genuine=observed[a][t] and all(observed[sub][t] for sub in members if sub!=a and math.isfinite(series[sub][t]))
                                if math.isfinite(y):
                                    xs.append(x);ys.append(y);real+=genuine
                                    if taper: weights.append(.5*(1-math.cos(2*math.pi*(t-part[0])/(len(part)-1))))
                            r=f32(pearson(xs,ys,metric)) if real>=minimum else math.nan
                            if taper and real>=minimum: r=correlation(xs,ys,metric,weights)
                            vals.append((r,real))
                        groupvals.append(vals)
                    perblock.append(groupvals)
                common=[all(math.isfinite(r) for group in bb for r,n in group) for bb in perblock]
                result=[]
                for gi,comps in enumerate(components):
                    vals=[];weights=[]
                    for ci in range(len(comps)):
                        keep=[bb[gi][ci] for bi,bb in enumerate(perblock) if math.isfinite(bb[gi][ci][0]) and (policy!='drop_common' or common[bi])]
                        if not keep or (policy=='require_all' and len(keep)!=len(perblock)):continue
                        r=keep[0][0] if len(keep)==1 else summ([r for r,n in keep],[n for r,n in keep])
                        vals.append(r); weights.append(sum(n for r,n in keep))
                    result.append(summ(vals,weights))
                return tuple(result)
            families=[{}, {}, {}]
            for sid in selections:
                for v in vox:
                    for c in range(2):
                        if dd['group']:
                            null=[]
                            for perm in dd['group'].values():
                                a,b=estimate(sid,v,(tuple(s+6*c for s in perm[:3]),tuple(s+6*c for s in perm[3:])))
                                null.append(f32(a-b))
                            families[0][sid,f'C{c+1}_A-B',v]=null
                        if dd['bootstrap']:
                            for g in range(2):
                                values=[estimate(sid,v,(tuple(s+6*c for s in draw[g*3:g*3+3]),))[0] for draw in dd['bootstrap'].values()]
                                finite=sorted(x for x in values if math.isfinite(x)); stem=f'C{c+1}_{"AB"[g]}'
                                close(out[sid,'BootN_'+stem][v],len(finite),'bootstrap N')
                                valid=len(finite)==len(values) and len(finite)>=10
                                close(out[sid,'BootValid_'+stem][v],int(valid),'bootstrap valid')
                                if valid:
                                    def quant(q):
                                        at=(len(finite)-1)*q;lo=int(at);return finite[lo]+(at-lo)*(finite[min(lo+1,len(finite)-1)]-finite[lo])
                                    close(out[sid,'BootLo_'+stem][v],quant(.025),'bootstrap low')
                                    close(out[sid,'BootHi_'+stem][v],quant(.975),'bootstrap high')
                        if shifts:
                            for g in range(2):families[2][sid,f'C{c+1}_{"AB"[g]}',v]=[estimate(sid,v,(tuple(s+6*c for s in range(g*3,g*3+3)),),draw)[0] for draw in sorted({key[0] for key in shifts})]
                    if dd['condition']:
                        null=[[],[],[]]
                        for signs in dd['condition'].values():
                            groups=tuple(tuple(s+6*((signs[s]<0)^side) for s in range(g*3,g*3+3)) for side in range(2) for g in range(2))
                            a,b,c,d=estimate(sid,v,groups);effects=[f32(a-c),f32(b-d),f32(f32(a-c)-f32(b-d))]
                            for e in range(3):null[e].append(effects[e])
                        for e,name in enumerate(('A','B','GxCond')):families[1][sid,'C1-C2_'+name,v]=null[e]
            for fi,family in enumerate(families):
                if not family:continue
                two=fi==1 or tail=='two';bi=fi==0 and tail=='bisided'
                nd=len(next(iter(family.values())))
                maxima=[max(math.inf if not math.isfinite(n[d]) else abs(n[d]) if two else n[d] for n in family.values()) for d in range(nd)]
                negmax=[max(math.inf if not math.isfinite(n[d]) else -n[d] for n in family.values()) for d in range(nd)]
                pvals={}
                for (sid,stem,v),null in family.items():
                    obs=null[0];valid=math.isfinite(obs);sign=-1 if bi and obs<0 else 1
                    cmp=lambda x:abs(x) if two else sign*x
                    pv=min(1,(2 if bi else 1)*sum(not math.isfinite(x) or cmp(x)>=cmp(obs) for x in null)/nd) if valid else 1
                    mx=negmax if bi and sign<0 else maxima
                    pf=min(1,(2 if bi else 1)*sum(x>=cmp(obs) for x in mx)/nd) if valid else 1
                    pre=('','C','T')[fi];label=('GroupDiff','Cond','TemporalISC')[fi]
                    close(out[sid,pre+'InferValid_'+stem][v],int(valid),'valid')
                    if valid:close(out[sid,label+'_'+stem][v],obs,'observed')
                    close(out[sid,pre+'P_'+stem][v],pv,('P',fi,sid,stem,v))
                    close(out[sid,pre+'PFWE_'+stem][v],pf,('PFWE',fi,sid,stem,v))
                    if fi==2:close(out[sid,'TNullN_'+stem][v],sum(map(math.isfinite,null)),'finite shift draws')
                    pvals[sid,stem,v]=pv
                ordered=sorted(pvals,key=pvals.get);q=1
                for rank in range(len(ordered),0,-1):
                    key=ordered[rank-1];q=min(q,pvals[key]*len(ordered)/rank);sid,stem,v=key
                    close(out[sid,('','C','T')[fi]+'Q_'+stem][v],q,'Q')
            return out

        base=['-window','12','-step','24','-exact','-nboot','100','-condition_contrast','C1','C2','-condition_exact']
        for method in ('pairwise','loo'):
            opts=base+['-window_taper','hann','-isc_method',method]
            p=run(opts); a=check(p,method,taper=True)
            assert a==output(run(opts,threads=2))
            opts=['-window','12','-step','24','-run_starts','0,24','-temporal_null','timeshift',
                  '-temporal_censor','joint_shift','-nnull','40','-temporal_tail','two',
                  '-min_shift','1','-isc_method',method,'-window_taper','hann']
            check(run(opts),method,taper=True)
        for method,metric in itertools.product(('pairwise','loo'),('pearson','spearman')):
            opts=base+['-isc_method',method,'-correlation',metric]
            p=run(opts);a=check(p,method,metric)
            q=run(opts,threads=2);assert a==output(q)
        for method,policy in itertools.product(('pairwise','loo'),('require_all','drop_common','drop_component')):
            opts=base[4:]+['-isc_method',method,'-summary','fisher_mean','-summary_weight','valid','-block_summary','fisher_mean','-block_weight','valid','-block_missing',policy]
            check(run(opts,'blocks_missing.txt'),method,kind='fisher_mean',weight=True,block=True,policy=policy)
        for minimum in (3,11):
            for method in ('pairwise','loo'):
                opts=['-window','12','-step','24','-run_starts','0,24','-temporal_null','timeshift','-temporal_censor','joint_shift','-nnull','40','-temporal_tail','two','-min_shift','1','-min_valid',str(minimum),'-isc_method',method]
                p=run(opts);a=check(p,method,minimum=minimum)
                assert a==output(run(opts,threads=2))
            check(run(base+['-min_valid',str(minimum)]),minimum=minimum)
        for tail in ('one','bisided'):check(run(base+['-tail',tail]),tail=tail)
        # Six-person bootstrap exercises actual CI endpoints, rather than only
        # small-group failure flags. Reference exclusion is by original identity.
        (root/'onegroup.txt').write_text((root/'missing.txt').read_text().replace(' A ',' All ').replace(' B ',' All '))
        for method in ('pairwise','loo'):
            p=run(['-nboot','100','-isc_method',method],filename='onegroup.txt');out=output(p);draws={}
            for row in table(root/(p+'.inference_draws.tsv')):
                draws.setdefault(int(row['Draw']),[]).append(int(row['SourceSubject'][1:])-1)
            nci=0
            for c in range(2):
                for v in vox:
                    results=[]
                    for members in draws.values():
                        rs=[]
                        series={sub:interpolate([source[c*6+sub][v][t] if masks[c*6+sub][t] else math.nan for t in range(48)],[0],args.interp) if args.interp else [source[c*6+sub][v][t] if masks[c*6+sub][t] else math.nan for t in range(48)] for sub in set(members)}
                        comp=itertools.combinations(members,2) if method=='pairwise' else [(a,None) for a in members]
                        for a,b in comp:
                            if a==b:continue
                            xs=[];ys=[];real=0
                            for t in range(48):
                                if not math.isfinite(series[a][t]):continue
                                if b is not None:
                                    if not math.isfinite(series[b][t]):continue
                                    y=series[b][t]; genuine=masks[c*6+a][t] and masks[c*6+b][t]
                                else:
                                    others=[series[s][t] for s in members if s!=a and math.isfinite(series[s][t])]
                                    if not others:continue
                                    y=f32(statistics.mean(others))
                                genuine=masks[c*6+a][t] and all(masks[c*6+s][t] for s in members if s!=a and math.isfinite(series[s][t])) if b is None else genuine
                                xs.append(series[a][t]);ys.append(y);real+=genuine
                            r=f32(pearson(xs,ys,'pearson')) if real>=3 else math.nan
                            if math.isfinite(r):rs.append(r)
                        results.append(f32(statistics.median(rs)) if rs else math.nan)
                    finite=sorted(r for r in results if math.isfinite(r));stem=f'C{c+1}_All'
                    close(out['sel000001','BootN_'+stem][v],len(finite),'six-subject BootN')
                    valid=len(finite)==100
                    close(out['sel000001','BootValid_'+stem][v],int(valid),'six-subject BootValid')
                    if valid:
                        nci+=1
                        for label,q in (('BootLo',.025),('BootHi',.975)):
                            at=99*q;lo=int(at);expected=finite[lo]+(at-lo)*(finite[lo+1]-finite[lo])
                            close(out['sel000001',label+'_'+stem][v],expected,'six-subject CI')
            assert nci>0,'CI endpoint test needs valid bootstrap intervals'

    print(f'PASS: {runs} missing-inference executions; {checks} independent comparisons')

if __name__=='__main__':main()
