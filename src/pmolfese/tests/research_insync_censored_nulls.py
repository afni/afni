#!/usr/bin/env python3
"""M8a research calibration; independent NumPy reference, no production enablement.

Circular AR(1) nulls, random/clustered/group-unequal/informative masks, artifacts
only in censored frames. Each repetition is the independent simulation unit.
"""
import argparse,json,time,warnings
from pathlib import Path
import numpy as np


def wilson(k,n):
    if not n: return None
    z=1.96; p=k/n; den=1+z*z/n
    center=(p+z*z/(2*n))/den; half=z*np.sqrt(p*(1-p)/n+z*z/(4*n*n))/den
    return [float(center-half),float(center+half)]


def statistic(data,keep,min_n=8,method='pairwise'):
    # D,S,V,T. One common support for both groups; finite-component contract.
    n=keep.sum(axis=-1); good=n>=min_n
    mean=np.sum(np.where(keep[:,None,None,:],data,0),axis=-1)/np.maximum(n[:,None,None],1)
    centered=np.where(keep[:,None,None,:],data-mean[:,:,:,None],0)
    result=[]
    for g in range(2):
        group=centered[:,g*3:g*3+3]
        if method=='pairwise':
            a=group[:,[0,0,1]]; b=group[:,[1,2,2]]
        else:
            a=group; b=(group.sum(axis=1,keepdims=True)-group)/2
        denom=np.sqrt(np.sum(a*a,axis=-1)*np.sum(b*b,axis=-1))
        with np.errstate(divide='ignore',invalid='ignore'):
            corr=np.sum(a*b,axis=-1)/denom
        corr=np.where(good[:,None,None] & (denom>0),corr,np.nan)
        result.append(corr)
    return np.stack(result,axis=1),n


def evaluate(signal,masks,offsets,policy,min_n=8,method='pairwise',invalid='complete'):
    nd,ns,nr=offsets.shape; nt=signal.shape[-1]; nv=signal.shape[-2]
    moved=np.empty((nd,ns,nv,nr*nt)); moved_masks=np.empty((nd,ns,nr*nt),bool)
    for r in range(nr):
        ix=(np.arange(nt)[None,None,:]+offsets[:,:,r,None])%nt
        for s in range(ns):
            moved[:,s,:,r*nt:(r+1)*nt]=signal[s,r][:,ix[:,s]].transpose(1,0,2)
            moved_masks[:,s,r*nt:(r+1)*nt]=masks[s,r][ix[:,s]]
    common=(moved_masks if policy=='joint' else np.broadcast_to(masks.reshape(ns,-1),(nd,ns,nr*nt))).all(axis=1)
    # Four disjoint blocks plus overlapping windows: joint max over all cells.
    components=[]; retained=[]; cells=[]
    for r in range(nr):
        for start in (0,nt//4,nt//2):
            a=r*nt+start; b=a+nt//2
            comp,n=statistic(moved[:,:,:,a:b],common[:,a:b],min_n,method)
            with warnings.catch_warnings():
                warnings.simplefilter('ignore',RuntimeWarning); value=np.median(comp,axis=2)
            cells.append(value)
            if start!=nt//4: components.append(comp); retained.append(n)
    components=np.stack(components,axis=-1); retained=np.stack(retained,axis=-1)
    aggregate=np.median(components,axis=-1)
    cells.append(np.median(aggregate,axis=2))
    values=np.stack(cells,axis=1).reshape(nd,-1)
    valid=np.isfinite(values).all(axis=0) if invalid=='complete' else np.isfinite(values[0]); out={}
    if np.any(valid):
        null=values[:,valid].copy(); obs=null[0].copy()
        if invalid=='bound': null[~np.isfinite(null)]=np.inf
        p=np.mean(null>=obs,axis=0)
        maxima=np.where(np.isfinite(values),values,np.inf).max(axis=1) if invalid=='bound' else null.max(axis=1)
        pf=np.mean(maxima[:,None]>=obs,axis=0)
        if invalid=='bound':
            allp=np.ones(len(valid));allp[valid]=p;p=allp
        order=np.argsort(p); q=np.empty(len(p)); q[order]=np.minimum.accumulate((p[order]*len(p)/np.arange(1,len(p)+1))[::-1])[::-1].clip(0,1)
        out=dict(fwe=bool(np.any(pf<=.05)),bh=bool(np.any(q<=.05)),point=bool(p[0]<=.05) if valid[0] else None)
    return values,valid,out,retained.reshape(-1)


def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--repetitions',type=int,default=500);p.add_argument('--draws',type=int,default=100);p.add_argument('--output',required=True);p.add_argument('--seed',type=int,default=20261105);p.add_argument('--rho',type=float,default=.6);p.add_argument('--min-shift',type=int,default=1);p.add_argument('--invalid',choices=['complete','bound'],default='complete');p.add_argument('--scenarios',default='random,clustered,unequal,informative,excluded_artifacts')
    args=p.parse_args()
    if args.repetitions<1 or args.draws<20 or not 0<=args.rho<1 or not 1<=args.min_shift<=47:p.error('require positive repetitions, draws>=20, 0<=rho<1 and 1<=min-shift<=47')
    if set(args.scenarios.split(','))-{'random','clustered','unequal','informative','excluded_artifacts'}:p.error('unknown scenario')
    began=time.monotonic(); rng=np.random.default_rng(args.seed); records=[]
    for scenario in args.scenarios.split(','):
      for method in ('pairwise','loo'):
        stats={k:dict(trials=0,point_trials=0,fwe=0,bh=0,point=0,valid_cells=0,all_cells=0,retained_min=9999,artifact_invariance_max=0.,support_histogram=[0]*49) for k in ('fixed','joint')}
        for rep in range(args.repetitions):
            ns,nr,nv,nt=6,2,2,96
            # Circular stationary AR spectrum: no artificial nonperiodic join.
            white=rng.normal(size=(ns,nr,nv,nt)); rho=args.rho
            transfer=1/(1-rho*np.exp(-2j*np.pi*np.fft.rfftfreq(nt)))
            signal=np.fft.irfft(np.fft.rfft(white,axis=-1)*transfer,n=nt,axis=-1)
            if scenario=='clustered':
                mask=np.ones((ns,nr,nt),bool)
                for s in range(ns):
                    for r in range(nr):
                        start=rng.integers(nt);mask[s,r,(start+np.arange(12))%nt]=False
            elif scenario=='unequal': mask=rng.random((ns,nr,nt))>np.array([.03]*3+[.2]*3)[:,None,None]
            elif scenario=='informative':
                score=np.abs(signal[:,:,0]);mask=score<np.quantile(score,.88,axis=-1,keepdims=True)
            else: mask=rng.random((ns,nr,nt))>.1
            offsets=rng.integers(args.min_shift,nt-args.min_shift+1,size=(args.draws,ns,nr));offsets[0]=0
            contaminated=signal.copy()
            if scenario=='excluded_artifacts':
                artifact=50*np.sin(np.arange(nt)*.21)+rng.normal(0,20,size=(ns,nr,nt))
                contaminated=np.where(mask[:,:,None,:],signal,artifact[:,:,None,:])
            for policy in stats:
                values,valid,result,retained=evaluate(contaminated,mask,offsets,policy,method=method,invalid=args.invalid)
                rec=stats[policy];rec['valid_cells']+=int(valid.sum());rec['all_cells']+=len(valid);rec['retained_min']=min(rec['retained_min'],int(retained.min()));rec['support_histogram']=(np.array(rec['support_histogram'])+np.bincount(retained,minlength=49)).tolist()
                if result:
                    rec['trials']+=1;rec['fwe']+=result['fwe'];rec['bh']+=result['bh']
                    if result['point'] is not None:rec['point_trials']+=1;rec['point']+=result['point']
                if scenario=='excluded_artifacts' and rep<5:
                    clean=evaluate(signal,mask,offsets,policy,method=method,invalid=args.invalid)[0]
                    rec['artifact_invariance_max']=max(rec['artifact_invariance_max'],float(np.nanmax(np.abs(clean-values))))
        for policy,rec in stats.items():
            rec.update(scenario=scenario,method=method,policy=policy,repetitions=args.repetitions,draws=args.draws,invalid=args.invalid,rho=args.rho,min_shift=args.min_shift,support_nominal_n=48,support_blocks=4)
            for label in ('fwe','bh','point'):
                n=rec['point_trials'] if label=='point' else rec['trials'];rec[label+'_rate']=rec[label]/n if n else None;rec[label+'_wilson95']=wilson(rec[label],n)
            records.append(rec);print(json.dumps(rec),flush=True)
    Path(args.output).write_text(json.dumps(dict(seed=args.seed,seconds=time.monotonic()-began,records=records),indent=2)+'\n')
if __name__=='__main__':main()
