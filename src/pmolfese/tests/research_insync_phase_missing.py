#!/usr/bin/env python3
"""Research gate for censored phase inference; NOT a production null method.

Requires NumPy (research dependency only). Reproducible circular Gaussian
signals give an exact complete-series phase control. Masked true-series phase
is an unavailable-data oracle: a diagnostic of the simulation, not a usable
method. Candidate spectra instead come from bounded linear completions.

No extrapolation or long-gap filling. Report ineligible completions rather than
silently replacing their data. Pointwise and joint voxel/window maxima include
identity; failed null cells contribute infinity. Simulations cannot prove
validity beyond the modeled data/missingness. See the upgrade plan for decisions.
"""
import argparse, itertools, json, math
from pathlib import Path
import numpy as np


def wilson(k,n):
    if not n:return [None,None]
    z=1.96;p=k/n;den=1+z*z/n;mid=(p+z*z/(2*n))/den
    half=z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/den
    return [mid-half,mid+half]


def completion(values, keep, maxgap=2):
    """Original-time clean anchors; values ends in [voxel, subject, time].

    keep is [subject,time] and broadcasts over draws/voxels. Never use filled
    points as anchors. Return NaN for any unfillable sample.
    """
    result=np.where(keep,values,np.nan).copy()
    for sub in range(keep.shape[0]):
        anchors=np.flatnonzero(keep[sub])
        for a,b in zip(anchors[:-1],anchors[1:]):
            if b-a-1>maxgap:continue
            for t in range(a+1,b):
                result[...,sub,t]=((b-t)*values[...,sub,a]+(t-a)*values[...,sub,b])/(b-a)
    return result


def corr(a,b,real):
    """Overlap-specific Pearson; gate on real count, estimate on usable values."""
    good=np.isfinite(a)&np.isfinite(b);n=good.sum(-1)
    x=np.where(good,a,0);y=np.where(good,b,0)
    mx=x.sum(-1)/np.maximum(n,1);my=y.sum(-1)/np.maximum(n,1)
    x=np.where(good,a-mx[...,None],0);y=np.where(good,b-my[...,None],0)
    den=np.sqrt((x*x).sum(-1)*(y*y).sum(-1))
    with np.errstate(invalid='ignore',divide='ignore'):
        r=(x*y).sum(-1)/den
    return np.where((real>=8)&(den>0),np.clip(r,-1,1),np.nan)


def estimate(values, observed, method):
    """Median ISC for full run and its two halves, [draw, voxel, selection]."""
    nt=values.shape[-1];ns=values.shape[-2];out=[]
    for start,stop in ((0,nt),(0,nt//2),(nt//2,nt)):
        x=values[...,start:stop];real=observed[...,start:stop];rs=[]
        if method=='pairwise':
            for a,b in itertools.combinations(range(ns),2):
                nr=(real[...,a,:]&real[...,b,:]&np.isfinite(x[...,a,:])&np.isfinite(x[...,b,:])).sum(-1)
                rs.append(corr(x[...,a,:],x[...,b,:],nr))
        else:
            for a in range(ns):
                others=[b for b in range(ns) if b!=a]
                xx=x[...,others,:];valid=np.isfinite(xx);n=valid.sum(-2)
                mean=np.where(valid,xx,0).sum(-2)/np.maximum(n,1)
                mean=np.where(n>0,mean,np.nan)
                nr=(real[...,a,:]&np.isfinite(x[...,a,:])&(n>0)&np.all(~valid|real[...,others,:],axis=-2)).sum(-1)
                rs.append(corr(x[...,a,:],mean,nr))
        # Explicit all-invalid handling without emitting an expected warning.
        matrix=np.stack(rs,axis=-1);count=np.isfinite(matrix).sum(-1)
        ordered=np.sort(np.where(np.isfinite(matrix),matrix,np.inf),axis=-1)
        lo=np.maximum((count-1)//2,0);hi=np.maximum(count//2,0)
        median=(np.take_along_axis(ordered,lo[...,None],-1)[...,0]+np.take_along_axis(ordered,hi[...,None],-1)[...,0])/2
        out.append(np.where(count,median,np.nan))
    return np.stack(out,axis=-1)


def phase(data, factors):
    """Independent subject phases, shared across voxels; DC/Nyquist unchanged."""
    return np.fft.irfft(np.fft.rfft(data)[None,...]*factors,n=data.shape[-1],axis=-1)


def signal(rng,nt,phi,shared):
    white=(rng.normal(size=(3,6,nt))+rng.normal(size=(1,6,nt)))/math.sqrt(2)
    freq=np.arange(nt//2+1)
    filt=1/(1-np.asarray(phi)[...,None]*np.exp(-2j*np.pi*freq/nt))
    x=np.fft.irfft(np.fft.rfft(white)*filt,n=nt)
    x/=np.std(x,axis=-1,keepdims=True)
    if shared:
        common=rng.normal(size=(3,1,nt))
        common=np.fft.irfft(np.fft.rfft(common)/(1-.8*np.exp(-2j*np.pi*freq/nt)),n=nt)
        common/=np.std(common,axis=-1,keepdims=True)
        x+=shared*common
    return x


def masks(rng,x,scenario,rate):
    ns,nt=x.shape[-2:];keep=np.ones((ns,nt),bool)
    if scenario=='complete':return keep
    if scenario=='edge':keep[:,0]=False;return keep
    if scenario=='long_gap':keep[:,nt//2:nt//2+5]=False;return keep
    if scenario=='localized':
        keep[:,1:nt//2]=np.arange(1,nt//2)%3==0
        return keep
    if scenario=='periodic':
        keep[:,1:-1]=np.arange(1,nt-1)%3==0
        return keep
    if scenario=='informative':
        task=np.sin(2*np.pi*np.arange(nt)/24)
        # Only isolated bad samples, so every gap is eligible for K=2.
        keep[:,1:-1:3]=x[0,:,1:-1:3]*task[1:-1:3]>0
        return keep
    for sub in range(ns):
        for start in rng.permutation(np.arange(1,nt-2)):
            if (~keep[sub]).sum()>=int(rate*nt):break
            if keep[sub,start-1:start+3].all():keep[sub,start:start+2]=False
    if scenario=='shared_mask':keep[:]=keep[0]
    return keep


def selfcheck():
    """Independent scalar checks of packing, overlap gates, LOO and Fourier invariants."""
    import statistics
    from run_insync_pairwise_missing import pearson
    rng=np.random.default_rng(17);x=rng.normal(size=(2,3,6,48))
    keep=np.ones((6,48),bool);keep[:,4:6]=False;keep[0,24:29]=False
    values=completion(x,keep);observed=keep[None,None,:,:]
    for method in ('pairwise','loo'):
        actual=estimate(values,observed,method)
        for d,v in itertools.product(range(2),range(3)):
            for wi,(start,stop) in enumerate(((0,48),(0,24),(24,48))):
                rs=[]
                for a,b in (itertools.combinations(range(6),2) if method=='pairwise' else ((a,None) for a in range(6))):
                    xx=[];yy=[];real=0
                    for t in range(start,stop):
                        held=values[d,v,a,t]
                        if not math.isfinite(held):continue
                        if b is not None:ref=values[d,v,b,t];genuine=keep[a,t] and keep[b,t]
                        else:
                            others=[s for s in range(6) if s!=a and math.isfinite(values[d,v,s,t])]
                            if not others:continue
                            ref=statistics.mean(values[d,v,s,t] for s in others)
                            genuine=keep[a,t] and all(keep[s,t] for s in others)
                        if not math.isfinite(ref):continue
                        xx.append(held);yy.append(ref);real+=genuine
                    if real>=8:rs.append(pearson(xx,yy,'pearson'))
                expected=statistics.median(rs) if rs else math.nan
                assert (math.isnan(expected) and np.isnan(actual[d,v,wi])) or abs(expected-actual[d,v,wi])<1e-12
    # Exact endpoints/long gaps, then preserved Fourier magnitudes and identity.
    assert np.isnan(values[...,0,24:29]).all()
    assert np.allclose(values[...,4],(2*x[...,3]+x[...,6])/3)
    factors=np.exp(1j*rng.normal(size=(5,1,6,25)));factors[...,0]=factors[...,-1]=1;factors[0]=1
    surrogate=phase(x[0],factors)
    assert np.allclose(surrogate[0],x[0])
    assert np.allclose(abs(np.fft.rfft(surrogate)),abs(np.fft.rfft(x[0]))[None])


CONFIG={
    'complete':(96,.8,0,'complete'),
    'white_30':(96,0.,.3,'random'),
    'red_30':(96,.8,.3,'random'),
    'shared_white_50':(96,0.,.5,'shared_mask'),
    'shared_red_50':(96,.8,.5,'shared_mask'),
    'short_red_50':(48,.9,.5,'random'),
    'long_red_30':(192,.8,.3,'random'),
    'heterogeneous':(96,np.linspace(.1,.9,6)[None,:],.3,'random'),
    'informative':(96,.8,0,'informative'),
    'edge':(96,.8,0,'edge'),
    'long_gap':(96,.8,0,'long_gap'),
    'periodic_white':(96,0.,0,'periodic'),
    'periodic_red':(96,.8,0,'periodic'),
    'localized_white':(96,0.,0,'localized'),
    'localized_red':(96,.8,0,'localized'),
}


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--replicates',type=int,default=200)
    ap.add_argument('--draws',type=int,default=200,help='includes identity')
    ap.add_argument('--seed',type=int,default=20261120)
    ap.add_argument('--scenarios',nargs='+',choices=CONFIG,default=list(CONFIG))
    ap.add_argument('--method',choices=('pairwise','loo'),default='pairwise')
    ap.add_argument('--shared',type=float,default=0,help='positive value is a power experiment, not null calibration')
    ap.add_argument('--output',type=Path,required=True)
    args=ap.parse_args()
    selfcheck()
    if args.draws<20 or args.replicates<1:ap.error('need >=20 draws and >=1 replicate')
    methods=['complete_phase','oracle_masked_phase','joint_shift','filled_phase_observed','filled_phase_augmented','refilled_phase_augmented']
    report={'seed':args.seed,'replicates':args.replicates,'draws':args.draws,'numpy':np.__version__,
            'method':args.method,'shared':args.shared,'alpha':.05,'family':'3 voxels x full/first-half/second-half windows','results':{}}
    for name in args.scenarios:
        nt,phi,rate,kind=CONFIG[name]
        # Scenario RNG independent of which other scenarios are requested.
        rng=np.random.default_rng(np.random.SeedSequence([args.seed,list(CONFIG).index(name)]))
        counts={m:dict(point=0,family=0,invalid=0,evaluated=0) for m in methods};unfillable=0;fraction=0.
        for rep in range(args.replicates):
            x=signal(rng,nt,phi,args.shared);keep=masks(rng,x,kind,rate);fraction+=1-keep.mean()
            fill=completion(x,keep);eligible=np.isfinite(fill).all();unfillable+=not eligible
            factors=np.exp(1j*rng.uniform(-np.pi,np.pi,(args.draws,1,6,nt//2+1)))
            factors[...,0]=1;factors[...,-1]=1;factors[0]=1
            truth=phase(x,factors);truth[0]=x
            observed=keep[None,None,:,:]
            offsets=rng.integers(0,nt,size=(args.draws,1,6,1));offsets[0]=0
            indices=(np.arange(nt)+offsets)%nt
            shifted=np.take_along_axis(np.broadcast_to(np.where(keep,x,np.nan),truth.shape),indices,-1)
            shifted_real=np.take_along_axis(np.broadcast_to(observed,truth.shape),indices,-1)
            draws={'complete_phase':(truth,np.ones((1,1,6,nt),bool)),
                   'oracle_masked_phase':(np.where(observed,truth,np.nan),observed),
                   'joint_shift':(shifted,shifted_real)}
            if eligible:
                surrogate=phase(fill,factors);surrogate[0]=fill
                draws.update(filled_phase_observed=(np.where(observed,surrogate,np.nan),observed),
                             filled_phase_augmented=(surrogate,observed),
                             refilled_phase_augmented=(completion(surrogate,keep),observed))
            for method,(data,real) in draws.items():
                stat=estimate(data,real,args.method);finite=np.isfinite(stat);obs=stat[0]
                # Failed null cells make maxima unresolved, rather than disappear.
                null=np.where(finite,stat,np.inf);maximum=null.reshape(args.draws,-1).max(-1)
                p=(null>=obs).mean(0);pf=(maximum[:,None,None]>=obs).mean(0)
                c=counts[method];c['evaluated']+=1;c['invalid']+=not finite[0].all()
                c['point']+=bool(finite[0,0,0] and p[0,0]<=.05)
                c['family']+=bool(np.any(finite[0]&(pf<=.05)))
        for c in counts.values():
            n=c['evaluated'];c['point_rate']=c['point']/n if n else None;c['family_rate']=c['family']/n if n else None
            c['point_wilson95']=wilson(c['point'],n);c['family_wilson95']=wilson(c['family'],n)
        report['results'][name]={'ntime':nt,'mean_missing_fraction':fraction/args.replicates,'unfillable':unfillable,
            'interpretation':'power' if args.shared else 'informative-mask stress test' if kind=='informative' else 'independent-signal, independent-mask null','methods':counts}
        args.output.write_text(json.dumps(report,indent=2)+'\n')
        print(name,{m:(c['point'],c['family'],c['evaluated']) for m,c in counts.items()},flush=True)


if __name__=='__main__':main()
