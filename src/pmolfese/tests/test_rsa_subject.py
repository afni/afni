#!/usr/bin/env python3
"""Independent numeric/CLI gate for 3dRSA participant-outcome regression.

Requires NumPy, SciPy and NiBabel. Run with --bin build/targets_built/3dRSA.
Fixtures and outputs stay in a temporary directory unless --work is supplied.
"""
import argparse
import csv
import itertools
import os
from pathlib import Path
import subprocess
import tempfile

import nibabel as nib
import numpy as np
from scipy.stats import rankdata, t as student_t


def read_tsv(path):
    with open(path) as f:
        return list(csv.DictReader((line for line in f if not line.startswith('#')), delimiter='\t'))


def close(a, b, label, atol=3e-5):
    np.testing.assert_allclose(a, b, rtol=3e-5, atol=atol, err_msg=label)


def rdm(pattern):
    return np.corrcoef(pattern)[np.triu_indices(len(pattern), 1)]


def fisher(a, b, rank=False):
    if rank:
        a, b = rankdata(a), rankdata(b)
    r = np.float32(np.corrcoef(a, b)[0, 1])
    return np.arctanh(np.clip(r, -np.float32(.999999), np.float32(.999999)))


def ols(y, x, cov):
    X = np.column_stack([x-x.mean(), np.ones(len(x)), cov-cov.mean()])
    inv = np.linalg.pinv(X)
    b = inv @ y
    e = y-X@b
    df = len(x)-X.shape[1]
    se = np.sqrt((e@e)/df * (inv@inv.T)[0, 0])
    return b[0], se, b[0]/se, b[1], X


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--bin', required=True)
    p.add_argument('--work')
    args = p.parse_args()
    exe = str(Path(args.bin).resolve())
    work = Path(args.work or tempfile.mkdtemp(prefix='rsa-subject-'))
    work.mkdir(parents=True, exist_ok=True)
    rng = np.random.default_rng(719)
    ns, repeats, nc, nv = 18, 3, 6, 12
    affine = np.diag([2., 2., 2., 1.])
    shape = (nv, 2, 1)
    mask = np.ones(shape, dtype=np.int16)
    mask[:, 1, :] = 2
    nib.save(nib.Nifti1Image(mask, affine), work/'mask.nii.gz')
    score = np.linspace(-1, 1, ns)
    iq = 100+15*score
    cov = rng.normal(size=ns)
    patterns = np.empty((ns, repeats, 2, nc, nv), dtype=np.float32)
    runs, ordinary = ['Subj Run InputFile ConditionFile'], ['Subj InputFile']
    (work/'conditions.txt').write_text(''.join(f'c{c}\n' for c in range(nc)))
    for s in range(ns):
        base = rng.normal(size=(2, nc, nv))
        for r in range(repeats):
            patterns[s, r, 0] = base[0] + (1.2-1.1*score[s])*rng.normal(size=(nc,nv))
            patterns[s, r, 1] = base[1] + 1.3*rng.normal(size=(nc,nv))
            data = patterns[s,r].transpose(2,0,1)[:, :, None, :]
            fn = f's{s}_r{r}.nii.gz'
            nib.save(nib.Nifti1Image(data,affine), work/fn)
            runs.append(f's{s} r{r} {fn} conditions.txt')
        ordinary.append(f's{s} s{s}_r0.nii.gz')
    (work/'runs.txt').write_text('\n'.join(runs)+'\n')
    (work/'ordinary.txt').write_text('\n'.join(ordinary)+'\n')

    def people(filename, ids=range(ns), x=iq, z=cov, header='Subj IQ Age'):
        (work/filename).write_text(header+'\n'+''.join(f's{s} {x[s]:.9g} {z[s]:.9g}\n' for s in ids))
    people('people.txt', reversed(range(ns)))
    env = dict(os.environ, AFNI_VERSION_CHECK='NO', AFNI_NIFTI_TYPE_WARN='NO', AFNI_DECONFLICT='OVERWRITE',
               AFNI_NO_OBLIQUE_WARNING='YES', OMP_NUM_THREADS='1')
    common = ['-mode','RSA','-mask','mask.nii.gz','-metric','pearson',
              '-subject_table','people.txt','-subject_predictor','IQ',
              '-subject_covariates','Age','-seed','723','-nperm','0','-quiet']

    def run(name, extra=(), base=common, expect=None, threads=1):
        pref = name+'.nii.gz'
        cmd = [exe]+list(base)+list(extra)+['-prefix',pref]
        res = subprocess.run(cmd,cwd=work,env=dict(env,OMP_NUM_THREADS=str(threads)),
                             stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
        (work/(name+'.log')).write_text(res.stdout)
        if expect:
            assert res.returncode != 0 and expect.lower() in res.stdout.lower(), res.stdout
            return
        assert res.returncode == 0, res.stdout
        return read_tsv(work/(pref+'.subject_regression.tsv')), read_tsv(work/(pref+'.subject_outcomes.tsv'))

    stability = ['-runwiseTable','runs.txt','-subject_outcome','stability']
    rows, subjects = run('stable',stability)
    observed = np.array([float(row['outcome_z']) for row in subjects]).reshape(2,ns).T
    expected = np.array([[np.mean([fisher(rdm(patterns[s,a,roi]),rdm(patterns[s,b,roi]))
                       for a,b in itertools.combinations(range(repeats),2)])
                       for roi in range(2)] for s in range(ns)])
    close(observed,expected,'independent repeat RDM and Fisher-mean reference')
    assert expected[-6:,0].mean() > expected[:6,0].mean()+.2
    assert float(rows[0]['beta']) > 0 and float(rows[0]['p']) < .05
    for k,row in enumerate(rows):
        b,se,t,intercept,_ = ols(observed[:,k],iq.astype(np.float32),cov.astype(np.float32))
        close([float(row[v]) for v in ['beta','SE','t','intercept']], [b,se,t,intercept], 'OLS reference')
        close(float(row['p']), student_t.sf(abs(t),ns-3)*2, 'parametric t p')
    image = nib.load(work/'stable.nii.gz').get_fdata().reshape(*shape,-1)
    for k,row in enumerate(rows):
        close(image[:,k,0,0],float(row['beta']),'painted beta map')
        close(image[:,k,0,8],1,'valid map')

    # Same-data atlas / tiny searchlight equality and OMP reproducibility.
    sl,_ = run('searchlight',stability+['-searchlight','RECT(100,0.1,0.1)'],threads=3)
    for row in sl:
        assert float(row['valid'])==1
    slimage = nib.load(work/'searchlight.nii.gz').get_fdata().reshape(*shape,-1)
    close(slimage,image,'searchlight vs identical atlas neighborhoods')
    threaded,_ = run('stable_threads',stability,threads=3)
    assert threaded==rows

    # Deliberately reorder every condition and the participant/run table.
    reordered = ['Subj Run InputFile ConditionFile']
    for s in reversed(range(ns)):
        for r in reversed(range(repeats)):
            order = rng.permutation(nc)
            fn,cf = f's{s}_r{r}_reorder.nii.gz',f's{s}_r{r}_conditions.txt'
            dat = patterns[s,r][:,order,:].transpose(2,0,1)[:,:,None,:]
            nib.save(nib.Nifti1Image(dat,affine),work/fn)
            (work/cf).write_text(''.join(f'c{c}\n' for c in order))
            reordered.append(f's{s} r{r} {fn} {cf}')
    (work/'reordered.txt').write_text('\n'.join(reordered)+'\n')
    reordered_rows,_ = run('reordered',['-runwiseTable','reordered.txt','-subject_outcome','stability'])
    for a,b in zip(rows,reordered_rows):
        close([float(a[v]) for v in ['beta','SE','t','p']], [float(b[v]) for v in ['beta','SE','t','p']], 'ID/condition matching')

    # Confounded score: correlated with outcomes only through nuisance Age.
    y = observed[:,0]
    z = (y-y.mean())/y.std()+rng.normal(size=ns)*.3
    M = np.column_stack([np.ones(ns),z])
    nuisance_y = y-M@np.linalg.lstsq(M,y,rcond=None)[0]
    u = rng.normal(size=ns)
    A = np.column_stack([M,nuisance_y]); u -= A@np.linalg.lstsq(A,u,rcond=None)[0]
    x = 100+10*z+3*u
    people('confound.txt',x=x,z=z)
    confbase = [v if v!='people.txt' else 'confound.txt' for v in common]
    adjusted,_ = run('adjusted',stability,base=confbase)
    unadjusted = confbase.copy(); q=unadjusted.index('-subject_covariates'); del unadjusted[q:q+2]
    naive,_ = run('unadjusted',stability,base=unadjusted)
    assert abs(float(adjusted[0]['t'])) < .001 and float(adjusted[0]['p']) > .99
    assert float(naive[0]['p']) < .001

    # Fixed category model fit from ordinary data (same score regression engine).
    category = np.array([0,0,0,1,1,1])
    model = (category[:,None]==category).astype(float)
    np.savetxt(work/'category.1D',model)
    fits = ['-dataTableFile','ordinary.txt','-subject_outcome','model_fit','-model_mat','category','category.1D']
    fitrows,fitsub = run('fits',fits)
    fitref = np.array([[fisher(rdm(patterns[s,0,k]),model[np.triu_indices(nc,1)])
                       for k in range(2)] for s in range(ns)])
    fitobs = np.array([float(v['outcome_z']) for v in fitsub]).reshape(2,ns).T
    close(fitobs,fitref,'category model fit reference')
    ranked = [v if v!='pearson' else 'spearman' for v in common]
    _,ranksub = run('rankfits',fits,base=ranked)
    rankref = np.array([[fisher(rdm(patterns[s,0,k]),model[np.triu_indices(nc,1)],True)
                        for s in range(ns)] for k in range(2)])
    close([float(v['outcome_z']) for v in ranksub],rankref.ravel(),'rank model fits')

    # Runwise model fits must retain crossnobis, rather than use repeat stability.
    _,crosssub = run('crossfit',['-runwiseTable','runs.txt']+fits[2:])
    crossref=[]
    for k in range(2):
        for s in range(ns):
            d=[]
            for a,b in itertools.combinations(range(nc),2):
                diff=patterns[s,:,k,a,:].astype(float)-patterns[s,:,k,b,:]
                d.append(np.mean([diff[u]@diff[v] for u,v in itertools.combinations(range(repeats),2)])/nv)
            crossref.append(fisher(d,model[np.triu_indices(nc,1)]))
    close([float(v['outcome_z']) for v in crosssub],crossref,'runwise crossnobis fits')

    # Exhaustive 6! Freedman-Lane permutations, independent NumPy reference,
    # same residual relabeling across both ROIs for the max-|t| family.
    nsmall=6
    (work/'smallruns.txt').write_text('\n'.join(runs[:1+nsmall*repeats])+'\n')
    people('smallpeople.txt',ids=range(nsmall))
    smallbase=[v if v!='people.txt' else 'smallpeople.txt' for v in common]
    smallbase[smallbase.index('-nperm')+1]='720'
    small,_=run('permutation',['-runwiseTable','smallruns.txt','-subject_outcome','stability'],base=smallbase)
    Y=observed[:nsmall]; x=iq[:nsmall].astype(np.float32); z=cov[:nsmall].astype(np.float32)
    M=np.column_stack([x-x.mean(),np.ones(nsmall),z-z.mean()]); Z=M[:,1:]
    pinv=np.linalg.pinv(M); zv=Z@np.linalg.pinv(Z)@Y; e=Y-zv
    null=[]
    def ts(vals):
        b=pinv@vals; resid=vals-M@b
        return b[0]/np.sqrt(np.sum(resid**2,axis=0)/(nsmall-3)*(pinv@pinv.T)[0,0])
    obs=ts(Y)
    for perm in itertools.permutations(range(nsmall)):
        null.append(ts(zv+e[list(perm)]))
    null=np.abs(null); tol=1e-5
    for k,row in enumerate(small):
        close(float(row['p']),np.mean(null[:,k]>=abs(obs[k])-tol),'exhaustive Freedman-Lane p',atol=1e-6)
        close(float(row['pFWE']),np.mean(null.max(axis=1)>=abs(obs[k])-tol),'exhaustive spatial max-t p',atol=1e-6)
        assert 1/720-1e-7 <= float(row['p']) <= float(row['pFWE'])+1e-7
    smallthread,_=run('permutation_threads',['-runwiseTable','smallruns.txt','-subject_outcome','stability'],base=smallbase,threads=3)
    assert smallthread==small

    # Invalid designs and inputs fail, never silently drop participants.
    people('constant.txt',x=np.ones(ns))
    people('collinear.txt',z=iq)
    people('nonfinite.txt',x=np.r_[np.nan,iq[1:]])
    people('missing.txt',ids=range(ns-1))
    people('duplicate.txt',ids=[0,0]+list(range(2,ns)))
    people('unknown.txt',ids=range(ns)); text=(work/'unknown.txt').read_text().replace('s0 ','extra ')
    (work/'unknown.txt').write_text(text)
    # Identity and numeric-column diagnostics now come from thd_datatable.
    for file,error in [('constant.txt','constant'),('collinear.txt','rank'),('nonfinite.txt','not finite'),
                       ('missing.txt','exactly one'),('duplicate.txt','duplicate'),('unknown.txt','unexpected')]:
        base=[v if v!='people.txt' else file for v in common]
        run('bad_'+file,stability,base=base,expect=error)
    run('bad_stability',stability+['-noise_norm','diag'],expect='noise_norm none')
    run('bad_models',stability+['-model_mat','category','category.1D'],expect='do not supply')
    run('bad_mode',stability+['-mode','IS-RSA'],expect='requires -mode RSA')
    (work/'three_runs.txt').write_text('\n'.join(runs[:1+3*repeats])+'\n')
    people('three_people.txt',ids=range(3))
    threebase=[v if v!='people.txt' else 'three_people.txt' for v in common]
    run('bad_df',['-runwiseTable','three_runs.txt','-subject_outcome','stability'],
        base=threebase,expect='degrees of freedom')
    (work/'missing_condition.txt').write_text(''.join(f'c{c}\n' for c in range(nc-1)))
    missing_runs=runs.copy()
    missing_runs[1]='s0 r0 s0_r0.nii.gz[0..4] missing_condition.txt'
    (work/'missing_runs.txt').write_text('\n'.join(missing_runs)+'\n')
    run('bad_condition',['-runwiseTable','missing_runs.txt','-subject_outcome','stability'],
        expect='every condition in every repeat')
    # Every run RDM constant: retain missing outcomes and explicit invalid maps.
    nib.save(nib.Nifti1Image(np.zeros((*shape,nc),dtype=np.float32),affine),work/'zero.nii.gz')
    (work/'zeroruns.txt').write_text('Subj Run InputFile\n'+''.join(f's{s} r{r} zero.nii.gz\n' for s in range(ns) for r in range(repeats)))
    zero,zsub=run('undefined',['-runwiseTable','zeroruns.txt','-subject_outcome','stability'])
    assert all(float(v['valid'])==0 and float(v['p'])==1 and float(v['n'])==0 for v in zero)
    assert all(np.isnan(float(v['outcome_z'])) for v in zsub)
    print(f'PASS: participant stability/model fits, OLS/covariates, exhaustive Freedman-Lane/FWE, '
          f'identity matching, searchlights, OMP, invalid inputs. Artifacts: {work}')


if __name__ == '__main__':
    main()
