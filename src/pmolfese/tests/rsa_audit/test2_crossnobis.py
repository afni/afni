#!/usr/bin/env python3
"""Test 2: runwise crossnobis RDMs (-runwiseTable) with -noise_norm
none/diag/shrinkage, single subject.

Full RDM recovery trick: with -metric pearson and one indicator model per
dyad k (e_k = 1 at dyad k, 0 elsewhere), the reported r_k is an exact affine
function of the neural distance d_k:
    r_k = (d_k - mean(d)) / (sd_pop(d) * sqrt(m-1)) .
So we predict every r_k from the oracle RDM and compare entrywise; this
checks the complete crossnobis geometry, not just one scalar summary.

Also checks corr_cov / cosine_cov against the explicit V^{-1} definition.
"""
import os, sys, json
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import oracle as o
from harness import *


def indicator_r(d):
    d = np.asarray(d, float)
    m = len(d)
    return (d - d.mean()) / (d.std() * np.sqrt(m - 1))


def wdiag_3drsa(R):
    """3dRSA's univariate weights: 1/sqrt(var floored at upper median of positive vars)."""
    var = np.sum(R ** 2, axis=0) / R.shape[0]
    pos = np.sort(var[var > 0])
    med = pos[len(pos) // 2]
    return 1.0 / np.sqrt(np.maximum(var, med))


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = np.random.default_rng(99)
    nc, nrun = 9, 3
    m = nc * (nc - 1) // 2
    results = []
    cases = [('p_lt_n', 10, 40), ('p_near_n', 60, 25), ('p_gt_n', 90, 20)]
    for cname, nv, nt in cases:
        d = os.path.join(work, cname)
        os.makedirs(d, exist_ok=True)
        write_mask(os.path.join(d, 'mask.nii'), nv)
        # spatially correlated noise so shrinkage/whitening is non-trivial
        A = rng.normal(size=(nv, nv)) * 0.3 + np.eye(nv)
        true = rng.normal(size=(nc, nv)) * 0.5
        folds, resids = [], []
        rows = ['Subj Run InputFile ResidFile']
        for r in range(nrun):
            noise = rng.normal(size=(nc, nv)) @ A
            pat = (true + noise).astype(np.float32)
            res = (rng.normal(size=(nt, nv)) @ A + rng.normal(size=nv) * 0.7).astype(np.float32)
            res[:, 0] *= 0.05          # a quiet voxel to exercise the variance floor
            folds.append(pat.astype(float)); resids.append(res.astype(float))
            write_patterns(os.path.join(d, f'beta_r{r}.nii'), pat)
            write_timeseries(os.path.join(d, f'resid_r{r}.nii'), res)
            rows.append(f's1 {r+1} beta_r{r}.nii resid_r{r}.nii')
        with open(os.path.join(d, 'runwise.txt'), 'w') as f:
            f.write('\n'.join(rows) + '\n')
        models = make_models(rng, nc)
        for name, (tri, isd) in models.items():
            write_model(os.path.join(d, f'{name}.1D'), tri)
        for k in range(m):
            e = np.zeros(m); e[k] = 1
            write_model(os.path.join(d, f'ind{k:02d}.1D'), e)
        Rdm = [r - r.mean(axis=0, keepdims=True) for r in resids]
        Rstack = np.vstack(Rdm)
        for nn in ['none', 'diag', 'shrinkage']:
            # oracle RDM
            if nn == 'none':
                noise = None
                d_or = o.calc_rdm_crossnobis(folds)
            elif nn == 'diag':
                w = wdiag_3drsa(Rstack)
                noise = np.diag(w ** 2)
                d_or = o.calc_rdm_crossnobis(folds, noise)
                # rsatoolbox-style diag (no floor, dof = n-1)
                prec_rs, _ = o.prec_from_residuals(resids, method='diag')
                d_rs = o.calc_rdm_crossnobis(folds, prec_rs)
            else:
                prec, shrink = o.prec_from_residuals(resids, dof=Rstack.shape[0], method='shrinkage_eye')
                noise = prec
                d_or = o.calc_rdm_crossnobis(folds, prec)
                prec_rs, shrink_rs = o.prec_from_residuals(resids, method='shrinkage_eye')  # dof = n-1
                d_rs = o.calc_rdm_crossnobis(folds, prec_rs)
            # --- 3dRSA: indicator recovery with pearson
            prefix = f'ind_{nn}'
            args = ['-mode', 'RSA', '-mask', 'mask.nii', '-runwiseTable', 'runwise.txt',
                    '-noise_norm', nn, '-metric', 'pearson', '-classic_null', 'conditions',
                    '-nperm', 20, '-seed', 3, '-no_dset', '-quiet', '-prefix', prefix]
            for k in range(m):
                args += ['-model_mat', f'ind{k:02d}', f'ind{k:02d}.1D']
            log = run_3drsa(args, d)
            tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
            got = np.array([tab[f'ind{k:02d}_r'][0] for k in range(m)])
            exp = indicator_r(d_or)
            err = np.max(np.abs(got - exp))
            rec = dict(case=cname, noise_norm=nn, check='indicator_recovery', max_abs_err=float(err),
                       r_pearson_oracle_vs_3dRSA=float(np.corrcoef(got, exp)[0, 1]))
            if nn != 'none':
                rec['rsatoolbox_convention_vs_3dRSA_convention_corr'] = float(np.corrcoef(d_rs, d_or)[0, 1])
                rec['rsatoolbox_convention_max_rel_diff'] = float(np.max(np.abs(d_rs - d_or)) / np.std(d_or))
            if nn == 'shrinkage':
                rec['oracle_shrink_dof_n'] = float(shrink); rec['oracle_shrink_dof_nm1'] = float(shrink_rs)
                import re
                mm = re.findall(r'shrink[^\n]*', log)
                rec['3dRSA_log_shrink_lines'] = mm[:3]
            results.append(rec); print(rec)
            # --- scalar metrics incl. cov-weighted ones, against the two models
            for met in ['pearson', 'spearman', 'rhoa', 'ktaua', 'ktaub', 'corr_cov', 'cosine_cov']:
                prefix = f'cmp_{nn}_{met}'
                args = ['-mode', 'RSA', '-mask', 'mask.nii', '-runwiseTable', 'runwise.txt',
                        '-noise_norm', nn, '-metric', met, '-classic_null', 'conditions',
                        '-nperm', 20, '-seed', 3, '-no_dset', '-quiet', '-prefix', prefix]
                for name in models:
                    args += ['-model_mat', name, f'{name}.1D']
                run_3drsa(args, d)
                tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
                for name, (tri, isd) in models.items():
                    g = float(tab[f'{name}_r'][0]); e = o.COMPARE[met](d_or, tri)
                    rec = dict(case=cname, noise_norm=nn, check='compare', metric=met, model=name,
                               got=g, expected=e, abs_err=abs(g - e))
                    results.append(rec)
                    if abs(g - e) > 2e-5:
                        print('  MISMATCH', rec)
    bad = [r for r in results if r.get('abs_err', r.get('max_abs_err', 0)) > 2e-5]
    print(f'test2 crossnobis: {len(results)-len(bad)}/{len(results)} checks within 2e-5')
    json.dump(results, open(os.path.join(work, 'results.json'), 'w'), indent=1)
    return len(bad) == 0


if __name__ == '__main__':
    sys.exit(0 if main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/rsa_audit/t2') else 1)
