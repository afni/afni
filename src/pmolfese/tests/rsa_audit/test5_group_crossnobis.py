#!/usr/bin/env python3
"""Test 5: multi-subject runwise crossnobis with the covariance-weighted
comparators (corr_cov / cosine_cov) and -noise_norm shrinkage.

Checks <model>_r (Fisher aggregate of per-subject whitened cosines), the
parametric p, and nc_low/nc_high against normalized-template Fisher aggregation
and the arithmetic-aggregation convention."""
import os, sys, json
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import oracle as o
from harness import *

CLIP = 0.999329


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = np.random.default_rng(4242)
    ns, nc, nv, nrun, nt = 9, 8, 25, 3, 30
    d = work
    write_mask(os.path.join(d, 'mask.nii'), nv)
    models = make_models(rng, nc)
    for name, (tri, isd) in models.items():
        write_model(os.path.join(d, f'{name}.1D'), tri)
    rows = ['Subj Run InputFile ResidFile']
    shared = rng.normal(size=(nc, nv))
    srdm_plain, srdm_white = [], []
    for s in range(ns):
        A = np.eye(nv) + 0.25 * rng.normal(size=(nv, nv))
        true = 0.7 * shared + 0.5 * rng.normal(size=(nc, nv))
        folds, resids = [], []
        for r in range(nrun):
            pat = (true + rng.normal(size=(nc, nv)) @ A).astype(np.float32)
            res = (rng.normal(size=(nt, nv)) @ A + rng.normal(size=nv)).astype(np.float32)
            folds.append(pat.astype(float)); resids.append(res.astype(float))
            write_patterns(os.path.join(d, f's{s}_beta_r{r}.nii'), pat)
            write_timeseries(os.path.join(d, f's{s}_resid_r{r}.nii'), res)
            rows.append(f's{s} {r+1} s{s}_beta_r{r}.nii s{s}_resid_r{r}.nii')
        srdm_plain.append(o.calc_rdm_crossnobis(folds))
        ntot = nt * nrun
        prec, _ = o.prec_from_residuals(resids, dof=ntot, method='shrinkage_eye')
        srdm_white.append(o.calc_rdm_crossnobis(folds, prec))
    open(os.path.join(d, 'runwise.txt'), 'w').write('\n'.join(rows) + '\n')
    results = []
    for nn, srdm in [('none', np.array(srdm_plain)), ('shrinkage', np.array(srdm_white))]:
        for met in ['corr_cov', 'cosine_cov', 'spearman']:
            prefix = f'{nn}_{met}'
            run_3drsa(['-mode', 'RSA', '-mask', 'mask.nii', '-runwiseTable', 'runwise.txt', '-noise_norm', nn,
                       '-metric', met, '-nperm', 0, '-noise_ceiling', '-no_dset', '-quiet', '-prefix', prefix,
                       '-model_mat', 'cont', 'cont.1D', '-model_mat', 'cat', 'cat.1D'], d)
            tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
            cmp = o.COMPARE[met]
            for name, (tri, isd) in models.items():
                z = np.arctanh(np.clip([cmp(srdm[s], tri) for s in range(ns)], -CLIP, CLIP))
                exp_r = float(np.tanh(z.mean()))
                t = z.mean() / (z.std(ddof=1) / np.sqrt(ns))
                exp_p = student_t_p2(t, ns - 1)
                got_r, got_p = float(tab[f'{name}_r'][0]), float(tab[f'{name}_p'][0])
                results.append(dict(noise_norm=nn, metric=met, model=name, check='fisher_mean_r', got=got_r,
                                    expected=exp_r, abs_err=abs(got_r - exp_r)))
                results.append(dict(noise_norm=nn, metric=met, model=name, check='parametric_p', got=got_p,
                                    expected=exp_p, abs_err=abs(got_p - exp_p)))
            lo_raw, hi_raw = o.noise_ceiling(srdm, met, fisher=True)
            lo_rs, hi_rs = o.noise_ceiling(srdm, met, normalize='rsatoolbox')
            got_lo, got_hi = float(tab['nc_low'][0]), float(tab['nc_high'][0])
            results.append(dict(noise_norm=nn, metric=met, check='nc_low_normalized_fisher', got=got_lo, expected=lo_raw,
                                abs_err=abs(got_lo - lo_raw)))
            results.append(dict(noise_norm=nn, metric=met, check='nc_high_normalized_fisher', got=got_hi, expected=hi_raw,
                                abs_err=abs(got_hi - hi_raw)))
            results.append(dict(noise_norm=nn, metric=met, check='nc_low_vs_arithmetic_convention', got=got_lo,
                                expected=lo_rs, abs_err=abs(got_lo - lo_rs), convention=True))
            results.append(dict(noise_norm=nn, metric=met, check='nc_high_vs_arithmetic_convention', got=got_hi,
                                expected=hi_rs, abs_err=abs(got_hi - hi_rs), convention=True))
    strict = [r for r in results if not r.get('convention')]
    bad = [r for r in strict if r['abs_err'] > 2e-5]
    print(f'test5 group crossnobis/cov-weighted: {len(strict)-len(bad)}/{len(strict)} strict checks within 2e-5')
    for r in bad:
        print('  MISMATCH', r)
    print('  noise-ceiling convention gap:')
    for r in results:
        if r.get('convention'):
            print(f"    {r['noise_norm']:9s} {r['metric']:10s} {r['check']:34s} 3dRSA={r['got']:+.4f} "
                  f"arithmetic={r['expected']:+.4f} diff={r['got']-r['expected']:+.4f}")
    json.dump(results, open(os.path.join(work, 'results.json'), 'w'), indent=1)
    return len(bad) == 0


if __name__ == '__main__':
    sys.exit(0 if main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/rsa_audit/t5') else 1)
