#!/usr/bin/env python3
"""Test 4: inference sanity + second-order IS-RSA geometry.

 (a) -classic_null conditions (joint row/column relabeling of the model):
     3dRSA's permutation p vs an oracle Monte-Carlo p with 20000 relabelings;
     must agree within Monte-Carlo error.
 (b) -classic_null subjects (sign flips of per-subject Fisher z): 3dRSA p vs
     the EXACT enumeration of all 2^n sign patterns.
 (c) -mode IS-RSA -featuretype rdm: the saved subject-by-subject outer matrix
     (-save_rdm) vs the oracle second-order RDM built from inner condition
     RDMs (1 - r for corr inner metric, Pearson outer metric).
"""
import os, sys, json, itertools
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import oracle as o
from harness import *

CLIP = 0.999329


def perm_model(tri, perm):
    M = o.squareform(tri)[np.ix_(perm, perm)]
    return o.triu(M)


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = np.random.default_rng(777)
    ns, nc, nv = 11, 9, 16
    d = work
    write_mask(os.path.join(d, 'mask.nii'), nv)
    models = make_models(rng, nc)
    for name, (tri, isd) in models.items():
        write_model(os.path.join(d, f'{name}.1D'), tri)
    shared = rng.normal(size=(nc, nv))
    pats, rows = [], ['Subj InputFile']
    for s in range(ns):
        p = (0.4 * shared + rng.normal(size=(nc, nv))).astype(np.float32)
        pats.append(p.astype(float))
        write_patterns(os.path.join(d, f's{s}.nii'), p)
        rows.append(f's{s} s{s}.nii')
    open(os.path.join(d, 'table.txt'), 'w').write('\n'.join(rows) + '\n')
    srdm = np.array([o.simmat_pearson(p) for p in pats])
    results = []
    for met in ['spearman', 'pearson']:
        cmp = o.COMPARE[met]
        # (a) condition relabeling null, nperm 4000
        prefix = f'condnull_{met}'
        args = ['-mode', 'RSA', '-mask', 'mask.nii', '-dataTableFile', 'table.txt', '-metric', met,
                '-classic_null', 'conditions', '-nperm', 4000, '-seed', 11, '-no_dset', '-quiet',
                '-prefix', prefix, '-model_mat', 'cont', 'cont.1D', '-model_mat', 'cat', 'cat.1D']
        run_3drsa(args, d)
        tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
        for name, (tri, isd) in models.items():
            obs = np.mean([np.arctanh(np.clip(cmp(srdm[s], tri), -CLIP, CLIP)) for s in range(ns)])
            null = []
            for k in range(20000):
                perm = rng.permutation(nc) if k > 0 else np.arange(nc)
                mt = perm_model(tri, perm)
                null.append(np.mean([np.arctanh(np.clip(cmp(srdm[s], mt), -CLIP, CLIP)) for s in range(ns)]))
            null = np.abs(np.array(null))
            p_or = np.mean(null >= abs(obs) - 1e-9)
            p_got = float(tab[f'{name}_p'][0])
            se = np.sqrt(p_or * (1 - p_or) / 4000 + p_or * (1 - p_or) / 20000)
            results.append(dict(check='condition_null_p', metric=met, model=name, got=p_got, expected=p_or,
                                z=float((p_got - p_or) / max(se, 1e-6)), stat_got=float(tab[f'{name}_r'][0]),
                                stat_exp=float(np.tanh(obs))))
        # (b) subject sign-flip null, exact enumeration
        prefix = f'subjnull_{met}'
        args = ['-mode', 'RSA', '-mask', 'mask.nii', '-dataTableFile', 'table.txt', '-metric', met,
                '-nperm', 4000, '-seed', 11, '-no_dset', '-quiet',
                '-prefix', prefix, '-model_mat', 'cont', 'cont.1D', '-model_mat', 'cat', 'cat.1D']
        run_3drsa(args, d)
        tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
        for name, (tri, isd) in models.items():
            z = np.array([np.arctanh(np.clip(cmp(srdm[s], tri), -CLIP, CLIP)) for s in range(ns)])
            def tstat(v):
                return v.mean() / (v.std(ddof=1) / np.sqrt(len(v)))
            tobs = abs(tstat(z))
            signs = np.array(list(itertools.product([1, -1], repeat=ns)))
            tnull = np.abs((signs * z).mean(axis=1) / ((signs * z).std(axis=1, ddof=1) / np.sqrt(ns)))
            p_exact = np.mean(tnull >= tobs - 1e-7)
            p_got = float(tab[f'{name}_p'][0])
            se = np.sqrt(p_exact * (1 - p_exact) / 4000)
            results.append(dict(check='signflip_null_p', metric=met, model=name, got=p_got, expected=p_exact,
                                z=float((p_got - p_exact) / max(se, 1e-6))))
    # (c) second-order IS-RSA
    beh = rng.normal(size=ns)
    rows = ['Subj beh InputFile'] + [f's{s} {beh[s]:.6f} s{s}.nii' for s in range(ns)]
    open(os.path.join(d, 'table2.txt'), 'w').write('\n'.join(rows) + '\n')
    for inner, ifun in [('corr', o.simmat_pearson), ('euclid', o.simmat_euclid)]:
        for outer in ['corr', 'euclid']:
            prefix = f'so_{inner}_{outer}'
            run_3drsa(['-mode', 'IS-RSA', '-featuretype', 'rdm', '-mask', 'mask.nii', '-dataTableFile', 'table2.txt',
                       '-condition_metric', inner, '-neural_metric', outer, '-model', 'b', 'beh:nn',
                       '-metric', 'spearman', '-nperm', 50, '-seed', 1, '-no_dset', '-quiet',
                       '-save_rdm', prefix, '-prefix', prefix], d)
            outer_mat = read_1d(os.path.join(d, f'{prefix}_roi0001.1D'))
            inner_tris = np.array([ifun(p) for p in pats])
            feats = inner_tris if inner == 'euclid' else 1 - inner_tris  # similarity -> distance sense
            exp = o.squareform(o.simmat_pearson(feats) if outer == 'corr' else o.simmat_euclid(feats))
            err = float(np.max(np.abs(o.triu(outer_mat) - o.triu(exp))))
            results.append(dict(check='second_order_outer_matrix', inner=inner, outer=outer, max_abs_err=err))
    bad = []
    for r in results:
        if 'z' in r and abs(r['z']) > 3.5:
            bad.append(r)
        if r.get('max_abs_err', 0) > (4e-5 if r.get('outer') == 'euclid' else 2e-5):  # 6-sig-digit 1D print of values ~10
            bad.append(r)
        print(' ', r)
    print(f'test4 inference/second-order: {len(results)-len(bad)}/{len(results)} checks pass')
    json.dump(results, open(os.path.join(work, 'results.json'), 'w'), indent=1)
    return len(bad) == 0


if __name__ == '__main__':
    sys.exit(0 if main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/rsa_audit/t4') else 1)
