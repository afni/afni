#!/usr/bin/env python3
"""Test 3: multi-subject classic RSA.

Checks, per metric (spearman / pearson / rhoa / ktaua / ktaub):
  * <model>_r  == tanh(mean_s atanh(r_s))        (Fisher-z aggregate)
  * <model>_p  (with -nperm 0) == two-sided Student t on the atanh(r_s)
  * nc_low / nc_high against normalized-template Fisher aggregation, with
    arithmetic aggregation reported as a convention comparison.
  * nc_high equals the same template supplied as a fixed model.
Checks -model_fit (nested leave-one-subject-out NNLS on standardized
components, ridge 0 and 0.01) against an exact active-set NNLS oracle.
"""
import os, sys, json
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import oracle as o
from harness import *

CLIP = 0.999329  # 3dRSA MYatanh clip


def model_fit_oracle(srdm, comps, ridge):
    """Replicates rsa_fit_subject_cv for classic RSA: for each held subject,
    stack all other subjects' triangles as y, components as X; standardized
    NNLS(+ridge); predict held subject's triangle; Pearson; mean z."""
    ns, m = srdm.shape
    X_one = np.column_stack(comps)
    zs, ws = [], []
    for hold in range(ns):
        y = np.concatenate([srdm[s] for s in range(ns) if s != hold])
        X = np.vstack([X_one] * (ns - 1))
        w, xm, xs = o.nnls_ridge_standardized(y, X, ridge)
        pred = ((X_one - xm) / xs) @ w
        r = o.pearson(srdm[hold], pred) if np.std(pred) > 0 else 0.0
        zs.append(np.arctanh(np.clip(r, -CLIP, CLIP)))
        ws.append(w / w.sum() if w.sum() > 0 else w)
    return float(np.tanh(np.mean(zs))), np.mean(ws, axis=0)


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = np.random.default_rng(31337)
    ns, nc, nv = 12, 10, 20
    m = nc * (nc - 1) // 2
    results = []
    for rep in range(2):
        d = os.path.join(work, f'rep{rep}')
        os.makedirs(d, exist_ok=True)
        write_mask(os.path.join(d, 'mask.nii'), nv)
        models = make_models(rng, nc)
        third = rng.normal(size=m) ** 2
        models['third'] = (third, True)
        for name, (tri, isd) in models.items():
            write_model(os.path.join(d, f'{name}.1D'), tri)
        # signal: subjects share a geometry driven partly by 'cont'
        shared = rng.normal(size=(nc, nv))
        rows = ['Subj InputFile']
        pats = []
        for s in range(ns):
            p = 0.6 * shared + rng.normal(size=(nc, nv))
            p = p.astype(np.float32)
            pats.append(p.astype(float))
            write_patterns(os.path.join(d, f's{s}.nii'), p)
            rows.append(f's{s} s{s}.nii')
        with open(os.path.join(d, 'table.txt'), 'w') as f:
            f.write('\n'.join(rows) + '\n')
        for nm, nfun in [('corr', o.simmat_pearson), ('euclid', o.simmat_euclid)]:
            srdm = np.array([nfun(p) for p in pats])
            for met in ['spearman', 'pearson', 'rhoa', 'ktaua', 'ktaub']:
                prefix = f'{nm}_{met}'
                args = ['-mode', 'RSA', '-mask', 'mask.nii', '-dataTableFile', 'table.txt',
                        '-neural_metric', nm, '-metric', met, '-nperm', 0, '-noise_ceiling',
                        '-no_dset', '-quiet', '-prefix', prefix]
                for name in models:
                    args += ['-model_mat', name, f'{name}.1D']
                if met == 'pearson':
                    args += ['-model_fit', 'mix=cont,cat,third', '-fit_ridge', 0]
                template = o._nc_transform(srdm, met).mean(axis=0)
                write_model(os.path.join(d, 'ceiling_template.1D'), template)
                args += ['-model_mat', 'ceiling_template', 'ceiling_template.1D']
                run_3drsa(args, d)
                tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
                # The same fixed template must have the same Fisher score as
                # the inclusive reference; this does not assert a global bound.
                template_r = float(tab['ceiling_template_r'][0])
                ceiling = float(tab['nc_high'][0])
                results.append(dict(rep=rep, neural=nm, metric=met,
                                    check='inclusive_template_model_score',
                                    got=ceiling, expected=template_r,
                                    abs_err=abs(ceiling - template_r)))
                cmp = o.COMPARE[met]
                for name, (tri, isd) in models.items():
                    rs = np.array([cmp(srdm[s], tri) for s in range(ns)])
                    z = np.arctanh(np.clip(rs, -CLIP, CLIP))
                    exp_r = float(np.tanh(z.mean()))
                    t = z.mean() / (z.std(ddof=1) / np.sqrt(ns))
                    exp_p = student_t_p2(t, ns - 1)
                    got_r, got_p = float(tab[f'{name}_r'][0]), float(tab[f'{name}_p'][0])
                    results.append(dict(rep=rep, neural=nm, metric=met, model=name, check='fisher_mean_r',
                                        got=got_r, expected=exp_r, abs_err=abs(got_r - exp_r)))
                    results.append(dict(rep=rep, neural=nm, metric=met, model=name, check='parametric_p',
                                        got=got_p, expected=exp_p, abs_err=abs(got_p - exp_p)))
                lo_raw, hi_raw = o.noise_ceiling(srdm, met, fisher=True)
                lo_rs, hi_rs = o.noise_ceiling(srdm, met, normalize='rsatoolbox')
                got_lo, got_hi = float(tab['nc_low'][0]), float(tab['nc_high'][0])
                results.append(dict(rep=rep, neural=nm, metric=met, check='nc_low_normalized_fisher',
                                    got=got_lo, expected=lo_raw, abs_err=abs(got_lo - lo_raw)))
                results.append(dict(rep=rep, neural=nm, metric=met, check='nc_high_normalized_fisher',
                                    got=got_hi, expected=hi_raw, abs_err=abs(got_hi - hi_raw)))
                results.append(dict(rep=rep, neural=nm, metric=met, check='nc_low_vs_rsatoolbox_convention',
                                    got=got_lo, expected=lo_rs, abs_err=abs(got_lo - lo_rs), convention=True))
                results.append(dict(rep=rep, neural=nm, metric=met, check='nc_high_vs_rsatoolbox_convention',
                                    got=got_hi, expected=hi_rs, abs_err=abs(got_hi - hi_rs), convention=True))
                if met == 'pearson':
                    for ridge in [0.0, 0.01]:
                        if ridge > 0:
                            prefix2 = f'{nm}_fit_ridge'
                            a2 = [prefix2 if (i > 0 and args[i-1] == '-prefix') else a for i, a in enumerate(args)]
                            a2[a2.index('-fit_ridge') + 1] = ridge
                            run_3drsa(a2, d)
                            tab2 = read_rsa_table(os.path.join(d, prefix2 + '.rsa.1D'))
                        else:
                            tab2 = tab
                        comps = [models[k][0] for k in ['cont', 'cat', 'third']]
                        exp_cv, exp_w = model_fit_oracle(srdm, comps, ridge)
                        got_cv = float(tab2['mix_cvR'][0])
                        got_w = np.array([tab2[f'mix_w_{k}'][0] for k in ['cont', 'cat', 'third']])
                        results.append(dict(rep=rep, neural=nm, ridge=ridge, check='model_fit_cvR',
                                            got=got_cv, expected=exp_cv, abs_err=abs(got_cv - exp_cv)))
                        results.append(dict(rep=rep, neural=nm, ridge=ridge, check='model_fit_weights',
                                            got=got_w.tolist(), expected=exp_w.tolist(),
                                            abs_err=float(np.max(np.abs(got_w - exp_w)))))
    strict = [r for r in results if not r.get('convention')]
    bad = [r for r in strict if r['abs_err'] > 2e-5]
    print(f'test3 group classic: {len(strict)-len(bad)}/{len(strict)} strict checks within 2e-5')
    for r in bad:
        print('  MISMATCH', r)
    conv = [r for r in results if r.get('convention')]
    print('  noise-ceiling convention gap (3dRSA Fisher mean vs arithmetic mean):')
    for r in conv:
        print(f"    {r['neural']:6s} {r['metric']:8s} {r['check']:34s} 3dRSA={r['got']:+.4f} rsatoolbox-style={r['expected']:+.4f} diff={r['got']-r['expected']:+.4f}")
    json.dump(results, open(os.path.join(work, 'results.json'), 'w'), indent=1)
    return len(bad) == 0


if __name__ == '__main__':
    sys.exit(0 if main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/rsa_audit/t3') else 1)
