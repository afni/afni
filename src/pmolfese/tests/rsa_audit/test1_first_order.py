#!/usr/bin/env python3
"""Test 1: single-subject classic RSA.  Checks every -neural_metric x -metric
combination of 3dRSA against the numpy oracle.  With one subject the reported
<model>_r is tanh(atanh(r)) = r, so this isolates RDM construction plus the
RDM comparison metric from any group aggregation."""
import os, sys, json
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import oracle as o
from harness import *

NEURAL = {'corr': o.simmat_pearson, 'scorr': o.simmat_spearman,
          'cosine': o.simmat_cosine, 'euclid': o.simmat_euclid}
METRICS = ['spearman', 'pearson', 'rhoa', 'ktaua', 'ktaub']


def main(work):
    # Regression: swapping tied/untied rank vectors must preserve equality.
    # Sequential SD division in np.corrcoef used to split this exact tie.
    a = np.arange(17, dtype=float)
    b = a.copy(); b[:2] = 0.5
    tied = o.simmat_spearman(np.array([a, b, b, a]))
    assert tied[0] == tied[5], "oracle split an exact Spearman tie"
    os.makedirs(work, exist_ok=True)
    rng = np.random.default_rng(2024)
    nc, nv = 9, 17
    results = []
    worst = 0.0
    for rep in range(3):
        d = os.path.join(work, f'rep{rep}')
        os.makedirs(d, exist_ok=True)
        # patterns with a shared baseline so centering matters, plus ties for scorr
        base = rng.normal(size=nv) * 2
        pat = base + rng.normal(size=(nc, nv))
        pat[:, :3] = np.round(pat[:, :3])     # some exactly tied features
        pat = pat.astype(np.float32)
        write_mask(os.path.join(d, 'mask.nii'), nv)
        write_patterns(os.path.join(d, 's1.nii'), pat)
        with open(os.path.join(d, 'table.txt'), 'w') as f:
            f.write('Subj InputFile\ns1 s1.nii\n')
        models = make_models(rng, nc)
        for name, (tri, isd) in models.items():
            write_model(os.path.join(d, f'{name}.1D'), tri)
        for center in ['none', 'subject']:
            patc = pat.astype(float)
            if center == 'subject':
                patc = patc - patc.mean(axis=0, keepdims=True)
            for nm, nfun in NEURAL.items():
                # 3dRSA skips centering for euclid (invariant anyway)
                neural_tri = nfun(patc if nm != 'euclid' else pat)
                mets = METRICS  # corr_cov/cosine_cov are crossnobis-only (test 3)
                for met in mets:
                    prefix = f'{center}_{nm}_{met}'
                    args = ['-mode', 'RSA', '-mask', 'mask.nii', '-dataTableFile', 'table.txt',
                            '-neural_metric', nm, '-metric', met, '-classic_null', 'conditions',
                            '-nperm', 20, '-seed', 7, '-no_dset', '-quiet',
                            '-center_conditions', center, '-prefix', prefix]
                    for name in models:
                        args += ['-model_mat', name, f'{name}.1D']
                    run_3drsa(args, d)
                    tab = read_rsa_table(os.path.join(d, prefix + '.rsa.1D'))
                    for name, (tri, isd) in models.items():
                        got = float(tab[f'{name}_r'][0])
                        exp = o.COMPARE[met](neural_tri, tri)
                        err = abs(got - exp)
                        worst = max(worst, err)
                        results.append(dict(rep=rep, center=center, neural=nm, metric=met,
                                            model=name, got=got, expected=exp, abs_err=err))
    ok = [r for r in results if r['abs_err'] < 2e-5]
    bad = [r for r in results if r['abs_err'] >= 2e-5]
    print(f'test1 first-order metrics: {len(ok)}/{len(results)} within 2e-5 (worst {worst:.2e})')
    for r in bad:
        print('  MISMATCH', r)
    json.dump(results, open(os.path.join(work, 'results.json'), 'w'), indent=1)
    return len(bad) == 0


if __name__ == '__main__':
    sys.exit(0 if main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/rsa_audit/t1') else 1)
