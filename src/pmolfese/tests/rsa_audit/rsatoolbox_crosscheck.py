#!/usr/bin/env python3
"""Cross-check the offline numpy oracle (oracle.py) against the real
rsatoolbox package.  Run this on a machine with internet access:

    pip install rsatoolbox numpy scipy
    python3 rsatoolbox_crosscheck.py

It does NOT need 3dRSA or AFNI.  The sandbox audit compared 3dRSA to
oracle.py; this script closes the loop by confirming that oracle.py agrees
with rsatoolbox itself (RDM calculation, noise precision estimators, RDM
comparison metrics, and the noise-ceiling convention).  Any line marked
MISMATCH means the oracle -- and therefore the audit's expectation for that
quantity -- differs from rsatoolbox and should be re-examined.
"""
import sys
import numpy as np

try:
    import rsatoolbox
    from rsatoolbox.data import Dataset
    from rsatoolbox.rdm import calc_rdm, RDMs, compare
    from rsatoolbox.data.noise import prec_from_residuals
    from rsatoolbox.inference import boot_noise_ceiling
except ImportError as e:
    sys.exit(f'rsatoolbox is required: {e}')

import oracle as o

print('rsatoolbox version:', getattr(rsatoolbox, '__version__', '?'))
rng = np.random.default_rng(12345)
nc, nv, nrun, nt = 9, 14, 3, 40
status = []


def check(name, a, b, tol=1e-8):
    a = np.asarray(a, float); b = np.asarray(b, float)
    err = float(np.max(np.abs(a - b)))
    ok = err <= tol
    status.append(ok)
    print(f"{'ok      ' if ok else 'MISMATCH'} {name:55s} max|diff| = {err:.2e}")


# ---------------------------------------------------------------- RDM calc
meas = rng.normal(size=(nc, nv)) + rng.normal(size=nv)
ds = Dataset(meas, obs_descriptors={'cond': np.arange(nc)})
check('calc_rdm euclidean', calc_rdm(ds, method='euclidean', descriptor='cond').get_vectors()[0], o.calc_rdm_euclidean(meas))
check('calc_rdm correlation', calc_rdm(ds, method='correlation', descriptor='cond').get_vectors()[0], o.calc_rdm_correlation(meas))

folds = [meas + rng.normal(size=(nc, nv)) for _ in range(nrun)]
resid = [rng.normal(size=(nt, nv)) @ (np.eye(nv) + 0.3 * rng.normal(size=(nv, nv))) for _ in range(nrun)]
obs = np.concatenate([np.full(nc, r) for r in range(nrun)])
ds_cv = Dataset(np.vstack(folds), obs_descriptors={'cond': np.tile(np.arange(nc), nrun), 'run': obs})
check('calc_rdm crossnobis (W=I)', calc_rdm(ds_cv, method='crossnobis', descriptor='cond', cv_descriptor='run').get_vectors()[0],
      o.calc_rdm_crossnobis(folds))

# ------------------------------------------------------- noise precision
R = np.vstack(resid)
for method in ['diag', 'shrinkage_eye', 'full']:
    try:
        prec_rs = prec_from_residuals(R, method=method)
        prec_or, _ = o.prec_from_residuals(R, method=method)
        check(f'prec_from_residuals({method}) default dof', prec_rs, prec_or, tol=1e-6 * np.max(np.abs(prec_rs)))
        prec_rs2 = prec_from_residuals(R, dof=R.shape[0], method=method)
        prec_or2, _ = o.prec_from_residuals(R, dof=R.shape[0], method=method)
        check(f'prec_from_residuals({method}) dof=n', prec_rs2, prec_or2, tol=1e-6 * np.max(np.abs(prec_rs2)))
    except Exception as e:  # noqa
        status.append(False); print(f'MISMATCH prec_from_residuals({method}) raised: {e}')
prec_rs = prec_from_residuals(R, method='shrinkage_eye')
check('calc_rdm crossnobis (shrinkage_eye noise)',
      calc_rdm(ds_cv, method='crossnobis', descriptor='cond', cv_descriptor='run', noise=prec_rs).get_vectors()[0],
      o.calc_rdm_crossnobis(folds, prec_rs))
check('calc_rdm mahalanobis', calc_rdm(ds, method='mahalanobis', descriptor='cond', noise=prec_rs).get_vectors()[0],
      o.calc_rdm_mahalanobis(meas, prec_rs))

# ----------------------------------------------------------- comparisons
m = nc * (nc - 1) // 2
v1 = rng.normal(size=m); v2 = rng.normal(size=m)
cat = (np.arange(nc)[:, None] % 2 != np.arange(nc)[None, :] % 2).astype(float)[np.triu_indices(nc, 1)]
v1t = np.round(v1 * 2) / 2  # ties
pairs = {'cosine': o.compare_cosine, 'corr': o.compare_correlation, 'spearman': o.compare_spearman,
         'rho-a': o.compare_rho_a, 'tau-a': o.compare_kendall_tau_a, 'kendall': o.compare_kendall_tau_b,
         'corr_cov': o.compare_correlation_cov_weighted, 'cosine_cov': o.compare_cosine_cov_weighted}
for a, b, tag in [(v1, v2, 'continuous'), (v1t, cat, 'tied/categorical'), (v1, cat, 'continuous vs categorical')]:
    ra = RDMs(a[None, :]); rb = RDMs(b[None, :])
    for method, fun in pairs.items():
        try:
            check(f'compare {method:10s} [{tag}]', compare(ra, rb, method=method)[0, 0], fun(a, b), tol=1e-7)
        except Exception as e:  # noqa
            status.append(False); print(f'MISMATCH compare {method} [{tag}] raised: {e}')

# -------------------------------------------------------- noise ceiling
ns = 10
shared = rng.normal(size=m)
mat = np.array([shared + 0.8 * rng.normal(size=m) + 2 * rng.normal() for _ in range(ns)])
rd = RDMs(mat, rdm_descriptors={'subj': np.arange(ns)})
for method, key in [('corr', 'pearson'), ('spearman', 'spearman'), ('cosine', 'cosine'), ('rho-a', 'rhoa'), ('tau-a', 'ktaua')]:
    try:
        lo_rs, hi_rs = boot_noise_ceiling(rd, method=method)
        lo_a, hi_a = o.noise_ceiling(mat, key, normalize='rsatoolbox')
        lo_b, hi_b = o.noise_ceiling(mat, key, normalize='raw')
        check(f'boot_noise_ceiling({method}) vs oracle normalized-mean', [lo_rs, hi_rs], [lo_a, hi_a], tol=1e-7)
        print(f'          ({method}) rsatoolbox=({lo_rs:+.5f},{hi_rs:+.5f})  oracle-normalized=({lo_a:+.5f},{hi_a:+.5f})  '
              f'raw-mean[3dRSA]=({lo_b:+.5f},{hi_b:+.5f})')
    except Exception as e:  # noqa
        status.append(False); print(f'MISMATCH boot_noise_ceiling({method}) raised: {e}')

print('\nSUMMARY:', sum(status), 'of', len(status), 'oracle quantities agree with rsatoolbox')
sys.exit(0 if all(status) else 1)
