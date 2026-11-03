"""Shared helpers for the 3dRSA audit harness: synthetic data, running 3dRSA,
parsing its outputs."""
import os
import re
import subprocess
import numpy as np

from niftiio import write_nifti

BIN = os.environ.get('RSA_BIN', '/tmp/afni_build/src/3dRSA')


def run_3drsa(args, cwd, quiet=True):
    cmd = [BIN] + [str(a) for a in args]
    env = dict(os.environ, OMP_NUM_THREADS='2', AFNI_NIFTI_TYPE_WARN='NO')
    r = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, env=env)
    if r.returncode != 0:
        raise RuntimeError('3dRSA failed:\n' + ' '.join(cmd) + '\n' + r.stdout[-3000:] + r.stderr[-3000:])
    return r.stdout + r.stderr


def read_rsa_table(path):
    """Parse PREFIX.rsa.1D -> dict column -> np.array (rows = ROIs)."""
    cols = None
    rows = []
    with open(path) as f:
        for line in f:
            if line.startswith('#ROI'):
                cols = line[1:].split()
            elif line.startswith('#') or not line.strip():
                continue
            else:
                rows.append(line.split())
    assert cols is not None, path
    out = {}
    for k, c in enumerate(cols):
        vals = [r[k] for r in rows]
        try:
            out[c] = np.array([float(v) for v in vals])
        except ValueError:
            out[c] = np.array(vals)
    return out


def read_1d(path):
    return np.loadtxt(path, comments='#')


def write_mask(path, nvox_per_roi, nroi=1):
    """A tiny grid: x indexes voxels, y indexes ROIs (label = y+1)."""
    mask = np.zeros((nvox_per_roi, nroi, 1), dtype=np.int16)
    for r in range(nroi):
        mask[:, r, 0] = r + 1
    write_nifti(path, mask)


def write_patterns(path, pat):
    """pat: [ncond, nvox] -> 4D NIfTI with x=voxel, 1 ROI row, t=cond."""
    pat = np.asarray(pat, np.float32)
    nc, nv = pat.shape
    write_nifti(path, pat.T[:, None, None, :])


def write_patterns_multi(path, pats):
    """pats: [nroi][ncond, nvox] -> 4D NIfTI with y = ROI."""
    nroi = len(pats)
    nc, nv = pats[0].shape
    arr = np.zeros((nv, nroi, 1, nc), np.float32)
    for r in range(nroi):
        arr[:, r, 0, :] = pats[r].T
    write_nifti(path, arr)


def write_timeseries(path, ts):
    """ts: [nt, nvox] -> 4D NIfTI."""
    ts = np.asarray(ts, np.float32)
    write_nifti(path, ts.T[:, None, None, :])


def write_model(path, vec_or_mat, n=None):
    m = np.asarray(vec_or_mat, float)
    if m.ndim == 1:
        from oracle import squareform
        m = squareform(m)
    np.savetxt(path, m, fmt='%.8g')
    return path


def make_models(rng, n):
    """A continuous dissimilarity model and a 2-group categorical one.
    Returns dict name -> (triangle vector, is_dist)."""
    pos = rng.normal(size=(n, 2))
    cont = np.sqrt(((pos[:, None, :] - pos[None, :, :]) ** 2).sum(-1))
    cont_tri = cont[np.triu_indices(n, 1)]
    grp = np.arange(n) % 2
    cat = (grp[:, None] != grp[None, :]).astype(float)
    cat_tri = cat[np.triu_indices(n, 1)]
    return {'cont': (cont_tri, True), 'cat': (cat_tri, True)}


def student_t_p2(t, df):
    """Two-sided Student-t p-value via regularized incomplete beta."""
    x = df / (df + t * t)
    return float(betainc(df / 2.0, 0.5, x))


def betainc(a, b, x):
    """Regularized incomplete beta I_x(a,b) (Numerical Recipes continued fraction)."""
    import math
    if x <= 0:
        return 0.0
    if x >= 1:
        return 1.0
    lbeta = math.lgamma(a + b) - math.lgamma(a) - math.lgamma(b) + a * math.log(x) + b * math.log(1 - x)
    front = math.exp(lbeta)
    if x < (a + 1) / (a + b + 2):
        return front * _betacf(a, b, x) / a
    return 1.0 - front * _betacf(b, a, 1 - x) / b


def _betacf(a, b, x, maxit=300, eps=1e-15):
    tiny = 1e-300
    qab, qap, qam = a + b, a + 1, a - 1
    c, d = 1.0, 1 - qab * x / qap
    d = 1 / (d if abs(d) > tiny else tiny)
    h = d
    for m in range(1, maxit + 1):
        m2 = 2 * m
        aa = m * (b - m) * x / ((qam + m2) * (a + m2))
        d = 1 + aa * d; d = 1 / (d if abs(d) > tiny else tiny)
        c = 1 + aa / c; c = c if abs(c) > tiny else tiny
        h *= d * c
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
        d = 1 + aa * d; d = 1 / (d if abs(d) > tiny else tiny)
        c = 1 + aa / c; c = c if abs(c) > tiny else tiny
        de = d * c
        h *= de
        if abs(de - 1) < eps:
            break
    return h
