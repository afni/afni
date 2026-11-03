"""Standalone numpy oracle reproducing rsatoolbox's reference formulas.

This file re-implements -- from the rsatoolbox source conventions
(rsatoolbox.rdm.calc, rsatoolbox.rdm.compare, rsatoolbox.data.noise,
rsatoolbox.inference.noise_ceiling) -- the arithmetic that 3dRSA's core
numerics are audited against.  It deliberately uses only numpy so it can run
in an offline sandbox.  `rsatoolbox_crosscheck.py` re-derives the same
quantities with the real rsatoolbox package where that is installable.

Conventions (rsatoolbox):
  * measurements are [n_cond, n_channel]
  * RDMs are strict-upper-triangle vectors in row-major (i<j) order
  * 'euclidean' is the SQUARED Euclidean distance divided by n_channel
  * 'correlation' is 1 - Pearson r
  * crossnobis averages (p_i^a - p_j^a)' W (p_i^b - p_j^b) / n_channel over
    all fold pairs a<b
"""
import itertools
import numpy as np


# ----------------------------------------------------------------------------
# utilities
# ----------------------------------------------------------------------------
def triu(mat):
    n = mat.shape[0]
    return mat[np.triu_indices(n, 1)]


def squareform(vec):
    m = len(vec)
    n = int(round((1 + np.sqrt(1 + 8 * m)) / 2))
    assert n * (n - 1) // 2 == m
    out = np.zeros((n, n))
    out[np.triu_indices(n, 1)] = vec
    return out + out.T


def rankdata(x):
    """Average ranks, 1-based (scipy.stats.rankdata default 'average')."""
    x = np.asarray(x, dtype=float)
    order = np.argsort(x, kind='mergesort')
    xs = x[order]
    ranks = np.empty(len(x))
    i = 0
    while i < len(x):
        j = i
        while j + 1 < len(x) and xs[j + 1] == xs[i]:
            j += 1
        ranks[order[i:j + 1]] = 0.5 * (i + 1 + j + 1)
        i = j + 1
    return ranks


def pearson(a, b):
    a = np.asarray(a, float) - np.mean(a)
    b = np.asarray(b, float) - np.mean(b)
    den = np.sqrt((a @ a) * (b @ b))
    return float(a @ b / den) if den > 0 else 0.0


# ----------------------------------------------------------------------------
# rsatoolbox.rdm.calc
# ----------------------------------------------------------------------------
def calc_rdm_euclidean(meas):
    meas = np.asarray(meas, float)
    ss = np.sum(meas ** 2, axis=1, keepdims=True)
    rdm = ss + ss.T - 2 * meas @ meas.T
    return triu(rdm) / meas.shape[1]


def calc_rdm_correlation(meas):
    ma = np.asarray(meas, float)
    ma = ma - ma.mean(axis=1, keepdims=True)
    ma = ma / np.sqrt(np.einsum('ij,ij->i', ma, ma))[:, None]
    return triu(1 - ma @ ma.T)


def calc_rdm_mahalanobis(meas, noise=None):
    meas = np.asarray(meas, float)
    if noise is None:
        return calc_rdm_euclidean(meas)
    kernel = meas @ noise @ meas.T
    d = np.diag(kernel)
    rdm = d[None, :] + d[:, None] - 2 * kernel
    return triu(rdm) / meas.shape[1]


def _crossnobis_single(m1, m2, noise):
    kernel = m1 @ noise @ m2.T
    d = np.diag(kernel)
    rdm = d[None, :] + d[:, None] - kernel - kernel.T
    return triu(rdm) / m1.shape[1]


def calc_rdm_crossnobis(folds, noise=None):
    """folds: list of [n_cond, n_channel] arrays, one per independent run."""
    folds = [np.asarray(f, float) for f in folds]
    nch = folds[0].shape[1]
    if noise is None:
        noise = np.eye(nch)
    rdms = [_crossnobis_single(folds[i], folds[j], noise)
            for i in range(len(folds)) for j in range(i + 1, len(folds))]
    return np.mean(rdms, axis=0)


# 3dRSA-native (non-rsatoolbox) first-order metrics, for completeness
def simmat_pearson(meas):
    return triu(np.corrcoef(np.asarray(meas, float)))


def simmat_spearman(meas):
    r = np.array([rankdata(row) for row in np.asarray(meas, float)])
    # Form each correlation with a single symmetric denominator. np.corrcoef
    # divides by the two SDs sequentially, which can split exact equalities
    # when their order is reversed and corrupt downstream rank comparisons.
    return np.array([pearson(r[i], r[j])
                     for i in range(len(r)) for j in range(i + 1, len(r))])


def simmat_cosine(meas):
    m = np.asarray(meas, float)
    nm = m / np.linalg.norm(m, axis=1, keepdims=True)
    return triu(nm @ nm.T)


def simmat_euclid(meas):
    """3dRSA 'euclid': plain (unsquared, unnormalized) Euclidean distance."""
    m = np.asarray(meas, float)
    return np.sqrt(calc_rdm_euclidean(m) * m.shape[1])


# ----------------------------------------------------------------------------
# rsatoolbox.data.noise
# ----------------------------------------------------------------------------
def _covariance_eye(matrix, dof):
    """Ledoit-Wolf shrinkage toward a scaled identity (rsatoolbox 'shrinkage_eye')."""
    n, p = matrix.shape
    s_sum = np.zeros((p, p))
    s2_sum = np.zeros((p, p))
    for row in matrix:
        xt_x = np.outer(row, row)
        s_sum += xt_x
        s2_sum += xt_x ** 2
    s = s_sum / dof
    b2 = np.sum(s2_sum / dof - s * s) / dof
    m = np.trace(s) / p
    d2 = np.sum((s - m * np.eye(p)) ** 2)
    b2 = min(d2, b2)
    s_shrink = b2 / d2 * m * np.eye(p) + (d2 - b2) / d2 * s
    return s_shrink, b2 / d2


def _covariance_diag(matrix, dof):
    return np.diag(np.sum(matrix ** 2, axis=0) / dof)


def _covariance_full(matrix, dof):
    return matrix.T @ matrix / dof


def cov_from_residuals(residuals, dof=None, method='shrinkage_eye', demean=True):
    """residuals: [n_time, n_channel] (one run) or list of runs (each demeaned
    separately, then stacked -- 3dRSA's convention)."""
    if isinstance(residuals, (list, tuple)):
        mats = [np.asarray(r, float) for r in residuals]
        if demean:
            mats = [r - r.mean(axis=0, keepdims=True) for r in mats]
        matrix = np.vstack(mats)
    else:
        matrix = np.asarray(residuals, float)
        if demean:
            matrix = matrix - matrix.mean(axis=0, keepdims=True)
    if dof is None:
        dof = matrix.shape[0] - 1
    if method == 'shrinkage_eye':
        return _covariance_eye(matrix, dof)
    if method == 'diag':
        return _covariance_diag(matrix, dof), None
    if method == 'full':
        return _covariance_full(matrix, dof), None
    raise ValueError(method)


def prec_from_residuals(residuals, dof=None, method='shrinkage_eye', demean=True):
    cov, shrink = cov_from_residuals(residuals, dof, method, demean)
    return np.linalg.inv(cov), shrink


# ----------------------------------------------------------------------------
# rsatoolbox.rdm.compare
# ----------------------------------------------------------------------------
def compare_cosine(v1, v2):
    v1 = np.asarray(v1, float); v2 = np.asarray(v2, float)
    return float(v1 @ v2 / np.sqrt((v1 @ v1) * (v2 @ v2)))


def compare_correlation(v1, v2):
    v1 = np.asarray(v1, float) - np.mean(v1)
    v2 = np.asarray(v2, float) - np.mean(v2)
    return compare_cosine(v1, v2)


def compare_spearman(v1, v2):
    return compare_correlation(rankdata(v1), rankdata(v2))


def compare_rho_a(v1, v2):
    """Expected Spearman under random tie breaking (rsatoolbox 'rho-a')."""
    r1 = rankdata(v1); r2 = rankdata(v2)
    n = len(r1)
    r1 = r1 - r1.mean(); r2 = r2 - r2.mean()
    return float(r1 @ r2 / (n ** 3 - n) * 12)


def _concordance(v1, v2):
    v1 = np.asarray(v1, float); v2 = np.asarray(v2, float)
    s1 = np.sign(v1[:, None] - v1[None, :])
    s2 = np.sign(v2[:, None] - v2[None, :])
    n = len(v1)
    iu = np.triu_indices(n, 1)
    prod = (s1 * s2)[iu]
    nc = np.sum(prod > 0); nd = np.sum(prod < 0)
    t1 = np.sum(s1[iu] == 0); t2 = np.sum(s2[iu] == 0)
    return nc, nd, t1, t2, n * (n - 1) // 2


def compare_kendall_tau_a(v1, v2):
    nc, nd, t1, t2, n0 = _concordance(v1, v2)
    return float((nc - nd) / n0)


def compare_kendall_tau_b(v1, v2):
    """scipy.stats.kendalltau default (tau-b)."""
    nc, nd, t1, t2, n0 = _concordance(v1, v2)
    return float((nc - nd) / np.sqrt((n0 - t1) * (n0 - t2)))


def pairwise_contrast_matrix(n):
    """rsatoolbox.util.matrix.pairwise_contrast: [n_dist, n_cond] with +1/-1."""
    pairs = list(itertools.combinations(range(n), 2))
    C = np.zeros((len(pairs), n))
    for k, (i, j) in enumerate(pairs):
        C[k, i] = 1; C[k, j] = -1
    return C


def get_v(n_cond, sigma_k=None):
    """V = (C Sigma_k C') o (C Sigma_k C')  (Diedrichsen et al. 2021)."""
    C = pairwise_contrast_matrix(n_cond)
    if sigma_k is None:
        sigma_k = np.eye(n_cond)
    xi = C @ sigma_k @ C.T
    return xi * xi


def compare_cosine_cov_weighted(v1, v2, sigma_k=None):
    """'slow' definition: cosine in the V^{-1} metric."""
    v1 = np.asarray(v1, float); v2 = np.asarray(v2, float)
    n = int(round((1 + np.sqrt(1 + 8 * len(v1))) / 2))
    V = get_v(n, sigma_k)
    w1 = np.linalg.solve(V, v1); w2 = np.linalg.solve(V, v2)
    return float(v1 @ w2 / np.sqrt((v1 @ w1) * (v2 @ w2)))


def compare_correlation_cov_weighted(v1, v2, sigma_k=None):
    v1 = np.asarray(v1, float) - np.mean(v1)
    v2 = np.asarray(v2, float) - np.mean(v2)
    return compare_cosine_cov_weighted(v1, v2, sigma_k)


def second_moment_G(vec):
    """Centered second-moment matrix G = -1/2 H D H from an RDM vector."""
    D = squareform(vec)
    n = D.shape[0]
    H = np.eye(n) - np.ones((n, n)) / n
    return -0.5 * H @ D @ H


def compare_cosine_G(v1, v2):
    """Frobenius cosine of G matrices (the identity 3dRSA relies upon)."""
    g1 = second_moment_G(v1); g2 = second_moment_G(v2)
    return float(np.sum(g1 * g2) / np.sqrt(np.sum(g1 * g1) * np.sum(g2 * g2)))


COMPARE = {
    'pearson': compare_correlation,
    'spearman': compare_spearman,
    'rhoa': compare_rho_a,
    'ktaua': compare_kendall_tau_a,
    'ktaub': compare_kendall_tau_b,
    'corr_cov': compare_correlation_cov_weighted,
    'cosine_cov': compare_cosine_cov_weighted,
    'cosine': compare_cosine,
}


# ----------------------------------------------------------------------------
# rsatoolbox.inference.noise_ceiling.boot_noise_ceiling
# ----------------------------------------------------------------------------
def _nc_transform(rdm_mat, method):
    """rsatoolbox normalizes each subject RDM before averaging so that the
    mean is the best-fitting template under the comparison measure
    (rsatoolbox discussion #388): unit norm for cosine, z-score for
    Pearson, ranks for Spearman/rho-a (and, lacking an optimum, tau-a),
    and the V^{-1}-induced norm for the whitened measures. The latter is
    our comparator-specific convention, not the current rsatoolbox pool_rdm
    implementation (which uses ordinary Euclidean norms for cov metrics)."""
    rdm_mat = np.asarray(rdm_mat, float).copy()
    if method == 'cosine':
        rdm_mat = rdm_mat / np.sqrt(np.sum(rdm_mat ** 2, axis=1, keepdims=True))
    elif method in ('pearson', 'corr'):
        rdm_mat = rdm_mat - rdm_mat.mean(axis=1, keepdims=True)
        rdm_mat = rdm_mat / np.sqrt(np.sum(rdm_mat ** 2, axis=1, keepdims=True))
    elif method in ('cosine_cov', 'corr_cov'):
        n = int(round((1 + np.sqrt(1 + 8 * rdm_mat.shape[1])) / 2))
        Vinv = np.linalg.inv(get_v(n))
        if method == 'corr_cov':
            rdm_mat = rdm_mat - rdm_mat.mean(axis=1, keepdims=True)
        nrm = np.sqrt(np.einsum('ij,jk,ik->i', rdm_mat, Vinv, rdm_mat))
        rdm_mat = rdm_mat / nrm[:, None]
    elif method in ('spearman', 'rhoa', 'ktaua', 'ktaub'):
        rdm_mat = np.array([rankdata(r) for r in rdm_mat])
    return rdm_mat


def noise_ceiling(rdm_mat, method, normalize='rsatoolbox', fisher=False):
    """Leave-one-out lower bound and full-set upper bound.

    normalize='rsatoolbox': each RDM is normalized/ranked as rsatoolbox's
       boot_noise_ceiling does before averaging.
    normalize='raw': legacy unnormalized subject templates.
    fisher=True: use 3dRSA's clipped Fisher aggregation for subject scores.
    Returns (lower, upper).
    """
    rdm_mat = np.asarray(rdm_mat, float)
    cmp = COMPARE[method]
    mean_src = _nc_transform(rdm_mat, method) if normalize == 'rsatoolbox' else rdm_mat
    n = rdm_mat.shape[0]
    full = mean_src.mean(axis=0)
    lo = []; hi = []
    for i in range(n):
        others = np.delete(mean_src, i, axis=0).mean(axis=0)
        hi.append(cmp(rdm_mat[i], full))
        lo.append(cmp(rdm_mat[i], others))
    if fisher:
        return tuple(float(np.tanh(np.mean(np.arctanh(np.clip(v, -0.999329, 0.999329)))))
                     for v in (lo, hi))
    return float(np.mean(lo)), float(np.mean(hi))


# ----------------------------------------------------------------------------
# nonnegative least squares (Lawson-Hanson active set), for -model_fit checks
# ----------------------------------------------------------------------------
def nnls(A, b, maxiter=None, tol=1e-12):
    A = np.asarray(A, float); b = np.asarray(b, float)
    m, n = A.shape
    x = np.zeros(n)
    P = np.zeros(n, bool)
    w = A.T @ (b - A @ x)
    it = 0
    maxiter = maxiter or 3 * n
    while (~P).any() and np.max(w[~P]) > tol:
        j = np.argmax(np.where(P, -np.inf, w))
        P[j] = True
        while True:
            s = np.zeros(n)
            s[P] = np.linalg.lstsq(A[:, P], b, rcond=None)[0]
            if np.all(s[P] > 0):
                x = s
                break
            mask = P & (s <= 0)
            alpha = np.min(x[mask] / (x[mask] - s[mask]))
            x = x + alpha * (s - x)
            P = P & (x > 1e-15)
            x[~P] = 0
        w = A.T @ (b - A @ x)
        it += 1
        if it > 50 * n:
            break
    return x


def nnls_ridge_standardized(y, X, ridge):
    """3dRSA's objective: z-score y and columns of X (population SD), then
    min ||y - Xw||^2 + ridge*m*||w||^2  s.t. w >= 0."""
    y = np.asarray(y, float); X = np.asarray(X, float)
    m, p = X.shape
    ym, ys = y.mean(), y.std()
    xm, xs = X.mean(axis=0), X.std(axis=0)
    yz = (y - ym) / ys
    Xz = (X - xm) / xs
    if ridge > 0:
        A = np.vstack([Xz, np.sqrt(ridge * m) * np.eye(p)])
        bb = np.concatenate([yz, np.zeros(p)])
    else:
        A, bb = Xz, yz
    w = nnls(A, bb)
    return w, xm, xs


def fisher_mean(rs, clip=0.999329):
    rs = np.clip(np.asarray(rs, float), -clip, clip)
    return float(np.tanh(np.mean(np.arctanh(rs))))
