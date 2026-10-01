#!/usr/bin/env python3
"""Validate the S3 unequal-support crossnobis covariance derivation.

This is a methods-validation script, not a 3dRSA regression test.  It compares
the zero-signal analytic covariance of F21's pairwise-valid crossnobis estimator
with Monte Carlo covariance under (1) exchangeable condition-estimate noise and
(2) run/condition-dependent diagonal noise.  The latter also demonstrates why
an availability-only covariance cannot cover arbitrary repeated/design-unequal
condition estimates.
"""

import argparse
import json
import numpy as np


def contrast_matrix(ncond):
    pairs = [(i, j) for i in range(ncond) for j in range(i + 1, ncond)]
    C = np.zeros((len(pairs), ncond))
    for a, (i, j) in enumerate(pairs):
        C[a, i], C[a, j] = 1.0, -1.0
    return pairs, C


def valid_distance_runs(availability, pairs):
    return np.asarray([availability[:, i] & availability[:, j]
                       for i, j in pairs], dtype=bool)


def analytic_covariance(C, valid, sigma_k, nvox):
    """Zero-signal covariance for F21's ordered-pair average.

    sigma_k[r] is the condition-estimate covariance in run r.  Spatial noise is
    identity here, so tr(Sigma_P^2)/P^2 = 1/P.  The general spatial factor is a
    single scalar and therefore cancels from covariance-normalized RDM cosine.
    """
    ndist, nrun = valid.shape
    count = valid.sum(axis=1)
    denom = count * (count - 1)
    gamma = np.asarray([C @ sigma_k[r] @ C.T for r in range(nrun)])
    V = np.zeros((ndist, ndist))
    for a in range(ndist):
        for b in range(a, ndist):
            common = np.flatnonzero(valid[a] & valid[b])
            total = 0.0
            for ii, r in enumerate(common):
                for s in common[ii + 1:]:
                    total += gamma[r, a, b] * gamma[s, a, b]
            V[a, b] = V[b, a] = 4.0 * total / (nvox * denom[a] * denom[b])
    return V


def crossnobis_batch(patterns, pairs, valid):
    """F21 estimator for a batch: [draw, run, condition, voxel]."""
    out = np.empty((patterns.shape[0], len(pairs)))
    nvox = patterns.shape[-1]
    for a, (i, j) in enumerate(pairs):
        delta = patterns[:, valid[a], i, :] - patterns[:, valid[a], j, :]
        summed = delta.sum(axis=1)
        self_product = np.square(delta).sum(axis=(1, 2))
        q = delta.shape[1]
        out[:, a] = (np.square(summed).sum(axis=1) - self_product) / (
            q * (q - 1) * nvox)
    return out


def empirical_covariance(rng, draws, batch, nvox, sigma_k, pairs, valid):
    ndist, nrun = valid.shape
    ncond = sigma_k.shape[1]
    chol = np.linalg.cholesky(sigma_k)
    sx = np.zeros(ndist)
    sxx = np.zeros((ndist, ndist))
    done = 0
    while done < draws:
        nb = min(batch, draws - done)
        z = rng.standard_normal((nb, nrun, ncond, nvox))
        x = np.empty_like(z)
        for r in range(nrun):
            x[:, r] = np.einsum("ij,bjp->bip", chol[r], z[:, r])
        d = crossnobis_batch(x, pairs, valid)
        sx += d.sum(axis=0)
        sxx += d.T @ d
        done += nb
    return (sxx - np.outer(sx, sx) / draws) / (draws - 1)


def relative_error(observed, predicted, fit_scale=False):
    scale = 1.0
    if fit_scale:
        scale = np.sum(observed * predicted) / np.sum(predicted * predicted)
    return (np.linalg.norm(observed - scale * predicted) /
            np.linalg.norm(observed), scale)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--draws", type=int, default=60000)
    ap.add_argument("--batch", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=20260829)
    ap.add_argument("--json", action="store_true")
    args = ap.parse_args()

    ncond, nrun, nvox = 6, 6, 48
    # Two complete runs plus deliberately different partial run sets. Every
    # condition pair remains estimable, but distance support counts and overlap
    # counts vary.
    availability = np.asarray([
        [1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 0, 0],
        [1, 1, 0, 0, 1, 1],
        [1, 0, 1, 0, 1, 1],
        [0, 1, 1, 1, 0, 1],
    ], dtype=bool)
    pairs, C = contrast_matrix(ncond)
    valid = valid_distance_runs(availability, pairs)
    if valid.sum(axis=1).min() < 2:
        raise RuntimeError("fixture contains an inestimable condition pair")

    rng = np.random.default_rng(args.seed)
    sigma_exchangeable = np.repeat(np.eye(ncond)[None, :, :], nrun, axis=0)
    V_exchangeable = analytic_covariance(C, valid, sigma_exchangeable, nvox)
    E_exchangeable = empirical_covariance(
        rng, args.draws, args.batch, nvox, sigma_exchangeable, pairs, valid)
    exchangeable_error, _ = relative_error(E_exchangeable, V_exchangeable)

    # Run- and condition-dependent variance, representing unequal precision
    # after differing trial counts/designs.  This stays diagonal so the failure
    # of the support-only approximation cannot be blamed on exotic covariance.
    variance = np.asarray([
        [1.00, 0.55, 1.60, 0.75, 1.25, 0.90],
        [0.65, 1.45, 0.80, 1.35, 0.60, 1.75],
        [1.50, 0.70, 1.10, 0.50, 1.80, 0.85],
        [0.80, 1.65, 0.60, 1.20, 0.95, 1.40],
        [1.30, 0.50, 1.55, 0.90, 1.70, 0.65],
        [0.55, 1.25, 0.70, 1.80, 0.75, 1.50],
    ])
    sigma_heterogeneous = np.asarray([np.diag(v) for v in variance])
    V_general = analytic_covariance(C, valid, sigma_heterogeneous, nvox)
    E_general = empirical_covariance(
        rng, args.draws, args.batch, nvox, sigma_heterogeneous, pairs, valid)
    general_error, _ = relative_error(E_general, V_general)
    support_only_error, support_scale = relative_error(
        E_general, V_exchangeable, fit_scale=True)

    # Balanced reduction: apart from one irrelevant global scale, the derived
    # covariance must become (C C') o (C C'), the current F4 matrix.
    all_valid = np.ones((len(pairs), nrun), dtype=bool)
    V_balanced = analytic_covariance(C, all_valid, sigma_exchangeable, nvox)
    V_f4 = np.square(C @ C.T)
    balanced_error, balanced_scale = relative_error(V_balanced, V_f4, fit_scale=True)

    result = {
        "draws": args.draws,
        "conditions": ncond,
        "runs": nrun,
        "voxels": nvox,
        "support_count_range": [int(valid.sum(axis=1).min()),
                                int(valid.sum(axis=1).max())],
        "exchangeable_relative_frobenius_error": exchangeable_error,
        "heterogeneous_general_relative_frobenius_error": general_error,
        "heterogeneous_support_only_relative_frobenius_error_after_scale":
            support_only_error,
        "heterogeneous_support_only_best_scale": support_scale,
        "balanced_reduction_relative_error": balanced_error,
        "balanced_reduction_scale": balanced_scale,
        "minimum_eigenvalue_exchangeable": float(np.linalg.eigvalsh(V_exchangeable)[0]),
        "minimum_eigenvalue_general": float(np.linalg.eigvalsh(V_general)[0]),
    }

    # Monte Carlo error is stochastic but comfortably below these conservative
    # bounds at the default draw count.  The final assertion encodes the methods
    # decision: support alone is materially wrong when condition precision varies.
    assert exchangeable_error < 0.06, result
    assert general_error < 0.06, result
    assert balanced_error < 1e-12, result
    assert result["minimum_eigenvalue_exchangeable"] > -1e-12, result
    assert result["minimum_eigenvalue_general"] > -1e-12, result
    assert support_only_error > 0.10, result

    if args.json:
        print(json.dumps(result, indent=2, sort_keys=True))
    else:
        print("S3 unequal-support covariance validation")
        for key, value in result.items():
            print("%-64s %s" % (key + ":", value))


if __name__ == "__main__":
    main()
