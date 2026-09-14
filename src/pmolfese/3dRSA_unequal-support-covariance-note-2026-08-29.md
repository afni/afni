# 3dRSA method note: covariance weighting with unequal run support

**Date:** 2026-08-29  
**Milestone:** September S3  
**Status:** derivation and simulation complete; bounded implementation is
methodologically ready, general F21 enablement is not.

## Decision

Do **not** remove 3dRSA's current blanket rejection of `corr_cov`/`cosine_cov`
for all F21 `ConditionFile` inputs.

The covariance of F21's pairwise-valid crossnobis RDM can be derived exactly
under the same zero-signal approximation used by the balanced F4 metric. The
result depends on the run support shared by every *pair of RDM entries*, not
only on each entry's own number of valid runs. Under exchangeable,
equal-precision condition estimates, that support-aware covariance is fully
determined by the existing `ConditionFile` mapping.

However, repeated local condition labels are averaged within run, and real
first-level designs can give different conditions and runs different beta
precision and covariance. `ConditionFile`, repetition counts, and `ResidFile`
do not identify the required condition-estimate covariance matrices. A
support-only formula was 21.8% wrong in covariance shape in the heterogeneous
simulation below even after giving it the best possible global rescaling.

Therefore:

- **GO:** schedule a bounded future implementation for missing/reordered but
  non-repeated F21 condition mappings, explicitly using the exchangeable
  canonical-condition-estimate approximation. Limit its first slice to fixed
  model effects and paired fixed-model contrasts.
- **NO-GO:** do not describe that slice as general unequal-design covariance,
  and do not enable it for repeated labels or estimands that require additional
  covariance/pooling definitions.
- **Current release behavior:** retain the explicit rejection until the bounded
  implementation and its independent C-level numerical gate exist. S3 itself
  changes no 3dRSA estimator.

## Current estimator

Let there be `K` conditions, `R` independent runs, and `P` voxels after any
requested spatial noise normalization. Index the `D=K(K-1)/2` condition
contrasts by `a=(i,j)`, and let `c_a` be the corresponding row of the contrast
matrix `C`: `+1` for condition `i`, `-1` for condition `j`, and zero elsewhere.

For run `r`, write the condition-by-voxel pattern matrix as `B_r`. The pattern
contrast is

```text
delta[a,r] = c_a B_r .
```

Let `u[a,r]` be one when run `r` contains both conditions in distance `a`, and
zero otherwise. The valid-run set and count are

```text
S_a = {r : u[a,r] = 1},       q_a = |S_a| >= 2.
```

F21's estimator in `THD_simmat_crossnobis_valid` is

```text
          1                         T
d_hat_a = -----------------  sum   delta[a,r] delta[a,s] .
          P q_a (q_a - 1)   r!=s
                            r,s in S_a
```

It is equivalently twice the sum over unordered valid run pairs divided by
`P q_a(q_a-1)`. This ordered/unordered equivalence is important for the factor
of four in the covariance below.

## Zero-signal covariance derivation

Assume first that runs are independent and that, at the zero-signal point,

```text
B_r ~ matrix-normal(0, Sigma_K[r], Sigma_P[r]),
```

where `Sigma_K[r]` is the covariance of condition estimates in run `r` and
`Sigma_P[r]` is spatial covariance. Define

```text
gamma[a,b,r] = c_a Sigma_K[r] c_b^T
tau[r,s]     = tr(Sigma_P[r] Sigma_P[s]) / P^2 .
```

For one unordered run pair `r<s`, Isserlis' identity and run independence give

```text
Cov( delta[a,r] delta[a,s]^T / P,
     delta[b,r] delta[b,s]^T / P )
  = tau[r,s] gamma[a,b,r] gamma[a,b,s].
```

At zero signal, products from two distinct unordered run pairs are uncorrelated,
even when those pairs share one run: the unmatched independent run contributes
a zero mean. Thus only run pairs present in both distances contribute. Let

```text
I_ab = S_a intersection S_b.
```

The covariance of the actual F21 estimator is

```text
V[a,b] = 4 / {q_a(q_a-1) q_b(q_b-1)}
         * sum over r<s in I_ab
             tau[r,s] gamma[a,b,r] gamma[a,b,s].                 (1)
```

Equation (1) is the central S3 result. It is the covariance of 3dRSA's current
equal-weight, pairwise-valid estimator—not the covariance of a hypothetical
precision-pooled replacement estimator.

This is consistent with Diedrichsen et al.'s partition-pair result: at zero
signal, one run-pair distance vector has covariance proportional to
`(C Sigma_K[r] C^T) o (C Sigma_K[s] C^T)`, and distinct partition-pair products
are uncorrelated. Their unbalanced-design appendix then recommends precision
pooling complete run-pair estimates. F21 instead has entry-specific missingness,
so (1) applies the same partition-pair covariance before summing only the
available contribution for each RDM entry.

### Exchangeable-condition special case

Under the same simplified condition-noise contract as current F4,
`Sigma_K[r] = sigma^2 I`, and under a common spatial covariance,
`tau[r,s]=tau`. Let

```text
A = C C^T,       h_ab = |I_ab|.
```

Then (1) reduces to

```text
V[a,b] = 2 tau sigma^4 A[a,b]^2 h_ab(h_ab-1)
         / {q_a(q_a-1) q_b(q_b-1)}.                            (2)
```

The critical term is `h_ab`: two distances can have the same individual
support counts but different covariance because their valid-run sets overlap
differently. Replacing `R` with `q_a`, or scaling only the diagonal of the
balanced covariance, is therefore incorrect.

When every distance uses all `R` runs, `q_a=q_b=h_ab=R`, so

```text
V = {2 tau sigma^4 / [R(R-1)]} * (C C^T) o (C C^T).
```

The leading factor is common to the whole matrix and cancels from normalized
whitened cosine/correlation. This exactly recovers 3dRSA's current balanced F4
matrix.

### Positive semidefiniteness

Equation (1) is a covariance matrix by construction. It can also be written as
a sum over run pairs. For each `r<s`, mask and scale every distance by its
availability in that pair and by `1/[q_a(q_a-1)]`; call the resulting diagonal
matrix `D_rs`. The contribution is proportional to

```text
D_rs * {(C Sigma_K[r] C^T) o (C Sigma_K[s] C^T)} * D_rs.
```

Each middle matrix is positive semidefinite by the Schur product theorem, so
their scaled sum is positive semidefinite. A numerical implementation should
still use a symmetric eigendecomposition or pivoted factorization and diagnose
rank deficiency rather than add an undocumented ridge.

### What the zero-signal approximation omits

With nonzero true condition differences, run-pair products that share one run
acquire signal-dependent covariance terms. The complete balanced formula in
Diedrichsen et al. includes them. `corr_cov`/`cosine_cov` deliberately use the
zero-distance covariance as a robust, data-independent approximation, so (1)
is the matching unequal-support extension—not a claim to the full sampling
covariance under every signal.

## Reproducible simulation

The validation script is
[`tests/validate_unequal_covariance.py`](tests/validate_unequal_covariance.py).
Run it from the AFNI repository root with:

```bash
build/venv/bin/python \
  src/pmolfese/tests/validate_unequal_covariance.py --json
```

The fixed fixture uses 6 conditions, 6 runs, 48 independent voxels, 60,000
zero-signal draws, and partial run mappings that yield 2–4 valid runs per
distance. It reconstructs F21's ordered-pair estimator directly and compares
its empirical covariance with (1).

| Validation | Relative Frobenius error |
|---|---:|
| Exchangeable noise: empirical vs support-aware equation (2) | 0.0119 |
| Run/condition-dependent variance: empirical vs general equation (1) | 0.0118 |
| Same heterogeneous simulation vs support-only equation (2), after optimal global rescaling | **0.2184** |
| Balanced analytic reduction vs `(C C') o (C C')`, after global rescaling | **0.0000** |

Both analytic covariance matrices were positive definite in this fixture
(minimum eigenvalues 0.00846 and 0.00981). The remaining approximately 1.2%
analytic-versus-empirical discrepancy is ordinary finite Monte Carlo error. The
script asserts conservative 6% validation bounds, exact balanced reduction,
nonnegative eigenvalues, and at least 10% structural failure of the deliberately
mis-specified support-only covariance under heterogeneous precision.

## Why current F21 metadata is insufficient for the general formula

`ConditionFile` identifies availability and `nrep` records how many local beta
bricks were averaged into a canonical condition. Neither defines
`Sigma_K[r]`:

- two repeated beta bricks need not be statistically independent;
- beta covariance depends on the first-level design matrix and temporal noise,
  not merely on repetition count;
- a run's residual time-series dataset estimates spatial/temporal residual
  structure, but without the design covariance it does not identify covariance
  among the estimated condition coefficients; and
- run-specific residual spatial covariance would also make `tau[r,s]` vary,
  whereas the current F4 approximation treats its spatial contribution as a
  common scale that cancels from normalized comparison.

Assuming variance `1/nrep` would silently invent an independent-repeat model
that the input contract never promised. General enablement therefore needs an
explicit per-run beta/design covariance input or a separately validated method
for deriving it from first-level metadata.

## Supported-estimand decision for a bounded implementation

The proposed bounded slice would require `ConditionFile`, prohibit any
`nrep>1`, and record the exchangeable canonical-condition-estimate approximation
in output metadata. Its covariance `V_s` is subject-specific because support
can differ by subject, but is constant across that subject's ROIs/searchlights.

| Estimand/workflow | Decision | Reason |
|---|:---:|---|
| Separate fixed `-model_mat` / `-model_series` effect with `corr_cov` or `cosine_cov` | **GO** | Direct subject-specific `V_s^-1` inner product. |
| Paired fixed-model contrast | **GO** | Both models use the same subject-specific covariance and paired null. |
| Population subject sign-flip null | **GO** | Operates on the resulting subject scores; support remains fixed. |
| Fixed-observed-subject condition-label null | **GO** | Relabel the model; keep measurement covariance attached to neural RDM entries. |
| Subject bootstrap, ROI/searchlight, synchronized spatial max-FWE | **GO** | Support/factorizations are fixed and cacheable outside location loops. |
| `noise_norm none|diag|shrinkage` | **GO under current common-spatial-shape approximation** | A common residual spatial factor is a global scale and cancels. |
| Repeated conditions within a run | **NO-GO** | `nrep` does not identify beta covariance. |
| Condition or dual bootstrap | **HOLD** | Resampling changes duplicated entries and covariance/support structure per draw. |
| Noise ceilings | **HOLD** | Different subject precision raises a separate group-template pooling decision. |
| Joint regression, `-ortvec`, commonality | **HOLD** | Requires an explicit GLS effect/R² and reduced-null contract. |
| Fitted component models | **HOLD** | Requires fold-specific covariance and weighted fitting/scoring rules. |
| Runwise second-order IS-RSA | **HOLD** | The first slice is classic fixed-model RDM comparison only. |

For `corr_cov`, subtract the ordinary triangle mean before applying `V_s^-1`,
preserving the existing metric and rsatoolbox definition. `cosine_cov` retains
the crossnobis zero. Model and neural triangles must use the same factorization.

## Future implementation outline

1. Build each subject's distance-by-run availability bitset from `nrep>0`.
2. Reject repeated labels for the bounded slice.
3. Construct the `D x D` support-aware covariance in (2), once per distinct
   subject support signature.
4. Factor it once; use a Moore–Penrose inverse only on a numerically documented
   estimable subspace, and reject a degenerate model norm.
5. Cache subject-specific transformed fixed models and reuse the factors across
   every ROI/searchlight and synchronized permutation.
6. Verify direct dense-`V^-1` effects, paired contrasts, both null families,
   bootstrap, atlas/searchlight max-FWE, and thread identity. Include fixtures
   with equal counts but different support overlap to catch omission of `h_ab`.
7. Preserve the current rejection for every workflow marked HOLD/NO-GO.

The balanced centered-kernel shortcut is not generally available because
support masks destroy its common `C C'` structure. The first implementation
should prefer a transparent dense factorization over an unreviewed optimization;
support factors are location-invariant, so the cubic cost is paid per subject
or distinct support signature, not per searchlight.

## Review checks completed

- Factor-of-two/four checked independently using ordered and unordered run-pair
  forms of the F21 estimator.
- Balanced limit recovers the exact F4 covariance up to the expected global
  factor `2 tau sigma^4/[R(R-1)]`.
- Positive-semidefinite representation checked analytically and numerically.
- Exchangeable and heterogeneous formulas checked against independently
  generated Monte Carlo patterns, not against 3dRSA covariance code.
- General equation cross-checked against the partition-pair covariance and
  unbalanced-design appendix of Diedrichsen et al.
- Input-identifiability and downstream-estimand boundaries reviewed separately
  from the covariance algebra.

## Sources

- Diedrichsen J, Berlot E, Mur M, Schütt HH, Kriegeskorte N.
  [Comparing representational geometries using the unbiased distance
  correlation](https://arxiv.org/abs/2007.02789). In particular, the
  zero-signal covariance, condition-covariance substitution, and unbalanced
  partition appendix (equations 53–58).
- Diedrichsen J, Provost S, Zareamoghaddam H.
  [On the distribution of cross-validated Mahalanobis
  distances](https://arxiv.org/abs/1607.01371). This gives the broader
  signal-dependent crossnobis sampling covariance and discusses unequal
  partition designs.
- rsatoolbox official documentation:
  [whitened RDM comparison](https://rsatoolbox.readthedocs.io/en/stable/comparing.html)
  and the
  [`V^-1` implementation](https://rsatoolbox.readthedocs.io/en/latest/_modules/rsatoolbox/rdm/compare.html).
- Local implementation audited:
  `THD_simmat_crossnobis_valid` and `THD_rdm_cov_transform` in
  `src/thd_simmatrix.c`, plus the F4 contract checks in `src/3dRSA.c`.
