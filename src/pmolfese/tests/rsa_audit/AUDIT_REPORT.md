# 3dRSA computational-accuracy audit

Original audit: 2026-10-30; verification and correction: 2026-11-02  
Binary audited: `3dRSA` built from this checkout (AFNI_26.3.05) on Linux aarch64  
Harness: `src/pmolfese/tests/rsa_audit/` (this directory)  
Oracle: rsatoolbox formula conventions, re-implemented in numpy (`oracle.py`); see "How the oracle was obtained" for the important caveat.

## Summary

The original audit exercised 567 numerical checks. Follow-up verification found
one substantive issue in classic-RSA noise-ceiling construction and aggregation.
That issue is now corrected in the source: comparator-specific subject templates
are pooled, and subject scores use the same clipped Fisher-z aggregate as model
`_r`. These remain inclusive and leave-one-out reference estimates, not guaranteed
bounds on the Fisher-aggregated objective.

The five original first-order mismatches were caused by the NumPy oracle splitting
an exact Spearman tie, not by 3dRSA. The oracle now uses a symmetric correlation
denominator. No production Spearman code change was needed. See Finding 2.

The original Linux audit used AFNI_26.3.05. Follow-up verification reproduced the
reported outputs using the installed macOS binary, compiled the relevant current
C correlation functions independently, and rebuilt 3dRSA to validate the ceiling
change. Tests 3 and 5 now assert normalized-template Fisher ceilings; arithmetic
aggregation is retained only as a convention comparison.

Validation of the rebuilt macOS binary: **619/619 strict checks passed**
(240 first-order, 135 crossnobis, 196 classic group, 12 inference/second-order,
36 group crossnobis). The expanded classic-group checks include tau-b and verify
that the inclusive template supplied as a fixed model reproduces `nc_high`.
Rank template sums are retained until leave-one-out subtraction to avoid splitting
exact ties through intermediate mean rounding.

## How the oracle was obtained (read this first)

The sandbox used for this audit has no PyPI or GitHub access, so `rsatoolbox` itself could not be installed or its source fetched. The oracle (`oracle.py`) is therefore a from-scratch numpy implementation of the rsatoolbox conventions for `rdm.calc` (euclidean, correlation, mahalanobis, crossnobis), `data.noise.prec_from_residuals` (diag, full, shrinkage_eye = Ledoit-Wolf toward a scaled identity), `rdm.compare` (cosine, corr, spearman, rho-a, tau-a, tau-b, corr_cov, cosine_cov using the explicit V = (CC') ∘ (CC') weighting), and `inference.boot_noise_ceiling`, plus an exact active-set NNLS. The rsatoolbox noise-ceiling normalization convention was additionally confirmed against the maintainers' own description in rsatoolbox discussion #388 ("For cosine similarity, they are normalized to unit norm. For Pearson correlation, the RDM vectors are normalized to zero mean and unit standard deviation... For the Spearman correlation, subject RDM vectors are first transformed to ranks").

To close the loop, `rsatoolbox_crosscheck.py` recomputes every oracle quantity with the real package. It needs only `pip install rsatoolbox scipy` and takes a few seconds; run it on your Mac and any line marked `MISMATCH` identifies an oracle quantity whose expectation in this audit should be re-examined. Nothing in the findings below depends on a quantity I was unsure about, and the two actionable findings were confirmed directly by reading the C source.

## What was checked, and the result

Test 1 (`test1_first_order.py`, single subject, `-classic_null conditions`, so the reported `_r` is exactly the subject's r). Every combination of `-neural_metric {corr, scorr, cosine, euclid}` × `-metric {spearman, pearson, rhoa, ktaua, ktaub}` × `-center_conditions {none, subject}` against a continuous distance model and a two-group categorical model with heavy ties, three random replicates. The original oracle produced five mismatches in one scorr replicate. Correcting its exact-tie handling resolves all five (see Finding 2).

Test 2 (`test2_crossnobis.py`, `-runwiseTable`, 3 runs). The full crossnobis RDM is recovered entry by entry via indicator models (with `-metric pearson`, r_k is an exact affine function of distance k), for voxel counts below, near and above the residual sample count, with `-noise_norm none/diag/shrinkage`. Max entrywise error 6e-7 in all nine configurations, including the shrinkage case where p = 90 voxels > n = 60 residual samples. The explicit covariance-weighted comparators corr_cov/cosine_cov agree with the slow V⁻¹ definition (3dRSA uses the centered second-moment identity; I verified numerically that the two are identical, and 3dRSA's results match to 1e-6). 135/135.

Test 3 (`test3_group.py`, 12 subjects). `<model>_r` equals tanh(mean atanh r_s) and, with `-nperm 0`, `<model>_p` equals the two-sided Student t on the Fisher z values (144/144 strict checks). The `-model_fit` nested leave-one-subject-out standardized nonnegative fit matches an exact active-set NNLS oracle for both `-fit_ridge 0` and `0.01` in `_cvR` and in the normalized mean weights, so the 1000-iteration coordinate-descent solver converges correctly on these designs. The original ceilings matched raw-mean templates with arithmetic aggregation. The updated checks require comparator-specific templates with Fisher aggregation (Finding 1).

Test 4 (`test4_inference.py`). Permutation p-values for `-classic_null conditions` agree with an independent 20,000-relabeling Monte-Carlo p within 0.8 standard errors; the relabeling is applied jointly to rows and columns as it should be. Subject sign-flip p-values agree with the exact enumeration of all 2¹¹ sign patterns to all printed digits (3dRSA enumerates exactly when 2ⁿ ≤ nperm; confirmed in `thd_permute.c`). The `-featuretype rdm` second-order subject-by-subject matrix saved by `-save_rdm` matches the oracle for all four inner/outer metric combinations. 12/12.

Test 5 (`test5_group_crossnobis.py`, 9 subjects, runwise, `-noise_norm none/shrinkage`). Group-level corr_cov, cosine_cov and spearman effects and parametric p-values on crossnobis RDMs: 36/36.

## Finding 1 (medium, fixed): classic-RSA noise ceiling used inconsistent templates and aggregation

Location: `3dRSA.c` ~lines 9763–9795 (`do_nc` block in the classic path).

The following describes the original implementation; the correction is described below. Two independent issues combined here.

First, the template. 3dRSA averages the raw subject RDM triangles and correlates each subject with that raw mean. rsatoolbox and the Nili et al. (2014) procedure that the help text cites instead average RDMs that have been transformed to match the comparator (z-scored for Pearson, ranked for Spearman/rho-a, unit-norm for cosine, V⁻¹-norm for the whitened measures). For Pearson this is not a stylistic preference: the mean over subjects of corr(RDM_s, t) is maximized by t ∝ Σ_s z(RDM_s), so the raw mean yields a value that is strictly below the best achievable fit. In the simulations the raw-template `nc_high` was lower than the rsatoolbox-style value in all 22 metric/data combinations tested, by 0.0004–0.013, and `nc_low` differed by up to ±0.015 in either direction.

Second, the aggregation. `nc_low`/`nc_high` are arithmetic means of the per-subject correlations, whereas `<model>_r` is tanh(mean atanh r_s). For nonnegative correlations, convexity of atanh makes the Fisher aggregate at least as large as the arithmetic mean. This is not a general inequality for signed correlations. In the test-3 data the difference was about 0.004.

Demonstration (test-3 data, `-neural_metric corr`): feeding 3dRSA a `-model_mat` that is literally the raw group-mean RDM gives `rawmean_r = 0.4082` against `nc_high = 0.4041` under `-metric pearson`; the z-scored mean gives 0.4084, and under `-metric spearman` the rank-mean gives 0.4031 versus `nc_high = 0.3955`. A user reading "model reaches/exceeds the ceiling" would be misled. Recomputing the ceiling with both a transformed template and the Fisher aggregate reproduces those model values exactly (0.4084 and 0.4031), These template scores are attainable, but attainability does not imply that they bound every model score. A small gradient step from the normalized Pearson template increased the Fisher score from 0.408438 to 0.408486.

Implemented correction: build separate template copies without modifying subject
RDMs used for model inference. Pearson templates are centered and normalized to
unit norm. Spearman/rho-a/Kendall templates use average ranks. Covariance-weighted
templates use unit Frobenius norm in the transformed second-moment representation
(with triangle-mean removal already applied for corr_cov). Constant zero-norm
inputs contribute zero vectors. Inclusive and leave-one-out correlations are
aggregated with the same clipped Fisher transform as model `_r`.

The help and output metadata now describe these as reference estimates. Normalized
pooling optimizes the arithmetic Pearson objective, not the Fisher objective;
Kendall rank pooling is heuristic. A model can still exceed `nc_high`. IS-RSA
`reliability` is unchanged.

## Finding 2 (withdrawn): the oracle split an exact Spearman tie

The original report attributed five test-1 mismatches to float32 accumulation in
`THD_simmat_fill_from_features`. Direct compilation of both `THD_pearson_corr`
and the proposed `simmat_pearson_corr` alternative disproved this reproducer:
both return equal values for the two affected RDM entries.

For zero-based condition pairs (1, 7) and (3, 5), the centered rank dot product is
79.5 and the squared norms are 407.5 and 408, with the latter reversed between
pairs. Both correlations are exactly `79.5 / sqrt(407.5 * 408)` (not necessarily
rational). The oracle's `np.corrcoef` divided by standard deviations sequentially,
producing 0.19497244621142565 and 0.19497244621142568. Ranking those unequal floating
values incorrectly breaks the tie.

The corrected oracle calculates each rank correlation using a single symmetric
denominator. The downstream continuous-model Spearman value is 0.07091833466,
matching 3dRSA's 0.070918, rather than the old oracle's 0.07387387387. All five
original discrepancies then fall below 3.7e-7. Changing the production C code to
double accumulation is unnecessary for this case and does not alter these results.
This verification does not claim float accumulation is immune to every possible
rounding issue; it establishes that this reported example is an oracle defect.

## Convention differences versus rsatoolbox (not bugs; consider documenting)

These are deliberate design choices that produce different numbers from rsatoolbox for the same input. None is wrong, but users porting analyses between the two tools will see them.

`-neural_metric euclid` is the plain Euclidean distance sqrt(Σ(a−b)²); rsatoolbox's `euclidean` is the squared distance divided by the channel count. Rank-based comparators (spearman, rhoa, ktaua, ktaub) are invariant to this; pearson and the ensuing Fisher-z aggregate are not. Crossnobis is, by contrast, exactly rsatoolbox's definition (squared, per-channel, mean over run pairs), so the ordinary and runwise distances are not on the same scale within 3dRSA either.

`-neural_metric corr` returns the similarity r, not the distance 1−r; the help already says so. A dissimilarity model correlated against it yields the negated value, and 3dRSA only warns about the mixed sense. This is intentional and documented, but it is the most likely source of sign confusion for rsatoolbox users.

`-noise_norm diag` floors residual variances at the upper median of the positive variances. rsatoolbox's `diag` applies no floor. With a genuinely quiet voxel in the test the two whitened RDMs correlated only 0.1–0.7 with each other; this is a large, intentional, documented difference. `-noise_norm shrinkage` matches `shrinkage_eye` except that 3dRSA divides by the number of residual samples where rsatoolbox's default dof is n−1; the resulting RDM entries differ by <5% relative and correlate >0.99999, which is negligible.

Group aggregation uses tanh(mean atanh r_s) and a sign-flip test on Fisher z; rsatoolbox's `eval_fixed` averages raw correlations (there is an open rsatoolbox issue, #370, proposing Fisher z). Either is defensible; 3dRSA's ceilings now use the same aggregate as its model scores (Finding 1).

`-model_fit` standardizes the components and response inside each fold before the nonnegative fit; rsatoolbox's `fit_regress_nn` fits unstandardized RDMs. With the default `-fit_ridge 0.01` it is also a ridge, not an NNLS. Both are documented in the help; the solver itself is correct for its stated objective.

## Reproducing

```
# in a shell with a 3dRSA binary on PATH or via RSA_BIN
cd src/pmolfese/tests/rsa_audit
RSA_BIN=/path/to/3dRSA python3 run_all.py /tmp/rsa_audit      # numpy only
pip install rsatoolbox scipy && python3 rsatoolbox_crosscheck.py  # validates oracle.py against rsatoolbox
```

`run_all.py` exits nonzero on any strict failure. The exact-tie oracle correction preserves the original tolerance. Convention gaps are printed but not counted. The harness writes its own NIfTI files (`niftiio.py`) and needs no nibabel or scipy.

## Not covered

IS-RSA behavioral model rules (annak/nn/euclid/match), `-model_dset`, `-model_series`, temporal nulls, bootstrap intervals, commonality/contrast inference, searchlight, surface, `-subject_outcome` regression, F21/S6 unbalanced crossnobis. The existing developer tests in `src/pmolfese/tests/` cover some of these.
