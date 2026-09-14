# 3dMVPA Roadmap

## Purpose and product boundary

This document proposes a staged implementation of `3dMVPA`: an AFNI-native
tool for supervised multivariate pattern classification in atlas ROIs and
moving searchlights.  The first release is deliberately narrow enough to be
made numerically trustworthy, but broad enough to perform a complete basic
fMRI MVPA analysis and to apply the same decoder to generic 1D feature vectors
from EEG, behavior, physiology, or already-reduced imaging data.

The design starts from the infrastructure built for `3dRSA` rather than from a
second standalone stack.  In particular, it reuses the `THD_datatable` input
framework, ROI/searchlight extraction in `thd_patterns`, relabeling and
max-statistic inference in `thd_permute`, and the map/output conventions that
are currently partly embedded in `3dRSA.c`.

`3dMVPA` is not an RSA mode and should not be added as another branch inside
`3dRSA`.  The programs answer different questions and need different primary
objects:

- `3dRSA` compares representational geometries.
- `3dMVPA` learns a mapping from feature vectors to labels and evaluates it on
  held-out samples.
- They should share data, geometry, resampling, map inference, and output
  machinery while keeping their scientific engines separate.

This roadmap uses the status symbols from
[`RSA_ROADMAP.md`](RSA_ROADMAP.md):

- **✅** implemented and verified
- **🚧** active or partly complete
- **⬜** not started
- **⛔** blocked on an external dependency
- **❓** open decision, not committed work

Last reconciled against the source tree and numeric gates on **2026-08-29**.

---

## The headline MVP

The minimum viable product is complete when a user can:

1. Supply one or more subjects, each with at least two independent runs/folds.
2. Supply either AFNI datasets or `.1D` matrices through one common
   `-dataTable` contract.
3. Decode two or more categorical classes with leave-one-run-out
   cross-validation.
4. Run the decoder in atlas ROIs, a volumetric searchlight, or once over all
   columns of a 1D feature matrix.
5. Use a robust linear classifier whose preprocessing and covariance estimates
   are learned inside each training fold.
6. Obtain held-out predictions, a confusion matrix, balanced accuracy, ordinary
   accuracy, and per-class recall.
7. Test accuracy with label permutations restricted within subject and run,
   with synchronized raw p, BH-FDR q, and searchlight max-statistic FWE values.
8. Reproduce the same result across OpenMP thread counts from a recorded seed.
9. Reject designs that would leak held-out data, have an untrainable fold, mix
   incompatible feature spaces, or use an invalid permutation scheme.
10. Build and test under AFNI's CMake and legacy Make systems, with independent
    numeric references in the existing `src/pmolfese/tests` style.

The MVP does **classification only**.  Regression, nested tuning, temporal
generalization, cross-subject decoding, and nonlinear classifiers are staged
after it.  Keeping those out of the first delivery is a correctness boundary,
not a claim that they are unimportant.

### MVP defaults

The following defaults should make the simplest invocation scientifically
defensible without hiding important choices:

| Decision | MVP default | Reason |
|---|---|---|
| Target | categorical `Label` | Classification has a clear, testable first scope. |
| Cross-validation | leave one `Run` out | Independent-run validation is the standard basic fMRI contract. |
| Classifier | shrinkage LDA | Linear, multiclass, interpretable, and stable when features outnumber samples. |
| Primary score | balanced accuracy | Does not reward the majority class in an imbalanced design. |
| Secondary score | ordinary accuracy | Familiar and useful for balanced designs. |
| Feature scaling | training-fold z-score | Prevents test-fold leakage and accommodates heterogeneous 1D features. |
| Constant features | drop using training data only | Avoids division by zero without inspecting held-out data. |
| Permutation | labels shuffled within subject × run | Preserves run structure and class counts while breaking feature-label association. |
| Tail | upper | Better-than-chance decoding is the primary hypothesis. |
| Spatial correction | synchronized max statistic | Corrects the searched spatial family without parametric map assumptions. |
| Seed | explicit, recorded default | Makes relabelings and threaded results reproducible. |

### MVP acceptance example

An fMRI invocation should converge on an interface like:

```text
3dMVPA                                                   \
  -dataTable @mvpa_runs.txt                              \
  -mask gray_matter+tlrc                                 \
  -searchlight 'SPHERE(6)'                               \
  -classifier lda -covariance shrinkage                  \
  -score balanced_accuracy                               \
  -permute 5000 -seed 314159                             \
  -prefix face_object_mvpa
```

where `mvpa_runs.txt` is:

```text
Subj  Run  InputFile                 LabelFile
s01   r1   s01.r1.betas+tlrc         s01.r1.labels.txt
s01   r2   s01.r2.betas+tlrc         s01.r2.labels.txt
s02   r1   s02.r1.betas+tlrc         s02.r1.labels.txt
s02   r2   s02.r2.betas+tlrc         s02.r2.labels.txt
```

Each fMRI sub-brick is one sample and each nonzero mask voxel is one feature.
Each `LabelFile` contains one whitespace-free class label per sub-brick, in
matching order.

The equivalent generic-vector input uses the same table:

```text
Subj  Run  InputFile                 LabelFile
p01   block1  p01.block1.features.1D p01.block1.labels.txt
p01   block2  p01.block2.features.1D p01.block2.labels.txt
```

For `.1D` input, rows are samples and columns are features.  No mask is used;
the full column set is one analysis location.  A single-subject table is valid,
which makes this path useful for EEG epochs or behavioral feature vectors as
well as for reduced fMRI features.

---

## Status dashboard

### MVP stages

| Stage | Deliverable | Status | Exit criterion |
|:---:|---|:---:|---|
| 0 | Freeze the scientific and CLI contract | ⬜ | Input orientation, folds, classifier math, scores, null, and outputs are specified with no ambiguous defaults. |
| 1 | Extract and verify the shared infrastructure | ✅ Complete | M1a–M1e preserve the baseline across CMake, legacy Make, plain/OpenMP, and optional-SUMA builds. |
| 2 | Build the sample-table, 1D, and fold layer | ⬜ | AFNI and `.1D` fixtures produce the same canonical sample matrix and invalid designs fail clearly. |
| 3 | Implement the ROI/1D decoding engine | ⬜ | Independent references match observed fold predictions, scores, covariance, and confusion statistics. |
| 4 | Add permutation inference, volumetric searchlights, output, and release integration | ⬜ | The end-to-end MVP acceptance suite passes in plain and OpenMP builds. |

### Post-MVP stages

| Stage | Theme | Status | Main additions |
|:---:|---|:---:|---|
| 5 | Hardened input and spatial coverage | ⬜ | Surface searchlights, sample metadata, censoring, weights, saved folds, richer selectors. |
| 6 | More estimators and metrics | ⬜ | Nearest centroid, diagonal LDA, logistic regression, binary AUC, decision values, calibration. |
| 7 | Leakage-safe model selection and feature selection | ⬜ | Nested CV, inner-fold tuning, ANOVA/top-k selection, PCA, tuning provenance. |
| 8 | Group and generalization designs | ⬜ | Leave-one-subject-out, cross-dataset/domain decoding, subject-level inference, site blocks. |
| 9 | Regression and encoding | ⬜ | Continuous targets, correlation/R²/MAE scores, nuisance-aware nulls, multi-output targets. |
| 10 | Time-resolved and EEG-oriented analysis | ⬜ | Time-resolved decoding, temporal generalization, channel neighborhoods, time × space FWE. |
| 11 | Performance and production scale | ⬜ | Batching, warm workspaces, permutation planning, memory preflight, benchmark gates. |

---

## Scientific contract

### Unit of observation

One **sample** is one feature vector paired with one categorical label.  For an
AFNI dataset, a sub-brick is a sample and selected voxels/nodes are features.
For a `.1D` file, a row is a sample and columns are features.

This orientation must be stated repeatedly in help and diagnostics because AFNI
`.1D` conventions can otherwise be ambiguous.  The program should never infer
that columns are samples from matrix shape.  A later `-transpose_1D` option may
be added explicitly, but silent orientation guessing is out of scope.

Each sample also belongs to:

- exactly one subject;
- exactly one run, which is the MVP cross-validation fold and permutation
  exchangeability block;
- exactly one class;
- exactly one input row/container; and
- optionally, after the MVP, one sample ID, trial group, weight, site, or other
  metadata value.

### What independence means

The held-out unit is a run, not a beta inside a run.  All learned operations
must use training runs only:

- feature means and scales;
- constant/invalid feature detection;
- class centroids;
- pooled covariance and shrinkage intensity;
- any feature selection, PCA, nuisance regression, or hyperparameter choice
  added later.

The test fold may be used only to generate predictions and evaluate them.  A
test sample must never affect its own preprocessing, covariance, selected
features, stopping rule, or model choice.

### Valid folds

For every subject and held-out run:

- the training set must contain at least one sample from every global class;
- the training residual degrees of freedom must be sufficient for the selected
  covariance estimator;
- the held-out samples must have labels known to the subject-level class set;
- the training and test feature widths must match;
- at least two folds must contribute predictions;
- every class must contribute at least one held-out prediction to a requested
  classwise or balanced score.

The MVP may allow a test run to omit a class because balanced accuracy is
computed from pooled out-of-fold predictions, not by averaging ill-defined
foldwise class recalls.  It must not allow a training fold to omit a class.

### Subject-level and group-level estimands

The primary subject-level result is balanced accuracy computed once from all of
that subject's pooled out-of-fold predictions:

```text
balanced_accuracy_s = mean_c recall_{s,c}
```

Ordinary accuracy is the fraction of all held-out samples predicted correctly.
Per-class recall and the complete confusion matrix are descriptive readouts.

For multiple subjects, the primary group effect is the equal-subject mean of
subject-level balanced accuracies.  Subjects are not weighted by trial count by
default; otherwise a subject with more retained trials silently defines the
population result.  A later explicit weighting option may relax this.

For a single subject, the subject score is the reported effect and the label
permutation is its inferential unit.  For multiple subjects, every group-null
draw independently permutes each subject's labels under the same draw index,
reruns all folds, and averages the resulting subject scores.  This retains the
within-subject design while producing a group-level null.

### Chance and effect scales

The program should report both the raw score and a chance-centered effect:

```text
effect = balanced_accuracy - 1 / nclass
```

for balanced accuracy.  Ordinary accuracy has a class-frequency-dependent
chance baseline and should not be centered at `1/nclass` unless the design is
balanced; report its raw value in the MVP and use permutation inference rather
than an asserted analytic baseline.

Permutation p-values test the score directly.  Chance-centering is an
interpretive effect scale, not the source of the p-value.

### Missing and non-finite values

MVP policy should be strict:

- A missing label is an error.
- A non-finite feature value is an error that identifies subject, run, sample,
  and feature/voxel.
- A feature constant in a particular training fold is dropped for that fold.
- If all features are dropped, that fold is invalid.
- A spatial neighborhood with fewer than `-min_features` usable features is
  marked invalid and excluded from all spatial multiple-comparison families.

Silent imputation is out of scope for the MVP.  Post-MVP imputation, if added,
must be learned inside each training fold.

---

## Input design: extend `THD_datatable`, do not bypass it

### Canonical MVP table

The MVP uses one row per **subject × run input container**:

| Column | Required | Meaning |
|---|:---:|---|
| `Subj` | yes | Subject identifier; repeated across that subject's runs. |
| `Run` | yes | Independent acquisition/block and default held-out fold. |
| `InputFile` | yes | AFNI dataset or `.1D` feature matrix. |
| `LabelFile` | yes | One categorical label per sample, in input order. |

`THD_read_datatable_file()` already supplies parsing, raw/numeric columns,
case-insensitive lookup, comments, line continuation, and consistent error
reporting.  `3dMVPA` should use it directly, then apply a decoder-specific
validation layer.  No second ad hoc table tokenizer should be created.

The table should be accepted as:

```text
-dataTable @mvpa_runs.txt
-dataTableFile mvpa_runs.txt
```

An inline table can technically be supported by the existing reader, but file
input should be recommended because MVPA tables are long and `InputFile` must be
last in the current inline grammar.  Since `LabelFile` follows `InputFile` in
the natural schema, the CLI either needs a documented inline column order with
`InputFile` last or should initially limit this schema to a file.  Resolve this
in Stage 0; do not introduce a parser exception silently.

### AFNI dataset interpretation

- `DSET_NVALS(InputFile)` is the sample count for the row.
- A `LabelFile` must contain exactly that many labels.
- Within an ROI/searchlight, `THD_roi_pattern()` yields
  `[sample][selected voxel]`, already the layout required by the classifier.
- All datasets used in one spatial analysis must match the mask grid and each
  other.
- Sample counts and class counts may differ across runs and subjects.
- Dataset sub-brick labels are useful diagnostics but are not silently treated
  as class labels in the MVP.

Requiring a separate `LabelFile` is more verbose than mining sub-brick labels,
but it creates an explicit, auditable mapping and exactly matches the generic
1D path.  A later importer can generate label files from sub-brick selectors or
AFNI labels.

### `.1D` interpretation

- Each non-comment data row is one sample.
- Each numeric column is one feature.
- Ragged rows, nonnumeric tokens, zero samples, or zero features are errors.
- The label count must equal the number of matrix rows.
- Every run for a subject must have the same feature count.
- Every subject must have the same feature count for a group mean, unless a
  later cross-subject design explicitly supplies feature alignment.
- `-mask`, `-roi_sel`, `-searchlight`, and `-surf` are rejected for 1D input.
- Mixing AFNI datasets and `.1D` files in one invocation is rejected in the
  MVP, even if dimensions happen to match.

The loader should accept AFNI's ordinary whitespace-separated 1D format and
ignore blank/comment lines.  It should document whether column selectors such
as `file.1D'[0..9]'` are resolved by AFNI I/O or are not supported.  Tests must
cover the actual decision.

### Label-file interpretation

Use the same strict conventions as the `ConditionFile` path in
`THD_runset_read()`:

- one whitespace-free label per non-comment line;
- blank lines and `#` comments ignored;
- no numeric-only requirement;
- exact count match with the input samples;
- stable, deterministic global class order, preferably lexical order;
- preserve original strings for tables, confusion labels, and diagnostics.

The reusable reader should move out of its current static 3dRSA/runset context
or be generalized rather than copied.

### Proposed canonical in-memory objects

Add a generic sample container rather than extending `THD_runset` until it
means two incompatible things.  Names are provisional:

```c
typedef enum {
   THD_FEATURE_DSET,
   THD_FEATURE_1D
} THD_feature_kind;

typedef struct {
   int nsub, nrun, nsamp, nclass;
   THD_feature_kind kind;

   char **subj_lab;       /* [nsub] */
   char **run_lab;        /* [nrun], unique within subject by mapping */
   char **class_lab;      /* [nclass], deterministic order */

   int *sample_subj;      /* [nsamp] */
   int *sample_run;       /* [nsamp] global run-row/fold index */
   int *sample_class;     /* [nsamp] */
   int *sample_row;       /* [nsamp] dataTable row/container */
   int *sample_local;     /* [nsamp] sub-brick or 1D row */

   int nrow;
   THD_3dim_dataset **dset; /* [nrow], dataset mode */
   float **matrix;          /* [nrow], row-major 1D mode */
   int *row_nsamp;
   int *row_nfeat;

   THD_datatable *table;  /* retained metadata/provenance */
   char *source;
} THD_sampleset;
```

The exact structure may change, but the separation of sample indices from
input containers is important.  It admits unbalanced trial counts, repeated
classes, missing classes in individual test runs, future sample censoring, and
future arbitrary folds without changing the physical input layout.

Add a separate fold description:

```c
typedef struct {
   int nfold, nsamp;
   int *fold_of;          /* [nsamp] */
   int *train_count;      /* [nfold] */
   int *test_count;       /* [nfold] */
   int **train_index;     /* or compact offsets + one index vector */
   int **test_index;
} THD_foldset;
```

For the MVP, folds derive deterministically from `Run` within subject.  The
object is generic from the start so Stage 5 can add a `FoldFile` or leave-k-run
schemes without rewriting classifier loops.

### Validation order

Fail early, before loading all bricks or allocating map-wide result arrays:

1. Parse the table and required columns.
2. Resolve unique subject × run rows and reject duplicates.
3. Open headers/read 1D dimensions.
4. Read labels and build the global class dictionary.
5. Check per-subject class/fold eligibility.
6. Check feature dimensions and spatial grids.
7. Build folds and print a design summary.
8. Estimate memory and compute cost.
9. Load or stream feature data.

The summary should report, per subject, runs, total samples, samples per class,
and samples per run.  It should warn about severe class imbalance but not reject
it when balanced accuracy remains defined.

---

## Classifier and preprocessing contract

### Shrinkage LDA

The MVP classifier is multiclass linear discriminant analysis with a shared
pooled covariance estimated from the training samples.  For each fold:

1. Select training samples only.
2. Estimate each feature's training mean and scale.
3. Drop features with zero/near-zero training variance.
4. Transform training and test samples with those training parameters.
5. Estimate class centroids from transformed training samples.
6. Estimate the pooled within-class covariance from training residuals.
7. Shrink the covariance toward a diagonal or scaled-identity target using an
   explicitly documented estimator.
8. Solve the discriminant functions without forming an unstable ordinary
   inverse where a factorization/regularized solve is available.
9. Predict the class with the largest discriminant score.
10. Store the predicted label and optional class scores for every held-out
    sample.

For class `c`, the standard equal-prior discriminant score is:

```text
delta_c(x) = x' Sigma^-1 mu_c - 0.5 mu_c' Sigma^-1 mu_c
```

The MVP should use equal class priors by default so class imbalance does not
enter twice through both training frequency and scoring.  An empirical-prior
option can follow later.

The shrinkage target and intensity must be frozen in Stage 0 and mirrored in
the independent NumPy/SciPy reference.  Existing `THD_noise_whalf()` and the
Ledoit-Wolf/shrinkage work in `thd_simmatrix` should be reused or generalized
only where their estimator exactly matches this supervised pooled-covariance
contract.  Similar names are not sufficient evidence of identical math.

### Feature scaling

Training-fold z-scoring should be the MVP default for generic vectors.  For
voxel patterns it is also a defensible default, but it changes the conventional
interpretation of an unscaled LDA.  Stage 0 should settle the public option:

```text
-scale zscore       # proposed default
-scale demean
-scale none
```

Whatever default is chosen, every parameter is fitted within each fold.  The
observed and every permuted analysis must execute the same preprocessing path.

Optional per-sample spatial demeaning is a distinct operation and must not be
conflated with per-feature training z-scoring.  If later added, name it
explicitly (for example `-pattern_center`) and test it independently.

### Numerically degenerate cases

Define behavior rather than relying on library accident:

- Drop a training-constant feature for that fold.
- Reject a fold with fewer than two total residual degrees of freedom.
- Reject a class absent from training.
- Resolve exact score ties deterministically by class order and count/report
  them.
- Treat a non-finite discriminant score as a hard error during development and
  as an invalid location only after its cause is understood.
- Record effective feature count and covariance rank/condition diagnostic per
  ROI in verbose/debug output.

### Workspace API

Classifier fitting happens inside fold × permutation × location loops.  Avoid
small repeated allocations.  A reusable workspace should own standardized
matrices, means, scales, centroids, covariance/factorization, scores, and index
buffers sized to the largest subject/fold/location.

The scientific kernel should be callable without AFNI datasets:

```c
int THD_mvpa_decode(
   const float *X, int nsamp, int nfeat,
   const int *label, const int *fold,
   const THD_mvpa_opts *opt,
   THD_mvpa_ws *ws,
   THD_mvpa_result *out
);
```

This separation is what lets tiny C unit fixtures, `.1D` inputs, ROI patterns,
and searchlights all exercise the same decoder.

---

## Permutation and multiple-comparison contract

### Null hypothesis

The primary null is that feature vectors carry no information about class
labels beyond the subject/run structure retained by the design.  Labels are
permuted **within each subject × run**, then the complete cross-validation
pipeline is rerun.

This is not equivalent to:

- permuting predictions after fitting;
- permuting labels across runs;
- permuting features independently;
- fitting once and only rescoring shuffled labels;
- shuffling searchlight locations; or
- testing accuracy against a binomial distribution that ignores overlapping
  training sets and repeated-measures structure.

The expensive operation is intentionally inside the null: every relabeling
must retrain every fold.

### Reuse from `thd_permute`

The existing layer already provides most of the required mechanics:

- `PERM_scheme` for exchangeability;
- `THD_perm_scheme_set_blocks()` for within-block relabeling;
- immutable `PERM_set` objects with identity in slot zero;
- exact enumeration when feasible and Monte Carlo otherwise;
- empirical p-values that cannot be zero;
- generic statistic callbacks;
- max-statistic accumulation;
- deterministic seeds and thread-independent relabeling sets.

One decoder-specific issue must be resolved carefully: subject × run blocks can
have different label compositions.  Permuting within each block preserves each
composition.  Equivalence classes may reduce exact enumeration only when two
labels are genuinely interchangeable for the entire statistic; do not apply
`eqclass` merely because labels repeat.

### Group permutations

A group draw should consist of an independent legal within-run permutation for
every subject.  There are two reasonable representations:

1. One global `PERM_set` over all samples with subject × run block labels.
2. One immutable set per subject, coordinated by shared draw index.

Prefer the global representation if it preserves independent random shuffles
within all blocks and does not create unnecessary storage.  Otherwise introduce
a thin decoder permutation-plan object containing per-subject sets.  In either
case, draw `p` must mean the same relabeling everywhere in the map.

### Spatial FWE

For each permutation draw:

1. Compute the group statistic at every valid ROI/searchlight center.
2. Convert to the chosen upper-tail statistic, normally the raw group balanced
   accuracy or its chance-centered equivalent.
3. Retain the maximum across the complete requested spatial family.
4. Compare each observed location with the sorted max-null distribution.

One synchronized permutation family across locations is essential.  Creating a
new random shuffle at each sphere destroys the joint null required for
max-statistic FWE and thread-count reproducibility.

For atlas ROIs, report raw p and BH-FDR q by default; permit max-FWE as an
option.  For searchlights, report raw p, q, and max-FWE when permutations are
requested.  Invalid/too-small neighborhoods do not enter the correction family.

### Monte Carlo planning

Searchlight decoding costs approximately:

```text
n_locations × n_permutations × sum_subjects(n_folds × classifier_fit_cost)
```

Unlike fixed-model RSA permutations, there is no general cache that removes
the retraining cost.  Before execution, print an estimate based on locations,
folds, subjects, samples, features, and permutations.  The help should explain
that 100 permutations may be useful for a smoke test but cannot support small
p-values, while 5,000–10,000 is more appropriate for final map inference when
compute permits.

Stage 11 may implement valid two-level/Stelzer-style null aggregation, but the
MVP should prefer a slow transparent full refit over an unverified shortcut.

---

## Output contract

### ROI and 1D table

Write a wide summary table with stable machine-readable column names.  Proposed
core columns are:

```text
ROI ROI_label nfeat nsub nsamp nclass nfold
bal_acc bal_acc_effect acc p q pfwe z zfwe
```

Rules:

- `bal_acc` is the primary group/subject score.
- `bal_acc_effect` is chance-centered when its baseline is defined.
- `acc` is ordinary pooled held-out accuracy.
- `p/q/pfwe` are empirical inference fields and appear only when requested.
- `z/zfwe` are signed normal equivalents of permutation p-values, not
  parametric z tests of accuracy.
- `nfeat` is the original ROI/searchlight width; foldwise retained-feature
  minima/maxima can appear in verbose/debug output.
- Exact class labels and their deterministic order are stored in table metadata
  or a companion file.

Per-subject rows should be available through `-subject_table PREFIX.subjects.tsv`
even when the headline is a group mean.  They are essential for audit,
visualization, and downstream alternatives.

### Prediction table

`-pred_out FILE` should write one observed out-of-fold row per sample:

```text
Subj Run Sample InputFile SampleIndex TrueLabel PredLabel Correct
```

Optional later columns include decision values/probabilities.  Permuted
predictions are not written by default.

### Confusion output

Write a tidy/long table rather than embedding a variable-width matrix in the
main table:

```text
ROI ROI_label Subj TrueLabel PredLabel Count
```

Include an aggregate subject label such as `GROUP` for the equal-subject
normalized group confusion matrix only if its definition is explicit.  Raw
trial-pooled group counts should not masquerade as an equal-subject statistic.

### Searchlight datasets

Use the 3dRSA convention of one effect/statistic per labeled sub-brick, painted
at the center voxel only.  Proposed bricks:

```text
bal_acc  bal_acc_effect  acc  p  q  pfwe  z  zfwe
```

Only requested/defined bricks should be written.  Accuracy bricks are plain
numeric data.  A permutation-derived signed z-equivalent may use an AFNI
statistical type only if its semantics and degrees-of-freedom representation
are valid; otherwise leave it untyped and document it, following 3dRSA's
`z_is_fizt` safeguard.

The history/provenance should record:

- command line and version;
- table source;
- input kind and orientation;
- class order and counts;
- fold rule;
- classifier, covariance, priors, and scaling;
- score and group aggregation;
- permutation count, exact/Monte Carlo status, block rule, and seed;
- mask/neighborhood and valid-location count;
- OpenMP thread count; and
- warnings or overrides.

### Prefix and overwrite policy

Follow AFNI conventions:

- `-prefix PREFIX` controls the primary dataset/table stem.
- Refuse to overwrite existing outputs unless AFNI's standard overwrite
  mechanism permits it.
- Determine every output path before expensive computation and fail early on a
  collision.
- Keep labels short enough for AFNI brick constraints while retaining full
  names in sidecar/table metadata.

---

## Reuse and refactoring map

### Reuse directly

| Existing component | Reuse in 3dMVPA |
|---|---|
| `thd_datatable.c/.h` | Parse input tables, retain arbitrary metadata, locate required columns, print/debug tables. |
| `THD_roilist_from_dset()` | Atlas/ROI analysis units. |
| `THD_roilist_searchlight()` | Volumetric moving neighborhoods. |
| `THD_roi_pattern()` | Extract `[sample][voxel]` pattern matrices from AFNI datasets. |
| `thd_permute.c/.h` | Exchangeability blocks, exact/Monte Carlo relabelings, empirical p values, max-null machinery, deterministic seeds. |
| Noise/covariance helpers in `thd_simmatrix` | Reuse only after verifying the estimator matches supervised pooled-covariance LDA. |
| `src/pmolfese/tests/run_numeric.py` conventions | Independent NumPy/SciPy reference calculations, failure-contract tests, map checks, thread identity. |
| AFNI CMake/Make/package integration for `3dRSA` | Template for target, objects, SUMA option, install component, and CTest registration. |

### Move/generalize once

M1 deliberately shares a narrow set of mechanisms rather than a complete
analysis loop:

| Owner | Shared responsibility |
|---|---|
| `thd_mapinfer.c/.h` | Plain BH-FDR over an explicitly declared family, an optional-validity-mask form of the same calculation, elementwise max-null accumulation, and a generic byte-category memory ledger. |
| `thd_patterns.c/.h` | Volumetric neighborhood-descriptor parsing, ROI/searchlight result painting, and the SUMA geodesic surface ROI-list builder. |
| Top-level program | Statistic calculation, loop scheduling, output schemas and typing, percentile/bootstrap summaries, error wording, and program-specific memory-category estimates. |

The OpenMP-over-location loop is intentionally **not** shared. `3dRSA` and
`3dMVPA` have different workspaces and retraining costs, and an opaque callback
framework would hide more than it reuses. Output brick creation/`FIZT` typing
and the RSA bootstrap percentile likewise remain private until a second real
consumer demonstrates the same contract.

Stage 1 is a no-scientific-change refactor for `3dRSA` and `1dTrdm`. Their
complete existing numeric suites are the safety gates. Pre-release freedom
allows private symbols, exact diagnostics, and output ordering to improve when
that is intentional and tested; it does not allow silent changes to estimands,
null families, numeric results, or seed/thread determinism.

### New decoder-specific files

Proposed split:

| File | Responsibility |
|---|---|
| `3dMVPA.c` | CLI/help, option validation, orchestration, AFNI history/output. |
| `thd_samples.c/.h` | `THD_sampleset`, AFNI/1D loaders, labels, subject/run mapping, design validation. |
| `thd_folds.c/.h` | LORO fold generation and future generic fold schemes. |
| `thd_decode.c/.h` | Scaling, shrinkage LDA, predictions, scores, confusion matrices, reusable workspaces. |
| `thd_mapinfer.c/.h` | Shared BH-FDR, max accumulation, and memory-ledger arithmetic extracted from `3dRSA`/`1dTrdm`. |

Avoid putting classifier code in `thd_simmatrix`: LDA consumes rectangular
sample × feature matrices and class labels, not similarity matrices.  Low-level
covariance/factorization helpers may live in a more generic numeric file if both
engines truly share them.

---

## Stage 0 — freeze the contract

**Status: ⬜ not started**

### Tasks

1. Write the exact `-help` synopsis before implementation.
2. Choose and document the inline-table policy.
3. Freeze `.1D` row/column orientation and selector behavior.
4. Freeze label ordering and tie-breaking.
5. Freeze the LDA covariance target, shrinkage estimator, and class priors.
6. Decide whether training-fold z-scoring is the universal default or differs
   between dataset and 1D modes.
7. Freeze subject and group aggregation rules.
8. Freeze the within-subject × run permutation representation.
9. Define the exact primary statistic accumulated for max-FWE.
10. Freeze output columns, dataset brick labels, and file naming.
11. List hard errors, invalid locations, and warnings separately.
12. Create tiny hand-computable fixtures before writing the classifier.

### Required design fixtures

- Balanced binary, two runs, separable features.
- Balanced binary null data.
- Three-class data.
- Unequal run sizes.
- Class imbalance.
- A test run missing one class but pooled OOF coverage complete.
- A training fold missing one class, which must fail.
- Constant features in only one fold.
- Exact discriminant-score tie.
- Single-subject 1D data.
- Multi-subject AFNI data with identical canonical values.

### Exit criteria

- Every MVP option has one meaning and a recorded default.
- The independent reference equations are written down.
- No implementation question can change the estimand without reopening this
  stage explicitly.

---

## Stage 1 — shared infrastructure extraction

**Status: ✅ complete (2026-08-29)**

### M1a baseline and shared-core contract

**Status: ✅ complete (2026-08-29)**

M1a changes no runtime code. It records the boundary that later extraction
must obey before symbols or files move.

#### Verified starting point

| Artifact | 2026-08-29 baseline |
|---|---:|
| `3dRSA.c` | 7,392 lines |
| `1dTrdm.c` | 992 lines |
| `thd_datatable.c` | 342 lines |
| `thd_patterns.c` | 698 lines |
| `thd_permute.c` | 1,625 lines |
| `thd_simmatrix.c` | 2,380 lines |
| `3dRSA` required-dependency numeric gate | 276/276 passing |
| focused `1dTrdm` gate | 36/36 passing |

The current CMake build and its registered RSA numeric CTests are green. This is
the comparison point for M1b–M1e; historical lower check counts elsewhere in
the RSA roadmap describe their older milestone boundaries.

#### Hard preservation contract

Extraction must preserve:

- observed effects and every scientific estimand;
- permutation/bootstrap membership, exchangeability units, tails, and multiple-
  comparison families;
- numeric tables and maps, subject to documented floating-point tolerance only
  where an implementation change makes bit identity impossible;
- seed identity and one-versus-many-thread results; and
- current success/failure coverage for ordinary, runwise/crossnobis, ROI,
  volume-searchlight, surface, temporal, and bridge paths.

Because neither program has shipped, private C symbols, exact error prose,
option ordering in help, and output column/brick ordering are not compatibility
promises. They may change deliberately, but only with corresponding help,
provenance, and tests. No such cleanup is part of M1 by default.

#### Ecosystem confinement

M1 may modify only `3dRSA`, `1dTrdm`, the dedicated support modules created for
this RSA/MVPA line (`thd_datatable`, `thd_patterns`, `thd_permute`,
`thd_simmatrix`, and the new `thd_mapinfer`), their focused tests, and the
minimum target/package/roadmap entries for those programs. It must not edit
unrelated AFNI libraries, programs, or global behavior. If an integration need
crosses that boundary, work stops for explicit approval.

#### Shared public responsibilities

The following API surface is the M1 target. Names and ownership are fixed here;
argument refinements needed for const-correctness or AFNI build compatibility
may be made during implementation without broadening responsibility.

```c
/* thd_mapinfer.h: dataset-agnostic inference/resource primitives */
void THD_bh_fdr(int n, const float *p, float *q);
void THD_bh_fdr_masked(int n, const float *p,
                       const unsigned char *valid, float *q);
void THD_max_accum(int n, float *dst, const float *src);

typedef struct {
   double input, geometry, shared, output, per_thread;
   double total, system, limit;
   int nthread;
} THD_memory_plan;

void THD_memory_plan_finish(THD_memory_plan *plan);

/* thd_patterns.h: spatial geometry and painting */
MCW_cluster *THD_searchlight_parse(const char *spec,
                                   float dx, float dy, float dz,
                                   char *err, size_t errlen);
void THD_roilist_paint(float *brick, const THD_roilist *rl,
                       const float *values);
#ifdef USE_SUMA
THD_roilist *THD_roilist_searchlight_surf(const char *surface,
                                          THD_3dim_dataset *mask,
                                          float radius, int all_nodes,
                                          char *err, size_t errlen);
#endif
```

`THD_bh_fdr_masked` counts only valid entries in the declared family; invalid
output slots receive `1.0`. `THD_memory_plan_finish` performs arithmetic only:
`total = input + geometry + shared + output + nthread * per_thread`. Programs
remain responsible for estimating categories and deciding whether to warn or
refuse. The parser returns `NULL` plus a bounded diagnostic instead of exiting,
so the caller retains its program name and error policy.

No RSA model, classifier, sample, fold, output-dataset, or OpenMP callback type
may enter either shared header.

#### M1 submilestones

| ID | Deliverable | Status | Gate |
|:---:|---|:---:|---|
| M1a | Baseline, preservation rules, module/API boundary, and confinement | ✅ | Roadmaps agree; current 276 + 36 tests recorded. |
| M1b | Extract BH-FDR and max accumulation into `thd_mapinfer`; make `3dRSA` and `1dTrdm` consumers | ✅ | Plain/masked/tied/aliased BH and max unit cases pass; 276 + 36 complete gates pass. |
| M1c | Move parser, painting, and SUMA surface builder into `thd_patterns` | ✅ | Direct grammar/painting unit gate plus ROI/volume/SUMA and 276 + 36 complete gates pass. |
| M1d | Replace RSA-only accounting container with the generic memory ledger | ✅ | Direct arithmetic and existing warning/refusal/override threshold tests agree; 276 + 36 complete gates pass. |
| M1e | Reconcile CMake/Make/package wiring and run the full equivalence matrix | ✅ | Plain/OpenMP and applicable SUMA builds/tests pass; install/package/test entries agree; no out-of-ecosystem edits. |

### Tasks

1. M1 is complete; stop before beginning decoder-specific Stage 0 work.
2. Keep percentiles, output typing/labels, and OpenMP loop ownership in each
   top-level program.
3. Keep `THD_runset` as RSA runwise input; build a distinct `THD_sampleset` in
   Stage 2 rather than forcing class samples into an RDM container.
4. Update Make/CMake object lists without introducing a SUMA dependency for
   non-SUMA builds.
5. Compare against the M1a gates after every move, not only at the end.

### M1b implementation record · ✅ complete

`thd_mapinfer.c/.h` now owns plain and validity-masked BH-FDR plus elementwise
max accumulation. Both `3dRSA` and `1dTrdm` consume the shared functions; their
private BH implementations and `3dRSA`'s private max helper were removed.
Masked BH excludes invalid entries from the family and writes `q=1` for them.

A direct C unit test covers ordinary BH, masked-family denominators, tied
p-values with aliased input/output, zero-length safety, and max accumulation.
The CMake/SUMA/OpenMP build succeeds, that unit gate passes, and the complete
registered `3dRSA` and `1dTrdm` numeric tests remain 276/276 and 36/36. M1b did
not touch spatial geometry, estimators, output typing, percentiles, or OpenMP
loop ownership.

### M1c implementation record · ✅ complete

`thd_patterns.c/.h` now owns the `SPHERE`/`RECT`/`RHDD`/`TOHD` parser, atlas
parcel versus searchlight-center painting, and the optional SUMA geodesic
surface ROI-list builder. `3dRSA` retains SUMA initialization and program-level
failure policy; the shared builders return bounded diagnostics to the caller.

A direct unit test covers all four descriptor forms, bare-radius equivalence,
invalid-radius and unknown-shape diagnostics, atlas-fill painting, and
center-only painting. The existing suite supplies real volumetric and 25-node
SUMA whole-surface equivalence. The configured CMake/SUMA/OpenMP build, both
shared-unit tests, and the complete 276 + 36 numeric gates pass. No estimator,
inference-family, output typing, percentile, memory, or loop-scheduling code
moved.

### M1d implementation record · ✅ complete

`thd_mapinfer.c/.h` now owns the generic `THD_memory_plan` byte-category
container and its arithmetic-only finalizer. `3dRSA` fills the shared
`input`, `geometry`, `shared`, `output`, and `per_thread` categories, then the
helper computes `total = input + geometry + shared + output + nthread *
per_thread`. The old RSA-only container is gone.

The extraction deliberately leaves dataset-size estimates, physical-memory
detection, default and explicit limits, warning thresholds, diagnostics,
refusal, and `-memory_override` policy in `3dRSA`. A direct unit case verifies
the total and that `system`/`limit` remain untouched; the existing forced-low-
limit refusal and override checks pass. The configured CMake/SUMA/OpenMP
targets, both shared-unit tests, and the complete 276/276 `3dRSA` plus 36/36
`1dTrdm` gates pass. No scientific estimator, inference family, output,
percentile, spatial, or loop-scheduling code moved.

### M1e implementation record · ✅ complete

The final integration audit reconciles the source lists and conditional
dependencies across CMake and legacy Make. Both programs build `thd_mapinfer`;
only `3dRSA` builds `thd_patterns` and receives `USE_SUMA`/SUMA linkage; both
receive `USE_OMP` only in OpenMP configurations. The legacy `1dTrdm.o`
dependency list now records its transitive `thd_permute.h` API, and the CTest
bridge is registered only when both executable targets exist.

The CMake matrix passed with SUMA/OpenMP both off, OpenMP on without SUMA, and
SUMA plus OpenMP on. Each configuration passed the focused 3dRSA/1dTrdm and
both shared-unit CTests; non-SUMA builds exercise 273 RSA checks and skip only
the three surface cases, while the SUMA build passes all 276, and `1dTrdm`
passes all 36 throughout. The supported sequential legacy GCC 14/OpenMP build
links both programs and passes the same 273 non-SUMA RSA checks plus all 36
temporal checks.

Both executables occur exactly once in the legacy program list and package
mapping, are assigned to `corebinaries`, and have generated CMake install rules
for `bin/3dRSA` and `bin/1dTrdm`. M1 changed no classifier science and no source
outside the confined RSA/MVPA ecosystem. Stage 1 is therefore closed.

### Exit criteria

- Existing `3dRSA` and `1dTrdm` scientific and numeric results match the M1a
  baseline.
- The shared public header contains no RSA model types.
- Both top-level tools can consume ROI lists and map helpers without including
  each other's source.
- No duplicated `paint_brick`, FDR, neighborhood, or surface searchlight code is
  introduced in `3dMVPA.c`.
- No file outside the confined RSA/MVPA ecosystem is modified.

---

## Stage 2 — samples, 1D input, labels, and folds

**Status: ⬜ not started**

### Tasks

1. Implement `THD_sampleset_read()` over `THD_datatable`.
2. Generalize/reuse the strict label-file reader.
3. Deduplicate repeated dataset opens where safe.
4. Implement a strict numeric `.1D` matrix reader or wrap an existing AFNI
   reader that preserves the required orientation and diagnostics.
5. Build deterministic subject, run, and class dictionaries.
6. Flatten containers into canonical sample indices.
7. Implement leave-one-run-out `THD_foldset` generation per subject.
8. Validate every training fold and pooled score.
9. Print the design audit summary.
10. Add `-show_table`, `-show_design`, and a dry validation path if consistent
    with AFNI conventions.
11. Add loaders that fill a caller-owned `[sample][feature]` buffer for one
    ROI/searchlight without allocation inside the location loop.
12. Add input memory accounting for dataset-resident and 1D-resident modes.

### Numeric and contract tests

- AFNI dataset extraction equals a saved `.1D` matrix of the same values.
- Sub-brick/sample ordering matches labels exactly.
- Multiple runs flatten in first-table-row order while class IDs remain
  deterministic.
- Unequal trials per run and repeated class labels load correctly.
- Duplicate subject × run rows fail.
- Label count too short/long fails with file and expected/actual counts.
- Ragged/nonnumeric/empty 1D files fail.
- Mixed input kinds fail.
- Grid and feature-width mismatches fail before decoding.
- One run, one class, and untrainable folds fail distinctly.
- Comments and continued table lines behave exactly as `THD_datatable` defines.

### Exit criteria

- AFNI and `.1D` forms of the same fixture yield byte-identical canonical
  sample labels/folds and numerically identical feature matrices.
- The layer has no classifier dependency.
- All invalid contracts fail before the expensive analysis loop.

---

## Stage 3 — ROI and 1D decoding engine

**Status: ⬜ not started**

### Tasks

1. Implement reusable workspace allocation.
2. Implement fold-local scaling and constant-feature removal.
3. Implement multiclass pooled-covariance shrinkage LDA.
4. Implement equal-prior discriminant scores and deterministic ties.
5. Pool out-of-fold predictions per subject.
6. Compute confusion counts, per-class recall, balanced accuracy, ordinary
   accuracy, and chance-centered balanced accuracy.
7. Aggregate subjects with equal weight.
8. Preserve sample provenance for `-pred_out`.
9. Run once per atlas ROI using `THD_roi_pattern()`.
10. Run once over all columns for `.1D` input.
11. Add verbose fold diagnostics without flooding normal output.

### Independent reference tests

- Binary and multiclass predictions match a direct NumPy implementation.
- Every fold's mean, scale, centroids, covariance, shrinkage, and decision
  values match saved reference intermediates.
- An intentional leakage trap differs from a globally standardized answer and
  matches the fold-local answer.
- Pooled balanced accuracy differs from a naïve mean of foldwise balanced
  accuracies when a test fold lacks a class, and matches the specified result.
- Equal-subject group averaging differs from trial-pooled accuracy in an
  unbalanced fixture and matches the specified result.
- AFNI ROI and equivalent `.1D` inputs produce identical predictions/scores.
- Class-label renaming without changing membership leaves numeric results
  unchanged except for deterministic output order.
- Feature-column permutation leaves predictions unchanged within tolerance.
- Duplicate identical features remain stable under shrinkage.
- Constant-only data produce the documented invalid/error result.

### Exit criteria

- Observed predictions and all primary metrics match the independent reference.
- No test-fold values enter any fitted quantity.
- The core decoder can be tested without opening an AFNI dataset.

---

## Stage 4 — inference, searchlights, outputs, and MVP release

**Status: ⬜ not started**

### Tasks

1. Build the within-subject × run permutation plan with identity draw zero.
2. Rerun the complete fold-local pipeline for every relabeling.
3. Compute empirical upper-tail p values for primary balanced accuracy.
4. Compute BH-FDR across requested atlas/searchlight locations.
5. Accumulate synchronized searchlight max-null values and FWE p values.
6. Parallelize over locations while keeping immutable input/fold/permutation
   plans shared and workspaces thread-local.
7. Add volumetric searchlight parsing and ROI lists through shared helpers.
8. Add memory/compute preflight and `-memory_limit`/override conventions.
9. Write ROI/1D summary, subject, prediction, and confusion tables.
10. Write labeled AFNI searchlight datasets with history/provenance.
11. Add progress reporting that is safe under OpenMP and suppressible with
    `-quiet`.
12. Add CMake, legacy Make, package/install, help, and CTest integration.
13. Add a dedicated `run_mvpa_numeric.py` rather than making the already-large
    RSA runner own two program contracts; share fixture helpers where useful.
14. Document one fMRI and one `.1D` tutorial example.

### Inference tests

- Exhaustive tiny binary permutation p value matches independent enumeration.
- Monte Carlo identity inclusion and `p >= 1/nperm` hold.
- Labels never move across a subject or run.
- Permuting a separable fixture destroys accuracy under the null as expected.
- Refitting is demonstrated by a fixture whose shuffled-label covariance/class
  means differ from the observed fit.
- Raw p, BH q, and max-FWE match independent calculations.
- A synchronized two-location fixture gives the expected max-null; independent
  per-location shuffles would fail it.
- Invalid searchlights are excluded from FDR/FWE families.
- One versus multiple OpenMP threads produces identical tables and datasets.
- Atlas containing one whole-mask ROI matches a searchlight with the same voxel
  set.
- Searchlight center painting does not fill entire neighborhoods.
- Output brick labels/types and table schemas are stable.

### MVP release gate

- All Stage 0–4 tests pass under CMake and legacy Make builds.
- Required Python numeric dependencies are a non-silent CI gate, following the
  3dRSA precedent.
- Valgrind/ASan or the locally supported equivalent finds no leaks or invalid
  accesses on atlas, searchlight, permutation, and 1D paths.
- Help examples execute.
- Peak-memory refusal and explicit override are tested.
- Results are invariant to thread count and reproducible from the recorded
  seed.
- The source, help, tests, and this dashboard are reconciled on the same date.

At this point `3dMVPA` is useful: it supplies leakage-safe LORO linear decoding,
ROI/searchlight mapping, generic vectors, held-out predictions, and valid
permutation inference.

---

## Stage 5 — hardened input and spatial coverage

**Status: ⬜ not started**

Add usability only after the canonical MVP path is stable:

- Surface geodesic searchlights through the shared `THD_roilist` builder.
- Mask-optional whole-mesh behavior using the same explicit contract as 3dRSA.
- `SampleIDFile` for stable trial identity in prediction tables.
- `FoldFile` or a per-sample fold column so one container may hold multiple
  folds.
- Sample censoring with an explicit boolean file/column.
- Sample weights, with a clear distinction between training loss weights and
  score aggregation weights.
- Dataset/sub-brick selectors and an importer that can derive label files from
  AFNI sub-brick labels.
- Multiple beta datasets per run and safe concatenation.
- Saved fold manifests so another tool can reproduce the exact split.
- Optional missing-feature imputation learned within folds.
- Minimum-neighborhood-feature thresholds and maps of usable feature counts.

Do not overload `LabelFile` to encode sample IDs, folds, censoring, and weights.
A tidy sample-metadata file or long-form sample table is preferable once more
than one per-sample attribute is needed.

---

## Stage 6 — more estimators and metrics

**Status: ⬜ not started**

Recommended sequence:

1. Nearest-centroid classifier as a simple, fast, covariance-free reference.
2. Diagonal LDA for high-dimensional low-sample settings and performance
   comparisons.
3. Binary ROC AUC from held-out decision values.
4. Macro-F1 and class-balanced variants, with explicit pooled-OOF definitions.
5. Linear logistic regression with a fixed regularization default.
6. Probability calibration only through an inner training split.
7. Optional empirical class priors.
8. Linear SVM only after deciding whether to call/reuse AFNI's existing 3dsvm
   machinery or accept a new dependency; do not embed a second libsvm casually.

Every estimator must implement the same prediction API and permutation refit
contract.  Metrics must declare their valid class count, tail, chance scale,
and FWE family.  Binary AUC must define positive-class selection explicitly.

---

## Stage 7 — nested tuning and feature selection

**Status: ⬜ not started**

This stage is scientifically important and especially leakage-prone.

### Candidate additions

- Inner leave-one-training-run-out selection of shrinkage/regularization.
- Univariate ANOVA/top-k feature selection.
- Training-only variance thresholds.
- PCA fitted only on inner/outer training data.
- Recursive feature elimination only if runtime is acceptable.
- Classifier/parameter grids with a declared selection score and tie rule.

### Non-negotiable nesting rule

For each outer held-out run, every selection and tuning choice is learned from
that outer training set.  If hyperparameters are tuned, an inner fold loop uses
only outer-training runs.  The chosen pipeline is then refit on all outer
training data and evaluated once on the outer test run.  Every permutation
repeats this full nested process.

Outputs should report foldwise chosen parameters and selection counts.  A
convenience option that selects features once on all samples is deliberately
out of scope because it invalidates held-out accuracy.

---

## Stage 8 — group and generalization designs

**Status: ⬜ not started**

Within-subject LORO decoding does not establish cross-person generalization.
Add distinct, explicitly named schemes:

- leave-one-subject-out;
- leave-one-site-out;
- train on one run/domain, test on another;
- train on one subject group, test on another;
- cross-classification across tasks or stimulus sets;
- leave-one-group-of-stimuli-out when repeated stimulus identity permits it.

Cross-subject voxel features require a common aligned space and matching feature
sets.  The program should validate that assumption, not imply that atlas masks
alone solve functional alignment.  Hyperalignment/searchlight
hyperalignment would be a separate major feature, not an incidental flag.

Permutation exchangeability changes with each design.  Site, family, subject,
run, and stimulus blocks are not interchangeable.  Each new fold scheme needs
its own null derivation and exhaustive tiny-design test before release.

---

## Stage 9 — regression and encoding

**Status: ⬜ not started**

Extend the same sample/fold engine to continuous targets only after categorical
classification is stable.

Potential estimators:

- ridge regression;
- elastic net if a suitable solver is available;
- multi-output ridge for behavioral profiles;
- linear encoding from stimulus features to voxel/1D responses.

Potential held-out scores:

- Pearson correlation;
- coefficient of determination relative to a training-only baseline;
- mean absolute error;
- root mean squared error.

Target centering/scaling, nuisance regression, hyperparameter selection, and
the null must all be fold-local.  Permuting continuous targets within run may be
valid for exchangeable trials; temporally autocorrelated targets may require
circular shifts, blocks, or phase randomization.  Reuse 3dRSA time-null
machinery only when the target/design assumptions match.

---

## Stage 10 — time-resolved and EEG-oriented MVPA

**Status: ⬜ not started**

The MVP's `.1D` route admits EEG-derived vectors but does not yet understand
time or channel topology.  A later time-aware layer can add:

- one decode score per time point;
- temporal generalization matrices (train time × test time);
- channel or sensor neighborhoods;
- frequency/time-frequency feature axes;
- synchronized correction over time, channel/searchlight, or time × space;
- trial groups that remain intact across all time points;
- output formats that preserve axis labels without flattening them ambiguously.

The critical efficiency design is to load each trial tensor once, reuse fixed
folds/permutations across every time point, and batch linear algebra.  The
critical inference design is to keep an entire trial's label together across
time.  Permuting time bins independently would be invalid.

Temporal generalization is a matrix-valued output and should not be squeezed
into the MVP's scalar-per-location map API.  It deserves its own output and
memory contract.

---

## Stage 11 — performance and production scale

**Status: ⬜ not started**

### Performance priorities

1. Measure before optimizing: record ROI and searchlight benchmarks by samples,
   features, classes, folds, subjects, permutations, and threads.
2. Reuse thread-local workspaces; eliminate allocations inside fit loops.
3. Precompute fold indices and permutation label vectors.
4. Reuse feature extraction for every permutation at a location.
5. Vectorize class-centroid and covariance accumulation.
6. Prefer stable factorizations and exploit symmetric matrices.
7. Batch locations only when memory preflight accounts for the batch.
8. Add early design-cost warnings before datasets are fully loaded.
9. Investigate valid subject-level null combination for searchlights only with
   reference tests demonstrating equivalence to the intended group hypothesis.
10. Preserve exact thread reproducibility unless an explicitly named faster
    nondeterministic mode is ever justified.

### Memory model

The preflight should account for:

- resident mask and input datasets/1D matrices;
- ROI/searchlight index structures;
- canonical labels/folds/permutations;
- extracted sample × max-feature buffers;
- per-thread scaling, covariance, centroid, factorization, score, and prediction
  workspaces;
- per-location observed and p/q/FWE arrays;
- per-permutation spatial max-null arrays;
- output datasets coexisting with inputs; and
- optional prediction/confusion tables.

As in 3dRSA, reducing `OMP_NUM_THREADS` should reduce the estimated per-thread
peak and the program should say so when relevant.

---

## Testing strategy

### Test layers

1. **Pure numeric kernel tests** — tiny matrices, saved intermediates, no AFNI
   I/O.
2. **Input-contract tests** — tables, labels, folds, selectors, grids, and 1D
   errors.
3. **Independent end-to-end references** — NumPy/SciPy implementation that does
   not call the C routines or copy their control flow blindly.
4. **Inference tests** — exhaustive small permutation groups, empirical p, FDR,
   max-FWE, block containment.
5. **Spatial tests** — ROI extraction, searchlight geometry, painting, atlas ↔
   equivalent-searchlight agreement.
6. **Reproducibility tests** — seed replay and one-thread versus many-thread
   identity.
7. **Build/release tests** — CMake, legacy Make, SUMA/non-SUMA, install/package,
   help invocation.
8. **Failure tests** — every documented rejection should have a targeted case
   and a useful message fragment assertion.

### Leakage traps that must exist

- Global z-scoring appears to improve a deliberately shifted test fold; only
  fold-local scaling matches the reference.
- Global feature selection finds a planted test-only feature; nested selection
  does not.
- Covariance estimated from all runs differs from training-only covariance.
- Hyperparameter selection on the outer test fold differs from nested CV.
- Fitting once and permuting only predictions yields a different null from full
  retraining.

The first and third traps belong in the MVP.  The others become gates when
their corresponding features are added.

### Invariants

- Relabeling class names without changing membership does not change results.
- Permuting feature columns does not change a linear classifier's predictions
  beyond numeric tolerance.
- Duplicating every subject's trials changes trial-pooled counts but not the
  intended equal-subject group weighting.
- Adding a constant feature does not change predictions.
- AFNI and `.1D` representations of identical values agree.
- Observed results do not change when the requested permutation count changes.
- Raw observed effects do not change when FDR/FWE output is toggled.
- Thread count does not change any output.
- Searchlight execution order does not change the synchronized max-null.

### Performance regression fixtures

Keep at least three stable benchmarks:

- small atlas ROI, many permutations;
- moderate volumetric searchlight, few permutations;
- high-feature `.1D` data, many folds/classes.

Track wall time and peak-memory estimates, but keep correctness gates separate
from noisy timing thresholds.  Add a regression threshold only after a stable
baseline exists on CI hardware.

---

## Proposed command-line surface

This is a planning interface, not yet a compatibility promise.

### Input and geometry

```text
-dataTable @FILE
-dataTableFile FILE
-mask DSET
-roi_sel LIST
-searchlight NBHD
-surf SURFACE
-min_features N
-show_table
-show_design
```

Rules:

- Dataset input requires `-mask` for atlas/volumetric searchlight analysis.
- `.1D` input rejects spatial options.
- `-surf` is used only with a surface searchlight after Stage 5.
- `-roi_sel` is meaningful only for atlas masks.

### Model and cross-validation

```text
-classifier lda
-covariance shrinkage
-scale zscore|demean|none
-class_prior equal|empirical
-cv loro
-score balanced_accuracy
```

Only the first/default choice of each family needs to ship in the MVP, but
parsing should reject unimplemented values explicitly rather than accepting and
ignoring them.

### Inference

```text
-permute N
-exact
-seed S
-fdr
-fwe
```

`-exact` should refuse when the legal group is too large rather than pretending
to enumerate.  The identity is part of every exact or Monte Carlo set.

### Output and execution

```text
-prefix PREFIX
-pred_out FILE
-confusion_out FILE
-subject_table FILE
-memory_limit G
-memory_override
-jobs/OMP_NUM_THREADS via AFNI convention
-quiet
-verb N
```

Avoid options that duplicate AFNI-wide environment behavior unless 3dRSA has
already established a local convention worth sharing.

---

## Error, warning, and provenance policy

### Hard errors

- Missing required table column or unreadable file.
- Duplicate subject × run row.
- Fewer than two runs for a subject in LORO.
- Fewer than two global classes.
- Training fold missing a class.
- Label/sample count mismatch.
- AFNI grid mismatch or 1D feature-width mismatch.
- Mixed dataset and 1D modes.
- Non-finite input.
- All features unusable in every requested location.
- Spatial option with 1D input.
- Searchlight without a valid volumetric mask in the MVP.
- Requested exact permutation group too large.
- Output collision.
- Estimated memory above the hard limit without explicit override.

### Warnings

- Severe class imbalance.
- Very small samples per class/fold.
- A test fold missing a class, while pooled OOF balanced accuracy remains
  defined.
- Many fold-constant features or small effective neighborhoods.
- Too few permutations for the requested p-value resolution.
- Searchlight compute estimate is unusually large.
- Ordinary accuracy has a nonuniform empirical chance level.
- Explicit memory override.

Warnings should be specific and bounded; do not print one line per searchlight
center.  Aggregate counts/ranges and preserve them in provenance where useful.

---

## Deliberately out of scope for the MVP

- Regression/continuous targets.
- Trialwise nuisance regression.
- Feature selection or PCA.
- Hyperparameter tuning.
- Nonlinear kernels.
- SVM integration.
- Cross-subject training/testing.
- Cross-classification across tasks/domains.
- Temporal generalization matrices.
- Real-time/online decoding.
- Hyperalignment.
- Searchlight cluster-extent inference.
- TFCE.
- Analytic binomial/t-test inference as a substitute for the design-aware label
  null.
- Combining RSA and decoding in one command.

Each can be reconsidered after the basic decoder is correct.  None should be
partially enabled behind an undocumented option.

---

## Decisions to settle before coding

| ID | Decision | Proposed answer | Why it matters |
|---|---|---|---|
| D1 | Input row meaning | Subject × run container | Efficient for multi-brick fMRI and matrix-based EEG/behavior. |
| D2 | Label source | Required `LabelFile` | Explicit and identical across AFNI/1D inputs. |
| D3 | 1D orientation | Rows samples, columns features | Matches common ML matrices and is unambiguous. |
| D4 | MVP CV | Leave one run out | Defensible independent-fold default for fMRI. |
| D5 | Classifier | Multiclass shrinkage LDA | Linear and stable in high dimensions. |
| D6 | Scaling | Training-fold z-score | Generic-vector compatibility and leakage safety. |
| D7 | Priors | Equal | Avoid majority-class reinforcement. |
| D8 | Primary score | Pooled-OOF balanced accuracy | Defined under imbalance and avoids fold-weight ambiguity. |
| D9 | Group aggregation | Equal-subject mean | Population target is subjects, not trials. |
| D10 | Permutation blocks | Subject × run | Preserves acquisition/fold structure and class counts. |
| D11 | Searchlight null | Full retraining per draw | Slow but matches the scientific null. |
| D12 | Surface searchlight | Stage 5 | Keeps MVP build matrix smaller while shared support is prepared. |
| D13 | Inline table | File-first; decide exact support | Natural schema conflicts with current inline `InputFile`-last rule. |
| D14 | Covariance implementation | Verify/generalize existing helper | Prevent similarly named estimators from silently differing. |

If any proposed answer changes, update the scientific contract, help, reference
implementation, and tests together.

---

## Recommended execution order

1. **M1a — complete:** freeze the shared-core boundary, preservation contract,
   baseline gates, and ecosystem confinement.
2. **M1b — complete:** share BH-FDR and max accumulation across both programs.
3. **M1c — complete:** share neighborhood parsing, result painting, and SUMA
   surface construction through `thd_patterns`.
4. **M1d — complete:** use the generic arithmetic-only memory ledger while
   keeping every estimate and policy decision in `3dRSA`.
5. **M1e — complete:** reconcile build/install/test wiring and pass the full
   plain/OpenMP/SUMA/legacy equivalence matrix.
6. Next approval boundary: complete Stage 0's decoder-specific scientific/CLI
   contract and create its
   reference fixtures. M1 makes no classifier or estimand decisions on its
   behalf.
7. Implement `THD_sampleset` and folds with no classifier code.
8. Implement and independently verify LDA on `.1D` matrices first.
9. Feed the same kernel from one AFNI atlas ROI and prove AFNI ↔ 1D equivalence.
10. Add multi-ROI group summaries and observed prediction/confusion outputs.
11. Add within-run permutations and exhaustive tiny-design tests.
12. Add volumetric searchlights and synchronized max-FWE.
13. Add decoder-specific compute preflight and OpenMP determinism tests.
14. Complete build/package/help/tutorial integration and declare the MVP.

This order keeps failures local: numeric decoding is settled before spatial
parallelism, and observed analysis is settled before permutation inference.

---

## Definition of done

The MVP is done only when all of the following are true:

- A basic fMRI ROI and volumetric searchlight classification can be specified
  entirely through documented AFNI-style options and a `THD_datatable` file.
- The identical decoder runs on `.1D` feature matrices without spatial options.
- Every reported score is based exclusively on held-out predictions.
- Every learned transform is training-fold local.
- The default classifier is robust when features outnumber samples.
- Binary, multiclass, balanced, and unbalanced fixtures match independent
  numeric references.
- Single- and multi-subject estimands are explicit and tested.
- The label null respects subject and run blocks and refits the complete model.
- Raw, FDR, and spatial max-FWE inference match independent/exhaustive
  references.
- Output includes auditable folds, predictions, class order, seed, and analysis
  provenance.
- AFNI datasets and equivalent `.1D` matrices agree.
- One versus many threads is identical.
- Memory is estimated before the expensive searchlight loop.
- The complete 3dRSA suite remains green after shared-code extraction.
- CMake, legacy Make, package/install, and CTest paths include `3dMVPA`.
- Help examples and the numeric runner pass from a clean build.

Anything less may be a useful prototype, but it is not yet the minimum viable
scientific product.
