# 3dRSA / 1dTrdm Roadmap

This is the canonical roadmap for the AFNI representational-similarity tools.
It merges the former September execution roadmap with the detailed historical
`3dRSA` roadmap. The opening section is the current milestone record; the
long-form dashboard, implementation history, capability index, audits, and
engineering notes follow it for traceability.

Last reconciled against the source, focused tests, and competitor audit on
**2026-08-30**.

## Working rule

Complete, independently verify, and document one milestone at a time. At each
milestone boundary, stop and ask whether to begin the next item. A later item is
not silently promoted merely because the current one is finished.

## Native repeated-run IS-RSA sequence

This sequence is distinct from `-runwiseTable` crossnobis RSA. It concerns
continuous, preprocessed subject time series observed in multiple labeled runs
(for example, four movie runs) and run-level behavioral measurements (for
example, happiness after each movie). The repeated rows are never independent
subjects.

| Stage | Name | Status | Scientific contract and completion gate |
|:---:|---|:---:|---|
| **1** | **Native repeated-run ingestion and concatenation** | ✅ Complete (2026-08-30) | A long `Subj × Run` `-dataTable`, `-run_column`, and `-run_normalize` ingest native per-run time series. Runs are matched by label, detrended/normalized separately, then concatenated. The table must be balanced; corresponding run lengths must match; row order and OpenMP count do not change results; preconcatenated input is an exact reference. |
| **2** | **Run-resolved neural IS-RSA** | ✅ Complete (2026-08-30) | `-run_analysis separate` reports each labeled run plus the equal-run mean of signed association effects; `mean` reports only that summary. One subject-label permutation is synchronized across run × space. Separate-run BH/max-FWE cover the joint run × space family; the mean has its spatial family. Independent single-run references, shuffled rows/run order, ROI/searchlight, and thread identity pass. |
| **3** | **Run-varying behavioral state models** | ✅ Complete (2026-08-30) | `-run_model COLUMN:NN|AnnaK` builds a behavioral subject RDM for each matching run. `-run_center COLUMN subject` emits both a within-subject state-deviation model and an across-run subject-mean trait model. A null relabels each subject's complete behavioral trajectory as one unit across every run and location. Raw/centered numerical references, row/run-order identity, multiplicity, help, and provenance are the gate. This is repeated-measures permutation IS-RSA, not yet a general mixed-effects regression. |
| **4** | **Explicit run design and planned contrasts** | ✅ Complete (2026-08-30) | `-run_factor Condition`/`Movie` validates fixed run metadata and `-run_contrast NAME=FACTOR:POS-NEG` tests the equal-run level difference for every fixed or run-varying model. `-model Group:match` creates a categorical same-group geometry, so its happy−sad contrast is the representational Group × Condition interaction. Whole trajectories are relabeled synchronously. Per model, `separate` shares run/contrast × space BH/max-FWE and `mean` shares contrast × space; different behavioral models remain separate planned families. Exact exhaustive references, multiple factors/contrasts, ROI/searchlight, row/run-order identity, dataset labels, and thread identity pass. Movies/runs are explicitly fixed, not sampled random effects. |
| **5** | **Covariate-adjusted repeated-run model** | ✅ Complete (2026-08-30) | `-model_joint` now fits every run's fixed and run-varying model RDMs together after rank transformation when requested and column standardization. It reports conditional standardized coefficients and partial correlations per run and as equal-run means. `-run_contrast` combines the signed coefficients, so `Group:match` happy−sad is the fixed-movie representational Group × Condition interaction adjusted for run-level Happiness (and, with `-run_center`, separate state/trait geometries). Model-specific Freedman–Lane reduced residuals use one synchronized subject permutation across runs and space; coefficient/contrast families retain Stage-4 BH/max-FWE. Per-run model correlations, high-correlation warnings, exact-duplicate rejection, explicit coefficient labels/provenance, NumPy regression references, shuffled-row and OpenMP identity form the gate. This stage is partial association, not Group × Happiness moderation and not a random-movie mixed model. |
| **6** | **Population/random-effects and incomplete-run extension** | ⬜ Next | Decide whether random subject slopes and movie/run sampling require a true hierarchical model beyond synchronized permutation. Only then add unbalanced/missing runs, run-level bootstrap/uncertainty, and general joint/contrast families. The gate requires an explicit population estimand, missingness policy, variance-component behavior, type-I-error simulations, and thread/spatial reproducibility. |

Implementation order is deliberate. Stage 5 now adjusts fixed movie/run
coefficients and their planned contrasts for other model geometries, including
run-varying Happiness. It does **not** claim that four movies are a random sample
of a movie population, estimate random subject/movie slopes, or make a Group ×
Happiness moderation claim. Those population and interaction estimands must be
settled explicitly before Stage 6 implementation.

## Traditional RSA condition-row input

**Status: ✅ Complete (2026-08-30).** Traditional `-mode RSA` now accepts either
one multi-brick `InputFile` row per subject or an arbitrarily ordered long
`Subj × condition` table with one selected brick per row. The explicit pair
`-condition_column CCC -condition_order L1,L2,...` binds condition values to the
row/column order of every unlabeled `-model_mat`; it does not constrain physical
table order. A shared `THD_datatable_index_columns` Cartesian index retains
original row numbers and rejects empty, duplicate, unexpected, or missing key
cells before dataset loading. 3dRSA additionally requires each cell to resolve
to exactly one brick and subject-level metadata to remain constant across that
subject's condition rows. Shuffled long input is exactly identical to compact
multi-brick input, with dedicated duplicate/missing/multi-brick tests; the
complete numeric gate passes **299/299**.

## Current milestone record

| Order | ID | Milestone | Priority | Status | Completion gate |
|:---:|:---:|---|:---:|:---:|---|
| **1** | S1 | **Classic-RSA condition-label fixed-effects inference.** Explicit condition-axis nulls for fixed-model effects and paired contrasts, including single-subject ROI/searchlight analyses; subject sign flip remains the population default. | P1 | ✅ Complete (2026-08-29) | Exhaustive/random condition references; ordinary and runwise/crossnobis paths; ROI/searchlight max-FWE; thread identity; help/provenance; complete gate. |
| **2** | S2 | **Condition-mean removal / re-meaning.** Explicit subject-level centering across conditions for ordinary angle-based RDMs. | P1 | ✅ Complete (2026-08-29) | Pearson/cosine references; Euclidean/crossnobis invariance; ROI/searchlight coverage; compatibility default; provenance. |
| **3** | S3 | **Unequal-support covariance method note.** Derive pair-specific valid-run distance covariance before scheduling unequal-support `corr_cov`/`cosine_cov`. | P1/P2 | ✅ Complete (2026-08-29) | Analytic derivation; simulation; supported-estimand matrix; bounded implementation decision. |
| **4** | S4 | **Native temporal RSA product decision.** Separate observation×feature×time RDM movies from `3dRSA`'s fMRI table semantics. | P1/P2 | ✅ Complete (2026-08-29) | Written axes/inference/interchange contract; measured reuse; companion-program decision. |
| **5** | S5 | **Seed representational connectivity.** Compare a fixed seed ROI's representational geometry with non-overlapping atlas targets or searchlights. | P2 | ✅ Complete (2026-08-29) | Classic/IS-RSA references; subject/condition nulls; crossnobis; spatial max-FWE; overlap exclusion; complete gate. |
| **6** | S6 | **Trial-beta descriptors.** Explicit subject×run×trial×condition nesting for already-estimated beta-series input, without another first-level GLM. | P2 | ✅ Complete (2026-08-29) | Trial aggregation/crossnobis references; classic and second-order RSA; spatial equivalence; strict identity/count contracts. |
| **7** | S7 | **Temporal companion `1dTrdm`.** Labeled observation×feature×time RDM-movie producer and guarded independent-sample `3dRSA -model_series` bridge. | P1/P2 | ✅ Complete (2026-08-29) | Four metric references; both window reductions; strict axes/counts/provenance; row/thread identity; live round trip. |
| **8** | S8 | **`1dTrdm` temporal inference.** Labeled fixed-model comparison using population-subject or fixed-condition nulls and one complete time family. | P1/P2 | ✅ Complete (2026-08-29) | Exact 16-sign/720-condition references; Fisher-z contract; raw p/BH/max-FWE; alignment and seed/thread/layout identity. |
| **9** | S9 | **`1dTrdm` cross-temporal estimation.** Separate symmetric representational-recurrence and cross-time crossnobis products. | P1/P2 | ✅ Complete (2026-08-29) | Independent RDM-correlation/partition-pair references; symmetry; exact diagonals; unique triangles; planted recurrence. |
| **10** | S10 | **`1dTrdm` explicit feature neighborhoods.** Search overlapping labeled feature sets without treating column adjacency as spatial structure. | P1/P2 | ✅ Complete (2026-08-29) | Strict graph validation; ordinary/crossnobis/dynamics/cross-time references; time×neighborhood BH/max-FWE; graph/thread/layout identity. |
| **11** | F17 | **Complete subject-bootstrap extensions.** Stratified `-block` resampling and fixed-out-of-sample LOO prediction intervals. | P2 | ✅ Complete (2026-08-29) | Within-stratum count preservation; scalar/AnnaK/profile independent references; ROI/searchlight maps; thread identity; explicit non-cluster and no-refit contracts. |
| **12** | M1a | **Freeze the shared 3dRSA/3dMVPA infrastructure boundary.** Record baseline gates, shared-module responsibilities, scientific invariants, and ecosystem confinement before moving code. | Architecture | ✅ Complete (2026-08-29) | Exact source/test inventory; narrow API ownership; pre-release compatibility policy; M1b–M1e gates; no runtime-code change. |
| **13** | M1b | **Extract shared BH-FDR and max accumulation.** Introduce dataset-agnostic `thd_mapinfer` primitives and make both `3dRSA` and `1dTrdm` consumers. | Architecture | ✅ Complete (2026-08-29) | Direct plain/masked/tied/aliased BH and max unit gate; CMake/SUMA/OpenMP build; unchanged 276 + 36 complete gates. |
| **14** | M1c | **Extract shared spatial geometry helpers.** Move neighborhood parsing, atlas/searchlight painting, and SUMA geodesic ROI-list construction into `thd_patterns`. | Architecture | ✅ Complete (2026-08-29) | Direct grammar/painting unit gate; real volume/SUMA equivalence; CMake/SUMA/OpenMP and unchanged 276 + 36 complete gates. |
| **15** | M1d | **Introduce the generic memory ledger.** Replace the RSA-only accounting container with shared byte categories and arithmetic while retaining every estimate and policy decision in `3dRSA`. | Architecture | ✅ Complete (2026-08-29) | Direct total/preserved-metadata unit gate; existing refusal/override thresholds; CMake/SUMA/OpenMP and unchanged 276 + 36 complete gates. |
| **16** | M1e | **Close shared-core integration.** Reconcile CMake, legacy Make, install/package, and CTest wiring and run the plain/OpenMP/SUMA equivalence matrix. | Architecture | ✅ Complete (2026-08-29) | Three CMake configurations, sequential legacy GCC/OpenMP build, install/package audit, shared units, 276 SUMA / 273 non-SUMA RSA checks, and 36 temporal checks. |

M1 and repeated-run IS-RSA Stages 1–5 are complete. Repeated-run Stage 6 and
decoder-specific 3dMVPA Stage 0 remain proposed design boundaries. Neither is
started until explicitly approved. New
scientific work should still be promoted only after its estimand,
exchangeability unit, correction family, and validation gate are written down.

## Current delivery summary

- **Inference scope:** `3dRSA` supports classic RSA and IS-RSA with population
  subject nulls, fixed-condition nulls where defined, synchronized spatial or
  time×space max-FWE, FDR, bootstrap intervals, model contrasts, regression,
  commonality, fitted models, and temporal nulls under their documented
  contracts.
- **Input scope:** ordinary condition-pattern tables in compact subject-row or
  arbitrarily ordered one-brick `Subj × condition` form, runwise crossnobis input,
  residual-noise normalization, missing/reordered run mappings, explicit
  trial-beta descriptors, native balanced repeated continuous runs, fixed or
  run-varying behavioral models (including state/trait decomposition),
  fixed run-design factors and planned level contrasts, categorical group
  geometry, run-resolved joint conditional regression, fixed/per-location
  models, and surface or volume atlas/searchlight domains are delivered.
- **Temporal companion:** `1dTrdm` consumes prepared time-row×feature-column
  observations and produces labeled subject RDM movies for correlation,
  cosine, Euclidean, or balanced crossnobis estimators. It does not ingest raw
  vendor data or estimate first-level trials.
- **Temporal inference:** one labeled fixed RDM may be tested across time using
  synchronized subject sign flips or condition relabelings. Cross-temporal
  RDM-dynamics and cross-time crossnobis remain descriptive products.
- **Feature neighborhoods:** a strict `Neighborhood Feature` graph supports
  overlapping generic-feature searches. Fixed-model inference spans one
  complete time×neighborhood family; recurrence and cross-time estimates are
  computed independently within each neighborhood.
- **Subject uncertainty:** `-block` now stratifies subject bootstrap draws,
  preserving the observed number of subjects in every stratum. With `-loo`,
  the same draws resample completed held-out prediction/target rows and write
  `_looBootLo/_looBootHi`; these bounds do not include fold-refitting
  instability and are not whole-cluster intervals.
- **Interchange boundary:** `1dTrdm -model_series_out independent` writes an
  all-feature group-mean series for an independent downstream `3dRSA` sample.
  Same-subject fusion still requires leave-one-subject or subject-indexed
  model movies.

The registered required-dependency integration gate currently passes all
**296/296** `3dRSA` checks and **36/36** focused `1dTrdm` checks.

## Current decisions and specialist holds

- The unequal-support covariance derivation supports a bounded future
  exchangeable-condition fixed-model slice. General heterogeneous precision,
  repeated mappings, regression/commonality, and resampling extensions remain
  on methodological hold. See
  [`3dRSA_unequal-support-covariance-note-2026-08-29.md`](3dRSA_unequal-support-covariance-note-2026-08-29.md).
- `1dTrdm` is the native temporal companion rather than an overloaded `3dRSA`
  input mode. See
  [`3dRSA_temporal-product-decision-2026-08-29.md`](3dRSA_temporal-product-decision-2026-08-29.md).
- BRSA/GBRSA, PCM, generic model-object frameworks, arbitrary metric plug-ins,
  topological RSA, and a unified RSA/decoding pipeline remain interoperability
  targets or deliberately out of scope.
- Directional decoding generalization, coordinate-derived sensor/source
  neighborhood builders, cross-temporal inference, LD-t/cvMANOVA, partial
  model RDMs, and cluster/TFCE correction are not scheduled milestones.

## Related design and audit documents

| Document | Role | Currency / reading guidance |
|---|---|---|
| [`3dMVPA_ROADMAP.md`](3dMVPA_ROADMAP.md) | Full specification for a separate AFNI decoding/classification program and the M1 shared-core contract. | M1a–M1e are complete. Decoder-specific Stage 0 requires milestone approval; the decoder itself remains unimplemented. |
| [`3dRSA_competitor-audit-2026-08-29.md`](3dRSA_competitor-audit-2026-08-29.md) | Ranked 2026-08-29 comparison with eight RSA/MVPA software families. | Best short rationale for priorities. Its original gap statements are a dated snapshot; recommendation cells record S1–S10 delivery. |
| [`3dRSA_rsa-tool-survey.md`](3dRSA_rsa-tool-survey.md) | Broad 2026-07-30 package-by-package survey and feature checklist. | Useful source/history, but many `3dRSA` gap cells are obsolete. Do not use its suggested next steps as the current queue. |
| [`3dRSA_temporal-product-decision-2026-08-29.md`](3dRSA_temporal-product-decision-2026-08-29.md) | Scientific and architectural contract separating `1dTrdm` temporal estimation from `3dRSA` spatial consumption. | Current design record for axes, estimators, cross-temporal meanings, inference, interchange, and S7–S10 gates. |
| [`3dRSA_unequal-support-covariance-note-2026-08-29.md`](3dRSA_unequal-support-covariance-note-2026-08-29.md) | Mathematical derivation and simulation for covariance weighting with unequal valid-run support. | Current method hold/GO record. A bounded fixed-model slice is methodologically ready; general enablement is not. |

---

## Detailed 3dRSA dashboard, implementation history, and audits

The remainder is the retained long-form roadmap. It includes historical
snapshots and completed execution plans, so dates and check counts inside those
sections describe their recorded milestone boundary unless explicitly labeled
as current.

## Status dashboard

Last reconciled against the source and numeric tests on **2026-08-30**.
**✅** means implemented and verified, **🚧** means active or partly complete,
**⬜** means not started, **⛔** means blocked on an external dependency, and
**❓** means an open DECISION that is not yet a committed task -- work that would
only be scheduled if the decision goes that way.
In the **Next** column, `hold` means a methodological contract must be reviewed
before implementation is scheduled; it is distinct from an external blocker.
The IDs record where an item originated; they are stable cross-references, not
priority codes: unprefixed IDs came from the original implementation plan,
`F` means a later feature/follow-on, `A` an audit finding, `B` build/release
engineering, and `M` an architecture decision. Deliberately out-of-scope ideas
are not tasks.

### Ready work — sorted by priority

M1 has closed the shared-core architecture sequence. The repeated-run sequence
has separately delivered Stages 1–5. Its Stage 6 population/random-effects
contract and decoder-specific Stage 0 are ready for explicit approval; neither
is silently promoted.

| Priority | ID | Task | Status | Why this is next / still open |
|:---:|---|---|:---:|---|
| 1 | Repeated-run Stage 6 | Freeze population/random-effects and incomplete-run estimands | ⬜ | Decide fixed versus sampled movies, random subject/movie effects, missing-run policy, uncertainty axes, interactions, and correction families before implementation. |
| 1 | 3dMVPA Stage 0 | Freeze the decoder scientific and CLI contract | ⬜ | Settle sample axes, folds, classifier math, scores, nulls, outputs, and reference fixtures before implementation. |

### Why the latest delivered work is useful

With F4, F21, and F22 complete, F23 made tied-model comparison fairer and F5b
extended the spectral temporal null spatially. Their delivery empties the
committed feature queue rather than exposing another user-blocking RSA gap.

#### F22 — condition-held-out fitted-model cross-validation · ✅ Delivered 2026-08-27

F7 can learn a nonnegative weighted mixture such as `visual + semantic + task`
from other subjects and score it on a held-out subject. That establishes
generalization across people, but the same stimuli and condition dyads occur in
training and testing. A flexible mixture may therefore learn weights tailored
to peculiarities of the experiment's particular condition set even when its
held-subject score is excellent.

F22 is useful because it tests whether the learned representational mixture
explains genuinely unseen stimuli. It prevents condition-set adaptation from
being mistaken for stimulus generalization, supports the stronger claim that a
theory generalizes across both people and stimuli, and makes F14 comparisons
between fitted models substantially more persuasive. A visual-plus-semantic
mixture might, for example, transfer cleanly to new participants but fail on
relationships among categories that were absent during fitting; the current
subject-only outer folds cannot expose that failure.

The delivered `-fit_condfold FILE` contract keeps condition leakage explicit.
It learns only from other-subject dyads among training conditions and tests only
held-subject dyads among held-out conditions. Dyads joining one training and one
held-out condition are excluded. FILE gives one fold label per condition; every
fold must hold at least three conditions and leave at least three for training.
The reported `_cvR` is `tanh(mean Fisher z)` over valid subject × condition
folds, descriptive weights average those same folds, and every condition-label
null draw completely refits the two-axis design. F14 fitted contrasts reuse the
same paired folds. Ordinary and runwise/crossnobis ROI/searchlight classic RSA
are supported; IS-RSA rejects the option because its fitted RDM axis contains
subjects rather than stimuli. F6's fixed-model dual bootstrap remains an
uncertainty reference, not a substitute for this cross-validation.

F22 was prioritized because it closed a scientific-validity gap in an already
shipped flexible model rather than adding another statistic. Independent effect
and weight references, fitted contrasts, malformed-fold contracts,
atlas/searchlight agreement, thread identity, and F21 unbalanced-crossnobis
composition now pass.

#### F4 — whitened unbiased RDM comparison · ✅ Delivered 2026-08-27

Crossnobis supplies unbiased neural distances, but its RDM entries are neither
equally precise nor independent. Some condition pairs are noisier, and distances
that share a condition have correlated sampling error. F21 can additionally
leave different pairs supported by different numbers of valid runs. Ordinary
Pearson, Spearman, or cosine comparison largely treats all upper-triangle entries
as equally informative.

F4 now carries the published zero-distance covariance into comparison-space
`corr_cov`/`cosine_cov`, downweight unreliable directions, and account for
correlations among distances. This should improve efficiency, make model
comparisons fairer when models emphasize differently noisy pairs, and extract
more of the value of run-independent crossnobis estimates.

This is distinct from the delivered `-noise_norm`: voxel-noise whitening acts on
patterns before crossnobis distances are estimated, whereas F4 whitens the RDM
comparison using uncertainty among the distance estimates. The delivered first
contract uses the Diedrichsen et al. exchangeable-condition approximation
`V=(C C') o (C C')`, implemented through its exact centered-second-moment/CKA
equivalent rather than materializing and inverting the dyad covariance. It is
restricted to balanced classic-RSA `-runwiseTable` input and fixed model RDMs.
Primary effects, fixed-model contrasts, Nili noise ceilings, subject bootstrap,
atlas ROIs, volumetric searchlights, residual `diag|shrinkage` voxel whitening,
FDR, and synchronized max-FWE are supported.

That restriction is substantive: F21 missing/repeated-condition mappings give
different dyads different run support, so their covariance is not the balanced
`V`; condition resampling changes it again. Those paths, IS-RSA outer matrices,
regression/commonality, and fitted models fail explicitly rather than silently
using unjustified weights. Direct dense `V^-1` NumPy references verify both
metrics and their composition with voxel whitening. See [Audit F4](#f4-whitened-unbiased-rdm-comparison).

#### F23 — expected Spearman rho-a · ✅ Delivered 2026-08-28

Categorical model RDMs commonly contain many ties—for example, zero for every
within-category pair and one for every between-category pair. Ordinary Spearman
assigns average ranks to ties, so models with different amounts or structures
of tying are not always compared on an entirely level scale. Expected Spearman
rho-a instead reports the expected correlation under random tie breaking.

That makes categorical-versus-continuous and differently tied categorical model
comparisons fairer while retaining a familiar correlation interpretation. It is
directly useful for category, task, identity, and binary-group hypotheses. Its
scientific reach is smaller than F22 or F4 because it improves one comparison
metric rather than opening a new generalization claim, but it is likely a
compact, well-bounded addition. `-metric rhoa` now implements the closed-form
expectation from average ranks with the untied rank variance. It supports
primary effects, paired contrasts, bootstrap intervals, LOO, noise ceilings,
temporal nulls, ROI/searchlight inference, and a cached fixed-model searchlight
path. It intentionally rejects joint/nuisance regression; ordinary `spearman`
remains the rank-regression objective.

#### F5b — phase-randomization searchlights · ✅ Delivered 2026-08-28

The delivered ROI-level `-null phase` preserves each subject's mean and power
spectrum while destroying phase-locked temporal alignment. F5b now applies that
spectrally matched null in moving searchlights, producing spatial maps of where
continuous-time IS-RSA exceeds what would be expected from autocorrelated series
with the same spectra.

This is valuable for movies, stories, music, continuous social interaction, and
resting-state-like analyses. It complements the existing circular-shift
searchlight: a circular shift preserves the exact time series and explores its
rotations, whereas phase randomization generates a much richer family of
spectrum-matched surrogates. Every worker now Fourier-transforms each
searchlight's local subject means once, reuses those spectra across every draw,
and replaces the cache when it advances to another center. The stateless phase
family remains shared across centers, preserving synchronized spatial max-FWE
and exact thread-count reproducibility without retaining a whole-brain spectral
cube.

### Holds, deferrals, blockers, and decisions

| ID | Task | Status | Queue | Reason |
|---|---|:---:|:---:|---|
| 7c | Mask-optional volumetric searchlight | ⬜ | deferred | Deferred by user request. Revisit only after defining and recording a reproducible implicit-volume domain/automask policy. |
| F12 | AFNI-only plotting consumer | ⛔ | blocked | AFNI PR #919 remains open and bundled `1dplot.py` still lacks the required plot paths. |
| M1 | Shared map-inference core | ✅ | complete | M1a–M1e pass the build/install/test matrix with changes confined to the dedicated RSA/MVPA ecosystem. |
| M2 | `3dMVPA` searchlight/ROI decoding | ⬜ | after Stage 0 | Product direction approved. Shared readers/geometry/permutation primitives help, but samples, folds, supervised estimators, and decoder outputs remain new work. |

### Completed — build, correctness, and regression infrastructure

| ID | Task | Status | Next | Evidence |
|---|---|:---:|:---:|---|
| B1 | Native AFNI Make/CMake/package/test integration | ✅ | — | Verified on 2026-08-22: the SUMA-enabled CMake target and plain legacy-Make target both build; CMake generates the `bin/3dRSA` install rule; packaging maps it to `corebinaries`; and the registered CTest passes. This supersedes the old standalone `build.sh`. |
| B2 | Clean private-module warnings in the legacy GCC 14 build | ✅ | — | Added a defensive table-dimension guard and separated misleading one-line conditionals. A legacy-Make compile of all six private translation units reports zero 3dRSA-source warnings; warnings originating in shared AFNI headers/FFT support remain out of scope and are suppressed only inside the namespaced wrapper. |
| 1 | Correctness and input-contract fixes (1a–1f) | ✅ | — | Implemented in `3dRSA.c` / `thd_simmatrix.c`; the targeted numeric regressions pass. Warning cleanup is tracked separately as B2. |
| 1i | Fix multi-model `-model_dset` searchlight diagnostic crash | ✅ | — | The collinearity sampler now performs the same on-the-fly sphere reduction as the streaming analysis instead of reading atlas-only `cmean`; a non-quiet two-dataset-model searchlight regression passes. |
| 1j | Fix `-save_rdm` plotting hints for `-model_dset` | ✅ | — | Plotting output now explains that a per-ROI dataset model has no single RDM file and emits heatmap commands only for fixed model files that were actually written; regression-tested. |
| 2 | Automated numeric regression runner | ✅ | — | `src/pmolfese/tests/run_numeric.py`: **276 passed, 0 failed** against the CMake/SUMA binary on 2026-08-29. |
| 2b | Make the numeric suite a non-silent CI gate | ✅ | — | CTest invokes the runner with `--require-deps`, which fails when NumPy/SciPy/nibabel are missing; the default direct invocation retains a convenient local skip. Both paths were verified on 2026-08-22. |
| 2c | Reconcile stale internal test/source comments | ✅ | — | The runner now describes its broader regression set, and the runwise fixture comment reflects the implemented end-to-end crossnobis estimator. |
| 2d | Close the runwise coverage bundle | ✅ | — | Runwise contrasts match an independent paired Fisher-z calculation and are thread-reproducible; missing `ResidFile` is explicitly rejected; exact identity-noise whitening reduces to the plain crossnobis result. |

### Completed — original scientific and spatial milestones

| ID | Task | Status | Next | Evidence |
|---|---|:---:|:---:|---|
| 3a | Reliability/noise-ceiling map bricks | ✅ | — | IS-RSA `reliability` and classic-RSA `nc_low`/`nc_high` output tests pass. |
| 3b | Model contrasts and paired sign-flip/signed-rank tests | ✅ | — | IS-RSA, ordinary classic-RSA, and runwise crossnobis contrast, FWE, sign reversal, identity, independent-reference, and thread-reproducibility tests pass. |
| 3c | Pairwise model commonality | ✅ | — | The three raw decomposition terms and two added partial-R² effects have NumPy value, identity, composition, subject-bootstrap, map, and thread tests. IS-RSA and classic RSA both have independently verified A-given-B/B-given-A Freedman–Lane p/FWE families; `common` retains its complete-null family. |
| 4a | Runwise classic-RSA input contract | ✅ | — | Valid and malformed runwise-table cases are covered by the runner. |
| 4b | Cross-validated squared-Euclidean/crossnobis ROI estimator | ✅ | — | Independent NumPy pipeline, significance, negative-distance, paired model-contrast, and thread tests pass. |
| 4c | Residual covariance and `diag`/`shrinkage` whitening in ROI mode | ✅ | — | Both whitening modes match independent NumPy calculations; run-label, thread, missing-residual, and identity-covariance tests pass. |
| 5 | Mahalanobis behavioral profiles | ✅ | — | NumPy, orthogonality, column-order, and invalid-column tests pass. |
| 6 | Classic-RSA runwise/crossnobis searchlight | ✅ | — | `-runwiseTable` now runs volumetric moving-neighborhood crossnobis with `none`, `diag`, or `shrinkage` noise normalization; all three match independent NumPy references everywhere, with valid FWE bricks and exact thread reproducibility. |
| 6b | Ordinary same-data classic-RSA searchlight | ✅ | — | Delivered 2026-08-25: `-dataTable -mode RSA -searchlight` now applies the established ordinary condition-pattern RDM estimator at every volume or surface neighborhood. A runtime warning and persistent table metadata distinguish it from crossnobis, and classic tables now correctly identify their null as subject sign flips; an independent NumPy reference, whole-atlas equivalence, valid max-FWE, and exact thread reproducibility pass. |
| 7 | `-model_dset` under searchlight | ✅ | — | Mean and pattern single-model atlas↔searchlight equivalence tests pass, as does the non-quiet two-model streaming diagnostic regression. |
| 7b | Mask-optional whole-mesh surface searchlight | ✅ | — | SUMA tests pass: no-mask execution, all-node coverage, and exact equivalence to an all-ones mask. |

### Completed — follow-on scientific features

| ID | Task | Status | Next | Evidence |
|---|---|:---:|:---:|---|
| F1 | Subject bootstrap and confidence intervals | ✅ | — | `-bootstrap N [-boot_ci P]` adds percentile bounds for primary model effects in plain/joint/nuisance-adjusted IS-RSA and plain/joint classic RSA, including crossnobis and per-location `-model_dset` searchlights. F17 supplies paired-contrast and pairwise-commonality bounds; F8 extends the same synchronized resampling contract to all ten three-predictor quantities. One immutable `THD_resample_set` is separate from `PERM_set`; independent references verify compact regression/decomposition refits, Fisher-z subject averaging, and exclusion of IS-RSA repeated-subject diagonal artifacts. |
| F17 | Subject-bootstrap extensions | ✅ | — | Completed 2026-08-29. Existing contrast, regression, and commonality intervals now compose with a stratified `-block` bootstrap that samples subjects within each stratum and preserves stratum sizes. `-loo -bootstrap` writes `_looBootLo/_looBootHi` for scalar NN, AnnaK, and multivariate-profile accuracy by resampling completed held-out prediction/target rows. Provenance distinguishes these fixed-OOF bounds from fold-refitting uncertainty and from whole-cluster bootstraps. Independent interval, map, and thread references pass. See [Audit F17](#f17-subject-bootstrap-extensions). |
| F2 | Stimulus/condition bootstrap | ✅ | — | `-cond_bootstrap N [-boot_ci P]` synchronously resamples every subject's neural condition axis and all model axes in classic RSA, including plain/joint ROI and runwise crossnobis ROI/searchlights. `-cond_group FILE` resamples variable-sized labeled groups as units. Duplicate-original-condition dyads are omitted, and independent NumPy references plus thread/map tests verify the intervals. |
| F6 | Dual subject × condition bootstrap | ✅ | — | Delivered 2026-08-27. Equal `-bootstrap N` and `-cond_bootstrap N` requests combine subject-only, condition-only, and simultaneous resampling variances with the finite-sample two-factor correction and a `min(subjects, condition groups)-1` t interval. Fixed plain/joint classic-RSA models and paired fixed-model contrasts write `_dualLo/_dualHi` tables/maps in ordinary or runwise/crossnobis atlas/searchlight analyses; grouped conditions use the number of independent groups. Six independent formula/contract/group/joint/contrast/map/thread checks pass. See [Audit F6](#f6-dual-subject--condition-bootstrap). |
| F3 | Circular-shift null for continuous IS-RSA | ✅ | — | `-null timeshift [-min_shift K]` independently rotates every subject's equal-length, gap-free ROI-mean series and tests primary separate-model effects. One immutable shift set drives atlas/searchlight, fixed/`-model_dset`, p/FDR/max-FWE, and thread reproducibility; F19 now serves every draw from a relative-lag table. |
| F4 | Whitened unbiased RDM comparison | ✅ | — | Delivered 2026-08-27. `-metric corr_cov|cosine_cov` applies the Diedrichsen zero-distance covariance to balanced runwise classic-RSA crossnobis RDMs through the exact centered-second-moment equivalent. Fixed effects/contrasts, ceilings, subject bootstrap, residual voxel whitening, atlas/searchlights, and spatial inference pass direct dense-`V^-1` references. Unequal-support F21 mappings, condition bootstrap, IS-RSA, and covariance-weighted regression remain explicit rejections. See [Audit F4](#f4-whitened-unbiased-rdm-comparison). |
| F5 | ROI-first phase-randomization null | ✅ | — | Delivered 2026-08-26. `-null phase` independently rotates every subject × positive-frequency Fourier bin while retaining DC, the even-length Nyquist bin, and the complete power spectrum. A stateless seeded family drives primary, paired-contrast, joint, nuisance-adjusted, p/FDR, and synchronized ROI max-FWE inference with fixed/`-model_dset` models. All neural metrics match independent NumPy references and 1-vs-6-thread output is exact. |
| F5b | Phase-randomization searchlights | ✅ | — | Delivered 2026-08-28. Each worker caches one center's local subject spectra across every draw, then replaces them at the next center; the stateless shared phase family supplies synchronized spatial max-FWE. Independent local-series r/p/FWE reconstruction, memory accounting, provenance, and exact 1-vs-6-thread output pass. See [Audit F5b](#f5b-phase-randomization-searchlights). |
| F23 | Expected Spearman rho-a | ✅ | — | Delivered 2026-08-28. `-metric rhoa` computes expected Spearman correlation under independent random tie breaking using the closed form, supports scalar comparison/contrast/bootstrap/LOO/ceiling/temporal-null ROI and searchlight paths, and joins the fixed-model Mantel cache. Closed-form, tied/no-tie, cache, contract, and thread tests pass. See [Audit F23](#f23-expected-spearman-rho-a). |
| S5 | Seed representational connectivity | ✅ | — | Delivered 2026-08-29. `-seed_mask SSS [-seed_roi VALUE]` builds one fixed seed geometry from the same subjects and estimator as every target, then maps its relation to non-overlapping atlas ROIs or searchlights. Classic ordinary/runwise-crossnobis and pattern/second-order IS-RSA paths pass independent effects, exhaustive condition/blocked-subject nulls, spatial FWE, overlap, provenance, and thread tests. See the current S5 record above. |
| S6 | Trial-beta descriptors | ✅ | — | Delivered 2026-08-29. A `TrialFile` column in `-runwiseTable` supplies one unique trial ID and condition per already-estimated beta sub-brick; trials are averaged within run before pair-valid crossnobis. Classic/second-order, atlas/searchlight, thread, provenance, and malformed-nesting references pass. First-level GLM fitting and trial×trial RDM inference remain external/separate. See the current S6 record above. |
| F10 | Model-aware and multivariate LOO prediction | ✅ | — | Delivered 2026-08-26. Scalar NN/euclid/absdiff retain neural-neighbor prediction; AnnaK now uses foldwise training-only neural-typicality regression; multivariate `:euclid`/`:mahal` models predict the complete profile and report the equal-weight mean measure-wise held-out correlation. Whole profiles move together under label nulls, exact target/estimand duplicates alone share an FWE family, and independent observed/exhaustive-null/FWE/searchlight/thread tests pass. See [Audit F10](#f10-model-awaremultivariate-loo-prediction). |
| F7 | Constrained fitted/weighted-component model | ✅ | — | Delivered 2026-08-27. `-model_fit NAME=A,B,... [-fit_ridge R]` learns standardized nonnegative ridge weights only from other subjects, scores the held subject, and repeats the complete nested fit for every synchronized subject- or condition-label draw. Classic/IS independent references, component recovery, p/FDR/max-FWE maps, contract rejection, and 1-vs-N-thread tests pass. See [Audit F7](#f7-constrained-fitted-component-model). |
| F14 | Held-out fitted-model comparisons | ✅ | — | Delivered 2026-08-27; explicit superiority completed 2026-08-30. `-model_contrast FITA-FITB` compares paired held-fold Fisher-z accuracies. Alignment/legacy inference shares each relabeling and completely refits both models. `-contrast_hypothesis superiority` instead averages common-valid fold differences within each outer subject and applies an ordinary centered paired outer-subject bootstrap with plus-one raw/max-FWE tails. Independent classic/IS effects, exact bootstrap p/FWE, labels, contracts, and thread tests pass. See [Audit F14](#f14-held-out-fitted-model-comparisons). |
| F22 | Condition-held-out fitted-model CV | ✅ | — | Delivered 2026-08-27. `-fit_condfold FILE` nests explicit held-condition folds inside held-subject fitting: train only on other-subject train/train dyads, score only held-subject held/held dyads, and exclude cross-boundary pairs. Complete condition-label null refits, F14 contrasts, ordinary/runwise atlas/searchlights, F21 mappings, provenance, contracts, and thread reproducibility pass independent references. See [Audit F22](#f22-condition-held-out-fitted-model-cv). |
| F8 | Three-predictor commonality | ✅ | — | Delivered 2026-08-27. `-model_commonality A,B,C` reports seven exhaustive raw regions plus three conditional partial-R² effects. Unique/partial quantities use their two-predictor reduced-model nulls; shared regions use complete relabeling. IS/classic exhaustive references, bootstrap, atlas/searchlight maps, FWE, contracts, and thread tests pass. See [Audit F8](#f8-three-predictor-commonality). |
| F21 | Unbalanced runwise/crossnobis input | ✅ | — | Delivered 2026-08-27. An optional per-row `ConditionFile` maps local beta bricks into a lexical global condition order, permits missing and repeated conditions, averages within-run repeats, and gives every condition pair its own valid-run crossnobis denominator. It composes with residual whitening, classic and second-order RSA, ROI/searchlight execution, and existing inference while preserving the old balanced path exactly. See [Audit F21](#f21-unbalanced-runwisecrossnobis-input). |
| F18 | Circular-shift contrasts and regression | ✅ | — | Delivered 2026-08-26. The one synchronized relative-lag neural null now tests paired fixed/per-location model contrasts, joint coefficients, and separately fitted nuisance-adjusted coefficients in atlas/searchlights. Fixed regression pseudoinverses are reused across draws; commonality, LOO, and segmented-series contracts remain deliberately separate. See [Audit F18](#f18-circular-shift-contrasts-and-regression). |
| F13 | `-model_dset` model contrasts | ✅ | — | Delivered 2026-08-22: either side of an IS-RSA `-model_contrast A-B` may be a per-location `-model_dset`; both modality RDMs are rebuilt in the same atlas ROI/searchlight and enter the existing paired same-relabeling Mantel difference with its own FDR/max-FWE family. Dataset-vs-dataset and mixed fixed-vs-dataset paths pass exhaustive-null, atlas/map, label, F9-interaction, and thread tests. |
| F15 | Classic-RSA commonality null | ✅ | — | Delivered 2026-08-25. Ordinary and runwise/crossnobis classic RSA report the mean subject decomposition. Unique/partial effects use subject-specific reduced fits with one shared condition relabeling per draw; `common` uses the matching complete neural relabeling. Exhaustive 6! p/FDR/FWE, bootstrap, atlas/searchlight, map-label, metric-contract, and thread tests pass. See [Audit F15](#f15-classic-rsa-commonality-null). |
| F16 | Runwise noise ceiling and LOO readouts | ✅ | — | Delivered 2026-08-25. `-noise_ceiling` now applies the Nili lower/upper bounds to each subject's independently estimated crossnobis RDM: `nc_low` evaluates against the mean of the other subjects, while the conventional optimistic `nc_high` includes the subject. Residual whitening precedes crossnobis; no condition model is fit. Independent unwhitened/whitened references, AFNI bricks, atlas↔searchlight equivalence, and thread identity pass. See [Audit F16](#f16-runwise-crossnobis-noise-ceiling). |
| F20 | Time-resolved EEG/MEG–fMRI fusion (`-model_series`) | ✅ | — | Delivered 2026-08-25. An ordered `TIME_LABEL MATRIX_FILE` list drives classic or second-order IS-RSA in ROIs/searchlights; one shared relabeling supplies BH FDR and max-statistic FWE over the joint time × space family. Long-form tables preserve time labels, AFNI bricks use deterministic `t####` labels, and six independent/exhaustive/reference/map/thread/contract checks pass. See [Audit F20](#f20-time-resolved-fusion). |

### Completed — performance and scale safeguards

| ID | Task | Status | Next | Evidence |
|---|---|:---:|:---:|---|
| F9 | Searchlight fixed-model permutation optimization | ✅ | — | Fixed Pearson/Spearman/rho-a label-null triangles are cached across centers/workers. The original 256-center, two-model + contrast, 500-relabeling Spearman OMP1 benchmark fell from 15.05 s to 0.51 s with byte-identical output; F23 adds exact rho-a atlas/cache equivalence. |
| F11 | Searchlight memory preflight and guard | ✅ | — | Preflight covers resident inputs, neighborhoods, result/null/output arrays, OpenMP scratch, crossnobis whitening, cached classic RDMs, F19 lag tables, F18 fixed regression designs, and F5b per-worker local spectra; it warns/refuses at configured memory thresholds unless explicitly overridden. |
| F19 | Circular relative-lag table for `-null timeshift` | ✅ | — | Delivered 2026-08-26. Each location computes every subject-pair neural metric once per relative lag; draws fill one reusable matrix by lookup. Corr/scorr/cosine/euclid references pass. A 256-center OMP1 benchmark (20 subjects × 60 TRs × 1,000 shifts) improved from 4.55 s to 2.10 s (2.17×) with byte-identical numeric rows. See [Audit F19](#f19-time-shift-lag-table). |

### Completed — audit resolutions

| ID | Task | Status | Next | Evidence |
|---|---|:---:|:---:|---|
| A1 | Commonality unique components lose power against a strong competing model | ✅ | — | Added partial-R² effects and predictor-specific Freedman–Lane nulls; exhaustive NumPy p/FWE references pass. See [Audit A1](#a1-commonality-unique-components-lose-power). |
| A2 | Reject invalid pattern-mode IS-RSA reliability split | ✅ | — | Pattern mode now fails explicitly because its flattened condition×voxel vector has no matched-repetition split axis. See [Audit A2](#a2-noise-ceiling-split-axis). |
| A3 | Second-order IS-RSA (`-featuretype rdm`) | ✅ | — | Ordinary and runwise/crossnobis subject condition RDMs feed the outer subject geometry with explicit inner/outer metrics, atlas/searchlight, and cross-modal inference. See [Audit A3](#a3-second-order-is-rsa). |
| A4 | Small correctness/efficiency items | ✅ | — | Deduplicated identical LOO target/estimand families, fixed contrast parsing, cached classic subject RDMs, typed extraction, and removed the second Ledoit–Wolf residual pass. The full 239-check gate remains green. See [Audit A4](#a4-smaller-audit-items). |
| A5 | Finite-input and strict-token contract | ✅ | — | Numeric options now consume the full token and reject non-finite/out-of-range values. Used model/nuisance columns, masks, and analyzed dataset bricks are checked before parallel inference; data outside the union analysis domain remain irrelevant. Similarity/crossnobis/noise kernels reject non-finite inputs and allocation failures defensively. Native-dataset, malformed-token, parser, and kernel-path regressions pass. |

For direct and adjacent implementations in rsatoolbox, PyMVPA, CoSMoMVPA,
nltools, BrainIAK, PCM, the MATLAB RSA Toolbox, and TDT, see the live
[other-package coverage matrix](#coverage-in-other-rsamvpa-packages).

### Recommended next execution plan

Revised on 2026-08-29 after completing M1e. The architecture sequence
is intentionally incremental:

1. **M1a — complete:** freeze the baseline, boundaries, and confinement.
2. **M1b — complete:** both programs now use shared BH-FDR and max
   accumulation from `thd_mapinfer`.
3. **M1c — complete:** neighborhood parsing, result painting, and SUMA surface
   construction now live in `thd_patterns`;
4. **M1d — complete:** `3dRSA` uses the arithmetic-only shared memory ledger
   while retaining its estimates, thresholds, warnings, and refusal policy;
5. **M1e — complete:** build/package/test wiring agrees and the complete
   plain/OpenMP/SUMA/legacy equivalence matrix passes;
6. next approval boundary: return to 3dMVPA Stage 0 to freeze the decoder-specific scientific and CLI
   contract before adding samples, folds, or a classifier.

Each line is a separate approval boundary. M1 does not change a scientific RSA
capability, and it does not require changes to unrelated AFNI infrastructure.
The exact confinement and preservation rules live in
[`3dMVPA_ROADMAP.md`](3dMVPA_ROADMAP.md#m1a-baseline-and-shared-core-contract).

F17, F6, and the balanced-input F4 contract are complete. The unequal-support
F4 extension remains recorded under its audit and is not promoted without a
validated covariance. A genuine dependent-cluster bootstrap would require a
separate sampling-cluster identifier and estimand; it is not implied by the
delivered `-block` strata.

### Tracked inference follow-ons

These are deliberately left open rather than being folded into a generic
"inference cleanup" item:

- **Dependent-cluster bootstrap:** a future whole-family/site resampler would
  need a sampling-cluster identifier distinct from the delivered `-block`
  exchangeability/bootstrap strata. It is not silently approximated by F17.
- **LOO training-instability interval:** F17 bounds the completed out-of-sample
  prediction vector over evaluated subjects. A nested interval that resamples
  training subjects and refits every held-subject fold remains a separate,
  substantially more expensive estimand.
- **F6 extensions:** commonality, fitted models, `-model_series`, and noise
  ceilings remain outside the delivered fixed-model two-factor interval. F22
  answers fitted-model condition generalization through nested CV rather than
  borrowing `_dualLo/_dualHi`; the other quantities still need their own
  estimands.
- **F7 fitted-model extensions:** paired held-out comparisons are now delivered
  by F14 and condition-generalizing folds by F22. Nuisance-aware nested fitting
  and bootstrap intervals for component weights remain explicit future
  estimands, not behaviors implied by `-model_fit` today.

After the dashboard and execution plan, this file has three detailed parts:

1. **[Do next](#do-next)** — the detailed implementation/audit ledger; the
   dashboard above is the current queue.
2. **[Nice to have / consider](#nice-to-have--consider)** — capabilities other
   RSA toolboxes have that 3dRSA could add, plus the ones we have deliberately
   decided to leave elsewhere.
3. **[What 3dRSA can do already](#what-3drsa-can-do-already)** — the shipped and
   validated feature set.

Effort figures are **engineering days for one developer familiar with AFNI C**,
covering implementation, help text, synthetic reference tests, and
plain/SUMA/OpenMP regression builds. They exclude scientific review,
real-dataset validation, and AFNI upstream review. Ranges are planning
estimates, not commitments. The original correctness audit was completed on
2026-07-30; roadmap status was last updated against the code on 2026-08-28.

---

## Do next

### 1. Fix the open correctness / contract bugs  ·  ✅ Done (2026-07-31)

These were cases where 3dRSA emitted subtly wrong numbers rather than merely
missing a feature, so they came first. All are now fixed, with a regression test
in `tests/run_numeric.py`.

1a. **Degenerate-regression FWE safety.** ✅ `THD_rdm_regress` now zeroes the
caller's `permnull` scratch up front, before every early return, so a constant
neural matrix (medial wall, ventricles, brain edge) contributes nothing to the
shared max-statistic null instead of folding in a previous element's values.

1b. **Distance-aware LOO.** ✅ `rdm_loo_predict` ranks the *negated* matrix when
it carries `is_dist`, so the neurally nearest subjects always get the largest
weight; `-neural_metric euclid -loo` no longer inverts (planted `looR = +0.95`).

1c. **No-permutation z output.** ✅ The second brick is FIZT-typed only when a
real test ran: `(-nperm > 0) || -mode RSA` (classic RSA at `nperm 0` falls back
to a genuine one-sample t across independent subjects). IS-RSA at `nperm 0` now
writes an untyped `_FZ` Fisher-z effect map and warns.

1d. **Block semantics in classic RSA.** ✅ `-block` under `-mode RSA` now errors
out with a clear message rather than silently doing nothing. (A real block-level
sign-flip null remains a separate, scoped feature.)

1e. **Matrix input contract.** ✅ `THD_simmat_read_1D` now rejects non-finite and
asymmetric `-model_mat` inputs at read time (symmetry within a magnitude-scaled
tolerance; AFNI already rejects literal `nan`/`inf` text upstream, so this also
covers values that overflow float32 to infinity). Mantel relabels rows AND
columns, so an asymmetric matrix would have given a wrong null. Regression-tested
in `tests/run_numeric.py` (asymmetric and overflow-to-inf fixtures rejected).

1f. **Permutation-count help mismatch.** ✅ Help now states the minimum p is
`1/N`, matching the FWE section's `1/nperm` floor.

1g. **Warning cleanup.** ✅ The original `(void)detail;` and parse-temp `rule`
warnings are fixed. Native integration later exposed additional private-module
warnings under GCC 14; completed dashboard item **B2** fixed those while leaving
shared AFNI header warnings alone.

1h. **Build portability.** ✅ Superseded by completed dashboard item **B1**. The
standalone `build.sh` described in the earlier audit is no longer present;
3dRSA is wired into AFNI's native Make, CMake, packaging, and CTest machinery.

### 2. Automated numeric regression runner  ·  ✅ Done (2026-07-31)

`tests/run_numeric.py` — **239 checks, all green on 2026-08-28** when its Python
dependencies are available. Generates
planted/null/degenerate fixtures, runs the important modes, and checks: observed
`r` against a golden value *and* an independent NumPy Mantel recomputation from
the tool's own `-save_rdm` output; null-200 FWE calibration and `p_fwe ≥ p_unc`
monotonicity; 1-vs-N-thread reproducibility of `p`/`p_fwe`; LOO sign and LOO-FWE
monotonicity; and one targeted regression per correctness fix above (1a–1e).
By default it skips (returns success) without NumPy, SciPy, or nibabel so direct
local use stays convenient. CTest passes `--require-deps`, making missing
dependencies a failure rather than a silent CI success (completed dashboard
item **2b**). It reads typed-brick state straight from the `+orig.HEAD`, so AFNI
need not be on PATH, and exits non-zero on any executed check failure. Add a
fixture for every feature below as it lands.

### 3. Quick wins  ·  ✅ Done

3a. **Reliability searchlight brick.** ✅ Done (2026-07-31). `-noise_ceiling`
now also paints its per-sphere reliability into the output dataset: a
`reliability` sub-brick for IS-RSA, `nc_low`/`nc_high` for classic RSA, after
the inferential maps (plain effect bricks, not FIZT). Under `-searchlight` this gives a
whole-brain reliability map to read beside the effect — a null effect where
reliability is high is a real miss, where it is low is just noise. Validated (3
checks in `run_numeric.py`): both modes write the maps and the painted value
equals the text-table column exactly.

3b. **Model contrasts + paired sign-flip test.** ✅ Done (2026-07-31).
`-model_label NAME` names the next model; `-model_contrast A-B` (repeatable)
tests whether model A fits better than B; `-group_test signflip|signedrank`
picks the classic-RSA paired test.

- **IS-RSA**: statistic is `r(neural,A) - r(neural,B)` per ROI, with the SAME
  subject relabeling applied to both models at every draw (`THD_mantel_contrast`)
  — they share their dyads, so relabeling independently would invent a
  between-model variance and be anticonservative.
- **classic RSA**: the within-subject paired difference of the two Fisher-z
  fits, tested across subjects by sign flip (default) or Wilcoxon signed-rank
  (`THD_signrank_signflip`, outlier-robust) — the same `PERM_ISE` engine.
- Each contrast is its own max-statistic FWE family on the shared relabelings.
  Output: `A-B_diff/_p/_q` (+ `_pfwe`) table columns and `A-B_diff`,`A-B_Zdiff`
  (+ `A-B_ZdiffFWE`) sub-bricks in stable appended slots so primary slots are undisturbed.
  IS-RSA accepts any fixed/per-location pairing: fixed vs fixed, fixed vs
  `-model_dset`, or two `-model_dset` modalities rebuilt in the same location.
- **Validated** (9 checks in `run_numeric.py`, all green): IS-RSA `diff` equals
  `r_A - r_B` exactly; sign reverses on label swap; the true model wins on the
  planted ROI (`diff>0`, `p<.05`); identical models give exactly `diff=0, p=1`;
  contrast FWE is monotone (`_pfwe ≥ _p`); diff/p/pfwe are 1-vs-N-thread
  identical; both classic group tests run; plain/OMP/SUMA builds clean.
  *Follow-on:* model-vs-model **fitted** comparisons must feed the contrast only
  held-out fold scores (needs the run/fold metadata from item 4).

3c. **Model commonality (variance partitioning).** ✅ Done (completed
2026-08-25).
`-model_commonality A,B` (repeatable) is the complement to the contrast: instead
of "which model wins", it splits the variance the two models jointly explain in
the neural RDM into **unique-A, unique-B, and common** —
`uniq_A = R²_AB − R²_B`, `uniq_B = R²_AB − R²_A`, `common = R²_A + R²_B − R²_AB`
(they sum to the joint R²). The natural readout for EEG/fMRI-style fusion, where
two brain-derived RDMs may each add something.

- `THD_commonality` (thd_simmatrix): the whole 2-predictor decomposition is done
  in **double precision** via the closed-form OLS — `common` is a difference of
  near-equal quantities that loses all its digits in single precision (an earlier
  float version gave a garbage `common` p; the double version matches numpy
  exactly). **`common` can be negative (suppression) and is reported unclipped.**
- The raw three-way decomposition is retained and `partialR2_A/B` are appended
  as interpretable conditional effect sizes. Unique-A/partial-A and
  unique-B/partial-B use their own Freedman–Lane reduced-residual nulls;
  `common` retains the complete neural-item relabeling null. Each quantity has
  its own p/FDR/max-FWE family. Fixed and per-location `-model_dset` predictors
  work in atlas and searchlight analyses, and commonality composes with
  `-model_contrast`.
- **Classic RSA is now supported without sign-flipping squared effects.** Each
  subject is decomposed separately and the reported effect is the mean subject
  component. Unique/partial A and B use subject-specific reduced fits with one
  synchronized condition relabeling applied to every subject; `common` uses a
  complete neural-condition relabeling. This condition-null family is separate
  from the primary model's subject sign flips and works for ordinary and
  runwise/crossnobis RSA in atlas ROIs and searchlights.
- Output: `uniq_A/uniq_B/common_A_B/partialR2_A/partialR2_B` columns, each with
  `_p/_q` and optional `_pfwe`, plus value/signed-z/FWE map bricks and optional
  synchronized subject-bootstrap bounds.
- **Validated:** independent NumPy decomposition, partial-R², exhaustive
  Freedman–Lane p/FWE, identity/composition, subject-bootstrap,
  atlas/searchlight map, label, metric-contract, and thread-reproducibility
  checks are part of the current **239-check** required-dependency suite.
- **Target/predictor contract:** `InputFile` always supplies the target neural
  geometry. `-model`, `-model_mat`, and `-model_dset` supply the two predictors;
  commonality cannot be computed from two model datasets without a separate
  target. For example, to partition fMRI geometry into unique EEG, unique
  behavior, and shared variance:

  ```text
  Subj  InputFile              EEGFile        MADRS  ...
  s01   s01_fmri_betas+tlrc    s01_eeg+tlrc   22
  s02   s02_fmri_betas+tlrc    s02_eeg+tlrc   14
  ...
  ```

  ```text
  -model_label eeg   -model_dset EEGFile
  -model_label behav -model MADRS:nn
  -model_commonality eeg,behav
  ```
- **Three-predictor scope is now delivered as F8.** A request of the form
  `-model_commonality A,B,C` adds the seven exhaustive raw regions and three
  conditional partial-R² effects while retaining this pairwise interface and
  output order. See [Audit F8](#f8-three-predictor-commonality).

> Update (2026-08-25): pairwise commonality now includes conditional
> Freedman–Lane inference for both IS-RSA and classic RSA, partial-R² reporting,
> and subject-bootstrap bounds.
> The current priority queue is the execution plan at the top of this document.

### 4. Runwise classic RSA, crossnobis, and noise whitening  ·  ✅ Done (ROI and searchlight)

This closed what had been the largest conventional fMRI-RSA gap and materially
changed 3dRSA's standing next to rsatoolbox/PyMVPA/CoSMoMVPA. The ordinary
same-data classic-RSA path still has the familiar positive distance bias;
crossnobis instead takes the cross-product of condition contrasts from
*independent* runs and has an interpretable zero point (see the
[representational-model framework](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005508)).

Stage it so each step is independently useful and testable:

**4a. Runwise input contract.** ✅ Done (2026-07-31). `-runwiseTable FILE`
(with `-mode RSA`), one row per subject × run:

```text
Subj  Run  InputFile          ResidFile
s01   1    s01_r1_betas+tlrc  s01_r1_errts+tlrc
s01   2    s01_r2_betas+tlrc  s01_r2_errts+tlrc
```

- `THD_runset` container + `THD_runset_read`/`_free`/`_print` in `thd_patterns`
  (reuses the `-dataTable` parser, then groups rows by subject and opens dataset
  headers). Carries subject / run / condition / voxel / residual metadata.
- Validated at read time: ≥2 runs per subject, matching condition count/order
  by default, a common grid, unique run labels within a subject, ResidFile grid
  match, and a residual-dof sanity check. `ResidFile` is optional (present →
  noise-normalized crossnobis is available; absent → unwhitened crossnobis).
  F21's optional `ConditionFile` mapping now replaces only the balanced
  count/order requirement. The runwise path remains mutually exclusive with
  `-dataTable` and grid-checked against `-mask`; the existing `-dataTable` path
  is untouched.
- The parser **loads, validates, and summarizes** the runwise input, then hands
  the validated runset to the completed 4b/4c estimators. **Validated** (6
  original contract checks in `run_numeric.py`): a valid table loads +
  summarizes; malformed tables are rejected with specific messages (<2 runs,
  duplicate run label, condition/grid mismatch, missing `-mode RSA`, mask-grid
  mismatch); `ResidFile` optional. Plain/OMP/SUMA builds clean.

**4b. Cross-validated squared Euclidean distance.** ✅ Done (2026-07-31). For a
condition pair `(i,j)` and run contrast `δ_ij,r = β_i,r − β_j,r`,
`d(i,j) = mean over ordered r≠s of ( δ_ij,r · δ_ij,s ) / P` — each product taken
from two DIFFERENT runs, so the noise averages out and the estimate is unbiased
(true value 0 when the conditions don't differ).

- `THD_simmat_crossnobis` (thd_simmatrix; `thd_permute` stays dataset-agnostic)
  computes it via `|Σ_r δ_r|² − Σ_r|δ_r|²` per voxel, no per-run delta vector
  materialized. Returns an `is_dist=1` RDM; **negative distances are kept
  unclipped** (clamping would restore the bias). A small helper in 3dRSA builds
  each subject's neural RDM from either the runset (crossnobis) or the ordinary
  pattern path, so **the entire downstream — model correlation, sign-flip,
  `-model_contrast`, FDR, max-stat FWE, table + bricks — is reused unchanged.**
- Integrated behind `-runwiseTable` + `-mode RSA` + `-model_mat`. All beta
  datasets are loaded up front; residuals stay unloaded until 4c. F16 now adds
  the Nili subject-LOO lower and inclusive upper noise-ceiling readouts to these
  crossnobis RDMs. The separate `-loo` option remains an IS-RSA scalar-behavior
  predictor and has no target variable under the classic-RSA runwise contract.
- **Validated** (7 checks in `run_numeric.py`, all green): on a planted group
  structure the crossnobis effect equals an independent numpy computation of the
  WHOLE pipeline (crossnobis → Spearman → Fisher-z mean) to 6 decimals
  (`0.694365`); the effect is significant; negative distances are produced
  (unbiasedness); and the result is 1-vs-N-thread reproducible. A two-model
  runwise contrast matches an independent within-subject Fisher-z-difference
  calculation, has valid monotone FWE, and is thread-reproducible.

**4c. Residual noise covariance and whitening.** ✅ Done (2026-08-01).
`-noise_norm none|diag|shrinkage` whitens the betas by the residual noise
covariance before the crossnobis dot products (a Mahalanobis distance,
`W = Σ⁻¹`). The covariance is estimated per subject per ROI from that subject's
residual time series — each run demeaned first — **never** from the betas being
compared.

- `THD_noise_wdiag` (thd_simmatrix): univariate — `1/sqrt(var_v)` per voxel, with
  variances floored to the median positive variance so a silent voxel isn't
  amplified.
- `THD_noise_whalf`: multivariate — `Σ = R'R/n`, **Ledoit-Wolf shrinkage toward
  `(trace/p)·I`**, `symeig_double`, small-eigenvalue floor, `Σ^{-1/2} =
  V diag(1/√λ) V'` (the same shrink+eigen-floor primitive proven in item 5). Stays
  invertible when voxels approach/exceed residual time points; warns when they
  exceed it.
- Wired via `-noise_norm` (requires `-runwiseTable` + `ResidFile`); residuals are
  loaded and whitening applied inside `rsa_subject_rdm`, so all of 4b's downstream
  (correlation, sign-flip, contrasts, FWE, output) is reused untouched.
- **Validated** (6 checks in `run_numeric.py`, all green): on spatially-correlated,
  heterogeneous-variance noise, **both diag and shrinkage match an independent
  numpy computation of the whole whitened pipeline** exactly (`diag 0.652745`,
  `shrinkage` differs from plain — multivariate whitening is doing work), and
  the result is run-label-swap invariant and thread-reproducible. Explicit tests
  verify that a requested whitening mode rejects a runwise table without
  `ResidFile`, and that exact identity residual covariance makes both whitening
  modes reduce to the plain crossnobis result.

**Item 4 is complete in ROI and volumetric-searchlight modes** — item 6 now
carries the same cross-validated and noise-normalized estimators through the
streaming searchlight path.

### F21. Unbalanced runwise/crossnobis input  ·  ✅ Done (2026-08-27)

F21 removes the remaining balanced-design restriction from `-runwiseTable`
without changing existing inputs. A table may add a `ConditionFile` column:

```text
Subj  Run  ConditionFile       InputFile             ResidFile
s01   1    s01_r1_conditions   s01_r1_betas+tlrc     s01_r1_errts+tlrc
s01   2    s01_r2_conditions   s01_r2_betas+tlrc     s01_r2_errts+tlrc
```

Each condition file contains one whitespace-free label per `InputFile`
sub-brick; blank lines and comments are ignored. Labels may be reordered,
absent, or repeated across a run. 3dRSA forms a stable lexical global condition
order, averages repeated local bricks into one run-level estimate, and records
that order in the result-table provenance. The supplied fixed model matrices
therefore use the printed lexical order. Omitting `ConditionFile` selects the
original balanced path, which remains byte-for-byte identical on the regression
fixture.

For condition pair `(i,j)`, only runs containing both members contribute. If
there are `R_ij` such runs, `THD_simmat_crossnobis_valid` uses the original fast
`|Σδ|² − Σ|δ|²` identity with denominator
`R_ij(R_ij−1)P`. Every pair must co-occur in at least two independent runs for
every subject; otherwise the loader fails with the subject and condition labels
rather than emitting a partial RDM. This keeps all downstream code on complete
condition RDMs while permitting the missing trials and unequal repetitions that
occur in real task-fMRI designs.

The mapping precedes residual whitening and feeds the same subject-RDM helper
used by classic RSA and second-order IS-RSA. It is therefore available in atlas
ROIs and volumetric crossnobis searchlights, with `none`, `diag`, or `shrinkage`
noise normalization and the existing contrast/bootstrap/commonality/ceiling
machinery. Searchlight preflight includes the largest local-brick remapping
buffer.

Seven F21 checks cover an independent NumPy pair-valid estimator with missing,
reordered, and repeated conditions; exact balanced-path parity; short mapping
and insufficient-pair rejection; second-order IS-RSA; atlas/searchlight and
thread identity; and composition with independently reproduced residual
whitening. The complete required-dependency gate passes **239/239**.

### S6. Trial-beta descriptors  ·  ✅ Done (2026-08-29)

S6 adds explicit trial identity without creating a second runwise estimator. A
`-runwiseTable` row can use `TrialFile` in place of `ConditionFile`; the local
file has exactly `Trial Condition` columns and one row per `InputFile`
sub-brick. `Subj` and `Run` remain authoritative in the outer table. Trial IDs
must be unique within subject, and condition labels are placed in the same
stable lexical global order used by F21.

The trial betas for each condition are averaged within run before residual
whitening and crossnobis. Every downstream consumer therefore receives the
same complete subject condition RDM contract: classic or second-order RSA,
atlas/searchlight execution, seed connectivity, contrasts, resampling, ceilings,
and synchronized spatial inference continue through the shared `THD_runset`
path. The descriptor does not redefine the inferential unit: output remains a
condition RDM, and there is no implicit trial bootstrap or trial×trial RDM.

Independent shuffled/repeated-trial aggregation and crossnobis references,
second-order geometry, atlas/searchlight equivalence, thread identity, strict
trial uniqueness/count/mutual-exclusion failures, and the explicit F4 covariance
rejection pass in the complete **275/275** numerical gate. First-level beta
estimation remains with `3dDeconvolve`, `3dREMLfit`, or `3dLSS`.

### 5. Mahalanobis behavioral profiles  ·  ✅ Done (2026-07-31)

`-model A,B,C:mahal` (alongside the existing `:euclid`) whitens the behavioral
profile by the measures' covariance, so correlated measures no longer
double-count: `d(i,j)^2 = (z_i - z_j)' R_reg^-1 (z_i - z_j)`, with `R` the
correlation matrix of the z-scored columns.

- `THD_simmat_from_profile_mahal` (thd_simmatrix): z-score (reject constant /
  non-finite columns), correlation matrix, **Ledoit-Wolf shrinkage toward the
  identity** (adaptive — shrinks more as measures approach the subject count),
  symmetric eigendecomposition (`symeig_double`), small-eigenvalue floor, and the
  regularized inverse. Reduces to `:euclid` when the measures are uncorrelated.
- 3dRSA prints a diagnostic per profile: number of measures, effective rank, and
  the shrinkage intensity, with a collinearity note when the effective rank is
  below the measure count. The regularized inverse is the safe default; no silent
  pseudoinverse.
- **Validated** (4 checks in `run_numeric.py`, all green): the model RDM matches
  an independent numpy replica of the whole pipeline to float precision
  (`7.6e-07`); exactly-orthogonal measures reduce to standardized Euclidean
  (`1e-6`); column-order invariant (`0`); a constant measure is rejected with a
  clear message. Duplicate/near-collinear columns are regularized (shrinkage
  lifts the degenerate direction) rather than blowing up the inverse.
- **Reuse:** the shrinkage + eigenvalue-flooring + regularized-inverse code is
  the same primitive item **4c** (residual noise whitening for crossnobis) needs
  — now built and tested in this smaller setting first, as planned.
  The implemented regularization handles correlated duplicates and is designed
  to be affine-scale invariant; explicit regressions for those two properties
  remain useful additions beyond the four checks above.

### 6. Classic-RSA searchlights  ·  ✅ Done (runwise 2026-08-22; ordinary 2026-08-25)

`-runwiseTable -mode RSA -searchlight NNN` now builds each subject's crossnobis
condition RDM inside every moving volumetric neighborhood, then reuses the
existing classic-RSA subject test, model contrast, FDR, synchronized max-stat
FWE, table, and center-voxel painting paths.

- All three estimators work: unwhitened cross-validated squared Euclidean and
  `-noise_norm diag|shrinkage`. Beta datasets (and residual datasets when
  whitening) remain loaded while neighborhoods stream through the shared
  `rsa_subject_rdm` path; per-thread scratch is sized to the largest sphere.
- **Validated** (5 checks in `run_numeric.py`): a radius covering the synthetic
  volume makes every searchlight location equal the independent NumPy pipeline
  for `none`, `diag`, and `shrinkage`; brick labels are
  `block_r/block_Z/block_ZFWE`; every FWE p is valid and monotone relative to the
  uncorrected p; and complete maps plus p/FWE tables are exactly reproducible at
  one versus multiple threads.

`-dataTable -mode RSA -searchlight NNN` now also maps the ordinary estimator
used by atlas/ROI classic RSA. Each subject's condition RDM is rebuilt from the
patterns in each moving volume or surface neighborhood, then enters the same
subject-level inference, contrast, bootstrap, ceiling, and output machinery.
This opens searchlight RSA for designs that do not repeat every condition in
independent runs—for example, studies with unique stimuli.

The distinction is intentionally visible rather than hidden. Ordinary
searchlights emit a runtime warning that the same condition estimates define
and are evaluated through the RDM, and every text result records
`estimator: same-data condition-pattern RDM`. Runwise outputs instead identify
their crossnobis/noise-normalization estimator. Crossnobis remains preferred
when independent repeated condition estimates exist because it has an unbiased
zero point; the ordinary estimator is supported because it is a legitimate and
widely used target-RDM workflow, not presented as cross-validated.

- **Validated** (5 added checks): a compact ordinary searchlight matches an
  independent NumPy condition-correlation → Spearman → subject Fisher-z
  calculation at every center; a one-label whole-volume atlas gives identical
  effect, p, q, and max-FWE p; FWE is valid and monotone; the warning and table
  estimator tag are present; and one-versus-many-thread tables are exact.

### 7. `-model_dset` under searchlight  ·  ✅ Done (2026-08-22)

The searchlight now accepts `-model_dset`: the second modality is streamed
through the same moving neighborhood and its cross-modal model rebuilt in every
sphere, giving a whole-brain map of where two modalities share a subject
geometry. Both feature modes work — the ROI-mean time course (`MODE_CONT`,
reduced per sphere on the fly) and the voxel pattern (`MODE_BETA`, extracted per
sphere). The second modality's datasets are held resident alongside the neural
ones; the searchlight help notes the doubled memory. `-save_rdm` stays disallowed
under searchlight (one matrix per voxel).

- **Validated** (4 checks in `run_numeric.py`): with a radius covering the whole
  volume, every searchlight voxel equals the atlas single-ROI cross-modal result
  **exactly** (both `mean` and `pattern` features) — the clean atlas↔searchlight
  consistency check. A non-quiet two-`model_dset` searchlight also verifies that
  the model-collinearity sampler reduces its sampled spheres on the fly rather
  than touching atlas-only storage, and a save-RDM regression verifies that
  plotting hints never advertise an unwritten fixed model file. Explicit
  thread/brick-label checks remain pending.

### 7b. Mask-optional surface searchlight  ·  ✅ Done (2026-08-02)

A surface `-searchlight` may now omit `-mask` to search the WHOLE mesh. Prompted
by the observation that a surface, unlike a volume, has no "not brain" region --
every node is cortex -- so "search everywhere" has a coherent meaning a
volumetric searchlight does not have (which still requires `-mask`).

- `-mask` stays required everywhere else: atlas/mask mode (volume or surface),
  volumetric searchlight, and surface mask/atlas mode without `-searchlight`.
  Only the surface-searchlight combination (`-surf` + `-searchlight`, no
  `-mask`) is exempted.
- When omitted, the geometry/domain is taken from the first `InputFile` in the
  data table (used only for its mesh domain, not its data values) and every mesh
  node the geometry dataset covers is searched.
- **Validated** (3 checks in `run_numeric.py`, SUMA-build only -- SKIPPED, not
  failed, on a plain build): a synthetic flat GIFTI mesh confirms the run
  succeeds and covers every node, and critically, **omitting `-mask` gives an
  EXACT, byte-identical result to an explicit all-nodes-in mask** -- the two are
  the same computation by construction, and the test proves it. A smaller mask
  is expected to tighten edge neighborhoods under the existing semantics, but
  that comparison is not currently asserted by the runner.
- A standalone fixture generator, `tests/mksurf.py`, is also available for
  manual surface testing/demos (a flat triangulated GIFTI grid with a planted
  IS-RSA effect in its center).

### F9. Fixed-model Mantel cache  ·  ✅ Done (2026-08-22)

Pearson/Spearman subject-label searchlights now materialize the model side of
the Mantel null once: each fixed model is relabeled, reduced to its strict upper
triangle, and centered (Pearson) or ranked-and-centered (Spearman) for every
member of the shared `PERM_set`. The immutable result is reused across all
centers and OpenMP workers by both primary model tests and fixed-model paired
contrasts. The changing neural triangle is prepared once per center.

- Observed statistics still use the established `THD_tri_corr` path. Kendall,
  time-shift, joint/nuisance regression, commonality/LOO, and per-location
  `-model_dset` work retain their prior exact implementations.
- Mixed fixed plus dataset-model analyses cache only the fixed matrices.
- F11's preflight includes the full `fixed models × relabelings × triangle`
  allocation before loading searchlight inputs.
- Pearson and Spearman cached searchlights match the uncached atlas path for
  effect, p, max-FWE p, and paired contrasts; cached output is identical at one
  and six threads. The required numeric suite is **239/239**.
- Local OMP1 benchmark (20 subjects, 190 dyads, 256 centers, two fixed models,
  one contrast, 500 relabelings): **15.05 s → 0.51 s (29.5× wall-clock)**, with
  byte-identical `.rsa.1D` output.

### F13. Per-location multimodal contrasts  ·  ✅ Done (2026-08-22)

An IS-RSA `-model_contrast A-B` may now name a fixed model, a per-location
`-model_dset`, or one of each. Each dataset model is independently reduced and
rebuilt inside the current atlas ROI or searchlight sphere before the existing
paired Mantel contrast is evaluated. Every null draw applies the same subject
relabeling to A and B, preserving their shared dyads; each contrast retains its
own FDR and synchronized max-statistic FWE family.

- Dataset-vs-dataset contrasts enable a direct spatially varying multimodal
  question such as `EEG-fMRI`; mixed contrasts can compare that local modality
  against a fixed behavioral/theoretical RDM.
- F9 caches any fixed model's primary test, while a mixed contrast correctly
  uses the ordinary paired path because its per-location side cannot be cached.
- Classic RSA remains fixed-condition-model only. F18 now reuses each rebuilt
  per-location model under a complete neural-series circular-shift null, so
  fixed, mixed, and dataset-vs-dataset IS-RSA contrasts are supported there too.
- Validation exhaustively enumerates a 64-member blocked relabeling group and
  independently reproduces the paired contrast p. Atlas and whole-volume
  searchlights agree for effect/p/max-FWE, map labels are present, and one- vs
  six-thread output is identical. Mixed fixed/dataset execution is also covered.

### F6. Dual subject × condition bootstrap  ·  ✅ Done (2026-08-27)

Giving equal positive counts to `-bootstrap N` and `-cond_bootstrap N` now asks
one question: how uncertain is a fixed-model classic-RSA effect when the target
population includes both new subjects and new conditions? It does not emit two
separate intervals and it does not mistake the simultaneous bootstrap variance
for the answer. For each effect, 3dRSA computes subject-only variance `Vs`,
condition-only variance `Vc`, and simultaneous variance `Vsc` on the estimator's
working scale, then applies the finite-sample two-factor correction documented
by rsatoolbox:

`V = S/(S-1) Vs + C/(C-1) Vc - SC/((S-1)(C-1)) (Vsc - Vs - Vc)`

The result is bounded below by each corrected one-axis variance and above by
`Vsc`. A two-sided t interval uses `df = min(S,C)-1`, where `C` is the number of
independently sampled condition groups, not the expanded number of condition
rows. Plain model effects and paired fixed-model contrasts stay in Fisher-z
space through variance estimation and map the endpoints back with `tanh`;
`-model_joint` coefficients remain on their reported standardized-beta scale.

- Equal draw counts are required so draw `b` pairs one subject sample with one
  condition sample. Existing one-axis calls retain their percentile intervals.
- Every condition draw is synchronized across all subjects and model RDMs;
  duplicate-original-condition diagonal artifacts remain omitted. Subject-only
  variance uses all subject draws, while condition and simultaneous variances
  use the condition draws retaining at least three distinct original items.
- `_dualLo/_dualHi` table columns and plain-float AFNI bricks are emitted for
  fixed primary models and paired fixed-model contrasts in ordinary or
  runwise/crossnobis classic RSA, with atlas and volumetric searchlight support.
- Commonality, fitted models, `-model_series`, and noise ceilings are explicitly
  rejected in this first contract. F22 now answers the materially different
  condition-held-out fitted-model question with nested cross-validation; the
  other extensions remain recorded follow-ons rather than silently borrowing a
  fixed-model interval.
- Independent SciPy/NumPy tests reproduce the corrected formula for crossnobis,
  variable-sized condition groups, joint regression, and paired contrasts;
  contract and atlas/searchlight/one-vs-four-thread tests also pass.

Reference implementation and rationale: [rsatoolbox dual-bootstrap
inference](https://rsatoolbox.readthedocs.io/en/stable/inference.html) and its
[`_dual_bootstrap` source](https://github.com/rsagroup/rsatoolbox/blob/main/src/rsatoolbox/util/inference_util.py).

### F17. Subject-bootstrap extensions  ·  ✅ Complete (2026-08-29)

`-bootstrap` now reports percentile intervals for every `-model_contrast`, in
addition to its existing primary-model bounds. The interval follows the same
paired estimand reported by each analysis rather than bootstrapping the two
models independently:

- IS-RSA applies each subject draw jointly to the neural RDM and both model
  RDMs and bounds `r(neural,A) - r(neural,B)`. Pairs between two sampled copies
  of the same original subject are omitted as artificial diagonal dyads.
- Classic RSA resamples the paired within-subject Fisher-z differences and
  bounds `tanh(mean(zA-zB))`, including runwise crossnobis inputs.
- Fixed, mixed, and two-`-model_dset` IS-RSA contrasts work in atlas ROIs and
  searchlights. Text tables and datasets expose `<A-B>_bootLo/_bootHi`; F11's
  preflight includes the added arrays and output bricks.
- Independent NumPy/SciPy references cover fixed IS-RSA, per-location IS-RSA,
  and classic crossnobis estimands. Atlas/searchlight equivalence, labels, and
  exact one-vs-many-thread output are also checked.

The second delivered slice adds IS-RSA regression coefficients:

- `-model_joint` refits all reported and nuisance columns on every compact
  missing-dyad subject draw; `-ortvec` without joint fits each reported model
  separately with every nuisance column still projected out.
- Each `-ortvec` retains its documented `|x_i-x_j|` and `x_i+x_j` pair-space
  columns. All response, model, and nuisance values are subset together before
  the selected Pearson/Spearman standardization and least-squares refit.
- `_bootLo/_bootHi` bounds the reported standardized coefficient (`_b`), not
  the partial correlation (`_pr`). Fixed and per-location `-model_dset`
  joint+nuisance fits work in atlas ROIs and searchlights, and F11 includes the
  compact designs plus coefficient draws in its per-thread peak estimate.
- Independent NumPy ranked-regression references cover joint+nuisance and
  separate+nuisance fits; per-location atlas/map and thread invariants pass.

The third delivered slice adds pairwise-commonality intervals, extended by the
completed A1 work on 2026-08-25:

- Every subject draw packs the neural, model-A, and model-B triangles together,
  omits duplicate-copy diagonal dyads, and recomputes unique-A, unique-B, common
  variance, and both partial-R² effects from one fit. The five quantities are
  never formed by combining independently bootstrapped fits.
- Tables and datasets append `<component>_bootLo/_bootHi` for all five
  quantities. Fixed, mixed, and two-per-location-model requests use the same
  atlas/searchlight path, and F11 counts the component results and compact
  per-thread scratch.
- Independent NumPy decomposition references pass for fixed and two-`model_dset`
  requests; atlas/searchlight values and map labels agree, and one-vs-many-thread
  bounds are identical.
- The raw bounds still describe `uniq_A = R²_AB - R²_B` (and analogously for B);
  the appended `partialR2_A/B` bounds describe the fraction of reduced-model
  residual variance explained by the added model. A1's delivered reduced-model
  Freedman–Lane tests remain separate from these sampling-uncertainty intervals.

The fourth slice defines `-block` as a stratified subject-bootstrap contract:

- Each bootstrap destination samples with replacement only from subjects with
  the same block label, preserving every block's observed sample size.
- The same immutable stratified draw set is shared across models and locations,
  so existing primary, contrast, regression, commonality, and spatial/thread
  guarantees continue to apply.
- This assumes subjects are independent within strata. It is explicitly not a
  whole-family or whole-site cluster bootstrap; dependent clusters would need a
  distinct sampling-cluster identifier.

The final slice supplies LOO uncertainty without changing the prediction
estimand:

- The program retains the one genuinely held-out prediction for every subject,
  then resamples completed `(prediction, observed target)` rows and recomputes
  `looR`. Duplicate rows are ordinary bootstrap multiplicity rather than neural
  dyads, and draws with fewer than three distinct evaluated subjects are omitted.
- Scalar NN/euclid/absdiff and AnnaK targets resample one prediction/target pair;
  multivariate profiles resample complete rows synchronously and retain the
  equal-weight mean of measure-wise correlations.
- Tables and plain-float maps add `<model>_looBootLo/_looBootHi`. Exact duplicate
  target/estimand families copy the same interval just as they share LOO work.
- Help and table provenance state that predictions are held fixed: the interval
  measures uncertainty over evaluated subjects, not training-set instability
  from refitting every fold inside a nested bootstrap.

Independent references reproduce singleton and paired-stratum draws plus
scalar-NN, AnnaK, and multivariate-profile percentile intervals. Searchlight
labels and exact one-vs-four-thread bounds pass as part of the complete
**276/276** required-dependency gate.

### 8. Recommended sequence

The status dashboard's priority column and the execution plan at the top of this
document are canonical. F11/F9 completed the searchlight-usability bundle, F13
completed spatially varying paired comparisons, F6 now supplies fixed-model
two-axis generalization intervals, and completed F17 bounds contrasts,
joint/nuisance-adjusted coefficients, commonality components, and fixed-OOF LOO
prediction accuracy, including stratified subject resampling. F21 removed the broadest runwise-input
eligibility restriction, F22 closed the fitted-model condition-generalization
gap, and F4 now supplies covariance-aware comparison for the scientifically
clean balanced-crossnobis contract. F23 and F5b then closed the final committed
metric and temporal-null extensions. No implementation item is now in Ready.

---

## Nice to have / consider

Capabilities other RSA toolboxes (rsatoolbox, PyMVPA, CoSMoMVPA, BrainIAK,
nltools) offer that 3dRSA could add later, followed by the things we have
decided to keep *out* of core 3dRSA and why.

### Worth adding, after the "Do next" list

These rows group related capabilities for comparison; they are **not** a second
priority order. The dashboard and execution plan above are canonical.

| Capability | What it adds | Priority | Effort | Notes |
|---|---|:---:|---:|---|
| **Subject bootstrap + confidence intervals** | ✅ Delivered for primary, joint/nuisance-adjusted, paired model-contrast, commonality, and fixed-OOF LOO effects: sampling-uncertainty intervals complement the null tests for classic and IS-RSA effects. | P2 | Done | `-bootstrap N [-boot_ci P]`; subject rows are resampled with replacement. With `-block`, sampling stays within strata and preserves their sizes; this is not whole-cluster resampling. IS-RSA omits repeated-subject neural diagonal artifacts, refits compact regression/commonality designs when needed, and keeps paired quantities synchronized. LOO bounds instead resample completed prediction/target rows and explicitly exclude fold-refitting uncertainty. Bootstrap indices live in their own `THD_resample_set`, separate from `PERM_set`. |
| **Stimulus / condition bootstrap** | ✅ Delivered: lets classic-RSA inference generalize beyond the exact conditions tested. | P2 | Done | `-cond_bootstrap N [-boot_ci P]` resamples condition indices jointly across each subject's neural RDM and every model RDM. `-cond_group FILE` keeps variable-sized related condition sets together; duplicate-condition diagonal artifacts are omitted. |
| **Circular-shift null for continuous IS-RSA** | ✅ Delivered for primary, paired-contrast, joint, and nuisance-adjusted effects: destroys shared-timeline alignment while preserving each subject's complete series. | P2 | Done | `-null timeshift [-min_shift K]`; equal-length, gap-free `-featuretype mean` series only. One identity-plus-random-offset set is shared across every ROI/searchlight for synchronized max-FWE and thread reproducibility. F18 added fixed/per-location contrasts and conditional regression; commonality, LOO, and segmented inputs still need distinct contracts. |
| **Whitened unbiased RDM comparison** | ✅ Delivered for balanced classic-RSA crossnobis: weights model-vs-neural comparison by the zero-distance covariance induced by shared conditions. | P3 | Done (balanced) | `-metric corr_cov|cosine_cov` uses `V=(C C') o (C C')`, matching rsatoolbox's exchangeable-condition default and [Diedrichsen et al.](https://arxiv.org/abs/2007.02789). Direct dense-`V^-1` fixtures verify the optimized second-moment implementation. Unequal F21 support and condition resampling remain a future covariance derivation, not an implicit fallback. |
| **Phase-randomization null** | ✅ Delivered in ROIs and searchlights: stronger spectral preservation than a single circular shift while removing phase-locked shared timing. | P2 | Done | `-null phase` preserves each subject's DC, real Nyquist bin, and Fourier magnitudes; independently randomizes the remaining phases; and supports primary, contrast, joint, nuisance-adjusted, FDR, and synchronized spatial max-FWE inference. F5b reuses one local spectrum per subject × center across all draws. Reference: BrainIAK `phaseshift_isc` / `phase_randomize`. |
| **Dual subject × condition bootstrap** | ✅ Delivered for fixed primary models, joint regression, and paired fixed-model contrasts: generalization over both people and stimuli at once. | P3 | Done | Equal `-bootstrap N` and `-cond_bootstrap N` use separate and simultaneous draws to form the finite-sample corrected variance, then write `_dualLo/_dualHi` t intervals with `df=min(subjects, condition groups)-1`. Ordinary/runwise atlas and searchlight paths, variable-sized groups, joint coefficients, and contrasts have independent references. Commonality, fitted models, model series, and ceilings remain explicit extension contracts. |
| **Second-order IS-RSA (`-featuretype rdm`)** | ✅ Delivered: compares subjects by their condition RDMs rather than voxel-by-voxel patterns, removing the cross-subject anatomical-correspondence assumption that `-featuretype pattern` makes. | P1 | Done | Roadmap **A3**. `-condition_metric` defines ordinary inner RDMs; `-runwiseTable` supplies crossnobis inner RDMs; `-neural_metric` compares their dissimilarity triangles. Fixed behavioral matrices and per-location `-model_dset` modalities use the existing inference stack. |
| **Time-resolved M/EEG–fMRI fusion (`-model_series`)** | ✅ Delivered: a time × space fusion map from an ordered per-timepoint M/EEG RDM list, with BH FDR and one max-statistic null over the joint time × space family. | P1 | Done | Roadmap **F20**. Supports classic and second-order IS-RSA, ordinary/runwise neural inputs, atlas/searchlight mapping, subject/condition bootstrap where already applicable, long-form labeled tables, and deterministic AFNI timepoint bricks. |
| **Native temporal RDM movies (`1dTrdm`)** | ✅ Delivered producer, temporal-inference, cross-temporal-estimation, and feature-neighborhood gates: labeled observation×feature×time inputs produce subject RDM movies, corrected fixed-model inference, representational-recurrence surfaces, cross-time crossnobis, explicit overlapping feature searches, and a guarded independent-sample `-model_series` bridge. | P1/P2 | Done (four release gates) | Correlation, cosine, Euclidean, and balanced crossnobis; mean/concatenated windows; population sign-flip or fixed-condition null; joint time or time×neighborhood BH/max-FWE; separate symmetric Pearson/Spearman RDM-dynamics and ordered-partition cross-time distance products within all features or each graph-defined neighborhood; explicit axes/counts/provenance; row/graph/seed/thread/layout identity; live 3dRSA round trip. |
| **Circular-shift lag table** | ✅ Delivered: makes the supported `-null timeshift` searchlight substantially cheaper by replacing full shifted-series matrix rebuilds with relative-lag lookups. | P1 | Done | Roadmap **F19**. Supports every `-neural_metric`, is included in F11 memory preflight, records its model-side contract, and produced a measured 2.17× OMP1 searchlight speedup on the retained benchmark. |
| **Constrained fitted / weighted-component model and paired held-out comparison** | ✅ Delivered: nonnegative ridge mixtures of named component RDMs with held-subject scoring, plus direct paired comparisons of two fitted models. | P3 | Done | `-model_fit NAME=A,B,... [-fit_ridge R]`; `-model_contrast FITA-FITB`. Classic RSA fits on other subjects' condition dyads; IS-RSA excludes every dyad touching the held subject. Every label draw refits both sides under the same relabeling. Outputs include individual CV effect/p/q/max-FWE, descriptive fold-mean weights, and paired Fisher-z accuracy-difference p/q/max-FWE. Not a general model-object framework; nuisance-aware fitting and weight bootstraps remain separate estimands. |

Supporting the above: the **internal dataset/RDM metadata layer** (keeps
subject/run/condition/feature/orientation labels attached so later stages cannot
mix axes) is folded into the runwise-input estimate (4a); the **bundled plotting
consumer** (convert `3dRSA_plots.py` to an AFNI-only `1dplot.py` orchestrator,
2–4 days) still waits on AFNI PR [#919](https://github.com/afni/afni/pull/919):
it remains open as of 2026-08-25 (last updated 2026-07-27), and the required
plot modes are absent locally.

### Three-way commonality  ·  ✅ Delivered as F8 (2026-08-27)

`-model_commonality A,B,C` partitions a target's variance among three
predictors—for example, unique EEG, unique fMRI, unique behavior, each
pairwise-shared region excluding the third predictor, and the three-way shared
region. The seven raw components obey the `2^3-1 = 7`-region identity and are
followed by three conditional partial-R² effects. Each quantity has its own
permutation, FDR, and synchronized spatial max-FWE result, maps, and optional
subject-bootstrap interval. See [Audit F8](#f8-three-predictor-commonality).

### Related follow-ups on features that already exist

- **Searchlight memory preflight/guard.** ✅ Delivered as F11: exact resident
  dataset bytes plus conservative neighborhood/shared/output/per-thread scratch
  estimates, default and scheduler-specific limits, and an explicit override.
- **Searchlight "permute-the-model-once" optimization.** ✅ Delivered as F9 for
  fixed Pearson/Spearman label-null models and paired fixed-model contrasts;
  F11 accounts for its shared cache memory.
- **Per-location model contrasts.** ✅ Delivered as F13 for dataset-vs-dataset
  and mixed fixed/dataset IS-RSA contrasts in atlas and searchlight analyses.
- **LOO extensions.** ✅ Delivered as F10: AnnaK uses foldwise training-only
  neural typicality, NN-like scalar rules retain neural-neighbor weighting, and
  multivariate behavioral profiles are predicted and permuted as complete rows.
- **Subject-bootstrap extensions (F17).** ✅ Completed: `-block` supplies
  within-stratum subject resampling with fixed stratum sizes, and `-loo`
  supplies fixed-OOF prediction-row bounds. Paired contrast,
  missing-dyad-aware IS-RSA joint/nuisance, and pairwise/three-predictor
  commonality intervals remain part of the same delivered family.
- **Commonality statistic and null (A1).** ✅ Partial-R² columns, reporting-scale
  guidance, and separate A-given-B/B-given-A Freedman–Lane nulls are delivered;
  `common` correctly retains its complete neural-item null.
- **Noise-ceiling split contract (A2).** ✅ Pattern-mode IS-RSA reliability is
  now rejected because its flattened condition×voxel input has no matched-
  repetition split axis; continuous mean-feature and classic-RSA ceilings remain
  supported and regression-tested.
- **Multimodal input contract.** ✅ Help now states that `-model_dset` requires
  the main data's voxel grid, so only source-localized M/EEG can be a
  per-location model; sensor-space or time-resolved RDMs use `-model_mat` or the
  shipped `-model_series`. Under `-null timeshift`, only the `InputFile` side is
  shifted while a `-model_dset` is rebuilt from unshifted data; F18 now records
  that interpretation in help/table provenance and applies it to primary,
  contrast, and regression effects. S6 now supplies trial/run nesting for
  already-estimated fMRI beta series; trial×trial multimodal inference remains
  separate from F20.
- **Searchlight hot path (A4).** ✅ A4c caches classic-RSA subject RDM triangles,
  A4d replaces per-element `THD_get_voxel` dispatch, and A4e eliminates the
  redundant Ledoit–Wolf `T·p²` residual-outer-product pass.
- **Circular-shift extensions (F18).** ✅ Paired contrasts and joint/nuisance
  regression are delivered. Commonality and LOO still need statistic-specific
  null contracts; censored or concatenated time series need an explicit
  run/segment descriptor before any within-segment shift generator is admitted.

### Deliberately out of scope

| Item | Why it stays out |
|---|---|
| **Bayesian / generative RSA, GBRSA** | A different paradigm — likelihoods, priors, optimization, convergence diagnostics — and 40–80+ days to do credibly. Interoperate via exported RDMs and AFNI maps instead of duplicating maintained specialist packages. |
| **Pattern Component Modeling (PCM)** | Same reasoning: a separate modeling framework (likelihood over pattern covariance), better served by round-tripping to the existing tools. |
| **Generic free-parameter `Model` object framework** | Recreating rsatoolbox's extensible Python model system inside an AFNI C command is a large API/serialization surface for limited gain. A single constrained fitted model (above) is the most we'd consider. |
| **Broad plug-in distance library** | Most extra metrics are not justified for fMRI beta patterns. Every new neural distance must state its noise model, orientation, data type, and inference behavior. `-model_mat` remains the escape hatch for anything exotic. |
| **Unified RSA + decoding pipeline** | Substantial new statistical scope; AFNI/TDT/PyMVPA already supply decoding. 3dRSA stays focused. |
| **No-code GUI** | 20–40+ days plus ongoing UI maintenance. Clear help, examples, and generated command templates have a better cost/benefit ratio. |
| **In-core graph/network conversion of IS-RSA matrices** | `-save_rdm` plus Python/R graph tools is a cleaner boundary than embedding a graph library. |

Three distinctions worth keeping straight when weighing the above:

- **Crossnobis is a distance *estimator*; whitened RDM comparison is a way to
  *compare* an estimated distance matrix with a model.** Different jobs.
- **Permutation / sign-flipping tests a null; bootstrapping estimates sampling
  uncertainty.** Different questions, different index sets.
- **Subject-label permutation tests behavior-to-brain association; time-shift /
  phase randomization tests whether temporal alignment matters.** Not
  interchangeable nulls.

---

## What 3dRSA can do already

3dRSA is not a prototype. It is an AFNI-native implementation of both **classic
within-subject RSA** and **inter-subject RSA (IS-RSA)**, with the spatial
machinery, nonparametric inference, and surface support to analyze real fMRI
data. This section summarizes the implemented capability set; the dashboard
records the deliberately open extensions and their remaining dependencies.

### The headline

3dRSA joins **volume and cortical-surface spatial mapping, classic RSA, IS-RSA,
restricted permutation, nuisance-aware model testing, max-statistic FWE
correction, and thread-reproducible AFNI output in one compiled program**. That
combination is the accomplishment — individual Python/MATLAB packages may offer
larger object APIs or specialized estimators, but this is a serious analysis
system, not glue around a correlation.

### Shipped capability index

| Feature | State |
|---|---|
| **Native AFNI build, install, and test lifecycle** | Wired into legacy Make, CMake/SUMA, AFNI install/package metadata, and registered CTest. The required-dependency integration gate currently passes all **299** 3dRSA checks plus the focused `1dTrdm` gate. |
| **IS-RSA (`-mode IS-RSA`) and classic RSA (`-mode RSA`)** | Both families implemented; observed effects match independent NumPy/nltools-style references to six decimals on the synthetic fixtures. |
| **Value-indexed traditional-RSA tables** | `-condition_column CCC -condition_order L1,L2,...` accepts one selected beta brick per subject/condition row in arbitrary table order. The declared labels are explicitly the row/column order of unlabeled `-model_mat` files. A reusable Cartesian data-table index rejects duplicate, missing, and unexpected key cells; the long and compact multi-brick forms are numerically identical. |
| **Second-order task-fMRI IS-RSA** | `-featuretype rdm` builds every subject's ordinary condition-pattern or runwise/crossnobis RDM, converts similarities to a common dissimilarity triangle, and compares those vectors across subjects. `-condition_metric` controls the ordinary inner estimator; `-neural_metric` controls the outer subject geometry. Behavioral/fixed models and per-location `-model_dset` modalities run in atlas and searchlight paths with existing permutation, bootstrap, contrast, commonality, and max-FWE machinery. |
| **Time-resolved M/EEG–fMRI fusion** | `-model_series` reads an ordered `TIME_LABEL MATRIX_FILE` list and evaluates every model RDM at every ROI/searchlight. The long-form table preserves verbatim time labels; AFNI bricks use deterministic `t####` labels; BH FDR and max-statistic FWE cover the complete time × space family under one shared relabeling set. Classic RSA and second-order IS-RSA accept ordinary or runwise neural inputs, with the existing bootstrap options where their estimands apply. |
| **Temporal RDM companion** | `1dTrdm` reads labeled observation×feature×time matrices, aligns subject/condition/partition axes, and writes subject-level temporal RDMs for correlation, cosine, Euclidean, or balanced crossnobis estimators. A labeled fixed model activates Pearson/Spearman inference: population-subject sign flips or synchronized fixed-condition relabeling supply raw p, BH q, and complete time or time×neighborhood max-FWE. Separately named descriptive products provide symmetric Pearson/Spearman RDM recurrence and ordered-independent-partition cross-time crossnobis while retaining the dyad axis; canonical outputs store only the unique time triangle and crossnobis diagonals exactly equal primary RDMs. A strict `Neighborhood Feature` graph supports overlapping sensor/source/generic-feature searches without assuming column adjacency, including neighborhood-local RDM, recurrence, cross-time, and fixed-model outputs. Explicit windows and sidecars preserve time, feature, graph, condition, counts, subject fits, inference, and provenance. A literal `-model_series_out independent` assertion guards the one-way all-feature group-mean bridge into `3dRSA`; same-subject fusion is not silently licensed. |
| **Seed representational connectivity** | `-seed_mask SSS [-seed_roi VALUE]` compares one seed's representational geometry with every non-overlapping atlas ROI or searchlight. Classic RSA correlates matched subject-specific seed/target condition RDMs, including ordinary or runwise crossnobis estimates; IS-RSA compares seed and target subject geometries for pattern or second-order RDM features. The existing subject/condition-label nulls, exchangeability blocks, BH FDR, synchronized spatial max-FWE, subject bootstrap, and target ceilings apply where defined. Targets sharing any seed feature are removed before inference and recorded in provenance. |
| **Subject-bootstrap confidence intervals** | `-bootstrap N [-boot_ci P]` writes `_bootLo/_bootHi` columns and plain-float map bricks for primary models, paired contrasts, all five pairwise-commonality quantities, and all ten three-predictor quantities. `-block` stratifies draws with fixed observed stratum sizes. With `-loo`, `_looBootLo/_looBootHi` resample completed held-out prediction/target rows for scalar NN, AnnaK, and synchronized multivariate-profile accuracy; predictions remain fixed, so this is not fold-refitting uncertainty. Regression intervals refit compact designs and bound `_b`; commonality intervals recompute the synchronized decomposition. All paths use one thread-reproducible resample set independent of permutation inference. Blocks are strata, not whole dependent clusters. |
| **Condition-bootstrap confidence intervals** | `-cond_bootstrap N [-boot_ci P]` writes per-model `_cbootLo/_cbootHi` columns and plain-float map bricks for classic RSA. Every subject neural RDM and model RDM uses the same draw; `-cond_group FILE` samples labeled condition groups as units. Supports plain/joint atlas RSA and runwise crossnobis ROIs/searchlights, is thread-reproducible, and omits duplicate-condition diagonal artifacts. |
| **Dual subject × condition confidence intervals** | Equal `-bootstrap N` and `-cond_bootstrap N` values activate the corrected two-factor estimator for fixed-model classic RSA. Subject-only, condition-only, and simultaneous variances produce one `_dualLo/_dualHi` t interval, with grouped conditions counted as independent units. Plain/joint primary effects and paired fixed-model contrasts run in ordinary or runwise/crossnobis atlas/searchlight analyses; independent formula and map/thread references pass. |
| **Circular-shift IS-RSA null** | `-null timeshift [-min_shift K]` tests whether temporal alignment matters for primary, paired-contrast, joint, and nuisance-adjusted effects from equal-length, gap-free ROI-mean series. A shared offset set supplies p/FDR/max-FWE in atlas and searchlights; F19 precomputes every subject-pair relative lag and serves each draw by lookup. With `-model_dset`, only `InputFile` shifts and the model modality remains fixed, explicitly breaking their temporal alignment. Regression retains its conditional coefficient/partial-effect estimand, while this complete-series null is explicitly distinct from label-null Freedman–Lane residual relabeling. |
| **Fourier phase-randomization IS-RSA null** | `-null phase` is the spectral counterpart to circular shifts in atlas ROIs and moving searchlights. It preserves every subject's complete-series mean and power spectrum while independently randomizing positive-frequency phases and rebuilding the neural geometry. One constant-memory stateless seed family supplies primary, paired-contrast, joint/nuisance-adjusted, p/FDR, and spatial max-FWE inference for all neural metrics; model matrices remain fixed. Each worker caches only its current location's spectra across draws. |
| **Behavioral rules** | Anna Karenina, nearest-neighbor, Euclidean similarity/distance, absolute difference, and multivariate profiles — standardized-Euclidean (`:euclid`) or covariance-whitened **Mahalanobis** (`:mahal`, Ledoit-Wolf-regularized). |
| **Explicit and cross-modal models** | `-model_mat` supplies one fixed matrix; `-model_series` supplies an ordered set of time-resolved fixed matrices; `-model_dset` turns a same-grid second modality into a model rebuilt independently at each atlas ROI or searchlight center. Multiple dataset models, mean/pattern/RDM features, streamed collinearity diagnostics, commonality, and circular-shift inference are supported. Under second-order IS-RSA, each modality first builds its own subject condition RDMs. |
| **Multiple models and nuisance adjustment** | `-model_joint` uses Freedman–Lane reduced-model relabeling; `-ortvec` removes a per-subject confound as *both* its `\|diff\|` and `sum` pairwise forms. Subject bootstrap refits the matching compact regression after invalid repeated-subject dyads are removed and reports coefficient intervals. |
| **Model contrasts (A vs B)** | `-model_contrast A-B` tests whether A fits better than B — paired same-relabeling Mantel difference (IS-RSA) or within-subject sign-flip / Wilcoxon signed-rank (classic RSA), with its own max-stat FWE family and optional paired subject-bootstrap interval. IS-RSA accepts fixed, mixed fixed/`-model_dset`, and two per-location dataset models in atlas/searchlight analyses. `-model_label` names models; hyphenated names are resolved by the longest valid A prefix. |
| **Model commonality (A,B[,C])** | `-model_commonality A,B` preserves the raw unique-A / unique-B / common decomposition (`common` may be negative = suppression, kept unclipped) and appends `partialR2_A/B`. `A,B,C` reports seven exhaustive raw regions plus three conditional partial-R² effects. IS-RSA and classic RSA use predictor-specific reduced-model Freedman–Lane nulls for unique/partial effects and the complete neural-item null for shared regions. Classic effects average subject decompositions and synchronize condition relabelings across subjects/locations, including ordinary and runwise/crossnobis searchlights. All quantities get maps and optional synchronized subject-bootstrap bounds; IS-RSA also accepts per-location `-model_dset` predictors for atlas/searchlight EEG–fMRI fusion. |
| **Cross-validated (crossnobis) distances** | `-runwiseTable` (subject × run betas) → unbiased cross-validated squared Euclidean condition RDMs, with negatives kept unclipped. Optional per-row `ConditionFile` mappings permit missing, reordered, and repeated conditions. Alternatively, `TrialFile` supplies an explicit unique trial ID and condition for every already-estimated beta sub-brick. Repeats/trials are averaged within run and each condition pair uses only its valid independent runs. `-noise_norm diag\|shrinkage` adds univariate or full (Ledoit-Wolf-whitened) normalization from `ResidFile`. Runs in atlas ROIs or volumetric searchlights for classic RSA and as the inner subject geometry for second-order IS-RSA. |
| **Comparison metrics** | Pearson, ordinary Spearman, expected Spearman `rhoa`, Kendall tau-b, Kendall tau-a, plus covariance-whitened `corr_cov` and origin-sensitive `cosine_cov`/WUC for balanced runwise classic-RSA crossnobis. Rho-a and tau-a avoid favoring tied categorical predictions; rho-a is the faster familiar-correlation option and remains a scalar comparator rather than a regression objective. The F4 metrics use the exchangeable-condition zero-distance covariance; unsupported contracts fail explicitly. |
| **Neural metrics** | Pearson `corr`, Spearman `scorr`, `cosine`, and Euclidean `euclid` (a dissimilarity). |
| **Reliability and noise ceilings** | Continuous-data IS-RSA split-half/interleaved geometry reliability; ordinary and runwise/crossnobis classic-RSA Nili leave-one-subject-out lower and inclusive group-mean upper bounds. Residual whitening is applied before crossnobis. All are written to the table and dataset sub-bricks (`reliability`, or `nc_low`/`nc_high`) and map under searchlights. Pattern/RDM-feature IS-RSA reliability remains explicitly rejected where no matched-repetition split exists. |
| **LOO prediction** | IS-RSA held-subject prediction with permutation p, FDR, max-stat FWE (`_looPfwe`/`_looZFWE`), optional fixed-OOF percentile bounds (`_looBootLo/_looBootHi`), tables, and searchlight map bricks. NN/euclid/absdiff scalar targets use distance-aware neural-neighbor weights; AnnaK uses foldwise training-only neural-typicality regression; multivariate `:euclid`/`:mahal` targets predict every measure and report their equal-weight mean predictive correlation. Null relabelings and bootstrap draws move complete profiles. Only exact target/estimand duplicates share computation/FWE/interval families. |
| **Fitted component models** | `-model_fit NAME=A,B,... [-fit_ridge R]` builds a standardized nonnegative ridge mixture of named fixed or per-location component RDMs. The default learns in outer leave-one-subject-out folds. For classic RSA, `-fit_condfold FILE` adds strict stimulus generalization: other-subject train/train dyads fit the weights, held-subject held/held dyads score them, and cross-boundary dyads are excluded. Complete refitting under synchronized subject-label (IS-RSA) or condition-label (classic RSA) draws supplies raw p, BH q, spatial max-FWE, typed maps, and thread-reproducible output. Fold-mean L1-normalized weights remain descriptive columns/maps. |
| **Volume searchlights** | Streamed center-voxel maps using AFNI's `SPHERE` / `RECT` / `RHDD` / `TOHD` neighborhood grammar for IS-RSA and both ordinary and runwise classic RSA. Ordinary `-dataTable` maps are explicitly tagged as same-data condition-pattern RDMs; `-runwiseTable` supplies crossnobis and optional residual-noise normalization. Both use synchronized max-FWE. |
| **Searchlight memory preflight** | Before loading resident subject/run/model datasets, reports an estimated peak split into exact input bytes, neighborhoods/shared/output arrays, and OpenMP-scaled scratch. Defaults to warning at 50% and refusing at 80% of detected RAM; `-memory_limit G` supports scheduler/container limits, while `-memory_override` records an explicit decision to proceed. |
| **Cached fixed-model searchlight nulls** | Pearson/Spearman/rho-a label-null searchlights precompute each fixed model's centered/ranked permuted triangles once and share them read-only across centers and threads, including fixed-model paired contrasts. F11 accounts for the cache; unsupported or per-location paths fall back exactly. A representative Spearman OMP1 test was 29.5× faster with byte-identical output. |
| **Surface data and geodesic searchlights** | Surface atlas/mask RSA in the plain build; optional `SUMA=1` build adds mesh-geodesic searchlights with dense/sparse node mapping. A surface `-searchlight` may omit `-mask` entirely to search the whole mesh — unlike a volume, every surface node is cortex, so there is no "not brain" region a mask would need to exclude (a volumetric searchlight still requires `-mask`). |
| **Parallel, reproducible inference** | OpenMP over ROIs plus immutable shared label, bootstrap, condition-bootstrap, circular-shift, or stateless phase draws. Exchangeability blocks (label-null IS-RSA) and synchronized per-model max-stat FWE are supported; outputs are byte-identical across thread counts for a fixed seed. |
| **Multiplicity handling** | Per element, per model: raw permutation p (`_p`), BH FDR q (`_q`), and max-statistic FWE p (`_pfwe`) with signed z map (`_ZFWE`); LOO gets its own FWE family (`_looPfwe` / `_looZFWE`). |
| **Strict input and option contracts** | Fixed matrices are checked for finiteness, symmetry, and dimensions; runwise tables validate subject/run/condition consistency, grids, residual requirements, one mapping label per local beta brick, and at least two common runs for every subject × condition pair. The canonical mapped condition order is printed. F22 fold files require exactly one label per condition, at least two folds, and at least three held plus three training conditions per fold; IS-RSA rejects the stimulus-fold option. Invalid estimator combinations fail explicitly instead of silently changing the analysis, including A2's rejection of pattern-mode IS-RSA reliability. |
| **Diagnostics and export** | Model intercorrelations and similarity/distance sense are reported; `-save_rdm` exports neural and fixed-model matrices with usable plotting hints while correctly explaining why a per-ROI `-model_dset` has no single model matrix to save. |
| **AFNI-native I/O and statistics** | Volumetric and surface datasets, AFNI neighborhood grammar, label tables, correctly typed statistic sub-bricks, ROI text tables, and AFNI/SUMA visualization conventions. |

### Coverage in other RSA/MVPA packages

Rechecked **2026-08-28** against current package documentation. This is a
capability map, not a claim that similarly named methods have identical
estimands, nulls, or multiplicity correction. **Direct** means the package
documents substantially the same analysis; **adjacent** means it supplies the
main building blocks or a related estimator; **none found** means no direct
implementation was established from the linked package documentation. Internal
AFNI integration and constant-factor optimizations are listed separately because
they are not meaningful cross-package scientific features.

| 3dRSA capability / roadmap item | State here | Other documented implementations | Important boundary |
|---|:---:|---|---|
| Classic ROI/searchlight RSA; target-RDM comparison | ✅ | **Direct:** [rsatoolbox](https://rsatoolbox.readthedocs.io/en/stable/), [PyMVPA RSA/searchlight](https://www.pymvpa.org/examples/rsa_fmri.html), [CoSMoMVPA RSA searchlight](https://cosmomvpa.org/_static/publish/run_rsm_measure_searchlight.html), [MATLAB RSA Toolbox](https://pmc.ncbi.nlm.nih.gov/articles/PMC3990488/), and TDT-based [ROI/searchlight RSA](https://github.com/CCN-github/RSA_fMRI_matlab/blob/master/RSA_roi.m). | 3dRSA now supports ordinary same-data and runwise/crossnobis searchlights. It labels the estimator explicitly rather than conflating conventional target-RDM mapping with cross-validated distance estimation. |
| IS-RSA / dyadic neural-behavior matrices | ✅ | **Direct:** [nltools `Adjacency`](https://nltools.org/auto_examples/01_DataOperations/plot_adjacency.html) supplies triangle-aware similarity/distance matrices and 2-D permutation. **Adjacent:** [BrainIAK ISC/ISFC](https://brainiak.org/docs/examples/isc/ISC.html). | BrainIAK's documented estimand is ISC/ISFC, not behavioral-model IS-RSA; 3dRSA integrates both classic RSA and IS-RSA spatial inference. |
| Volume, surface, and M/EEG-like searchlight machinery | ✅ | **Direct/adjacent:** [CoSMoMVPA](https://cosmomvpa.org/philosophy.html) uses one dataset/measure/searchlight interface for volume, surface, and MEEG; [PyMVPA](https://www.pymvpa.org/generated/mvpa2.measures.searchlight.sphere_searchlight.html) accepts arbitrary scalar measures in a sphere searchlight. | 3dRSA currently accepts AFNI volume/surface data; sensor-space M/EEG remains outside its input contract. |
| Run/chunk-aware crossnobis, unbalanced observations, and residual precision/whitening (4a–4c, 6, F21) | ✅ | **Direct:** [rsatoolbox crossnobis](https://rsatoolbox.readthedocs.io/en/stable/demo_dissimilarities.html) uses observation and session descriptors, supports repeated condition observations across partitions, and accepts optional estimated precision. **Adjacent/direct variants:** PyMVPA documents `CDist`; TDT's RSA example includes crossvalidation and multivariate noise normalization. | 3dRSA's per-row condition mapping admits missing/reordered/repeated conditions, averages repeats within run, and estimates every pair from its valid independent runs. Negative distances, residual-derived diagonal/shrinkage whitening, subject tests, contrasts, bootstraps, and searchlight max-FWE remain one tested path. |
| Multiple model regression / nuisance adjustment | ✅ | **Direct/adjacent:** [PyMVPA RSA `Regression`](https://www.pymvpa.org/generated/mvpa2.measures.rsa.html) supports arbitrary predictor DSMs; [CoSMoMVPA](https://cosmomvpa.org/_static/publish/run_rsm_measure_searchlight.html) documents regression-based RSA; rsatoolbox supplies fixed and flexible model evaluation. | The packages differ in coefficient scaling and inference. 3dRSA uses pair-space nuisances and Freedman–Lane reduced-model relabeling. |
| Paired fixed/per-location model contrasts (3b, F13) | ✅ | **Adjacent:** [rsatoolbox inference](https://rsatoolbox.readthedocs.io/en/stable/inference.html) evaluates and compares multiple models; classic MATLAB RSA inference also compares model performance. | No direct external match was established for 3dRSA's same-relabeling per-location cross-modal contrast with spatial max-FWE. |
| Pairwise commonality, partial-R², and synchronized inference (3c, A1, F15) | ✅ | **Adjacent only:** PyMVPA/CoSMoMVPA support multiple-RDM regression. **None found** for the explicit unique-A / unique-B / common decomposition plus reduced-model p/FDR/spatial max-FWE and bootstrap maps. | Generic regression coefficients are not commonality components. 3dRSA now supplies both subject-label IS-RSA and condition-label classic-RSA nulls. |
| Subject and grouped condition bootstraps (F1, F2) | ✅ | **Direct:** [rsatoolbox inference](https://rsatoolbox.readthedocs.io/en/stable/inference.html) documents separate RDM/subject and pattern/condition bootstraps, including grouped descriptors. MATLAB RSA Toolbox established subject/stimulus bootstrap inference. | 3dRSA applies synchronized draws to atlas/searchlight maps, crossnobis, contrasts, regression, and commonality, with duplicate-original dyads removed. |
| Dual subject × condition generalization (F6) | ✅ | **Direct elsewhere:** rsatoolbox documents a dedicated [`dual_bootstrap`](https://rsatoolbox.readthedocs.io/en/stable/inference.html) that combines simultaneous and separate resampling variance estimates. | 3dRSA now implements and independently verifies the same corrected variance structure for fixed classic-RSA effects and paired contrasts, including grouped conditions and atlas/searchlight output. It deliberately does not claim that this fixed-model interval solves condition-held-out fitted-model CV. |
| Noise ceilings and RDM reliability (3a, A2, F16) | ✅ | **Direct:** [rsatoolbox](https://rsatoolbox.readthedocs.io/en/stable/inference.html) provides fixed and cross-validated noise ceilings; [PCM](https://pcm-toolbox-python.readthedocs.io/en/latest/inference.html) uses free-model likelihood ceilings. **Adjacent:** [PyMVPA `PDistConsistency`](https://www.pymvpa.org/examples/rsa_fmri.html) maps between-run RDM reliability. | 3dRSA has continuous IS-RSA reliability plus ordinary and residual-whitened crossnobis Nili ceilings. Its lower bound excludes the evaluated subject; the inclusive upper bound is intentionally optimistic. |
| Model-aware scalar and multivariate-profile IS-RSA LOO prediction (F10) | ✅ | **None found** as the same AnnaK-typicality / rank-weighted-neighbor held-subject predictors in the surveyed general RSA packages. | rsatoolbox leave-one-out usually cross-validates fitted *models* across RDMs; that is not this behavioral-target predictor. 3dRSA's profile score deliberately gives each target measure equal weight; it is not a Mahalanobis prediction-error score. |
| Circular-shift null (F3/F18/F19) and further extensions | ✅ / planned | **Direct null machinery:** nltools added circular-shift time-series permutations; [BrainIAK](https://brainiak.org/docs/brainiak.html) provides phase-shift ISC inference. | External implementations target correlation/ISC. 3dRSA uses relative-lag IS-RSA lookups synchronized over spatial max-FWE for primary, contrast, and conditional-regression effects. Commonality/LOO and segmented-series shifts remain future method/data contracts. |
| Phase-randomization null (F5/F5b) | ✅ ROI + searchlight | **Direct elsewhere:** [BrainIAK `phase_randomize` / `phaseshift_isc`](https://brainiak.org/docs/brainiak.utils.html) and nltools time-series permutation utilities. | 3dRSA integrates a real-signal phase generator with fixed/per-location models, four neural metrics, contrasts, conditional regression, FDR, synchronized spatial max-FWE, provenance, local-spectrum reuse, and thread reproducibility. |
| Expected Spearman rho-a (F23) | ✅ | **Direct elsewhere:** [rsatoolbox `compare_rho_a`](https://rsatoolbox.readthedocs.io/en/stable/comparing.html) implements the same expectation under random tie breaking and recommends it for tied model RDMs. | 3dRSA adds the closed-form statistic to AFNI-native ROI/searchlight permutation inference, paired contrasts, bootstrap, ceilings, LOO, temporal nulls, and its fixed-model searchlight cache; regression remains ordinary rank-transformed Spearman. |
| Whitened unbiased RDM comparison (F4) | ✅ balanced crossnobis | **Direct elsewhere:** [rsatoolbox whitened cosine/correlation](https://rsatoolbox.readthedocs.io/en/stable/comparing.html). **Adjacent:** PCM models second moments with a likelihood. | 3dRSA now matches rsatoolbox's default exchangeable-condition covariance for `corr_cov` and `cosine_cov` while integrating subject inference, contrasts, ceilings, atlas/searchlight max-FWE, bootstrap, and residual voxel whitening. F21 unequal-support covariance remains deliberately open. |
| Second-order task-fMRI IS-RSA (A3) | ✅ | **Building blocks:** rsatoolbox compares stacks of RDMs; nltools compares pairwise matrices; PyMVPA/CoSMoMVPA generate local condition RDMs. **None found** for the complete subject-condition-RDM → subject-similarity → behavioral IS-RSA searchlight in one command. | 3dRSA now integrates the full estimator, including ordinary and runwise/crossnobis inner RDMs, per-location cross-modal models, synchronized spatial inference, and explicit inner/outer metric provenance. |
| Time-resolved M/EEG–fMRI fusion with joint time × space FWE (F20) | ✅ | **Adjacent/direct mapping:** rsatoolbox evaluates time-resolved M/EEG RDMs ([MNE example](https://rsatoolbox.readthedocs.io/en/stable/demo_meg_mne.html)); CoSMoMVPA shares measures across fMRI and MEEG. | 3dRSA now consumes ordered time-labeled RDMs, writes AFNI timepoint maps and a long table, and applies BH/max-statistic inference to the joint time × space family. No surveyed package documentation established that same complete AFNI-native workflow. |
| Fitted/weighted component models, held-out comparisons, and condition generalization (F7, F14, F22) | ✅ | **Direct elsewhere:** rsatoolbox flexible models plus cross-validation/bootstrap; [PCM/PcmPy](https://pcm-toolbox-python.readthedocs.io/en/latest/fitting.html) fits component/free models and evaluates them by group cross-validation. | 3dRSA supplies a constrained nonnegative-ridge, held-subject, refitted-null model, paired held-fold Fisher-z comparisons, and explicit condition folds that train/test on disjoint dyad sets with synchronized spatial inference. This is deliberately not a clone of either general model framework. |
| Three-predictor commonality (F8) | ✅ | **Adjacent only:** arbitrary multi-RDM regression exists in PyMVPA and CoSMoMVPA. **None found** for a seven-region commonality decomposition with separate synchronized spatial FWE families. | 3dRSA reports seven raw commonality regions plus three conditional partial-R² effects, with reduced-model nulls for unique/partial quantities, complete relabeling for shared quantities, synchronized spatial max-FWE, maps, and bootstrap intervals in IS-RSA and classic RSA. |
| Unified RSA + decoding core (M1/M2 decision) | decision | **Direct elsewhere:** PyMVPA, CoSMoMVPA, and TDT already run RSA and decoding through shared dataset/searchlight machinery. | Their architecture is evidence for M1/M2, but AFNI-native decoding would still require a new validated estimator/output program. |
| Visualization / plotting consumer (F12) | blocked | **Direct elsewhere:** rsatoolbox visualizes RDMs and model-evaluation results; nltools plots adjacency/MDS/graphs; the MATLAB RSA Toolbox includes RDM/MDS displays. | 3dRSA exports matrices and AFNI maps now; its AFNI-only consumer remains blocked on the required `1dplot.py` functionality. |
| Engineering-only items: native build/warnings/tests, memory guard, fixed-null cache, typed extraction, RDM cache, lag-table optimization (B1/B2/2, F9/F11, A4, F19) | ✅ | No useful like-for-like scientific package comparison. | These determine whether shipped analyses are installable, reproducible, memory-safe, and practical; package feature lists rarely expose equivalent implementation details or benchmarks. |

### Particularly distinctive

Not generally available *together* in the other toolboxes:

- **Cross-modal per-ROI model** (`-model_dset`) — another modality as the model,
  rebuilt per ROI **or per searchlight sphere**, baked into the option grammar.
- **Pair-space nuisance regression** (`-ortvec`) — a per-subject confound removed
  as both its `|diff|` and `sum` pairwise forms, assuming no model shape.
- **Freedman–Lane partial permutation** for joint models over RDM triangles.
- **Classic-RSA commonality with synchronized condition-label inference** rather
  than invalid subject sign flips of nonnegative squared semipartials.
- **AnnaK / NN exact-orthogonality**, documented and exploited.
- **Nested nonnegative component fitting in both classic RSA and IS-RSA**, with
  held-subject scoring, complete refitting under every synchronized label draw,
  spatial max-FWE, and per-location component-weight diagnostics.
- **Noise-normalized crossnobis searchlights** with residual-derived diagonal or
  Ledoit-Wolf shrinkage whitening, negative distances retained.
- **Primary, paired-contrast, commonality, grouped-condition, and corrected
  dual subject × condition uncertainty maps** whose bootstrap draws are
  separate from null relabelings and synchronized over the spatial map.
- **Circular-shift IS-RSA with max-FWE**, including paired fixed/per-location
  contrasts and joint/nuisance regression, using one deterministic
  subject-offset set across searchlights.
- **Time-resolved M/EEG–fMRI fusion with joint time × space inference** from one
  ordered, labeled RDM series, in both ROI and searchlight analyses.
- **Synchronized max-statistic FWE** across the whole spatial map, from the same
  shared relabeling set that makes results thread-count independent.

### Correctness and reproducibility verified through 2026-08-28

- Neural/model matrices are reduced to the strict upper triangle.
- Permutation relabels items by applying the same permutation to rows and
  columns — never shuffling dependent dyads (the classic Mantel/IS-RSA error).
- Separate models use two-sided Mantel-style correlation tests; joint/nuisance
  models use standardized triangle regression. The label null uses
  Freedman–Lane reduced-model residual relabeling; the circular-shift null keeps
  the design fixed and destroys complete raw-series temporal alignment. Classic
  RSA tests subject-level effects by sign flip.
- Pairwise commonality reports the raw three-term decomposition plus two partial-
  R² effects. Its unique/partial components use predictor-specific
  Freedman–Lane nulls while `common` retains the complete neural-item null;
  classic RSA shares each condition relabeling across all subjects and map
  locations before averaging the subject components.
- The same relabeling index is shared across ROIs/searchlights, giving a valid
  synchronized max-statistic FWE null.
- Fitted component weights never see the held subject's evaluation dyads.
  Classic folds train on other subjects; IS-RSA folds exclude every dyad that
  touches the held subject. Every null draw repeats the complete fit, and its
  own synchronized max-statistic family spans the spatial map.
- Crossnobis (including negative distances), diagonal/full residual whitening,
  runwise contrasts, reliability/noise ceilings, and map/statistic metadata match
  independent references or exact invariants.
- Bootstrap draws are distinct from permutation draws; paired contrast effects
  resample both fits together, and repeated-original-subject or
  repeated-original-condition diagonal artifacts are omitted. Circular-shift
  draws preserve each complete subject series while breaking shared alignment;
  phase draws preserve each series' mean and power spectrum while destroying
  phase-locked timing.
- Pearson, Spearman, expected Spearman rho-a, cosine, Euclidean, average ranks,
  Kendall tau-a, BH FDR,
  split reliability, Nili ceilings, and the data-table/ROI extraction paths match
  their documented formulas.

Baseline validation recorded through 2026-08-22: plain and `SUMA=1` builds
succeed; planted IS-RSA
(`r=0.997379`), pattern IS-RSA (`r=0.630688`), classic RSA (`mean r=0.497046`),
and Pearson joint-regression outputs matched scipy/numpy to six decimals; on 200
null ROIs × 1,000 relabelings mean uncorrected p was 0.470, ~8% fell below .05,
no FWE p fell below .05, every FWE p ≥ its uncorrected p, and 1- vs 8-thread
output was byte-identical. Since then, independent numeric references and
thread/map invariants have been added for crossnobis searchlights, noise
whitening, per-location models, both one-factor bootstraps, circular shifts,
rho-a, and ROI/searchlight phase randomization.
*All correctness, contract, warning, and portability items surfaced by the
original audit (1a–1h), plus A1, A2, A4a, A4b, and A4d, are fixed; the current
required-dependency suite reports 239 passed, 0 failed.*

---

## Independent audit, 2026-08-22

An independent read of `3dRSA.c`, `thd_simmatrix.c`, `thd_permute.c`,
`thd_patterns.c`, `thd_datatable.c`, and the later `thd_phasefft.c` wrapper
against this roadmap and the program
help. **Every dashboard item marked ✅ that was sampled is genuinely
implemented in the code**, the option contracts reject what they claim to
reject, the build/CTest wiring is real (`src/Makefile.INCLUDE:1793`,
`src/CMakeLists.txt:130`, `tests/CMakeLists.txt:29`), and the shared
relabeling/bootstrap/shift/phase sets are genuinely immutable and separately
streamed. The findings below are new; they are additions, not retractions.

At the time of this 2026-08-22 audit the numeric suite could not be executed (no
SciPy/nibabel in the available interpreter), so its then-current "123/123"
figure was carried forward rather than re-verified. The current dashboard
supersedes that historical note with a verified **239/239** run. The A1 result
below was obtained from an
independent NumPy reimplementation of `rdm_commonality_one` plus its
permutation null, and a live end-to-end 3dRSA run on synthetic AFNI fixtures.

### A1. Commonality unique components lose power

**Resolved 2026-08-25.** The analysis below describes the original implementation
that motivated A1; the delivered resolution is recorded after the simulation.

The original `rdm_commonality_one` path reported and tested
`uniq_A = R²_AB − R²_B`. Two things then interact badly:

1. The statistic is a **difference of R²**, not a partial R². As `R²_B`
   approaches the ceiling, the room left for `R²_AB − R²_B` shrinks toward zero
   even when A's unique contribution to the generating model is unchanged.
2. The original null **relabelled the neural items**, which decoupled *both* models at
   once. So the reference distribution is the complete null, whose spread is a
   fixed ~1/m regardless of how strong B actually is.

Independent simulation (20 subjects, 190 dyads, 1000 relabelings, unique-A
signal held constant at 0.5):

| B signal | mean observed `uniq_A` | power, original raw/complete-null test | power, partial `R²` |
|---:|---:|---:|---:|
| 0.0 | 0.198 | 1.00 | 1.00 |
| 2.0 | 0.043 | 0.99 | 1.00 |
| 4.0 | 0.013 | **0.06** | **1.00** |

Type I error is fine (0.028 with unrelated models, 0.050 with models correlated
at 0.7), so this is **not** a false-positive problem and nothing already
reported from `-model_commonality` is wrong. It is a power problem, and it
bites hardest in exactly the advertised use case — EEG–fMRI fusion, where the
competing modality's RDM is strongly related to the neural target, so a null
`uniq_EEG` cannot be read as "EEG adds nothing".

Recommended fix, in order of cost:

- ✅ **Delivered 2026-08-25:** report and test the **partial** `R²`,
  `(R²_AB − R²_B)/(1 − R²_B)`, alongside the raw difference. It restored full
  power in all three simulation regimes above. `common` stays on the raw scale,
  so the three-way decomposition identity is unchanged; the two partial forms
  are added columns, not replacements. Values, bootstrap bounds,
  fixed/per-location maps, and thread reproducibility are covered by independent
  numeric tests; their final conditional inference is the next delivered item.
- ✅ **Delivered 2026-08-25:** give the unique components a **Freedman–Lane** null, the same
  reduced-model residual relabeling `-model_joint` already uses: to test
  `uniq_A`, fit the neural triangle on B, relabel only that residual, refit.
  This gets the reference distribution right rather than merely rescaling the
  statistic. Exhaustive pair-block references independently reproduce both raw
  and partial-R² p-values plus their synchronized max-FWE families.
- ✅ **Delivered 2026-08-25:** help distinguishes the raw decomposition scale
  from partial R², recommends partial R² for comparing unique-effect magnitude
  across differently strong competitors, and documents the conditional null.

### A2. Noise-ceiling split axis  ·  ✅ Resolved 2026-08-25

Under `-mode IS-RSA -featuretype pattern` the feature vector produced by
`THD_roi_pattern` is laid out `[sub-brick][voxel]`. The reliability split in
`3dRSA.c` indexes that flat vector directly, so:

- `-nc_split half` takes the first `nfeat/2` entries of the flattened vector. For
  an even number of condition bricks this is the first half of the conditions;
  for an odd number it also cuts through the middle brick. Either way it is not
  a split-half of matched content.
- `-nc_split interleave` takes every other flattened entry, mostly **alternating
  voxels** within each brick (with parity crossing brick boundaries). Neighboring
  voxels are correlated after normalization and smoothing, so this inflates the
  reliability estimate — the same bias the help already warns about for fast-
  sampled time series, but unmentioned here.

**Resolution:** `-noise_ceiling` with `-featuretype pattern` now exits before
analysis with a methodological explanation, and the help states that IS-RSA
reliability currently applies to continuous mean features. An eventual pattern-
mode estimator needs explicit matched-repetition/run metadata; merely splitting
odd/even conditions would still compare different content. The new rejection,
the unchanged continuous IS-RSA reliability maps, and the unchanged classic-RSA
ceilings pass the registered numeric gate.

### A3. Second-order IS-RSA  ·  ✅ Resolved 2026-08-25

`-featuretype pattern` builds the subject-by-subject neural matrix by
correlating subject A's ROI voxel pattern against subject B's, **voxel by
voxel**. That assumes fine-grained anatomical correspondence across subjects
after normalization, which is exactly the assumption hyperalignment exists
because it does not hold. For continuous data `-featuretype mean` sidesteps the
problem (the ROI mean time course is aligned by the shared stimulus, not by
anatomy), which is why the default is right — but the pattern branch inherits a
strong hidden assumption that the help does not state.

The standard, correspondence-free IS-RSA for condition data is **second-order**:
build each subject's own condition × condition RDM, then compute the
subject-by-subject similarity *of those RDMs*, then relate that to behavior.
Nothing about it needs voxel alignment.

**Resolution:** `-mode IS-RSA -featuretype rdm` now implements that estimator in
atlas ROIs and volumetric searchlights. With an ordinary `-dataTable`,
`-condition_metric corr|scorr|cosine|euclid` constructs every subject's inner
condition RDM. With `-runwiseTable`, the inner estimator is crossnobis and may
use `-noise_norm diag|shrinkage`. Similarity-valued inner matrices are converted
to `1-similarity`; Euclidean and signed crossnobis distances are retained as
dissimilarities, including meaningful negative crossnobis estimates. The strict
triangles are stacked and `-neural_metric` constructs the outer subject ×
subject neural matrix.

That outer matrix enters the existing subject-axis model and inference stack:
separate/joint/nuisance-adjusted models, label permutations, subject bootstrap,
contrasts, pairwise commonality, FDR/max-FWE, `-save_rdm`, and atlas/searchlight
outputs. For ordinary input, behavioral table columns and `-model_dset` are
supported; each model modality builds its own condition RDM before its outer
subject matrix is formed. For runwise input, fixed subject-by-subject
`-model_mat` models are supported. The runwise table intentionally contains no
subject-level behavioral columns, so column models, `-ortvec`, `-block`, LOO,
and `-model_dset` are rejected there rather than guessed.

The remaining boundaries are explicit rather than silent. At least three
conditions are required; ordinary neighborhoods need at least two voxels;
`-noise_ceiling` has no matched second-order split contract; and condition
bootstrap remains a classic-RSA procedure. Adding behavioral-column convenience
to runwise second-order input would require preserving one constant value per
subject or introducing a separate subject table. It is useful ergonomics, but
not a scientific blocker because the same subject model can be supplied as
`-model_mat`; keep it as a future input-contract extension rather than reopening
A3. Surface data reaches the same shared estimator, but A3 does not yet have a
surface-specific independent-reference regression; add one before describing
second-order surface execution as separately verified.

Eight new numeric checks independently reconstruct ordinary, cross-modal, and
crossnobis inner/outer matrices, compare atlas with whole-volume searchlights,
verify exact thread reproducibility, inspect estimator metadata, and exercise
the runwise-model rejection. The complete required-dependency suite passes
239/239.

### F16. Runwise crossnobis noise ceiling  ·  ✅ Resolved 2026-08-25

`-noise_ceiling` now works with classic `-runwiseTable` input. At each ROI or
searchlight, `rsa_subject_rdm` first constructs every subject's crossnobis RDM
by averaging condition-contrast products over distinct run pairs. If
`-noise_norm diag|shrinkage` is requested, residual-derived whitening is applied
to the run patterns before those products. Thus the RDM supplied to the ceiling
is already an unbiased, independent-run estimate; the ceiling never falls back
to an ordinary same-data distance.

The reported bounds preserve the established Nili definitions. For subject
*s*, `nc_low` compares that subject's crossnobis triangle with the mean triangle
of all **other** subjects, then averages the correlations over subjects. This is
the leave-one-subject-out readout and cannot reuse the evaluated subject in its
template. `nc_high` compares each subject with the all-subject mean, including
itself; that inclusion is intentional because the upper bound asks how well an
optimally pooled data-derived RDM could perform. It is labeled and documented as
an optimistic upper bound rather than being presented as held-out performance.

There is no condition leakage to repair for a fixed model:
the model matrix has no fitted parameters, and independent runs already occupy
the two sides of each crossnobis product. F7 now learns component weights on
other subjects and holds the evaluated subject out, but it does not claim
generalization to unseen conditions; condition-held-out inner CV remains an
explicit future fitted-model extension. The command's separate
`-loo` option remains an IS-RSA predictor of a scalar subject variable; classic
runwise tables contain no such target, so `-loo` is still rejected with guidance
to use `-noise_ceiling` and read `nc_low` when the intended quantity is the
subject-LOO reliability bound.

The table records that the ceiling used run-independent crossnobis RDMs, and
`nc_low`/`nc_high` remain plain-float AFNI bricks. Five new checks match
unwhitened and diagonally whitened results to independent NumPy references,
verify brick values and labels, prove atlas/whole-volume-searchlight equality,
and require exact 1-vs-6-thread output. The complete suite passes 239/239.

### F15. Classic-RSA commonality null  ·  ✅ Resolved 2026-08-25

`-model_commonality A,B` now runs in classic RSA for both ordinary same-data
condition RDMs and runwise/crossnobis RDMs, in atlas ROIs and volumetric
searchlights. Each subject's five-component decomposition is computed first;
the reported group effects are the means of `uniq_A`, `uniq_B`, `common`,
`partialR2_A`, and `partialR2_B`. Optional subject-bootstrap bounds resample
those subject components, preserving the sampling unit.

The inference contract deliberately differs from the primary classic-RSA
subject sign flip. Squared semipartial unique effects are nonnegative, so
sign-flipping them would produce a meaningless null. For A-given-B and
B-given-A, 3dRSA instead fits the reduced model separately within each subject,
relabels its residual RDM by condition, adds the fitted reduced component back,
and recomputes the decomposition. One condition permutation is synchronized
across every subject and spatial location in a draw. `common` is not an
added-variable effect, so its null completely relabels each prepared neural RDM
using that same condition permutation. Pearson and Spearman are supported;
Kendall metrics are rejected because this is a regression decomposition.

The classic commonality condition set is immutable and separate from the
primary model's subject-sign set; exact group sizes may therefore differ.
Each of the five components has its own p/FDR and synchronized spatial max-FWE
family, and table metadata records both the null type and actual relabeling
count. Algebraic ties caused by model symmetries are compared at float precision
so exact enumeration cannot change merely because equivalent subject terms
accumulate in a different order. With `-nperm 0`, values and bootstrap bounds
remain available while the uncalibrated companion map is labeled `_FZ` and is
not FIZT-typed.

Six targeted checks cover point-estimate execution, an independent exhaustive
6! Freedman–Lane/complete-null reference for effect/p/FDR/FWE, subject-bootstrap
bounds, whole-atlas versus whole-volume-searchlight equality and map labels,
exact 1-vs-6-thread reproducibility, and metric rejection. The complete suite
passes 239/239.

### F5. ROI-first phase-randomization null  ·  ✅ Resolved 2026-08-26

`-null phase` now supplies a spectral-preserving temporal null for continuous
IS-RSA with equal-length, gap-free `-featuretype mean` series. For every random
slot, each subject and positive-frequency bin receives its own seeded uniform
phase. Conjugate symmetry reconstructs a real series; DC and the even-length
Nyquist bin are unchanged. The identity remains slot 0, so empirical p-values
retain the program-wide `1/N` floor.

The implementation deliberately reuses F18's complete statistic bundle rather
than defining a phase-only analysis path:

- primary model effects, paired fixed or per-ROI `-model_dset` contrasts, joint
  coefficients, and separately fitted nuisance-adjusted coefficients all use
  the same reconstructed neural matrix per draw;
- model and nuisance matrices remain fixed, so the null asks whether the neural
  series' phase-locked alignment contributes model-associated geometry;
- one constant-memory `THD_phase_set` derives phases statelessly from
  seed × draw × subject × frequency, making ROI max-FWE and OpenMP results
  independent of thread scheduling;
- AFNI's reentrant mixed-radix FFT is namespaced behind `thd_phasefft.c`, while
  one reusable feature matrix and similarity matrix avoid per-draw allocation;
- corr, scorr, cosine, and Euclidean neural matrices, primary/contrast/regression
  p-values, spatial max-FWE, spectrum preservation, metadata, failure contracts,
  and exact 1-vs-6-thread output match independent NumPy tests.

F5 was initially delivered ROI/atlas-only. F5b below now supplies moving
searchlights. Commonality, LOO, `-block`, non-mean features, segmented inputs,
and undersized permutation sets still fail explicitly rather than falling back
to label inference because those are separate method/data contracts.

### F23. Expected Spearman rho-a  ·  ✅ Resolved 2026-08-28

`-metric rhoa` (also accepting `rho-a` and `rho_a`) implements the expected
Spearman correlation when ties in each triangle are broken independently at
random. For `m` compared entries it average-ranks both vectors, centers them at
`(m+1)/2`, and returns

```text
12 * dot(centered_average_ranks_A, centered_average_ranks_B) / (m^3 - m)
```

The fixed untied-rank denominator is the important distinction from ordinary
Spearman: tied categorical predictions do not obtain an advantage from having
a smaller rank variance. With no ties, rho-a equals ordinary Spearman exactly.

Rho-a is integrated anywhere 3dRSA consumes a scalar matrix comparison:
primary fixed/per-location models, paired model contrasts, subject/condition
bootstrap intervals, LOO prediction, reliability/noise ceilings, label and
temporal nulls, atlas ROIs, and volume/surface searchlights. Fixed-model label
searchlights rank and center every permuted model triangle once in the shared
F9 cache; workers only rank the changing neural triangle and take the rho-a
dot product. Joint/nuisance regression, commonality, and fitted-component
models reject rho-a because their estimands are least-squares objectives;
`-metric spearman` remains the supported rank-transformed regression.

Five targeted checks compare tied inputs to an independent SciPy-rankdata
closed form, verify the intended reduction relative to ordinary tied
Spearman, prove the no-tie identity, exercise the regression rejection, and
match cached searchlight r/p/FWE plus paired contrasts to the uncached atlas
path.

### F5b. Phase-randomization searchlights  ·  ✅ Resolved 2026-08-28

The F5 temporal engine already separated location-local data from the shared
stateless phase family. F5b completes that design for moving neighborhoods:

- a worker reduces the current sphere to one mean series per subject;
- it Fourier-transforms those local series once into a per-worker spectrum;
- every null draw rotates that retained spectrum, reconstructs one reusable
  real feature matrix, and refills one reusable neural similarity matrix;
- when the worker advances, the same storage is overwritten by the next
  center's spectra rather than accumulating a whole-brain spectral cube; and
- the immutable seed × draw × subject × frequency phase factors stay shared
  across all centers, preserving synchronized spatial max-FWE and independence
  from OpenMP scheduling.

F11 memory preflight now includes the complex local spectra, inverse-FFT work,
reconstructed series, neural matrix, and temporal-regression scratch per
thread. It also recognizes that the phase family is stateless and therefore
does not charge an erroneous `nperm × nsub` shared permutation array.

Three targeted checks independently rebuild four local searchlight series and
all phase draws, matching observed rho, uncorrected p, and across-center
max-statistic FWE; verify the persisted local-cache/shared-family provenance;
and require byte-identical table results at one versus six threads. The
remaining commonality, LOO, blocked, segmented-series, and non-mean-feature
restrictions are unchanged from F5.

### F19. Time-shift relative-lag table  ·  ✅ Resolved 2026-08-26

The old `rsa_timeshift_mantel` copied every subject series, rebuilt a complete
neural matrix with `THD_simmat_from_features`, and allocated/freed that matrix
for every draw at every location. F19 uses the circular-shift identity that the
metric between subject *i* shifted by *a* and subject *j* shifted by *b* depends
only on relative lag `(b-a) mod T`.

`THD_simmat_lag_table` now prepares each subject once per location—centering for
Pearson, ranking and centering for Spearman, retaining level for cosine, and
retaining the raw series for Euclidean distance. It computes only relative lags
that actually occur in the immutable shift set, stores them in strict-pair-major
order, and `THD_simmat_from_lag_table` fills one reusable neural matrix per draw.
The cost changes from `O(nperm × npair × T)` feature operations plus repeated
allocation to `O(nused_lag × npair × T + nperm × npair)`; `nused_lag` is at most
`min(T,nperm)`. The direct circular cross-products are thread-local and support
arbitrary `T`, avoiding the shared/static state and legal-length restrictions
of AFNI's legacy FFT wrapper inside an OpenMP-over-locations loop.

F11 counts the shared pair×lag usage mask and each worker's prepared series,
norms, lag table, and reusable matrix. Help and table provenance now state the
engine and clarify the multimodal null: under `-model_dset`, only `InputFile`
shifts; the model modality stays unshifted, intentionally breaking their shared
temporal alignment.

Two new checks independently reconstruct Pearson, Spearman, cosine, and
Euclidean shifted neural matrices and verify effect/p/FWE, then inspect the
engine/model-side provenance. Existing independent offset/statistic,
atlas/searchlight, max-FWE, and exact 1-vs-6-thread tests remain green. The full
gate passes 239/239.

On the retained 256-center whole-volume-sphere fixture (OMP1, 20 subjects × 60
TRs, 1,000 shifts, Spearman outer comparison), the warm three-run median fell
from **4.55 s to 2.10 s wall time (2.17×)** and from **4.40 s to 1.96 s user CPU
(2.24×)**. Numeric rows are byte-identical; the sole table diff is the new
provenance line. The gain will grow with `nperm/T`, while small permutation
budgets do not pay for unused lags because the shared usage mask skips them.

### F18. Circular-shift contrasts and regression  ·  ✅ Resolved 2026-08-26

F18 extends the F3/F19 null without changing its data-level intervention. Every
draw independently rotates each subject's complete gap-free ROI-mean series,
rebuilds the neural subject matrix from the relative-lag table, and leaves every
fixed or per-location `-model_dset` matrix unshifted. The same draw now feeds
three additional families: paired `r(neural,A)-r(neural,B)` contrasts, all
reported coefficients from one `-model_joint` fit, and each separately fitted
model coefficient with the full `-ortvec` nuisance set projected out. Each
family retains its own p/FDR and synchronized spatial max-FWE correction.

The regression distinction is explicit. The reported standardized coefficient
and partial-r retain their ordinary conditional meaning because the complete
model/nuisance design is fitted on every draw. Its circular-shift null, however,
is a **complete neural-series alignment null**: it destroys all cross-subject
temporal alignment before reconstructing the response RDM. It is not the
predictor-specific Freedman–Lane reduced-residual relabeling used under
`-null labels`. Help and table provenance name that difference so the two p
values cannot be mistaken for interchangeable tests of the same null.

One F19 lag-table fill and one shifted neural matrix serve every requested
statistic at a location. Model triangles are extracted once. For regression,
`THD_tri_design_new/apply` retains each ranked/z-scored design pseudoinverse and
only transforms/refits the changing response triangle, avoiding an SVD per
shift. F11 now counts those fixed triangles, pseudoinverses, coefficient
scratch, and contrast scratch in each worker's memory estimate.

Eight new checks independently reconstruct Pearson/Spearman paired differences and standardized
joint/nuisance coefficients for every shift, verify uncorrected and spatial
max-FWE p-values, inspect the complete-series-null provenance, compare atlas
with whole-volume `-model_dset` regression/contrast searchlights and their AFNI
brick labels, and require exact one-vs-six-thread output. Commonality, LOO, and
censored/concatenated series remain rejected or unsupported: the first two need
statistic-specific shifted-null estimands, while the last needs a run/segment
descriptor and within-segment generator. Those boundaries are follow-ons, not
unfinished parts of the delivered fixed-model/conditional-regression scope.

### F10. Model-aware/multivariate LOO prediction  ·  ✅ Resolved 2026-08-26

`-loo` now follows the behavioral model's prediction hypothesis instead of
silently applying one scalar nearest-neighbor estimator to every rule.

- Scalar `:nn`, `:euclid`, and `:absdiff` models retain the established
  outcome-blind neural-neighbor predictor: within each held-subject fold,
  subjects receive the average-rank weight of their neural closeness. Neural
  distances are negated first, so nearer subjects always receive more weight.
- Scalar `:annak` models use neural typicality. In fold *i*, every training
  subject's typicality is its mean neural closeness to the other training
  subjects (excluding *i*); a training-only linear fit maps that typicality to
  behavior. The held subject is predicted from its mean closeness to the same
  training set. Thus neither the held target nor held-subject edges enter the
  fitted mapping.
- Multivariate `COLUMN1,COLUMN2,...:euclid|mahal` models retain all source-column
  identities. One set of outcome-blind neural-neighbor weights predicts every
  held measure, and `looR` is the equal-weight arithmetic mean of the
  measure-wise predicted-versus-true correlations under `-metric`. Null
  relabelings move complete subject-profile rows, preserving covariance among
  the targets.

The profile LOO estimand deliberately does not change between `:euclid` and
`:mahal`: those rules still define different behavioral RDMs for the RSA fit,
but the prediction readout asks whether the same raw multivariate subject
profile can be recovered and gives every reported measure equal weight. It is
not a Mahalanobis prediction-error statistic. Exact duplicates may share work
and one max-FWE null only when both their ordered target columns and prediction
estimand match. Consequently AnnaK and NN models built from the same scalar
column now have distinct computations and FWE families; scalar NN/euclid/
absdiff duplicates may share, as may profile euclid/mahal duplicates over the
same ordered columns.

The existing output contract is preserved: per model, tables contain
`_looR/_looP/_looQ` and `_looPfwe`, while atlas/searchlight datasets contain
`_looR/_looZ` and `_looZFWE`. Six added regressions cover an independent NumPy
reconstruction of all three estimands, exhaustive pair-block profile-row nulls,
per-estimand spatial max-FWE, distinction from NN on one scalar target,
searchlight map/label output, and exact one-vs-four-thread reproduction.

Boundaries intentionally left open are recorded rather than hidden. A
`-model_mat` or `-model_dset` has no raw held-subject target and remains skipped.
F17 now supplies fixed-OOF prediction-row confidence intervals, explicitly not
nested fold-refitting uncertainty. Circular-shift/phase LOO would need a
distinct temporal-prediction null; and a future covariance-weighted
multivariate accuracy would be a new output statistic, not a silent
reinterpretation of `looR`.

### F7. Constrained fitted component model  ·  ✅ Resolved 2026-08-27

`-model_fit NAME=A,B,... [-fit_ridge R]` now defines a named nonnegative ridge
mixture of two or more existing model RDMs. Components may be fixed matrices or
per-location `-model_dset` matrices, but they must share a similarity/distance
sense and the comparison metric must be Pearson. Within every fold, neural and
component training entries are standardized; cyclic coordinate descent solves
`min ||y-Xw||² + R*n_dyad*||w||²` subject to `w >= 0`. The default `R=0.01` is
therefore stable across different training-dyad counts; `R=0` is nonnegative
least squares.

The delivered estimand generalizes over subjects and has a hard leakage
boundary. Classic RSA fits on all condition dyads from the other subjects and
scores the held subject's condition RDM. IS-RSA fits only on training-subject
dyads—every edge touching the held subject is excluded—then scores that held
subject's edges. Fold correlations are Fisher transformed, averaged, and
returned as `_cvR`. Mean fold weights are L1-normalized and exported as
`NAME_w_COMPONENT`; they are descriptive allocation diagnostics rather than
weight-level hypothesis tests.

Inference includes fitting rather than conditioning on observed weights. Each
IS-RSA null draw jointly relabels subjects in every component; each classic-RSA
draw jointly relabels component condition axes. The complete held-subject fit
is repeated for every draw. Each fitted model receives raw p, BH q, and its own
synchronized spatial max-FWE family plus `_cvR`, `_cvZ`, `_cvZFWE`, and weight
maps. The searchlight memory preflight accounts for fitted-model workspaces,
null arrays, result maps, and weight maps.

Five regressions added to the required gate independently reconstruct classic
and IS-RSA nonnegative-ridge folds, verify component weights and held-out
accuracy, inspect AFNI CV/FWE/weight labels, reject rank-metric fitting, and
require exact 1-vs-N-thread output. The complete gate passes **239/239**.

The constrained boundary is intentional. F14 now supplies paired comparisons
between held-out fitted models, and F22 supplies explicit condition-generalizing
folds for classic RSA. Nuisance-aware fitting and bootstrap intervals for
weights or fitted-score differences remain explicit future estimands;
`-model_series` and temporal nulls are rejected rather than silently given a
non-nested approximation. A generic serializable model-object framework remains
out of scope.

### F4. Whitened unbiased RDM comparison  ·  ✅ Resolved 2026-08-27

`-metric corr_cov` and `-metric cosine_cov` now compare balanced runwise
crossnobis RDMs after whitening the covariance among their condition-pair
entries. For the condition-pair contrast matrix `C`, the first delivered
contract uses the Diedrichsen et al. zero-distance approximation

```text
V = (C C') o (C C')
similarity = (d1' V^-1 d2) /
             sqrt((d1' V^-1 d1) (d2' V^-1 d2))
```

`corr_cov` subtracts each compact RDM's ordinary mean before that quadratic
form. `cosine_cov` retains the crossnobis zero and is the whitened unbiased RDM
cosine (WUC). Rather than allocate and invert the `ntri × ntri` covariance in
every worker, 3dRSA uses the exact identity between this quadratic form and the
Frobenius cosine of double-centered second-moment matrices. Each model transform
is cached once per worker and each subject transform once per location; F11
accounts for this memory.

The supported analysis surface is deliberately useful but narrow: fixed model
effects and paired fixed-model contrasts, subject sign-flip inference, BH FDR,
synchronized spatial max-FWE, subject bootstrap, Nili lower/upper ceilings,
atlas ROIs, volumetric searchlights, and `none|diag|shrinkage` voxel-noise
normalization all work. Output provenance records both the selected metric and
the covariance approximation. Model matrices must encode dissimilarities for
the origin-sensitive `cosine_cov`; unlike correlation, an arbitrary similarity
cannot be converted without knowing its scale and zero.

The following remain explicit rejections, not undocumented approximations:

- ordinary same-data classic RSA and IS-RSA outer matrices, which are not the
  unbiased squared-distance estimator this WUC contract targets;
- F21 `ConditionFile` inputs, whose missing/repeated conditions give different
  dyads different run-pair support and therefore a different covariance;
- condition/dual bootstrap, because resampling and duplicate removal alter `V`;
- joint/nuisance regression, commonality, and fitted models, which require a
  separately validated covariance-weighted fitting estimand.

The independent regression fixture forms the dense `V`, solves both quadratic
forms directly, and matches 3dRSA primary effects, paired contrasts, both noise
ceilings, and bootstrap bounds for `corr_cov` and `cosine_cov`. It also verifies
residual voxel-whitening composition, atlas/searchlight equivalence, exact
one-vs-six-thread output, persistent provenance, and every rejection above.
The complete required-dependency suite passes **239/239**.

### F14. Held-out fitted-model comparisons  ·  ✅ Resolved 2026-08-27

The existing `-model_contrast A-B` grammar now also accepts two names created
by `-model_fit`. The reported `A-B_cvDiff` is the mean paired outer-fold
Fisher-z accuracy difference. Because each F7 score is
`tanh(mean(fold Fisher-z))`, the implementation obtains the same estimand as
`atanh(A_cvR)-atanh(B_cvR)` while retaining the fold-paired interpretation.
Both sides use the identical held-subject folds; under F22 they also use the
identical held-condition folds. A mixed fixed-versus-fitted request is rejected
explicitly because an in-sample model association and a held-out prediction
accuracy are not commensurate estimands.

Inference is paired all the way through the nested fit. Under IS-RSA, one
subject-label draw is shared by both models; under classic RSA, one condition
relabeling is shared by both. Both component mixtures are completely refit for
every draw before their signed accuracy difference is taken. Two-sided raw p,
BH q, and an independent synchronized spatial max-FWE family are reported as
`_cvP`, `_cvQ`, and `_cvPfwe`. Dataset output adds `_cvDiff`, `_cvZdiff`, and
`_cvZdiffFWE`; at `-nperm 0`, the second map is the uncalibrated effect-scale
`_cvFZdiff` and is deliberately not FIZT-typed.

The signed fitted-model nulls are retained until all requested F14 contrasts
have consumed them, then converted to absolute values for F7's individual
max-FWE families. Searchlight memory preflight includes the paired-null and
result arrays. Six new regressions independently reproduce classic and IS-RSA
held-fold differences, require valid p≤FWE, inspect AFNI labels, reject mixed
estimands, and require exact one-vs-four-thread results. The complete required
gate, including the later superiority and finite-input additions, passes
**289/289**.

The 2026-08-30 superiority extension makes the scientific null explicit. For
each outer subject, it averages `z_A-z_B` only across held-condition folds that
are valid for both models. An ordinary synchronized subject bootstrap resamples
those paired outer-subject effects, centers every draw as `|d*-d_obs|`, and uses
`(1+exceedances)/(1+draws)` for raw and spatial max-FWE p-values. The models are
fit once within each original leak-free outer fold; the bootstrap does not
pretend to be a second nested refitting scheme. This differs deliberately from
fixed-RDM IS-RSA superiority, where repeated-subject copies require omission of
artificial diagonal dyads. Output provenance names the common-valid-fold
estimand, bootstrap unit, centering, plus-one tail, and synchronized max family.
An independent SplitMix64 bootstrap reference and exact one-vs-six-thread test
cover the new path.

Confidence intervals for a fitted-score difference and for fitted weights
remain separate future estimands; a centered superiority test is not relabeled
as an uncertainty interval.

### F22. Condition-held-out fitted-model CV  ·  ✅ Resolved 2026-08-27

`-fit_condfold FILE` adds strict unseen-stimulus validation to classic-RSA
`-model_fit`. FILE contains one whitespace-free fold label per condition in
model-matrix order; comments and blank lines are ignored. At least two folds are
required, and every fold must contain at least three held conditions while
leaving at least three training conditions. Under F21 mapped runwise input, the
file follows the lexical condition order printed in the output.

For each held subject and held-condition fold, training includes only dyads
between two training conditions from the other subjects. Testing includes only
dyads between two held conditions from the held subject. A dyad crossing the
condition boundary is excluded from both sets. Components and the neural target
are standardized from the training entries only; the existing nonnegative ridge
solver then predicts the untouched held/held dyads. `_cvR` is `tanh(mean z)`
over valid subject × condition folds, and descriptive normalized weights are
averaged over those same folds.

Classic condition-label nulls preserve the fold membership of the neural
condition positions, jointly relabel every component model, and rerun the
complete two-axis fit for each draw. Raw p, BH q, spatial max-FWE, tables, maps,
and F14 fitted-model contrasts therefore retain the existing inference contract.
Ordinary same-data and runwise/crossnobis atlas or volumetric-searchlight inputs
are supported. IS-RSA rejects `-fit_condfold` because its fitted matrix axis is
subjects rather than stimuli; the option cannot recover an already-collapsed
condition axis.

Five F22 checks independently reproduce effects and fold-normalized weights,
verify paired fitted-model differences, inspect provenance and map labels,
exercise malformed/undersized/IS-RSA/no-model rejection, require exact
one-vs-four-thread searchlight output, and reproduce the result after F21
missing/repeated-condition crossnobis mapping. The complete required gate passes
**239/239**.

### F8. Three-predictor commonality  ·  ✅ Resolved 2026-08-27

`-model_commonality A,B,C` extends the existing repeatable pairwise grammar
without changing any `A,B` output. For subset coefficients of determination
`R²_A`, `R²_AB`, and `R²_ABC`, the seven raw regions are

```text
unique A | B,C       = R²_ABC - R²_BC
unique B | A,C       = R²_ABC - R²_AC
unique C | A,B       = R²_ABC - R²_AB
common A,B not C     = R²_AC + R²_BC - R²_C - R²_ABC
common A,C not B     = R²_AB + R²_BC - R²_B - R²_ABC
common B,C not A     = R²_AB + R²_AC - R²_A - R²_ABC
common A,B,C         = R²_A + R²_B + R²_C - R²_AB - R²_AC - R²_BC + R²_ABC
```

They sum to `R²_ABC`; shared regions remain signed and unclipped so suppression
is visible. Three additional effects report the variance remaining after the
two competitors that each added predictor explains:
`partialR²_A|BC = (R²_ABC-R²_BC)/(1-R²_BC)`, with the analogous B and C
quantities. More than three predictors is rejected explicitly rather than
silently truncating the request.

Inference follows the estimand. Each unique/partial pair uses its own
two-predictor reduced fit and Freedman–Lane residual relabeling. The four shared
regions use complete neural-item relabeling because no single reduced model
isolates those signed inclusion/exclusion terms. Each of the ten quantities has
raw p, BH q, and its own synchronized spatial max-FWE family. The same contract
works for IS-RSA and classic RSA, fixed and per-location models, atlas ROIs and
ordinary/runwise/crossnobis searchlights. Classic RSA synchronizes each
condition draw across subjects; optional subject bootstraps cover all ten
effects. Output labels distinguish all seven regions and all three conditional
partial-R² effects.

Seven independent regressions were added: exhaustive IS-RSA and classic null
references, all ten point estimates, p/FWE, missing-dyad and classic subject
bootstraps, atlas/searchlight maps and labels, exact one-vs-four-thread output,
and the over-width parser rejection. The complete required gate passes
**239/239**.

### F20. Time-resolved fusion  ·  ✅ Resolved 2026-08-25

`-model_series LLL` now takes an ordered text list with one
`TIME_LABEL MATRIX_FILE` row per timepoint. A header of `Time ModelFile` or
`Time MatrixFile` is optional; blank/comment lines are ignored; labels must be
unique single tokens; and relative matrix paths resolve from the list file's
directory. Each matrix passes the same square, symmetry, diagonal, dimension,
and finite-value checks as `-model_mat`, and at least two timepoints are
required.

The series uses the established estimator at every timepoint and spatial
location. It supports classic RSA and second-order IS-RSA, ordinary and runwise
neural inputs, atlas and searchlight mapping, and subject/condition bootstrap
where those existing estimands apply. The same immutable relabeling or sign-flip
index is used across the entire sweep. BH FDR is computed over all time × space
cells, and each null draw's maximum is taken over that same joint family before
max-statistic FWE p-values are assigned.

The tabular result is long-form, with `time_index`, the verbatim `time_label`,
and `effect` on every ROI row. Dataset bricks retain the existing statistic/Z/
ZFWE layout and use deterministic safe labels such as `t0000_r`, `t0000_Z`, and
`t0000_ZFWE`; table metadata records the joint multiplicity contract and maps
those indices back to input labels. Plain fixed-series analyses avoid an
O(timepoints × triangle) per-thread regression workspace, while F11 still
accounts for the fixed permutation cache where it is used.

Combinations needing a different time-series statistic are rejected explicitly:
`-model_joint`, `-ortvec`, `-model_contrast`, `-model_commonality`, and `-loo`.
`-model_series` is also mutually exclusive with every single-model source and
`-model_label`. Six new checks independently reconstruct exact sign-flip
statistics and the joint FDR/FWE family, compare atlas with a whole-volume
searchlight, inspect table/brick provenance, verify thread reproducibility, and
exercise malformed/mixed-input rejections. The complete suite passes 239/239.

Two adjacent multimodal boundaries remain visible rather than silently folded
into F20. `-model_dset` requires a source-localized second modality on the main
data's voxel grid; sensor-space or time-frequency RDMs belong in `-model_mat` or
`-model_series`, and the help now says so. S6 supplies the fMRI beta-side
subject×run×trial×condition descriptor and aggregates trials to run-level
condition patterns for crossnobis. Direct trial×trial EEG-feature × fMRI-beta
RSA still needs its own cross-modal alignment and inference contract.
Time-series joint regression, contrasts, commonality,
nuisance adjustment, and LOO likewise need their own statistic contracts; they
are extensions, not missing pieces of the primary fusion analysis delivered
here.

### A4. Smaller audit items  ·  ✅ Completed 2026-08-25

Five independent, individually landable fixes, all now complete with regression
or performance validation. A4a is the only slice intended to alter an inference
family; A4b–A4e preserve reported numeric results.

**A4a. LOO duplicates its statistic and its FWE family.** ✅ Done 2026-08-25.
`-loo` predicts the raw column values, so two models built from the same column
with different rules (`-model beh:nn -model beh:annak`) produce byte-identical
`looR`/`looP`. Models are now grouped by `mod[mm].icol`: each distinct source
column is predicted once per location and owns one max-null family, while every
requested model label retains its `looR/P/Q/Pfwe` table columns and map bricks.
F11 counts distinct families rather than duplicate null arrays, and the table
records output-model versus source-column counts. Regression tests verify four
labeled outputs from one column, exact equality of every LOO quantity, and the
single-family metadata.

**A4b. `-model_contrast` name resolution is first-match, not longest-prefix.**
✅ Done 2026-08-25. The resolver now scans every valid split and retains the
longest model name on the A side, matching its documented contract. A targeted
fixture makes both `a-(b-c)` and `(a-b)-c` valid parses of `a-b-c` and verifies
that `(a-b)-c` supplies the reported contrast.

**A4c. Classic-RSA contrasts recompute every subject RDM a third time.**
✅ Done 2026-08-25. Each location now builds every subject RDM once and retains
only its compact upper triangle. Separate and joint primary fits, Nili ceilings,
condition-bootstrap draws, and every contrast reuse that cache; runwise
crossnobis and residual-covariance whitening therefore run once per subject per
location rather than once per model/contrast consumer. F11 includes the
`nsub × ntri` per-thread cache (plus fixed model triangles for joint fits).
The independent classic, joint, condition-bootstrap, crossnobis, whitening,
contrast, searchlight, and thread regressions all remain green. On a warm OMP1
75-center whole-mask shrinkage-crossnobis searchlight with 8 subjects × 4 runs,
two models, and one contrast, three-run median wall time fell from **5.22 s to
1.95 s (2.68×)** and user CPU from **5.03 s to 1.76 s (2.86×)**, with exact
numeric rows.

**A4d. `THD_roi_pattern` dispatches per (voxel, brick).** ✅ Done 2026-08-25.
The extractor now resolves each brick's data pointer, datum type, and scale
factor once, then walks the selected voxels through a typed pointer. It preserves
`THD_get_voxel` semantics for byte, short, int, float, double, complex, RGB, and
RGBA storage. A mixed byte/short/int/float/double searchlight fixture is exactly
equal to float32 input for effect, p, q, and max-FWE across all centers; the full
suite passes 239/239. On a warm OMP1 256-center, 20-subject × 60-brick pattern
searchlight, a three-run median fell from **0.26 s to 0.24 s wall time (8%)** and
from **0.15 s to 0.13 s user CPU (13%)**, with exact numeric rows. This small
fixture includes startup and I/O, so the measured end-to-end gain—not the former
“large constant-factor” expectation—is the claim carried forward.

**A4e. `THD_noise_whalf`'s Ledoit–Wolf term is O(T·p²) in double precision.**
✅ Done 2026-08-25. Covariance formation now accumulates residual outer products
by row while collecting `sum_t ||r_t||⁴`. The shrinkage numerator then uses
`sum_t ||r_t r_t' - S||_F² = sum_t ||r_t||⁴ - T||S||_F²`, eliminating the
second `T·p²` traversal while preserving the estimator. The help and a runtime
warning now explain that full shrinkage still forms and eigendecomposes a dense
`p × p` matrix per subject/location, recommending that neighborhoods above
roughly 128 voxels be benchmarked or switched to `diag`/smaller ROIs.

On the existing OMP1 whole-mask fixture (75 centers, 8 subjects × 4 runs, 320
residual samples per subject, 75 voxels per neighborhood), the warm three-run
median fell from **1.66 s to 0.58 s wall time (2.86×)** and from **1.49 s to
0.40 s user CPU (3.73×)**. Numeric table rows are byte-identical after excluding
the intentionally time-derived default seed metadata, the independent NumPy
shrinkage/crossnobis references pass, and the complete suite remains 239/239.

---

## Active architecture: a shared map-inference core and 3dMVPA

**Status: ✅ M1 complete (2026-08-29).** The product decision is
made: build `3dMVPA` as a separate program over a deliberately narrow shared
base, one approved submilestone at a time.

### Measured starting point

M1a baseline source counts were `3dRSA.c` 7,392 lines, `1dTrdm.c` 992,
`thd_datatable.c` 342, `thd_patterns.c` 698, `thd_permute.c` 1,625, and
`thd_simmatrix.c` 2,380. The M1a gates are 276/276 `3dRSA` checks, 36/36 focused
`1dTrdm` checks, and the registered RSA CTest.

The reusable portion is meaningful but should not be summarized as a decoder
percentage. `THD_datatable`, ROI/searchlight geometry and extraction, immutable
permutation sets, empirical p-values, and max-null machinery are genuine shared
primitives. Most RDM algebra is RSA-specific. `THD_runset` supplies useful
input lessons and beta descriptors, but it is not the future classifier's
canonical sample container.

### What M1 now means

M1 shares only three bounded groups of behavior:

- BH-FDR, validity-aware BH-FDR, max-null accumulation, and memory-ledger
  arithmetic in new `thd_mapinfer.c/.h`;
- neighborhood parsing, ROI/searchlight painting, and geodesic surface ROI-list
  construction in `thd_patterns.c/.h`; and
- existing table, pattern, and permutation APIs that are already genuinely
  generic.

Percentile/bootstrap summaries, output bricks and `FIZT` typing, top-level
OpenMP loops, program error policy, estimators, and output schemas remain owned
by the programs. M1 is divided into M1a–M1e in
[`3dMVPA_ROADMAP.md`](3dMVPA_ROADMAP.md#m1a-baseline-and-shared-core-contract).

The preservation contract is scientific rather than an accidental pre-release
ABI promise: estimands, nulls, correction families, numeric results, seeds, and
thread identity are hard invariants. Private symbol names, exact diagnostic
wording, and current output ordering may be deliberately improved with their
tests and documentation. M1 is confined to `3dRSA`, `1dTrdm`, the dedicated
RSA/MVPA support modules, their tests, and minimum build/roadmap wiring; it does
not modify unrelated AFNI infrastructure.

### What M2 still needs

1. A distinct `THD_sampleset` that flattens AFNI or `.1D` containers into
   labeled sample × feature matrices without overloading `THD_runset`.
2. Explicit fold construction and leakage validation.
3. A new supervised shrinkage-LDA engine. Existing covariance/whitening code
   may contribute low-level math only after estimator equivalence is proven;
   crossnobis is not itself a trained classifier.
4. Held-out predictions, balanced accuracy, confusion statistics, group
   aggregation, and decoder-specific output/provenance.
5. Full retraining inside every label permutation. Existing immutable
   relabeling sets and max-null reduction are reusable, but the classifier cost
   and scientific null are not obtained "for free."

M1 therefore lowers duplication and risk; it does not make the decoder nearly
implemented. A unified RSA-and-decoding command remains deliberately out of
scope: two scientific engines share infrastructure, not identity.

### M1b delivery record

`thd_mapinfer.c/.h` now supplies plain BH-FDR, validity-masked BH-FDR, and
elementwise max accumulation. `3dRSA` uses it for all spatial/time×space FDR
and max-null reductions; `1dTrdm` uses it for temporal or
time×feature-neighborhood FDR and max-null reduction. The former private BH
implementations and RSA max helper are gone. `3dRSA.c` is now 7,360 lines,
`1dTrdm.c` 980, and the shared implementation 70 lines.

A direct CTest covers ordinary and masked family sizes, tied p-values with
aliased arrays, zero-length safety, and max accumulation. The configured
CMake/SUMA/OpenMP targets build, the unit test passes, and the complete
registered gates remain 276/276 for `3dRSA` and 36/36 for `1dTrdm`. No spatial,
estimator, percentile, output-typing, or loop-scheduling code moved.

### M1c delivery record

`thd_patterns.c/.h` now owns volumetric neighborhood parsing, atlas-parcel
versus searchlight-center result painting, and optional SUMA geodesic surface
ROI-list construction. `3dRSA` retains program-level SUMA initialization and
error policy, consuming bounded diagnostics returned by the shared builders.
The private copies are gone: `3dRSA.c` is now 7,216 lines and
`thd_patterns.c` 879.

A direct CTest covers bare radii, all four neighborhood descriptors, malformed
descriptors, and both painting modes. The existing numeric suite verifies real
ROI, volumetric searchlight, and 25-node SUMA whole-mesh equivalence. The
configured CMake/SUMA/OpenMP build, both shared-unit tests, and the complete
276/276 `3dRSA` plus 36/36 `1dTrdm` gates pass. No estimator, inference-family,
output-typing, percentile, memory, or OpenMP-loop code moved.

### M1d delivery record

`thd_mapinfer.c/.h` now supplies the generic `THD_memory_plan` category ledger
and the exact arithmetic-only total calculation. `3dRSA`'s private memory
container is gone; the program populates `input`, `geometry`, `shared`,
`output`, and per-thread scratch bytes and calls the shared finalizer. Physical-
memory lookup, category estimates, default/explicit limits, warning thresholds,
diagnostics, refusal, and explicit override remain program-owned. `3dRSA.c` is
now 7,207 lines and `thd_mapinfer.c` 77.

The direct unit test verifies the total and that the finalizer does not alter
`system` or `limit`. Existing forced-low-limit refusal and explicit-override
numeric checks pass, as do the configured CMake/SUMA/OpenMP build, both shared
unit tests, and the complete 276/276 `3dRSA` plus 36/36 `1dTrdm` gates. No
scientific, spatial, output, percentile, or loop-scheduling behavior moved.

### M1e delivery record

CMake and legacy Make now list the same program-specific support sources:
`3dRSA` owns spatial patterns/phase support while both programs consume the
table, map-inference, permutation, and similarity modules. `USE_SUMA` and SUMA
linkage remain exclusive to `3dRSA`; OpenMP is conditional for both. The
legacy temporal object dependency now names `thd_permute.h`, and its CTest
bridge requires both program targets.

Three configured CMake corners pass: no SUMA/no OpenMP, OpenMP without SUMA,
and SUMA with OpenMP. Non-SUMA configurations pass 273 RSA cases with only the
three surface checks skipped; the SUMA configuration passes all 276. All 36
`1dTrdm` cases and both shared-unit tests pass throughout. A sequential legacy
GCC 14/OpenMP build also links both programs and passes 273 + 36. Generated
CMake install rules and the package map place exactly one `3dRSA` and one
`1dTrdm` in `corebinaries`. No out-of-ecosystem source was edited.

---

## Companion library structure (for reference)

- **`thd_mapinfer.c`** — plain/masked BH-FDR and elementwise max-null
  accumulation are delivered in M1b; generic memory-ledger arithmetic is
  delivered in M1d.
- **`thd_datatable.c`** — the `-dataTable` reader (no prior C implementation in
  AFNI), plus reusable value-based Cartesian indexing/validation that returns
  original-row mappings without mutating or physically sorting the table.
- **`thd_permute.c`** — the general permutation engine: relabeling schemes/sets,
  exchangeability blocks, Freedman-Lane + generic drivers, max-stat FWE. Stays
  dataset-agnostic (depends only on `matrix.h`); OpenMP, exact-enumeration cap,
  and reentrant Fisher-Yates were added on this line of work.
- **`thd_simmatrix.c`** — similarity/dissimilarity matrices (rules, triangles,
  metrics) *and* the RDM permutation inference (Mantel, Freedman-Lane over
  triangles, LOO, sign-flip) that consumes `thd_permute`'s `PERM_set`. The former
  `thd_mantel.c` was folded in here and deleted.
- **`thd_patterns.c`** — dataset → feature vectors and ROI/searchlight
  geometry, including shared parsing, painting, and SUMA surface construction.

### Engineering lessons retained

- Changing the *source* of relabelings cannot change observed
  `r`/`beta`/`partial_r`; exact matching of observed effects is a strong refactor
  safety check, while p-values need calibration checks.
- RDM algebra belongs in `thd_simmatrix`; `thd_permute` stays dataset-agnostic.
- `THD_simmat_to_tri_perm`, `THD_tri_to_simmat`, `THD_tri_corr`, and
  `THD_rank_avg` are shared primitives — do not duplicate them.
