# MVPA/RSA Refactoring and Future-Program Plan

## Purpose and status

This document records opportunities to make `3dRSA` smaller and easier to
maintain, identifies support modules that could serve more than one AFNI
program, and collects candidate programs that could grow from the RSA/MVPA
infrastructure.

**Status: proposed, not started.** M1a--M1e are complete. The R1 sequence below
is a possible refactoring boundary before decoder-specific 3dMVPA Stage 0, but
neither R1 nor Stage 0 begins without explicit approval.

The existing ecosystem confinement remains in force: work belongs in `3dRSA`,
`1dTrdm`, `3dMVPA`, their dedicated `thd_*` support modules, their tests, and
the minimum build/roadmap wiring. Unrelated AFNI source is not part of this
plan.

Last audited against the source tree on **2026-08-29**.

---

## Headline recommendation

There is substantial room to shrink `3dRSA.c`, but the next changes should
mostly be **private decomposition**, not premature public abstraction.

The immediate goal is a top-level `3dRSA.c` of roughly 2,000--3,000 lines that
contains orchestration and the location loop. Total source lines may stay flat
or grow slightly; the gains are bounded ownership, direct unit testing, and
less fragile option and output bookkeeping.

Only mechanisms with multiple credible consumers should become `thd_*`
modules. RSA estimands, model fitting, bootstrap interpretation, diagnostics,
and output contracts stay RSA-owned unless a second real program proves an
identical contract.

---

## Measured anatomy of `3dRSA.c`

`3dRSA.c` currently contains 7,207 lines. Its approximate responsibility map
is:

| Responsibility | Approximate lines | Current region |
|---|---:|---:|
| Types, fitted-model helpers, and memory preflight | 736 | 1--736 |
| Help text | 1,259 | 737--1,995 |
| RSA construction, temporal inference, and bootstraps | 1,197 | 1,996--3,192 |
| Parsing, validation, and input preparation | 1,648 | 3,193--4,840 |
| Main ROI/searchlight computation | 1,418 | 4,841--6,258 |
| FDR/FWE finalization | 144 | 6,259--6,402 |
| Text-table output | 357 | 6,403--6,759 |
| AFNI dataset output | 350 | 6,760--7,109 |
| Plot suggestions and cleanup | 98 | 7,110--7,207 |

This is not one monolithic scientific algorithm. The largest contributors are
help, configuration, validation, result-family bookkeeping, output assembly,
and several distinct RSA estimators sharing one translation unit.

---

## R1: private `3dRSA` decomposition

### R1a -- extract program help

Create `3dRSA_help.c/.h` and move `usage_3dRSA()` into it.

Expected effects:

- remove about 1,260 lines from `3dRSA.c` immediately;
- isolate the long introduction, option reference, examples, compile date, and
  citations;
- make help-only changes less likely to disturb runtime code; and
- preserve byte-for-byte or intentionally reviewed help output.

This should remain program-private. AFNI help text is too program-specific to
justify a generic help library.

### R1b -- introduce an RSA configuration object

Create `rsa_options.c/.h` around an opaque or bounded `RSA_options` object:

```c
typedef struct RSA_options RSA_options;

RSA_options *RSA_options_parse(int argc, char **argv);
int          RSA_options_validate(RSA_options *opt);
void         RSA_options_free(RSA_options *opt);
```

Move the following responsibilities out of `main()`:

- CLI parsing and defaults;
- model, contrast, commonality, fitted-model, and nuisance specifications;
- cross-option validation;
- resolution of the analysis mode, feature interpretation, null, and output
  requests; and
- a printable resolved-analysis description for provenance.

This could remove roughly 1,000--1,500 lines and replace the very large group
of top-level local variables with a structured configuration. Validation tests
must preserve scientific defaults and must deliberately review diagnostic
ordering: parser refactors can otherwise change which invalid combination is
reported first.

### R1c -- introduce a private result schema

The current table and AFNI-brick writers independently reproduce offsets for
primary effects, LOO, FWE, contrasts, commonality, ceilings, bootstrap bounds,
fitted models, weights, and fitted contrasts. Replace that arithmetic with a
declarative RSA-owned catalog, for example:

```c
typedef struct {
    char  *label;
    int    value_kind;
    int    stat_kind;
    float *values;
} RSA_result_column;
```

Proposed private files:

- `rsa_results.c/.h` -- result families, labels, availability, ordering, and
  offsets;
- `rsa_output_table.c` -- long-form and ordinary text output; and
- `rsa_output_dset.c` -- AFNI brick allocation, painting, labels, and statistic
  typing.

This should remove roughly 700--900 lines from the top-level source and, more
importantly, eliminate duplicated table-column and brick-offset bookkeeping.
The catalog must remain explicit about plain-float versus statistical bricks
and about which p/q/FWE families actually ran.

It remains RSA-private until `3dMVPA` supplies a second concrete output schema.

### R1d -- separate private scientific engines

The pre-`main()` helpers divide naturally into several cohesive modules:

| Proposed file | RSA-owned responsibility |
|---|---|
| `rsa_fit.c/.h` | Nonnegative ridge fitting, subject-held-out and condition-held-out fits, fitted contrasts, and reusable fit workspaces. |
| `rsa_bootstrap.c/.h` | Subject bootstrap summaries, grouped condition resampling, dual-bootstrap variance, and commonality/LOO intervals. |
| `rsa_temporal.c/.h` | Model-series input, circular-shift and phase evaluation, and time-resolved inference. |
| `rsa_neural.c/.h` | Ordinary/runwise subject RDM construction, second-order RDM construction, seed representational geometry, and RSA-specific whitening composition. |

These modules could remove another 1,500--2,000 lines from `3dRSA.c`. Their
interfaces should use caller-owned workspaces where practical, avoid hidden
OpenMP state, and expose bounded failure results rather than embedding new
program-level policy.

They are not `thd_*` modules: their estimands and failure contracts are RSA
specific even when they use generic matrices, folds, or resampling sets.

### R1e -- reconcile and verify

Repeat the M1 integration matrix after all private moves:

- CMake without OpenMP or SUMA;
- CMake with OpenMP and without SUMA;
- CMake with OpenMP and SUMA;
- sequential legacy Make;
- install/package and CTest registration; and
- complete `3dRSA`, `1dTrdm`, and shared-unit gates.

Observed statistics, p/q/FWE values, bootstrap intervals, brick labels/types,
seed behavior, and thread identity are invariants. Private symbol names and
intentionally improved diagnostics are not accidental compatibility promises.

---

## Shared support modules with credible reuse

In this document, "shared library" normally means a compiled-in AFNI `thd_*`
support module, not a new runtime `.so` boundary.

### Priority summary

| Priority | Proposed component | Likely consumers | Timing |
|---:|---|---|---|
| 1 | `thd_samples.c/.h` | `3dMVPA`, encoding/PCM programs, tabular RDM construction | Build for 3dMVPA |
| 2 | `thd_folds.c/.h` | `3dMVPA`, encoding models, cross-domain generalization | Build for 3dMVPA |
| 3 | Richer `thd_mapinfer.c/.h` inference-family container | `3dRSA`, `1dTrdm`, `3dMVPA`, future mapped tests | Design after concrete result schemas are compared |
| 4 | `thd_covariance.c/.h` | Shrinkage LDA, crossnobis whitening, encoding models, PCM | Extract while implementing the decoder |
| 5 | `thd_resample_summary.c/.h` | RSA bootstraps, decoder uncertainty, future programs | Only after a second estimand matches |
| 6 | `thd_resultmap.c/.h` | Standard labeled ROI/searchlight datasets | Only after 3dMVPA output exists |

### `thd_samples.c/.h`

This is the most important missing shared abstraction. A `THD_sampleset`
should describe a rectangular sample x feature dataset with explicit:

- sample IDs;
- subject IDs;
- run/partition IDs;
- categorical labels or continuous targets;
- arbitrary retained metadata;
- finite/missing-value state; and
- AFNI-dataset or generic `.1D` source information.

It must not inherit `THD_runset`'s condition-RDM assumptions. `THD_runset`
organizes condition patterns for crossvalidated distances; a classifier or
encoding model organizes independent labeled observations. Shared loaders may
learn from the existing trial descriptors without pretending that these are
the same scientific object.

### `thd_folds.c/.h`

Fold construction and leakage validation should be estimator-independent. A
bounded fold descriptor could support:

- leave-one-run-out;
- leave-one-subject-out;
- grouped K-fold;
- explicitly saved folds;
- cross-domain train/test partitions; and
- checks for empty classes, duplicated membership, and forbidden leakage.

`3dRSA`'s private condition-fold logic might eventually consume a low-level
label-to-fold parser. Its dyad inclusion and train/train versus held/held rules
remain RSA-specific.

### Extend `thd_mapinfer.c/.h` with an inference-family container

The delivered module contains BH-FDR, validity-aware BH-FDR, elementwise
max-null accumulation, and memory-ledger arithmetic. Programs still manage
large parallel arrays for observed values, p, q, signed z, FWE p/z, validity,
and max-null values.

A possible `THD_infer_family` would explicitly own or bind:

- number of effects and locations;
- one- or two-sided tail convention;
- valid-location mask;
- FDR family shape;
- max-null family shape; and
- signed-z conversion.

It must not calculate the program's statistic or infer a scientific family
from array shape. `3dRSA`, `1dTrdm`, and `3dMVPA` are already credible
consumers, but their concrete result layouts should be compared before freezing
the API.

### `thd_covariance.c/.h`

The broadly reusable portion of `thd_simmatrix` is lower-level covariance and
stable factorization, not RDM algebra. A focused numeric module could provide:

- pooled covariance calculation;
- diagonal or identity-target shrinkage;
- eigenvalue flooring and effective-rank diagnostics;
- inverse and inverse-square-root factors; and
- allocation-free factor application through caller workspaces.

This would support shrinkage LDA, crossnobis whitening, encoding models, and
pattern-component models. Extraction should happen while implementing
`3dMVPA`, when direct references can prove that the supervised pooled-
covariance estimator is unchanged. Crossnobis whitening is not automatically
the same estimator as LDA covariance.

### `thd_resample_summary.c/.h`

A small array-level module might eventually own:

- finite-value filtering;
- linear-interpolated quantiles;
- percentile intervals;
- sample variance; and
- possibly paired interval summaries.

It must not encode RSA's subject, condition, dual-bootstrap, or fixed-out-of-
sample interpretations. AFNI already has image-oriented quantile functions;
new code should not duplicate them without establishing why a caller-owned
array interface has a different useful contract.

### `thd_resultmap.c/.h`

After `3dMVPA` has a working output catalog, compare its needs with RSA. A
shared declarative map writer could potentially accept:

- a master dataset and `THD_roilist`;
- one value vector and label per brick;
- plain-float or AFNI-statistic typing metadata;
- output-domain and validity information; and
- program-supplied history/provenance.

Do not extract this merely because both programs write AFNI datasets. Shared
mechanics are worthwhile only if label, typing, collision, sparse-surface, and
history contracts actually match.

---

## Abstractions to avoid

The following would make the architecture more opaque or claim reuse before a
stable contract exists:

- a generic callback-based searchlight executor;
- a universal AFNI CLI parser;
- a broad miscellaneous statistics utility;
- a generic output writer before `3dMVPA` supplies a second schema;
- moving RSA bootstrap estimands or fitted-RDM models into `thd_permute`;
- placing supervised classifiers in `thd_simmatrix`; or
- combining RSA and decoding inside one command.

The current decision to keep each program's OpenMP-over-location loop remains
sound. RSA and decoding have different workspace, retraining, invalid-location,
and output behavior. Geometry and inference reductions can be shared without
hiding the scientific loop behind callbacks.

---

## Candidate future AFNI programs

### 1. `3dMVPA` -- first priority

`3dMVPA` remains the clearest next program and the first real consumer of the
new sample, fold, covariance, and result-map boundaries. Its initial purpose is
leakage-safe, run-aware linear classification in atlas ROIs, volumetric
searchlights, and generic 1D feature matrices, with held-out predictions and
permutation inference.

It should coexist with the historical `3dsvm`. The new program's identity is a
modern validated sample/fold contract, ROI/searchlight inference, and one
shared decoder for AFNI datasets and generic feature matrices--not merely a new
front end for the older SVM program.

### 2. `1dRdm` -- static companion to `1dTrdm`

`1dRdm` would fill the non-temporal tabular gap. It could:

- read labeled observation x feature data;
- aggregate repeated observations by subject, condition, and partition;
- construct ordinary or crossvalidated RDMs;
- write subject matrices and conditioning/QC summaries;
- compare matrices without requiring spatial datasets; and
- write matrices directly consumable by `3dRSA -model_mat`.

The intended product family would be:

- `1dRdm` -- static tabular representational geometry;
- `1dTrdm` -- time-resolved representational geometry; and
- `3dRSA` -- spatial and group representational inference.

This is cleaner than overloading `1dTrdm` for inputs with no meaningful time
axis.

### 3. `3dPCM` or `3dPatternModel`

A pattern-component modeling program would complement RSA by evaluating
representational second-moment models with a likelihood-based framework rather
than only RDM correlations. It could reuse sample descriptors, ROI/searchlight
geometry, covariance/factorization, folds, permutation planning, and map
inference.

This is scientifically attractive but substantially harder to specify and
validate than `3dMVPA`; it should follow a stable shared sample/covariance core.

### 4. `3dEncode`

A crossvalidated encoding-model program could model stimulus or design features
to voxel/ROI responses with:

- ridge regression;
- fixed or nested regularization;
- held-out prediction;
- feature-group variance partitioning; and
- voxelwise, ROI, or searchlight accuracy maps.

It would reuse `THD_sampleset`, folds, covariance/linear algebra, permutations,
and result maps. Its help must distinguish crossvalidated representational
encoding from time-series model fitting in `3dDeconvolve` and `3dREMLfit`.

### 5. Temporal decoding as a `3dMVPA` stage

Time-resolved decoding and temporal-generalization matrices are better treated
as later `3dMVPA` modes than as another executable. `1dTrdm` already supplies
useful time-axis, feature-axis, and neighborhood conventions. A future decoder
can add:

- one score per time point;
- train-time x test-time generalization matrices;
- channel or feature neighborhoods; and
- synchronized correction over time, neighborhood, or their joint family.

### Programs not recommended yet

- **Generic `3dSearchlight`:** insufficient scientific contract and excessive
  callback machinery.
- **Separate representational-connectivity program:** `3dRSA` already owns seed
  representational connectivity coherently.
- **Generic `1dPermute`:** the reusable inference library is more valuable than
  a command with an underspecified statistic.
- **`3dHyperalign`:** potentially valuable, but it is a much larger functional-
  alignment product and should wait until cross-subject 3dMVPA is stable.
- **Separate cross-decoding command:** cross-task/domain generalization should
  first be a validated `3dMVPA` stage, not another binary.

---

## Dependency and execution order

```text
R1 private 3dRSA decomposition
  |
  +-- R1a help
  +-- R1b options/configuration
  +-- R1c result schema/output
  +-- R1d RSA scientific modules
  `-- R1e complete integration gate

3dMVPA Stage 0: freeze scientific and CLI contract
  |
  +-- thd_samples
  +-- thd_folds
  +-- thd_decode
  `-- covariance extraction only where estimator equivalence is proven

Second concrete program/output consumer
  |
  +-- consider richer thd_mapinfer family objects
  +-- consider thd_resultmap
  `-- consider bounded resample summaries
```

R1 and 3dMVPA Stage 0 are separate approval boundaries. R1 reduces maintenance
risk; it does not settle decoder estimands. Stage 0 settles decoder contracts;
it should not become an excuse to generalize every RSA mechanism.

---

## Refactoring acceptance gates

Every R1 submilestone should satisfy gates proportional to its responsibility.
R1e repeats the complete matrix.

1. **Scientific identity:** observed effects, tests, correction families,
   intervals, null semantics, and invalid-case decisions are unchanged unless
   an intentional contract change is separately approved and tested.
2. **Seed/thread identity:** fixed seeds remain byte-identical across supported
   OpenMP thread counts.
3. **Output identity:** table schemas, AFNI brick labels/order/types, model-series
   naming, and surface node mapping remain unchanged unless intentionally
   revised with fixtures.
4. **Help and provenance:** moved text remains complete; compile date and
   resolved analysis choices remain visible.
5. **Direct units:** new private boundaries receive allocation, error, and
   reference tests rather than relying only on end-to-end coverage.
6. **Build matrix:** plain/OpenMP and non-SUMA/SUMA CMake corners plus sequential
   legacy Make continue to link the correct program-specific sources.
7. **Packaging:** exactly one installed `3dRSA` and `1dTrdm` remain in
   `corebinaries`; future programs are added only at their release milestone.
8. **Confinement:** no unrelated AFNI source is changed to make a local
   refactor convenient.

---

## Decision rules for future shared code

Promote a private helper into a `thd_*` module only when all of the following
are true:

1. at least two concrete programs need the same representation or operation;
2. the scientific meaning does not depend on one program's options;
3. ownership, allocation, tail conventions, missingness, and error behavior can
   be documented without referring to a caller's globals;
4. the API can be tested directly against an independent reference;
5. the abstraction reduces duplicated policy rather than merely moving lines;
   and
6. it does not require a hidden callback framework or hidden OpenMP state.

These rules preserve the useful architecture already established by
`thd_datatable`, `thd_patterns`, `thd_permute`, `thd_simmatrix`, and
`thd_mapinfer`: share data structures and trustworthy mechanisms while keeping
each program's estimand, scheduling, diagnostics, and product identity clear.

