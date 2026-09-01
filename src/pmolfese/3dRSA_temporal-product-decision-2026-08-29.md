# Native temporal RSA product decision, 2026-08-29

## Decision

Build native observation×feature×time RDM estimation and cross-temporal RSA as
a **companion AFNI program/data layer**, named `1dTrdm` (temporal RDM).
Keep `3dRSA -model_series` as the stable fixed-RDM-series consumer and
time×AFNI-space inference engine. Do not teach `3dRSA` to reinterpret its
fMRI-oriented `InputFile` column as trial×channel×time data, and do not require
AFNI to read vendor M/EEG formats.

The decision itself did not authorize implementation; subsequent user requests
did. The labeled RDM-movie producer, temporal fixed-model inference, two
separate cross-temporal estimators, and explicit feature neighborhoods are now
delivered.

The selected `1dTrdm` name abbreviates temporal RDM. The `1d` prefix conveys that the
scientific feature axis is not necessarily a voxel grid: it may be sensors,
electrodes, source vertices, frequency bins, or preselected features.

## Why this boundary

`3dRSA -model_series` already owns a coherent downstream contract:

- its input is an ordered `TIME_LABEL MATRIX_FILE` list of complete fixed RDMs;
- every RDM has the same item axis and passes the existing matrix validation;
- one synchronized relabeling/sign-flip set drives the complete analysis; and
- BH FDR and max-statistic FWE cover the joint time×ROI/searchlight family.

Native temporal estimation introduces different objects and validation rules:
repeated observations nested in subject, condition, and partition; a numeric
time axis; a generic feature axis; time windows; time-varying missingness;
feature neighborhoods; and, for cross-temporal work, two distinct time axes.
Those are not extra columns on the current fMRI table. They are a new data
model.

This separation also matches established practice. rsatoolbox represents
observation×channel×time data as a `TemporalDataset` and produces an RDM movie
with an explicit time descriptor and optional bins. CoSMoMVPA's temporal
generalization examples produce train-time×test-time results from labeled
channel×time datasets. NeuroRA separately exposes dynamic RDM-series comparison
and cross-temporal condition-RDM arrays. The architecture must keep these two
ideas distinct: a cross-temporal RSA surface is not automatically a decoding
train/test generalization surface.

The scientific demand is substantial. Time-resolved MEG–fMRI RSA is central to
Cichy et al.'s fusion work, while King and Dehaene formalized temporal
generalization as an analysis of when learned representations generalize. That
publication record supports a companion product, but it does not justify
turning a volumetric group program into a raw M/EEG package.

## Versioned input and axis contract

The companion's first contract should be versioned and label-first. An
unlabeled dense tensor is not a sufficient scientific input.

### Observation table

One row represents one independently estimated trial or pattern observation:

```text
Subj  Observation  Condition  Partition  InputFile
s01   tr0001       face01     run1       s01_tr0001.1D
s01   tr0002       face02     run1       s01_tr0002.1D
```

- `Subj`, `Observation`, `Condition`, and `InputFile` are required.
- `Observation` must be unique within subject.
- `Partition` is optional for ordinary RDMs and required for crossvalidated
  estimators. It denotes an independent run/fold, not an arbitrary trial
  counter.
- Each `InputFile` is **time rows × feature columns**. Every file in the first
  contract has the same shape and finite values.
- Trial estimation and preprocessing remain upstream. AFNI programs such as
  `3dDeconvolve`/`3dREMLfit` may produce fMRI estimates; MNE, EEGLAB, FieldTrip,
  or equivalent software may produce sensor/source estimates. The companion
  does not duplicate those systems or ingest FIF/SET/vendor raw formats.

### Time axis

A separate time-axis table supplies one row per input row:

```text
time_index  time_value  time_unit  time_label
0           -0.100      s          -100ms
1            0.000      s             0ms
```

`time_index` is zero-based and contiguous; `time_value` is finite and strictly
increasing; one unit applies to the complete input; labels are unique. The
axis length must equal the row count of every observation file.

The default movie uses one sample per output time. Any temporal window has an
explicit width, step, and reduction rule. Window boundaries and their member
samples are written to provenance; there is no implicit smoothing or
resampling.

### Feature axis

A separate feature table supplies one row per input column and at minimum a
unique `feature_label`. Optional type and coordinate columns may identify
sensors, electrodes, source vertices, frequency bins, or other features. The
first RDM-movie slice uses all selected features as one pattern. A later
feature-searchlight slice must receive an explicit neighborhood graph or a
documented coordinate-to-neighborhood rule; column adjacency is never assumed.

Frequency is a feature descriptor in the initial contract, not a silently
promoted fourth tensor axis. A later time×frequency search family must declare
that axis and its multiplicity explicitly.

### Condition and partition validation

The initial crossvalidated contract is deliberately balanced: every subject
must contain every condition in at least two independent partitions, with a
common condition order after label alignment. Ordinary RDMs may have unequal
trial counts, but the count per subject×condition is reported. Nonfinite cells,
ambiguous labels, changing feature sets, and unsupported missing partitions are
rejected rather than pairwise-deleted.

## Estimator contract

For each subject and output time/window:

1. reduce samples within the requested time window;
2. average observations within subject×condition for an ordinary RDM, or within
   subject×partition×condition for a crossvalidated RDM;
3. apply any explicitly selected condition re-meaning policy at that same
   nesting level; and
4. construct the condition RDM with the established `thd_simmatrix` kernels.

The first estimator set should be correlation distance, cosine distance,
Euclidean distance, and crossnobis. Crossnobis requires the partition contract.
The unnormalized form uses identity feature covariance; noise normalization
must identify whether a supplied covariance/whitener is common across time or
time-specific. It must not estimate a different covariance from an
undocumented mixture of test data at every latency.

The primary product is a **subject×time×condition-dyad RDM movie** with full axis
labels and estimator provenance. A human-readable long table is canonical for
checking labels; a compact binary representation may be added for scale, but it
must carry the same versioned axes.

## Cross-temporal contract

Two products must not share one ambiguous `cross_temporal` switch:

1. **RDM-dynamics similarity:** compare the dyad vector at `time_a` with the
   dyad vector at `time_b`. This yields a subject×time_a×time_b second-order
   similarity surface. It answers whether representational geometry recurs.
2. **Cross-time crossvalidated distance:** combine a condition contrast at
   `time_a` in one partition with the same contrast at `time_b` in an
   independent partition, averaged over valid ordered partition pairs. This
   preserves a condition-dyad axis and yields
   subject×time_a×time_b×dyad values.

Neither product is a classifier trained at one time and tested at another.
Directional decoding should remain a separately named analysis. For the
within-modality balanced RSA products above, the time surface is symmetric;
inference searches the diagonal plus one triangle and mirrors the result for
display, so duplicated cells do not inflate the correction family.

Cross-temporal estimation followed the ordinary RDM movie, labels, partition
validation, and round-trip interchange. Its public switches remain separate:
`-rdm_dynamics pearson|spearman` and `-cross_time_crossnobis`.

## Inference contract

The companion owns inference over temporal/generic-feature axes; `3dRSA` owns
inference over AFNI ROI/searchlight space when consuming a fixed model series.

- **Population effect:** compute each subject's model fit at each searched cell,
  Fisher-transform correlation-like fits, and use synchronized subject sign
  flips. The tested population is subjects.
- **Fixed condition effect:** one condition permutation relabels RDM rows and
  columns identically for every subject and every searched cell. The tested
  population is the fixed observed subject/condition sample and is labeled as
  such.
- **Multiplicity:** the command declares the complete searched family before
  computation: time; unique time×time cells; time×feature-neighborhood; or
  unique time×time×feature-neighborhood cells. One permutation draw supplies
  one maximum across that entire family. BH FDR uses the same declared cells.
- **Output:** raw effect, raw p, BH q, and max-FWE p retain all axis labels. The
  randomization seed, number/type of draws, window definition, and family size
  are recorded.
- **Not in the initial product:** cluster mass, TFCE, silent per-latency
  correction, pairwise deletion, and a parametric fallback when the requested
  permutation set is unavailable.

Temporal autocorrelation does not by itself require a separate correction when
the null draw is synchronized and its maximum is taken over the complete time
family. It does matter for any future time-series shift null, which needs an
explicit continuous-segment contract rather than trial labels reused as if
they were a continuous recording.

## Interchange with 3dRSA

`1dTrdm` writes the ordered list and per-time `.1D` matrices already
accepted by `3dRSA -model_series`, plus a sidecar containing the versioned time,
condition, estimator, and aggregation provenance. This is a one-way fixed-model
bridge, not a promise that all temporal inputs become native `3dRSA` inputs.

The primary companion output remains subject-level. Aggregating it into one
fixed model series is valid when the temporal RDM is an external/fixed model or
comes from an independent sample. If it is estimated from the same subjects
whose fMRI RDMs are tested, a group mean can create self-inclusion and does not
inherit `3dRSA`'s ordinary fixed-model population interpretation. Same-subject
fusion therefore requires either leave-one-subject model movies or a future
subject-indexed series contract; the existing `-model_series` must not imply
that this dependence vanished.

`3dRSA` should continue to reject time-series joint regression, commonality,
contrasts, fitted mixtures, nuisance adjustment, and LOO until each has a
declared statistic and joint temporal multiplicity family. The companion does
not make those estimands automatic.

## Measured reuse estimate

The direct callable reuse inventory is reproducible with:

```text
python3 src/pmolfese/tests/measure_temporal_reuse.py
```

Against the 2026-08-29 source it finds:

| Existing library | Directly reusable function bodies | Body LOC |
|---|---:|---:|
| `thd_simmatrix.c` | 30 | 747 |
| `thd_permute.c` | 15 | 325 |
| **Total** | **45** | **1,072** |

That is 27.3% of the 3,925 physical lines in those two implementation files.
The inventory conservatively includes complete callable bodies for RDM
construction, crossnobis/noise normalization, triangle comparison, cached
Mantel fits, sign-flip/signed-rank tests, permutation schemes, resampling, and
permutation-result conversion. It excludes headers and anything that first
needs extraction from `3dRSA.c`.

Reuse is scientifically meaningful but does not make this a small flag. The
implementation-responsibility inventory is:

| Responsibility | Reuse assessment |
|---|---|
| RDM metrics, crossnobis, whitening, RDM comparison | Direct library reuse |
| Relabeling/sign-flip sets and p/Z conversion | Direct library reuse |
| BH and joint maximum-family mechanics | Small extraction/refactor from `3dRSA` |
| Ordered `-model_series` list reader | Existing consumer; add a companion writer |
| Observation manifest and nested-label validation | New |
| Time/feature axis objects and window provenance | New |
| Trial/partition aggregation and temporal tensor storage | New |
| Cross-time crossvalidated estimator | New |
| Generic feature-neighborhood graph | New, later slice |
| Temporal/feature output container and family orchestration | New |
| FIF/SET/vendor acquisition and preprocessing | Explicitly external |

Thus the numerical kernels are reusable, but most product-level code is new
axis, I/O, validation, and orchestration work. A separate executable can link
the 1,072 measured LOC without copying them and avoids coupling this new data
model to the 6,903-line `3dRSA.c` application driver.

## Alternatives rejected

### Put everything in `3dRSA`

Rejected. It would overload `InputFile`, mix generic sensor/electrode axes with
AFNI voxel-grid assumptions, expand a monolithic driver, and make time-only
inference look like a special case of spatial fMRI inference. Kernel reuse does
not require executable-level coupling.

### Keep native temporal RSA permanently external

Rejected as the long-term product decision. The workflow is common across
major RSA/MVPA packages and prominent applications, and AFNI already has the
core RDM and permutation kernels. External preprocessing remains desirable,
but forcing every user to reconstruct labels, windows, estimators, and temporal
correction independently leaves a material reproducibility gap.

### Implement cross-temporal analysis first

Rejected. It would establish a complex two-time-axis output before the
single-time RDM movie, partition validation, and interchange semantics are
tested. The single-time movie is the necessary first release gate.

## Implementation release gates

1. **RDM-movie producer — complete 2026-08-29:** independent NumPy references for every metric;
   shuffled-row/label alignment; balanced partition rejection; window boundary
   tests; one-versus-many-thread identity; complete provenance; and exact
   `1dTrdm` → `3dRSA -model_series` round trip on an independent/fixed model.
2. **Temporal inference — complete 2026-08-29:** exhaustive small subject/condition null references;
   max-FWE over the declared time family; BH reference; and no-result changes
   when merely changing the output layout.
3. **Cross-temporal estimator — complete 2026-08-29:** independent partition-pair formula; symmetry
   and diagonal reductions; unique-triangle family size; and a synthetic
   transient-versus-sustained representation example.
4. **Feature neighborhoods — complete 2026-08-29:** explicit graph validation,
   brute-force neighborhood references, and joint time×neighborhood max-FWE;
   time×time×neighborhood products remain descriptive.

All four gates are implemented as `1dTrdm`, with correlation, cosine, Euclidean,
and balanced crossnobis estimators; mean or concatenated windows; explicit
time/condition/feature/count/provenance outputs; deterministic row alignment and
OpenMP execution; and the guarded independent-sample bridge. Its registered
CTest passes 36/36 focused assertions. These include the live `3dRSA` round
trip plus exhaustive subject-sign and synchronized-condition inference,
BH/max-FWE references, sampled seed/thread/output-layout identity, independent
Pearson/Spearman RDM-dynamics references, the ordered partition-pair cross-time
formula, symmetry, exact diagonal reduction, unique-triangle size, and a planted
recurrence example.
The user-facing help additionally explains the EEG/MEG use case, lays out the
output family as a pseudo-table, demonstrates the guarded handoff to `3dRSA`,
and prints the AFNI compile date at its footer.
The feature gate adds a strict `Neighborhood Feature` membership graph,
overlapping ordinary/crossnobis references, neighborhood-local dynamics and
cross-time distances, and synchronized inference over the complete
time×neighborhood family. Cross-temporal neighborhood surfaces remain
descriptive, so a time×time×neighborhood inferential family is not implied.

## Primary references checked

- [rsatoolbox temporal RDM calculation source](https://rsatoolbox.readthedocs.io/en/stable/_modules/rsatoolbox/rdm/calc.html)
  and [temporal RSA demo](https://rsatoolbox.readthedocs.io/en/latest/demo_temporal.html)
- [rsatoolbox MNE/MEG temporal-dataset demo](https://rsatoolbox.readthedocs.io/en/latest/demo_meg_mne.html)
- [CoSMoMVPA M/EEG time-generalization example](https://www.cosmomvpa.org/ex_meeg_time_generalization.html)
- [NeuroRA cross-temporal RSA documentation](https://neurora.github.io/documentation/ctrsa.html)
- [Cichy et al. 2014, resolving object recognition in space and time](https://pmc.ncbi.nlm.nih.gov/articles/PMC4261693/)
- [Cichy et al. 2016, comparison of deep neural networks to MEG and fMRI](https://pmc.ncbi.nlm.nih.gov/articles/PMC4961022/)
- [King and Dehaene 2014, temporal generalization method](https://pmc.ncbi.nlm.nih.gov/articles/PMC5635958/)
