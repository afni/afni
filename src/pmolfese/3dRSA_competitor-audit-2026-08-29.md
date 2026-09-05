# 3dRSA competitor audit, 2026-08-29

> **Dated prioritization snapshot.** The gap statements below describe what was
> missing when this audit was conducted. S1–S10 subsequently delivered the
> condition-null, centering, temporal, cross-temporal, and feature-neighborhood
> work; recommendation cells note those outcomes. Use
> [`RSA_ROADMAP.md`](RSA_ROADMAP.md) for current status.

## Bottom line

3dRSA is no longer missing a foundational group fMRI-RSA capability. It now
covers ROI and volume/surface searchlights, ordinary and runwise crossnobis
RDMs, residual noise whitening, subject/condition/dual bootstrap, noise
ceilings, fixed and fitted model comparison, nuisance regression, commonality,
second-order IS-RSA, temporal nulls, and time-resolved RDM fusion. In several of
those combinations its synchronized spatial max-FWE and IS-RSA inference are
broader than the general-purpose packages reviewed here.

The audit did find **two major remaining scientific/workflow gaps**:

1. a condition-label fixed-effects test for classic RSA, including a useful
   single-subject analysis; and
2. native temporal/spatiotemporal neural data handling beyond importing an
   already-computed `-model_series`.

A third item, partition-wise condition-mean removal, is smaller to implement but
important as a preprocessing safeguard for correlation/cosine RDMs. None of
these invalidates the current group crossnobis path. There is no P0 correctness
defect in the implemented estimator/inference contracts identified by this
comparison.

## Method

The comparison used the current 3dRSA source/help and the 239-check numeric gate
as the baseline, not the historical 2026-07-30 survey. Eight software families
were checked: **rsatoolbox**, the MATLAB **RSA Toolbox**, **PyMVPA**,
**CoSMoMVPA**, **TDT**, **BrainIAK**, **NeuroRA**, and **nltools**. Nilearn and
PCM are discussed where they define an important workflow or modeling boundary,
but are not counted as full RSA competitors.

"Software frequency" below means explicit or directly composable support among
those eight families, not downloads, citations, or publication prevalence:

- **very common:** at least 5/8;
- **common:** 3–4/8;
- **recurring:** 2/8;
- **specialist:** 1/8.

Counts are deliberately conservative. A generic array operation or a method
that could be custom-coded is not counted as a packaged RSA feature. Scientific
criticalness combines frequency, consequences of omission, fit to an AFNI fMRI
program, and use in notable methods/applications. A frequent convenience can
therefore rank below a less frequent validity feature.

## Ranked gaps

| Rank | Missing or materially partial feature | Software frequency | Criticalness | Recommendation |
|:---:|---|:---:|:---:|---|
| **1** | **Classic-RSA condition-label fixed-effects inference / single-subject inference.** Primary classic effects currently use subject sign flips or signed rank. The condition-relabeling machinery exists for fitted models and commonality, but there is no user-selectable primary fixed-effects null. | **Common (3–4/8).** Explicit in the MATLAB RSA Toolbox and rsatoolbox; PyMVPA and CoSMoMVPA provide compatible permutation/null-distribution machinery. | **P1.** This is the clearest conventional RSA workflow 3dRSA still cannot express. It matters for single-subject studies, rare cohorts, within-subject replication, and deliberately fixed stimulus/participant sets. It must be labeled fixed effects: it does not generalize to a population of subjects. | Add something explicit such as `-classic_null subjects\|conditions`, defaulting to the current population-level subject test. Reuse one synchronized row+column condition permutation across subjects and locations for spatial max-FWE. Do not silently switch nulls when `nsub=1`. |
| **2** | **Native temporal/spatiotemporal RSA and temporal generalization.** `-model_series` consumes an external RDM movie and performs strong time×space inference, while `1dTrdm` creates labeled subject RDM movies, owns model inference across time or time×explicit-feature-neighborhood families, and estimates two distinct symmetric cross-temporal RSA products globally or within overlapping neighborhoods. Directional decoding generalization and native coordinate-derived sensor/source graphs remain open. | **Common (4/8):** rsatoolbox temporal datasets/RDM movies, CoSMoMVPA multidimensional M/EEG neighborhoods and time generalization, NeuroRA temporal and cross-temporal RSA, and TDT time-resolved designs. | **P1/P2 boundary.** Major for M/EEG, intracranial data, and dynamic representational studies; only P2 for a deliberately fMRI-centered AFNI command because F20 already accepts externally estimated time-resolved RDMs. Cichy et al.'s MEG–fMRI fusion is a notable high-impact use. | **S7–S10 delivered:** `1dTrdm` estimates correlation, cosine, Euclidean, or balanced crossnobis subject RDM movies with labeled axes/windows, a guarded independent-sample `3dRSA -model_series` bridge, fixed-model Pearson/Spearman inference using population sign flips or synchronized condition relabeling with joint time or time×neighborhood BH/max-FWE, Pearson/Spearman RDM-dynamics recurrence, ordered-partition cross-time crossnobis with exact diagonal reduction, and a strict overlapping `Neighborhood Feature` graph applied to all temporal products. Directional decoding remains separate. See [`3dRSA_temporal-product-decision-2026-08-29.md`](3dRSA_temporal-product-decision-2026-08-29.md). |
| **3** | **Condition-mean removal (cocktail-blank / re-meaning) and explicit pattern preprocessing provenance.** There is no option to subtract a subject's mean pattern across conditions before ordinary correlation/cosine RDM construction. When run partitions exist, methods guidance recommends doing this within each partition. | **Recurring (2/8 explicit):** CoSMoMVPA and PyMVPA expose `center_data`; the operation is also recommended in RSA methods guidance. | **P1 for ordinary correlation/cosine RDMs; low for Euclidean/crossnobis.** Shared baselines can distort angle-based RDMs. Euclidean and crossnobis condition contrasts already cancel a common within-run pattern, so this should not be presented as a repair to those paths. | Add `-center_conditions none\|subject` for ordinary input (name negotiable), record it in table metadata, and test correlation/cosine against independent centered references. A later trial/run-aware input can center within partitions. Keep the default unchanged for compatibility, but warn or explain when ordinary angle metrics are used without it. |
| **4** | **Complete covariance-aware RDM comparison beyond the balanced fixed-model contract.** `corr_cov`/`cosine_cov` reject F21 unequal pair support, condition bootstrap, regression/commonality, and fitted models. | **Specialist (1/8 direct):** rsatoolbox is the main packaged reference; PCM is adjacent. | **P1/P2.** Low frequency but high scientific value because it completes 3dRSA's flagship crossnobis path and follows Diedrichsen et al.'s efficiency argument. The hard part is methodological: unequal run support changes the distance covariance, and regression needs a defensible generalized least-squares contract. | Prioritize a derivation for F21 unequal-support covariance before API work. Treat covariance-weighted fixed regression as a separate second slice. Continue rejecting unsupported combinations rather than falling back silently. |
| **5** | **Representational connectivity / all-to-all comparison among brain-region RDMs.** `-save_rdm` enables this externally and `-model_dset` compares modalities at the same location, but 3dRSA cannot directly produce ROI×ROI similarity networks or seed-RDM searchlights. | **Common (3/8 directly or through first-class RDM stacks):** MATLAB RSA Toolbox, rsatoolbox, and nltools; PyMVPA can compose a target-RDM searchlight. | **P2.** Representational connectivity was part of RSA's original stated scope and remains used, but exported RDMs make this a workflow gap rather than a blocked scientific analysis. A whole all-to-all searchlight would also have a large output and multiplicity contract. | Start with a bounded **seed-RDM/seed-ROI → atlas or searchlight map** mode, with synchronized inference. Keep all-to-all network analysis external unless there is a concrete AFNI use case. |
| **6** | **Trial-level/beta-series input and first-level model integration.** 3dRSA now consumes explicitly nested already-estimated trial betas, but deliberately does not estimate trialwise responses, build trial×trial RDMs, or read/refit a design and residual model directly. | **Recurring (2/8 direct/adjacent):** TDT has SPM/AFNI design and residual workflows, and BrainIAK BRSA fits time series and designs. Nilearn supplies the common Python GLM layer but is outside the eight-package count. | **P2.** Trialwise RSA is widespread, but fitting the first-level GLM inside 3dRSA would duplicate mature AFNI programs and greatly expand validation. | **S6 delivered 2026-08-29:** a `TrialFile` per subject×run maps beta sub-bricks to unique trial IDs and conditions; trials are averaged within run before the existing crossnobis estimator. GLM estimation remains in `3dDeconvolve`/`3dREMLfit`/`3dLSS`; trial×trial RDM inference remains a separate future contract. |
| **7** | **Cluster/TFCE inference over spatial or temporal neighborhoods.** 3dRSA has exact synchronized max-stat FWE, but no cluster-mass, cluster-size, or TFCE statistic. | **Recurring (2/8):** CoSMoMVPA directly supports Monte Carlo cluster and TFCE over its neighborhood spaces; NeuroRA exposes temporal/spatial statistical helpers. | **P2/P3.** Cluster/TFCE can be more sensitive to extended effects, especially time×space maps, but max-stat FWE is already valid and avoids a cluster-forming-threshold choice. AFNI also has downstream spatial tools, although they do not automatically reuse 3dRSA's exact permutation family. | Only add if real analyses show max-stat power is limiting. Reuse the shared permutation maps and require an explicit adjacency definition; do not bolt a parametric cluster correction onto RSA p maps. |
| **8** | **Linear-discriminant t / cross-validated MANOVA-style RDM entries.** Crossnobis supplies unbiased noise-normalized distances, but not LD-t's pair-specific standard-error normalization or cvMANOVA effect summaries. | **Recurring (2/8):** MATLAB RSA Toolbox and TDT. | **P3.** Scientifically legitimate and useful for pairwise discriminability, but crossnobis is the more common modern RDM estimator and already covers the main unbiased-distance need. Nili et al. introduced LD-t as a bridge between decoding and RSA. | Prefer interoperability or a later estimator plug-in after the shared map-inference refactor. Do not call LD-t a metric distance; negatives are meaningful. |
| **9** | **Partial/missing model-RDM support.** F21 allows missing observations while estimating neural crossnobis RDMs, but an input `-model_mat` must be complete and finite; there is no pair mask or condition-label alignment for partially overlapping RDMs. | **Specialist (1/8 direct):** rsatoolbox has `from_partials`, missing-data RDM operations, and label descriptors. | **P3.** Valuable for large naturalistic stimulus sets and cross-study/model comparisons, but pairwise missingness complicates synchronized permutation, regression, ceilings, and covariance weighting. | Consider a labeled long-form RDM input plus an explicit intersection policy. Reject per-draw changing samples unless the inferential contract is worked out. |
| **10** | **Bundled RDM/MDS/model-result visualization.** Current output and hints are usable, but the AFNI-only plotting consumer remains blocked. | **Very common (5+/8).** rsatoolbox, MATLAB RSA Toolbox, CoSMoMVPA, nltools, and NeuroRA make visualization a first-class workflow; BrainVoyager does too outside the counted set. | **P3 scientific / P1 usability.** It prevents fewer scientific errors than the items above, but inspecting RDMs is one of the best defenses against wrong condition order, sign, or model construction. The original RSA papers foreground RDM and MDS displays. | Keep F12 as release/usability work. At minimum ship robust heatmaps, labeled model-vs-neural scatterplots, model-performance/noise-ceiling plots, and MDS from saved RDMs once the AFNI plotting dependency lands. |

## Important specialist capabilities that should remain out of core

These are real methods, but their presence in a notable publication or package
does not make them missing switches in 3dRSA:

| Capability | Frequency | Assessment |
|---|:---:|---|
| **Bayesian/generative RSA and pooled shared structure (BRSA/GBRSA)** | Specialist (BrainIAK) | High-impact alternative estimator, not feature parity. It needs likelihoods, priors, nuisance time-series modeling, optimization, and convergence diagnostics. Export to BrainIAK remains the right boundary. |
| **Pattern Component Modeling / marginal-likelihood model comparison** | Specialist (PCM, adjacent to the eight) | A distinct generative framework. Do not reproduce it inside an AFNI C command. |
| **Fully generic model-object framework** | Specialist/direct in rsatoolbox, adjacent in PCM | 3dRSA's constrained nonnegative fitted component model plus held-subject and held-condition validation covers the most defensible high-value slice. A serializable plug-in model API is disproportionate scope. |
| **Broad arbitrary distance catalog (Poisson, Jaccard, Bures, etc.)** | Common as a software convenience | Most are not justified for fMRI beta patterns. `-model_mat` and exported RDMs are adequate escape hatches; new in-core distances should require an explicit noise model and inference contract. |
| **Topological RSA** | Emerging/specialist | Promising research method, not yet a field-standard software omission. Reassess if it appears in maintained general RSA packages and replicated applications. |

## What is already unusually strong in 3dRSA

Feature counting understates 3dRSA's position. The following combinations were
not found together in another audited package:

- classic RSA and IS-RSA in one AFNI-native ROI, volume-searchlight, and
  surface-searchlight program;
- balanced or missing/repeated-condition crossnobis with residual-derived
  diagonal/full shrinkage whitening;
- subject, grouped-condition, and corrected dual-axis bootstrap with map output;
- synchronized max-stat FWE over space, and over joint time×space for
  `-model_series`;
- fixed/per-location model contrasts, two- and three-predictor commonality with
  statistic-specific nulls, and held-subject/held-condition fitted mixtures;
- circular-shift and phase-randomized continuous IS-RSA nulls in ROIs and
  searchlights; and
- second-order IS-RSA over ordinary or crossnobis within-subject RDMs.

That is why the recommendation is **not** to pursue blanket parity. Add the
classic condition-null and centering safeguard, decide whether native temporal
RSA belongs in this program, then choose from the bounded P2 items based on
real AFNI user demand.

## Suggested execution order

1. **A5: classic primary condition-label null** — about 3–6 engineering days;
   much of the permutation and max-FWE infrastructure already exists.
2. **A6: subject/partition-wise condition centering** — about 2–4 days including help,
   provenance, ordinary/runwise invariance tests, and searchlight coverage.
3. **Method note for unequal-support covariance comparison** — derive and
   validate before scheduling code; this is a research task, not merely an
   implementation task.
4. **Product decision on native temporal RSA** — either specify a companion
   temporal program/data layer or formally retain `-model_series` as the
   interoperability boundary.
5. **Completed 2026-08-29:** the selected demand-driven workflow was seed
   representational connectivity. `-seed_mask` now supports ordinary and
   runwise/crossnobis classic RSA plus pattern/second-order IS-RSA in atlas and
   searchlight analyses, with non-overlap enforcement and synchronized
   inference. Plotting remains separately deferred.
6. **Completed 2026-08-29:** `TrialFile` descriptors now provide explicit
   subject×run×trial×condition nesting for already-estimated beta series while
   preserving condition-level crossnobis inference. First-level GLM fitting and
   trial×trial RDM inference remain intentionally outside this slice.

## Sources checked

- [rsatoolbox stable documentation](https://rsatoolbox.readthedocs.io/en/stable/)
  and its [temporal RSA demo](https://rsatoolbox.readthedocs.io/en/latest/demo_temporal.html)
- [Nili et al. 2014, MATLAB RSA Toolbox](https://doi.org/10.1371/journal.pcbi.1003553)
- [PyMVPA RSA measures](https://www.pymvpa.org/generated/mvpa2.measures.rsa.html)
  and [`PDistTargetSimilarity.center_data`](https://www.pymvpa.org/generated/mvpa2.measures.rsa.PDistTargetSimilarity.html)
- [CoSMoMVPA documentation](https://cosmomvpa.org/documentation.html),
  [`cosmo_target_dsm_corr_measure`](https://cosmomvpa.org/matlab/cosmo_target_dsm_corr_measure.html),
  and its [re-meaning guidance](https://cosmomvpa.org/faq.html#why-should-i-consider-re-meaning-when-doing-representational-similarity-analysis-rsa)
- [The Decoding Toolbox](https://sites.google.com/site/tdtdecodingtoolbox/)
- [BrainIAK BRSA/GBRSA documentation](https://brainiak.org/docs/examples/brsa/brsa_demo.html)
- [NeuroRA documentation](https://neurora.github.io/documentation/index.html)
  and [cross-temporal RSA](https://neurora.github.io/documentation/ctrsa.html)
- [Kriegeskorte et al. 2008, original RSA framework](https://doi.org/10.3389/neuro.06.004.2008)
- [Diedrichsen & Kriegeskorte 2017, representational models](https://doi.org/10.1371/journal.pcbi.1005508)
- [Diedrichsen et al., whitened unbiased RDM similarity](https://arxiv.org/abs/2007.02789)
- [Cichy et al. 2016, MEG-fMRI RSA fusion](https://pmc.ncbi.nlm.nih.gov/articles/PMC4961022/)
