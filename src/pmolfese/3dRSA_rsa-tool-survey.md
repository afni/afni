# Survey of RSA (and IS-RSA) Tools for fMRI

Context: dartbrains.org's [RSA page](https://dartbrains.org/RSA/) and naturalistic-data.org's [IS-RSA page](https://naturalistic-data.org/content/Intersubject_RSA.html) both build on **nltools** (Luke Chang lab). Below is a broader landscape of RSA-capable tools, matched to features worth stealing/replicating.

> **Historical baseline (2026-07-30).** The package comparisons remain useful,
> but section 0 records the implementation state at the time of the survey and
> is intentionally not the live tracker. For current code status, completed
> gaps, open defects, and next steps, use
> [`RSA_ROADMAP.md`](RSA_ROADMAP.md), reconciled through 2026-08-29.
>
> **Current re-audit (2026-08-29):** the ranked competitor comparison after the
> large August feature set is in
> [`3dRSA_competitor-audit-2026-08-29.md`](3dRSA_competitor-audit-2026-08-29.md).

---

## 0. Where 3dRSA stands against this survey

| Survey capability | 3dRSA status |
|---|---|
| ROI and volume searchlight RSA / IS-RSA | **Done.** ROI means or multivoxel patterns; AFNI neighborhood grammar; streamed searchlights. |
| Surface data and surface searchlights | **Done with a build distinction.** Surface atlas/mask analysis works in the plain build; geodesic searchlights use `SUMA_getoffsets2` in the optional `SUMA=1` build. |
| Neural similarities/distances | **Done:** Pearson, Spearman, cosine, Euclidean. |
| Scalar behavioral models | **Done:** Anna Karenina, nearest-neighbor, raw Euclidean similarity, raw absolute difference. |
| Multivariate behavioral profiles | **Partial:** standardized Euclidean is done; covariance-aware Mahalanobis remains. |
| Explicit/model-file RDMs | **Partial contract.** Reads square 1D matrices, but does not yet validate finiteness or symmetry before using the upper triangle. |
| Cross-modal per-ROI models | **Done for atlas/ROI analysis;** `-model_dset` remains unsupported in searchlights. |
| Multiple models / nuisance adjustment | **Done:** standardized joint regression, partial correlations, pair-space `-ortvec`, and Freedman-Lane residual relabeling. |
| Non-parametric inference | **Done for current statistics:** synchronized item relabeling for IS-RSA, subject sign flips for classic RSA, exchangeability blocks for IS-RSA, BH FDR, and per-model max-stat FWE. |
| Model comparison metrics | **Done:** Pearson, Spearman, Kendall tau-a, Kendall tau-b. Kendall metrics are intentionally excluded from least-squares joint/nuisance paths. |
| Noise ceiling / reliability | **Partial:** Nili-style leave-one-subject-out lower and group-mean upper bounds for classic RSA; split-half/interleaved subject-geometry reliability for IS-RSA. The IS-RSA value is a reliability diagnostic, not a formal upper/lower model-performance ceiling, and it is not yet written as a searchlight map brick. |
| LOO / out-of-sample readout | **Partial:** scalar behavior can be predicted from rank-weighted neural similarity, with permutation p/FDR. The current distance-metric path weights farther subjects more heavily and needs correction. The method is a useful predictive diagnostic, not a fitted rsatoolbox-style model, and it lacks max-stat FWE. |
| Cross-validated unbiased distances | **Not done:** no run/chunk-aware input, crossnobis, or noise-covariance whitening. |
| Bootstrap / stimulus-level inference | **Not done.** Current inference is subject/item relabeling plus sign flipping; no subject/stimulus bootstrap or signed-rank model-comparison layer. |
| Model objects with fitted/free parameters | **Not done.** Models are fixed matrices or fixed linear predictors. |
| Bayesian/generative group RSA | **Not done; deliberately deferred.** |
| Visualization | **In transition.** 3dRSA emits matrices/tables and suggested commands. Histogram, heatmap, and scatter helpers are implemented in open AFNI PR [#919](https://github.com/afni/afni/pull/919), not merged as of 2026-07-30; conversion of `3dRSA_plots.py` to an AFNI-only orchestrator remains. |
| Automated validation | **Partial.** Synthetic generators cover planted/null, joint, and classic scenarios, but there is not yet one repeatable test runner with numeric assertions, edge cases, and dependencies. |

### Gap matrix: features other tools provide that 3dRSA does not yet fully provide

Legend: ✅ = implemented; 🟨 = partial or adjacent capability, but not an
equivalent implementation; ❌ = not implemented; — = support is not established
by this survey or is outside the package's main scope. The marks summarize the
capabilities documented below; they are not an exhaustive certification of
every package API.

#### Statistical and modeling gaps

| Feature | 3dRSA | rsatoolbox | RSA Toolbox | PyMVPA | CoSMoMVPA | BrainIAK | nltools | PCM |
|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Run/chunk-aware inputs | ❌ | ✅ | — | ✅ | ✅ | — | — | — |
| Crossnobis / cross-validated Mahalanobis | ❌ | ✅ | — | — | — | 🟨 | — | 🟨 |
| Residual noise-covariance estimation and whitening | ❌ | ✅ | — | — | — | ✅ | — | 🟨 |
| Free-parameter or fitted model objects | ❌ | ✅ | — | — | — | ✅ | — | ✅ |
| Subject-level bootstrap inference | ❌ | ✅ | ✅ | — | — | ✅ | ✅ | — |
| Stimulus/condition-level bootstrap inference | ❌ | ✅ | ✅ | — | — | — | — | — |
| Signed-rank model-comparison inference | ❌ | ✅ | ✅ | — | — | — | — | — |
| Whitened, unbiased RDM comparison | ❌ | ✅ | — | — | — | — | — | 🟨 |
| Bayesian/generative group RSA | ❌ | — | — | — | — | ✅ | — | 🟨 |
| Pooled shared group representational structure | ❌ | — | — | — | — | ✅ | — | — |
| Circular-shift / phase-randomization time-series nulls | ❌ | — | — | — | — | ✅ | 🟨 | — |
| Repeated-subject diagonal handling during bootstrap | ❌ | — | — | — | — | ✅ | ✅ | — |
| Mahalanobis option for multivariate distances | 🟨 | ✅ | — | — | — | 🟨 | — | 🟨 |
| Broad, extensible distance library (for example Poisson) | 🟨 | ✅ | — | — | — | — | ✅ | — |

The strongest scientific gap is the first three rows as a unit: 3dRSA cannot
yet carry run/chunk metadata through the analysis, estimate residual covariance,
or produce cross-validated unbiased distances. The strongest inference gap is
bootstrap/signed-rank support, followed by time-series-specific null models for
IS-RSA inputs derived from continuous data.

#### Workflow and ecosystem gaps

| Feature | 3dRSA | rsatoolbox | RSA Toolbox | PyMVPA | CoSMoMVPA | nltools | NeuroRA | BrainVoyager | TDT |
|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| User-facing Dataset/RDM object with metadata | ❌ | ✅ | 🟨 | ✅ | ✅ | ✅ | 🟨 | — | 🟨 |
| Built-in RDM or model-result visualization | 🟨 | ✅ | ✅ | — | — | ✅ | — | ✅ | — |
| Unified RSA and decoding pipeline | ❌ | — | — | ✅ | — | — | 🟨 | — | ✅ |
| One analysis API across fMRI, surface, and M/EEG-like data | 🟨 | — | — | — | ✅ | — | ✅ | — | — |
| Reliability as a searchlight output map | 🟨 | — | — | ✅ | ✅ | — | — | — | — |
| Cross-modal model data in searchlights | 🟨 | — | — | 🟨 | ✅ | — | 🟨 | — | — |
| Graph/network conversion for dyadic matrices | ❌ | — | — | — | — | ✅ | — | — | — |
| No-code GUI for basic first-/second-level RSA | ❌ | — | — | — | — | — | — | ✅ | — |
| Shared run/chunk cross-validation infrastructure | ❌ | ✅ | — | ✅ | ✅ | — | — | — | ✅ |
| Plug-in measure architecture shared by RSA and other maps | 🟨 | — | — | ✅ | ✅ | — | — | — | ✅ |

Here, 3dRSA's most consequential architectural gap is the lack of a first-class
dataset/RDM/model layer carrying labels, runs, and feature metadata. Its largest
usability gaps are complete bundled plotting, a reliability searchlight brick,
and searchlight support for `-model_dset`. A GUI, graph conversion, and a unified
decoding pipeline are useful ecosystem ideas, but they are lower priority than
statistical correctness and validation.

### What the audit says about correctness

The implemented inference follows the key RSA requirements in the references:
only one matrix triangle is analyzed, item permutations relabel rows and columns
together, dyads are never treated as independent observations, joint tests use
reduced-model residual relabeling, and FWE maxima are synchronized across
locations. The 2026-07-30 audit found open edge cases in constant-regression
max-null output, distance-direction LOO weights, no-permutation z typing, block
semantics in classic RSA, and asymmetric matrix input; these are tracked in
`RSA_ROADMAP.md`.

The highest-value next statistical addition is still **run/chunk-aware
crossnobis with residual-based covariance whitening**. The highest-value
engineering addition is an **automated end-to-end numeric test runner**.

---

## 1. Standard (within-subject) RSA tools

| Tool | Language | Core approach | Notable features worth replicating | Maintenance / notes |
|---|---|---|---|---|
| **rsatoolbox** (formerly "pyrsa", successor to the 2014 Matlab RSA Toolbox) | Python | `Dataset` → `RDM` → `Model` → inference objects | Clean object model: Dataset, RDMs (supports stacks of RDMs, cross-validated distances), a **Model** abstraction with `.predict()` + `.fit()` so models can have free parameters (fixed, selection, weighted, interpolation models); rich **inference module** for model comparison with noise ceilings, bootstrap/permutation tests, signed-rank tests, RDM comparison methods (cosine, corr, whitened unbiased distance — Diedrichsen et al. 2021); built-in visualization (`show_rdm`, MDS plots, model comparison bar+arrow plots); flexible distance measures incl. crossnobis/cross-validated Mahalanobis, Poisson, whitened distances; noise-covariance estimation from GLM residuals. | Actively developed (2025 eLife/bioRxiv paper), rsagroup/rsatoolbox on GitHub. **This is probably the single best reference for "gold-standard" RSA statistics** (noise ceilings, model comparison, RDM inference). |
| **RSA Toolbox (Matlab, Nili et al. 2014)** | MATLAB | Original toolbox from the Kriegeskorte lab (PLOS Comp Bio 2014) | Established RDM computation, MDS visualization, RDM correlation (Spearman/Kendall tau-a), permutation & bootstrap significance tests, "second-order" RSA (comparing RDMs to RDMs) | Superseded by rsatoolbox (Python) but still widely cited/used; good source for the *statistical logic* (e.g., recommending Kendall's tau-a for categorical model RDMs). |
| **PyMVPA** | Python | `rsa` module inside a general MVPA framework | `PDist` (compute dissimilarity matrices), `PDistConsistency` (split-half reliability of RDMs across chunks/runs — useful "noise ceiling" style check), `PDistTargetSimilarity` (searchlight correlating local RDM to a target/model RDM), tight integration with searchlight (`sphere_searchlight`) and `ChainLearner`/`TransposeMapper` for combining measures | Mature but less actively developed; good searchlight-RSA implementation pattern. |
| **CoSMoMVPA** | MATLAB/Octave | "measures" applied over ROI or searchlight neighborhoods | Treats fMRI & M/EEG data as first-class equally (same RSA code runs on volumetric, surface-based, or MEEG data); `cosmo_target_dsm_corr_measure` for searchlight RSA against a target dissimilarity matrix; surface-based searchlight support (via AFNI/FreeSurfer); RSA **between-participant reliability searchlight**; simple lightweight dataset struct that's easy to reason about | Good model for surface-based / cross-modality generalization of RSA code. |
| **BrainIAK — (Group) Bayesian RSA (BRSA/GBRSA)** | Python | Generative/Bayesian model of the RDM rather than "estimate patterns then correlate" | Models the **covariance structure of condition-evoked patterns directly via a generative model**, marginalizing out voxel-wise amplitudes/noise — shown to reduce the bias that traditional two-step RSA introduces (Cai et al. 2016/2019); Group-BRSA learns a **shared representational structure across participants**; can use the learned structure as an empirical prior for decoding new data; explicit spatial noise-correlation modeling; utilities to build design matrices from AFNI/FSL timing files | Worth replicating if you care about **bias correction and pooling across subjects into one RDM estimate** rather than averaging subject-level RDMs post hoc. |
| **NeuroRA** | Python | Cross-modality RSA toolbox | Single toolbox spanning EEG/MEG/fNIRS/sEEG/ECoG/fMRI/behavior; includes Neural Pattern Similarity (NPS), Spatiotemporal Pattern Similarity (STPS), RSA, and ISC in one API; explicit demo comparing decoding-based vs. RSA-based approaches | Useful if your "we" want one API across modalities, not fMRI-only. |
| **BrainVoyager (built-in RSA module)** | GUI (commercial) | Point-and-click, no coding | First-level dialog: computes RDMs per ROI directly from condition-specific volume maps + VOI ROI files; Second-level dialog: correlates RDMs from different ROIs/subjects/modalities/models against each other with no extra scripting | Good UX reference if you want a **GUI-driven, ROI-file-based workflow** rather than a scripting API — lowers the barrier for non-programmers. |
| **Nilearn** | Python | No dedicated RSA module, but provides the building blocks | `nilearn.decoding.SearchLight` infrastructure can be repurposed with a custom scoring function for RSA; general-purpose GLM (first-level modeling of beta/t-maps that feed RSA), plus plotting utilities | Not RSA-specific, but many labs build RSA searchlights on top of it because it's the standard fMRI-in-Python data-loading/GLM layer. |
| **PCM Toolbox** (Pattern Component Modeling, Diedrichsen lab) | MATLAB / Python | Related family: models second-moment matrices (like RDMs but in similarity, not distance, space) via explicit generative component models | Explicit **model-comparison via marginal likelihood** rather than only RDM correlation; combines multiple RDM/G-matrix components with free weights, similar spirit to rsatoolbox's flexible models but with a more principled likelihood-based fitting procedure; noise-ceiling computation | Good if "replicate" means going beyond correlating RDMs toward proper model-fitting/comparison with likelihoods. |
| **The Decoding Toolbox (TDT)** | MATLAB | MVPA/decoding suite with an RSA option | Searchlight- and ROI-based RSA integrated into the same pipeline as decoding accuracy maps, shared cross-validation infrastructure | Useful if a lab wants RSA and decoding accuracy maps produced from one unified pipeline/config file. |

---

## 2. Inter-Subject RSA (IS-RSA) tools

| Tool | Language | Core approach | Notable features worth replicating |
|---|---|---|---|
| **nltools** (`Adjacency` class) — used in the naturalistic-data.org & dartbrains tutorials | Python | Build a subject × subject similarity/distance matrix per measure (behavior, brain), then relate the two matrices | `Adjacency` object wraps pairwise matrices and supports `.similarity()`, `.distance()`, `.plot()`, conversion to/from `networkx` graphs; supports multiple **distance metrics** (correlation-based similarity via `1 - corr`, Euclidean, and others via scikit-learn's `pairwise_distances`, chosen because it's faster than `np.corrcoef` and supports many metrics); explicit teaching material on the two IS-RSA "models" — **Nearest-Neighbor/AnnaK model** (similarity driven by high scorers on some trait) vs. **distance-based model** (similarity driven by trait-similarity per se) — with guidance on which distance metric implies which assumption; supports whole-brain, ROI, and searchlight IS-RSA; **FDR correction** across ROIs; permutation-based null (needed because IS-RSA dyads aren't independent, so parametric stats are invalid) | Actively maintained (Chang/Jolly/Cheong labs); this is the most directly relevant reference given your links. |
| **BrainIAK** (ISC/ISFC modules) | Python | Not literally IS-RSA but the underlying inter-subject correlation/connectivity machinery | Fast, MPI/optimized ISC and Inter-Subject Functional Connectivity (ISFC); **subject-level bootstrapping with proper handling of the diagonal/duplicate resampling artifact** (both BrainIAK and nltools convert repeated-resample diagonal values to NaN before computing summary stats) — a subtle correctness detail worth replicating exactly; permutation/circular-shift/phase-randomization null models for time series (used for generating valid nulls for dyadic measures) | Good reference for the **null-model machinery** (circular shift, phase randomization) that IS-RSA also needs since dyads violate independence assumptions. |
| **NeuroRA** | Python | Has a dedicated ISC module alongside RSA/NPS/STPS | Puts ISC and RSA under one shared API/object model, so extending to "IS-RSA" (their RSA class + ISC) is straightforward in the same toolbox | Same-toolbox convenience if unifying within- and between-subject analyses is a priority. |
| **Custom pipelines in the IS-RSA literature** (Finn et al. 2020; Chen et al. 2020) | Python/MATLAB (bespoke) | Same nltools-style dyadic-matrix approach, applied at whole-brain, ROI, or searchlight resolution | Papers explicitly compare **AnnaK/nearest-neighbor vs. distance models**, discuss when multivariate behavioral profiles (not just scalar summary scores) give more precise dyadic predictors, and use Mantel-test-style permutation for significance | Good conceptual/statistical reference even without a distinct "toolbox" — worth mirroring the model-choice framework, not just the code. |

---

## 3. Cross-cutting features seen across tools (a checklist for "what to replicate")

**Data structures**
- A clean `Dataset`/pattern container that keeps track of condition labels, runs/chunks, and voxel/feature indices (rsatoolbox, PyMVPA, CoSMoMVPA).
- An `RDM`/`Adjacency`-style object that wraps a distance matrix with metadata and built-in plotting/conversion methods, rather than passing raw matrices around (rsatoolbox `RDMs`, nltools `Adjacency`).

**Distance/similarity computation**
- Multiple distance metrics beyond Pearson correlation: Euclidean, Mahalanobis, cross-validated ("crossnobis"), Poisson (rsatoolbox); scikit-learn `pairwise_distances` for speed/flexibility (nltools).
- Noise-covariance estimation from GLM residuals to whiten patterns before distance computation (rsatoolbox, BrainIAK BRSA).
- Cross-validated / unbiased distance estimators to avoid the "positive bias" that naive RSA has when reusing the same data to estimate and compare patterns (rsatoolbox, BrainIAK BRSA).

**Model comparison / inference**
- Model objects with free parameters and a `.fit()` method, not just fixed model RDMs (rsatoolbox, PCM Toolbox).
- Noise ceilings (upper/lower bound on how well any model could do given data reliability) (rsatoolbox, RSA Toolbox).
- Proper non-parametric inference: bootstrap over stimuli/subjects, permutation tests, signed-rank tests — because RDM entries and IS-RSA dyads are non-independent (rsatoolbox, nltools, BrainIAK).
- RDM-RDM comparison metrics matched to data type (Kendall's tau-a for categorical models, whitened unbiased distance for continuous, cosine similarity) (RSA Toolbox, rsatoolbox, Diedrichsen et al. 2021).

**Searchlight / spatial mapping**
- Sphere and surface-based searchlights with a plug-in "measure" function so RSA, decoding, and reliability searchlights share the same spatial-iteration code (PyMVPA, CoSMoMVPA, TDT).
- Split-half / between-run RDM reliability maps, i.e. testing how stable a local RDM is before trusting it (PyMVPA `PDistConsistency`, CoSMoMVPA reliability searchlight).

**IS-RSA-specific**
- Explicit support for both the **AnnaK (nearest-neighbor) model** and the **distance/similarity model**, since the choice of distance metric encodes a substantive hypothesis about how a trait relates to brain similarity (nltools/naturalistic-data.org tutorial).
- Multivariate behavioral distance (not just scalar-summary Euclidean distance) as a richer dyadic predictor (Chen et al. 2020 erotic-movie IS-RSA paper).
- Correct handling of bootstrap resampling artifacts on the diagonal (NaN-out repeated-subject self-pairs) (nltools, BrainIAK).
- Valid null generation for time-series-derived similarity (circular shift, phase randomization) reused from the ISC literature (BrainIAK, naturalistic-data.org).

**Usability / group-level**
- A GUI path with no scripting required for basic first-/second-level RSA (BrainVoyager) — worth considering if end users include non-programmers.
- Group-level pooling of RDMs into one shared-structure estimate rather than post-hoc averaging of subject RDMs (BrainIAK GBRSA).

---

## Suggested next steps

1. Turn the existing synthetic generators and the audit edge cases into one
   repeatable runner with numeric assertions for observed effects, empirical
   p-value calibration, block restrictions, max-stat FWE, and thread
   reproducibility.
2. Specify a run/chunk-aware input extension, then implement crossnobis and
   noise-covariance whitening. This closes the largest statistical gap with
   rsatoolbox/PyMVPA for classic fMRI RSA.
3. Precompute fixed model relabelings for searchlights and add
   `-model_dset` streaming plus a reliability output brick.
4. After AFNI PR #919 merges, finish the AFNI-only plotting orchestration.
