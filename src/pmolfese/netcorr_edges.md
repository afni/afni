# Edge time-series support in `3dNetCorr`

## Goal

Add optional edge time-series (ETS) outputs to `3dNetCorr` while preserving its existing ROI extraction, correlation calculations, output formats, and default behavior.

For standardized ROI time series \(z_i(t)\) and \(z_j(t)\), define the edge time series as

\[
e_{ij}(t) = z_i(t)z_j(t), \qquad i < j.
\]

Using sample-standardized signals, the existing Pearson correlation should be recovered by

\[
r_{ij} = \frac{1}{T-1}\sum_t e_{ij}(t).
\]

The first implementation should support atlas/ROI-based ETS and global cofluctuation amplitude. It should not initially calculate edge functional connectivity (an edges-by-edges matrix) or implement voxelwise/searchlight analyses.

## Why the full `.netets` matrix is usually not what's needed

In the edge-time-series literature (e.g. Zamani Esfahlani et al. 2020, *PNAS*; Faskowitz et al. 2020, *Nat. Neurosci.*), \(e_{ij}(t)\) is treated as a cheap intermediate, not a deliverable: each entry is a single multiply of two already-computed z-scores, so it costs almost nothing to calculate, but at \(O(ET)\) it is expensive to store, and downstream analyses almost never consume the raw matrix directly. The standard pattern is compute-reduce-discard: derive a summary statistic per edge or per frame in one streaming pass, then throw the edge matrix away.

The two reductions that dominate actual usage are:

1. **Global cofluctuation amplitude, `RSS(t)`** — already in this plan as `-ets_rss`.
2. **Event-based FC**: threshold `RSS(t)` to find high-amplitude ("event") frames and low-amplitude frames, then average \(e_{ij}(t)\) over just those frame subsets to get high- and low-amplitude connectivity matrices. The central empirical finding driving this literature is that FC computed from a small fraction of high-amplitude frames closely resembles the full time-averaged FC — i.e., a minority of frames drive most of static connectivity. This is arguably the most common ETS-derived product in practice, and it is just as cheap to stream as RSS: `O(NT)` for the RSS pass to pick frames, plus one more `O(NT + T)`-memory edge pass to accumulate the selected-frame sums.

A secondary but useful reduction is **per-edge temporal moments** (mean, variance) computed via an online (Welford-style) accumulator during the same edge pass — the mean reproduces the existing `.netcc` correlation (already used as a correctness check), and the variance characterizes how "bursty" vs. steady an edge's cofluctuation is, without ever storing its time series.

Given this, the plan below keeps `-ets_out` (the full `.netets` dump) as an option for custom downstream analysis, but adds two new streaming, `O(E)`-output options — event-based FC and per-edge moments — that match what the field actually computes after ETS, at a small fraction of the output size and no added `O(ET)` risk.

## Proposed data flow

```text
4D BOLD dataset + ROI atlas
             |
             v
ROI_AVE_TS[network][ROI][time]       existing
             |
             +----> Corr_Matr                          existing .netcc
             |
             +----> standardized ROI TS
             |          +----> global RSS               new .netrss
             |          |         +----> event frame selection (high/low RSS)
             |          |
             |          +----> edge products (streamed, one edge at a time)
             |                     +----> full edge matrix        new .netets        (opt-in, large)
             |                     +----> edge metadata            new .netets.meta
             |                     +----> per-edge moments         new .netets_stats  (small)
             |                     +----> event-based FC matrices  new .netcc_hiCF /
             |                                                         .netcc_loCF    (small)
             |
             +----> raw ROI time series       existing .netts
```

The edge calculations should branch from `ROI_AVE_TS` after `CalcAveRTS()` has populated it. The existing correlation path should remain unchanged.

## Initial command-line interface

Add four independent options, all opt-in and off by default:

```text
-ets_out             Output the time series for every unique ROI pair (large; O(E*T)).
-ets_rss             Output global edge cofluctuation amplitude at every time point.
-ets_stats           Output per-edge temporal mean and variance, without storing edge time series.
-ets_event_fc        Output high- and low-amplitude event-based FC matrices, without storing
                      edge time series. Implies the same RSS calculation as -ets_rss.
```

Two further options control event-frame selection, only meaningful with `-ets_event_fc`:

```text
-ets_event_frac FRAC   Fraction of frames (by RSS rank) used for each of the high- and
                        low-amplitude event matrices. Default 0.10 (top/bottom 10%).
-ets_event_thresh VAL  Absolute RSS threshold instead of a fraction; frames >= VAL are
                        "high," frames <= VAL are "low." Mutually exclusive with
                        -ets_event_frac.
```

None of the four `-ets_*` options require each other except `-ets_event_fc`'s implicit dependence on an RSS pass (handled internally; the user does not need to also pass `-ets_rss`). `-ets_out` remains independent and is the only one of the four whose output scales as `O(E*T)`; the other three are `O(E)` or smaller and safe to enable by default in typical group pipelines.

For each atlas sub-brick/network, produce:

```text
PREFIX_000.netets            edge-by-time numeric matrix              (from -ets_out)
PREFIX_000.netets.meta       mapping from edge rows to ROI indices    (from -ets_out)
                              and labels
PREFIX_000.netrss            global RSS time series                   (from -ets_rss or
                                                                        implied by -ets_event_fc)
PREFIX_000.netets_stats      per-edge mean and variance, one row      (from -ets_stats)
                              per edge, same edge ordering as .netets.meta
PREFIX_000.netcc_hiCF        high-amplitude event-based FC matrix,    (from -ets_event_fc)
                              same format as .netcc
PREFIX_000.netcc_loCF        low-amplitude event-based FC matrix,     (from -ets_event_fc)
                              same format as .netcc
PREFIX_000.netets_events.meta  threshold/fraction used, number of     (from -ets_event_fc)
                              frames selected for each of high/low
```

File-naming details (suffixes, exact column layouts) are illustrative here and should be finalized against AFNI's existing naming conventions during implementation, not treated as final.

The `.netets` orientation should follow the existing `.netts` convention: one edge per row and one time point per column. The help text must state the orientation explicitly because much of the external ETS literature represents the same data as time-by-edge.

The proposed first version should use ASCII output for consistency and inspectability. Binary or NIML output can be considered after the semantics and metadata format are stable.

## Implementation outline

### 1. Add option and state variables

Near the existing `TS_OUT`, `TS_LABEL`, and related variables in `src/ptaylor/3dNetCorr.c`, add flags such as:

```c
int ETS_OUT = 0;
int ETS_RSS_OUT = 0;
int ETS_STATS_OUT = 0;
int ETS_EVENT_FC_OUT = 0;
double ETS_EVENT_FRAC = 0.10;
double ETS_EVENT_THRESH = -1.0;   /* unset sentinel; -ets_event_frac used unless overridden */
```

Parse `-ets_out`, `-ets_rss`, `-ets_stats`, `-ets_event_fc`, `-ets_event_frac`, and `-ets_event_thresh` next to the existing `-ts_out` option, following the codebase's existing flat `strcmp` chain style. Reject `-ets_event_frac` and `-ets_event_thresh` together at parse time with a clear error, and reject either one without `-ets_event_fc`. Add all six options to:

- the detailed help text and usage synopsis in `3dNetCorr.c`;
- `src/prog_opts.c`, so AFNI's option-suggestion machinery recognizes them;
- the AFNI history entry for the change.

When neither option is supplied, program behavior and output must remain unchanged.

### 2. Standardize the ROI time series

After `ROI_AVE_TS` is populated, standardize every usable ROI time series:

\[
z_i(t) = \frac{x_i(t)-\bar{x}_i}{s_i},
\]

where \(s_i\) is the sample standard deviation computed with denominator \(T-1\). This convention makes the sum of ETS values divided by \(T-1\) agree with the existing Pearson correlation.

Allocate standardized data only when an ETS option is active:

```c
double ***ROI_Z_TS;
```

Its logical layout should match `ROI_AVE_TS`:

```text
ROI_Z_TS[network][ROI][time]
```

Note this is a second full `double***` allocation alongside `ROI_AVE_TS` — memory cost is `O(NT)` on top of the existing `O(NT)` for `ROI_AVE_TS`, not free. That's cheap relative to edge-level storage (`O(ET)`), so a full second array is acceptable, but the doc should say so explicitly rather than imply the extra cost is negligible. If it ever matters, `ROI_Z_TS` could be standardized lazily per-network instead of allocated for all networks at once, since `.netets`/`.netrss` are already written per network/sub-brick.

Put the standardization operation in a small testable helper in `src/ptaylor/rsfc.c` and declare it in `src/ptaylor/rsfc.h`, for example:

```c
int CalcZscoreRTS(const double *x, double *z, int nt);
```

The helper should report failure for fewer than two observations, nonfinite inputs, or zero variance. The caller should include the network and ROI label in any user-facing error.

### 3. Define deterministic edge ordering

For \(N\) ROIs, write the \(E=N(N-1)/2\) unique undirected edges in upper-triangle order:

```c
edge = 0;
for (i = 0; i < nroi; ++i) {
    for (j = i + 1; j < nroi; ++j) {
        /* edge connects ROI i and ROI j */
        ++edge;
    }
}
```

The ordering must be documented and tested. Metadata must use both the internal zero-based ROI indices and the actual atlas labels, since atlas labels may be nonconsecutive.

A proposed `.netets.meta` layout is:

```text
# edge  roi_index_1  roi_index_2  roi_label_1  roi_label_2  name_1  name_2
0       0            1            2            7            LH_V1   LH_V2
1       0            2            2            12           LH_V1   LH_MT
```

Label strings may require quoting or escaping if whitespace is permitted. The final format should be chosen to remain easy to parse with AFNI, Python, MATLAB, and R tools. Specifically, check whether downstream consumers (`fat_mvm_prep.py`/`lib_fat_funcs.py`, and any R-side readers) expect a fixed column count or rely on header parsing before finalizing the layout — if labels can contain whitespace, either guarantee a separate whitespace-free identifier column for programmatic matching or require quoting with a documented escaping convention, rather than leaving column-splitting ambiguous.

### 4. Stream `.netets` output

Do not allocate an edge-by-time array. For each edge, calculate one temporary vector and immediately write it:

```c
for (t = 0; t < nt; ++t)
    edge_ts[t] = ROI_Z_TS[k][i][t] * ROI_Z_TS[k][j][t];
```

This keeps additional working memory near \(O(NT + T)\), rather than \(O(ET)\). Note that this streaming writer is a deliberate departure from `3dNetCorr`'s existing style: today's `.netcc`/`.netts` writers fully buffer their (much smaller, `O(N)`/`O(N^2)`) data in memory and write with plain `fprintf` loops only after all computation finishes. `.netets` is `O(N^2 T)`, which is why buffering doesn't scale here and streaming is justified — call this out explicitly in the code comments and help text so it doesn't read as an unexplained stylistic inconsistency with the rest of the program.

The output file can still be large, so print a summary before writing:

```text
++ Network 0: 400 ROIs, 79,800 edges, 1,200 time points
++ Writing 95,760,000 edge-time-series values
```

Do not leave the large-output threshold as a "consider it later" item — decide it now, since a `-ets_out` on a 400-ROI atlas at 1200 timepoints already produces ~95M values (~1GB+ ASCII) with no warning under the current plan. Add an explicit size check against a documented threshold (e.g., estimated output >10GB, matching the scale of overrides AFNI already uses elsewhere for expensive operations) that requires a companion flag such as `-ok_ets_out_huge` to proceed. Do not impose a silent truncation.

The streaming writer should be placed in a helper, for example:

```c
int WriteEdgeTS(...);
```

The helper should write both the numeric matrix and its metadata using exactly the same nested edge loop, preventing ordering drift between the files.

### 5. Calculate global cofluctuation amplitude efficiently

Define global cofluctuation amplitude as

\[
RSS(t)=\sqrt{\sum_{i<j}[z_i(t)z_j(t)]^2}.
\]

Calculate it without constructing the edge matrix, using

\[
RSS(t)^2 = \frac{1}{2}
\left[
\left(\sum_i z_i(t)^2\right)^2 - \sum_i z_i(t)^4
\right].
\]

This reduces RSS calculation from \(O(ET)\) to \(O(NT)\). Implement it in a separately testable helper such as:

```c
int CalcEdgeRSS(double **zts, int nroi, int nt, double *rss);
```

Protect the square-root argument from tiny negative values caused by floating-point roundoff by clamping values close to zero. Define "close to zero" concretely rather than leaving it as prose — e.g. clamp when the argument is negative but larger than `-N_TOL * DBL_EPSILON * scale`, where `scale` is a representative magnitude such as the mean of \(\left(\sum_i z_i(t)^2\right)^2\) over the network, and `N_TOL` is a small documented multiplier (start around 10-100 and justify empirically in testing). A materially negative value (outside that band) should be treated as an internal error.

Write `.netrss` as a single numeric time series with enough precision to reproduce rankings and event thresholds reliably. Include comments identifying the network, number of ROIs, number of time points, and normalization convention if AFNI's 1D readers permit them cleanly.

### 6. Calculate per-edge temporal moments without storage (`-ets_stats`)

For each edge, accumulate the mean and (sample) variance of \(e_{ij}(t)\) across \(t\) using a single-pass, numerically stable (Welford-style) online update, inside the same per-edge loop used for streaming `.netets` — whether or not `.netets` is actually written:

```c
int UpdateWelford(double x, int t, double *mean, double *m2);
int FinalizeWelfordVariance(double m2, int nt, double *var);
```

This produces one row per edge — `edge, mean, variance` — written to `.netets_stats` using the identical edge-ordering loop as `.netets.meta`, so row `k` in both files always refers to the same edge without a join key. The mean of edge `k` must agree with `Corr_Matr[i][j]` for that edge under the same tolerance already used for the `.netets` correctness check (Section 8 below); test this the same way.

Output size here is `O(E)`, not `O(ET)`, so none of the large-output safeguards in "Output-size and scope safeguards" apply to `-ets_stats`.

### 7. Calculate event-based FC without storage (`-ets_event_fc`)

Event-based FC requires two passes over the data, both already `O(NT)`-or-cheaper and neither requiring the edge matrix to be materialized:

1. **Frame selection pass.** Compute `RSS(t)` for all `t` using the existing `O(NT)` identity from Section 5 (reuse the same calculation whether or not `-ets_rss` was also requested — do not compute it twice). Rank frames by `RSS(t)` and select the top `ETS_EVENT_FRAC` fraction as "high" and the bottom `ETS_EVENT_FRAC` fraction as "low," or apply `ETS_EVENT_THRESH` directly if given. Record the resulting threshold value and frame counts regardless of which selection mode was used, for `.netets_events.meta`.

2. **Selected-frame accumulation pass.** Reuse the same per-edge streaming loop as `.netets`/`-ets_stats`. For each edge, instead of (or in addition to) writing every `e_{ij}(t)`, accumulate two running sums restricted to the frame-index sets chosen in step 1:

```c
double hi_sum, lo_sum;
for (t = 0; t < nt; ++t) {
    double eij = ROI_Z_TS[k][i][t] * ROI_Z_TS[k][j][t];
    if (is_hi_frame[t])  hi_sum += eij;
    if (is_lo_frame[t])  lo_sum += eij;
}
```

Divide each accumulated sum by its frame count (not `T-1`) to get the event-based FC values, and write them into `.netcc_hiCF` / `.netcc_loCF` using the same matrix layout as the existing `.netcc` writer, so downstream tools (including `fat_mvm_prep.py`, see "Group-level analysis" below) can read them with no format changes.

Because this reuses the same per-edge loop as `-ets_out` and `-ets_stats`, all three options should share one internal edge-iteration helper that performs whichever subset of {write full row, update Welford accumulators, accumulate hi/lo sums} is active, rather than three separate loops over the same edges — this avoids recomputing `e_{ij}(t)` multiple times per edge when more than one `-ets_*` option is requested together.

Output size here is `O(E)` for the two FC matrices plus `O(T)` for the frame-selection bookkeeping — no `.netets`-scale storage is ever required for `-ets_event_fc` on its own.

### 8. Preserve existing correlation behavior

Do not replace the existing `CORR_FUN()` calculation with the ETS calculation in the initial implementation. Keeping the paths separate minimizes regression risk and provides a valuable correctness check.

For every valid edge, tests should confirm:

\[
\frac{1}{T-1}\sum_t e_{ij}(t)
\approx Corr\_Matr[i][j].
\]

If a debug or verification mode is added, it could perform this comparison internally with a documented tolerance. It should not add production-time overhead by default.

## Weighting and censoring semantics

This is the main behavior that must be settled explicitly.

### `-weight_ts`

`-weight_ts` modifies the signals used in forming ROI averages. The resulting `ROI_AVE_TS` can be standardized normally, so ETS can follow the existing behavior without additional machinery. The help should state that the edge time series are derived from the weighted ROI time series.

### `-weight_corr`

Ordinary ETS based on unweighted z-scores does not decompose a weighted Pearson correlation. A weighted decomposition requires weighted means, weighted scaling, and an explicit definition of the framewise weight contribution.

For the first implementation, reject the combination of `-weight_corr` with any of the four `-ets_*` options and print a clear explanation. Do not silently ignore the weights or emit ETS-derived output whose values disagree with the reported `.netcc` matrix.

A weighted ETS definition can be designed and added separately after its mathematical convention and behavior for zero-weight/censored frames are documented.

## Null, constant, and invalid ROI time series

An all-zero or constant ROI cannot be standardized. Use the following initial policy:

- By default, stop and report the network number, ROI index, atlas label, and label-table name.
- With `-allow_roi_zeros`, write zero-valued edge time series for all edges incident on that ROI and emit a warning once per affected ROI. This applies uniformly across all four `-ets_*` outputs: zero-valued edges contribute zero to `.netets`, `.netets_stats` (mean/variance both zero), and both event-FC accumulators.
- Record affected ROIs or edges in `.netets.meta`.
- Treat NaN and infinite ROI values as fatal errors rather than propagating them silently.

The implementation should distinguish an all-zero ROI from a nonzero constant ROI in diagnostic messages even if both ultimately receive the same permitted output behavior.

## Output-size and scope safeguards

These safeguards apply only to `-ets_out`. `-ets_stats` and `-ets_event_fc` produce `O(E)` output (one row/matrix entry per edge, not per edge-time-point) and need no size-overflow guard beyond what `.netcc` already handles today, since their output is the same order of magnitude as the existing `.netcc` matrix. This is itself part of the justification for treating them as low-cost, encouraged-by-default outputs rather than opt-in-with-caution ones like `-ets_out`.

Before writing `.netets`, calculate the number of edges and output values using a wide integer type. Check for overflow when evaluating:

```text
nedges = nroi * (nroi - 1) / 2
nvalues = nedges * nt
```

Report estimated ASCII size or at least dimensions for large outputs. File-write failures must stop the program with the filename and edge row at which writing failed.

Do not add full edge functional connectivity in this change. For 400 ROIs, 79,800 edges imply approximately 6.37 billion eFC entries. A future eFC implementation would require a separate design covering binary output, chunking, symmetry, numerical definition, and size limits.

## Searchlight and voxelwise analyses

The first implementation should remain atlas-based. `3dNetCorr` assumes a fixed set of labeled ROIs and already provides all required regional time series and metadata. Searchlights have overlapping, center-dependent node definitions and do not fit this abstraction cleanly.

Seed-to-voxel ETS can already be composed separately from standardized 4D data and a standardized seed time series using `3dcalc`. Whole-brain voxel-pair ETS or local searchlight ETS should be considered a separate program or workflow rather than an extension of the initial `3dNetCorr` change.

## Group-level analysis of ETS-derived outputs

`3dMVM`'s `-dataTable` option only accepts a long-format table of one scalar value per subject per edge (per its numeric `InputFile` branch); it does not ingest time-series-shaped data directly, and there is no existing AFNI precedent for group-level analysis of edge time series or dynamic FC. The existing bridge for `.netcc`/`.grid` files is `fat_mvm_prep.py`, which flattens per-subject matrices into a `3dMVM`-ready table.

The `-ets_stats` and `-ets_event_fc` outputs are designed to close most of this gap without any new group-level tooling: `.netets_stats`, `.netcc_hiCF`, and `.netcc_loCF` are already per-subject, per-edge scalar matrices in (or directly convertible to) `.netcc` shape, so `fat_mvm_prep.py` should be able to ingest them with the same matrix-reading logic it already uses for `.netcc`/`.grid` files, needing at most a new file-type case rather than new reduction logic — because the reduction (mean/variance, or high/low-amplitude averaging) has already happened inside `3dNetCorr` itself, not deferred to the prep step.

`.netets` (the full opt-in edge-by-time dump, from `-ets_out` only) is the one output that remains genuinely out of scope for direct group analysis, because it is time-resolved rather than a per-subject-per-edge scalar. Reducing it to a scalar is still a modeling decision that has to be made explicitly, either via `-ets_stats`/`-ets_event_fc` at the individual-subject level (preferred, since the reduction choice is then documented in the `3dNetCorr` call itself) or via a custom downstream analysis of the raw `.netets` file for users who need something `-ets_stats`/`-ets_event_fc` don't cover.

Recommended follow-on, once these outputs are stable: extend `fat_mvm_prep.py` (or a sibling script) with the small addition needed to read `.netets_stats`/`.netcc_hiCF`/`.netcc_loCF`, producing a table in the same format `3dMVM`'s numeric `-dataTable` branch already consumes. Keep this step external and inspectable (an intermediate table, as with the existing `*_MVMtbl.txt`) rather than inlining matrix-format reading into `3dMVM.R`, to preserve the ability to catch subject-matching errors before a model fit runs. This is deliberately not part of the initial `3dNetCorr` implementation sequence below, since it depends on the ETS-derived output formats being finalized first — but it is now a much smaller follow-on than before, since `-ets_stats`/`-ets_event_fc` do the statistically meaningful work of reduction, leaving only file-format plumbing.

## Testing plan

Add focused tests covering:

1. A small synthetic dataset with hand-calculated z-scores and edge products.
2. Agreement between the ETS temporal sum divided by \(T-1\) and `.netcc` Pearson correlations.
3. Agreement between explicit pairwise RSS and the \(O(NT)\) RSS identity.
4. Deterministic upper-triangle edge ordering.
5. Correct metadata for nonconsecutive ROI labels and label-table strings.
6. Multiple atlas sub-bricks/networks.
7. Empty, all-zero, nonzero-constant, NaN, and infinite ROI time series.
8. `-allow_roi_zeros` behavior.
9. `-weight_ts` behavior.
10. Rejection of `-weight_corr` with ETS options.
11. Small values near floating-point precision limits in the RSS square-root argument.
12. Output-size arithmetic using enough ROIs and frames to exercise wide integer types.
13. Regression coverage confirming byte-for-byte unchanged standard outputs when ETS options are absent.
14. `-ets_stats` mean matches `Corr_Matr` and matches the temporal mean computed by explicitly averaging `.netets` rows, for the same synthetic dataset.
15. `-ets_stats` variance matches a hand-computed/NumPy reference variance for a small synthetic edge.
16. `-ets_event_fc` frame selection: `-ets_event_frac` picks the correct top/bottom-`FRAC` frames by RSS rank on a synthetic `RSS(t)` with known ordering, including tie-breaking behavior.
17. `-ets_event_fc` frame selection: `-ets_event_thresh` selects the correct frames for a synthetic dataset with a known threshold crossing.
18. `.netcc_hiCF`/`.netcc_loCF` values match an explicit average of `.netets` rows restricted to the selected frame indices, for the same synthetic dataset.
19. Rejection of `-ets_event_frac` and `-ets_event_thresh` used together, and of either one without `-ets_event_fc`.
20. Combining multiple `-ets_*` options in one run produces edge products computed only once per edge (verify via instrumentation/counters in a debug build, not just output correctness), confirming the shared edge-iteration helper is actually shared.
21. `-allow_roi_zeros` behavior propagates correctly into `.netets_stats` (zero mean/variance) and into `.netcc_hiCF`/`.netcc_loCF` (zero-valued affected edges).

Where practical, compare test outputs with a short independent NumPy or MATLAB reference implementation.

## Documentation additions

The `3dNetCorr -help` text should explain:

- the ETS equation and its relationship to Pearson correlation;
- that ETS values are framewise contributions to correlation, not single-frame correlation estimates;
- the edge ordering and output orientation;
- the `.netets.meta` mapping;
- the RSS definition;
- why `-ets_stats` and `-ets_event_fc` exist as cheaper, `O(E)` alternatives to the full `.netets` dump, and that most users likely want these two rather than `-ets_out`, per standard ETS-literature practice;
- the `-ets_stats` mean/variance definitions and their `.netets_stats` layout;
- the `-ets_event_fc` selection semantics (`-ets_event_frac` vs. `-ets_event_thresh`), the `.netcc_hiCF`/`.netcc_loCF` outputs, and the `.netets_events.meta` record of the threshold/counts actually used;
- expected scaling with the number of ROIs;
- handling of constant ROIs;
- interactions with `-weight_ts`, `-weight_corr`, and `-allow_roi_zeros`;
- that the initial feature is atlas/ROI based and does not implement searchlights or eFC.

Add a minimal example. This one uses the two low-cost, literature-standard reductions; a user who genuinely needs the raw matrix can add `-ets_out` separately:

```sh
3dNetCorr                                      \
    -inset errts+tlrc                          \
    -in_rois atlas+tlrc                        \
    -prefix subject_network                    \
    -ts_out                                    \
    -ets_stats                                 \
    -ets_event_fc                              \
    -ets_rss
```

## Recommended implementation sequence

1. Add option parsing, help text, and no-op plumbing for all four `-ets_*` options (plus `-ets_event_frac`/`-ets_event_thresh`).
2. Add and unit-test ROI standardization.
3. Add and unit-test RSS calculation and `.netrss` output.
4. Add deterministic edge enumeration and metadata output.
5. Add the shared per-edge streaming helper (Section 4/7's internal edge-iteration loop), initially driving only `.netets` output (`-ets_out`).
6. Extend the shared helper with the Welford accumulators for `-ets_stats` and `.netets_stats` output.
7. Extend the shared helper with event-frame selection and the hi/lo accumulators for `-ets_event_fc`, `.netcc_hiCF`/`.netcc_loCF`, and `.netets_events.meta`.
8. Add validation against existing `.netcc` correlations, for both the `.netets` temporal mean and the `-ets_stats` mean.
9. Add null-ROI and weighting behavior, verified across all four `-ets_*` outputs.
10. Add regression, performance, and output-size tests.
11. Update AFNI history and user documentation.
12. (Follow-on, separate change) Extend `fat_mvm_prep.py` to ingest `.netets_stats`/`.netcc_hiCF`/`.netcc_loCF` for group analysis, per "Group-level analysis of ETS-derived outputs" above.

This sequence delivers the small, low-output RSS feature first, then builds the shared streaming machinery once against the largest output (`.netets`) before layering the two cheaper `O(E)` reductions on top of the same loop — keeping each new numerical component independently verifiable while avoiding three separate, drifting edge-iteration implementations.
