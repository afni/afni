# 3dRSA 1D oracle validation

## Purpose

`3dRSA` accepts AFNI's quoted 1D-dataset syntax (for example,
`input.1D'`).  In that representation each column is a synthetic voxel and
each row is a sub-brick.  This makes it possible to test the RSA calculations
with tiny, fully specified datasets while exercising the ordinary AFNI dataset
reader and the public `3dRSA` command-line interface.

The executable test is
`src/pmolfese/tests/run_1d_oracle.py`.  It is registered with CTest as
`3dRSA:1d-oracle` and needs only Python's standard library plus the built
`3dRSA` binary.  It deliberately does not use NumPy, SciPy, or another AFNI
program to compute its expected values.  Instead, it implements the relevant
Pearson, Spearman (midrank), Euclidean RDM, Fisher-mean, crossnobis, exact-null,
and pairwise zero-censor calculations directly.  Agreement is therefore a
black-box check rather than the same production code validating itself.

## Fixture layout

Each campaign uses six subjects, six conditions, seven time points, and four
synthetic voxels per ROI.  The atlas is a one-row 1D dataset whose values are
ROI labels.  Input files are quoted so that their logical rows are bricks and
their logical columns are voxels.  Campaigns run with 2, 5, and 10 ROIs.  This
covers the smallest useful multiple-ROI case, a typical compact case, and a
larger layout that catches accidental fixed-size or ROI-index assumptions.

All numeric comparisons require finite results and use independently calculated
expected values.  Scalar effects normally use a tolerance of `3e-6`; saved RDM
entries use `6e-5` to allow for output precision.  Repeated OpenMP results are
required to agree exactly with the one-thread result.

## Covered behavior

### Classic RSA

For every ROI count, the oracle verifies compact and shuffled-long input-table
layouts against independently calculated condition RDMs and Fisher-mean model
correlations.  It also verifies these invariants:

- rescaling the input by `1e-8` and by `1e5` does not change correlation-based
  classic RSA;
- a fixed voxel-wise translation does not change it;
- subject ordering, jointly permuted condition/model ordering, and jointly
  permuted atlas/data voxel ordering do not change it;
- constant patterns produce the defined zero comparator result rather than a
  NaN or crash;
- tied distances with `-metric spearman` use average ranks (midranks), which
  protects the tied-rank comparator fix.

The test also enumerates all `6! = 720` condition permutations for the classic
condition null.  It independently verifies both the uncorrected p value and
the maximum-statistic FWE p value.

### IS-RSA modes

The test independently constructs and checks saved neural RDMs as well as the
reported effect for:

- ROI-mean IS-RSA;
- ROI-mean IS-RSA with neural Spearman correlation (`-neural_metric scorr`);
- pattern feature extraction;
- second-order RDM feature extraction.

This catches mistakes that might leave a final effect plausible while producing
the wrong intermediate neural representational matrix.

### Crossnobis and run maps

Three-run crossnobis fixtures verify the ordered independent-run-pair average.
One fixture has all condition/run cells; another omits one condition in one run
through a run-condition map.  The generated data intentionally contain at
least one negative crossvalidated distance, so the test also protects against
an invalid nonnegative-distance assumption.

### `-zcensor`

The zero-censor fixture gives every subject/ROI a different all-zero local
pattern at a time point.  It also puts `[1, -1, 2, -2]` at the following time
point: its ROI mean is zero, but the local pattern is not all zero.  The latter
must be retained, proving that censoring is determined from the local original
voxel pattern, not from the ROI mean.

For every pair of subjects, the oracle independently retains only the
intersection of their nonzero local time points, requiring at least three.
It checks both Pearson (`corr`) and Spearman (`scorr`) neural similarity,
reported effects, and saved neural RDMs.  It repeats Pearson with one and two
OpenMP threads and with `-polort` placed before `-zcensor`; the latter must be
equivalent because `-zcensor` resets detrending to `-polort -1`.

Negative contract tests verify that the command rejects `-zcensor` with a
non-correlation neural metric and aborts rather than producing an invalid
similarity when a pair has fewer than three jointly retained time points.

## Input-contract failures

The two-ROI campaign also confirms clear failure for an unquoted/transposed 1D
input (whose voxel count does not match the atlas) and for a non-finite input
value.  These checks guard the 1D orientation convention and the finite-data
input contract.

## Running the campaign

From an AFNI build directory configured with the test suite:

```sh
cmake --build build --target 3dRSA
ctest --test-dir build -R '^3dRSA:1d-oracle$' --output-on-failure
```

To inspect the generated fixtures, run the script directly and provide a work
directory:

```sh
python3 src/pmolfese/tests/run_1d_oracle.py \
  --bin build/targets_built/3dRSA --work /tmp/3drsa-1d-oracle
```

## Scope

This oracle is an intentionally small-volume functional test.  It establishes
the 1D dataset convention and core numeric behavior without requiring NIfTI or
surface support.  Real-volume atlas, searchlight, and seed-to-target behavior,
including real-volume `-zcensor`, is covered separately by
`src/pmolfese/tests/run_numeric.py` (`3dRSA:numeric`).
