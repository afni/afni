#!/usr/bin/env python3
"""Small, dependency-free black-box oracle for 3dRSA's 1D-dataset input.

AFNI's trailing-apostrophe 1D-dataset syntax (``file.1D'``) transposes the
logical dataset: every input row is a brick and every input column is a voxel.
This test intentionally uses that representation for both the atlas and input
datasets.  Its independently implemented arithmetic therefore catches a
transposed loader, wrong ROI membership, condition/brick confusion, and changes
to the core RSA calculation without relying on NIfTI, numpy, scipy, or AFNI
command-line helpers.

For each of 2, 5, and 10 ROIs it checks compact and shuffled-long classic RSA,
IS-RSA mean/pattern/second-order-RDM extraction, crossnobis with unbalanced
run condition maps, exact small-null p/FWE values, and algebraic invariants.
"""
import argparse
import itertools
import math
import os
import shutil
import subprocess
import tempfile


N_SUBJECTS = 6
N_CONDITIONS = 6
N_TIMEPOINTS = 7
VOXELS_PER_ROI = 4


def write_1d(path, rows):
    with open(path, "w") as stream:
        for row in rows:
            stream.write(" ".join("%.9g" % value for value in row) + "\n")


def read_matrix(path):
    rows = []
    with open(path) as stream:
        for line in stream:
            line = line.strip()
            if line and not line.startswith("#"):
                rows.append([float(value) for value in line.split()])
    return rows


def read_rsa_table(path):
    header, rows = None, []
    with open(path) as stream:
        for line in stream:
            if line.startswith("#ROI"):
                header = line[1:].split()
            elif line.strip() and not line.startswith("#") and header is not None:
                rows.append(dict(zip(header, line.split())))
    if header is None:
        raise RuntimeError("no 3dRSA table header in %s" % path)
    return header, rows


def pearson(a, b):
    if len(a) != len(b) or len(a) < 2:
        raise ValueError("invalid correlation inputs")
    am = sum(a) / len(a)
    bm = sum(b) / len(b)
    aa = [value - am for value in a]
    bb = [value - bm for value in b]
    denom = math.sqrt(sum(value * value for value in aa) *
                      sum(value * value for value in bb))
    return sum(x * y for x, y in zip(aa, bb)) / denom if denom else 0.0


def rank_average(values):
    order = sorted(range(len(values)), key=lambda index: values[index])
    ranks = [0.0] * len(values)
    start = 0
    while start < len(order):
        end = start + 1
        while end < len(order) and values[order[end]] == values[order[start]]:
            end += 1
        rank = (start + 1 + end) / 2.0
        for index in order[start:end]:
            ranks[index] = rank
        start = end
    return ranks


def spearman(a, b):
    return pearson(rank_average(a), rank_average(b))


def euclidean(a, b):
    return math.sqrt(sum((x - y) ** 2 for x, y in zip(a, b)))


def rdm(features):
    return [[0.0 if ii == jj else euclidean(features[ii], features[jj])
             for jj in range(len(features))]
            for ii in range(len(features))]


def spearman_simmat(features):
    return [[1.0 if ii == jj else spearman(features[ii], features[jj])
             for jj in range(len(features))]
            for ii in range(len(features))]


def zero_censored_simmat(features, keep, metric):
    """Independent pairwise-local all-zero-frame censoring reference."""
    size = len(features)
    matrix = [[1.0 if ii == jj else 0.0 for jj in range(size)]
              for ii in range(size)]
    for ii in range(size):
        for jj in range(ii + 1, size):
            left = [features[ii][timepoint] for timepoint in range(N_TIMEPOINTS)
                    if keep[ii][timepoint] and keep[jj][timepoint]]
            right = [features[jj][timepoint] for timepoint in range(N_TIMEPOINTS)
                     if keep[ii][timepoint] and keep[jj][timepoint]]
            if len(left) < 3:
                raise ValueError("oracle fixture retained fewer than three frames")
            value = spearman(left, right) if metric == "scorr" else pearson(left, right)
            matrix[ii][jj] = matrix[jj][ii] = value
    return matrix


def triangle(matrix):
    return [matrix[ii][jj] for ii in range(len(matrix))
            for jj in range(ii + 1, len(matrix))]


def fisher_mean(values):
    return math.tanh(sum(math.atanh(value) for value in values) / len(values))


def crossnobis(patterns, present=None):
    """Independent ordered-run-pair crossvalidated squared Euclidean RDM."""
    nrun, ncondition = len(patterns), len(patterns[0])
    nvoxel = len(patterns[0][0])
    matrix = [[0.0] * ncondition for _ in range(ncondition)]
    for first in range(ncondition):
        for second in range(first + 1, ncondition):
            use = [run for run in range(nrun) if present is None or
                   (present[run][first] and present[run][second])]
            if len(use) < 2:
                raise ValueError("fewer than two independent runs for a condition pair")
            value = 0.0
            for left in use:
                for right in use:
                    if left == right:
                        continue
                    for voxel in range(nvoxel):
                        dl = patterns[left][first][voxel] - patterns[left][second][voxel]
                        dr = patterns[right][first][voxel] - patterns[right][second][voxel]
                        value += dl * dr
            value /= len(use) * (len(use) - 1) * nvoxel
            matrix[first][second] = matrix[second][first] = value
    return matrix


def model_matrix(size):
    # Fixed, symmetric, non-degenerate condition/subject model.  It is
    # deliberately not the neural RDM, so the tests exercise the comparator
    # rather than merely obtaining a trivial r=1 result.
    matrix = [[0.0] * size for _ in range(size)]
    for ii in range(size):
        for jj in range(ii + 1, size):
            value = abs(ii - jj) + 0.19 * ((ii + 2 * jj) % 3) + 0.07 * (ii + jj)
            matrix[ii][jj] = matrix[jj][ii] = value
    return matrix


def classic_subject(subject, nroi):
    """Condition x fake-voxel rows for a compact classic-RSA input."""
    rows = []
    for condition in range(N_CONDITIONS):
        row = []
        for roi in range(nroi):
            for voxel in range(VOXELS_PER_ROI):
                # Each factor perturbs a different axis, which makes every ROI
                # and subject a distinct, finite condition RDM.
                value = (
                    (condition + 1) * (voxel + 1)
                    + 0.31 * ((condition + 2 * voxel) % 3)
                    + 0.17 * subject * (voxel - 1.5)
                    + 0.13 * roi * ((condition + voxel + 1) % 4)
                    + 0.03 * subject * condition * (roi + 1)
                )
                row.append(value)
        rows.append(row)
    return rows


def mean_subject(subject, nroi):
    """Time x fake-voxel rows for IS-RSA's ROI-mean feature type."""
    rows = []
    for timepoint in range(N_TIMEPOINTS):
        row = []
        for roi in range(nroi):
            for voxel in range(VOXELS_PER_ROI):
                # The last term sums to zero within each ROI; it makes the
                # input genuinely multi-voxel while preserving a simple,
                # independently computed ROI mean below.
                value = (
                    (subject + 1) * (timepoint + 2)
                    + 0.11 * timepoint * timepoint
                    + 0.23 * roi * ((timepoint + subject) % 3)
                    + 0.17 * subject * subject
                    + (voxel - 1.5) * (0.09 + 0.02 * subject)
                )
                row.append(value)
        rows.append(row)
    return rows


def tied_mean_subject(subject, nroi):
    """Tied time courses for the neural `scorr` (Spearman) constructor."""
    rows = []
    for timepoint in range(N_TIMEPOINTS):
        row = []
        for roi in range(nroi):
            mean = float((timepoint + 2 * subject + roi) % 3)
            for voxel in range(VOXELS_PER_ROI):
                row.append(mean + (voxel - 1.5) * 0.2)
        rows.append(row)
    return rows


def zcensor_mean_subject(subject, nroi):
    """Includes whole-pattern zeros and a nonzero pattern whose mean is zero."""
    rows = mean_subject(subject, nroi)
    for roi in range(nroi):
        start = roi * VOXELS_PER_ROI
        zero_time = (subject + 3 * roi) % N_TIMEPOINTS
        cancel_time = (zero_time + 1) % N_TIMEPOINTS
        rows[zero_time][start:start + VOXELS_PER_ROI] = [0.0] * VOXELS_PER_ROI
        # This vector has mean zero but is not an all-zero local pattern.  It
        # must remain in the pairwise feature vector.
        rows[cancel_time][start:start + VOXELS_PER_ROI] = [1.0, -1.0, 2.0, -2.0]
    return rows


def runwise_subject(subject, run_index, nroi):
    """Canonical condition patterns for one independent crossnobis run."""
    side = (2.0, -2.0, 0.25)[run_index]
    rows = []
    for condition in range(N_CONDITIONS):
        row = []
        for roi in range(nroi):
            for voxel in range(VOXELS_PER_ROI):
                value = (0.41 * condition * (voxel + 1) + 0.19 * roi *
                         ((condition + voxel + 1) % 3) + 0.07 * subject *
                         (condition - voxel) + 0.11 * run_index * (voxel + 1))
                # c00/c01 deliberately reverse between runs 0 and 1.  Their
                # crossnobis distance is therefore negative, as it should be.
                if condition == 0:
                    value += side
                elif condition == 1:
                    value -= side
                row.append(value)
        rows.append(row)
    return rows


def roi_features(rows, roi):
    start = roi * VOXELS_PER_ROI
    return [row[start:start + VOXELS_PER_ROI] for row in rows]


def roi_mean_timecourse(rows, roi):
    return [sum(values) / len(values) for values in roi_features(rows, roi)]


def assert_close(actual, expected, label, tol=3.0e-6):
    if not math.isfinite(actual) or abs(actual - expected) > tol:
        raise AssertionError("%s: got %.9g, expected %.9g (tol %.1g)" %
                             (label, actual, expected, tol))


def assert_matrix(actual, expected, label):
    if len(actual) != len(expected) or any(len(a) != len(b)
                                           for a, b in zip(actual, expected)):
        raise AssertionError("%s: matrix dimensions differ" % label)
    for ii, (arow, erow) in enumerate(zip(actual, expected)):
        for jj, (value, want) in enumerate(zip(arow, erow)):
            # THD_simmat_write_1D deliberately emits compact decimal text.
            assert_close(value, want, "%s[%d,%d]" % (label, ii, jj), tol=6.0e-5)


def run(command, work, threads):
    environment = dict(os.environ, OMP_NUM_THREADS=str(threads))
    completed = subprocess.run(command, cwd=work, env=environment, text=True,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if completed.returncode:
        raise RuntimeError("3dRSA failed (OMP_NUM_THREADS=%d):\n%s" %
                           (threads, completed.stdout))


def expect_failure(command, work, needle):
    completed = subprocess.run(command, cwd=work, text=True, stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
    if completed.returncode == 0 or needle not in completed.stdout:
        raise AssertionError("expected failure containing %r, got:\n%s" %
                             (needle, completed.stdout))


def make_table(path, files):
    with open(path, "w") as stream:
        stream.write("Subj InputFile\n")
        for subject, filename in enumerate(files):
            stream.write("s%02d %s'\n" % (subject, filename))


def table_effects(path, model_name, nroi):
    header, rows = read_rsa_table(path)
    required = {"ROI", "nvox", model_name + "_r"}
    missing = required.difference(header)
    if missing:
        raise AssertionError("%s lacks columns %s" % (path, sorted(missing)))
    if len(rows) != nroi:
        raise AssertionError("%s: got %d ROIs, expected %d" %
                             (path, len(rows), nroi))
    output = []
    for roi, row in enumerate(rows, 1):
        if int(row["ROI"]) != roi or int(row["nvox"]) != VOXELS_PER_ROI:
            raise AssertionError("%s: incorrect fake-atlas row %r" % (path, row))
        output.append(float(row[model_name + "_r"]))
    return output


def table_columns(path, model_name, nroi, columns):
    header, rows = read_rsa_table(path)
    names = [model_name + "_" + column for column in columns]
    if len(rows) != nroi or any(name not in header for name in names):
        raise AssertionError("%s lacks expected numeric columns %s" % (path, names))
    return {column: [float(row[name]) for row in rows] for column, name in zip(columns, names)}


def run_classic(binary, work, nroi, threads, expected):
    prefix = "classic_%02d_t%d" % (nroi, threads)
    command = [
        binary, "-mask", "atlas.1D'", "-mode", "RSA", "-dataTableFile", "classic.tbl",
        "-model_mat", "oracle", "model_conditions.1D", "-neural_metric", "euclid",
        "-metric", "pearson", "-nperm", "0", "-no_dset", "-quiet", "-prefix", prefix,
    ]
    run(command, work, threads)
    observed = table_effects(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi)
    for roi, (value, want) in enumerate(zip(observed, expected), 1):
        assert_close(value, want, "classic %d ROI %d" % (nroi, roi))
    return observed


def run_classic_custom(binary, work, nroi, table, model, metric, expected, tag,
                       mask="atlas.1D'"):
    prefix = "%s_%02d" % (tag, nroi)
    command = [
        binary, "-mask", mask, "-mode", "RSA", "-dataTableFile", table,
        "-model_mat", "oracle", model, "-neural_metric", "euclid", "-metric", metric,
        "-nperm", "0", "-no_dset", "-quiet", "-prefix", prefix,
    ]
    run(command, work, 1)
    observed = table_effects(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi)
    for roi, (value, want) in enumerate(zip(observed, expected), 1):
        assert_close(value, want, "%s %d ROI %d" % (tag, nroi, roi))
    return observed


def run_isrsa(binary, work, nroi, threads, table, featuretype, expected_effect,
              expected_rdm, tag, condition_metric=None, neural_metric="euclid"):
    prefix = "%s_%02d_t%d" % (tag, nroi, threads)
    save = "%s_saved_%02d_t%d" % (tag, nroi, threads)
    command = [
        binary, "-mask", "atlas.1D'", "-mode", "IS-RSA", "-featuretype", featuretype,
        "-dataTableFile", table, "-model_mat", "oracle", "model_subjects.1D",
        "-neural_metric", neural_metric, "-metric", "pearson", "-nperm", "0", "-no_dset",
        "-save_rdm", save, "-quiet", "-prefix", prefix,
    ]
    if condition_metric is not None:
        command.extend(["-condition_metric", condition_metric])
    run(command, work, threads)
    observed = table_effects(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi)
    for roi, (value, want) in enumerate(zip(observed, expected_effect), 1):
        assert_close(value, want, "%s %d ROI %d" % (tag, nroi, roi))
        matrix = read_matrix(os.path.join(work, "%s_roi%04d.1D" % (save, roi)))
        assert_matrix(matrix, expected_rdm[roi - 1], "%s %d ROI %d RDM" %
                      (tag, nroi, roi))
    return observed


def run_zcensor(binary, work, nroi, threads, expected_effect, expected_rdm, metric,
                preceding_polort=False):
    tag = "_reset_polort" if preceding_polort else ""
    prefix = "zcensor_%s%s_%02d_t%d" % (metric, tag, nroi, threads)
    save = "zcensor_%s%s_saved_%02d_t%d" % (metric, tag, nroi, threads)
    command = [
        binary, "-mask", "atlas.1D'", "-mode", "IS-RSA", "-featuretype", "mean",
        "-dataTableFile", "zcensor_mean.tbl", "-model_mat", "oracle", "model_subjects.1D",
        "-neural_metric", metric, "-metric", "pearson",
    ]
    if preceding_polort:
        command.extend(["-polort", "1"])
    command.extend(["-zcensor", "-nperm", "0", "-no_dset", "-save_rdm", save,
                    "-quiet", "-prefix", prefix])
    run(command, work, threads)
    observed = table_effects(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi)
    for roi, (value, want) in enumerate(zip(observed, expected_effect), 1):
        assert_close(value, want, "zcensor %s %d ROI %d" % (metric, nroi, roi))
        matrix = read_matrix(os.path.join(work, "%s_roi%04d.1D" % (save, roi)))
        assert_matrix(matrix, expected_rdm[roi - 1],
                      "zcensor %s %d ROI %d RDM" % (metric, nroi, roi))
    return observed


def make_long_table(work, rows_by_subject):
    """One selected fake-dataset brick per shuffled Subject x Condition row."""
    records = []
    for subject, rows in enumerate(rows_by_subject):
        for condition, row in enumerate(rows):
            name = "long_s%02d_c%02d.1D" % (subject, condition)
            write_1d(os.path.join(work, name), [row])
            records.append((subject, condition, name))
    with open(os.path.join(work, "classic_long.tbl"), "w") as stream:
        stream.write("Subj Condition InputFile\n")
        # Deliberately neither subject nor condition sorted.
        for subject, condition, name in reversed(records):
            stream.write("s%02d c%02d %s'\n" % (subject, condition, name))


def run_classic_long(binary, work, nroi, expected):
    prefix = "classic_long_%02d" % nroi
    order = ",".join("c%02d" % condition for condition in range(N_CONDITIONS))
    command = [
        binary, "-mask", "atlas.1D'", "-mode", "RSA", "-dataTableFile", "classic_long.tbl",
        "-condition_column", "Condition", "-condition_order", order,
        "-model_mat", "oracle", "model_conditions.1D", "-neural_metric", "euclid",
        "-metric", "pearson", "-nperm", "0", "-no_dset", "-quiet", "-prefix", prefix,
    ]
    run(command, work, 1)
    observed = table_effects(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi)
    for roi, (value, want) in enumerate(zip(observed, expected), 1):
        assert_close(value, want, "shuffled long classic %d ROI %d" % (nroi, roi))
    return observed


def make_runwise_tables(work, nroi):
    """Write balanced and ConditionFile-mapped crossnobis fixtures."""
    plain, mapped = [], []
    for subject in range(N_SUBJECTS):
        for run_index in range(3):
            canonical = runwise_subject(subject, run_index, nroi)
            plain_name = "run_plain_s%02d_r%d.1D" % (subject, run_index)
            write_1d(os.path.join(work, plain_name), canonical)
            plain.append((subject, run_index, plain_name))

            raw, labels = list(canonical), ["c%02d" % c for c in range(N_CONDITIONS)]
            if run_index == 0:
                # Two c00 beta rows, whose exact mean is the canonical c00.
                delta = [0.04 * (voxel + 1) for voxel in range(nroi * VOXELS_PER_ROI)]
                raw = [list(canonical[0]), list(canonical[0])] + [list(row) for row in canonical[1:]]
                raw[0] = [value + offset for value, offset in zip(raw[0], delta)]
                raw[1] = [value - offset for value, offset in zip(raw[1], delta)]
                labels = ["c00", "c00"] + labels[1:]
            elif run_index == 2:
                # c05 is absent in run 2: pairs involving c05 retain exactly
                # the two valid independent runs 0/1.
                raw = [list(row) for row in canonical[:-1]]
                labels = labels[:-1]
            name = "run_mapped_s%02d_r%d.1D" % (subject, run_index)
            cond = "run_mapped_s%02d_r%d.conditions" % (subject, run_index)
            write_1d(os.path.join(work, name), raw)
            with open(os.path.join(work, cond), "w") as stream:
                stream.write("\n".join(labels) + "\n")
            mapped.append((subject, run_index, name, cond))

    with open(os.path.join(work, "run_plain.tbl"), "w") as stream:
        stream.write("Subj Run InputFile\n")
        for subject, run_index, name in plain:
            stream.write("s%02d r%d %s'\n" % (subject, run_index, name))
    with open(os.path.join(work, "run_mapped.tbl"), "w") as stream:
        stream.write("Subj Run InputFile ConditionFile\n")
        for subject, run_index, name, cond in reversed(mapped):
            stream.write("s%02d r%d %s' %s\n" % (subject, run_index, name, cond))


def run_crossnobis(binary, work, nroi, table, expected, tag):
    prefix = "%s_%02d" % (tag, nroi)
    command = [
        binary, "-mask", "atlas.1D'", "-mode", "RSA", "-runwiseTable", table,
        "-model_mat", "oracle", "model_conditions.1D", "-metric", "pearson",
        "-nperm", "0", "-no_dset", "-quiet", "-prefix", prefix,
    ]
    run(command, work, 1)
    observed = table_effects(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi)
    for roi, (value, want) in enumerate(zip(observed, expected), 1):
        assert_close(value, want, "%s %d ROI %d" % (tag, nroi, roi))
    return observed


def run_exact_condition_null(binary, work, nroi, subject_rdms, model):
    """Compare the complete six-condition relabeling group, including FWE."""
    prefix = "classic_exact_%02d" % nroi
    command = [
        binary, "-mask", "atlas.1D'", "-mode", "RSA", "-dataTableFile", "classic.tbl",
        "-model_mat", "oracle", "model_conditions.1D", "-neural_metric", "euclid",
        "-metric", "pearson", "-classic_null", "conditions", "-nperm", "720", "-no_dset",
        "-quiet", "-prefix", prefix,
    ]
    run(command, work, 1)
    observed = table_columns(os.path.join(work, prefix + ".rsa.1D"), "oracle", nroi,
                             ("p", "pfwe"))
    all_stats = []
    for permutation in itertools.permutations(range(N_CONDITIONS)):
        ptri = [model[permutation[ii]][permutation[jj]]
                for ii in range(N_CONDITIONS) for jj in range(ii + 1, N_CONDITIONS)]
        all_stats.append([sum(math.atanh(pearson(triangle(subject_rdms[roi][subject]), ptri))
                              for subject in range(N_SUBJECTS)) / N_SUBJECTS
                          for roi in range(nroi)])
    identity = all_stats[0]
    maxima = [max(abs(value) for value in stats) for stats in all_stats]
    for roi in range(nroi):
        raw = sum(abs(stats[roi]) >= abs(identity[roi]) for stats in all_stats) / len(all_stats)
        fwe = sum(value >= abs(identity[roi]) for value in maxima) / len(maxima)
        assert_close(observed["p"][roi], raw, "exact condition p %d ROI %d" % (nroi, roi))
        assert_close(observed["pfwe"][roi], fwe,
                     "exact condition FWE %d ROI %d" % (nroi, roi))


def input_contract_failures(binary, work, nroi):
    """Protect the transpose convention and finite-data contract explicitly."""
    if nroi != 2:
        return
    with open(os.path.join(work, "classic_untransposed.tbl"), "w") as stream:
        stream.write("Subj InputFile\n")
        for subject in range(N_SUBJECTS):
            stream.write("s%02d %s\n" % (subject, "classic_s%02d.1D" % subject))
    base = [binary, "-mask", "atlas.1D'", "-mode", "RSA", "-dataTableFile",
            "classic_untransposed.tbl", "-model_mat", "oracle", "model_conditions.1D",
            "-nperm", "0", "-no_dset", "-quiet", "-prefix", "bad_orientation"]
    expect_failure(base, work, "voxels but the -mask has")

    nan_files = []
    for subject in range(N_SUBJECTS):
        name = "nan_s%02d.1D" % subject
        rows = classic_subject(subject, nroi)
        if subject == 0:
            rows[2][3] = float("nan")
        write_1d(os.path.join(work, name), rows)
        nan_files.append(name)
    make_table(os.path.join(work, "classic_nan.tbl"), nan_files)
    base[6] = "classic_nan.tbl"
    base[-1] = "bad_nan"
    expect_failure(base, work, "Failed parsing data row")

    zbase = [binary, "-mask", "atlas.1D'", "-mode", "IS-RSA", "-featuretype", "mean",
             "-dataTableFile", "zcensor_mean.tbl", "-model_mat", "oracle", "model_subjects.1D",
             "-neural_metric", "euclid", "-zcensor", "-nperm", "0", "-no_dset",
             "-quiet", "-prefix", "bad_zcensor_metric"]
    expect_failure(zbase, work, "only '-neural_metric corr'")

    bad_files = []
    for subject in range(N_SUBJECTS):
        name = "zcensor_too_few_s%02d.1D" % subject
        rows = zcensor_mean_subject(subject, nroi)
        if subject == 0:
            rows = [[0.0] * (nroi * VOXELS_PER_ROI) for _ in rows]
        write_1d(os.path.join(work, name), rows)
        bad_files.append(name)
    make_table(os.path.join(work, "zcensor_too_few.tbl"), bad_files)
    zbase[8] = "zcensor_too_few.tbl"
    zbase[13] = "corr"
    zbase[-1] = "bad_zcensor_count"
    expect_failure(zbase, work, "retained fewer than 3 jointly")


def exercise(binary, root, nroi):
    work = os.path.join(root, "roi_%02d" % nroi)
    os.mkdir(work)

    atlas = [[roi + 1 for roi in range(nroi) for _ in range(VOXELS_PER_ROI)]]
    write_1d(os.path.join(work, "atlas.1D"), atlas)

    cfiles, classic = [], []
    for subject in range(N_SUBJECTS):
        name = "classic_s%02d.1D" % subject
        rows = classic_subject(subject, nroi)
        write_1d(os.path.join(work, name), rows)
        cfiles.append(name)
        classic.append(rows)
    make_table(os.path.join(work, "classic.tbl"), cfiles)

    mcond = model_matrix(N_CONDITIONS)
    write_1d(os.path.join(work, "model_conditions.1D"), mcond)
    model_tri = triangle(mcond)
    classic_expected, classic_rdms = [], []
    for roi in range(nroi):
        rdms = [rdm(roi_features(rows, roi)) for rows in classic]
        classic_rdms.append(rdms)
        corr = [pearson(triangle(matrix), model_tri) for matrix in rdms]
        classic_expected.append(fisher_mean(corr))
    make_long_table(work, classic)

    def write_classic_variant(tag, transform):
        files = []
        for subject, rows in enumerate(classic):
            name = "%s_s%02d.1D" % (tag, subject)
            write_1d(os.path.join(work, name), transform(rows))
            files.append(name)
        table = tag + ".tbl"
        make_table(os.path.join(work, table), files)
        return table

    scaled_small = write_classic_variant(
        "classic_scaled_small", lambda rows: [[1.0e-8 * value for value in row] for row in rows])
    scaled_large = write_classic_variant(
        "classic_scaled_large", lambda rows: [[1.0e5 * value for value in row] for row in rows])
    common_offset = [0.31 * ((voxel % VOXELS_PER_ROI) - 1.5)
                     for voxel in range(nroi * VOXELS_PER_ROI)]
    translated = write_classic_variant(
        "classic_translated", lambda rows: [[value + offset for value, offset in zip(row, common_offset)]
                                             for row in rows])
    condition_permutation = (3, 0, 5, 1, 4, 2)
    reordered = write_classic_variant(
        "classic_condition_reordered", lambda rows: [rows[index] for index in condition_permutation])
    permuted_model = [[mcond[ii][jj] for jj in condition_permutation]
                      for ii in condition_permutation]
    write_1d(os.path.join(work, "model_conditions_reordered.1D"), permuted_model)
    voxel_permutation = tuple(reversed(range(nroi * VOXELS_PER_ROI)))
    write_1d(os.path.join(work, "atlas_voxel_reordered.1D"),
             [[atlas[0][index] for index in voxel_permutation]])
    voxel_reordered = write_classic_variant(
        "classic_voxel_reordered", lambda rows: [[row[index] for index in voxel_permutation]
                                                   for row in rows])
    with open(os.path.join(work, "classic_subject_reordered.tbl"), "w") as stream:
        stream.write("Subj InputFile\n")
        for subject in reversed(range(N_SUBJECTS)):
            stream.write("s%02d %s'\n" % (subject, cfiles[subject]))

    # Exact integer distances deliberately contain ties, so this checks that
    # the Spearman comparator uses fractional average ranks, not tie breaking.
    tie_positions = (0.0, 1.0, 3.0, 6.0, 10.0, 15.0)
    tie_rows = [[position] + [0.0] * (VOXELS_PER_ROI - 1)
                for position in tie_positions]
    tie_model = [[0.0 if ii == jj else float(((ii + jj) % 3) + ((ii * jj) % 2))
                  for jj in range(N_CONDITIONS)] for ii in range(N_CONDITIONS)]
    write_1d(os.path.join(work, "model_conditions_tied.1D"), tie_model)
    tied_tri = triangle(tie_model)
    tied_neural = triangle(rdm(tie_rows))
    tied_expected = [spearman(tied_neural, tied_tri)] * nroi
    tie_files = []
    for subject in range(N_SUBJECTS):
        name = "classic_tied_s%02d.1D" % subject
        write_1d(os.path.join(work, name), [row * nroi for row in tie_rows])
        tie_files.append(name)
    make_table(os.path.join(work, "classic_tied.tbl"), tie_files)

    constant_files = []
    for subject in range(N_SUBJECTS):
        name = "classic_constant_s%02d.1D" % subject
        write_1d(os.path.join(work, name), [[4.25] * (nroi * VOXELS_PER_ROI)
                                            for _ in range(N_CONDITIONS)])
        constant_files.append(name)
    make_table(os.path.join(work, "classic_constant.tbl"), constant_files)

    mfiles, means = [], []
    for subject in range(N_SUBJECTS):
        name = "mean_s%02d.1D" % subject
        rows = mean_subject(subject, nroi)
        write_1d(os.path.join(work, name), rows)
        mfiles.append(name)
        means.append(rows)
    make_table(os.path.join(work, "mean.tbl"), mfiles)

    zfiles, zmeans = [], []
    for subject in range(N_SUBJECTS):
        name = "zcensor_mean_s%02d.1D" % subject
        rows = zcensor_mean_subject(subject, nroi)
        write_1d(os.path.join(work, name), rows)
        zfiles.append(name)
        zmeans.append(rows)
    make_table(os.path.join(work, "zcensor_mean.tbl"), zfiles)

    tied_mfiles, tied_means = [], []
    for subject in range(N_SUBJECTS):
        name = "tied_mean_s%02d.1D" % subject
        rows = tied_mean_subject(subject, nroi)
        write_1d(os.path.join(work, name), rows)
        tied_mfiles.append(name)
        tied_means.append(rows)
    make_table(os.path.join(work, "tied_mean.tbl"), tied_mfiles)

    msub = model_matrix(N_SUBJECTS)
    write_1d(os.path.join(work, "model_subjects.1D"), msub)
    subject_model_tri = triangle(msub)
    isrsa_expected, isrsa_rdms = [], []
    for roi in range(nroi):
        features = [roi_mean_timecourse(rows, roi) for rows in means]
        neural = rdm(features)
        isrsa_rdms.append(neural)
        isrsa_expected.append(pearson(triangle(neural), subject_model_tri))

    scorr_expected, scorr_rdms = [], []
    for roi in range(nroi):
        features = [roi_mean_timecourse(rows, roi) for rows in tied_means]
        neural = spearman_simmat(features)
        scorr_rdms.append(neural)
        scorr_expected.append(pearson(triangle(neural), subject_model_tri))

    zcorr_expected, zcorr_rdms, zscorr_expected, zscorr_rdms = [], [], [], []
    for roi in range(nroi):
        features = [roi_mean_timecourse(rows, roi) for rows in zmeans]
        keep = [[any(value != 0.0 for value in roi_features([rows[timepoint]], roi)[0])
                 for timepoint in range(N_TIMEPOINTS)]
                for rows in zmeans]
        neural = zero_censored_simmat(features, keep, "corr")
        zcorr_rdms.append(neural)
        zcorr_expected.append(pearson(triangle(neural), subject_model_tri))
        neural = zero_censored_simmat(features, keep, "scorr")
        zscorr_rdms.append(neural)
        zscorr_expected.append(pearson(triangle(neural), subject_model_tri))

    pattern_expected, pattern_rdms = [], []
    second_expected, second_rdms = [], []
    for roi in range(nroi):
        pattern_features = [[value for row in rows for value in roi_features([row], roi)[0]]
                            for rows in classic]
        neural = rdm(pattern_features)
        pattern_rdms.append(neural)
        pattern_expected.append(pearson(triangle(neural), subject_model_tri))
        inner = [triangle(rdm(roi_features(rows, roi))) for rows in classic]
        neural = rdm(inner)
        second_rdms.append(neural)
        second_expected.append(pearson(triangle(neural), subject_model_tri))

    make_runwise_tables(work, nroi)
    cross_plain, cross_mapped = [], []
    negative_seen = False
    for roi in range(nroi):
        plain_subject = []
        mapped_subject = []
        for subject in range(N_SUBJECTS):
            runs = [roi_features(runwise_subject(subject, run_index, nroi), roi)
                    for run_index in range(3)]
            plain_subject.append(crossnobis(runs))
            present = [[True] * N_CONDITIONS, [True] * N_CONDITIONS,
                       [True] * (N_CONDITIONS - 1) + [False]]
            mapped_subject.append(crossnobis(runs, present))
        if any(value < 0.0 for matrix in plain_subject for value in triangle(matrix)):
            negative_seen = True
        cross_plain.append(fisher_mean([pearson(triangle(matrix), model_tri)
                                        for matrix in plain_subject]))
        cross_mapped.append(fisher_mean([pearson(triangle(matrix), model_tri)
                                         for matrix in mapped_subject]))
    if not negative_seen:
        raise AssertionError("crossnobis fixture failed to create a negative distance")

    c1 = run_classic(binary, work, nroi, 1, classic_expected)
    c2 = run_classic(binary, work, nroi, 2, classic_expected)
    cl = run_classic_long(binary, work, nroi, classic_expected)
    for tag, table, model in (("invariant_scaled_small", scaled_small, "model_conditions.1D"),
                              ("invariant_scaled_large", scaled_large, "model_conditions.1D"),
                              ("invariant_translated", translated, "model_conditions.1D"),
                              ("invariant_subject_order", "classic_subject_reordered.tbl", "model_conditions.1D"),
                              ("invariant_condition_order", reordered, "model_conditions_reordered.1D")):
        run_classic_custom(binary, work, nroi, table, model, "pearson", classic_expected, tag)
    run_classic_custom(binary, work, nroi, voxel_reordered, "model_conditions.1D", "pearson",
                       classic_expected, "invariant_voxel_order", mask="atlas_voxel_reordered.1D'")
    run_classic_custom(binary, work, nroi, "classic_tied.tbl", "model_conditions_tied.1D",
                       "spearman", tied_expected, "invariant_spearman_ties")
    run_classic_custom(binary, work, nroi, "classic_constant.tbl", "model_conditions.1D",
                       "pearson", [0.0] * nroi, "edge_constant_patterns")
    i1 = run_isrsa(binary, work, nroi, 1, "mean.tbl", "mean", isrsa_expected,
                    isrsa_rdms, "isrsa_mean")
    i2 = run_isrsa(binary, work, nroi, 2, "mean.tbl", "mean", isrsa_expected,
                    isrsa_rdms, "isrsa_mean")
    s1 = run_isrsa(binary, work, nroi, 1, "tied_mean.tbl", "mean", scorr_expected,
                    scorr_rdms, "isrsa_scorr", neural_metric="scorr")
    z1 = run_zcensor(binary, work, nroi, 1, zcorr_expected, zcorr_rdms, "corr")
    z2 = run_zcensor(binary, work, nroi, 2, zcorr_expected, zcorr_rdms, "corr")
    zp = run_zcensor(binary, work, nroi, 1, zcorr_expected, zcorr_rdms, "corr",
                     preceding_polort=True)
    zs = run_zcensor(binary, work, nroi, 1, zscorr_expected, zscorr_rdms, "scorr")
    p1 = run_isrsa(binary, work, nroi, 1, "classic.tbl", "pattern", pattern_expected,
                    pattern_rdms, "isrsa_pattern")
    r1 = run_isrsa(binary, work, nroi, 1, "classic.tbl", "rdm", second_expected,
                    second_rdms, "isrsa_rdm", condition_metric="euclid")
    xplain = run_crossnobis(binary, work, nroi, "run_plain.tbl", cross_plain,
                            "crossnobis_plain")
    xmap = run_crossnobis(binary, work, nroi, "run_mapped.tbl", cross_mapped,
                          "crossnobis_mapped")
    run_exact_condition_null(binary, work, nroi, classic_rdms, mcond)
    input_contract_failures(binary, work, nroi)
    for label, a, b in (("classic", c1, c2), ("IS-RSA", i1, i2),
                        ("zcensor", z1, z2)):
        for roi, (left, right) in enumerate(zip(a, b), 1):
            assert_close(left, right, "%s OMP agreement, %d ROIs, ROI %d" %
                         (label, nroi, roi), tol=0.0)
    for label, observed, expected in (("shuffled long classic", cl, classic_expected),
                                      ("IS-RSA pattern", p1, pattern_expected),
                                      ("IS-RSA scorr", s1, scorr_expected),
                                      ("zcensor corr", z1, zcorr_expected),
                                      ("zcensor polort reset", zp, zcorr_expected),
                                      ("zcensor scorr", zs, zscorr_expected),
                                      ("IS-RSA second-order", r1, second_expected),
                                      ("crossnobis plain", xplain, cross_plain),
                                      ("crossnobis mapped", xmap, cross_mapped)):
        for roi, (value, want) in enumerate(zip(observed, expected), 1):
            assert_close(value, want, "%s %d ROI %d" % (label, nroi, roi))
    print("PASS 1D oracle: %2d ROIs (classic/IS-RSA/zcensor/crossnobis/exact null; OMP 1/2)" %
          nroi)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bin", required=True, help="path to 3dRSA")
    parser.add_argument("--work", help="preserve fixtures in this directory")
    args = parser.parse_args()
    binary = os.path.abspath(args.bin)
    if not os.path.isfile(binary) or not os.access(binary, os.X_OK):
        raise SystemExit("3dRSA binary is not executable: %s" % binary)

    owned = args.work is None
    root = tempfile.mkdtemp(prefix="3drsa-1d-oracle-") if owned else os.path.abspath(args.work)
    if not owned:
        os.makedirs(root, exist_ok=True)
    try:
        for nroi in (2, 5, 10):
            exercise(binary, root, nroi)
    finally:
        if owned:
            shutil.rmtree(root)


if __name__ == "__main__":
    main()
