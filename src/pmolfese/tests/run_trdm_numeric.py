#!/usr/bin/env python3
"""Independent numeric and contract regression gate for 1dTrdm."""

import argparse
import csv
import os
import shutil
import subprocess
import sys
import tempfile


def run(cmd, cwd=None, env=None):
    p = subprocess.run(cmd, cwd=cwd, env=env, stdout=subprocess.PIPE,
                       stderr=subprocess.STDOUT, text=True)
    return p.returncode, p.stdout


def read_long(path):
    header = None
    rows = []
    with open(path) as f:
        for line in f:
            if not line.strip() or line.startswith("#"):
                continue
            tok = line.split()
            if header is None:
                header = tok
            else:
                rows.append(dict(zip(header, tok)))
    return rows


def read_rsa_series(path):
    header = None
    rows = []
    with open(path) as f:
        for line in f:
            if line.startswith("#ROI"):
                header = line[1:].split()
            elif line.strip() and not line.startswith("#") and header is not None:
                rows.append(dict(zip(header, line.split())))
    return rows


def write_axes(root, nt, nf):
    time = os.path.join(root, "time.txt")
    feat = os.path.join(root, "features.txt")
    with open(time, "w") as f:
        f.write("time_index time_value time_unit time_label\n")
        for t in range(nt):
            f.write("%d %.6f s t%03d\n" % (t, -0.1 + 0.05 * t, t))
    with open(feat, "w") as f:
        f.write("feature_label Sensor Region\n")
        for j in range(nf):
            f.write("f%02d E%02d R%d\n" % (j, j, j % 2))
    return time, feat


def make_fixture(root):
    import numpy as np
    rng = np.random.default_rng(20260829)
    ns, npart, nc, nr, nt, nf = 4, 3, 6, 2, 5, 4
    cond = rng.normal(size=(nc, nt, nf))
    # A transient representation at t0 recurs at t1 and t3; t2 is distinct.
    cond[:, 1, :] = cond[:, 0, :] + rng.normal(scale=0.02, size=(nc, nf))
    cond[:, 3, :] = cond[:, 0, :] + rng.normal(scale=0.02, size=(nc, nf))
    subj = rng.normal(scale=0.35, size=(ns, 1, 1, nt, nf))
    part = rng.normal(scale=0.18, size=(ns, npart, 1, nt, nf))
    noise = rng.normal(scale=0.08, size=(ns, npart, nc, nr, nt, nf))
    data = cond[None, None, :, None, :, :] + subj[:, :, :, None, :, :] + \
           part[:, :, :, None, :, :] + noise
    # Reference the actual float tokens consumed by mri_read_1D.
    stored = np.empty_like(data, dtype=np.float32)
    records = []
    for s in range(ns):
        for p in range(npart):
            for c in range(nc):
                for r in range(nr):
                    rel = "obs_s%02d_p%02d_c%02d_r%02d.1D" % (s, p, c, r)
                    np.savetxt(os.path.join(root, rel), data[s, p, c, r], fmt="%.9g")
                    stored[s, p, c, r] = np.loadtxt(os.path.join(root, rel), dtype=np.float32)
                    records.append(("s%02d" % s, "o_p%02d_c%02d_r%02d" % (p, c, r),
                                    "c%02d" % c, "p%02d" % p, rel))
    obs = os.path.join(root, "observations.txt")
    with open(obs, "w") as f:
        f.write("Subj Observation Condition Partition InputFile\n")
        for row in records:
            f.write("%s %s %s %s %s\n" % row)
    shuffled = os.path.join(root, "observations_shuffled.txt")
    order = rng.permutation(len(records))
    with open(shuffled, "w") as f:
        f.write("Subj Observation Condition Partition InputFile\n")
        for i in order:
            f.write("%s %s %s %s %s\n" % records[i])
    time, feat = write_axes(root, nt, nf)
    neighborhoods = os.path.join(root, "neighborhoods.txt")
    neighborhood_rows = [("right", "f03"), ("left", "f01"), ("right", "f01"),
                         ("left", "f00"), ("right", "f02"), ("left", "f02")]
    with open(neighborhoods, "w") as f:
        f.write("Neighborhood Feature\n")
        for row in neighborhood_rows:
            f.write("%s %s\n" % row)
    neighborhoods_shuffled = os.path.join(root, "neighborhoods_shuffled.txt")
    with open(neighborhoods_shuffled, "w") as f:
        f.write("Neighborhood Feature\n")
        for i in rng.permutation(len(neighborhood_rows)):
            f.write("%s %s\n" % neighborhood_rows[i])
    groups = np.asarray([0, 0, 1, 1, 2, 2])
    model = (groups[:, None] != groups[None, :]).astype(np.float32)
    mord = np.asarray([2, 5, 0, 4, 1, 3])
    modelfile = os.path.join(root, "temporal_model.1D")
    modelaxis = os.path.join(root, "temporal_model_conditions.1D")
    np.savetxt(modelfile, model[np.ix_(mord, mord)], fmt="%.1f")
    with open(modelaxis, "w") as f:
        f.write("ConditionIndex Condition\n")
        for i, c in enumerate(mord):
            f.write("%d c%02d\n" % (i, c))
    return dict(ns=ns, npart=npart, nc=nc, nr=nr, nt=nt, nf=nf,
                x=stored, records=records, obs=obs, shuffled=shuffled,
                time=time, feat=feat, model=model, modelfile=modelfile,
                modelaxis=modelaxis, neighborhoods=neighborhoods,
                neighborhoods_shuffled=neighborhoods_shuffled,
                neighborhood_labels=["left", "right"],
                neighborhood_features=[[0, 1, 2], [1, 2, 3]])


def windowed(x, start, width, reduce):
    import numpy as np
    z = x[..., start:start + width, :]
    if reduce == "mean":
        return z.mean(axis=-2, dtype=np.float32)
    return z.reshape(z.shape[:-2] + (width * z.shape[-1],))


def reference(fx, metric, width=1, step=1, reduce="mean", center="none", features=None):
    import numpy as np
    x = fx["x"] if features is None else fx["x"][..., features]
    out = []
    for s in range(fx["ns"]):
        sm = []
        for start in range(0, fx["nt"] - width + 1, step):
            w = windowed(x[s], start, width, reduce)
            if metric == "crossnobis":
                pat = w.mean(axis=2, dtype=np.float32)  # partition x condition x feature
                if center == "partition":
                    pat = pat - pat.mean(axis=1, keepdims=True, dtype=np.float32)
                d = np.zeros((fx["nc"], fx["nc"]), float)
                for a in range(fx["nc"]):
                    for b in range(a + 1, fx["nc"]):
                        delta = pat[:, a] - pat[:, b]
                        val = sum(np.dot(delta[p].astype(float), delta[q].astype(float))
                                  for p in range(fx["npart"])
                                  for q in range(fx["npart"]) if p != q)
                        val /= fx["npart"] * (fx["npart"] - 1) * delta.shape[1]
                        d[a, b] = d[b, a] = val
            else:
                pat = w.mean(axis=(0, 2), dtype=np.float32)
                if center == "subject":
                    pat = pat - pat.mean(axis=0, keepdims=True, dtype=np.float32)
                if metric == "corr":
                    d = 1.0 - np.corrcoef(pat)
                elif metric == "cosine":
                    n = np.sqrt((pat.astype(float) ** 2).sum(axis=1))
                    d = 1.0 - pat.dot(pat.T) / np.outer(n, n)
                else:
                    d = np.sqrt(((pat[:, None].astype(float) -
                                  pat[None, :].astype(float)) ** 2).sum(axis=2))
                np.fill_diagonal(d, 0.0)
            sm.append(d)
        out.append(sm)
    return np.asarray(out)


def matrices(root, prefix, fx, nwin):
    import numpy as np
    out = []
    for s in range(fx["ns"]):
        sm = []
        for t in range(nwin):
            sm.append(np.loadtxt("%s_s%04d_s%02d_t%04d.1D" % (prefix, s, s, t)))
        out.append(sm)
    return np.asarray(out)


def long_matrices(path, fx):
    """Rebuild float RDMs from the canonical nine-digit long output."""
    import numpy as np
    out = np.zeros((fx["ns"], fx["nt"], fx["nc"], fx["nc"]), np.float32)
    for row in read_long(path):
        s, t = int(row["Subj"][1:]), int(row["TimeIndex"])
        a, b = int(row["ConditionA"][1:]), int(row["ConditionB"][1:])
        out[s, t, a, b] = out[s, t, b, a] = np.float32(row["Dissimilarity"])
    return out


def dynamics_reference(rdm, compare):
    import numpy as np
    from scipy.stats import rankdata
    ns, nt, nc, _ = rdm.shape
    iu = np.triu_indices(nc, 1)
    out = np.zeros((ns, nt, nt), float)
    for s in range(ns):
        tri = [rdm[s, t][iu] for t in range(nt)]
        if compare == "spearman":
            tri = [rankdata(x, method="average") for x in tri]
        for a in range(nt):
            for b in range(nt):
                out[s, a, b] = np.corrcoef(tri[a], tri[b])[0, 1]
    return out


def cross_time_reference(fx, width=1, step=1, reduce="mean", center="none", features=None):
    import numpy as np
    starts = range(0, fx["nt"] - width + 1, step)
    nw = len(list(starts)); out = np.zeros((fx["ns"], nw, nw, fx["nc"], fx["nc"]), float)
    for s in range(fx["ns"]):
        pat = []
        for start in starts:
            raw = fx["x"][s] if features is None else fx["x"][s][..., features]
            z = windowed(raw, start, width, reduce).mean(axis=2, dtype=np.float32)
            if center == "partition":
                z = z - z.mean(axis=1, keepdims=True, dtype=np.float32)
            pat.append(z)
        for a in range(nw):
            for b in range(nw):
                for ca in range(fx["nc"]):
                    for cb in range(ca + 1, fx["nc"]):
                        val = 0.0
                        for p in range(fx["npart"]):
                            da = pat[a][p, ca].astype(float) - pat[a][p, cb].astype(float)
                            for q in range(fx["npart"]):
                                if p != q:
                                    db = pat[b][q, ca].astype(float) - pat[b][q, cb].astype(float)
                                    val += np.dot(da, db)
                        val /= fx["npart"] * (fx["npart"] - 1) * len(da)
                        out[s, a, b, ca, cb] = out[s, a, b, cb, ca] = val
    return out


def read_dynamics(path, fx, nwin):
    import numpy as np
    out = np.zeros((fx["ns"], nwin, nwin), float)
    for row in read_long(path):
        s, a, b = int(row["Subj"][1:]), int(row["TimeAIndex"]), int(row["TimeBIndex"])
        out[s, a, b] = out[s, b, a] = float(row["Similarity"])
    return out


def read_cross_time(path, fx, nwin):
    import numpy as np
    out = np.zeros((fx["ns"], nwin, nwin, fx["nc"], fx["nc"]), float)
    for row in read_long(path):
        s, ta, tb = int(row["Subj"][1:]), int(row["TimeAIndex"]), int(row["TimeBIndex"])
        ca, cb = int(row["ConditionA"][1:]), int(row["ConditionB"][1:])
        v = float(row["Crossnobis"])
        out[s, ta, tb, ca, cb] = out[s, ta, tb, cb, ca] = v
        out[s, tb, ta, ca, cb] = out[s, tb, ta, cb, ca] = v
    return out


def read_neighborhood_rdms(path, fx, nwin):
    import numpy as np
    ng = len(fx["neighborhood_labels"])
    out = np.zeros((fx["ns"], ng, nwin, fx["nc"], fx["nc"]), np.float32)
    for row in read_long(path):
        s, g, t = int(row["Subj"][1:]), int(row["NeighborhoodIndex"]), int(row["TimeIndex"])
        a, b = int(row["ConditionA"][1:]), int(row["ConditionB"][1:])
        out[s, g, t, a, b] = out[s, g, t, b, a] = np.float32(row["Dissimilarity"])
    return out


def read_neighborhood_dynamics(path, fx, nwin):
    import numpy as np
    ng = len(fx["neighborhood_labels"]); out = np.zeros((fx["ns"], ng, nwin, nwin), float)
    for row in read_long(path):
        s, g = int(row["Subj"][1:]), int(row["NeighborhoodIndex"])
        a, b, v = int(row["TimeAIndex"]), int(row["TimeBIndex"]), float(row["Similarity"])
        out[s, g, a, b] = out[s, g, b, a] = v
    return out


def read_neighborhood_cross_time(path, fx, nwin):
    import numpy as np
    ng = len(fx["neighborhood_labels"])
    out = np.zeros((fx["ns"], ng, nwin, nwin, fx["nc"], fx["nc"]), float)
    for row in read_long(path):
        s, g = int(row["Subj"][1:]), int(row["NeighborhoodIndex"])
        ta, tb = int(row["TimeAIndex"]), int(row["TimeBIndex"])
        ca, cb, v = int(row["ConditionA"][1:]), int(row["ConditionB"][1:]), float(row["Crossnobis"])
        out[s, g, ta, tb, ca, cb] = out[s, g, ta, tb, cb, ca] = v
        out[s, g, tb, ta, ca, cb] = out[s, g, tb, ta, cb, ca] = v
    return out


def trdm_cmd(binary, fx, prefix, metric, width=1, step=1, reduce="mean",
             center="none", obs=None, jobs=1, series=False):
    cmd = [binary, "-dataTable", obs or fx["obs"], "-time_axis", fx["time"],
           "-feature_axis", fx["feat"], "-metric", metric, "-prefix", prefix,
           "-window_width", str(width), "-window_step", str(step),
           "-window_reduce", reduce, "-center_conditions", center,
           "-jobs", str(jobs), "-quiet"]
    if series:
        cmd += ["-model_series_out", "independent" if series is True else series]
    return cmd


def check_labeled_inputs(binary, fx, work, check):
    """Exercise the labeled loader against the existing numeric input path."""
    import numpy as np
    records = []
    for i, row in enumerate(fx["records"]):
        path = "labeled_" + row[-1]
        values = [line.split() for line in open(os.path.join(work, row[-1]))]
        # Establish the familiar order in the first file, shuffle subsequent
        # files independently, and move the time columns among the features.
        order = list(range(fx["nf"]))
        if i:
            order = np.random.default_rng(i).permutation(fx["nf"]).tolist()
        with open(os.path.join(work, path), "w") as f:
            f.write(("# Columns: " if i % 2 else "") +
                    "time_value " + " ".join("f%02d" % j for j in order) +
                    " time_unit time_label time_index\n")
            for t, vals in enumerate(values):
                f.write("%.6f %s s t%03d %d\n" %
                        (-0.1 + 0.05 * t, " ".join(vals[j] for j in order), t, t))
        records.append(row[:-1] + (path,))

    obs = os.path.join(work, "labeled_observations.txt")

    def write_records(path, rows):
        with open(path, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for row in rows:
                f.write("%s %s %s %s %s\n" % row)

    write_records(obs, records)

    def command(prefix, metric="corr", table=obs, axes=(), **kwargs):
        cmd = trdm_cmd(binary, fx, prefix, metric, obs=table, **kwargs)
        for opt in ("-time_axis", "-feature_axis"):
            if opt not in axes:
                ix = cmd.index(opt)
                del cmd[ix:ix + 2]
        return cmd + ["-input_format", "labeled"]

    cases = [("corr", 1, "mean", "none"), ("cosine", 2, "mean", "subject"),
             ("euclid", 2, "concat", "none"), ("crossnobis", 2, "concat", "partition")]
    for metric, width, reduce, center in cases:
        old = os.path.join(work, "labeled_reference_" + metric)
        new = os.path.join(work, "labeled_result_" + metric)
        extra = ["-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                 "-nperm", "16", "-seed", "123", "-rdm_dynamics", "pearson"]
        rc, out = run(trdm_cmd(binary, fx, old, metric, width=width, reduce=reduce,
                              center=center) + extra)
        rn, on = run(command(new, metric, width=width, reduce=reduce, center=center) + extra,
                     cwd=os.path.dirname(work))
        suffixes = [".trdm.1D", ".trdm.time.1D", ".trdm.features.1D",
                    ".trdm.fits.1D", ".trdm.inference.1D", ".trdm.dynamics.1D"]
        check("labeled %s equals numeric RDMs, inference, dynamics and axes" % metric,
              rc == rn == 0 and all(read_long(old + suf) == read_long(new + suf)
                                    for suf in suffixes), (out + on)[-500:])

    reference_rows = read_long(os.path.join(work, "labeled_reference_corr.trdm.1D"))
    for ix, axes in enumerate([("-time_axis",), ("-feature_axis",),
                               ("-time_axis", "-feature_axis")]):
        prefix = os.path.join(work, "labeled_external_%d" % ix)
        rc, out = run(command(prefix, axes=axes))
        check("labeled agrees with supplied " + ", ".join(axes),
              rc == 0 and read_long(prefix + ".trdm.1D") == reference_rows, out[-500:])

    # A supplied feature axis controls order, including neighborhood lookup.
    reverse_axis = os.path.join(work, "labeled_reverse_features.txt")
    with open(reverse_axis, "w") as f:
        f.write("feature_label\n" + "\n".join("f%02d" % j for j in reversed(range(fx["nf"]))) + "\n")
    prefix = os.path.join(work, "labeled_reverse_axis")
    rc, out = run(command(prefix) + ["-feature_axis", reverse_axis,
                                    "-feature_neighborhoods", fx["neighborhoods"]])
    got = read_neighborhood_rdms(prefix + ".trdm.neighborhoods.1D", fx, fx["nt"]) if rc == 0 else None
    check("labeled external feature order and neighborhood membership",
          rc == 0 and [r["FeatureLabel"] for r in read_long(prefix + ".trdm.features.1D")] ==
          ["f%02d" % j for j in reversed(range(fx["nf"]))] and
          all(np.allclose(got[:, g], reference(fx, "corr", features=features), atol=3e-6)
              for g, features in enumerate(fx["neighborhood_features"])), out[-500:])

    minimal = []
    for row in records:
        lines = open(os.path.join(work, row[-1])).read().splitlines()
        header = lines[0].removeprefix("# Columns: ").split()
        src = [dict(zip(header, line.split())) for line in lines[1:]]
        path = "minimal_" + row[-1]
        with open(os.path.join(work, path), "w") as f:
            f.write("time_value time_unit " + " ".join("f%02d" % j for j in range(fx["nf"])) + "\n")
            for cell in src:
                f.write("%s s %s\n" % (cell["time_value"],
                        " ".join(cell["f%02d" % j] for j in range(fx["nf"]))))
        minimal.append(row[:-1] + (path,))
    minimal_obs = os.path.join(work, "labeled_minimal.txt")
    write_records(minimal_obs, minimal)
    for external in (False, True):
        prefix = os.path.join(work, "labeled_minimal_%d" % external)
        rc, out = run(command(prefix, table=minimal_obs, axes=("-time_axis",) if external else ()))
        times = read_long(prefix + ".trdm.time.1D") if rc == 0 else []
        labels = [r["TimeLabel"] for r in times]
        check("labeled optional index/labels: " + ("inherit external axis" if external else "generate labels"),
              rc == 0 and labels == (["t%03d" % t for t in range(fx["nt"])] if external else
                                     [format(float("%.6f" % (-0.1 + 0.05*t)), ".6g") + "s"
                                      for t in range(fx["nt"])]), out[-500:])
        if rc == 0:
            meta = open(prefix + ".trdm.meta").read()
            check("labeled resolved axis provenance %d" % external,
                  "input_format labeled" in meta and "(null)" not in meta and
                  "feature_axis_origin embedded" in meta)

    original = open(os.path.join(work, records[-1][-1])).read()
    failures = [
        ("duplicate feature", original.replace("f01", "f00"), "duplicate column"),
        ("case-colliding feature", original.replace("f01", "F00"), "duplicate column"),
        ("unknown feature", original.replace("f01", "other"), "missing feature"),
        ("feature case mismatch", original.replace("f01", "F01"), "missing feature"),
        ("missing time column", original.replace("time_value", "latency"), "requires time_value"),
        ("time mismatch", original.replace("-0.100000", "-0.110000"), "time_value disagrees"),
        ("unit mismatch", original.replace(" s ", " ms "), "time_unit"),
        ("label mismatch", original.replace("t000", "early"), "time_label disagrees"),
        ("invalid index", original.replace("t000 0", "t000 1"), "time_index"),
        ("nonfinite time", original.replace("-0.100000", "nan"), "invalid time_value"),
        ("missing time row", "\n".join(original.splitlines()[:-1]) + "\n", "time rows"),
    ]
    for bad in ("nan", "inf", "1e100", "text"):
        lines = original.splitlines()
        cells = lines[1].split(); cells[1] = bad; lines[1] = " ".join(cells)
        failures.append(("invalid feature " + bad, "\n".join(lines) + "\n", "column"))
    for ix, (name, contents, message) in enumerate(failures):
        badpath = "labeled_bad_%d.1D" % ix
        with open(os.path.join(work, badpath), "w") as f:
            f.write(contents)
        badobs = os.path.join(work, "labeled_bad_%d.txt" % ix)
        write_records(badobs, records[:-1] + [records[-1][:-1] + (badpath,)])
        rc, out = run(command(os.path.join(work, "labeled_bad_out_%d" % ix), table=badobs))
        check("labeled rejects " + name, rc != 0 and message in out and badpath in out, out[-500:])

    # The first observation must validate inferred axes, not merely establish
    # a malformed reference that every later file is compared against.
    original = open(os.path.join(work, records[0][-1])).read()
    failures = [
        ("duplicate time labels", original.replace("t001", "t000"), "repeats time_label"),
        ("nonincreasing times", original.replace("-0.050000", "-0.200000"), "strictly increasing"),
        ("empty unit", original.replace(" s ", ' "" '), "time_unit"),
        ("empty label", original.replace("t000", '""'), "single-token time_label"),
    ]
    for ix, (name, contents, message) in enumerate(failures):
        badpath = "labeled_bad_axis_%d.1D" % ix
        with open(os.path.join(work, badpath), "w") as f:
            f.write(contents)
        badobs = os.path.join(work, "labeled_bad_axis_%d.txt" % ix)
        write_records(badobs, [records[0][:-1] + (badpath,)] + records[1:])
        rc, out = run(command(os.path.join(work, "labeled_bad_axis_out_%d" % ix), table=badobs))
        check("labeled rejects " + name, rc != 0 and message in out and badpath in out, out[-500:])


def read_labeled_square(path):
    """Read the documented square-table contract independently of AFNI."""
    import numpy as np
    header, labels, values, kind = None, [], [], None
    with open(path) as stream:
        for line in stream:
            if line.startswith("# matrix_kind "):
                kind = line.split()[-1]
            elif line.startswith("# Columns: "):
                header = next(csv.reader([line[len("# Columns: "):].strip()],
                                        delimiter=" ", skipinitialspace=True))
            elif line.strip() and not line.startswith("#"):
                fields = next(csv.reader([line.strip()], delimiter=" ", skipinitialspace=True))
                labels.append(fields[0]); values.append([float(x) for x in fields[1:]])
    if header is None or len(values) != len(header) - 1:
        raise AssertionError("invalid labeled square table: " + path)
    return header[0], header[1:], labels, np.asarray(values, dtype=np.float32), kind


def check_labeled_matrices(binary, fx, work, check):
    import numpy as np
    cond = ["c%02d" % c for c in range(fx["nc"])]
    for mode in ("independent", "paired", "loo", "neighborhood"):
        prefix = os.path.join(work, "square_labeled_" + mode)
        cmd = trdm_cmd(binary, fx, prefix, "crossnobis", width=2,
                       reduce="concat", center="partition",
                       series=False if mode == "neighborhood" else mode)
        cmd += ["-matrix_format", "labeled", "-rdm_dynamics", "pearson"]
        if mode == "neighborhood":
            cmd += ["-feature_neighborhoods", fx["neighborhoods"]]
        rc, out = run(cmd)
        if rc:
            check("labeled square " + mode, False, out[-500:])
            continue
        nwin = fx["nt"] - 1
        ref = reference(fx, "crossnobis", width=2, reduce="concat", center="partition")
        windows = ["w%04d" % t for t in range(nwin)]
        good = True
        for s in range(fx["ns"]):
            for t in range(nwin):
                path = "%s_s%04d_s%02d_t%04d.1D" % (prefix, s, s, t)
                axis, cols, rows, mat, kind = read_labeled_square(path)
                good &= (axis == "Condition" and cols == rows == cond and kind == "dissimilarity"
                         and np.allclose(mat, ref[s, t], atol=3e-6, rtol=3e-6))
            axis, cols, rows, mat, kind = read_labeled_square("%s_s%04d_s%02d_dynamics.1D" % (prefix, s, s))
            good &= (axis == "Time" and cols == rows == windows and kind == "similarity"
                     and np.allclose(mat, dynamics_reference(ref, "pearson")[s], atol=3e-6))
        check("labeled subject RDM and window-dynamics axes: " + mode, good)
        if mode == "neighborhood":
            good = True
            for g, name in enumerate(fx["neighborhood_labels"]):
                nr = reference(fx, "crossnobis", width=2, reduce="concat", center="partition",
                               features=fx["neighborhood_features"][g])
                for s in range(fx["ns"]):
                    stem = "%s_s%04d_s%02d_n%04d_%s" % (prefix, s, s, g, name)
                    for t in range(nwin):
                        axis, cols, rows, mat, kind = read_labeled_square(stem + "_t%04d.1D" % t)
                        good &= cols == rows == cond and np.allclose(mat, nr[s, t], atol=3e-6, rtol=3e-6)
                    axis, cols, rows, mat, kind = read_labeled_square(stem + "_dynamics.1D")
                    good &= cols == rows == windows and np.allclose(mat, dynamics_reference(nr, "pearson")[s], atol=3e-6)
            check("labeled neighborhood RDMs and dynamics", good)
        else:
            suffix = ".model_series.1D" if mode == "independent" else ".%s_model_series.1D" % mode
            manifest = read_long(prefix + suffix)
            good = True
            for row in manifest:
                t = windows.index(row["Time"])
                axis, cols, rows, mat, kind = read_labeled_square(os.path.join(work, row["ModelFile"]))
                if mode == "independent":
                    expected = ref.mean(axis=0)[t]
                else:
                    s = int(row["Subj"][1:])
                    expected = ref[s, t] if mode == "paired" else np.delete(ref, s, axis=0).mean(axis=0)[t]
                good &= cols == rows == cond and np.allclose(mat, expected, atol=3e-6, rtol=3e-6)
            check("labeled " + mode + " manifest resolves to correct matrices", good)
        check("square format recorded in provenance: " + mode,
              "matrix_format labeled" in open(prefix + ".trdm.meta").read())
        rc, out = run(cmd)
        check("labeled square overwrite protection: " + mode, rc != 0 and "exists" in out)

    # Exercise quoting and preserve negative crossnobis estimates with a tiny,
    # deliberately opposing pair of independent partition contrasts.
    names = ['face one', '#house', 'say "hi"']
    obs = os.path.join(work, "square_labels.txt")
    with open(obs, "w") as stream:
        writer = csv.writer(stream, delimiter=" ", lineterminator="\n", quoting=csv.QUOTE_ALL)
        writer.writerow(["Subj", "Observation", "Condition", "Partition", "InputFile"])
        for p in range(2):
            for c, name in enumerate(names):
                path = "square_p%d_c%d.1D" % (p, c)
                np.savetxt(os.path.join(work, path), np.full((fx["nt"], fx["nf"]), (-1)**p*c))
                writer.writerow(["s00", "p%d_c%d" % (p, c), name, "p%d" % p, path])
    prefix = os.path.join(work, "square_quoted")
    rc, out = run(trdm_cmd(binary, fx, prefix, "crossnobis", obs=obs) + ["-matrix_format", "labeled"])
    if rc == 0:
        axis, cols, rows, mat, kind = read_labeled_square(prefix + "_s0000_s00_t0000.1D")
        check("quoted labels and negative crossnobis survive square output",
              cols == rows == sorted(names) and np.all(mat[np.triu_indices(3, 1)] < 0)
              and np.array_equal(mat, mat.T) and np.all(np.diag(mat) == 0))
    else:
        check("quoted labels and negative crossnobis survive square output", False, out[-500:])


def bh_ref(p):
    import numpy as np
    p = np.asarray(p, float); order = np.argsort(p); q = np.empty_like(p); qmin = 1.0
    for rank in range(len(order) - 1, -1, -1):
        qmin = min(qmin, p[order[rank]] * len(order) / (rank + 1.0))
        q[order[rank]] = min(1.0, qmin)
    return q


def infer_reference(rdm, model, null_type, compare="spearman"):
    import itertools
    import numpy as np
    from scipy.stats import rankdata
    ns, nt, nc, _ = rdm.shape; iu = np.triu_indices(nc, 1); mt = model[iu]

    def pearson32(y, x):
        y, x = np.asarray(y, np.float32), np.asarray(x, np.float32)
        ym = np.float32(0); xm = np.float32(0)
        for i in range(len(y)):
            ym = np.float32(ym + y[i]); xm = np.float32(xm + x[i])
        ym = np.float32(ym / len(y)); xm = np.float32(xm / len(x))
        yy = np.float32(0); xx = np.float32(0); xy = np.float32(0)
        for i in range(len(y)):
            a, b = np.float32(y[i] - ym), np.float32(x[i] - xm)
            yy = np.float32(yy + np.float32(a * a))
            xx = np.float32(xx + np.float32(b * b))
            xy = np.float32(xy + np.float32(a * b))
        return np.float32(0 if yy <= 0 or xx <= 0 else
                          xy / np.sqrt(np.float32(yy * xx)))

    def fit(mat):
        y = mat[iu]
        if compare == "spearman":
            y = rankdata(y, method="average").astype(np.float32)
            x = rankdata(mt, method="average").astype(np.float32)
        else:
            x = mt
        return pearson32(y, x)

    def fish(r):
        return np.float32(-4.0 if r < -0.999329 else 4.0 if r > 0.999329 else np.arctanh(r))

    def ft(v):
        v = np.asarray(v, np.float32); bar = np.float32(0)
        for z in v:
            bar = np.float32(bar + z)
        bar = np.float32(bar / np.float32(len(v))); ss = np.float32(0)
        for z in v:
            d = np.float32(z - bar); ss = np.float32(ss + np.float32(d * d))
        sd = np.float32(np.sqrt(np.float32(ss / np.float32(len(v) - 1))))
        if sd <= 0:
            return np.float32(1e30 if bar > 0 else -1e30 if bar < 0 else 0)
        return np.float32(bar / np.float32(sd / np.sqrt(np.float32(len(v)))))

    rf = np.asarray([[fit(rdm[s, t]) for t in range(nt)] for s in range(ns)], np.float32)
    zf = np.asarray([[fish(rf[s, t]) for t in range(nt)] for s in range(ns)], np.float32)
    zmean = np.asarray([np.float32(np.sum(zf[:, t], dtype=np.float64) / ns)
                        for t in range(nt)])
    effect = np.tanh(zmean).astype(np.float32)
    if null_type == "subjects":
        stat = np.asarray([ft(zf[:, t]) for t in range(nt)], np.float32)
        null = np.asarray([[abs(ft(zf[:, t] * np.asarray(sg, np.float32)))
                            for sg in itertools.product((-1, 1), repeat=ns)]
                           for t in range(nt)], np.float32)
    else:
        stat = zmean
        vals = [[] for _ in range(nt)]
        for perm in itertools.permutations(range(nc)):
            ix = np.asarray(perm)
            for t in range(nt):
                zz = np.asarray([fish(fit(rdm[s, t][np.ix_(ix, ix)])) for s in range(ns)],
                                np.float32)
                vals[t].append(abs(np.float32(np.sum(zz, dtype=np.float64) / ns)))
        null = np.asarray(vals, np.float32)
    if null_type == "conditions":
        for t in range(nt):
            ao = abs(stat[t]); tol = np.float32(64 * np.finfo(np.float32).eps * (1 + ao))
            null[t, np.abs(null[t] - ao) <= tol] = ao
    p = np.mean(null >= np.abs(stat)[:, None], axis=1)
    mx = np.max(null, axis=0)
    pf = np.asarray([np.mean(mx >= abs(z)) for z in stat])
    return dict(rfit=rf, zfit=zf, effect=effect, stat=stat, p=p, q=bh_ref(p), pfwe=pf)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", required=True)
    ap.add_argument("--rsa-bin")
    ap.add_argument("--threads", type=int, default=4)
    ap.add_argument("--work")
    ap.add_argument("--require-deps", action="store_true")
    args = ap.parse_args()
    args.bin = os.path.abspath(args.bin)
    if args.rsa_bin:
        args.rsa_bin = os.path.abspath(args.rsa_bin)
    try:
        import numpy as np
        import nibabel as nib
    except ImportError as e:
        if args.require_deps:
            raise
        print("SKIP 1dTrdm numeric tests: %s" % e)
        return 0

    owned = args.work is None
    work = args.work or tempfile.mkdtemp(prefix="trdm_numeric_")
    os.makedirs(work, exist_ok=True)
    failures = []

    def check(name, ok, detail=""):
        print(("PASS " if ok else "FAIL ") + name)
        if not ok:
            failures.append(name + ((": " + detail) if detail else ""))

    try:
        fx = make_fixture(work)
        check_labeled_inputs(args.bin, fx, work, check)
        check_labeled_matrices(args.bin, fx, work, check)
        rch, helptext = run([args.bin, "-help"])
        rcn, noargtext = run([args.bin])
        help_contract = ["especially useful for EEG and MEG analyses",
                         "Output file                              | Written when",
                         "EXAMPLE: EEG/MEG RDM MOVIE -> fMRI RSA",
                         "EXAMPLE: PAIRED EEG/MEG -> fMRI RSA",
                         "EXAMPLE: LEAVE-ONE-SUBJECT-OUT EEG/MEG -> fMRI RSA",
                         "EXAMPLE: LABELED EEG TRIALS -> LABELED CONDITION RDMs",
                         "EXAMPLE: MEG WITH GENERATED TIME LABELS AND INDEPENDENT RUNS",
                         "EXAMPLE: WINDOWS, SENSOR GROUPS, AND EXISTING AXIS FILES",
                         "-matrix_format numeric|labeled",
                         "-model_series_subjects paired",
                         "-model_series_subjects loo",
                         "3dRSA -mode RSA", "-dataTable FILE", "-prefix OUT",
                         "++ Compile date ="]
        check("expanded scientific help, canonical input/output options, bridge example, and compile date",
              rch == rcn == 0 and all(x in helptext for x in help_contract) and
              "++ Compile date =" in noargtext)
        cases = [("corr", 1, 1, "mean", "none"),
                 ("cosine", 2, 2, "mean", "subject"),
                 ("euclid", 2, 1, "concat", "none"),
                 ("crossnobis", 2, 1, "concat", "partition")]
        for metric, width, step, reduce, center in cases:
            pre = os.path.join(work, "case_" + metric)
            rc, out = run(trdm_cmd(args.bin, fx, pre, metric, width, step,
                                   reduce, center), cwd=os.path.dirname(work))
            nwin = 1 + (fx["nt"] - width) // step
            got = matrices(work, pre, fx, nwin) if rc == 0 else np.asarray([])
            ref = reference(fx, metric, width, step, reduce, center)
            check("%s numeric reference" % metric,
                  rc == 0 and np.allclose(got, ref, atol=3e-6, rtol=3e-6),
                  out[-300:])

        # Row-order invariance includes numeric matrices and canonical long rows.
        base = os.path.join(work, "row_base")
        shuf = os.path.join(work, "row_shuffle")
        rc1, o1 = run(trdm_cmd(args.bin, fx, base, "corr", jobs=1))
        rc2, o2 = run(trdm_cmd(args.bin, fx, shuf, "corr", obs=fx["shuffled"], jobs=1))
        mb = matrices(work, base, fx, fx["nt"]) if rc1 == 0 else np.asarray([])
        ms = matrices(work, shuf, fx, fx["nt"]) if rc2 == 0 else np.asarray([])
        rb = read_long(base + ".trdm.1D") if rc1 == 0 else []
        rs = read_long(shuf + ".trdm.1D") if rc2 == 0 else []
        check("shuffled observation rows are invariant",
              rc1 == rc2 == 0 and np.array_equal(mb, ms) and rb == rs, (o1 + o2)[-300:])

        # Static scheduling must be byte-stable across thread counts.
        t1 = os.path.join(work, "thread_1")
        tn = os.path.join(work, "thread_n")
        rc1, o1 = run(trdm_cmd(args.bin, fx, t1, "crossnobis", 2, 1, "concat",
                               "partition", jobs=1))
        rcn, on = run(trdm_cmd(args.bin, fx, tn, "crossnobis", 2, 1, "concat",
                               "partition", jobs=max(2, args.threads)))
        files1 = sorted(x for x in os.listdir(work) if x.startswith("thread_1_s"))
        same = rc1 == rcn == 0 and all(open(os.path.join(work, a), "rb").read() ==
                                      open(os.path.join(work, a.replace("thread_1", "thread_n")), "rb").read()
                                      for a in files1)
        check("thread identity (1 vs %d)" % max(2, args.threads), same, (o1 + on)[-300:])

        expected = [base + x for x in (".trdm.1D", ".trdm.meta", ".trdm.time.1D",
                                        ".trdm.conditions.1D", ".trdm.features.1D",
                                        ".trdm.counts.1D")]
        counts = read_long(base + ".trdm.counts.1D")
        xcounts = read_long(os.path.join(work, "case_crossnobis.trdm.counts.1D"))
        wins = read_long(os.path.join(work, "case_euclid.trdm.time.1D"))
        check("labeled axes and provenance sidecars", all(os.path.isfile(x) for x in expected) and
              "version 5" in open(base + ".trdm.meta").read() and
              len(read_long(base + ".trdm.1D")) == fx["ns"] * fx["nt"] * 15 and
              len(counts) == fx["ns"] * fx["nc"] and
              all(x["Partition"] == "all" and x["Observations"] == "6" for x in counts) and
              len(xcounts) == fx["ns"] * fx["npart"] * fx["nc"] and
              all(x["Observations"] == "2" for x in xcounts) and
              [(x["StartIndex"], x["EndIndex"], x["TimeLabel"]) for x in wins] ==
              [(str(t), str(t + 1), "w%04d" % t) for t in range(fx["nt"] - 1)])

        # Release gate 3: the two cross-temporal products have distinct
        # estimands, a unique time triangle, symmetry, and diagonal identities.
        cross = os.path.join(work, "cross_temporal")
        xcmd = trdm_cmd(args.bin, fx, cross, "crossnobis", center="partition", jobs=1) + \
               ["-rdm_dynamics", "pearson", "-cross_time_crossnobis"]
        rcx, outx = run(xcmd)
        primary = long_matrices(cross + ".trdm.1D", fx) if rcx == 0 else np.asarray([])
        gdyn = read_dynamics(cross + ".trdm.dynamics.1D", fx, fx["nt"]) if rcx == 0 else np.asarray([])
        gxct = read_cross_time(cross + ".trdm.cross_time_crossnobis.1D", fx, fx["nt"]) if rcx == 0 else np.asarray([])
        rdyn = dynamics_reference(primary, "pearson") if rcx == 0 else np.asarray([])
        rxct = cross_time_reference(fx, center="partition")
        ncell = fx["nt"] * (fx["nt"] + 1) // 2
        drows = read_long(cross + ".trdm.dynamics.1D") if rcx == 0 else []
        xrows = read_long(cross + ".trdm.cross_time_crossnobis.1D") if rcx == 0 else []
        check("RDM-dynamics independent recurrence reference and unique family",
              rcx == 0 and np.allclose(gdyn, rdyn, atol=3e-6, rtol=3e-6) and
              len(drows) == fx["ns"] * ncell and
              np.allclose(gdyn, gdyn.transpose(0, 2, 1), atol=0, rtol=0), outx[-500:])
        check("cross-time crossnobis ordered-partition reference and symmetry",
              rcx == 0 and np.allclose(gxct, rxct, atol=3e-6, rtol=3e-6) and
              len(xrows) == fx["ns"] * ncell * 15 and
              np.allclose(gxct, gxct.transpose(0, 2, 1, 3, 4), atol=0, rtol=0), outx[-500:])
        diag = np.asarray([gxct[:, t, t] for t in range(fx["nt"])]).transpose(1, 0, 2, 3)
        check("cross-time crossnobis diagonal exactly reduces to primary RDM",
              rcx == 0 and np.array_equal(diag.astype(np.float32), primary.astype(np.float32)))
        check("synthetic transient-versus-sustained recurrence is recovered",
              rcx == 0 and np.all(gdyn[:, 0, 1] > gdyn[:, 0, 2]) and
              np.all(gdyn[:, 0, 3] > gdyn[:, 0, 2]))

        # Row order, worker count, and matrix layout cannot alter either product.
        xlayout = os.path.join(work, "cross_layout")
        xlcmd = trdm_cmd(args.bin, fx, xlayout, "crossnobis", center="partition",
                         obs=fx["shuffled"], jobs=max(2, args.threads)) + \
                ["-rdm_dynamics", "pearson", "-cross_time_crossnobis",
                 "-subject_matrices", "no"]
        rclx, outlx = run(xlcmd)
        no_xmats = not any(x.startswith("cross_layout_s") for x in os.listdir(work))
        check("cross-temporal row/thread/output-layout invariance",
              rcx == rclx == 0 and no_xmats and
              read_long(cross + ".trdm.dynamics.1D") == read_long(xlayout + ".trdm.dynamics.1D") and
              read_long(cross + ".trdm.cross_time_crossnobis.1D") ==
              read_long(xlayout + ".trdm.cross_time_crossnobis.1D"), (outx + outlx)[-500:])

        # Spearman dynamics remains available independently of crossnobis.
        sdp = os.path.join(work, "spearman_dynamics")
        rcsd, outsd = run(trdm_cmd(args.bin, fx, sdp, "corr") +
                          ["-rdm_dynamics", "spearman", "-subject_matrices", "no"])
        sprimary = long_matrices(sdp + ".trdm.1D", fx) if rcsd == 0 else np.asarray([])
        sgot = read_dynamics(sdp + ".trdm.dynamics.1D", fx, fx["nt"]) if rcsd == 0 else np.asarray([])
        check("Spearman RDM-dynamics independent reference",
              rcsd == 0 and np.allclose(sgot, dynamics_reference(sprimary, "spearman"),
                                        atol=3e-6, rtol=3e-6), outsd[-400:])

        # Release gate 4: explicit overlapping feature neighborhoods.
        nord = os.path.join(work, "neighborhood_corr")
        nrc, nout = run(trdm_cmd(args.bin, fx, nord, "corr", width=2, reduce="concat") +
                        ["-feature_neighborhoods", fx["neighborhoods"],
                         "-subject_matrices", "no"])
        nnwin = fx["nt"] - 1
        ngot_ord = read_neighborhood_rdms(nord + ".trdm.neighborhoods.1D", fx, nnwin) if nrc == 0 else np.asarray([])
        nref_ord = np.stack([reference(fx, "corr", width=2, reduce="concat", features=z)
                             for z in fx["neighborhood_features"]], axis=1)
        axis_rows = read_long(nord + ".trdm.neighborhood_axis.1D") if nrc == 0 else []
        check("overlapping feature-neighborhood ordinary RDM reference and axis",
              nrc == 0 and np.allclose(ngot_ord, nref_ord, atol=3e-6, rtol=3e-6) and
              [(x["Neighborhood"], x["Feature"]) for x in axis_rows] ==
              [("left", "f00"), ("left", "f01"), ("left", "f02"),
               ("right", "f01"), ("right", "f02"), ("right", "f03")], nout[-500:])

        npre = os.path.join(work, "neighborhood_full")
        ncmd = trdm_cmd(args.bin, fx, npre, "crossnobis", center="partition", jobs=1) + \
               ["-feature_neighborhoods", fx["neighborhoods"],
                "-rdm_dynamics", "pearson", "-cross_time_crossnobis",
                "-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                "-compare", "pearson", "-temporal_null", "subjects", "-nperm", "16",
                "-seed", "211", "-subject_matrices", "no"]
        nr, no = run(ncmd)
        ngrdm = read_neighborhood_rdms(npre + ".trdm.neighborhoods.1D", fx, fx["nt"]) if nr == 0 else np.asarray([])
        ngdyn = read_neighborhood_dynamics(npre + ".trdm.neighborhood_dynamics.1D", fx, fx["nt"]) if nr == 0 else np.asarray([])
        ngxct = read_neighborhood_cross_time(npre + ".trdm.neighborhood_cross_time_crossnobis.1D", fx, fx["nt"]) if nr == 0 else np.asarray([])
        nrref = np.stack([reference(fx, "crossnobis", center="partition", features=z)
                          for z in fx["neighborhood_features"]], axis=1)
        ndref = np.stack([dynamics_reference(nrref[:, g], "pearson")
                          for g in range(len(fx["neighborhood_labels"]))], axis=1)
        nxref = np.stack([cross_time_reference(fx, center="partition", features=z)
                          for z in fx["neighborhood_features"]], axis=1)
        check("feature-neighborhood crossnobis, dynamics, and cross-time references",
              nr == 0 and np.allclose(ngrdm, nrref, atol=3e-6, rtol=3e-6) and
              np.allclose(ngdyn, ndref, atol=3e-6, rtol=3e-6) and
              np.allclose(ngxct, nxref, atol=3e-6, rtol=3e-6), no[-600:])
        check("neighborhood cross-time diagonals and unique triangles",
              nr == 0 and all(np.array_equal(ngxct[:, :, t, t].astype(np.float32),
                                             ngrdm[:, :, t].astype(np.float32))
                              for t in range(fx["nt"])) and
              len(read_long(npre + ".trdm.neighborhood_dynamics.1D")) ==
              fx["ns"] * 2 * ncell and
              len(read_long(npre + ".trdm.neighborhood_cross_time_crossnobis.1D")) ==
              fx["ns"] * 2 * ncell * 15)

        nirows = read_long(npre + ".trdm.neighborhood_inference.1D") if nr == 0 else []
        niref = infer_reference(ngrdm.reshape(fx["ns"], 2 * fx["nt"], fx["nc"], fx["nc"]),
                                fx["model"], "subjects", "pearson") if nr == 0 else None
        nival = lambda key: np.asarray([float(x[key]) for x in nirows])
        nmeta = open(npre + ".trdm.meta").read() if nr == 0 else ""
        check("joint time-by-neighborhood synchronized inference family",
              nr == 0 and len(nirows) == 2 * fx["nt"] and
              np.allclose(nival("Effect"), niref["effect"], atol=3e-6) and
              np.allclose(nival("Stat"), niref["stat"], atol=3e-5) and
              np.array_equal(nival("P"), niref["p"]) and
              np.array_equal(nival("Q"), niref["q"]) and
              np.array_equal(nival("PFWE"), niref["pfwe"]) and
              "family time-x-neighborhood" in nmeta and "family_size 10" in nmeta and
              not os.path.exists(npre + ".trdm.inference.1D"), no[-700:])

        # Graph/table order, observation order, threads, and layout are irrelevant.
        nshuf = os.path.join(work, "neighborhood_shuffled")
        nscmd = trdm_cmd(args.bin, fx, nshuf, "crossnobis", center="partition",
                         obs=fx["shuffled"], jobs=max(2, args.threads)) + \
                ["-feature_neighborhoods", fx["neighborhoods_shuffled"],
                 "-rdm_dynamics", "pearson", "-cross_time_crossnobis",
                 "-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                 "-compare", "pearson", "-temporal_null", "subjects", "-nperm", "16",
                 "-seed", "211", "-subject_matrices", "no"]
        nsr, nso = run(nscmd)
        nfiles = ["neighborhoods.1D", "neighborhood_axis.1D", "neighborhood_dynamics.1D",
                  "neighborhood_cross_time_crossnobis.1D", "neighborhood_fits.1D",
                  "neighborhood_inference.1D"]
        check("feature-neighborhood graph/row/thread/output-layout invariance",
              nr == nsr == 0 and not any(x.startswith("neighborhood_shuffled_s") for x in os.listdir(work)) and
              all(read_long(npre + ".trdm." + z) == read_long(nshuf + ".trdm." + z)
                  for z in nfiles), (no + nso)[-700:])

        # Guarded group series equals the arithmetic subject mean at every window.
        bridge = os.path.join(work, "bridge")
        rcb, ob = run(trdm_cmd(args.bin, fx, bridge, "corr", series=True))
        bm = matrices(work, bridge, fx, fx["nt"]) if rcb == 0 else np.asarray([])
        gm = np.asarray([np.loadtxt("%s_group_t%04d.1D" % (bridge, t))
                         for t in range(fx["nt"])]) if rcb == 0 else np.asarray([])
        check("independent-sample model-series group mean",
              rcb == 0 and np.allclose(gm, bm.mean(axis=0), atol=2e-7), ob[-300:])

        paired = os.path.join(work, "paired_bridge")
        loopre = os.path.join(work, "loo_bridge")
        rcpair, opair = run(trdm_cmd(args.bin, fx, paired, "corr", series="paired"))
        rcloo, oloo = run(trdm_cmd(args.bin, fx, loopre, "corr", series="loo"))
        pm = matrices(work, paired, fx, fx["nt"]) if rcpair == 0 else np.asarray([])
        lm = np.asarray([[np.loadtxt("%s_loo_s%04d_s%02d_t%04d.1D" %
                                    (loopre, s, s, t)) for t in range(fx["nt"])]
                         for s in range(fx["ns"])]) if rcloo == 0 else np.asarray([])
        prows = read_long(paired + ".paired_model_series.1D") if rcpair == 0 else []
        lrows = read_long(loopre + ".loo_model_series.1D") if rcloo == 0 else []
        lref = np.asarray([(pm.sum(axis=0) - pm[s]) / (fx["ns"] - 1)
                           for s in range(fx["ns"])]) if rcpair == 0 else np.asarray([])
        check("paired subject-indexed model-series manifest",
              rcpair == 0 and len(prows) == fx["ns"] * fx["nt"] and
              [x["Subj"] for x in prows[::fx["nt"]]] ==
              ["s%02d" % s for s in range(fx["ns"])], opair[-300:])
        check("leave-one-subject-out temporal templates",
              rcloo == 0 and len(lrows) == fx["ns"] * fx["nt"] and
              np.allclose(lm, lref, atol=1e-5, rtol=1e-5), oloo[-300:])

        # Release gate 2: exhaustive subject and condition temporal inference.
        # Use the already independently validated exported float RDMs here so
        # the exhaustive null reference tests inference in isolation; exact
        # categorical-model ties can otherwise turn sub-ULP estimator rounding
        # into different >= counts despite equivalent RDM values.
        rdmref = long_matrices(base + ".trdm.1D", fx)
        isub = os.path.join(work, "infer_subject")
        scmd = trdm_cmd(args.bin, fx, isub, "corr", jobs=1) + \
               ["-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                "-compare", "spearman", "-temporal_null", "subjects", "-nperm", "16",
                "-seed", "191"]
        rcs, outs = run(scmd)
        srows = read_long(isub + ".trdm.inference.1D") if rcs == 0 else []
        sfits = read_long(isub + ".trdm.fits.1D") if rcs == 0 else []
        sref = infer_reference(rdmref, fx["model"], "subjects", "spearman")
        sval = lambda key: np.asarray([float(x[key]) for x in srows])
        check("exhaustive subject-sign temporal inference reference",
              rcs == 0 and len(srows) == fx["nt"] and
              np.allclose(sval("Effect"), sref["effect"], atol=3e-6) and
              np.allclose(sval("Stat"), sref["stat"], atol=3e-5) and
              np.array_equal(sval("P"), sref["p"]) and
              np.array_equal(sval("Q"), sref["q"]) and
              np.array_equal(sval("PFWE"), sref["pfwe"]), outs[-500:])
        gotr = np.asarray([float(x["Fit"]) for x in sfits]).reshape(fx["ns"], fx["nt"])
        gotz = np.asarray([float(x["FisherZ"]) for x in sfits]).reshape(fx["ns"], fx["nt"])
        check("labeled subject fits and shuffled model-axis alignment",
              rcs == 0 and np.allclose(gotr, sref["rfit"], atol=3e-6) and
              np.allclose(gotz, sref["zfit"], atol=3e-6) and
              [x["TimeLabel"] for x in srows] == ["t%03d" % t for t in range(fx["nt"])])

        icond = os.path.join(work, "infer_condition")
        ccmd = trdm_cmd(args.bin, fx, icond, "corr", jobs=max(2, args.threads)) + \
               ["-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                "-compare", "pearson", "-temporal_null", "conditions", "-nperm", "720",
                "-seed", "193"]
        rcc, outc = run(ccmd)
        crows = read_long(icond + ".trdm.inference.1D") if rcc == 0 else []
        cref = infer_reference(rdmref, fx["model"], "conditions", "pearson")
        cval = lambda key: np.asarray([float(x[key]) for x in crows])
        check("exhaustive synchronized-condition temporal inference reference",
              rcc == 0 and len(crows) == fx["nt"] and
              np.allclose(cval("Effect"), cref["effect"], atol=3e-6) and
              np.allclose(cval("Stat"), cref["stat"], atol=3e-6) and
              np.allclose(cval("P"), cref["p"], atol=7e-8, rtol=0) and
              np.allclose(cval("Q"), cref["q"], atol=7e-8, rtol=0) and
              np.allclose(cval("PFWE"), cref["pfwe"], atol=7e-8, rtol=0), outc[-500:])
        cmeta = open(icond + ".trdm.meta").read() if rcc == 0 else ""
        check("temporal family and fixed-sample provenance",
              "nperm_used 720" in cmeta and "relabelings exact" in cmeta and
              "family_size 5" in cmeta and
              "tested_population fixed-observed-subject-condition-sample" in cmeta)

        # Output selection and worker count cannot change scientific results.
        isample = os.path.join(work, "infer_sample1")
        sample1 = trdm_cmd(args.bin, fx, isample, "corr", jobs=1) + \
               ["-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                "-compare", "spearman", "-temporal_null", "subjects", "-nperm", "11",
                "-seed", "197"]
        rcsamp, outsamp = run(sample1)
        ilayout = os.path.join(work, "infer_layout")
        lcmd = trdm_cmd(args.bin, fx, ilayout, "corr", jobs=max(2, args.threads)) + \
               ["-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                "-compare", "spearman", "-temporal_null", "subjects", "-nperm", "11",
                "-seed", "197", "-subject_matrices", "no"]
        rcl, outl = run(lcmd)
        no_mats = not any(x.startswith("infer_layout_s") for x in os.listdir(work))
        check("sampled inference is seed/thread/output-layout invariant",
              rcsamp == rcl == 0 and no_mats and
              open(isample + ".trdm.inference.1D", "rb").read() ==
              open(ilayout + ".trdm.inference.1D", "rb").read() and
              open(isample + ".trdm.fits.1D", "rb").read() ==
              open(ilayout + ".trdm.fits.1D", "rb").read(), (outsamp + outl)[-400:])

        # Consume the generated relative-path list in a real classic-RSA run.
        if args.rsa_bin:
            shape = (2, 2, 1); mask = os.path.join(work, "mask.nii.gz")
            nib.save(nib.Nifti1Image(np.ones(shape, dtype=np.int16), np.eye(4)), mask)
            tab = os.path.join(work, "rsa_table.txt")
            rng = np.random.default_rng(7331)
            with open(tab, "w") as f:
                f.write("Subj InputFile\n")
                for s in range(5):
                    pat = rng.normal(size=(fx["nc"], np.prod(shape))).astype(np.float32)
                    fn = os.path.join(work, "rsa_s%02d.nii.gz" % s)
                    nib.save(nib.Nifti1Image(pat.T.reshape(shape + (fx["nc"],)), np.eye(4)), fn)
                    f.write("r%02d %s\n" % (s, fn))
            rpre = os.path.join(work, "rsa_bridge")
            rc, out = run([args.rsa_bin, "-dataTableFile", tab, "-mask", mask,
                           "-mode", "RSA", "-model_series", bridge + ".model_series.1D",
                           "-metric", "spearman", "-nperm", "0", "-no_dset",
                           "-prefix", rpre, "-quiet"], cwd="/")
            text = open(rpre + ".rsa.1D").read() if rc == 0 else ""
            check("exact 1dTrdm -> 3dRSA model-series round trip",
                  rc == 0 and all("t%03d" % t in text for t in range(fx["nt"])), out[-500:])

            # Dependent fusion: the same subject labels select either each
            # participant's own EEG RDM or a template averaged over all other
            # EEG participants. Independently reproduce the time-wise effects.
            stab = os.path.join(work, "rsa_subject_table.txt")
            fpats = []
            with open(stab, "w") as f:
                f.write("Subj InputFile\n")
                for s in range(fx["ns"]):
                    pat = rng.normal(size=(fx["nc"], np.prod(shape))).astype(np.float32)
                    fpats.append(pat)
                    fn = os.path.join(work, "rsa_match_s%02d.nii.gz" % s)
                    nib.save(nib.Nifti1Image(pat.T.reshape(shape + (fx["nc"],)), np.eye(4)), fn)
                    f.write("s%02d %s\n" % (s, fn))
            iu = np.triu_indices(fx["nc"], 1)
            neural = np.asarray([1.0 - np.corrcoef(x) for x in fpats])

            def spearman(a, b):
                from scipy.stats import rankdata
                return np.corrcoef(rankdata(a), rankdata(b))[0, 1]

            def fusion_reference(models):
                vals = []
                for t in range(fx["nt"]):
                    z = [np.arctanh(np.clip(spearman(neural[s][iu], models[s, t][iu]),
                                                   -0.999329, 0.999329))
                         for s in range(fx["ns"])]
                    vals.append(np.tanh(np.mean(z)))
                return np.asarray(vals)

            def run_subject_series(label, mode, manifest, jobs=1, nperm=0,
                                   extra=None):
                pre = os.path.join(work, label)
                env = os.environ.copy(); env["OMP_NUM_THREADS"] = str(jobs)
                cmd = [args.rsa_bin, "-dataTableFile", stab, "-mask", mask,
                       "-mode", "RSA", "-model_series_subjects", mode, manifest,
                       "-metric", "spearman", "-nperm", str(nperm), "-no_dset",
                       "-prefix", pre, "-quiet"]
                if extra:
                    cmd.extend(extra)
                rc0, out0 = run(cmd, env=env)
                rows = read_rsa_series(pre + ".rsa.1D") if rc0 == 0 else []
                return rc0, out0, rows, pre

            prc, pout, prs, ppre = run_subject_series(
                "paired_fusion", "paired", paired + ".paired_model_series.1D")
            lrc, lout, lrs, lpre = run_subject_series(
                "loo_fusion", "loo", loopre + ".loo_model_series.1D",
                max(2, args.threads), 720)
            peff = np.asarray([float(x["effect"]) for x in prs])
            leff = np.asarray([float(x["effect"]) for x in lrs])
            check("paired subject-specific EEG-fMRI fusion reference",
                  prc == 0 and len(prs) == fx["nt"] and
                  np.allclose(peff, fusion_reference(pm), atol=4e-6) and
                  "mode=paired" in open(ppre + ".rsa.1D").read(), pout[-500:])
            check("LOO EEG-template to held-fMRI-subject fusion reference",
                  lrc == 0 and len(lrs) == fx["nt"] and
                  np.allclose(leff, fusion_reference(lm), atol=4e-6) and
                  "mode=loo" in open(lpre + ".rsa.1D").read() and
                  "condition labels (fixed observed subjects)" in
                  open(lpre + ".rsa.1D").read(), lout[-500:])

            lsrc, lsout, _, _ = run_subject_series(
                "bad_loo_subject_null", "loo",
                loopre + ".loo_model_series.1D", nperm=16,
                extra=["-classic_null", "subjects"])
            check("LOO rejects an invalid independent-subject null",
                  lsrc != 0 and "overlap across participants" in lsout,
                  lsout[-500:])

            # Exercise the synchronized condition-label null on the full
            # subject x time grid. Its max statistic must cover all times, and
            # its point effect must be the same paired estimand as above.
            crc, cout, crs, cpre = run_subject_series(
                "paired_condition_null", "paired",
                paired + ".paired_model_series.1D", max(2, args.threads), 720,
                ["-classic_null", "conditions"])
            cp = np.asarray([float(x["p"]) for x in crs])
            cpfwe = np.asarray([float(x["pfwe"]) for x in crs])
            ceff = np.asarray([float(x["effect"]) for x in crs])
            ctext = open(cpre + ".rsa.1D").read() if crc == 0 else ""
            check("paired fusion synchronized-condition null and time-family FWE",
                  crc == 0 and len(crs) == fx["nt"] and
                  np.allclose(ceff, fusion_reference(pm), atol=4e-6) and
                  np.all((cp >= 0) & (cp <= 1)) and
                  np.all((cpfwe >= cp) & (cpfwe <= 1)) and
                  "condition labels (fixed observed subjects)" in ctext,
                  cout[-500:])

            # The manifest is a strict, exact-label contract: neither an
            # incomplete temporal grid nor an unmatched participant can be
            # silently dropped from a paired analysis.
            bad_manifest = os.path.join(work, "bad_subject_series.1D")
            with open(paired + ".paired_model_series.1D") as src, \
                    open(bad_manifest, "w") as dst:
                lines = src.readlines()
                dst.writelines(lines[:-1])
            brc, bout, _, _ = run_subject_series(
                "bad_subject_fusion", "paired", bad_manifest)
            bad_label_manifest = os.path.join(work, "bad_subject_label_series.1D")
            with open(paired + ".paired_model_series.1D") as src, \
                    open(bad_label_manifest, "w") as dst:
                for line in src:
                    dst.write(("ghost" + line[3:]) if line.startswith("s00 ") else line)
            lbrc, lbout, _, _ = run_subject_series(
                "bad_subject_label_fusion", "paired", bad_label_manifest)
            check("rejects incomplete or unmatched subject-indexed temporal grids",
                  brc != 0 and "incomplete" in bout and lbrc != 0 and
                  ("missing imaging subject" in lbout or
                   "unexpected subject" in lbout), (bout + lbout)[-500:])

        # Focused negative-contract cases.
        rc, out = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_assert"), "corr") +
                      ["-model_series_out", "same_subjects"])
        check("rejects unasserted model-series fusion", rc != 0 and "independent" in out)

        rc, out = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_cross_time"), "corr") +
                      ["-cross_time_crossnobis"])
        check("rejects cross-time crossnobis without crossnobis metric",
              rc != 0 and "requires -metric crossnobis" in out)

        badgraph = os.path.join(work, "bad_neighborhood_unknown.txt")
        with open(badgraph, "w") as f:
            f.write("Neighborhood Feature\nleft f00\nleft missing_sensor\n")
        rcg, outg = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_graph"), "corr") +
                        ["-feature_neighborhoods", badgraph])
        dupgraph = os.path.join(work, "bad_neighborhood_duplicate.txt")
        with open(dupgraph, "w") as f:
            f.write("Neighborhood Feature\nleft f00\nleft f00\n")
        rcdg, outdg = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_graph_dup"), "corr") +
                          ["-feature_neighborhoods", dupgraph])
        check("rejects unknown and duplicate neighborhood memberships",
              rcg != 0 and "unknown Feature" in outg and
              rcdg != 0 and "repeats Feature" in outdg, (outg + outdg)[-500:])

        onegraph = os.path.join(work, "bad_neighborhood_small.txt")
        with open(onegraph, "w") as f:
            f.write("Neighborhood Feature\nsolo f00\n")
        rcsmall, outsmall = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_graph_small"), "corr") +
                                ["-feature_neighborhoods", onegraph])
        rcbridge, outbridge = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_graph_bridge"), "corr",
                                          series=True) +
                                  ["-feature_neighborhoods", fx["neighborhoods"]])
        check("rejects underspecified ordinary neighborhoods and ambiguous bridge",
              rcsmall != 0 and "too few output features" in outsmall and
              rcbridge != 0 and "all-feature bridge" in outbridge,
              (outsmall + outbridge)[-500:])

        badinfer = trdm_cmd(args.bin, fx, os.path.join(work, "bad_infer"), "corr") + \
                   ["-model_mat", fx["modelfile"]]
        rci, outi = run(badinfer)
        badaxis = os.path.join(work, "bad_model_axis.1D")
        with open(badaxis, "w") as f:
            f.write("ConditionIndex Condition\n0 c00\n1 c01\n2 c02\n3 c03\n4 c04\n5 c99\n")
        badlabels = trdm_cmd(args.bin, fx, os.path.join(work, "bad_labels"), "corr") + \
                    ["-model_mat", fx["modelfile"], "-model_conditions", badaxis]
        rclab, outlab = run(badlabels)
        oneobs = os.path.join(work, "one_subject.txt")
        with open(oneobs, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for row in fx["records"]:
                if row[0] == "s00":
                    f.write("%s %s %s %s %s\n" % row)
        onecmd = trdm_cmd(args.bin, fx, os.path.join(work, "bad_one_subject"), "corr",
                          obs=oneobs) + \
                 ["-model_mat", fx["modelfile"], "-model_conditions", fx["modelaxis"],
                  "-temporal_null", "subjects", "-nperm", "16"]
        rcone, outone = run(onecmd)
        check("rejects incomplete or mismatched inference axes",
              rci != 0 and "required together" in outi and rclab != 0 and
              "missing observation Condition" in outlab and rcone != 0 and
              "needs at least 2 subjects" in outone, (outi + outlab + outone)[-600:])

        dup = os.path.join(work, "bad_duplicate.txt")
        with open(dup, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for row in fx["records"] + [fx["records"][0]]:
                f.write("%s %s %s %s %s\n" % row)
        rc, out = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_dup"), "corr", obs=dup))
        check("rejects duplicate subject observation IDs", rc != 0 and "repeats Observation" in out)

        missing = os.path.join(work, "bad_missing.txt")
        with open(missing, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for row in fx["records"]:
                if not (row[0] == "s00" and row[2] == "c00"):
                    f.write("%s %s %s %s %s\n" % row)
        rc1, out1 = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_missing_ord"),
                                 "corr", obs=missing))
        xmissing = os.path.join(work, "bad_cross_cell.txt")
        with open(xmissing, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for row in fx["records"]:
                if not (row[0] == "s00" and row[2] == "c00" and row[3] == "p00"):
                    f.write("%s %s %s %s %s\n" % row)
        rc2, out2 = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_missing_x"),
                                 "crossnobis", obs=xmissing))
        check("rejects missing ordinary and crossnobis cells", rc1 != 0 and rc2 != 0 and
              "no observation" in out1 and "balanced crossnobis" in out2, (out1 + out2)[-400:])

        badtime = os.path.join(work, "bad_time.txt")
        with open(badtime, "w") as f:
            f.write("time_index time_value time_unit time_label Extra\n0 0 s zero x\n")
        badcmd = trdm_cmd(args.bin, fx, os.path.join(work, "bad_time"), "corr")
        badcmd[badcmd.index(fx["time"])] = badtime
        rc, out = run(badcmd)
        check("rejects non-strict time axis", rc != 0 and "exactly" in out)

        badshape = os.path.join(work, "bad_shape.1D")
        np.savetxt(badshape, np.ones((fx["nt"] - 1, fx["nf"])), fmt="%.1f")
        shapeobs = os.path.join(work, "bad_shape_table.txt")
        with open(shapeobs, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for i, row in enumerate(fx["records"]):
                f.write("%s %s %s %s %s\n" % (row[:-1] + ((os.path.basename(badshape) if i == 0 else row[-1]),)))
        rc, out = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_shape_out"),
                               "corr", obs=shapeobs))
        badfinite = os.path.join(work, "bad_finite.1D")
        z = fx["x"][0, 0, 0, 0].copy(); z[2, 1] = np.nan
        np.savetxt(badfinite, z, fmt="%.9g")
        finiteobs = os.path.join(work, "bad_finite_table.txt")
        with open(finiteobs, "w") as f:
            f.write("Subj Observation Condition Partition InputFile\n")
            for i, row in enumerate(fx["records"]):
                f.write("%s %s %s %s %s\n" % (row[:-1] + ((os.path.basename(badfinite) if i == 0 else row[-1]),)))
        rcf, outf = run(trdm_cmd(args.bin, fx, os.path.join(work, "bad_finite_out"),
                                 "corr", obs=finiteobs))
        check("rejects observation shape/nonfinite mismatch",
              rc != 0 and "need 5 x 4" in out and rcf != 0 and
              ("non-finite" in outf or "cannot read observation" in outf),
              (out + outf)[-400:])

        rc, out = run(trdm_cmd(args.bin, fx, base, "corr"))
        check("refuses overwrite without AFNI overwrite opt-in", rc != 0 and "exists" in out)

    finally:
        if failures:
            print("\n%d failure(s):\n%s" % (len(failures), "\n".join(failures)))
            print("kept work directory: %s" % work)
        elif owned:
            shutil.rmtree(work)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
