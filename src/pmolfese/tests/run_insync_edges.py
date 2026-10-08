#!/usr/bin/env python3
"""Black-box oracle for 3dInSync -edges / -edges_rss / -edges_events.

Synthetic ROI signals are written as AFNI 1D datasets (columns are voxels, rows
are time points, read with the trailing apostrophe).  Every expected value is
computed here with the standard library only: z-score each ROI series (sample
SD), form z_i*z_j, correlate across subjects, and take the pairwise median.
Agreement therefore checks 3dInSync against code that shares none of it.
"""

import argparse
import itertools
import math
import os
import random
import statistics
import subprocess
import sys
import tempfile

NSUB, NROI, NT, VPR = 6, 4, 40, 3
TOL = 5e-6


def pearson(x, y):
    mx, my = sum(x) / len(x), sum(y) / len(y)
    sxy = sum((a - mx) * (b - my) for a, b in zip(x, y))
    sxx = sum((a - mx) ** 2 for a in x)
    syy = sum((b - my) ** 2 for b in y)
    return sxy / math.sqrt(sxx * syy)


def zscore(x):
    m = sum(x) / len(x)
    sd = math.sqrt(sum((a - m) ** 2 for a in x) / (len(x) - 1))
    return [(a - m) / sd for a in x]


def pairwise_median(series, members):
    return statistics.median(pearson(series[a], series[b])
                             for a, b in itertools.combinations(members, 2))


def make_data(rng):
    lat = [[rng.gauss(0, 1) for _ in range(3)] for _ in range(NT)]
    mix = [[rng.gauss(0, 1) for _ in range(NROI)] for _ in range(3)]
    data = []
    for _ in range(NSUB):
        sub = []
        for t in range(NT):
            sub.append([sum(lat[t][k] * mix[k][r] for k in range(3)) +
                        1.2 * rng.gauss(0, 1) for r in range(NROI)])
        data.append(sub)
    return data  # [subject][time][roi]


def write_inputs(work, data, groups):
    for s, sub in enumerate(data):
        with open(os.path.join(work, "s%02d.1D" % s), "w") as f:
            for row in sub:
                f.write(" ".join("%.9g" % v for roi in row for v in [roi] * VPR) + "\n")
    with open(os.path.join(work, "atlas.1D"), "w") as f:
        f.write(" ".join(str(r + 1) for r in range(NROI) for _ in range(VPR)) + "\n")
    with open(os.path.join(work, "tab.txt"), "w") as f:
        f.write("Subj Group InputFile\n")
        for s in range(NSUB):
            f.write("s%02d %s s%02d.1D'\n" % (s, groups[s], s))


def run(binary, work, prefix, *extra, threads=2):
    env = dict(os.environ, AFNI_NOMMAP="YES", OMP_NUM_THREADS=str(threads))
    cmd = [binary, "-quiet", "-prefix", prefix, "-atlas", "atlas.1D'",
           "-dataTableFile", "tab.txt"] + list(extra)
    r = subprocess.run(cmd, cwd=work, env=env, capture_output=True, text=True)
    if r.returncode != 0:
        raise AssertionError("3dInSync failed: %s\n%s" % (" ".join(cmd), r.stderr))


def table(path):
    with open(path) as f:
        rows = [l.split() for l in f if l.strip()]
    return rows[0], rows[1:]


def check(cond, msg):
    if not cond:
        raise AssertionError(msg)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", required=True)
    args = ap.parse_args()
    rng = random.Random(5)
    data = make_data(rng)
    groups = ["A", "A", "A", "B", "B", "B"]

    with tempfile.TemporaryDirectory() as work:
        write_inputs(work, data, groups)
        # z[s][roi] = z-scored series
        z = [[zscore([data[s][t][r] for t in range(NT)]) for r in range(NROI)]
             for s in range(NSUB)]
        edge = {(i, j): [[z[s][i][t] * z[s][j][t] for t in range(NT)] for s in range(NSUB)]
                for i, j in itertools.combinations(range(NROI), 2)}
        node = [[z[s][r] for s in range(NSUB)] for r in range(NROI)]
        rss = [[math.sqrt(sum((z[s][i][t] * z[s][j][t]) ** 2
                              for i, j in itertools.combinations(range(NROI), 2)))
                for t in range(NT)] for s in range(NSUB)]
        gmem = {"A": [0, 1, 2], "B": [3, 4, 5]}

        run(args.bin, work, "e", "-edges", "-edges_matrix", "-edges_rss", "-edges_events", "0.25")
        _, rows = table(os.path.join(work, "e.edge.1D"))
        check(len(rows) == 2 * len(edge), "edge row count")
        for r in rows:
            i, j = int(r[2]) - 1, int(r[3]) - 1
            exp = pairwise_median(edge[(i, j)], gmem[r[1]])
            check(abs(exp - float(r[6])) < TOL, "edge ISC %s %d-%d" % (r[1], i, j))
            for col, roi in ((8, i), (9, j)):
                exp = pairwise_median(node[roi], gmem[r[1]])
                check(abs(exp - float(r[col])) < TOL, "node ISC column %d" % col)
        check(not os.path.exists(os.path.join(work, "e.node.1D")), "stray node file")
        hdr, rows = table(os.path.join(work, "e.frames.1D"))
        check(hdr[:5] == ["Condition", "Group", "TR", "OrigTR", "MeanRSS"], "frames header")
        check(len(rows) == 2 * NT, "frames row count")
        for r in rows:
            check(abs(pairwise_median(rss, gmem[r[1]]) - float(r[5])) < TOL, "RSS ISC")
            t = int(r[2])
            mean = sum(rss[s][t] for s in gmem[r[1]]) / 3
            check(abs(mean - float(r[4])) < TOL, "MeanRSS")

        k = math.ceil(0.25 * NT - 1e-9)
        hi = []
        for s in range(NSUB):
            order = sorted(range(NT), key=lambda t: (-rss[s][t], t))
            hi.append(set(order[:k]))
        for r in rows:
            t = int(r[2])
            n = sum(1 for s in gmem[r[1]] if t in hi[s])
            check(n == int(r[7]) and int(r[8]) == 3, "event count at TR %d" % t)

        # .netcc: node ISC on the diagonal, symmetric edge ISC off it
        with open(os.path.join(work, "e_edge_A.netcc")) as f:
            lines = [l.rstrip("\n") for l in f]
        check(lines[0].startswith("# %d " % NROI), "netcc ROI count")
        check(lines[1].startswith("# 2 "), "netcc matrix count")
        cc = lines.index("# CC")
        mat = [[float(v) for v in lines[cc + 1 + i].split()] for i in range(NROI)]
        for i in range(NROI):
            check(abs(mat[i][i] - pairwise_median(node[i], gmem["A"])) < 1e-3, "netcc diagonal")
            for j in range(NROI):
                check(abs(mat[i][j] - mat[j][i]) < 1e-9, "netcc symmetry")

        # nulls: reproducible across thread counts, and well-formed
        extra = ["-edges", "-edges_rss", "-edges_events", "0.25", "-edges_nnull", "30",
                 "-min_shift", "3", "-seed", "7"]
        run(args.bin, work, "n1", *extra, threads=1)
        run(args.bin, work, "n2", *extra, threads=4)
        for kind in ("edge", "frames"):
            a = open(os.path.join(work, "n1.%s.1D" % kind)).read()
            b = open(os.path.join(work, "n2.%s.1D" % kind)).read()
            check(a == b, "%s null differs between thread counts" % kind)
        hdr, rows = table(os.path.join(work, "n1.edge.1D"))
        check(hdr[-7:] == ["NullMean", "Excess", "P", "Z", "Q", "PFWE", "ZFWE"], "null columns")
        for r in rows:
            p, pf = float(r[12]), float(r[15])
            check(1.0 / 30 - 1e-6 <= p <= 1.0 and p <= pf + 1e-9, "edge p-values")
            check(abs(float(r[6]) - float(r[10]) - float(r[11])) < 1e-6, "Excess = ISC - NullMean")

        hdr, _ = table(os.path.join(work, "n1.frames.1D"))
        check("RSS_P" in hdr and "EvPFWE" in hdr, "frames null columns")
        # without -edges_matrix no matrix files are written
        check(not any(f.endswith(".netcc") for f in os.listdir(work) if f.startswith("n1")),
              "unrequested netcc")

        # option contract
        for bad in (["-edges_nnull", "30"], ["-edges_matrix"], ["-edges_events", "0.9"],
                    ["-edges", "-edges_nnull", "30", "-zcensor"]):
            r = subprocess.run([args.bin, "-quiet", "-prefix", "bad", "-atlas", "atlas.1D'",
                                "-dataTableFile", "tab.txt"] + bad,
                               cwd=work, capture_output=True, text=True)
            check(r.returncode != 0, "accepted invalid options %s" % bad)

    print("PASS: 3dInSync edge oracle")


if __name__ == "__main__":
    sys.exit(main())
