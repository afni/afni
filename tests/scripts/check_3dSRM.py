#!/usr/bin/env python3
"""End-to-end checks of the 3dSRM program (fit mode) on generated data.

Builds a small multi-subject data set (NIfTI volumes with offsets, scales, constant voxels, and an
all-zero volume; per-subject masks; and 1D inputs), runs the real 3dSRM on it, and compares every
output with an independent computation: the library driver (srm_cli, which is the same thd_srm.c
but with none of the table/dataset/preprocessing code) run on arrays preprocessed here in numpy.

It also asserts the logging convention: the program name appears on the banner line only.

  python check_3dSRM.py --prog build/targets_built/3dSRM --workdir ./check_3dsrm

Threads are capped and the programs run at low priority, so it is safe on a working machine.
Requires numpy and nibabel. The run is SKIPPED, not failed, if either is
missing, unless --require-deps is given; CTest gives it, so a missing environment cannot pass.
AFNI_SRM_KERNEL in the environment (VOXEL or GRAM) reaches 3dSRM, so the same checks can be run
on either fitting kernel.
"""

import argparse
import hashlib
import os
import re
import subprocess
import sys
from pathlib import Path

try:
    import nibabel as nib
    import numpy as np
except ImportError as e:
    if "--require-deps" in sys.argv:
        sys.exit(f"missing a required module: {e}")
    print(f"SKIP: {e}")
    sys.exit(0)

REPO = Path(__file__).resolve().parents[2]

ENV = dict(os.environ, OMP_NUM_THREADS="4", OPENBLAS_NUM_THREADS="4", VECLIB_MAXIMUM_THREADS="4",
           MKL_NUM_THREADS="4", NUMEXPR_NUM_THREADS="4")


# ------------------------------------------------------------------ the library driver

def build_cli(workdir, given, openmp=True):
    """Compile srm_cli from the repo. With OpenMP when Homebrew's libomp is
    present: the fit is threaded, and the results do not depend on the threads."""
    if given:
        return Path(given)
    src = REPO / "src" / "pmolfese" / "tests" / "srm_cli.c"
    thd = REPO / "src" / "thd_srm.c"
    omp = Path("/opt/homebrew/opt/libomp")
    use_omp = openmp and (omp / "include" / "omp.h").exists()
    exe = workdir / ("srm_cli_omp" if use_omp else "srm_cli")
    if not exe.exists() or exe.stat().st_mtime < max(src.stat().st_mtime, thd.stat().st_mtime,
                                                     (REPO / "src" / "thd_srm.h").stat().st_mtime):
        cmd = ["cc", "-O2", "-I", str(REPO / "src"), "-o", str(exe), str(src), "-lm"]
        if use_omp:
            cmd[2:2] = ["-DUSE_OMP", "-Xpreprocessor", "-fopenmp", f"-I{omp}/include",
                        f"-L{omp}/lib", "-lomp"]
        subprocess.run(cmd, check=True)
    return exe


def run_ours(cli, workdir, tag, algo, views, k, niter, center, init_w=None,
             nstart=1, tol=0.0, seed=1234567, extra=None, seglen=None, coupling=None):
    """Fit the views (each V_i x T) with the driver; returns S, W, the objective trace, and the
    noise model (prob), as float64 arrays."""
    d = workdir / tag
    d.mkdir(parents=True, exist_ok=True)
    T = views[0].shape[1]
    cmd = [str(cli), "--algo", algo, "--k", str(k), "--niter", str(niter),
           "--center", str(int(center)), "--T", str(T), "--out", str(d / "out"),
           "--nstart", str(nstart), "--tol", repr(tol), "--seed", str(seed)]
    if init_w is not None:
        for i, w in enumerate(init_w):
            w.astype(np.float32).tofile(d / f"init.W{i}.f32")
        cmd += ["--init", str(d / "init")]
    for i, x in enumerate(views):
        f = d / f"x{i}.f32"
        a = np.ascontiguousarray(x, dtype=np.float32)
        stamp = d / f"x{i}.md5"
        digest = hashlib.md5(a.tobytes()).hexdigest()       # a size match alone is not a content match
        if not f.exists() or not stamp.exists() or stamp.read_text() != digest:
            a.tofile(f)
            stamp.write_text(digest)
        cmd += ["--view", str(x.shape[0]), str(f)]
    cmd += list(extra or [])
    if coupling is not None:
        cmd += ["--seglen", ",".join(str(s) for s in seglen), "--coupling", repr(float(coupling))]
    subprocess.run(cmd, check=True)
    out = {}
    out["s"] = np.fromfile(d / "out.S.f32", dtype=np.float32).reshape(k, T).astype(np.float64)
    out["w"] = [np.fromfile(d / f"out.W{i}.f32", dtype=np.float32).reshape(-1, k).astype(np.float64)
                for i in range(len(views))]
    out["obj"] = np.fromfile(d / "out.obj.f64", dtype=np.float64)
    if coupling is not None:
        out["wr"] = [[np.fromfile(d / f"out.Wr{i}_{r}.f32", dtype=np.float32).reshape(-1, k).astype(np.float64)
                      for r in range(len(seglen))] for i in range(len(views))]
    out["starts"] = np.fromfile(d / "out.starts.f64", dtype=np.float64)
    if algo == "prob":
        out["rho2"] = np.fromfile(d / "out.rho2.f64", dtype=np.float64)
        out["sigma"] = np.fromfile(d / "out.sigma.f32", dtype=np.float32).reshape(k, k).astype(np.float64)
    niter_done, converged, _, best = open(d / "out.info.txt").read().split()
    out["niter"], out["converged"], out["best"] = int(niter_done), bool(int(converged)), int(best)
    return out


def run_load_update(cli, model_file, out, new_files, new_rows, niter, new_obs=None):
    cmd = [str(cli), "--load-model", str(model_file), "--out", str(out), "--niter", str(niter), "--tol", "0"]
    for f, r in zip(new_files, new_rows):
        cmd += ["--update-view", str(r), str(f)]
    for j, m in enumerate(new_obs or []):
        if m is not None and not m.all():
            fo = Path(f"{out}.uobs{j}.u8")
            m.astype(np.uint8).tofile(fo)
            cmd += ["--update-obs", str(j), str(fo)]
    subprocess.run(cmd, check=True)


def parse_srm_file(path):
    """A reader written from the format described in thd_srm.h, not from the C code."""
    hdr, subj, items, blocks = {}, [], [], {}
    lines = Path(path).read_text().splitlines()
    i = 0
    while i < len(lines):
        ln = lines[i].strip()
        i += 1
        if not ln or ln.startswith("#"):
            continue
        key, *rest = ln.split()
        if key == "data":
            name, r, c = rest[0], int(rest[1]), int(rest[2])
            vals = []
            while len(vals) < r * c:
                vals += lines[i].split()
                i += 1
            blocks[name] = np.array(vals[:r * c], dtype=np.float64).reshape(r, c)
        elif key == "subject":
            subj.append((rest[0], int(rest[1])))
        elif key == "item":
            items.append(rest[0])
        elif key == "end":
            break
        else:
            hdr[key] = rest
    return hdr, subj, items, blocks


def relfro(a, b):
    return np.linalg.norm(a - b) / np.linalg.norm(b)


def procrustes_residual(a, b):
    """|R a - b| / |b| for the best orthogonal R."""
    u, _, vt = np.linalg.svd(b @ a.T)
    return relfro(u @ vt @ a, b)

failures = []


def check(label, value, limit, fmt="{:.2e}"):
    ok = value <= limit
    print(f"    {label:70s} {fmt.format(value):>10s}  (limit {limit:g})  {'ok' if ok else 'FAIL'}")
    if not ok:
        failures.append(label)


def run(cmd, expect_ok=True):
    p = subprocess.run(["nice", "-n", "15"] + [str(x) for x in cmd], capture_output=True, text=True, env=ENV)
    if expect_ok and p.returncode != 0:
        print(p.stdout, p.stderr)
        raise SystemExit(f"command failed: {' '.join(str(x) for x in cmd)}")
    return p


# --------------------------------------------------------------------- data

NX, NY, NZ = 10, 10, 6
RUNS = [60, 50, 70]
K = 3


def make_world(rng, nsub=8):
    T = sum(RUNS)
    s = np.zeros((K, T))
    c0 = 0
    for L in RUNS:                                   # smooth shared signal, restarted in every run
        s[:, c0] = rng.standard_normal(K)
        for t in range(1, L):
            s[:, c0 + t] = 0.7 * s[:, c0 + t - 1] + np.sqrt(1 - 0.49) * rng.standard_normal(K)
        c0 += L
    s *= np.array([[1.6], [1.1], [0.7]])
    subs = []
    for i in range(nsub):
        mask = np.zeros((NX, NY, NZ), dtype=bool)
        if i < nsub // 2:
            mask[1:9, 1:9, 1:5] = True               # 256 voxels
        else:
            mask[2:8, 1:9, 1:5] = True               # 192 voxels: a different mask per subject
        idx = np.flatnonzero(mask.ravel(order="F"))  # AFNI order: x fastest
        v = len(idx)
        q, _ = np.linalg.qr(rng.standard_normal((v, K)))
        sigma = 0.6 + 0.15 * i
        x = q @ s + sigma * rng.standard_normal((v, T))
        x = x * rng.uniform(0.5, 3.0, (v, 1)) + rng.uniform(50, 500, (v, 1))     # scale and offset
        subs.append({"label": f"sub{i + 1:02d}", "group": "ctl" if i % 2 == 0 else "pat", "mask": mask,
                     "idx": idx, "x": x})
    # a voxel that never varies, one that is flat in a single run, and an all-zero volume
    subs[1]["x"][5, :] = 7.0
    if len(RUNS) > 1:
        subs[2]["x"][9, RUNS[0]:RUNS[0] + RUNS[1]] = 3.0
    return s, subs


def write_nifti(path, arr4d):
    nib.save(nib.Nifti1Image(arr4d.astype(np.float32), np.diag([2.0, 2.0, 2.0, 1.0])), str(path))


def write_world(d, subs, zero_volume=None):
    d.mkdir(parents=True, exist_ok=True)
    rows = []
    for sub in subs:
        c0 = 0
        mask_file = d / f"{sub['label']}_mask.nii.gz"
        write_nifti(mask_file, sub["mask"].astype(np.float32)[..., None])
        for r, L in enumerate(RUNS):
            vol = np.zeros((NX, NY, NZ, L), dtype=np.float32)
            flat = vol.reshape(-1, L, order="F")
            flat[sub["idx"], :] = sub["x"][:, c0:c0 + L]
            vol = flat.reshape((NX, NY, NZ, L), order="F")
            if zero_volume and zero_volume[0] == sub["label"] and zero_volume[1] == r:
                vol[..., zero_volume[2]] = 0.0
            f = d / f"{sub['label']}_run{r + 1}.nii.gz"
            write_nifti(f, vol)
            rows.append((sub["label"], sub["group"], f"r{r + 1}", mask_file, f))
            c0 += L
    return rows


def write_table(path, rows, with_mask):
    lines = ["Subj Group Run " + ("Mask " if with_mask else "") + "InputFile"]
    for s, g, r, m, f in rows:
        lines.append(f"{s} {g} {r} " + (f"{m} " if with_mask else "") + f"{f}")
    Path(path).write_text("\n".join(lines) + "\n")


def load_afni(prefix):
    """An output dataset by prefix, whichever form AFNI wrote it in."""
    for suf in ("+orig.HEAD", ".nii.gz", ".nii"):
        f = Path(str(prefix) + suf)
        if f.exists():
            return np.asarray(nib.load(str(f)).dataobj)
    raise FileNotFoundError(prefix)


def preprocess(sub, keep=None, zscore=True):
    """What 3dSRM should do: censor, z-score within runs, drop features that never vary."""
    x = sub["x"].astype(np.float32).astype(np.float64)
    cols = np.arange(x.shape[1]) if keep is None else np.flatnonzero(keep)
    x = x[:, cols]
    lens, c0 = [], 0
    for L in RUNS:
        lens.append(int(((cols >= c0) & (cols < c0 + L)).sum()))
        c0 += L
    z = np.zeros_like(x)
    bad = np.zeros(x.shape[0], dtype=bool)
    c0 = 0
    for L in lens:
        seg = x[:, c0:c0 + L]
        sd = seg.std(axis=1)
        bad |= ~(sd > 1e-7 * np.abs(seg.mean(axis=1)))
        z[:, c0:c0 + L] = (seg - seg.mean(axis=1, keepdims=True)) / np.where(sd > 0, sd, 1.0)[:, None]
        c0 += L
    return z[~bad], ~bad, lens


# --------------------------------------------------------------------- checks

def reference_fit(cli, workdir, tag, arrays, k, niter, algo, extra=None):
    views = [a.astype(np.float32) for a in arrays]
    return run_ours(cli, workdir, tag, algo, views, k, niter, False, extra=["--canon"] + (extra or []))


def case_basic(prog, cli, work, rng, with_mask_col, zcensor, algo):
    name = f"{'maskcol' if with_mask_col else 'globalmask'}_{'zc' if zcensor else 'nozc'}_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    s_true, subs = make_world(rng)
    zero = ("sub04", 1, 10) if zcensor else None
    rows = write_world(d, subs, zero)
    write_table(d / "table.txt", rows, with_mask_col)
    # global mask: the union geometry must serve every subject, so use the smaller common mask here
    if not with_mask_col:
        common = np.logical_and.reduce([s["mask"] for s in subs])
        write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
        for s in subs:                                # restrict every subject to the common mask
            sel = common.ravel(order="F")[s["idx"]]
            s["x"], s["idx"] = s["x"][sel], s["idx"][sel]
        rows = write_world(d, subs, zero)
        write_table(d / "table.txt", rows, False)
    niter = 60
    cmd = [prog, "-prefix", d / "out", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0,
           "-dataTableFile", d / "table.txt", "-save_W", "-progress", "line", "-overwrite"]
    if not with_mask_col:
        cmd += ["-mask", d / "mask.nii.gz"]
    if zcensor:
        cmd += ["-zcensor", "-cenmode", "drop"]          # the time point is dropped for everyone (checked below)
    p = run(cmd)
    log = p.stdout + p.stderr
    # the logging convention: the program is named once, on the banner
    named = [ln for ln in log.splitlines() if "3dSRM" in ln]
    check("the program name appears on the banner line only", float(len(named) - 1), 0.0, "{:.0f}")
    check("every message line starts with '++', '*+', '**' or ' +' (AFNI style)",
          float(sum(1 for ln in log.splitlines() if ln.strip() and not ln.startswith(("++", "*+", "**", " +", "\r")))),
          0.0, "{:.0f}")

    keep = None
    if zcensor:
        keep = np.ones(sum(RUNS), dtype=bool)
        keep[RUNS[0] + 10] = False
    pre = [preprocess(s, keep) for s in subs]
    T = pre[0][0].shape[1]
    ref = reference_fit(cli, work, name + "_ref", [z for z, _, _ in pre], K, niter, algo)

    S = np.loadtxt(d / "out.S.1D").T                     # file: one row per time point
    check("S.1D vs the library driver, relative difference", relfro(S, ref["s"]), 2e-4)
    fit = (d / "out.fit.txt").read_text().splitlines()
    rows_fit = [ln.split("\t") for ln in fit if not ln.startswith("#") and not ln.startswith("Subj")]
    used = [int(r[3]) for r in rows_fit]
    check("features used per subject (constant ones dropped)", float(sum(abs(u - ok.sum()) for u, (_, ok, _) in zip(used, pre))), 0.0, "{:.0f}")
    if algo == "prob":
        rho = np.array([float(r[5]) for r in rows_fit])
        check("rho2 vs the library driver, max relative", float(np.abs(rho - ref["rho2"]).max() / ref["rho2"].max()), 2e-3)
    # projected data and the maps
    worst_p = worst_w = 0.0
    for i, sub in enumerate(subs):
        z, ok, _ = pre[i]
        proj = load_afni(d / f"out.proj.{sub['label']}").reshape(K, T)
        worst_p = max(worst_p, relfro(proj, ref["w"][i].T @ z))
        wv = load_afni(d / f"out.W.{sub['label']}").reshape(-1, K, order="F")
        worst_w = max(worst_w, relfro(wv[sub["idx"][ok]], ref["w"][i]))
    check("projected series (dataset) vs W'X from the driver, worst subject", worst_p, 5e-3)
    check("W maps (-save_W) vs the driver, worst subject", worst_w, 5e-3)
    # the model file
    hdr, subj, _, blocks = parse_srm_file(d / "out.srm")
    check("model file: S matches S.1D", float(np.abs(blocks["S"] - S).max()), 2e-6)
    check("model file: runs and censoring recorded",
          float(int(hdr["nseg"][0]) != 3 or int(hdr["ntime_in"][0]) != sum(RUNS) or int(hdr["ntime"][0]) != T), 0.0, "{:.0f}")
    return log


def case_1D_and_errors(prog, cli, work, rng):
    print("\n[1D inputs, a mixed table, and refusals]", flush=True)
    d = work / "mixed"
    s_true, subs = make_world(rng, nsub=6)
    d.mkdir(parents=True, exist_ok=True)
    # two "EEG" subjects as 1D files, features = channels; they hear the same latent signal
    eeg = []
    for e in range(2):
        nch = 10 + 4 * e
        q, _ = np.linalg.qr(rng.standard_normal((nch, K)))
        x = (q @ s_true + 0.9 * rng.standard_normal((nch, sum(RUNS)))) * rng.uniform(0.5, 2, (nch, 1)) + 20
        eeg.append({"label": f"eeg{e + 1}", "group": "ctl", "x": x})
    rows = write_world(d, subs)
    lines = ["Subj Group Run Mask InputFile"] + [f"{s} {g} {r} {m} {f}" for s, g, r, m, f in rows]
    for e in eeg:
        c0 = 0
        for r, L in enumerate(RUNS):
            f = d / f"{e['label']}_run{r + 1}.1D"
            np.savetxt(f, e["x"][:, c0:c0 + L].T, fmt="%.8g")        # time x channels
            lines.append(f"{e['label']} {e['group']} r{r + 1} - {f}")
            c0 += L
    (d / "table.txt").write_text("\n".join(lines) + "\n")
    out = d / "out"
    p = run([prog, "-prefix", out, "-nfeatures", K, "-niter", 40, "-tol", 0, "-dataTableFile", d / "table.txt",
             "-progress", "off", "-save_W", "-proj_format", "1D", "-overwrite"])
    names = [s["label"] for s in subs] + [e["label"] for e in eeg]
    pre = [preprocess(s)[0] for s in subs] + [preprocess({"x": e["x"]})[0] for e in eeg]
    ref = reference_fit(cli, work, "mixed_ref", pre, K, 40, "prob")
    S = np.loadtxt(d / "out.S.1D").T
    check("a table mixing datasets and 1D subjects: S vs the driver", relfro(S, ref["s"]), 2e-4)
    pe = np.loadtxt(d / "out.proj.eeg1.1D").T
    check("a 1D subject's projected series (.1D) vs W'X", relfro(pe, ref["w"][len(subs)].T @ pre[len(subs)]), 5e-3)
    wmat = np.loadtxt(d / "out.W.eeg2.1D")
    check("a 1D subject's W (.1D), one row per channel", relfro(wmat, ref["w"][len(subs) + 1]), 5e-3)

    # refusals, each with a message that says what is wrong
    def refuses(label, args, text, table=None):
        t = table or (d / "table.txt")
        pp = run([prog, "-prefix", d / "ref", "-nfeatures", K, "-progress", "off", "-dataTableFile", t, "-overwrite"] + args,
                 expect_ok=False)
        check(f"refuses: {label}", float(not (pp.returncode != 0 and text in (pp.stdout + pp.stderr))), 0.0, "{:.0f}")

    refuses("-cv_adapt without -run_coupling", ["-cv_adapt", "0.25", "-cv", "run"], "-cv_adapt needs -run_coupling")
    refuses("-reliability within without -cv", ["-reliability", "within"], "needs -cv run or -cv loso")
    refuses("several -nfeatures values without -cv", ["-nfeatures", "3,4"], "need -cv")
    refuses("-save_r2 without -cv", ["-save_r2"], "need -cv")
    bad = d / "bad_nomask.txt"
    bad.write_text("\n".join(["Subj Run InputFile"] + [f"{s} {r} {f}" for s, g, r, m, f in rows[:6]]) + "\n")
    refuses("datasets with no mask anywhere", [], "need a mask", table=bad)
    short = d / "bad_run.txt"
    short.write_text("\n".join(lines[:-1]) + "\n")                 # drops one run of the last 1D subject
    refuses("a subject missing a run", [], "", table=short)
    before = (d / "out.S.1D").stat().st_mtime_ns
    again = run([prog, "-prefix", out, "-nfeatures", K, "-niter", 5, "-dataTableFile", d / "table.txt", "-progress", "off"],
                expect_ok=False)
    check("refuses: existing outputs without -overwrite", float(not (again.returncode != 0 and "already exists" in again.stdout + again.stderr)), 0.0, "{:.0f}")
    # the refusal comes before the work: it must not have rewritten anything
    check("... and left the earlier output untouched", float((d / "out.S.1D").stat().st_mtime_ns != before), 0.0, "{:.0f}")


# --------------------------------------------------------------------- cross-validation

def np_polar(a):
    u, _, vt = np.linalg.svd(a, full_matrices=False)
    return u @ vt


def np_fisher(r):
    return float(np.tanh(np.arctanh(np.clip(r, -0.9999999, 0.9999999)).mean()))


def np_isc(p, ref):
    return np_fisher(np.array([np.corrcoef(p[c], ref[c])[0, 1] for c in range(p.shape[0])]))


def np_segment_match(a, b, win):
    """BrainIAK-example recipe for one subject against one reference."""
    k, T = a.shape
    nseg = T - win
    za = np.array([a[:, i:i + win].ravel() for i in range(nseg)])
    zb = np.array([b[:, i:i + win].ravel() for i in range(nseg)])
    za = (za - za.mean(1, keepdims=True)); za /= np.maximum(np.linalg.norm(za, axis=1, keepdims=True), 1e-300)
    zb = (zb - zb.mean(1, keepdims=True)); zb /= np.maximum(np.linalg.norm(zb, axis=1, keepdims=True), 1e-300)
    cm = za @ zb.T
    for i in range(nseg):
        for j in range(nseg):
            if abs(i - j) < win and i != j:
                cm[i, j] = -np.inf
    ncand = np.array([1 + sum(1 for j in range(nseg) if abs(i - j) >= win) for i in range(nseg)])
    return float((np.argmax(cm, axis=1) == np.arange(nseg)).mean()), float(np.mean(1.0 / ncand))


def read_scores(path):
    """A scores table. For scores_folds.txt, the 'all' rows without the Factor and Level columns (the layout the
    checks were written for); read_levels gives the -score_by rows."""
    rows = [ln.split("\t") for ln in Path(path).read_text().splitlines() if ln and not ln.startswith("#")]
    head, body = rows[0], rows[1:]
    if "Fold" in head and "Factor" in head:
        fi, li = head.index("Factor"), head.index("Level")
        keep = [c for c in range(len(head)) if c not in (fi, li)]
        body = [[r[c] for c in keep] for r in body if r[fi] == "all"]
        head = [head[c] for c in keep]
    return head, body


def read_levels(path):
    """Rows of a scores file as dicts keyed by column name."""
    rows = [ln.split("\t") for ln in Path(path).read_text().splitlines() if ln and not ln.startswith("#")]
    return [dict(zip(rows[0], r)) for r in rows[1:]]


def num(x):
    return float("nan") if x == "NA" else float(x)


def make_cv_world(rng, runs, nsub=8):
    """A world on a common mask with the given run structure."""
    global RUNS
    old = RUNS
    RUNS = runs
    try:
        s_true, subs = make_world(rng, nsub)
        common = np.logical_and.reduce([s["mask"] for s in subs])
        for s in subs:
            sel = common.ravel(order="F")[s["idx"]]
            s["x"], s["idx"] = s["x"][sel], s["idx"][sel]
        return s_true, subs, common
    finally:
        RUNS = old


def case_cv(prog, cli, work, rng, mode, algo, runs, win=5, K_=3, niter=50, label=""):
    global RUNS
    name = f"cv_{mode}_{algo}_{'x'.join(map(str, runs))}{label}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    old = RUNS
    RUNS = runs
    try:
        s_true, subs, common = make_cv_world(rng, runs)
        rows = write_world(d, subs)
        write_table(d / "table.txt", rows, False)
        write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
        cmd = [prog, "-prefix", d / "out", "-nfeatures", K_, "-algorithm", algo, "-niter", niter, "-tol", 0,
               "-dataTableFile", d / "table.txt", "-mask", d / "mask.nii.gz", "-cv", mode, "-match_window", win,
               "-save_r2", "-progress", "off", "-overwrite"]
        p = run(cmd)
        log = p.stdout + p.stderr
        named = [ln for ln in log.splitlines() if "3dSRM" in ln]
        check("the program name appears on the banner line only", float(len(named) - 1), 0.0, "{:.0f}")
        pre = [preprocess(s)[0] for s in subs]
        n, T = len(pre), pre[0].shape[1]
        folds = []
        c0 = 0
        for L in runs:
            folds.append((c0, L))
            c0 += L
        if len(runs) == 1:                                   # one run: two contiguous blocks
            folds = [(0, T // 2), (T // 2, T - T // 2)]
        head, body = read_scores(d / "out.scores_folds.txt")
        got = {(r[0], r[2]): [num(r[4]), num(r[5]), num(r[6]), num(r[7])] for r in body}
        names = [s["label"] for s in subs]
        fold_names = sorted({r[2] for r in body}, key=lambda x: [r[2] for r in body].index(x))
        want = {}
        vox_ref = [np.zeros(z.shape[0]) for z in pre]
        if mode == "run":
            full = reference_fit(cli, work, name + "_full", pre, K_, niter, algo)
            for fi_, (c0, L) in enumerate(folds):
                tr = [np.delete(z, np.arange(c0, c0 + L), axis=1) for z in pre]
                te = [z[:, c0:c0 + L] for z in pre]
                r = reference_fit(cli, work, f"{name}_f{fi_}", tr, K_, niter, algo)
                P = [r["w"][i].T @ te[i] for i in range(n)]
                for i in range(n):
                    oth = [j for j in range(n) if j != i]
                    ref_u = sum(P[j] for j in oth) / len(oth)
                    if algo == "prob":
                        prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(K_)
                        ref_r = np.linalg.solve(prec, sum(P[j] / r["rho2"][j] for j in oth))
                    else:
                        ref_r = ref_u
                    acc, ch = np_segment_match(P[i], ref_u, win)
                    xh = r["w"][i] @ ref_r
                    sse = ((te[i] - xh) ** 2).sum(1)
                    sst = ((te[i] - te[i].mean(1, keepdims=True)) ** 2).sum(1)
                    want[(names[i], fold_names[fi_])] = [np_isc(P[i], ref_u), acc, ch, 1.0 - sse.sum() / sst.sum()]
                    vox_ref[i] += np.where(sst > 0, 1.0 - sse / np.where(sst > 0, sst, 1), 0.0) / len(folds)
                # held-out projections, compared up to a rotation of the gauge, in this fold's columns
                if fi_ == 0:
                    pass
            worst_proj = 0.0
            for i in range(n):
                pj = load_afni(d / f"out.proj.{names[i]}").reshape(K_, T)
                for fi_, (c0, L) in enumerate(folds):
                    tr = [np.delete(z, np.arange(c0, c0 + L), axis=1) for z in pre]
                    r = run_ours(cli, work, f"{name}_f{fi_}", algo, [x.astype(np.float32) for x in tr], K_, niter, False,
                                   extra=["--canon"])
                    Pref = r["w"][i].T @ pre[i][:, c0:c0 + L]
                    worst_proj = max(worst_proj, procrustes_residual(pj[:, c0:c0 + L], Pref))
            check("held-out projections equal the fold models' up to a rotation of the gauge", worst_proj, 2e-3)
        else:
            # leave one subject out: warm start from the full fit without the subject, loso_iter = 5 steps
            full = reference_fit(cli, work, name + "_full", pre, K_, niter, algo)
            for i in range(n):
                oth = [j for j in range(n) if j != i]
                dd = work / f"{name}_drop{i}"
                dd.mkdir(parents=True, exist_ok=True)
                extra = []
                if algo == "prob":
                    full["rho2"][oth].astype(np.float64).tofile(dd / "rho2.f64")
                    full["sigma"].astype(np.float32).tofile(dd / "sigma.f32")
                    extra = ["--init-rho2", str(dd / "rho2.f64"), "--init-sigma", str(dd / "sigma.f32")]
                r = run_ours(cli, work, f"{name}_drop{i}", algo, [pre[j].astype(np.float32) for j in oth], K_, 5, False,
                               init_w=[full["w"][j] for j in oth], extra=extra)
                for fi_, (c0, L) in enumerate(folds):
                    tr = np.delete(pre[i], np.arange(c0, c0 + L), axis=1)
                    te = pre[i][:, c0:c0 + L]
                    s_tr = np.delete(r["s"], np.arange(c0, c0 + L), axis=1)
                    s_te = r["s"][:, c0:c0 + L]
                    W = np_polar(tr @ s_tr.T)
                    Pm = W.T @ te
                    acc, ch = np_segment_match(Pm, s_te, win)
                    sse = ((te - W @ s_te) ** 2).sum(1)
                    sst = ((te - te.mean(1, keepdims=True)) ** 2).sum(1)
                    want[(names[i], fold_names[fi_])] = [np_isc(Pm, s_te), acc, ch, 1.0 - sse.sum() / sst.sum()]
                    vox_ref[i] += np.where(sst > 0, 1.0 - sse / np.where(sst > 0, sst, 1), 0.0) / len(folds)
        errs = np.array([[abs(got[k_][q] - want[k_][q]) for q in range(4)] for k_ in want])
        check("isc, per subject and fold, vs the reference", float(errs[:, 0].max()), 5e-4)
        # the tables carry 7 significant digits, so a fraction like 0.78431372 is rounded in the file
        check("segment-matching accuracy vs the reference", float(errs[:, 1].max()), 1e-6)
        check("... and its chance level", float(errs[:, 2].max()), 1e-6)
        check("held-out R^2 vs the reference", float(errs[:, 3].max()), 5e-4)
        worst_r2map = 0.0
        for i in range(n):
            z, ok, _ = (pre[i], None, None)
            vol = load_afni(d / f"out.r2.{names[i]}").reshape(-1, order="F")
            # the voxels used by this subject, in dataset order
            _, okmask, _ = preprocess(subs[i])
            worst_r2map = max(worst_r2map, float(np.abs(vol[subs[i]["idx"][okmask]] - vox_ref[i]).max()))
        check("voxelwise held-out R^2 maps (-save_r2) vs the reference", worst_r2map, 2e-3)
        # per-subject file: the mean over folds, and the carried covariates, and the factor columns
        head_s, body_s = read_scores(d / "out.scores.txt")
        check("scores.txt: one row per subject with Factor/Level columns", float(not (len(body_s) == len(subs) and head_s[2:4] == ["Factor", "Level"])), 0.0, "{:.0f}")
        mean_isc = np.array([np.mean([want[(names[i], f)][0] for f in fold_names]) for i in range(len(subs))])
        check("scores.txt: the subject mean over folds", float(np.abs(np.array([num(r[5]) for r in body_s]) - mean_isc).max()), 5e-4)
    finally:
        RUNS = old


def np_adapt_w(xa, sa, anchor, lam):
    """The contract of SRM_fit_view_coupled for one run: polar(X S' + lambda s2 W_anchor), s2 = |S|^2 / (k T)."""
    s2 = (sa ** 2).sum() / sa.size
    return np_polar(xa @ sa.T + lam * s2 * anchor)


def adapt_ref_run(cli, work, tag, pre, names, runs, algo, lam, frac, win, K_, niter, streams=None):
    """The -cv run -cv_adapt scores of these subjects, fitted among themselves: {(Subj, Fold): {"run": [...], "anchor": [...]}}
    with [isc, match, chance, r2, nsamp]."""
    n, want = len(pre), {}
    starts = np.cumsum([0] + list(runs[:-1]))
    for fi_, (c0, L) in enumerate(zip(starts, runs)):
        tr = [np.delete(z, np.arange(c0, c0 + L), axis=1).astype(np.float32) for z in pre]
        r = run_ours(cli, work, f"{tag}_f{fi_}", algo, tr, K_, niter, False,
                     extra=["--canon"] + (["--streams", ",".join(map(str, streams))] if streams is not None else []),
                     seglen=[x for q, x in enumerate(runs) if q != fi_], coupling=lam)
        na = int(frac * L)
        te = [z[:, c0:c0 + L] for z in pre]
        P = [r["w"][i].T @ te[i] for i in range(n)]
        sa = sum(Pi[:, :na] for Pi in P) / n
        Wr = [np_adapt_w(te[i][:, :na], sa, r["w"][i], lam) for i in range(n)]
        xr = [te[i][:, na:] for i in range(n)]
        for W_list in ("run", "anchor"):
            Ws = Wr if W_list == "run" else r["w"]
            Pr = [Ws[i].T @ xr[i] for i in range(n)]
            for i in range(n):
                oth = [j for j in range(n) if j != i]
                ref_u = sum(Pr[j] for j in oth) / len(oth)
                if algo == "prob":
                    prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(K_)
                    ref_r = np.linalg.solve(prec, sum(Pr[j] / r["rho2"][j] for j in oth))
                else:
                    ref_r = ref_u
                acc, ch = np_segment_match(Pr[i], ref_u, win)
                sse = ((xr[i] - Ws[i] @ ref_r) ** 2).sum()
                sst = ((xr[i] - xr[i].mean(1, keepdims=True)) ** 2).sum()
                want.setdefault((names[i], f"r{fi_ + 1}"), {})[W_list] = [np_isc(Pr[i], ref_u), acc, ch, 1.0 - sse / sst, L - na]
    return want


def adapt_errors(got, want):
    """[nsamp, isc, match, chance, r2, isc_anchor, match_anchor, r2_anchor] errors, one row per (Subj, Fold)."""
    e = []
    for k_, v in want.items():
        g = got[k_]
        e.append([abs(g[0] - v["run"][4]), abs(g[1] - v["run"][0]), abs(g[2] - v["run"][1]), abs(g[3] - v["run"][2]),
                  abs(g[4] - v["run"][3]), abs(g[5] - v["anchor"][0]), abs(g[6] - v["anchor"][1]), abs(g[7] - v["anchor"][3])])
    return np.array(e)


def case_cv_adapt(prog, cli, work, rng, algo, runs=(60, 50, 70), lam=30.0, frac=0.3, win=5, K_=3, niter=40):
    """-cv_adapt under -cv run, against a numpy reference built on the driver's coupled fold fits; and under
    -cv loso, the limits: a very large lambda makes the run's W the anchor."""
    global RUNS
    runs = list(runs)
    name = f"cv_adapt_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    old = RUNS
    RUNS = runs
    try:
        s_true, subs, common = make_cv_world(rng, runs, nsub=6)
        rows = write_world(d, subs)
        pre = [preprocess(s)[0] for s in subs]
    finally:
        RUNS = old
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    names = [s["label"] for s in subs]
    n, T = len(pre), pre[0].shape[1]
    base = [prog, "-nfeatures", K_, "-algorithm", algo, "-niter", niter, "-tol", 0, "-dataTableFile", d / "table.txt",
            "-mask", d / "mask.nii.gz", "-match_window", win, "-progress", "off", "-overwrite"]
    p = run(base + ["-prefix", d / "run", "-run_coupling", lam, "-cv", "run", "-cv_adapt", frac])
    log = p.stdout + p.stderr
    check("the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    check("the log reports the anchor's scores beside the run W's", float("the anchor W on the same samples" not in log), 0.0, "{:.0f}")
    head, body = read_scores(d / "run.scores_folds.txt")
    check("scores_folds.txt has isc_anchor, match_anchor, r2_anchor", float(head[-3:] != ["isc_anchor", "match_anchor", "r2_anchor"]), 0.0, "{:.0f}")
    got = {(r[0], r[2]): [num(x) for x in r[3:11]] for r in body}
    want = adapt_ref_run(cli, work, name, pre, names, runs, algo, lam, frac, win, K_, niter)   # S of the first samples: all anchors
    e = adapt_errors(got, want)
    check("nsamp: the samples after each run's first FRAC", float(e[:, 0].max()), 0.0, "{:.0f}")
    check("run W: held-out isc vs numpy (S of the first samples from all anchors)", float(e[:, 1].max()), 5e-4)
    check("run W: segment matching vs numpy", float(e[:, 2].max()), 1e-6)
    check("run W: chance level", float(e[:, 3].max()), 1e-6)
    check("run W: held-out R^2 vs numpy", float(e[:, 4].max()), 5e-4)
    check("anchor: isc on the same samples vs numpy", float(e[:, 5].max()), 5e-4)
    check("anchor: segment matching on the same samples vs numpy", float(e[:, 6].max()), 1e-6)
    check("anchor: R^2 on the same samples vs numpy", float(e[:, 7].max()), 5e-4)
    rows_s = read_levels(d / "run.scores.txt")
    mean_ok = all(abs(num(r_["isc_anchor"]) - np.mean([want[(r_["Subj"], f"r{q + 1}")]["anchor"][0] for q in range(len(runs))])) < 5e-4
                  and abs(num(r_["isc"]) - np.mean([want[(r_["Subj"], f"r{q + 1}")]["run"][0] for q in range(len(runs))])) < 5e-4
                  for r_ in rows_s)
    check("scores.txt: the fold means of both, side by side", float(not mean_ok or len(rows_s) != n), 0.0, "{:.0f}")

    if algo != "prob":
        return
    # -cv loso: lambda so large that the run's W is the anchor, so both columns agree; a modest lambda differs
    run(base + ["-prefix", d / "loso_big", "-run_coupling", 1e9, "-cv", "loso", "-cv_adapt", frac])
    _, bb = read_scores(d / "loso_big.scores_folds.txt")
    diff = max(max(abs(num(r[4]) - num(r[8])), abs(num(r[5]) - num(r[9])), abs(num(r[7]) - num(r[10]))) for r in bb)
    check("-cv loso, lambda 1e9: the run's W is the anchor (scores agree)", diff, 1e-4)
    check("-cv loso: nsamp is the rest of each run", float(any(int(r[3]) != L - int(frac * L) for r in bb
                                                              for q, L in enumerate(runs) if r[2] == f"r{q + 1}")), 0.0, "{:.0f}")
    run(base + ["-prefix", d / "loso", "-run_coupling", lam, "-cv", "loso", "-cv_adapt", frac])
    _, bl = read_scores(d / "loso.scores_folds.txt")
    check("-cv loso, lambda 30: the run's W changes the scores", float(max(abs(num(r[4]) - num(r[8])) for r in bl) < 1e-4), 0.0, "{:.0f}")

    def fails(args, needle, label):
        pr = run(base + ["-prefix", d / "bad"] + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    fails(["-run_coupling", lam, "-cv_adapt", frac], "needs -cv run or -cv loso", "E: -cv_adapt without -cv")
    fails(["-run_coupling", lam, "-cv", "run", "-cv_adapt", 0.02], "need at least 3 (-nfeatures)", "E: too few samples to estimate the run's W")
    fails(["-run_coupling", lam, "-cv", "run", "-cv_adapt", 0.9], "for time-segment matching", "E: too few samples left to score")

    # -cv_fit within: each group's own -cv run, adapted, against the same reference on that group alone
    run(base + ["-prefix", d / "within", "-run_coupling", lam, "-cv", "run", "-cv_adapt", frac, "-cv_fit", "within"])
    _, bw = read_scores(d / "within.scores_folds.txt")
    gotw = {(r[0], r[2]): [num(x) for x in r[3:11]] for r in bw}
    ew = []
    for g in ("ctl", "pat"):
        idx = [i for i, s_ in enumerate(subs) if s_["group"] == g]
        wg = adapt_ref_run(cli, work, f"{name}_w{g}", [pre[i] for i in idx], [names[i] for i in idx], runs, algo, lam, frac, win, K_, niter,
                           streams=idx)
        ew.append(adapt_errors(gotw, wg))
    ew = np.vstack(ew)
    check("-cv_fit within -cv_adapt: run W scores vs each group's own reference", float(ew[:, 1:5].max()), 5e-4)
    check("-cv_fit within -cv_adapt: anchor scores vs each group's own reference", float(ew[:, 5:8].max()), 5e-4)
    # -balance with groups of equal size: every draw is the whole table, so the result is the plain one
    run(base + ["-prefix", d / "bal", "-run_coupling", lam, "-cv", "run", "-cv_adapt", frac, "-balance", 2])
    _, bbal = read_scores(d / "bal.scores_folds.txt")
    gotb = {(r[0], r[2]): [num(x) for x in r[3:11]] for r in bbal}
    check("-balance -cv_adapt, equal groups: both columns averaged over draws = the plain run",
          float(max(abs(gotb[k_][q] - got[k_][q]) for k_ in got for q in range(8))), 1e-6)
    # -cv_fit reference: the other group is placed in the reference fit's S and adapts too; lambda 1e9 makes it the anchor
    run(base + ["-prefix", d / "ref", "-run_coupling", 1e9, "-cv", "loso", "-cv_adapt", frac, "-cv_fit", "reference:ctl"])
    _, brf = read_scores(d / "ref.scores_folds.txt")
    check("-cv_fit reference -cv_adapt, lambda 1e9: run W = anchor for both groups",
          max(abs(num(r[4]) - num(r[8])) for r in brf), 1e-4)
    check("... and the non-reference group is scored on the adapted samples too",
          float(any(int(r[3]) != L - int(frac * L) for r in brf for q, L in enumerate(runs) if r[2] == f"r{q + 1}")), 0.0, "{:.0f}")


def compare_compress(d, a, b, names, maps=False, recon=False, r2map=False, tol=1e-4, score_tol=1e-4, label=""):
    """A run without -compress (prefix a) against one with it (prefix b): S, every score, projections, and (maps)
    W and the reconstructions on the voxels; fit.txt and the subjects' sizes in the model file."""
    out = {}
    S1, S2 = np.loadtxt(d / f"{a}.S.1D", comments="#"), np.loadtxt(d / f"{b}.S.1D", comments="#")
    out["S"] = relfro(S2, S1)
    if (d / f"{a}.scores_folds.txt").exists():
        r1, r2 = read_levels(d / f"{a}.scores_folds.txt"), read_levels(d / f"{b}.scores_folds.txt")
        e = 0.0
        for x, y in zip(r1, r2):
            for k_ in x:
                if k_ in ("Subj", "Group", "Fold", "Factor", "Level"):
                    continue
                if x[k_] == "NA" or y[k_] == "NA":
                    e = max(e, float(x[k_] != y[k_]))
                else:
                    e = max(e, abs(float(x[k_]) - float(y[k_])))
        out["scores"] = e
        r1, r2_ = read_levels(d / f"{a}.scores.txt"), read_levels(d / f"{b}.scores.txt")
        e = 0.0
        for x, y in zip(r1, r2_):                         # the per-subject file too (reliability columns and all)
            for k_ in x:
                if k_ in ("Subj", "Group", "Factor", "Level"):
                    continue
                e = max(e, float(x[k_] != y[k_]) if "NA" in (x[k_], y[k_]) else abs(float(x[k_]) - float(y[k_])))
        out["scores.txt"] = e
    pj = 0.0
    for nm in names:
        p1 = Path(d / f"{a}.proj.{nm}.1D")
        if p1.exists():
            pj = max(pj, relfro(np.loadtxt(d / f"{b}.proj.{nm}.1D", comments="#"), np.loadtxt(p1, comments="#")))
        else:
            pj = max(pj, relfro(load_afni(d / f"{b}.proj.{nm}"), load_afni(d / f"{a}.proj.{nm}")))
    out["proj"] = pj
    for what, on in (("W", maps), ("recon", recon), ("r2", r2map)):
        if not on:
            continue
        e = 0.0
        for nm in names:
            f1 = Path(d / f"{a}.{what}.{nm}.1D")
            if f1.exists():
                e = max(e, relfro(np.loadtxt(d / f"{b}.{what}.{nm}.1D", comments="#"), np.loadtxt(f1, comments="#")))
            else:
                e = max(e, relfro(load_afni(d / f"{b}.{what}.{nm}"), load_afni(d / f"{a}.{what}.{nm}")))
        out[what] = e
    f1 = [ln.split("\t")[:5] for ln in (d / f"{a}.fit.txt").read_text().splitlines() if not ln.startswith("#")]
    f2 = [ln.split("\t")[:5] for ln in (d / f"{b}.fit.txt").read_text().splitlines() if not ln.startswith("#")]
    out["fit.txt"] = float(f1 != f2)
    _, s1, _, _ = parse_srm_file(d / f"{a}.srm")
    _, s2, _, _ = parse_srm_file(d / f"{b}.srm")
    out["model"] = float(s1 != s2)
    tag = f"-compress {label}: "
    check(tag + "S as without it", out["S"], tol)
    if "scores" in out:
        check(tag + "every held-out score as without it", out["scores"], score_tol)
    check(tag + "projections as without it", out["proj"], tol)
    if maps:
        check(tag + "W on the voxels (X M, the subject read again)", out["W"], tol)
    if recon:
        check(tag + "reconstructions on the voxels", out["recon"], tol)
    if r2map:
        check(tag + "-save_r2 maps (each fold's W on the voxels)", out["r2"], 1e-4)
    if "scores.txt" in out:
        check(tag + "scores.txt as without it", out["scores.txt"], score_tol)
    check(tag + "fit.txt counts voxels, as without it", out["fit.txt"], 0.0, "{:.0f}")
    check(tag + "the model file lists each subject's voxels", out["model"], 0.0, "{:.0f}")
    return out


def case_compress(prog, cli, work, rng):
    """-compress (each subject held as R, R'R = X'X) against the default, on data with more voxels than time points."""
    global RUNS
    print("\n[-compress against the default]", flush=True)
    d = work / "compress"
    old = RUNS
    RUNS = [60, 50, 70]
    try:
        _, subs, common = make_cv_world(rng, RUNS, nsub=6)
        rows = write_world(d, subs)
    finally:
        RUNS = old
    names = [s_["label"] for s_ in subs]
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    T = 180
    miss = []
    for i, s_ in enumerate(subs):                       # censoring, for the missing-data and drop cases
        m = rng.uniform(size=T) < 0.06
        m[20 + 4 * i:23 + 4 * i] = True
        miss.append(m)
        np.savetxt(d / f"{s_['label']}_cens.1D", (~m).astype(int), fmt="%d")
    (d / "cens.txt").write_text("Subj Group Run Censor InputFile\n" + "\n".join(
        f"{s_} {g} {r} {d / (s_ + '_cens.1D')} {f}" for s_, g, r, _, f in rows) + "\n")
    base = ["-mask", d / "mask.nii.gz", "-progress", "off", "-overwrite", "-niter", 30, "-tol", 0, "-match_window", 5]
    p = run([prog, "-prefix", d / "c0", "-nfeatures", K, "-cv", "run", "-dataTableFile", d / "table.txt"] + base)
    check("-compress is the default when every subject has more voxels than time points (192 > 180)",
          float("-compress: every subject is held" not in p.stdout + p.stderr), 0.0, "{:.0f}")
    pr = run([prog, "-prefix", d / "bad", "-nfeatures", K, "-dataTableFile", d / "table.txt", "-compress", "-no_compress"] + base,
             expect_ok=False)
    check("E: -compress with -no_compress", float(pr.returncode == 0 or "not both" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")
    log = p.stdout + p.stderr
    check("-compress: the log gives the factors' size, and says the start is the default's",
          float("-compress: every subject is held as a rank x 180 factor" not in log or "the same random start" not in log), 0.0, "{:.0f}")
    check("-compress: the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    cases = [
        ("prob, -cv run, -save_W -save_recon -save_r2", ["-nfeatures", K, "-cv", "run", "-save_W", "-save_recon", "-save_r2"], "table.txt", True, True),
        ("det, -cv loso -save_r2", ["-nfeatures", K, "-algorithm", "det", "-cv", "loso", "-save_r2"], "table.txt", False, False),
        ("-cenmode missing, -cv run -save_r2", ["-nfeatures", K, "-cv", "run", "-cenmode", "missing", "-save_W", "-save_r2"], "cens.txt", True, False),
        ("-cenmode drop, -cv run", ["-nfeatures", K, "-cv", "run", "-cenmode", "drop"], "cens.txt", False, False),
        ("-cenmode interp", ["-nfeatures", K, "-cv", "run"], "cens.txt", False, False),
        ("-run_coupling, -cv_adapt -save_r2", ["-nfeatures", K, "-cv", "run", "-run_coupling", 30, "-cv_adapt", 0.3, "-save_r2"], "table.txt", False, False),
        ("-cv loso -cv_adapt -cenmode missing -save_r2", ["-nfeatures", K, "-cv", "loso", "-run_coupling", 30, "-cv_adapt", 0.3,
                                                          "-cenmode", "missing", "-save_r2"], "cens.txt", False, False),
        ("-zscore none (centered fits), maps", ["-nfeatures", K, "-zscore", "none", "-save_W", "-save_recon"], "table.txt", True, True),
        ("-nfeatures 2,3 scan", ["-nfeatures", "2,3", "-cv", "run"], "table.txt", False, False),
    ]
    for label, args, tab, maps, recon in cases:
        a, b = "ref_" + str(abs(hash(label)) % 10**6), "cmp_" + str(abs(hash(label)) % 10**6)
        run([prog, "-prefix", d / a, "-dataTableFile", d / tab, "-no_compress"] + base + args)
        run([prog, "-prefix", d / b, "-dataTableFile", d / tab, "-compress"] + base + args)
        if "scan" in label:
            n1 = [ln for ln in (d / f"{a}.nfeat.txt").read_text().splitlines() if ln and not ln.startswith("#")][1:]
            n2 = [ln for ln in (d / f"{b}.nfeat.txt").read_text().splitlines() if ln and not ln.startswith("#")][1:]
            e = max(abs(float(x) - float(y)) for l1, l2 in zip(n1, n2) for x, y in zip(l1.split()[1:], l2.split()[1:]))
            check("-compress -nfeatures 2,3: the scan as without it (the start of each k)", e, 1e-4)
            continue
        compare_compress(d, a, b, names, maps=maps, recon=recon, r2map=("-save_r2" in args), label=label)
    # fits of group subsets: every subject starts from its own whole-table start, so these are exact too
    for label, args in (("-cv_fit within -save_r2", ["-nfeatures", K, "-cv", "run", "-cv_fit", "within", "-save_r2"]),
                        ("-cv_fit reference -balance", ["-nfeatures", K, "-cv", "loso", "-cv_fit", "reference:ctl", "-balance", 2]),
                        ("-reliability split", ["-nfeatures", K, "-cv", "run", "-reliability", "split", "-nsplit", 3])):
        a, b = "sub_ref_" + str(abs(hash(label)) % 10**6), "sub_cmp_" + str(abs(hash(label)) % 10**6)
        run([prog, "-prefix", d / a, "-dataTableFile", d / "table.txt", "-no_compress"] + base + args)
        run([prog, "-prefix", d / b, "-dataTableFile", d / "table.txt", "-compress"] + base + args)
        compare_compress(d, a, b, names, r2map=("-save_r2" in args), label=label)
    # the item axis (one subject of .1D items), and -repeated_stimulus with -reliability within
    inames, isubs = make_item_world(rng, nitem=30, nrep=4, nsub=5, n1d=1, shared_runs=True, run_offsets=False)
    irows = write_item_world(d / "items", inames, isubs, rng)
    write_item_table(d / "items.txt", irows)
    ib = ["-axis", "item", "-nfeatures", K, "-cv", "run", "-progress", "off", "-overwrite", "-niter", 30, "-tol", 0, "-proj_format", "1D",
          "-reliability", "within"]
    run([prog, "-prefix", d / "it_ref", "-dataTableFile", d / "items.txt", "-no_compress"] + ib)
    p = run([prog, "-prefix", d / "it_cmp", "-dataTableFile", d / "items.txt", "-compress"] + ib)
    pa = run([prog, "-prefix", d / "it_auto", "-dataTableFile", d / "items.txt"] + ib)
    check("-compress is off by default when a subject has no more features than items (a .1D subject)",
          float("held whole (not compressed): 1 subject has no more features than items" not in pa.stdout + pa.stderr), 0.0, "{:.0f}")
    compare_compress(d, "it_ref", "it_cmp", [s_["label"] for s_ in isubs], label="-axis item -reliability within")
    RUNS = [50, 50, 50]
    try:
        _, rsubs, rcommon = make_cv_world(rng, RUNS, nsub=5)
        rrows = write_world(d / "rep", rsubs)
    finally:
        RUNS = old
    write_table(d / "rep.txt", rrows, False)
    write_nifti(d / "rep_mask.nii.gz", rcommon.astype(np.float32)[..., None])
    rb = ["-mask", d / "rep_mask.nii.gz", "-nfeatures", K, "-repeated_stimulus", "-reliability", "within", "-cv", "run",
          "-match_window", 4, "-progress", "off", "-overwrite", "-niter", 30, "-tol", 0]
    run([prog, "-prefix", d / "rep_ref", "-dataTableFile", d / "rep.txt", "-no_compress"] + rb)
    run([prog, "-prefix", d / "rep_cmp", "-dataTableFile", d / "rep.txt", "-compress"] + rb)
    compare_compress(d, "rep_ref", "rep_cmp", [s_["label"] for s_ in rsubs], label="-repeated_stimulus -reliability within")
    # -cenmode drop -zcensor: all-zero volumes (two subjects) drop for everyone; -compress finds them in a first read
    RUNS = [60, 50, 70]
    try:
        _, zsubs, zcommon = make_cv_world(rng, RUNS, nsub=5)
        zrows = write_world(d / "zc", zsubs, zero_volume=(zsubs[1]["label"], 1, 7))
        zrows2 = write_world(d / "zc2", zsubs[2:3], zero_volume=(zsubs[2]["label"], 2, 30))
    finally:
        RUNS = old
    zrows = [r_ for r_ in zrows if r_[0] != zsubs[2]["label"]] + zrows2
    (d / "zc.txt").write_text("Subj Group Run Censor InputFile\n" + "\n".join(
        f"{s_} {g} {r} {d / (zsubs[0]['label'] + '_cens.1D') if False else '-'} {f}" for s_, g, r, _, f in zrows) + "\n")
    write_nifti(d / "zc_mask.nii.gz", zcommon.astype(np.float32)[..., None])
    zb = ["-mask", d / "zc_mask.nii.gz", "-nfeatures", K, "-cv", "run", "-cenmode", "drop", "-zcensor", "-match_window", 5,
          "-progress", "off", "-overwrite", "-niter", 30, "-tol", 0]
    run([prog, "-prefix", d / "zc_ref", "-dataTableFile", d / "zc.txt", "-no_compress"] + zb)
    p = run([prog, "-prefix", d / "zc_cmp", "-dataTableFile", d / "zc.txt", "-compress"] + zb)
    check("-compress -cenmode drop -zcensor: says it reads every subject once more",
          float("read once more, first" not in p.stdout + p.stderr), 0.0, "{:.0f}")
    check("... and the two zero volumes are dropped for everyone (178 of 180 kept)",
          float(np.loadtxt(d / "zc_cmp.S.1D", comments="#").shape[0] != 178), 0.0, "{:.0f}")
    compare_compress(d, "zc_ref", "zc_cmp", [s_["label"] for s_ in zsubs], label="-cenmode drop -zcensor")


def case_nfeat_scan(prog, cli, work, rng):
    global RUNS
    print("\n[-nfeatures scan]", flush=True)
    d = work / "scan"
    s_true, subs, common = make_cv_world(rng, RUNS)
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    base = [prog, "-nfeatures", "2,3,4", "-niter", 40, "-tol", 0, "-dataTableFile", d / "table.txt", "-mask", d / "mask.nii.gz",
            "-cv", "run", "-match_window", 5, "-progress", "off", "-overwrite"]
    p = run(base + ["-prefix", d / "scan"])
    log = p.stdout + p.stderr
    check("the scan reports the best -nfeatures", float("best -nfeatures by held-out isc" not in log), 0.0, "{:.0f}")
    check("the scan writes no model", float((d / "scan.srm").exists() or (d / "scan.S.1D").exists()), 0.0, "{:.0f}")
    head, body = read_scores(d / "scan.nfeat.txt")
    check("nfeat.txt has a row per value", float(len(body) != 3), 0.0, "{:.0f}")
    run([prog, "-prefix", d / "k3", "-nfeatures", 3, "-niter", 40, "-tol", 0, "-dataTableFile", d / "table.txt", "-mask", d / "mask.nii.gz",
         "-cv", "run", "-match_window", 5, "-progress", "off", "-overwrite"])
    _, single = read_scores(d / "k3.scores.txt")
    row3 = [r for r in body if r[0] == "3"][0]
    check("the K=3 row equals the mean over subjects of a single-K run (isc)",
          abs(num(row3[1]) - float(np.mean([num(r[5]) for r in single]))), 1e-6)
    check("... and match", abs(num(row3[3]) - float(np.mean([num(r[6]) for r in single]))), 1e-6)



# --------------------------------------------------------------------- model_in, update_S, apply_W

def table_for(path, rows, subjects, with_mask=False):
    keep = [r for r in rows if r[0] in subjects]
    write_table(path, keep, with_mask)


def case_model_in(prog, cli, work, rng, algo):
    name = f"model_in_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    runs = [60, 50, 70]
    s_true, subs, common = make_cv_world(rng, runs, nsub=10)
    subs[7]["x"][3, :] = 5.0                                     # a feature that never varies, in a new subject
    old = globals()["RUNS"]
    globals()["RUNS"] = runs
    try:
        rows = write_world(d, subs)
        pre = [preprocess(s)[0] for s in subs]
    finally:
        globals()["RUNS"] = old
    names = [s["label"] for s in subs]
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    base_names, up1_names, up2_names = names[:6], names[6:8], names[8:]
    table_for(d / "base.txt", rows, base_names)
    table_for(d / "new1.txt", rows, up1_names)
    table_for(d / "new2.txt", rows, up2_names)
    common_args = ["-mask", d / "mask.nii.gz", "-progress", "off", "-overwrite"]
    K_, T = 3, sum(runs)

    run([prog, "-prefix", d / "base", "-nfeatures", K_, "-algorithm", algo, "-niter", 60, "-tol", 0, "-save_W",
         "-dataTableFile", d / "base.txt"] + common_args)
    hdr, subj, _, blocks = parse_srm_file(d / "base.srm")
    S = blocks["S"]
    base_bytes = (d / "base.srm").read_bytes()

    # -- A: a W for each new subject against the fixed S
    p = run([prog, "-prefix", d / "mi", "-model_in", d / "base.srm", "-dataTableFile", d / "new1.txt"] + common_args)
    log = p.stdout + p.stderr
    check("A: the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    check("A: no new model and no S are written", float((d / "mi.srm").exists() or (d / "mi.S.1D").exists()), 0.0, "{:.0f}")
    check("A: the model file is untouched", float((d / "base.srm").read_bytes() != base_bytes), 0.0, "{:.0f}")
    worst, worst_r2 = 0.0, 0.0
    rho2 = None
    if algo == "prob":
        rho2 = np.array([float(x) for x in Path(d / "mi.rho2.1D").read_text().splitlines()[1:]])
    for j, nm in enumerate(up1_names):
        z = pre[names.index(nm)]
        W = np_polar(z @ S.T)
        P = load_afni(d / f"mi.proj.{nm}").reshape(K_, T)
        worst = max(worst, float(np.abs(P - W.T @ z).max()))
        if rho2 is not None:
            r2 = ((z - W @ S) ** 2).mean()
            worst_r2 = max(worst_r2, abs(rho2[j] - r2) / r2)
    check("A: new subjects' data in the shared space vs numpy (W = polar(X S'))", worst, 2e-4)
    if rho2 is not None:
        check("A: their noise variances vs numpy", worst_r2, 1e-5)

    # -- B: -cv run scores a new subject on runs neither its W nor S saw
    run([prog, "-prefix", d / "cvmi", "-model_in", d / "base.srm", "-cv", "run", "-match_window", 5, "-save_r2",
         "-dataTableFile", d / "new1.txt"] + common_args)
    head, body = read_scores(d / "cvmi.scores_folds.txt")
    got = {(r[0], r[2]): [num(r[4]), num(r[5]), num(r[6]), num(r[7])] for r in body}
    fold_names = sorted({r[2] for r in body})
    folds, c0 = [], 0
    for L in runs:
        folds.append((c0, L))
        c0 += L
    errs = []
    for nm in up1_names:
        z = pre[names.index(nm)]
        for fi_, (c0, L) in enumerate(folds):
            tr = np.delete(z, np.arange(c0, c0 + L), axis=1)
            te = z[:, c0:c0 + L]
            s_tr = np.delete(S, np.arange(c0, c0 + L), axis=1)
            s_te = S[:, c0:c0 + L]
            W = np_polar(tr @ s_tr.T)
            Pm = W.T @ te
            acc, ch = np_segment_match(Pm, s_te, 5)
            sse = ((te - W @ s_te) ** 2).sum(1)
            sst = ((te - te.mean(1, keepdims=True)) ** 2).sum(1)
            wnt = [np_isc(Pm, s_te), acc, ch, 1.0 - sse.sum() / sst.sum()]
            errs.append([abs(got[(nm, fold_names[fi_])][q] - wnt[q]) for q in range(4)])
    errs = np.array(errs)
    check("B: held-out isc vs numpy", float(errs[:, 0].max()), 5e-4)
    check("B: segment matching vs numpy", float(errs[:, 1].max()), 1e-6)
    check("B: ... and its chance level", float(errs[:, 2].max()), 1e-6)
    check("B: held-out R^2 vs numpy", float(errs[:, 3].max()), 5e-4)

    # -- C: -update_S against the library driver, and the chain of two updates
    def ref_update(model_file, tag, upd_names, niter):
        rows_ = [pre[names.index(n)] for n in upd_names]
        files = []
        for n, z in zip(upd_names, rows_):
            f = d / f"{tag}_{n}.f32"
            np.ascontiguousarray(z, dtype=np.float32).tofile(f)
            files.append(f)
        out = d / f"{tag}_ref"
        run_load_update(cli, model_file, out, files, [z.shape[0] for z in rows_], niter)
        S_ref = np.fromfile(f"{out}.u_S.f32", dtype=np.float32).reshape(K_, T).astype(np.float64)
        a_ref = np.fromfile(f"{out}.u_asum.f64").reshape(K_, T)
        info = open(f"{out}.u_info.txt").read().split()
        return S_ref, a_ref, float(info[2]), int(info[3]), int(info[4])

    p = run([prog, "-prefix", d / "up1", "-model_in", d / "base.srm", "-update_S", "-niter", 80, "-tol", 0,
             "-cv", "run", "-match_window", 5, "-dataTableFile", d / "new1.txt"] + common_args)
    log = p.stdout + p.stderr
    S_ref, a_ref, drift_ref, nupd_ref, neff_ref = ref_update(d / "base.srm", "u1", up1_names, 80)
    S_got = np.loadtxt(d / "up1.S.1D", comments="#").T
    check("C: updated S vs the library driver (relative)", float(np.abs(S_got - S_ref).max() / np.abs(S_ref).max()), 2e-4)
    h1, s1, _, b1 = parse_srm_file(d / "up1.srm")
    check("C: updated model: S in the file vs the library driver (relative)",
          float(np.abs(b1["S"] - S_ref).max() / np.abs(S_ref).max()), 2e-4)
    check("C: updated model: a_sum vs the library driver (relative)",
          float(np.abs(b1["a_sum"] - a_ref).max() / np.abs(a_ref).max()), 2e-4)
    check("C: updated model: subjects and update count in the header",
          float(not (len(s1) == 8 and int(h1["nsubj"][0]) == 8 and int(h1["nupdate"][0]) == nupd_ref
                     and int(h1["nsubj_eff"][0]) == neff_ref and [x[0] for x in s1] == names[:8])), 0.0, "{:.0f}")
    check("C: updated model: drift vs the library driver", abs(float(h1["drift"][0]) - drift_ref) / max(drift_ref, 1e-12), 2e-2)
    # scores are computed from the model as it was, before the update
    cvmi = (d / "cvmi.scores.txt").read_text()
    check("C: -cv scores with -update_S equal those without (computed before the update)",
          float((d / "up1.scores.txt").read_text() != cvmi), 0.0, "{:.0f}")

    run([prog, "-prefix", d / "up2", "-model_in", d / "up1.srm", "-update_S", "-niter", 80, "-tol", 0,
         "-dataTableFile", d / "new2.txt"] + common_args)
    S_ref2, a_ref2, drift_ref2, nupd_ref2, neff_ref2 = ref_update(d / "up1.srm", "u2", up2_names, 80)
    h2, s2, _, b2 = parse_srm_file(d / "up2.srm")
    check("C: second update: S vs the library driver (relative)", float(np.abs(b2["S"] - S_ref2).max() / np.abs(S_ref2).max()), 2e-4)
    check("C: second update: 10 subjects, 2 updates",
          float(not (len(s2) == 10 and int(h2["nupdate"][0]) == 2 and nupd_ref2 == 2)), 0.0, "{:.0f}")

    # -- errors and protection
    def fails(args, needle, label):
        pr = run([prog, "-progress", "off"] + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    fails(["-prefix", d / "e1", "-model_in", d / "base.srm", "-update_S", "-dataTableFile", d / "base.txt", "-mask", d / "mask.nii.gz"],
          "already one of the", "E: -update_S refuses a subject that is already in the model")
    fails(["-prefix", d / "e2", "-model_in", d / "base.srm", "-nfeatures", 3, "-dataTableFile", d / "new1.txt", "-mask", d / "mask.nii.gz"],
          "-nfeatures is defined by the model", "E: -nfeatures with -model_in is refused")
    fails(["-prefix", d / "e3", "-model_in", d / "base.srm", "-cv", "loso", "-dataTableFile", d / "new1.txt", "-mask", d / "mask.nii.gz"],
          "-cv loso", "E: -cv loso with -model_in is refused")
    fails(["-prefix", d / "e4", "-update_S", "-nfeatures", 3, "-dataTableFile", d / "new1.txt", "-mask", d / "mask.nii.gz"],
          "-update_S needs -model_in", "E: -update_S alone is refused")
    fails(["-prefix", d / "e5", "-model_in", d / "base.srm", "-niter", 5, "-dataTableFile", d / "new1.txt", "-mask", d / "mask.nii.gz"],
          "apply only to -update_S", "E: -niter without -update_S is refused")
    short = [r for r in rows if r[0] in up1_names and r[2] != "r3"]
    write_table(d / "short.txt", short, False)
    fails(["-prefix", d / "e6", "-model_in", d / "base.srm", "-dataTableFile", d / "short.txt", "-mask", d / "mask.nii.gz"],
          "do not fit the model", "E: new data with a different run structure are refused with one clear message")
    no_ow = ["-prefix", d / "base", "-model_in", d / "base.srm", "-update_S", "-niter", 20,
             "-dataTableFile", d / "new1.txt", "-mask", d / "mask.nii.gz"]
    fails(no_ow, "already exists", "E: -update_S does not overwrite the model it started from without -overwrite")
    check("E: ... and the model file is still intact", float((d / "base.srm").read_bytes() != base_bytes), 0.0, "{:.0f}")
    run([prog, "-progress", "off"] + no_ow + ["-overwrite"])
    h3, s3, _, _ = parse_srm_file(d / "base.srm")
    check("E: with -overwrite the same name is updated in place", float(len(s3) != 8), 0.0, "{:.0f}")


def case_apply_W(prog, cli, work, rng, zs):
    name = f"apply_W_{zs}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    runs = [60, 50, 70]
    s_true, subs, common = make_cv_world(rng, runs, nsub=5)
    old = globals()["RUNS"]
    globals()["RUNS"] = runs
    try:
        rows = write_world(d, subs)
    finally:
        globals()["RUNS"] = old
    names = [s["label"] for s in subs]
    write_table(d / "base.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    K_ = 3
    run([prog, "-prefix", d / "base", "-nfeatures", K_, "-niter", 40, "-tol", 0, "-save_W", "-zscore", zs, "-mask", d / "mask.nii.gz",
         "-dataTableFile", d / "base.txt", "-progress", "off", "-overwrite"])
    # other data for the same people: different lengths, even between subjects
    task_runs = {nm: ([40, 55] if i % 2 == 0 else [45, 50]) for i, nm in enumerate(names)}
    task, trows = {}, []
    for i, (s, nm) in enumerate(zip(subs, names)):
        v = len(s["idx"])
        x = rng.standard_normal((v, sum(task_runs[nm]))) * 3.0 + 100.0
        task[nm] = x
        c0 = 0
        mask_file = d / "mask.nii.gz"
        for r, L in enumerate(task_runs[nm]):
            vol = np.zeros((NX, NY, NZ, L), dtype=np.float32)
            flat = vol.reshape(-1, L, order="F")
            flat[s["idx"], :] = x[:, c0:c0 + L]
            write_nifti(d / f"{nm}_task{r + 1}.nii.gz", flat.reshape((NX, NY, NZ, L), order="F"))
            trows.append((nm, s["group"], f"r{r + 1}", mask_file, d / f"{nm}_task{r + 1}.nii.gz"))
            c0 += L
    write_table(d / "task.txt", trows, False)
    p = run([prog, "-prefix", d / "task", "-apply_W", d / "base", "-dataTableFile", d / "task.txt", "-progress", "off", "-overwrite"])
    log = p.stdout + p.stderr
    check("the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    worst = 0.0
    for s, nm in zip(subs, names):
        Wvol = load_afni(d / f"base.W.{nm}")                      # NX x NY x NZ x K
        Wm = Wvol.reshape(-1, K_, order="F")[s["idx"]] if Wvol.ndim == 4 else None
        Wm = np.stack([Wvol[..., c].reshape(-1, order="F")[s["idx"]] for c in range(K_)], axis=1)
        x = task[nm].astype(np.float32).astype(np.float64)
        if zs == "run":
            z, c0 = np.zeros_like(x), 0
            for L in task_runs[nm]:
                seg = x[:, c0:c0 + L]
                z[:, c0:c0 + L] = (seg - seg.mean(1, keepdims=True)) / seg.std(1, keepdims=True)
                c0 += L
        else:
            z = x - x.mean(1, keepdims=True)
        Tn = x.shape[1]
        P = load_afni(d / f"task.proj.{nm}").reshape(K_, Tn)
        worst = max(worst, float(np.abs(P - Wm.T @ z).max()))
    check("projected new data vs numpy (own W, z-scored as the model's were)", worst, 2e-4)
    pr = run([prog, "-prefix", d / "bad", "-apply_W", d / "base", "-dataTableFile", d / "task.txt", "-mask", d / "mask.nii.gz",
              "-progress", "off", "-overwrite"], expect_ok=False)
    check("-mask with -apply_W is refused", float(pr.returncode == 0), 0.0, "{:.0f}")
    gone = [r for r in trows if r[0] != names[0]]
    gone = [("stranger" if r[0] == names[1] else r[0],) + tuple(r[1:]) for r in gone]
    write_table(d / "stranger.txt", gone, False)
    pr = run([prog, "-prefix", d / "bad2", "-apply_W", d / "base", "-dataTableFile", d / "stranger.txt", "-progress", "off", "-overwrite"], expect_ok=False)
    check("a subject missing from the model is refused", float(pr.returncode == 0 or "not in the model" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")
    pr = run([prog, "-prefix", d / "task", "-apply_W", d / "base", "-dataTableFile", d / "task.txt", "-progress", "off"], expect_ok=False)
    check("existing outputs are not overwritten", float(pr.returncode == 0), 0.0, "{:.0f}")


def case_1D_apply_W(prog, cli, work, rng):
    print("\n[1D subjects: dropped features and -apply_W]", flush=True)
    d = work / "apply_W_1D"
    d.mkdir(parents=True, exist_ok=True)
    T1, K_ = 90, 3
    s = rng.standard_normal((K_, T1))
    rows = []
    cols = {}
    for i in range(4):
        v = 12 + 3 * i
        x = (np.linalg.qr(rng.standard_normal((v, K_)))[0] @ s + 0.5 * rng.standard_normal((v, T1))) * 2.0 + 10.0
        if i == 1:
            x[4, :] = 3.0                                        # a column that never varies
        cols[f"s{i + 1}"] = x
        np.savetxt(d / f"s{i + 1}.1D", x.T, fmt="%.9g")
        rows.append(f"s{i + 1} g {d}/s{i + 1}.1D")
    (d / "base.txt").write_text("Subj Group InputFile\n" + "\n".join(rows) + "\n")
    run([prog, "-prefix", d / "base", "-nfeatures", K_, "-niter", 30, "-tol", 0, "-save_W", "-proj_format", "1D",
         "-dataTableFile", d / "base.txt", "-progress", "off", "-overwrite"])
    W2 = np.loadtxt(d / "base.W.s2.1D", comments="#")
    check("W of a 1D subject has a row for every column of the input", float(W2.shape[0] != cols["s2"].shape[0]), 0.0, "{:.0f}")
    check("... and the dropped column's row is zero", float(np.abs(W2[4]).max()), 0.0)
    new = {k_: rng.standard_normal((v.shape[0], 70)) + 5.0 for k_, v in cols.items()}
    rows = []
    for k_, x in new.items():
        np.savetxt(d / f"{k_}_new.1D", x.T, fmt="%.9g")
        rows.append(f"{k_} g {d}/{k_}_new.1D")
    (d / "new.txt").write_text("Subj Group InputFile\n" + "\n".join(rows) + "\n")
    run([prog, "-prefix", d / "new", "-apply_W", d / "base", "-proj_format", "1D", "-dataTableFile", d / "new.txt",
         "-progress", "off", "-overwrite"])
    worst = 0.0
    for k_, x in new.items():
        W = np.loadtxt(d / f"base.W.{k_}.1D", comments="#")
        xs = x.astype(np.float32).astype(np.float64)
        z = (xs - xs.mean(1, keepdims=True)) / xs.std(1, keepdims=True)
        P = np.loadtxt(d / f"new.proj.{k_}.1D", comments="#").T
        worst = max(worst, float(np.abs(P - W.T @ z).max()))
    check("1D: projected new data vs numpy", worst, 2e-4)


# --------------------------------------------------------------------- -axis item

ITEM_RUNS = ["r1", "r2", "r3"]
ITEM_CATS = ["face", "house", "tool"]


def afni_labels(prefix):
    """Volume labels of an AFNI +orig dataset, read from the .HEAD text (BRICK_LABS)."""
    head = Path(str(prefix) + "+orig.HEAD").read_text()
    i = head.index("name = BRICK_LABS")
    m = re.search(r"count = (\d+)\s*\n'", head[i:])
    text = head[i + m.end():i + m.end() + int(m.group(1)) - 1]
    return text.split("~")[:-1] if text.endswith("~") else text.split("~")


def expected_keys(table_rows, repeat_col=False):
    """The column keys and their order, from the help text: subjects and runs in order of first appearance;
    each subject's rows in run order, then table order; an item a subject has more than once is keyed
    Item:1..n in that order (Item:R with a Repeat column); the first subject's order is the model's."""
    runs = []
    for r in table_rows:
        if r["run"] is not None and r["run"] not in runs:
            runs.append(r["run"])
    subs = []
    for r in table_rows:
        if r["subj"] not in subs:
            subs.append(r["subj"])
    keyed = {}
    for s in subs:
        mine = [(runs.index(r["run"]) if r["run"] is not None else 0, pos, r) for pos, r in enumerate(table_rows) if r["subj"] == s]
        mine.sort(key=lambda t: (t[0], t[1]))
        total = {}
        for _, _, r in mine:
            total[r["item"]] = total.get(r["item"], 0) + 1
        seen, out = {}, []
        for _, _, r in mine:
            seen[r["item"]] = seen.get(r["item"], 0) + 1
            if total[r["item"]] > 1:
                out.append((f"{r['item']}:{r['rep'] if repeat_col else seen[r['item']]}", r))
            else:
                out.append((r["item"], r))
        keyed[s] = out
    return subs, [k for k, _ in keyed[subs[0]]], keyed


def make_item_world(rng, nitem=24, nrep=6, nsub=6, n1d=2, use_runs=True, k=K, shared_runs=False, same_run=False,
                    run_offsets=True):
    """Items with a latent response each; a repeat shares its item's response. Each subject has its own random
    run assignment (or, with shared_runs, everyone has the same one), and, with run_offsets, a per-voxel offset
    in every run (betas of per-run GLMs; without them, betas of one GLM).
    With same_run, the repeats of it01 fall in the same run. Repeats carry labels 'a', 'b' in reverse run order."""
    s_item = rng.standard_normal((k, nitem)) * np.linspace(1.6, 0.7, k)[:, None]
    names = [f"it{j + 1:02d}" for j in range(nitem)]

    def assignment():
        occ = []
        for j in range(nitem):
            if not use_runs:
                occ.append((j, None, None))
                continue
            rr = rng.permutation(len(ITEM_RUNS))
            if j < nrep:
                r1, r2 = (int(rr[0]), int(rr[0])) if (same_run and j == 0) else (int(rr[0]), int(rr[1]))
                first, second = sorted([r1, r2])
                occ += [(j, first, "b"), (j, second, "a")]
            else:
                occ.append((j, int(rr[0]), None))
        return occ
    common = assignment() if shared_runs else None
    subs = []
    for i in range(nsub):
        occ = list(common) if shared_runs else assignment()
        if i >= nsub - n1d:
            v = 12 + 3 * i
            mask, idx = None, None
        else:
            mask = np.zeros((NX, NY, NZ), dtype=bool)
            if i % 2 == 0:
                mask[1:9, 1:9, 1:5] = True               # 256 voxels
            else:
                mask[2:8, 1:9, 1:5] = True               # 192: masks differ between subjects
            idx = np.flatnonzero(mask.ravel(order="F"))
            v = len(idx)
        q, _ = np.linalg.qr(rng.standard_normal((v, k)))
        scale = rng.uniform(0.5, 3.0, (v, 1))
        offs = {rr: rng.uniform(-40, 40, (v, 1)) for rr in range(len(ITEM_RUNS))}
        cols = []
        for j, rr, _ in occ:
            x = q @ s_item[:, j:j + 1] + 0.5 * rng.standard_normal((v, 1))
            cols.append(x * scale + 200 + (offs[rr] if (rr is not None and run_offsets) else 0.0))
        subs.append({"label": f"s{i + 1:02d}", "group": "ctl" if i % 2 == 0 else "pat", "mask": mask, "idx": idx,
                     "occ": occ, "vals": cols, "is1D": i >= nsub - n1d})
    if use_runs:                                        # flat in one run for one subject: dropped under -zscore run
        s0 = subs[0]
        for c, (j, rr, _) in enumerate(s0["occ"]):
            if rr == 1:
                s0["vals"][c][4] = 9.0
    return names, subs


def write_item_world(d, names, subs, rng, layout_cycle=("per_run", "one_file", "per_item")):
    """Write the inputs (datasets read through [n] selectors in three layouts, or one-row .1D files) and
    return the table rows, shuffled."""
    d.mkdir(parents=True, exist_ok=True)
    rows = []
    for i, s in enumerate(subs):
        lab = s["label"]
        occ_rows = []
        if s["is1D"]:
            for c in range(len(s["occ"])):
                f = d / f"{lab}_c{c:03d}.1D"
                np.savetxt(f, s["vals"][c].T, fmt="%.8g")                 # one row: the item's features
                occ_rows.append((c, str(f)))
        else:
            mask_file = d / f"{lab}_mask.nii.gz"
            write_nifti(mask_file, s["mask"].astype(np.float32)[..., None])
            layout = layout_cycle[i % len(layout_cycle)]
            groups = {}
            for c, (j, rr, _) in enumerate(s["occ"]):
                key = "all" if layout == "one_file" else (f"c{c:03d}" if layout == "per_item" else f"run{rr}")
                groups.setdefault(key, []).append(c)
            for key, cs in groups.items():
                order = list(rng.permutation(cs))                         # volumes in a scrambled order
                vol = np.zeros((NX, NY, NZ, len(order)), dtype=np.float32)
                flat = vol.reshape(-1, len(order), order="F")
                for b, c in enumerate(order):
                    flat[s["idx"], b] = s["vals"][c][:, 0]
                vol = flat.reshape((NX, NY, NZ, len(order)), order="F")
                f = d / f"{lab}_{key}.nii.gz"
                write_nifti(f, vol)
                for b, c in enumerate(order):
                    occ_rows.append((c, f"{f}[{b}]" if len(order) > 1 else str(f)))
        for c, path in occ_rows:
            j, rr, rep = s["occ"][c]
            rows.append({"subj": lab, "group": s["group"], "item": names[j], "c": c, "rep": rep or "-",
                         "run": ITEM_RUNS[rr] if rr is not None else None, "cat": ITEM_CATS[j % len(ITEM_CATS)],
                         "mask": "-" if s["is1D"] else str(d / f"{lab}_mask.nii.gz"), "file": path})
    order = rng.permutation(len(rows))
    return [rows[p] for p in order]


def write_item_table(path, rows, with_run=True, with_repeat=False, with_cat=False):
    head = "Subj Group Item " + ("Run " if with_run else "") + ("Repeat " if with_repeat else "") + \
           ("Category " if with_cat else "") + "Mask InputFile"
    lines = [head]
    for r in rows:
        lines.append(f"{r['subj']} {r['group']} {r['item']} " + (f"{r['run']} " if with_run else "") +
                     (f"{r['rep']} " if with_repeat else "") + (f"{r['cat']} " if with_cat else "") + f"{r['mask']} {r['file']}")
    Path(path).write_text("\n".join(lines) + "\n")


def item_preprocess(sub, keyed_rows, keys, zmode="all"):
    """The subject's features x keys matrix: z-scored over all items ('all'), over the items of each of the
    subject's own runs ('run'), or as given ('none'); features that never vary (in a run, for 'run') dropped."""
    x = np.zeros((sub["vals"][0].shape[0], len(keys)))
    run_of = np.zeros(len(keys), dtype=int)
    for key, r in keyed_rows:
        j = keys.index(key)
        x[:, j] = sub["vals"][r["c"]][:, 0].astype(np.float32)
        run_of[j] = ITEM_RUNS.index(r["run"]) if (zmode == "run" and r["run"] is not None) else 0
    z = np.zeros_like(x)
    bad = np.zeros(x.shape[0], dtype=bool)
    for rr in np.unique(run_of):
        cols = run_of == rr
        seg = x[:, cols]
        sd = seg.std(axis=1)
        bad |= ~(sd > 1e-7 * np.abs(seg.mean(axis=1)))
        z[:, cols] = seg if zmode == "none" else (seg - seg.mean(axis=1, keepdims=True)) / np.where(sd > 0, sd, 1.0)[:, None]
    return z[~bad], ~bad


def s1d_keys(path):
    for ln in Path(path).read_text().splitlines():
        if ln.startswith("# items:"):
            return ln.split()[2:]
    return None


def item_fails(prog, d, args, needle, label, table):
    pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-dataTableFile", table] + args,
             expect_ok=False)
    out = pr.stdout + pr.stderr
    check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
    if pr.returncode == 0 or needle not in out:
        print(out)


def case_items(prog, cli, work, rng, algo, use_runs=True, zmode="all"):
    name = f"items_{algo}_{'runs' if use_runs else 'noruns'}_z{zmode}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    names, subs = make_item_world(rng, use_runs=use_runs, same_run=use_runs)
    rows = write_item_world(d, names, subs, rng)
    write_item_table(d / "table.txt", rows, with_run=use_runs)
    niter = 60
    zopt = [] if zmode == "all" else ["-zscore", zmode]
    p = run([prog, "-prefix", d / "out", "-axis", "item", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0,
             "-dataTableFile", d / "table.txt", "-progress", "line", "-overwrite"] + zopt)
    log = p.stdout + p.stderr
    check("the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    check("the reading progress counts items", float("reading:" not in log or "items" not in log), 0.0, "{:.0f}")

    sub_order, keys, keyed = expected_keys(rows)
    by_label = {s["label"]: s for s in subs}
    pre = [item_preprocess(by_label[lab], keyed[lab], keys, zmode) for lab in sub_order]
    ref = run_ours(cli, work, name + "_ref", algo, [z.astype(np.float32) for z, _ in pre], K, niter, zmode == "none",
                   extra=["--canon"])

    S = np.loadtxt(d / "out.S.1D").T
    check("item keys in S.1D, in the order the help describes (repeats in one run by row)",
          float(s1d_keys(d / "out.S.1D") != keys), 0.0, "{:.0f}")
    check(f"S.1D (one row per item, -zscore {zmode}) vs the library driver", relfro(S, ref["s"]), 2e-4)
    rows_fit = [ln.split("\t") for ln in (d / "out.fit.txt").read_text().splitlines() if ln and not ln.startswith(("#", "Subj"))]
    check("subjects in order of first appearance", float([r[0] for r in rows_fit] != sub_order), 0.0, "{:.0f}")
    check("features used per subject (constant features dropped)",
          float(sum(abs(int(r[3]) - ok.sum()) for r, (_, ok) in zip(rows_fit, pre))), 0.0, "{:.0f}")
    worst, labs_ok = 0.0, True
    for i, lab in enumerate(sub_order):
        z, _ = pre[i]
        if by_label[lab]["is1D"]:
            continue
        proj = load_afni(d / f"out.proj.{lab}").reshape(K, len(keys))
        mu = z.mean(1, keepdims=True) if zmode == "none" else 0.0
        worst = max(worst, relfro(proj, ref["w"][i].T @ (z - mu)))
        labs_ok &= afni_labels(d / f"out.proj.{lab}") == keys
    check("projected items vs W'X from the driver, worst subject", worst, 5e-3)
    check("projected datasets are labelled with the item keys", float(not labs_ok), 0.0, "{:.0f}")
    hdr, subj, items, blocks = parse_srm_file(d / "out.srm")
    check("model file: axis item, -zscore, and the keys in column order",
          float(hdr.get("axis", [""])[0] != "item" or hdr["zscore"][0] != zmode or items != keys or int(hdr["nitem"][0]) != len(keys)),
          0.0, "{:.0f}")
    if not use_runs or zmode != "all":
        return

    # the row order of the table matters only through the order in which subjects first appear (which
    # sets each one's random start) and runs first appear (which orders repeats): keep the shortest
    # prefix that fixes both, and permute the rest so that no two rows of the same item and subject
    # trade places (that would renumber repeats in one run)
    seen_s, seen_r, m = set(), set(), 0
    while len(seen_s) < len(sub_order) or len(seen_r) < len(ITEM_RUNS):
        seen_s.add(rows[m]["subj"]); seen_r.add(rows[m]["run"]); m += 1
    rest = [rows[m + p] for p in rng.permutation(len(rows) - m)]
    same_run_pairs = [r for r in rows if r["item"] == "it01"]
    rest = [r for r in rest if r["item"] != "it01"]
    rows2 = rows[:m] + [r for r in same_run_pairs if not any(r is q for q in rows[:m])] + rest
    write_item_table(d / "table2.txt", rows2)
    run([prog, "-prefix", d / "out2", "-axis", "item", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0,
         "-dataTableFile", d / "table2.txt", "-progress", "off", "-overwrite"])
    keys2 = s1d_keys(d / "out2.S.1D")
    S2 = np.loadtxt(d / "out2.S.1D").T
    perm = [keys2.index(k_) for k_ in keys]
    check("the permuted table gives the columns in another order", float(keys2 == keys), 0.0, "{:.0f}")
    check("table row order does not matter (S matched by item key)", relfro(S2[:, perm], S), 2e-5)

    # a Repeat column names repeats: here 'b' is the earlier run's, so the keys follow the labels, not the runs
    write_item_table(d / "table_rep.txt", rows, with_repeat=True)
    run([prog, "-prefix", d / "rep", "-axis", "item", "-nfeatures", K, "-niter", 5, "-dataTableFile", d / "table_rep.txt",
         "-progress", "off", "-overwrite"])
    _, keys_rep, _ = expected_keys(rows, repeat_col=True)
    check("a Repeat column keys repeats as Item:R", float(s1d_keys(d / "rep.S.1D") != keys_rep or "it02:a" not in keys_rep), 0.0, "{:.0f}")

    # repeats without a Run column are allowed, numbered by table order
    write_item_table(d / "table_norun.txt", rows, with_run=False)
    run([prog, "-prefix", d / "norun", "-axis", "item", "-nfeatures", K, "-niter", 5, "-dataTableFile", d / "table_norun.txt",
         "-progress", "off", "-overwrite"])
    _, keys_nr, _ = expected_keys([dict(r, run=None) for r in rows])
    check("repeats without a Run column: numbered by table order", float(s1d_keys(d / "norun.S.1D") != keys_nr), 0.0, "{:.0f}")

    # refusals, each naming what to do
    def table_with(rows_, fname, **kw):
        write_item_table(d / fname, rows_, **kw)
        return d / fname
    T0 = d / "table.txt"
    ax = ["-axis", "item"]
    for opt, needle in ((["-censor", T0], "-censor drops time points"), (["-zcensor"], "-zcensor is for time"),
                        (["-cv_blocks", 3], "items have no order"), (["-match_window", 5], "identified on its own"),
                        (["-time_labels", "x", T0], "table column"), (["-repeated_stimulus"], "recur across runs"),
                        (["-run_coupling", 10], "-axis time only")):
        item_fails(prog, d, ax + opt, needle, f"E: {opt[0]} with -axis item is refused, saying why", T0)
    item_fails(prog, d, ["-item_column", "Item"], "need -axis item", "E: -item_column without -axis item", T0)
    item_fails(prog, d, ["-fold_column", "Run", "-cv", "run"], "-fold_column is for -axis item", "E: -fold_column with -axis time", T0)
    item_fails(prog, d, ax + ["-fold_column", "Run"], "needs -cv", "E: -fold_column without -cv", T0)
    pr = run([prog, "-progress", "off", "-prefix", d / "cvrun", "-overwrite", "-nfeatures", K, "-niter", 5, "-dataTableFile", T0]
             + ax + ["-cv", "run"])
    check("randomized runs: -cv run holds out each subject's own runs (missing for that subject in the refit)",
          float("missing for that subject only" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")
    item_fails(prog, d, ax + ["-zscore", "run"], "needs at least 3", "E: -zscore run with a run of fewer than 3 items",
               table_with([dict(r, run=("r9" if (r["subj"] == sub_order[1] and r["item"] in ("it23", "it24")) else r["run"])) for r in rows], "tiny.txt"))
    item_fails(prog, d, ax + ["-zscore", "run"], "no 'Run' column", "E: -zscore run without a Run column", table_with(rows, "norun2.txt", with_run=False))
    s2 = sub_order[1]
    miss = [r for r in rows if not (r["subj"] == s2 and r["item"] == "it20")]
    item_fails(prog, d, ax, f"Subj {s2} is missing Item it20", "E: a missing item names the Subj and Item", table_with(miss, "miss.txt"))
    extra = rows + [dict(next(r for r in rows if r["subj"] == s2 and r["item"] == "it20"), item="it99")]
    item_fails(prog, d, ax, f"Subj {s2} has Item it99, which Subj", "E: an extra item names the Subj and Item", table_with(extra, "extra.txt"))
    one = next(r for r in rows if r["subj"] == s2 and r["item"] == "it02")
    fewer = [r for r in rows if r is not one]
    item_fails(prog, d, ax, f"Subj {s2} has Item it02 1 time", "E: a repeat missing for one subject gives both counts", table_with(fewer, "fewer.txt"))
    duprep = [dict(r, rep="a") if (r["subj"] == s2 and r["item"] == "it02") else r for r in rows]
    item_fails(prog, d, ax, "with Repeat a twice", "E: a Repeat label used twice for one item", table_with(duprep, "duprep.txt", with_repeat=True))
    colon = [dict(r, item="a:b") if r is rows[0] else r for r in rows]
    item_fails(prog, d, ax, "may not contain ':'", "E: an item label with ':' is refused", table_with(colon, "colon.txt"))
    ds_row = next(r for r in rows if r["file"].endswith("]"))
    multi = [dict(r, file=r["file"].rsplit("[", 1)[0] + "[0,1]") if r is ds_row else r for r in rows]
    item_fails(prog, d, ax, "selects 2 volumes", "E: a selector picking two volumes is refused", table_with(multi, "multi.txt"))
    zero_sub = next(s for s in subs if not s["is1D"])
    zf = d / "zero_item.nii.gz"
    write_nifti(zf, np.zeros((NX, NY, NZ, 1), dtype=np.float32))
    zrow = next(r for r in rows if r["subj"] == zero_sub["label"])
    zero = [dict(r, file=str(zf)) if r is zrow else r for r in rows]
    item_fails(prog, d, ax, "is all zero in the mask", "E: an all-zero item names the Subj and item", table_with(zero, "zero.txt"))
    one_d = next(s for s in subs if s["is1D"])
    two = d / "two_rows.1D"
    np.savetxt(two, np.vstack([one_d["vals"][0].T, one_d["vals"][1].T]), fmt="%.8g")
    r1d = next(r for r in rows if r["subj"] == one_d["label"])
    tworow = [dict(r, file=str(two)) if r is r1d else r for r in rows]
    item_fails(prog, d, ax, "a .1D input is one row", "E: a .1D item with two rows is refused", table_with(tworow, "tworow.txt"))


def np_item_identify(a, b):
    """SRM_item_identify written from its contract: item j of a goes to the item of b it correlates with best
    across components (ties to the lower index); chance 1/n."""
    za = a - a.mean(0, keepdims=True); za /= np.where(np.linalg.norm(za, axis=0) > 0, np.linalg.norm(za, axis=0), 1)
    zb = b - b.mean(0, keepdims=True); zb /= np.where(np.linalg.norm(zb, axis=0) > 0, np.linalg.norm(zb, axis=0), 1)
    cm = za.T @ zb
    return float((np.argmax(cm, axis=1) == np.arange(a.shape[1])).mean()), 1.0 / a.shape[1]


def case_items_model_in(prog, cli, work, rng, algo, k=4, niter=60):
    """-model_in on the item axis: new subjects' items, in their own table order and their own randomized runs,
    put in the model's key order; W = polar(X S') against the fixed S; -cv run on each subject's own runs."""
    name = f"items_model_in_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    names, subs = make_item_world(rng, nitem=30, nrep=4, nsub=8, n1d=2, k=k, run_offsets=False)
    rows = write_item_world(d, names, subs, rng)
    base_l = [s["label"] for s in subs[:5]]
    new_l = [s["label"] for s in subs[5:]]                     # one dataset subject, two .1D subjects
    base_rows = [r for r in rows if r["subj"] in base_l]
    write_item_table(d / "base.txt", base_rows)
    new_rows = [r for r in rows if r["subj"] in new_l]
    new_l = list(dict.fromkeys(r["subj"] for r in new_rows))   # the program's order: first appearance
    base_l = list(dict.fromkeys(r["subj"] for r in base_rows))
    write_item_table(d / "new.txt", new_rows)
    common = ["-axis", "item", "-progress", "off", "-overwrite", "-proj_format", "1D"]
    run([prog, "-prefix", d / "base", "-nfeatures", k, "-algorithm", algo, "-niter", niter, "-tol", 0, "-save_W",
         "-dataTableFile", d / "base.txt"] + common)
    hdr, _, mkeys, blocks = parse_srm_file(d / "base.srm")
    S = blocks["S"]
    by_label = {s_["label"]: s_ for s_ in subs}
    _, _, keyed = expected_keys(new_rows)
    pre = {lab: item_preprocess(by_label[lab], keyed[lab], mkeys)[0] for lab in new_l}
    check("the new subjects' own order of items differs from the model's",
          float(all([k_ for k_, _ in keyed[lab]] == mkeys for lab in new_l)), 0.0, "{:.0f}")

    # -- A: a W for each new subject against the fixed S, the columns put in the model's order
    p = run([prog, "-prefix", d / "mi", "-model_in", d / "base.srm", "-dataTableFile", d / "new.txt"] + common)
    log = p.stdout + p.stderr
    check("A: the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    worst, keys_ok, worst_r2 = 0.0, True, 0.0
    rho2 = np.array([float(x) for x in Path(d / "mi.rho2.1D").read_text().splitlines()[1:]]) if algo == "prob" else None
    for j, lab in enumerate(new_l):
        z = pre[lab]
        W = np_polar(z @ S.T)
        P = np.loadtxt(d / f"mi.proj.{lab}.1D", comments="#").T
        worst = max(worst, relfro(P, W.T @ z))
        keys_ok &= s1d_keys(d / f"mi.proj.{lab}.1D") == mkeys
        if rho2 is not None:
            r2 = ((z - W @ S) ** 2).mean()
            worst_r2 = max(worst_r2, abs(rho2[j] - r2) / r2)
    check("A: new subjects' items in the shared space vs numpy (W = polar(X S'), model order)", worst, 1e-4)
    check("A: the projections list the model's item keys", float(not keys_ok), 0.0, "{:.0f}")
    if rho2 is not None:
        check("A: their noise variances vs numpy", worst_r2, 1e-5)

    # -- B: -cv run: each subject's own runs held out (randomized), S fixed; item identification
    run([prog, "-prefix", d / "cvmi", "-model_in", d / "base.srm", "-cv", "run", "-dataTableFile", d / "new.txt"] + common)
    head, body = read_scores(d / "cvmi.scores_folds.txt")
    check("B: the metric is 'ident'", float(head[5] != "ident"), 0.0, "{:.0f}")
    got = {(r[0], r[2]): [num(r[3]), num(r[4]), num(r[5]), num(r[6]), num(r[7])] for r in body}
    errs, own = [], []
    levels = list(dict.fromkeys(r["run"] for lab in new_l for _, r in keyed[lab]))
    for lab in new_l:
        run_of = {k_: r["run"] for k_, r in keyed[lab]}
        z = pre[lab]
        f_own = []
        for lv in levels:
            te = np.array([j for j, k_ in enumerate(mkeys) if run_of[k_] == lv])
            f_own.append(te)
            W = np_polar(np.delete(z, te, axis=1) @ np.delete(S, te, axis=1).T)
            Pm, s_te, x_te = W.T @ z[:, te], S[:, te], z[:, te]
            acc, ch = np_item_identify(Pm, s_te)
            sse = ((x_te - W @ s_te) ** 2).sum()
            sst = ((x_te - x_te.mean(1, keepdims=True)) ** 2).sum()
            wnt = [len(te), np_isc(Pm, s_te), acc, ch, 1.0 - sse / sst]
            errs.append([abs(got[(lab, lv)][q] - wnt[q]) for q in range(5)])
        own.append(f_own)
    errs = np.array(errs)
    check("B: the subjects' runs really differ (own folds)", float(all(np.array_equal(own[0][q], own[1][q]) for q in range(len(levels)))), 0.0, "{:.0f}")
    check("B: items held out per subject and fold", float(errs[:, 0].max()), 0.0, "{:.0f}")
    check("B: held-out isc vs numpy", float(errs[:, 1].max()), 5e-4)
    check("B: item identification vs numpy", float(errs[:, 2].max()), 1e-6)
    check("B: ... and its chance level", float(errs[:, 3].max()), 1e-6)
    check("B: held-out R^2 vs numpy", float(errs[:, 4].max()), 5e-4)

    # -- C: -update_S on the item axis: a new model with the same keys and more subjects
    run([prog, "-prefix", d / "up", "-model_in", d / "base.srm", "-update_S", "-niter", 30, "-tol", 0,
         "-dataTableFile", d / "new.txt"] + common)
    h2, s2, k2, b2 = parse_srm_file(d / "up.srm")
    files = []
    for lab in new_l:
        f = d / f"u_{lab}.f32"
        np.ascontiguousarray(pre[lab], dtype=np.float32).tofile(f)
        files.append(f)
    run_load_update(cli, d / "base.srm", d / "u_ref", files, [pre[lab].shape[0] for lab in new_l], 30)
    S_ref = np.fromfile(f"{d / 'u_ref'}.u_S.f32", dtype=np.float32).reshape(k, len(mkeys)).astype(np.float64)
    check("C: updated S (items in the model's order) vs the library driver (relative)",
          float(np.abs(b2["S"] - S_ref).max() / np.abs(S_ref).max()), 2e-4)
    check("C: the updated model keeps axis item and the keys, and has the new subjects",
          float(h2["axis"][0] != "item" or k2 != mkeys or [x[0] for x in s2] != base_l + new_l), 0.0, "{:.0f}")
    check("C: S.1D of the update lists the keys", float(s1d_keys(d / "up.S.1D") != mkeys), 0.0, "{:.0f}")

    # -- errors
    def fails(args, needle, label):
        pr = run([prog, "-progress", "off", "-overwrite"] + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    write_item_table(d / "short.txt", [r for r in new_rows if r["item"] != "it30"])
    fails(["-prefix", d / "e1", "-axis", "item", "-model_in", d / "base.srm", "-dataTableFile", d / "short.txt"],
          "first: it30", "E: items not the model's name the first missing key")
    fails(["-prefix", d / "e2", "-model_in", d / "base.srm", "-dataTableFile", d / "new.txt"],
          "give -axis item", "E: an item model without -axis item")
    fails(["-prefix", d / "e3", "-axis", "item", "-model_in", d / "base.srm", "-zscore", "run", "-dataTableFile", d / "new.txt"],
          "-zscore is defined by the model", "E: -zscore with -model_in")
    return d, subs, base_l


def case_items_apply_W(prog, cli, work, rng, k=3):
    """Align on a movie (time axis, -zscore run), then carry each subject over to item betas with no Run column
    (z-scored over all of their items instead, as the help says); subjects have different items."""
    name = "items_apply_W"
    print(f"\n[{name}]", flush=True)
    d = work / name
    names, subs = make_item_world(rng, nitem=20, nrep=0, nsub=4, n1d=1, use_runs=False, k=k)
    rows = write_item_world(d, names, subs, rng)
    # the movie: the same people (the same masks and .1D widths), two runs of a shared series
    T1, runs = 70, [35, 35]
    s_t = rng.standard_normal((k, T1))
    trows = []
    for s_ in subs:
        v = s_["vals"][0].shape[0]
        x = np.linalg.qr(rng.standard_normal((v, k)))[0] @ s_t + 0.6 * rng.standard_normal((v, T1)) + 50.0
        c0 = 0
        for r, L in enumerate(runs):
            if s_["is1D"]:
                f = d / f"{s_['label']}_movie{r + 1}.1D"
                np.savetxt(f, x[:, c0:c0 + L].T, fmt="%.8g")
                trows.append(f"{s_['label']} {s_['group']} r{r + 1} - {f}")
            else:
                vol = np.zeros((NX * NY * NZ, L), dtype=np.float32)
                vol[s_["idx"]] = x[:, c0:c0 + L]
                f = d / f"{s_['label']}_movie{r + 1}.nii.gz"
                write_nifti(f, vol.reshape((NX, NY, NZ, L), order="F"))
                trows.append(f"{s_['label']} {s_['group']} r{r + 1} {d / (s_['label'] + '_mask.nii.gz')} {f}")
            c0 += L
    (d / "movie.txt").write_text("Subj Group Run Mask InputFile\n" + "\n".join(trows) + "\n")
    run([prog, "-prefix", d / "movie", "-nfeatures", k, "-niter", 30, "-tol", 0, "-save_W", "-dataTableFile", d / "movie.txt",
         "-progress", "off", "-overwrite"])
    # the task: each subject has its own set of items (some dropped)
    keep_rows = []
    for i, s_ in enumerate(subs):
        drop = set(rng.choice(names, size=i + 1, replace=False))
        keep_rows += [r for r in rows if r["subj"] == s_["label"] and r["item"] not in drop]
    write_item_table(d / "task.txt", keep_rows, with_run=False)
    p = run([prog, "-prefix", d / "task", "-axis", "item", "-apply_W", d / "movie", "-proj_format", "1D",
             "-dataTableFile", d / "task.txt", "-progress", "off", "-overwrite"])
    log = p.stdout + p.stderr
    check("the program name appears on the banner line only", float(len([l for l in log.splitlines() if "3dSRM" in l]) - 1), 0.0, "{:.0f}")
    check("a model z-scored within runs, items without runs: said, and z-scored over all items",
          float("z-scored over all of their items" not in log), 0.0, "{:.0f}")
    worst, keys_ok, nkeys = 0.0, True, set()
    for s_ in subs:
        lab = s_["label"]
        mine = [r for r in keep_rows if r["subj"] == lab]
        keys = [r["item"] for r in mine]                   # no runs, no repeats: table order
        x = np.array([s_["vals"][r["c"]][:, 0] for r in mine]).T.astype(np.float32).astype(np.float64)
        sd = x.std(1, keepdims=True)
        z = np.where(sd > 0, (x - x.mean(1, keepdims=True)) / np.where(sd > 0, sd, 1.0), 0.0)
        if s_["is1D"]:
            W = np.loadtxt(d / f"movie.W.{lab}.1D", comments="#")
        else:
            Wv = load_afni(d / f"movie.W.{lab}")
            W = np.stack([Wv[..., c].reshape(-1, order="F")[s_["idx"]] for c in range(k)], axis=1)
        P = np.loadtxt(d / f"task.proj.{lab}.1D", comments="#").T
        worst = max(worst, relfro(P, W.T @ z))
        keys_ok &= s1d_keys(d / f"task.proj.{lab}.1D") == keys
        nkeys.add(len(keys))
    check("projected items vs numpy (own W from the movie, z-scored over the subject's items)", worst, 1e-4)
    check("each projection lists that subject's own items", float(not keys_ok), 0.0, "{:.0f}")
    check("subjects had different numbers of items", float(len(nkeys) < 2), 0.0, "{:.0f}")
    lab0 = subs[0]["label"]
    two = [r for r in keep_rows if r["subj"] != lab0] + [r for r in keep_rows if r["subj"] == lab0][:2]
    write_item_table(d / "two.txt", two, with_run=False)
    pr = run([prog, "-prefix", d / "bad", "-axis", "item", "-apply_W", d / "movie", "-dataTableFile", d / "two.txt",
              "-progress", "off", "-overwrite"], expect_ok=False)
    check("E: a subject with 2 items is refused, saying why",
          float(pr.returncode == 0 or "needs at least 3" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")


def case_items_cv(prog, cli, work, rng, mode, algo, folds_by, k=5, niter=50, score_by=False):
    """-cv on items: folds are the runs (shared by everyone), the levels of a category column, or ("own", with
    -cv loso) each subject's own runs when run order was randomized."""
    name = f"items_cv_{mode}_{algo}_{folds_by}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    names, subs = make_item_world(rng, nitem=36, nrep=6, nsub=6, n1d=1, k=k, shared_runs=(folds_by != "own"), run_offsets=False)
    rows = write_item_world(d, names, subs, rng)
    write_item_table(d / "table.txt", rows, with_cat=True)
    fold_opt = ["-fold_column", "Category"] if folds_by == "category" else []
    if score_by:
        fold_opt += ["-score_by", "Category", "-reliability", "within"]
    run([prog, "-prefix", d / "out", "-axis", "item", "-nfeatures", k, "-algorithm", algo, "-niter", niter, "-tol", 0,
         "-dataTableFile", d / "table.txt", "-cv", mode, "-save_r2", "-progress", "off", "-overwrite"] + fold_opt)
    sub_order, keys, keyed = expected_keys(rows)
    by_label = {s["label"]: s for s in subs}
    pre = [item_preprocess(by_label[lab], keyed[lab], keys)[0] for lab in sub_order]
    n, T = len(pre), len(keys)
    col_of = "cat" if folds_by == "category" else "run"
    lev_sub = {lab: [next(r for kk, r in keyed[lab] if kk == key)[col_of] for key in keys] for lab in sub_order}
    levels = list(dict.fromkeys(v for lab in sub_order for v in lev_sub[lab]))
    folds = [np.array([j for j in range(T) if lev_sub[sub_order[0]][j] == lv]) for lv in levels]
    own_folds = {lab: [np.array([j for j in range(T) if lev_sub[lab][j] == lv]) for lv in levels] for lab in sub_order}
    if folds_by == "own":
        check("randomized runs: subjects really have different folds",
              float(all(np.array_equal(own_folds[sub_order[0]][q], own_folds[sub_order[1]][q]) for q in range(len(levels)))), 0.0, "{:.0f}")
    head, body = read_scores(d / "out.scores_folds.txt")
    check("scores name the metric 'ident' on the item axis", float(head[5] != "ident"), 0.0, "{:.0f}")
    check("folds are the levels, in order of first appearance", float([r[2] for r in body[:len(levels)]] != levels), 0.0, "{:.0f}")
    got = {(r[0], r[2]): [num(r[4]), num(r[5]), num(r[6]), num(r[7])] for r in body}
    want, vox_ref = {}, [np.zeros(z.shape[0]) for z in pre]
    lev_want = {}
    if mode == "run" and folds_by == "run" and score_by:   # each fold's items of each category, scored on their own
        cat_of = {lab: [next(r for kk, r in keyed[lab] if kk == key)["cat"] for key in keys] for lab in sub_order}
        for fi_, cols in enumerate(folds):
            r = reference_fit(cli, work, f"{name}_f{fi_}", [np.delete(z, cols, axis=1) for z in pre], k, niter, algo)
            P = [r["w"][i].T @ pre[i][:, cols] for i in range(n)]
            for i, lab in enumerate(sub_order):
                oth = [j for j in range(n) if j != i]
                ref_u = sum(P[j] for j in oth) / len(oth)
                if algo == "prob":
                    prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(k)
                    ref_r = np.linalg.solve(prec, sum(P[j] / r["rho2"][j] for j in oth))
                else:
                    ref_r = ref_u
                for cat in ITEM_CATS:
                    sub = np.array([cat_of[lab][c] == cat for c in cols])
                    if sub.sum() < 3:
                        continue
                    acc, ch = np_item_identify(P[i][:, sub], ref_u[:, sub])
                    te, hv = pre[i][:, cols][:, sub], (r["w"][i] @ ref_r)[:, sub]
                    sse = ((te - hv) ** 2).sum(); sst = ((te - te.mean(1, keepdims=True)) ** 2).sum()
                    lev_want[(lab, levels[fi_], cat)] = [int(sub.sum()), np_isc(P[i][:, sub], ref_u[:, sub]), acc, ch, 1.0 - sse / sst]
        got_l = {(r_["Subj"], r_["Fold"], r_["Level"]): [num(r_["nsamp"]), num(r_["isc"]), num(r_["ident"]), num(r_["chance"]), num(r_["r2"])]
                 for r_ in read_levels(d / "out.scores_folds.txt") if r_["Factor"] == "Category"}
        e = np.array([[abs(got_l[k_][q] - lev_want[k_][q]) for q in range(5)] for k_ in lev_want])
        check("-score_by Category: each level's nsamp", float(e[:, 0].max()), 0.0, "{:.0f}")
        check("-score_by Category: isc within each category vs the reference", float(e[:, 1].max()), 5e-4)
        check("-score_by Category: identification among the category's items", float(e[:, 2].max()), 1e-6)
        check("-score_by Category: chance = 1 over the category's items", float(e[:, 3].max()), 1e-6)
        check("-score_by Category: R^2 within each category", float(e[:, 4].max()), 5e-4)
        rows_s = [r_ for r_ in read_levels(d / "out.scores.txt") if r_["Factor"] == "Category"]
        mean_ok = all(abs(num(r_["isc"]) - np.mean([lev_want[(r_["Subj"], f_, r_["Level"])][1] for f_ in levels
                                                     if (r_["Subj"], f_, r_["Level"]) in lev_want])) < 5e-4 for r_ in rows_s)
        check("scores.txt: a row per subject and category, the mean over folds", float(not mean_ok or len(rows_s) != n * len(ITEM_CATS)), 0.0, "{:.0f}")
        # -reliability within: each repeated item's two measurements, in the subject's own space, overall and per category
        worst = 0.0
        all_rows = read_levels(d / "out.scores.txt")
        for i, lab in enumerate(sub_order):
            reps = {}
            for j, key in enumerate(keys):
                if ":" in key:
                    reps.setdefault(key.split(":")[0], []).append(j)
            cat_first = {b: cat_of[lab][cols[0]] for b, cols in reps.items()}
            for lev in [None] + ITEM_CATS:
                bases = [b for b in reps if lev is None or cat_first[b] == lev]
                xa = np.array([pre[i][:, reps[b][0]] for b in bases]).T if bases else np.zeros((1, 0))
                xb = np.array([pre[i][:, reps[b][1]] for b in bases]).T if bases else np.zeros((1, 0))
                rp, rs = np_split_rel(xa, xb) if len(bases) >= 1 else (float("nan"), float("nan"))
                r_ = next(x for x in all_rows if x["Subj"] == lab and x["Level"] == ("all" if lev is None else lev))
                for got_v, want_v in ((r_["rel_pattern"], rp), (r_["rel_series"], rs)):
                    g = num(got_v)
                    worst = max(worst, 0.0 if (np.isnan(g) and np.isnan(want_v)) else abs(g - want_v) if np.isfinite(g) else 1.0)
        check("-reliability within (items: repeat 1 vs repeat 2), overall and per category", worst, 1e-5)
    if mode == "run" and folds_by == "own":            # every refit sees every item; own held-out items missing
        for fi_ in range(len(levels)):
            masks = [np.ones(T, dtype=bool) for _ in range(n)]
            for i, lab in enumerate(sub_order):
                masks[i][own_folds[lab][fi_]] = False
            r = reference_fit(cli, work, f"{name}_f{fi_}", pre, k, niter, algo, extra=obs_extra(d, f"own{fi_}", masks))
            for i, lab in enumerate(sub_order):
                cols = own_folds[lab][fi_]
                P = [r["w"][j].T @ pre[j][:, cols] for j in range(n)]
                oth = [j for j in range(n) if j != i]
                ref_u = sum(P[j] for j in oth) / len(oth)
                if algo == "prob":
                    prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(k)
                    ref_r = np.linalg.solve(prec, sum(P[j] / r["rho2"][j] for j in oth))
                else:
                    ref_r = ref_u
                acc, ch = np_item_identify(P[i], ref_u)
                te = pre[i][:, cols]
                sse = ((te - r["w"][i] @ ref_r) ** 2).sum(1)
                sst = ((te - te.mean(1, keepdims=True)) ** 2).sum(1)
                want[(lab, levels[fi_])] = [np_isc(P[i], ref_u), acc, ch, 1.0 - sse.sum() / sst.sum()]
            if not by_label[sub_order[0]]["is1D"]:     # held-out projections, in this fold's model up to the gauge
                pj = load_afni(d / f"out.proj.{sub_order[0]}").reshape(k, T)
                cols = own_folds[sub_order[0]][fi_]
                check(f"fold {levels[fi_]}: held-out projections equal the fold model's up to a rotation",
                      procrustes_residual(pj[:, cols], r["w"][0].T @ pre[0][:, cols]), 2e-3)
    elif mode == "run":
        for fi_, cols in enumerate(folds):
            tr = [np.delete(z, cols, axis=1) for z in pre]
            te = [z[:, cols] for z in pre]
            r = reference_fit(cli, work, f"{name}_f{fi_}", tr, k, niter, algo)
            P = [r["w"][i].T @ te[i] for i in range(n)]
            for i in range(n):
                oth = [j for j in range(n) if j != i]
                ref_u = sum(P[j] for j in oth) / len(oth)
                if algo == "prob":
                    prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(k)
                    ref_r = np.linalg.solve(prec, sum(P[j] / r["rho2"][j] for j in oth))
                else:
                    ref_r = ref_u
                acc, ch = np_item_identify(P[i], ref_u)
                sse = ((te[i] - r["w"][i] @ ref_r) ** 2).sum(1)
                sst = ((te[i] - te[i].mean(1, keepdims=True)) ** 2).sum(1)
                want[(sub_order[i], levels[fi_])] = [np_isc(P[i], ref_u), acc, ch, 1.0 - sse.sum() / sst.sum()]
        worst_proj = 0.0
        for i in range(n):
            if by_label[sub_order[i]]["is1D"]:
                continue
            pj = load_afni(d / f"out.proj.{sub_order[i]}").reshape(k, T)
            for fi_, cols in enumerate(folds):
                r = run_ours(cli, work, f"{name}_f{fi_}", algo, [np.delete(z, cols, axis=1).astype(np.float32) for z in pre],
                             k, niter, False, extra=["--canon"])
                worst_proj = max(worst_proj, procrustes_residual(pj[:, cols], r["w"][i].T @ pre[i][:, cols]))
        check("held-out item projections equal the fold models' up to a rotation of the gauge", worst_proj, 2e-3)
    else:
        full = reference_fit(cli, work, name + "_full", pre, k, niter, algo)
        for i in range(n):
            oth = [j for j in range(n) if j != i]
            dd = work / f"{name}_drop{i}"
            dd.mkdir(parents=True, exist_ok=True)
            extra = []
            if algo == "prob":
                full["rho2"][oth].astype(np.float64).tofile(dd / "rho2.f64")
                full["sigma"].astype(np.float32).tofile(dd / "sigma.f32")
                extra = ["--init-rho2", str(dd / "rho2.f64"), "--init-sigma", str(dd / "sigma.f32")]
            r = run_ours(cli, work, f"{name}_drop{i}", algo, [pre[j].astype(np.float32) for j in oth], k, 5, False,
                         init_w=[full["w"][j] for j in oth], extra=extra)
            for fi_, cols in enumerate(own_folds[sub_order[i]]):
                tr, te = np.delete(pre[i], cols, axis=1), pre[i][:, cols]
                W = np_polar(tr @ np.delete(r["s"], cols, axis=1).T)
                Pm, s_te = W.T @ te, r["s"][:, cols]
                acc, ch = np_item_identify(Pm, s_te)
                sse = ((te - W @ s_te) ** 2).sum(1)
                sst = ((te - te.mean(1, keepdims=True)) ** 2).sum(1)
                want[(sub_order[i], levels[fi_])] = [np_isc(Pm, s_te), acc, ch, 1.0 - sse.sum() / sst.sum()]
    errs = np.array([[abs(got[k_][q] - want[k_][q]) for q in range(4)] for k_ in want])
    check("isc per subject and fold vs the reference", float(errs[:, 0].max()), 5e-4)
    check("item identification accuracy vs the reference", float(errs[:, 1].max()), 1e-6)
    check("... and its chance level (1 over the fold's items)", float(errs[:, 2].max()), 1e-6)
    check("held-out R^2 vs the reference", float(errs[:, 3].max()), 5e-4)
    acc_mean = float(np.mean([want[k_][1] for k_ in want]))
    ch_mean = float(np.mean([want[k_][2] for k_ in want]))
    check("identification is well above chance on this signal (sanity)", float(not acc_mean > 3 * ch_mean), 0.0, "{:.0f}")
    if folds_by == "category":                          # Category varies over a subject's items: not carried along
        head_s, _ = read_scores(d / "out.scores.txt")
        check("an item-level column is not carried into scores.txt", float("Category" in head_s), 0.0, "{:.0f}")
        bad = [dict(r, cat="tool") if (r["subj"] == sub_order[2] and r["item"] == "it01") else r for r in rows]
        write_item_table(d / "badfold.txt", bad, with_cat=True)
        pr = run([prog, "-progress", "off", "-prefix", d / "badfold", "-overwrite", "-nfeatures", k, "-niter", 5, "-dataTableFile",
                  d / "badfold.txt", "-axis", "item", "-cv", "run", "-fold_column", "Category"])
        check("a fold column that differs between subjects gives each subject its own folds",
              float("differs between subjects" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")


def case_time_zscore_all(prog, cli, work, rng):
    """-zscore all on the time axis: each feature z-scored over the whole timeline, run boundaries ignored."""
    print("\n[time axis, -zscore all]", flush=True)
    d = work / "time_zall"
    _, subs, common = make_cv_world(rng, RUNS)
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    run([prog, "-prefix", d / "out", "-nfeatures", K, "-niter", 40, "-tol", 0, "-zscore", "all", "-mask", d / "mask.nii.gz",
         "-dataTableFile", d / "table.txt", "-progress", "off", "-overwrite"])
    pre = []
    for sub in subs:
        x = sub["x"].astype(np.float32).astype(np.float64)
        x = x[x.std(1) > 1e-7 * np.abs(x.mean(1))]       # only a feature flat over everything is dropped
        pre.append((x - x.mean(1, keepdims=True)) / x.std(1, keepdims=True))
    ref = reference_fit(cli, work, "time_zall_ref", pre, K, 40, "prob")
    check("-zscore all (time axis) vs the driver on data z-scored over the whole timeline",
          relfro(np.loadtxt(d / "out.S.1D").T, ref["s"]), 2e-4)
    hdr, _, _, _ = parse_srm_file(d / "out.srm")
    check("the model records zscore all", float(hdr["zscore"][0] != "all"), 0.0, "{:.0f}")


def write_xmat(path, nfull, starts, good=None):
    """A design matrix in afni_proc.py's X.xmat.1D form (commented NIML), with one column of ones: only the
    header attributes matter here."""
    good = list(range(nfull)) if good is None else good
    rows = "\n".join("1" for _ in good)
    Path(path).write_text("# <matrix\n#  ni_type = \"1*double\"\n"
                          f"#  ni_dimen = \"{len(good)}\"\n#  RowTR = \"2\"\n"
                          f"#  GoodList = \"{','.join(map(str, good))}\"\n#  NRowFull = \"{nfull}\"\n"
                          f"#  RunStart = \"{','.join(map(str, starts))}\"\n#  ColumnLabels = \"Run#1Pol#0\"\n# >\n"
                          f"{rows}\n# </matrix>\n")


def case_concat(prog, cli, work, rng):
    """One input per subject holding every run (afni_proc.py's errts): -concat, or an Xmat column, gives the runs."""
    print("\n[runs inside one input: -concat and Xmat]", flush=True)
    d = work / "concat"
    _, subs, common = make_cv_world(rng, RUNS)
    rows = write_world(d, subs)
    write_table(d / "runs.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    starts = [0, RUNS[0], RUNS[0] + RUNS[1]]
    T = sum(RUNS)
    one, xm = ["Subj Group InputFile"], ["Subj Group Xmat InputFile"]
    for sub in subs:
        imgs = [nib.load(str(d / f"{sub['label']}_run{r + 1}.nii.gz")) for r in range(len(RUNS))]
        f = d / f"errts.{sub['label']}.nii.gz"
        nib.save(nib.Nifti1Image(np.concatenate([np.asarray(i.dataobj) for i in imgs], axis=3), imgs[0].affine), str(f))
        write_xmat(d / f"X.{sub['label']}.xmat.1D", T, starts)
        one.append(f"{sub['label']} {sub['group']} {f}")
        xm.append(f"{sub['label']} {sub['group']} {d / ('X.' + sub['label'] + '.xmat.1D')} {f}")
    (d / "one.txt").write_text("\n".join(one) + "\n")
    (d / "xmat.txt").write_text("\n".join(xm) + "\n")
    base = [prog, "-nfeatures", K, "-niter", 40, "-tol", 0, "-mask", d / "mask.nii.gz", "-progress", "off", "-overwrite",
            "-cv", "run", "-match_window", 5]
    run(base + ["-prefix", d / "runs", "-dataTableFile", d / "runs.txt"])
    p = run(base + ["-prefix", d / "cat", "-dataTableFile", d / "one.txt", "-concat", "1D: " + " ".join(map(str, starts))])
    log_cat = p.stdout + p.stderr
    p = run(base + ["-prefix", d / "xm", "-dataTableFile", d / "xmat.txt"])
    log_xm = p.stdout + p.stderr
    S0 = np.loadtxt(d / "runs.S.1D")
    check("-concat: S identical to separate run files", float(np.abs(np.loadtxt(d / "cat.S.1D") - S0).max()), 0.0, "{:.2e}")
    check("Xmat column: S identical to separate run files", float(np.abs(np.loadtxt(d / "xm.S.1D") - S0).max()), 0.0, "{:.2e}")
    def scores(f):                                      # every column but the fold's name (r1 from a Run column, 1 here)
        return [r[:2] + r[3:] for r in read_scores(d / f)[1]]
    f0 = scores("runs.scores_folds.txt")
    check("-concat: the -cv run scores are the separate-run ones", float(scores("cat.scores_folds.txt") != f0), 0.0, "{:.0f}")
    check("Xmat: the -cv run scores are the separate-run ones", float(scores("xm.scores_folds.txt") != f0), 0.0, "{:.0f}")
    check("the log reports the runs found in the inputs", float("each input holds 3 runs" not in log_cat or "RunStart" not in log_xm), 0.0, "{:.0f}")
    p = run([prog, "-nfeatures", K, "-niter", 5, "-mask", d / "mask.nii.gz", "-progress", "off", "-overwrite",
             "-prefix", d / "long", "-dataTableFile", d / "one.txt"])
    check("one long input without -concat: the log suggests -concat or Xmat", float("give -concat or an Xmat column" not in p.stdout + p.stderr), 0.0, "{:.0f}")

    # a censored GoodList: reported, not yet applied
    lab0 = subs[0]["label"]
    write_xmat(d / "X.cens.xmat.1D", T, starts, good=[t for t in range(T) if t not in (5, 6, 70)])
    (d / "xmat_cens.txt").write_text((d / "xmat.txt").read_text().replace(f"X.{lab0}.xmat.1D", "X.cens.xmat.1D"))
    p = run(base + ["-prefix", d / "xc", "-dataTableFile", d / "xmat_cens.txt", "-cv", "none"])
    check("a censored GoodList is reported", float("GoodList censors time points" not in p.stdout + p.stderr), 0.0, "{:.0f}")

    def fails(args, needle, label):
        pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-mask", d / "mask.nii.gz"] + args,
                 expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    fails(["-dataTableFile", d / "one.txt", "-concat", "1D: 5 60 110"], "must start at 0", "E: -concat not starting at 0")
    fails(["-dataTableFile", d / "one.txt", "-concat", "1D: 0 110 60"], "must increase", "E: -concat out of order")
    fails(["-dataTableFile", d / "one.txt", "-concat", f"1D: 0 60 {T}"], "the last run starts at", "E: -concat past the end")
    fails(["-dataTableFile", d / "runs.txt", "-concat", "1D: 0 60 110"], "Use one or the other", "E: -concat with a Run column")
    fails(["-dataTableFile", d / "xmat.txt", "-concat", "1D: 0 60 110"], "not both", "E: -concat and an Xmat column")
    fails(["-axis", "item", "-dataTableFile", d / "one.txt", "-concat", "1D: 0 60 110"], "it is for -axis time", "E: -concat with -axis item")
    write_xmat(d / "X.short.xmat.1D", T - 1, starts)
    (d / "xmat_short.txt").write_text((d / "xmat.txt").read_text().replace(f"X.{lab0}.xmat.1D", "X.short.xmat.1D"))
    fails(["-dataTableFile", d / "xmat_short.txt"], "(NRowFull), but the input has", "E: an Xmat whose NRowFull is not the input's length")
    write_xmat(d / "X.other.xmat.1D", T, [0, 90])
    (d / "xmat_other.txt").write_text((d / "xmat.txt").read_text().replace(f"X.{subs[1]['label']}.xmat.1D", "X.other.xmat.1D"))
    fails(["-dataTableFile", d / "xmat_other.txt"], "every subject needs the same runs", "E: Xmat runs that differ between subjects")


def interp_censored(x, cens, runs):
    """What -cenmode interp does, written with np.interp: within each run, a censored time point gets the
    linear interpolation of the nearest kept ones (the nearest one at a run's edge)."""
    y = x.astype(np.float32).astype(np.float64).copy()
    c0 = 0
    for L in runs:
        t = np.arange(c0, c0 + L)
        good, bad = t[~cens[t]], t[cens[t]]
        for f in range(y.shape[0]):
            y[f, bad] = np.interp(bad, good, y[f, good])
        c0 += L
    return y


def case_censor(prog, cli, work, rng):
    """Each subject's own censoring, from zero volumes (-zcensor), a Censor column, and an Xmat GoodList."""
    print("\n[per-subject censoring: interp and drop]", flush=True)
    d = work / "censor"
    _, subs, common = make_cv_world(rng, RUNS)
    d.mkdir(parents=True, exist_ok=True)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    T = sum(RUNS)
    starts = [0, RUNS[0], RUNS[0] + RUNS[1]]
    cens = [np.zeros(T, dtype=bool) for _ in subs]
    cens[0][[3, 4, 100]] = True                          # zero volumes in the input (-zcensor)
    cens[1][[RUNS[0], RUNS[0] + 1, 20, 21, 22, 23, T - 1]] = True   # a run's first points, a block, the last point
    cens[2][[7, 150]] = True                             # the Xmat's GoodList
    lines = ["Subj Group Xmat Censor InputFile"]
    for i, sub in enumerate(subs):
        vol = np.zeros((NX, NY, NZ, T), dtype=np.float32)
        flat = vol.reshape(-1, T, order="F")
        flat[sub["idx"], :] = sub["x"]
        if i == 0:
            flat[:, cens[0]] = 0.0
        f = d / f"errts.{sub['label']}.nii.gz"
        write_nifti(f, flat.reshape((NX, NY, NZ, T), order="F"))
        xm = d / f"X.{sub['label']}.xmat.1D"
        write_xmat(xm, T, starts, good=[t for t in range(T) if not (i == 2 and cens[2][t])])
        cf = "-"
        if i == 1:
            cf = d / "censor_s1.1D"
            np.savetxt(cf, (~cens[1]).astype(int), fmt="%d")
        lines.append(f"{sub['label']} {sub['group']} {xm} {cf} {f}")
    (d / "table.txt").write_text("\n".join(lines) + "\n")
    base = [prog, "-nfeatures", K, "-niter", 40, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile", d / "table.txt",
            "-zcensor", "-progress", "off", "-overwrite"]
    p = run(base + ["-prefix", d / "interp"])
    log = p.stdout + p.stderr

    def prep(x):                                         # z-score within runs, drop features flat in a run
        return preprocess({"x": x})[0]
    pre = []
    for i, sub in enumerate(subs):
        x = sub["x"].copy()
        if i == 0:
            x[:, cens[0]] = 0.0
        pre.append(prep(interp_censored(x, cens[i], RUNS)))
    ref = reference_fit(cli, work, "censor_interp_ref", pre, K, 40, "prob")
    check("interp: S vs the driver on data interpolated in numpy", relfro(np.loadtxt(d / "interp.S.1D").T, ref["s"]), 2e-4)
    fit = [ln.split("\t") for ln in (d / "interp.fit.txt").read_text().splitlines() if ln and not ln.startswith(("#", "Subj"))]
    check("fit.txt counts each subject's own censored time points",
          float([int(r[6]) for r in fit] != [int(c.sum()) for c in cens]), 0.0, "{:.0f}")
    check("interp: every subject keeps every time point", float(np.loadtxt(d / "interp.S.1D").shape[0] != T), 0.0, "{:.0f}")
    check("the log says how censoring was handled", float("filled in by linear interpolation" not in log), 0.0, "{:.0f}")

    p = run(base + ["-prefix", d / "drop", "-cenmode", "drop"])
    union = np.logical_or.reduce(cens)
    pre_d = []
    for i, sub in enumerate(subs):
        x = sub["x"].copy()
        pre_d.append(preprocess({"x": x}, keep=~union)[0])
    ref_d = reference_fit(cli, work, "censor_drop_ref", pre_d, K, 40, "prob")
    check("drop: S vs the driver with the union of censoring dropped for all",
          relfro(np.loadtxt(d / "drop.S.1D").T, ref_d["s"]), 2e-4)
    run(base + ["-prefix", d / "cov", "-cv", "run", "-match_window", 5])
    rows_s = [r_ for r_ in read_levels(d / "cov.scores.txt") if r_["Factor"] == "all"]
    want_c = {sub["label"]: (2 if i == 2 else 0) for i, sub in enumerate(subs)}       # only the third Xmat censors
    ok = all(r_.get("Xmat.ncensored") is not None and int(float(r_["Xmat.ncensored"])) == want_c[r_["Subj"]]
             and int(float(r_["Xmat.ntime"])) == T and int(float(r_["Xmat.nrun"])) == 3 and abs(float(r_["Xmat.TR"]) - 2.0) < 1e-6
             and abs(float(r_["Xmat.frac_censored"]) - want_c[r_["Subj"]] / T) < 1e-5 for r_ in rows_s)
    check("scores.txt carries each subject's Xmat summary (Xmat.TR, .nrun, .ntime, .ncensored, .frac_censored)",
          float(not ok or len(rows_s) != len(subs)), 0.0, "{:.0f}")
    check("drop: the log counts what everyone loses", float(f"{int(union.sum())} of {T} time points are censored in at least one" not in p.stdout + p.stderr), 0.0, "{:.0f}")
    p = run(base + ["-prefix", d / "warn", "-zcensor_warn", 2])
    check("-zcensor_warn names the subjects above it, from any source of censoring",
          float(f"Subj {subs[1]['label']} has 7 of" not in p.stdout + p.stderr or f"Subj {subs[2]['label']}" in p.stdout + p.stderr), 0.0, "{:.0f}")

    def fails(args, needle, label, table=None):
        nf = [] if "-model_in" in args else ["-nfeatures", K]
        pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-mask", d / "mask.nii.gz",
                  "-dataTableFile", table or (d / "table.txt")] + nf + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    np.savetxt(d / "short.1D", np.ones(T - 2, dtype=int), fmt="%d")
    (d / "t_short.txt").write_text((d / "table.txt").read_text().replace(str(d / "censor_s1.1D"), str(d / "short.1D")))
    fails([], "has 178 values; the subject's inputs have 180", "E: a Censor file of the wrong length", d / "t_short.txt")
    bad = np.ones(T, dtype=int); bad[5] = 2
    np.savetxt(d / "two.1D", bad, fmt="%d")
    (d / "t_two.txt").write_text((d / "table.txt").read_text().replace(str(d / "censor_s1.1D"), str(d / "two.1D")))
    fails([], "expected 0 (censored) or 1 (kept)", "E: a Censor value that is not 0 or 1", d / "t_two.txt")
    allr = np.ones(T, dtype=int); allr[RUNS[0]:RUNS[0] + RUNS[1]] = 0
    np.savetxt(d / "allrun.1D", allr, fmt="%d")
    (d / "t_all.txt").write_text((d / "table.txt").read_text().replace(str(d / "censor_s1.1D"), str(d / "allrun.1D")))
    fails([], "every time point of run 2 is censored", "E: interpolation in a run with nothing kept", d / "t_all.txt")
    # -model_in: a new subject's censoring is interpolated; drop would change the model's timeline
    run(base + ["-prefix", d / "model", "-cenmode", "drop"])
    fails(["-model_in", d / "model.srm", "-cenmode", "drop"], "would change the model's timeline", "E: -cenmode drop with -model_in")
    p = run([prog, "-prefix", d / "newsub", "-model_in", d / "interp.srm", "-mask", d / "mask.nii.gz", "-dataTableFile", d / "table.txt",
             "-zcensor", "-progress", "off", "-overwrite"], expect_ok=False)
    check("-model_in takes per-subject censoring (interpolated)", float("filled in by linear interpolation" not in p.stdout + p.stderr), 0.0, "{:.0f}")


def np_em_missing(xs, obs, w0, niter, algo):
    """SRM with missing data written from the model, not from the C: x_it = W_i s_t + e (observed (i,t) only),
    s_t ~ N(0, Sigma), e ~ N(0, rho2_i I). Returns the objective trace (PROB: the log density of the observed
    entries, from their full covariance at each t, without the 2 pi constant; DET: the squared error over
    observed entries) and the final S."""
    n, k, T = len(xs), w0[0].shape[1], xs[0].shape[1]
    W = [w.astype(np.float64).copy() for w in w0]
    rho2, Sig = np.ones(n), np.eye(k)
    trace = []
    for it in range(niter + 1):
        if algo == "det":
            S = np.zeros((k, T))
            for t in range(T):
                o = [i for i in range(n) if obs[i][t]]
                if o:
                    S[:, t] = sum(W[i].T @ xs[i][:, t] for i in o) / len(o)
            trace.append(sum(((xs[i][:, t] - W[i] @ S[:, t]) ** 2).sum() for i in range(n) for t in range(T) if obs[i][t]))
            if it == niter:
                return trace, S
            W = [np_polar(sum(np.outer(xs[i][:, t], S[:, t]) for t in range(T) if obs[i][t])) for i in range(n)]
            continue
        Es, Cs, L = np.zeros((k, T)), [], 0.0
        for t in range(T):
            o = [i for i in range(n) if obs[i][t]]
            Wo = np.vstack([W[i] for i in o]) if o else np.zeros((0, k))
            psi = np.concatenate([np.full(xs[i].shape[0], rho2[i]) for i in o]) if o else np.zeros(0)
            y = np.concatenate([xs[i][:, t] for i in o]) if o else np.zeros(0)
            if o:
                Cfull = Wo @ Sig @ Wo.T + np.diag(psi)
                Lc = np.linalg.cholesky(Cfull)               # SPD: log det from the factor
                z = np.linalg.solve(Lc, y)
                L += -0.5 * (2.0 * np.log(np.diag(Lc)).sum() + z @ z)
            prec = np.linalg.inv(Sig) + (sum(1.0 / rho2[i] for i in o)) * np.eye(k)
            C = np.linalg.inv(prec)
            Es[:, t] = C @ (sum(W[i].T @ xs[i][:, t] / rho2[i] for i in o) if o else np.zeros(k))
            Cs.append(C)
        trace.append(L)
        if it == niter:
            return trace, Es
        W = [np_polar(sum(np.outer(xs[i][:, t], Es[:, t]) for t in range(T) if obs[i][t])) for i in range(n)]
        for i in range(n):
            ts = [t for t in range(T) if obs[i][t]]
            r = sum((xs[i][:, t] ** 2).sum() - 2 * xs[i][:, t] @ W[i] @ Es[:, t] + Es[:, t] @ Es[:, t] + np.trace(Cs[t]) for t in ts)
            rho2[i] = r / (len(ts) * xs[i].shape[0])
        Sig = (Es @ Es.T + sum(Cs)) / T


def case_missing_lib(cli, work, rng):
    """The library's missing-data fit (SRM_params.obs, through srm_cli --obs) against np_em_missing, step for step."""
    print("\n[missing data in the fit: the library against a numpy EM]", flush=True)
    d = work / "missing_lib"
    d.mkdir(parents=True, exist_ok=True)
    k, T, nv = 3, 50, [20, 24, 18, 22]
    s_true = rng.standard_normal((k, T)) * np.array([[1.5], [1.0], [0.6]])
    xs, obs, w0 = [], [], []
    for i, v in enumerate(nv):
        q, _ = np.linalg.qr(rng.standard_normal((v, k)))
        x = q @ s_true + (0.5 + 0.1 * i) * rng.standard_normal((v, T))
        xs.append(x.astype(np.float32).astype(np.float64))
        ob = rng.uniform(size=T) > 0.2
        ob[9] = False if i < 2 else ob[9]
        obs.append(ob)
        w0.append(np.linalg.qr(rng.standard_normal((v, k)))[0])
    for algo in ("prob", "det"):
        extra = []
        for i, ob in enumerate(obs):
            f = d / f"obs{i}.u8"
            ob.astype(np.uint8).tofile(f)
            extra += ["--obs", str(i), str(f)]
        garbage = [np.where(ob[None, :], x, 7.0e3) for x, ob in zip(xs, obs)]     # never read
        r = run_ours(cli, work, f"missing_lib_{algo}", algo, [g.astype(np.float32) for g in garbage], k, 25, False,
                     init_w=w0, extra=extra)
        tr, S = np_em_missing(xs, obs, w0, 25, algo)
        tr = np.array(tr)
        check(f"{algo}: objective trace vs the numpy EM, max relative", float(np.abs(r["obj"][:26] - tr).max() / np.abs(tr).max()), 1e-6)
        check(f"{algo}: S after 25 iterations vs the numpy EM", relfro(r["s"], S), 1e-4)


def preprocess_obs(x, miss, runs):
    """-cenmode missing, from the help: z-score each feature over its observed samples within each run; a
    feature with no variance in its observed samples of some run is dropped; missing samples become 0."""
    x = x.astype(np.float32).astype(np.float64)
    z, bad, c0 = np.zeros_like(x), np.zeros(x.shape[0], dtype=bool), 0
    for L in runs:
        cols = np.arange(c0, c0 + L)
        ob = cols[~miss[cols]]
        m, sd = x[:, ob].mean(1), x[:, ob].std(1)
        bad |= ~(sd > 1e-7 * np.abs(m))
        z[:, ob] = (x[:, ob] - m[:, None]) / np.where(sd > 0, sd, 1.0)[:, None]
        c0 += L
    return z[~bad]


def np_segment_match_obs(a, b, win, valid):
    """Segment matching where a window holding an invalid sample is neither scored nor a candidate."""
    T = a.shape[1]
    nseg = T - win
    ok = np.array([valid[i:i + win].all() for i in range(nseg)])
    za = np.array([a[:, i:i + win].ravel() for i in range(nseg)])
    zb = np.array([b[:, i:i + win].ravel() for i in range(nseg)])
    za = za - za.mean(1, keepdims=True); za /= np.maximum(np.linalg.norm(za, axis=1, keepdims=True), 1e-300)
    zb = zb - zb.mean(1, keepdims=True); zb /= np.maximum(np.linalg.norm(zb, axis=1, keepdims=True), 1e-300)
    hits, chs = [], []
    for i in np.flatnonzero(ok):
        cand = [j for j in np.flatnonzero(ok) if j == i or abs(i - j) >= win]
        c = [za[i] @ zb[j] for j in cand]
        hits.append(cand[int(np.argmax(c))] == i)
        chs.append(1.0 / len(cand))
    return float(np.mean(hits)), float(np.mean(chs))


def obs_extra(d, tag, masks):
    """srm_cli --obs arguments for per-view observed masks (None: all observed)."""
    out = []
    for i, m in enumerate(masks):
        if m is not None and not m.all():
            f = d / f"{tag}_obs{i}.u8"
            m.astype(np.uint8).tofile(f)
            out += ["--obs", str(i), str(f)]
    return out


def case_missing(prog, cli, work, rng, mode, algo, win=5, niter=40):
    """-cenmode missing end to end: each subject's censored time points are missing for that subject only."""
    name = f"missing_{mode}_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    _, subs, common = make_cv_world(rng, RUNS, nsub=6)
    rows = write_world(d, subs)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    T = sum(RUNS)
    miss = []
    for i, sub in enumerate(subs):
        m = rng.uniform(size=T) < 0.08
        m[30 + 3 * i:34 + 3 * i] = True                 # a block, at a different place for each subject
        miss.append(m)
        np.savetxt(d / f"{sub['label']}_censor.1D", (~m).astype(int), fmt="%d")
    lines = ["Subj Group Run Censor InputFile"] + [f"{s_} {g} {r} {d / (s_ + '_censor.1D')} {f}" for s_, g, r, _, f in rows]
    (d / "table.txt").write_text("\n".join(lines) + "\n")
    cmd = [prog, "-prefix", d / "out", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz",
           "-dataTableFile", d / "table.txt", "-cenmode", "missing", "-progress", "off", "-overwrite"]
    if mode != "none":
        cmd += ["-cv", mode, "-match_window", win]
    p = run(cmd)
    log = p.stdout + p.stderr
    pre = [preprocess_obs(sub["x"], miss[i], RUNS) for i, sub in enumerate(subs)]
    obsm = [~m for m in miss]
    n = len(subs)
    names = [sub["label"] for sub in subs]
    if mode == "none":
        full = reference_fit(cli, work, name + "_full", pre, K, niter, algo, extra=obs_extra(d, "full", obsm))
        check("missing: S vs the driver with each subject's observed mask", relfro(np.loadtxt(d / "out.S.1D").T, full["s"]), 2e-4)
        worst, zero_ok = 0.0, True
        for i in range(n):
            pj = load_afni(d / f"out.proj.{names[i]}").reshape(K, T)
            worst = max(worst, relfro(pj[:, obsm[i]], (full["w"][i].T @ pre[i])[:, obsm[i]]))
            zero_ok &= bool(np.all(pj[:, miss[i]] == 0.0))
        check("projected series vs W'X at observed time points", worst, 5e-3)
        check("projected series are 0 at the subject's missing time points", float(not zero_ok), 0.0, "{:.0f}")
        check("the log says censored points are missing for that subject only", float("missing for that subject only" not in log), 0.0, "{:.0f}")
        return
    folds, c0 = [], 0
    for L in RUNS:
        folds.append(np.arange(c0, c0 + L)); c0 += L
    head, body = read_scores(d / "out.scores_folds.txt")
    got = {(r[0], r[2]): [num(r[3]), num(r[4]), num(r[5]), num(r[6]), num(r[7])] for r in body}
    fold_names = list(dict.fromkeys(r[2] for r in body))
    want = {}
    if mode == "run":
        for fi_, cols in enumerate(folds):
            tr = [np.delete(z, cols, axis=1) for z in pre]
            te = [z[:, cols] for z in pre]
            otr = [np.delete(o, cols) for o in obsm]
            ote = [o[cols] for o in obsm]
            r = reference_fit(cli, work, f"{name}_f{fi_}", tr, K, niter, algo, extra=obs_extra(d, f"f{fi_}", otr))
            P = [r["w"][i].T @ te[i] for i in range(n)]
            for i in range(n):
                oth = [j for j in range(n) if j != i]
                ref_u, ref_r = np.zeros((K, len(cols))), np.zeros((K, len(cols)))
                for t in range(len(cols)):
                    o = [j for j in oth if ote[j][t]]
                    if not o:
                        continue
                    ref_u[:, t] = sum(P[j][:, t] for j in o) / len(o)
                    if algo == "prob":
                        prec = np.linalg.inv(r["sigma"]) + sum(1.0 / r["rho2"][j] for j in o) * np.eye(K)
                        ref_r[:, t] = np.linalg.solve(prec, sum(P[j][:, t] / r["rho2"][j] for j in o))
                    else:
                        ref_r[:, t] = ref_u[:, t]
                valid = ote[i] & np.array([any(ote[j][t] for j in oth) for t in range(len(cols))])
                acc, ch = np_segment_match_obs(P[i], ref_u, win, valid)
                xv, hv = te[i][:, valid], (r["w"][i] @ ref_r)[:, valid]
                sse = ((xv - hv) ** 2).sum()
                sst = ((xv - xv.mean(1, keepdims=True)) ** 2).sum()
                want[(names[i], fold_names[fi_])] = [int(valid.sum()), np_isc(P[i][:, valid], ref_u[:, valid]), acc, ch, 1.0 - sse / sst]
    else:
        full = reference_fit(cli, work, name + "_full", pre, K, niter, algo, extra=obs_extra(d, "full", obsm))
        for i in range(n):
            oth = [j for j in range(n) if j != i]
            dd = work / f"{name}_drop{i}"
            dd.mkdir(parents=True, exist_ok=True)
            extra = obs_extra(dd, "o", [obsm[j] for j in oth])
            if algo == "prob":
                full["rho2"][oth].astype(np.float64).tofile(dd / "rho2.f64")
                full["sigma"].astype(np.float32).tofile(dd / "sigma.f32")
                extra += ["--init-rho2", str(dd / "rho2.f64"), "--init-sigma", str(dd / "sigma.f32")]
            r = run_ours(cli, work, f"{name}_drop{i}", algo, [pre[j].astype(np.float32) for j in oth], K, 5, False,
                         init_w=[full["w"][j] for j in oth], extra=extra)
            for fi_, cols in enumerate(folds):
                otr = np.delete(obsm[i], cols)
                tr = np.delete(pre[i], cols, axis=1)[:, otr]
                W = np_polar(tr @ np.delete(r["s"], cols, axis=1)[:, otr].T)
                valid = obsm[i][cols]
                te, s_te = pre[i][:, cols], r["s"][:, cols]
                Pm = W.T @ te
                acc, ch = np_segment_match_obs(Pm, s_te, win, valid)
                xv, hv = te[:, valid], (W @ s_te)[:, valid]
                sse = ((xv - hv) ** 2).sum()
                sst = ((xv - xv.mean(1, keepdims=True)) ** 2).sum()
                want[(names[i], fold_names[fi_])] = [int(valid.sum()), np_isc(Pm[:, valid], s_te[:, valid]), acc, ch, 1.0 - sse / sst]
    errs = np.array([[abs(got[k_][q] - want[k_][q]) for q in range(5)] for k_ in want])
    check("nsamp: the observed (and referenced) samples of each fold", float(errs[:, 0].max()), 0.0, "{:.0f}")
    check("isc on observed samples vs the reference", float(errs[:, 1].max()), 5e-4)
    check("segment matching without windows that hold a missing sample", float(errs[:, 2].max()), 1e-6)
    check("... and its chance level", float(errs[:, 3].max()), 1e-6)
    check("held-out R^2 on observed samples vs the reference", float(errs[:, 4].max()), 5e-4)
    if mode == "loso" and algo == "prob":              # refusals, and -model_in with missing data
        def fails(args, needle, label):
            pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-mask", d / "mask.nii.gz",
                      "-dataTableFile", d / "table.txt"] + args, expect_ok=False)
            out = pr.stdout + pr.stderr
            check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
            if pr.returncode == 0 or needle not in out:
                print(out)
        few = miss[0].copy(); few[RUNS[0]:RUNS[0] + RUNS[1] - 2] = True
        np.savetxt(d / "few.1D", (~few).astype(int), fmt="%d")
        (d / "t_few.txt").write_text((d / "table.txt").read_text().replace(str(d / (names[0] + "_censor.1D")), str(d / "few.1D")))
        pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-mask", d / "mask.nii.gz", "-nfeatures", K,
                  "-dataTableFile", d / "t_few.txt", "-cenmode", "missing"], expect_ok=False)
        check("E: a run with fewer than 3 observed time points for a subject",
              float(pr.returncode == 0 or "observed time points in run" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")
        p2 = run([prog, "-prefix", d / "newsub", "-model_in", d / "out.srm", "-mask", d / "mask.nii.gz", "-dataTableFile",
                  d / "table.txt", "-cenmode", "missing", "-cv", "run", "-match_window", win, "-progress", "off", "-overwrite"], expect_ok=False)
        check("-model_in -cv run with missing data runs", float(p2.returncode != 0), 0.0, "{:.0f}")
        if p2.returncode == 0:
            pj = load_afni(d / f"newsub.proj.{names[1]}").reshape(K, T)
            check("-model_in: held-out projections are 0 at missing time points", float(not np.all(pj[:, miss[1]] == 0.0)), 0.0, "{:.0f}")


def case_missing_more(prog, cli, work, rng, algo, lam=30.0, niter=40, win=5):
    """-cenmode missing with -run_coupling, -update_S, -apply_W, and -cv_adapt; each against the driver or numpy."""
    name = f"missing_more_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    _, subs, common = make_cv_world(rng, RUNS, nsub=7)
    rows = write_world(d, subs)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    T, n = sum(RUNS), len(subs)
    names = [s_["label"] for s_ in subs]
    miss = []
    for i, sub in enumerate(subs):
        m = rng.uniform(size=T) < 0.08
        m[30 + 3 * i:34 + 3 * i] = True
        miss.append(m)
        np.savetxt(d / f"{sub['label']}_censor.1D", (~m).astype(int), fmt="%d")
    obsm = [~m for m in miss]
    pre = [preprocess_obs(sub["x"], miss[i], RUNS) for i, sub in enumerate(subs)]

    def table(path, who):
        lines = ["Subj Group Run Censor InputFile"] + [f"{s_} {g} {r} {d / (s_ + '_censor.1D')} {f}" for s_, g, r, _, f in rows if s_ in who]
        Path(path).write_text("\n".join(lines) + "\n")
    table(d / "all.txt", names)
    table(d / "base.txt", names[:5])
    table(d / "new.txt", names[5:])
    common_args = ["-mask", d / "mask.nii.gz", "-cenmode", "missing", "-progress", "off", "-overwrite"]

    # -- -run_coupling with missing data: S vs the driver's coupled fit with the same masks
    run([prog, "-prefix", d / "rc", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0, "-run_coupling", lam,
         "-dataTableFile", d / "all.txt"] + common_args)
    ref = run_ours(cli, work, name + "_rc", algo, [z.astype(np.float32) for z in pre], K, niter, False,
                   extra=["--canon"] + obs_extra(d, "rc", obsm), seglen=RUNS, coupling=lam)
    check("-run_coupling, missing data: S vs the driver (coupled, each subject's mask)", relfro(np.loadtxt(d / "rc.S.1D").T, ref["s"]), 2e-4)
    _, _, _, b = parse_srm_file(d / "rc.srm")
    check("... the model file counts the subjects observed at each time point",
          float(np.abs(b["nsubj_t"].ravel() - np.sum(obsm, axis=0)).max()), 0.0, "{:.0f}")

    # -- -cv run, -cv_adapt with missing data: the scored samples are the observed (and referenced) rest of each run
    run([prog, "-prefix", d / "ad", "-nfeatures", K, "-algorithm", algo, "-niter", 20, "-tol", 0, "-run_coupling", lam,
         "-cv", "run", "-cv_adapt", 0.3, "-match_window", win, "-dataTableFile", d / "all.txt"] + common_args)
    _, body = read_scores(d / "ad.scores_folds.txt")
    bad, c0 = 0, 0
    for q, L in enumerate(RUNS):
        na = int(0.3 * L)
        rest = np.arange(c0 + na, c0 + L)
        for i in range(n):
            valid = obsm[i][rest] & np.any([obsm[j][rest] for j in range(n) if j != i], axis=0)
            r_ = next(r for r in body if r[0] == names[i] and r[2] == f"r{q + 1}")
            bad += int(r_[3]) != int(valid.sum())
        c0 += L
    check("-cv_adapt, missing data: nsamp is the observed, referenced rest of each run", float(bad), 0.0, "{:.0f}")
    run([prog, "-prefix", d / "adbig", "-nfeatures", K, "-algorithm", algo, "-niter", 20, "-tol", 0, "-run_coupling", 1e9,
         "-cv", "loso", "-cv_adapt", 0.3, "-match_window", win, "-dataTableFile", d / "all.txt"] + common_args)
    _, bb = read_scores(d / "adbig.scores_folds.txt")
    diff = max(abs(num(r[4]) - num(r[8])) for r in bb if r[4] != "NA")
    check("-cv loso -cv_adapt, missing data, lambda 1e9: the run W is the anchor", diff, 1e-4)

    # -- a base model with missing data; new subjects placed (W, rho2 from their observed samples) and absorbed
    run([prog, "-prefix", d / "base", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0, "-save_W",
         "-dataTableFile", d / "base.txt"] + common_args)
    _, _, _, bb2 = parse_srm_file(d / "base.srm")
    S = bb2["S"]
    run([prog, "-prefix", d / "mi", "-model_in", d / "base.srm", "-dataTableFile", d / "new.txt"] + common_args)
    if algo == "prob":
        rho2 = np.array([float(x) for x in Path(d / "mi.rho2.1D").read_text().splitlines()[1:]])
        e = 0.0
        for j, i in enumerate(range(5, n)):
            o = obsm[i]
            W = np_polar(pre[i][:, o] @ S[:, o].T)
            r2 = ((pre[i][:, o] - W @ S[:, o]) ** 2).mean()
            e = max(e, abs(rho2[j] - r2) / r2)
        check("-model_in, missing data: noise variances over the observed samples only", e, 1e-5)
    run([prog, "-prefix", d / "up", "-model_in", d / "base.srm", "-update_S", "-niter", 30, "-tol", 0,
         "-dataTableFile", d / "new.txt"] + common_args)
    files = []
    for i in range(5, n):
        f = d / f"u_{names[i]}.f32"
        np.ascontiguousarray(pre[i], dtype=np.float32).tofile(f)
        files.append(f)
    run_load_update(cli, d / "base.srm", d / "u_ref", files, [pre[i].shape[0] for i in range(5, n)], 30,
                    new_obs=[obsm[i] for i in range(5, n)])
    S_ref = np.fromfile(f"{d / 'u_ref'}.u_S.f32", dtype=np.float32).reshape(K, T).astype(np.float64)
    _, s_up, _, bu = parse_srm_file(d / "up.srm")
    check("-update_S, missing data on both sides: S vs the driver", float(np.abs(bu["S"] - S_ref).max() / np.abs(S_ref).max()), 2e-4)
    check("-update_S: the per-time counts add the new subjects where observed",
          float(np.abs(bu["nsubj_t"].ravel() - np.sum(obsm, axis=0)).max()), 0.0, "{:.0f}")
    check("-update_S: the model has all 7 subjects", float(len(s_up) != n), 0.0, "{:.0f}")

    # -- -apply_W with missing data: z-scored over observed samples, 0 where missing
    task, trows, tmiss = {}, [], {}
    for i, sub in enumerate(subs[:5]):
        L2 = [40, 45]
        x = rng.standard_normal((len(sub["idx"]), sum(L2))) * 2.0 + 30.0
        m = rng.uniform(size=sum(L2)) < 0.1
        task[sub["label"]], tmiss[sub["label"]] = x, m
        np.savetxt(d / f"{sub['label']}_tcens.1D", (~m).astype(int), fmt="%d")
        c0 = 0
        for r, L in enumerate(L2):
            vol = np.zeros((NX * NY * NZ, L), dtype=np.float32)
            vol[sub["idx"]] = x[:, c0:c0 + L]
            f = d / f"{sub['label']}_task{r + 1}.nii.gz"
            write_nifti(f, vol.reshape((NX, NY, NZ, L), order="F"))
            trows.append(f"{sub['label']} {sub['group']} r{r + 1} {d / (sub['label'] + '_tcens.1D')} {f}")
            c0 += L
    (d / "task.txt").write_text("Subj Group Run Censor InputFile\n" + "\n".join(trows) + "\n")
    run([prog, "-prefix", d / "task", "-apply_W", d / "base", "-dataTableFile", d / "task.txt", "-cenmode", "missing",
         "-progress", "off", "-overwrite"])
    worst, zero_ok = 0.0, True
    for sub in subs[:5]:
        lab = sub["label"]
        Wv = load_afni(d / f"base.W.{lab}")
        W = np.stack([Wv[..., c].reshape(-1, order="F")[sub["idx"]] for c in range(K)], axis=1)
        z = preprocess_obs(task[lab], tmiss[lab], [40, 45])
        P = load_afni(d / f"task.proj.{lab}").reshape(K, -1)
        want = W.T @ z
        want[:, tmiss[lab]] = 0.0
        worst = max(worst, relfro(P, want))
        zero_ok &= bool(np.all(P[:, tmiss[lab]] == 0.0))
    check("-apply_W, missing data: projections vs numpy (z over observed samples)", worst, 1e-4)
    check("-apply_W, missing data: 0 at the missing time points", float(not zero_ok), 0.0, "{:.0f}")


def case_apply_W_concat(prog, cli, work, rng, how):
    """-apply_W on one input per subject holding several runs: run starts from -concat, or from each subject's
    Xmat (lengths differ between subjects, with censored time points interpolated)."""
    name = f"apply_W_{how}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    _, subs, common = make_cv_world(rng, RUNS, nsub=4)
    rows = write_world(d, subs)
    write_table(d / "base.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    run([prog, "-prefix", d / "base", "-nfeatures", K, "-niter", 30, "-tol", 0, "-save_W", "-mask", d / "mask.nii.gz",
         "-dataTableFile", d / "base.txt", "-progress", "off", "-overwrite"])
    lines, task = ["Subj Group " + ("Xmat " if how == "xmat" else "") + "InputFile"], {}
    for i, sub in enumerate(subs):
        runs = [40, 50] if how == "concat" else ([40, 50] if i % 2 == 0 else [45, 35, 30])
        Tn = sum(runs)
        x = rng.standard_normal((len(sub["idx"]), Tn)) * 2.0 + 40.0
        vol = np.zeros((NX * NY * NZ, Tn), dtype=np.float32)
        vol[sub["idx"]] = x
        f = d / f"{sub['label']}_errts.nii.gz"
        write_nifti(f, vol.reshape((NX, NY, NZ, Tn), order="F"))
        cens = np.zeros(Tn, dtype=bool)
        if how == "xmat":
            cens[[5, 6, 41 + i]] = True
            starts = np.cumsum([0] + runs[:-1])
            write_xmat(d / f"{sub['label']}.xmat.1D", Tn, starts, good=list(np.flatnonzero(~cens)))
            lines.append(f"{sub['label']} {sub['group']} {d / (sub['label'] + '.xmat.1D')} {f}")
        else:
            lines.append(f"{sub['label']} {sub['group']} {f}")
        task[sub["label"]] = (x, runs, cens)
    (d / "task.txt").write_text("\n".join(lines) + "\n")
    extra = ["-concat", "1D: 0 40"] if how == "concat" else []
    p = run([prog, "-prefix", d / "task", "-apply_W", d / "base", "-dataTableFile", d / "task.txt", "-progress", "off",
             "-overwrite"] + extra)
    worst = 0.0
    for sub in subs:
        lab = sub["label"]
        x, runs, cens = task[lab]
        y = interp_censored(x, cens, runs)
        z, c0 = np.zeros_like(y), 0
        for L in runs:
            seg = y[:, c0:c0 + L]
            z[:, c0:c0 + L] = (seg - seg.mean(1, keepdims=True)) / seg.std(1, keepdims=True)
            c0 += L
        Wv = load_afni(d / f"base.W.{lab}")
        W = np.stack([Wv[..., c].reshape(-1, order="F")[sub["idx"]] for c in range(K)], axis=1)
        P = load_afni(d / f"task.proj.{lab}").reshape(K, -1)
        worst = max(worst, relfro(P, W.T @ z))
    check(f"-apply_W with {'-concat' if how == 'concat' else 'an Xmat column'}: projections z-scored within each run"
          + (", censored points interpolated" if how == "xmat" else ""), worst, 1e-4)


def main_missing(args, cli, rng):
    case_missing(args.prog, cli, args.workdir, rng, "none", "prob")
    for mode in ("run", "loso"):
        for algo in ("prob", "det"):
            case_missing(args.prog, cli, args.workdir, rng, mode, algo)
    for algo in ("prob", "det"):
        case_missing_more(args.prog, cli, args.workdir, rng, algo)
    case_apply_W_concat(args.prog, cli, args.workdir, rng, "concat")
    case_apply_W_concat(args.prog, cli, args.workdir, rng, "xmat")


def case_xmat_items(prog, cli, work, rng, k=4):
    """-axis item from afni_proc.py-style output: one row per subject (Xmat, stats bucket). The designs and the
    buckets are made by the real 3dDeconvolve (trial-wise -stim_times_IM, and a plain -stim_times condition)."""
    print("\n[items from each subject's Xmat and stats bucket (real 3dDeconvolve)]", flush=True)
    dec = Path(prog).parent / "3dDeconvolve"
    if not dec.exists():
        print("    SKIP: no 3dDeconvolve next to 3dSRM")
        return
    d = work / "xmat_items"
    d.mkdir(parents=True, exist_ok=True)
    nrun, L, TR = 3, 60, 2.0
    face = [8, 22, 36, 50, 64, 78, 92, 106]
    (d / "face.1D").write_text("\n".join(" ".join(str(t) for t in face) for _ in range(nrun)) + "\n")
    (d / "house.1D").write_text("\n".join("15 57 99" for _ in range(nrun)) + "\n")
    (d / "resp.1D").write_text("\n".join("30 70" for _ in range(nrun)) + "\n")
    stims = ["-num_stimts", 3, "-stim_times_IM", 1, d / "face.1D", "GAM", "-stim_label", 1, "face",
             "-stim_times", 2, d / "house.1D", "GAM", "-stim_label", 2, "house",
             "-stim_times", 3, d / "resp.1D", "GAM", "-stim_label", 3, "resp"]
    run([dec, "-nodata", nrun * L, TR, "-concat", "1D: 0 60 120", "-polort", 1] + stims +
        ["-x1D", d / "Xsim.1D", "-x1D_stop"])
    X = np.loadtxt(d / "Xsim.1D", comments="#")
    nface = nrun * len(face)
    lat = rng.standard_normal((k, nface)) * np.linspace(1.5, 0.6, k)[:, None]      # each trial's shared response
    mask = np.zeros((6, 6, 4), dtype=bool); mask[1:5, 1:5, :] = True
    idx = np.flatnonzero(mask.ravel(order="F"))
    write_nifti(d / "mask.nii.gz", mask.astype(np.float32)[..., None])
    lines = ["Subj Group Xmat InputFile"]
    for i in range(4):
        lab = f"s{i + 1}"
        q, _ = np.linalg.qr(rng.standard_normal((len(idx), k)))
        B = np.zeros((X.shape[1], len(idx)))
        B[:2 * nrun] = rng.uniform(50, 150, (2 * nrun, len(idx)))                     # baselines
        B[2 * nrun:2 * nrun + nface] = (q @ lat).T * 3.0 + 0.3 * rng.standard_normal((nface, len(idx)))
        B[2 * nrun + nface:] = rng.standard_normal((2, len(idx)))
        Y = X @ B + 0.8 * rng.standard_normal((nrun * L, len(idx)))
        ins = []
        for r in range(nrun):
            vol = np.zeros((6, 6, 4, L), dtype=np.float32)
            flat = vol.reshape(-1, L, order="F")
            flat[idx, :] = Y[r * L:(r + 1) * L].T
            f = d / f"{lab}_r{r + 1}.nii.gz"
            write_nifti(f, flat.reshape((6, 6, 4, L), order="F"))
            ins.append(f)
        run([dec, "-force_TR", TR, "-input"] + ins + ["-polort", 1] + stims + ["-x1D", d / f"X.{lab}.xmat.1D", "-bucket", d / f"stats.{lab}",
                                                               "-overwrite", "-noFDR"])
        lines.append(f"{lab} ctl {d / ('X.' + lab + '.xmat.1D')} {d / ('stats.' + lab + '+orig')}")
    (d / "table.txt").write_text("\n".join(lines) + "\n")
    base = [prog, "-axis", "item", "-nfeatures", k, "-niter", 40, "-tol", 0, "-mask", d / "mask.nii.gz",
            "-dataTableFile", d / "table.txt", "-progress", "off", "-overwrite"]
    # the expanded table
    shown = run(base + ["-show_table"]).stdout
    rows = [ln.split() for ln in shown.splitlines() if ln.strip() and not ln.startswith("#")]
    hdr = rows[0]
    check("Xmat table: one item row per subject and stimulus column (all conditions: no Run column)",
          float(len(rows) - 1 != 4 * (nface + 2) or "Run" in hdr or hdr[-3:] != ["Item", "Condition", "InputFile"]), 0.0, "{:.0f}")
    shown_f = run(base + ["-xmat_stims", "face", "-show_table"]).stdout
    rows_f = [ln.split() for ln in shown_f.splitlines() if ln.strip() and not ln.startswith("#")]
    check("-xmat_stims face: the trials only, each in one run, so a Run column",
          float(len(rows_f) - 1 != 4 * nface or rows_f[0][-4:] != ["Item", "Condition", "Run", "InputFile"]), 0.0, "{:.0f}")
    hf = rows_f[0]
    ok_run = all(r[hf.index("Run")] == str(1 + int(r[hf.index("Item")].split("#")[1]) // len(face)) for r in rows_f[1:])
    check("each trial's Run is the run its regressor lives in", float(not ok_run), 0.0, "{:.0f}")
    # the same items by hand, selected by label: the same fit
    labs = {f"s{i + 1}": afni_labels(d / f"stats.s{i + 1}") for i in range(4)}
    # the same items by hand: AFNI's names quoted, unquoted, and the selector by label in quotes (shell style)
    man_q, man_u = ["Subj Group Item InputFile  # a comment after the header"], ["Subj Group Item InputFile"]
    for i in range(4):
        lab = f"s{i + 1}"
        for c in [f"face#{j}" for j in range(nface)] + ["house", "resp"]:
            col = c if "#" in c else c + "#0"
            bk = d / ("stats." + lab + "+orig")
            man_q.append(f'{lab} ctl "{c}" {bk}[{col}_Coef]')
            man_u.append(f"{lab} ctl {c} {bk}[{labs[lab].index(col + '_Coef')}]   # trailing comment")
    (d / "manual_q.txt").write_text("\n".join(man_q) + "\n")
    (d / "manual_u.txt").write_text("\n".join(man_u) + "\n")
    run(base + ["-prefix", d / "xm"])
    S0 = np.loadtxt(d / "xm.S.1D")
    for tag in ("q", "u"):
        i = base.index("-dataTableFile")
        run(base[:i] + base[i + 2:] + ["-dataTableFile", d / f"manual_{tag}.txt", "-prefix", d / f"man_{tag}"])
    check('a hand-written table with "face#0" quoted and stats[face#0_Coef] by label gives the same fit',
          float(np.abs(np.loadtxt(d / "man_q.S.1D") - S0).max()), 0.0, "{:.2e}")
    check("... and with face#0 unquoted (a '#' inside a word is text; at a word's start, a comment)",
          float(np.abs(np.loadtxt(d / "man_u.S.1D") - S0).max()), 0.0, "{:.2e}")
    (d / "shown.txt").write_text(shown)
    i = base.index("-dataTableFile")
    run(base[:i] + base[i + 2:] + ["-dataTableFile", d / "shown.txt", "-prefix", d / "shown"])
    check("-show_table output, read back as a table, gives the same fit", float(np.abs(np.loadtxt(d / "shown.S.1D") - S0).max()), 0.0, "{:.2e}")
    (d / "unclosed.txt").write_text('Subj Item InputFile\ns1 "face#0 x.nii\n')
    (d / "transposed.txt").write_text("Subj Item InputFile\ns1 a x.1D'\n")         # AFNI's transpose mark is text
    pr = run([prog, "-axis", "item", "-nfeatures", k, "-dataTableFile", d / "transposed.txt", "-show_table"], expect_ok=False)
    check("a quote inside a word is text (x.1D' reads as written)", float("x.1D'" not in pr.stdout), 0.0, "{:.0f}")
    pr = run([prog, "-axis", "item", "-nfeatures", k, "-dataTableFile", d / "unclosed.txt", "-show_table"], expect_ok=False)
    check("E: a table with an unclosed quote names the line", float(pr.returncode == 0 or "line 2 has a quote that is never closed" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")
    # and against numpy on the betas as 3dDeconvolve wrote them
    pre = []
    keys = s1d_keys(d / "xm.S.1D")
    for i in range(4):
        lab = f"s{i + 1}"
        bk = load_afni(d / f"stats.{lab}").reshape(-1, len(labs[lab]), order="F")[idx]
        cols = [labs[lab].index((kk if "#" in kk else kk + "#0") + "_Coef") for kk in keys]
        x = bk[:, cols].astype(np.float64)
        pre.append((x - x.mean(1, keepdims=True)) / x.std(1, keepdims=True))
    ref = reference_fit(cli, work, "xmat_items_ref", pre, k, 40, "prob")
    check("Xmat items: S vs the driver on the bucket's betas (z-scored over items)", relfro(np.loadtxt(d / "xm.S.1D").T, ref["s"]), 2e-4)
    # -cv run on the trials: the runs come from the Xmat
    p = run(base + ["-prefix", d / "cv", "-xmat_stims", "face", "-cv", "run"])
    head, body = read_scores(d / "cv.scores_folds.txt")
    check("-cv run on Xmat trials: three folds, the runs", float(sorted({r[2] for r in body}) != ["1", "2", "3"]), 0.0, "{:.0f}")
    acc = np.mean([num(r[5]) for r in body]); ch = np.mean([num(r[6]) for r in body])
    check("trial identification well above chance (sanity)", float(not acc > 3 * ch), 0.0, "{:.0f}")
    # the time axis: each subject's runs in one file, the Xmat column gives runs and, for -score_by, conditions
    tl = ["Subj Group Xmat InputFile"]
    for i in range(4):
        lab = f"s{i + 1}"
        imgs = [nib.load(str(d / f"{lab}_r{r + 1}.nii.gz")) for r in range(nrun)]
        f = d / f"errts.{lab}.nii.gz"
        nib.save(nib.Nifti1Image(np.concatenate([np.asarray(im.dataobj) for im in imgs], axis=3), imgs[0].affine), str(f))
        tl.append(f"{lab} ctl {d / ('X.' + lab + '.xmat.1D')} {f}")
    (d / "time.txt").write_text("\n".join(tl) + "\n")
    Xs = np.loadtxt(d / "X.s1.xmat.1D", comments="#")
    groups = {"face": range(2 * nrun, 2 * nrun + nface), "house": [2 * nrun + nface], "resp": [2 * nrun + nface + 1]}
    norm = np.array([np.abs(Xs[:, list(cs)]).max(1) / np.abs(Xs[:, list(cs)]).max() for cs in groups.values()])
    cond = np.where(norm.max(0) >= 0.5, norm.argmax(0) + 1, 0)            # 1 face, 2 house, 3 resp, 0 none
    np.savetxt(d / "cond.1D", cond, fmt="%d")
    tb = [prog, "-nfeatures", 3, "-niter", 30, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile", d / "time.txt",
          "-cv", "run", "-match_window", 3, "-progress", "off", "-overwrite"]
    run(tb + ["-prefix", d / "tx", "-score_by", "Condition"])
    run(tb + ["-prefix", d / "tl", "-score_by", "c", "-time_labels", "c", d / "cond.1D"])
    a_ = {(r_["Subj"], r_["Fold"], r_["Level"]): [r_[q] for q in ("nsamp", "isc", "match", "chance", "r2")]
          for r_ in read_levels(d / "tx.scores_folds.txt") if r_["Factor"] == "Condition"}
    names_ = {"face": "1", "house": "2", "resp": "3"}
    b_ = {(r_["Subj"], r_["Fold"], r_["Level"]): [r_[q] for q in ("nsamp", "isc", "match", "chance", "r2")]
          for r_ in read_levels(d / "tl.scores_folds.txt") if r_["Factor"] == "c"}
    same = len(a_) > 0 and all(b_.get((k_[0], k_[1], names_[k_[2]])) == v for k_, v in a_.items())
    check("-score_by Condition from the Xmat = -time_labels from the regressors (each TR: its strongest stimulus, at half its peak)",
          float(not same), 0.0, "{:.0f}")

    # refusals
    def fails(args, needle, label, table=None):
        i = base.index("-dataTableFile")
        pr = run(base[:i] + base[i + 2:] + ["-dataTableFile", table or (d / "table.txt")] + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    (d / "sel.txt").write_text((d / "table.txt").read_text().replace("stats.s1+orig", "stats.s1+orig[0..5]"))
    fails(["-prefix", d / "bad"], "without a selector", "E: an Xmat row's InputFile with a selector", d / "sel.txt")
    fails(["-prefix", d / "bad", "-xmat_stims", "nothing"], "is in the list 'nothing'", "E: -xmat_stims naming no stimulus", d / "table.txt")
    (d / "wrongb.txt").write_text((d / "table.txt").read_text().replace(str(d / "stats.s2+orig"), str(d / "s2_r1.nii.gz")))
    fails(["-prefix", d / "bad"], "has no volume labelled face#0_Coef", "E: InputFile that is not the Xmat's stats bucket", d / "wrongb.txt")


def case_score_time(prog, cli, work, rng, win=4, niter=40):
    """-score_by on the time axis with -time_labels: blocks of two labels and unlabelled gaps."""
    print("\n[-score_by on the time axis (-time_labels)]", flush=True)
    d = work / "score_time"
    _, subs, common = make_cv_world(rng, RUNS, nsub=6)
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    T = sum(RUNS)
    labs = np.array([(1 + (t // 12) % 2) if (t % 12) < 10 else 0 for t in range(T)])
    np.savetxt(d / "blk.1D", labs, fmt="%d")
    run([prog, "-prefix", d / "out", "-nfeatures", K, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile",
         d / "table.txt", "-cv", "run", "-match_window", win, "-score_by", "blk", "-time_labels", "blk", d / "blk.1D",
         "-progress", "off", "-overwrite"])
    pre = [preprocess(s_)[0] for s_ in subs]
    n = len(subs)
    names = [s_["label"] for s_ in subs]
    folds, c0 = [], 0
    for L in RUNS:
        folds.append(np.arange(c0, c0 + L)); c0 += L
    fold_names = list(dict.fromkeys(r_["Fold"] for r_ in read_levels(d / "out.scores_folds.txt")))
    want = {}
    for fi_, cols in enumerate(folds):
        r = reference_fit(cli, work, f"score_time_f{fi_}", [np.delete(z, cols, axis=1) for z in pre], K, niter, "prob")
        P = [r["w"][i].T @ pre[i][:, cols] for i in range(n)]
        for i in range(n):
            oth = [j for j in range(n) if j != i]
            ref_u = sum(P[j] for j in oth) / len(oth)
            prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(K)
            ref_r = np.linalg.solve(prec, sum(P[j] / r["rho2"][j] for j in oth))
            for lv in (1, 2):
                sub = labs[cols] == lv
                acc, ch = np_segment_match_obs(P[i], ref_u, win, sub)
                te, hv = pre[i][:, cols][:, sub], (r["w"][i] @ ref_r)[:, sub]
                sse = ((te - hv) ** 2).sum(); sst = ((te - te.mean(1, keepdims=True)) ** 2).sum()
                want[(names[i], fold_names[fi_], str(lv))] = [int(sub.sum()), np_isc(P[i][:, sub], ref_u[:, sub]), acc, ch, 1.0 - sse / sst]
    got = {(r_["Subj"], r_["Fold"], r_["Level"]): [num(r_["nsamp"]), num(r_["isc"]), num(r_["match"]), num(r_["chance"]), num(r_["r2"])]
           for r_ in read_levels(d / "out.scores_folds.txt") if r_["Factor"] == "blk"}
    e = np.array([[abs(got[k_][q] - want[k_][q]) for q in range(5)] for k_ in want])
    check("-time_labels: each level's nsamp (unlabelled time points in neither)", float(e[:, 0].max()), 0.0, "{:.0f}")
    check("-time_labels: isc within each level", float(e[:, 1].max()), 5e-4)
    check("-time_labels: segment matching among the level's own windows", float(e[:, 2].max()), 1e-6)
    check("-time_labels: ... and its chance", float(e[:, 3].max()), 1e-6)
    check("-time_labels: R^2 within each level", float(e[:, 4].max()), 5e-4)

    def fails(args, needle, label):
        pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-mask", d / "mask.nii.gz",
                  "-dataTableFile", d / "table.txt"] + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    fails(["-score_by", "blk", "-time_labels", "blk", d / "blk.1D"], "needs -cv", "E: -score_by without -cv")
    fails(["-cv", "run", "-time_labels", "blk", d / "blk.1D"], "only through -score_by", "E: -time_labels not named in -score_by")
    fails(["-cv", "run", "-score_by", "Group"], "already has it", "E: -score_by a per-subject column")
    fails(["-cv", "run", "-score_by", "Condition"], "Condition comes from an Xmat column", "E: -score_by Condition without an Xmat")
    np.savetxt(d / "short.1D", labs[:-3], fmt="%d")
    fails(["-cv", "run", "-score_by", "x", "-time_labels", "x", d / "short.1D"], "give one per input time point", "E: -time_labels of the wrong length")


def ref_scores_run(cli, work, tag, pre, members, folds, algo, k, niter, win):
    """-cv run among the given subjects only: fold refits of those subjects; reference = the others among them."""
    out = {}
    for fi_, (c0, L) in enumerate(folds):
        cols = np.arange(c0, c0 + L)
        r = reference_fit(cli, work, f"{tag}_f{fi_}", [np.delete(pre[i], cols, axis=1) for i in members], k, niter, algo,
                          extra=["--streams", ",".join(map(str, members))])          # each subject's whole-table start
        P = {i: r["w"][q].T @ pre[i][:, cols] for q, i in enumerate(members)}
        for q, i in enumerate(members):
            oth = [qq for qq in range(len(members)) if qq != q]
            ref_u = sum(P[members[qq]] for qq in oth) / len(oth)
            if algo == "prob":
                prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(k)
                ref_r = np.linalg.solve(prec, sum(P[members[qq]] / r["rho2"][qq] for qq in oth))
            else:
                ref_r = ref_u
            acc, ch = np_segment_match(P[i], ref_u, win)
            te = pre[i][:, cols]
            sse = ((te - r["w"][q] @ ref_r) ** 2).sum(); sst = ((te - te.mean(1, keepdims=True)) ** 2).sum()
            out[(i, fi_)] = [np_isc(P[i], ref_u), acc, ch, 1.0 - sse / sst]
    return out


def ref_scores_loso(cli, work, tag, pre, members, folds, algo, k, niter, win, others=()):
    """-cv loso among the given subjects (the full fit of them, then each left out, 5 warm steps); others are
    scored as new subjects in the full fit of members (W from their training columns)."""
    out = {}
    full = reference_fit(cli, work, tag + "_full", [pre[i] for i in members], k, niter, algo,
                         extra=["--streams", ",".join(map(str, members))])

    def score(i, W_of, S):
        for fi_, (c0, L) in enumerate(folds):
            cols = np.arange(c0, c0 + L)
            tr, te = np.delete(pre[i], cols, axis=1), pre[i][:, cols]
            W = np_polar(tr @ np.delete(S, cols, axis=1).T)
            Pm, s_te = W.T @ te, S[:, cols]
            acc, ch = np_segment_match(Pm, s_te, win)
            sse = ((te - W @ s_te) ** 2).sum(); sst = ((te - te.mean(1, keepdims=True)) ** 2).sum()
            out[(i, fi_)] = [np_isc(Pm, s_te), acc, ch, 1.0 - sse / sst]
    for q, i in enumerate(members):
        oth = [qq for qq in range(len(members)) if qq != q]
        dd = work / f"{tag}_drop{q}"
        dd.mkdir(parents=True, exist_ok=True)
        extra = []
        if algo == "prob":
            full["rho2"][oth].astype(np.float64).tofile(dd / "rho2.f64")
            full["sigma"].astype(np.float32).tofile(dd / "sigma.f32")
            extra = ["--init-rho2", str(dd / "rho2.f64"), "--init-sigma", str(dd / "sigma.f32")]
        r = run_ours(cli, work, f"{tag}_drop{q}", algo, [pre[members[qq]].astype(np.float32) for qq in oth], k, 5, False,
                     init_w=[full["w"][qq] for qq in oth], extra=extra)
        score(i, None, r["s"])
    for i in others:
        score(i, None, full["s"])
    return out


def case_cvfit(prog, cli, work, rng, mode, fit, algo, balance=0, win=5, niter=40):
    """-cv_fit within|reference:ctl and -balance, against numpy fits of exactly the subjects involved."""
    name = f"cvfit_{mode}_{fit.replace(':', '_')}_{algo}_b{balance}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    _, subs, common = make_cv_world(rng, RUNS, nsub=7)                   # ctl: 4 subjects, pat: 3
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    cmd = [prog, "-prefix", d / "out", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz",
           "-dataTableFile", d / "table.txt", "-cv", mode, "-match_window", win, "-cv_fit", fit, "-progress", "off", "-overwrite"]
    if balance:
        cmd += ["-balance", balance]
    run(cmd)
    pre = [preprocess(s_)[0] for s_ in subs]
    names = [s_["label"] for s_ in subs]
    grp = {g: [i for i, s_ in enumerate(subs) if s_["group"] == g] for g in ("ctl", "pat")}
    folds, c0 = [], 0
    for L in RUNS:
        folds.append((c0, L)); c0 += L
    fits = []                                            # (members, others) of every fit 3dSRM made
    if balance:
        for ln in (d / "out.balance.txt").read_text().splitlines():
            if ln.startswith("#") or ln.startswith("Fit"):
                continue
            mem = sorted(names.index(x) for x in ln.split("\t")[3].split(","))
            oth = [i for i in range(len(subs)) if i not in grp["ctl"]] if fit.startswith("reference") else []
            fits.append((mem, oth))
    elif fit == "within":
        fits = [(grp["ctl"], []), (grp["pat"], [])]
    else:
        fits = [(grp["ctl"], grp["pat"])]
    sums = {}
    for fi_, (mem, oth) in enumerate(fits):
        tag = f"{name}_fit{fi_}"
        sc = (ref_scores_run(cli, work, tag, pre, mem, folds, algo, K, niter, win) if mode == "run"
              else ref_scores_loso(cli, work, tag, pre, mem, folds, algo, K, niter, win, oth))
        for key, v in sc.items():
            sums.setdefault(key, []).append(v)
    want = {key: list(np.mean(np.array(v), axis=0)) for key, v in sums.items()}
    fold_names = list(dict.fromkeys(r_["Fold"] for r_ in read_levels(d / "out.scores_folds.txt")))
    got = {(names.index(r_["Subj"]), fold_names.index(r_["Fold"])): [num(r_["isc"]), num(r_["match"]), num(r_["chance"]), num(r_["r2"])]
           for r_ in read_levels(d / "out.scores_folds.txt") if r_["Factor"] == "all"}
    e = np.array([[abs(got[k_][q] - want[k_][q]) for q in range(4)] for k_ in want])
    what = f"-cv {mode} -cv_fit {fit}" + (f" -balance {balance}" if balance else "")
    in_fit = {i for mem, oth in fits for i in list(mem) + list(oth)}
    unscored = [i for i in range(len(subs)) if i not in in_fit]
    check(f"{what}: every subject in some fit is scored, the others are NA",
          float(len(want) != len(in_fit) * len(folds) or any(np.isfinite(got[(i, f_)][0]) for i in unscored for f_ in range(len(folds)))),
          0.0, "{:.0f}")
    check(f"{what}: isc vs numpy fits of the subjects involved", float(e[:, 0].max()), 5e-4)
    check(f"{what}: segment matching", float(e[:, 1].max()), 1e-6)
    check(f"{what}: R^2", float(e[:, 3].max()), 5e-4)
    if balance:
        nfit = len(fits)
        check(f"{what}: PREFIX.balance.txt lists the fits (the larger group subsampled {balance} times)",
              float(nfit != (balance if fit == "pooled" else balance + (1 if fit == "within" else 0))), 0.0, "{:.0f}")
        sizes = {len(m) for m, _ in fits}
        check(f"{what}: every subset the size of the smallest group" + (" from each group" if fit == "pooled" else ""),
              float(sizes != ({6} if fit == "pooled" else {3})), 0.0, "{:.0f}")


def case_cvfit_errors(prog, work, rng):
    print("\n[-cv_fit and -balance: refusals]", flush=True)
    d = work / "cvfit_err"
    _, subs, common = make_cv_world(rng, RUNS, nsub=5)
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])

    def fails(args, needle, label, table=None):
        pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-mask", d / "mask.nii.gz",
                  "-dataTableFile", table or (d / "table.txt")] + args, expect_ok=False)
        out = pr.stdout + pr.stderr
        check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
        if pr.returncode == 0 or needle not in out:
            print(out)
    fails(["-cv_fit", "within"], "need -cv run or -cv loso", "E: -cv_fit without -cv")
    fails(["-cv", "run", "-cv_fit", "reference:ctl"], "use -cv loso", "E: -cv_fit reference with -cv run")
    fails(["-cv", "loso", "-cv_fit", "reference:nobody"], "there is no group 'nobody'", "E: -cv_fit reference to an unknown group")
    fails(["-cv", "run", "-cv_fit", "sideways"], "must be pooled, within, or reference", "E: an unknown -cv_fit")
    fails(["-cv", "loso", "-cv_fit", "within"], "group pat has 2 subjects", "E: a group too small for -cv loso within")
    one = (d / "table.txt").read_text().replace(" pat ", " ctl ")
    (d / "onegroup.txt").write_text(one)
    fails(["-cv", "run", "-balance", 3], "at least two groups", "E: -balance with one group", d / "onegroup.txt")


def main_cvfit(args, cli, rng):
    for algo in ("prob", "det"):
        case_cvfit(args.prog, cli, args.workdir, rng, "run", "within", algo)
    case_cvfit(args.prog, cli, args.workdir, rng, "loso", "within", "prob")
    case_cvfit(args.prog, cli, args.workdir, rng, "loso", "reference:ctl", "prob")
    case_cvfit(args.prog, cli, args.workdir, rng, "loso", "reference:ctl", "det")
    case_cvfit(args.prog, cli, args.workdir, rng, "run", "within", "prob", balance=3)
    case_cvfit(args.prog, cli, args.workdir, rng, "run", "pooled", "prob", balance=3)
    case_cvfit(args.prog, cli, args.workdir, rng, "loso", "reference:ctl", "prob", balance=2)
    case_cvfit_errors(args.prog, args.workdir, rng)
    for algo in ("prob", "det"):
        case_reliability_split(args.prog, cli, args.workdir, rng, algo)
    case_repeated_stimulus(args.prog, cli, args.workdir, rng)
    case_nperm(args.prog, cli, args.workdir, rng)


def np_shared_similarity(s1, s2):
    """Mean squared cosine of the principal angles between the row spaces of two k x T responses."""
    q1, _ = np.linalg.qr(s1.T)
    q2, _ = np.linalg.qr(s2.T)
    c = np.linalg.svd(q1.T @ q2, compute_uv=False)
    return float(np.mean(np.clip(c, 0, 1) ** 2))


def case_reliability_split(prog, cli, work, rng, algo, niter=30, nsplit=4):
    """-reliability split: every split listed is refitted (two halves, warm-started from the fit to everyone)
    and the similarity of the two S recomputed."""
    name = f"rel_split_{algo}"
    print(f"\n[{name}]", flush=True)
    d = work / name
    _, subs, common = make_cv_world(rng, RUNS, nsub=9)                 # ctl 5, pat 4
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    run([prog, "-prefix", d / "out", "-nfeatures", K, "-algorithm", algo, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz",
         "-dataTableFile", d / "table.txt", "-reliability", "split", "-nsplit", nsplit, "-progress", "off", "-overwrite"])
    pre = [preprocess(s_)[0] for s_ in subs]
    names = [s_["label"] for s_ in subs]
    full = reference_fit(cli, work, name + "_full", pre, K, niter, algo)
    splits = read_levels(d / "out.reliability_splits.txt")
    worst, sims = 0.0, {}
    for q, sp in enumerate(splits):
        sim = []
        for h, half in enumerate((sp["HalfA"], sp["HalfB"])):
            idx = [names.index(x) for x in half.split(",")]
            dd = work / f"{name}_s{q}_{h}"
            dd.mkdir(parents=True, exist_ok=True)
            extra = []
            if algo == "prob":
                full["rho2"][idx].astype(np.float64).tofile(dd / "rho2.f64")
                full["sigma"].astype(np.float32).tofile(dd / "sigma.f32")
                extra = ["--init-rho2", str(dd / "rho2.f64"), "--init-sigma", str(dd / "sigma.f32")]
            r = run_ours(cli, work, f"{name}_s{q}_{h}", algo, [pre[i].astype(np.float32) for i in idx], K, niter, False,
                         init_w=[full["w"][i] for i in idx], extra=extra)
            sim.append(r["s"])
        v = np_shared_similarity(sim[0], sim[1])
        worst = max(worst, abs(v - float(sp["similarity"])))
        sims.setdefault(sp["Group"], []).append(v)
        halves = set(sp["HalfA"].split(",")) | set(sp["HalfB"].split(","))
        same_group = all(subs[names.index(x)]["group"] == sp["Group"] for x in halves)
        if not same_group or len(halves) != sum(1 for s_ in subs if s_["group"] == sp["Group"]):
            worst = 1.0
    check(f"-reliability split ({algo}): each split's similarity vs numpy refits of the two halves", worst, 1e-4)
    summ = {r_["Group"]: r_ for r_ in read_levels(d / "out.reliability.txt")}
    ok = all(abs(float(summ[g]["mean"]) - np.mean(v)) < 1e-5 and abs(float(summ[g]["p2.5"]) - np.percentile(v, 2.5)) < 1e-5
             and abs(float(summ[g]["p97.5"]) - np.percentile(v, 97.5)) < 1e-5 and int(summ[g]["Nsplit"]) == nsplit for g, v in sims.items())
    check("reliability.txt: each group's mean and 2.5/97.5 percentiles over its splits", float(not ok or len(sims) != 2), 0.0, "{:.0f}")


def np_split_rel(xa, xb):
    """Split-half reliability as the help defines it: r_pattern, the Fisher mean over samples of the correlation
    across features; r_series, over features of the correlation across samples (3 samples at least); each corrected
    to the whole by Spearman-Brown."""
    def fisher(r):
        r = np.clip(np.array([x for x in r if np.isfinite(x)]), -0.9999999, 0.9999999)
        return float(np.tanh(np.arctanh(r).mean())) if r.size else float("nan")

    def corr(a, b):
        a, b = a - a.mean(), b - b.mean()
        den = np.sqrt((a * a).sum() * (b * b).sum())
        return float((a * b).sum() / den) if den > 0 else float("nan")
    n = xa.shape[1]
    rp = fisher([corr(xa[:, j], xb[:, j]) for j in range(n)]) if n >= 2 else float("nan")
    rs = fisher([corr(xa[i], xb[i]) for i in range(xa.shape[0])]) if n >= 3 else float("nan")
    sb = lambda r: 2 * r / (1 + r)
    return sb(rp), sb(rs)


def case_repeated_stimulus(prog, cli, work, rng, nrep=3, L=50, niter=40):
    """-repeated_stimulus: the runs are averaged for the fit; odd runs against even runs give -reliability within."""
    print("\n[-repeated_stimulus and -reliability within (time axis)]", flush=True)
    d = work / "repeated"
    global RUNS
    old = RUNS
    RUNS = [L] * nrep
    try:
        s_one = rng.standard_normal((K, L)) * np.array([[1.6], [1.1], [0.7]])
        subs, mask = [], np.zeros((NX, NY, NZ), dtype=bool)
        mask[1:9, 1:9, 1:5] = True
        idx = np.flatnonzero(mask.ravel(order="F"))
        for i in range(6):
            q, _ = np.linalg.qr(rng.standard_normal((len(idx), K)))
            x = np.concatenate([q @ s_one + (0.8 + 0.1 * i) * rng.standard_normal((len(idx), L)) for _ in range(nrep)], axis=1)
            x = x * rng.uniform(0.5, 3.0, (len(idx), 1)) + rng.uniform(50, 500, (len(idx), 1))
            subs.append({"label": f"sub{i + 1:02d}", "group": "ctl" if i % 2 == 0 else "pat", "mask": mask, "idx": idx, "x": x})
        rows = write_world(d, subs)
        write_table(d / "table.txt", rows, False)
        write_nifti(d / "mask.nii.gz", mask.astype(np.float32)[..., None])
        p = run([prog, "-prefix", d / "out", "-nfeatures", K, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile",
                 d / "table.txt", "-repeated_stimulus", "-reliability", "within", "-cv", "run", "-match_window", 4,
                 "-progress", "off", "-overwrite"])
        pre = [preprocess(s_) for s_ in subs]                      # z-scored within each run, constant features dropped
        avg = [z.reshape(z.shape[0], nrep, L).mean(1) for z, _, _ in pre]
        ref = reference_fit(cli, work, "repeated_ref", avg, K, niter, "prob")
        check("-repeated_stimulus: S (one run, averaged) vs the driver on the averaged runs",
              relfro(np.loadtxt(d / "out.S.1D").T, ref["s"]), 2e-4)
        check("-repeated_stimulus: the log says the runs are averaged", float("are averaged for the fit" not in p.stdout + p.stderr), 0.0, "{:.0f}")
        rows_s = {r_["Subj"]: r_ for r_ in read_levels(d / "out.scores.txt") if r_["Factor"] == "all"}
        worst = 0.0
        for i, s_ in enumerate(subs):
            z = pre[i][0].reshape(pre[i][0].shape[0], nrep, L)
            xa, xb = z[:, 0::2].mean(1), z[:, 1::2].mean(1)
            rp, rs = np_split_rel(xa, xb)
            r_ = rows_s[s_["label"]]
            ratio_err = abs(float(r_["rel_ratio"]) - float(r_["isc"]) / rp) if rp > 0 else float(r_["rel_ratio"] != "NA")
            worst = max(worst, abs(float(r_["rel_pattern"]) - rp), abs(float(r_["rel_series"]) - rs), ratio_err)
        check("-reliability within (odd runs vs even runs, Spearman-Brown) and rel_ratio = isc / rel_pattern", worst, 1e-5)

        def fails(args, needle, label):
            pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-mask", d / "mask.nii.gz",
                      "-dataTableFile", d / "table.txt"] + args, expect_ok=False)
            out = pr.stdout + pr.stderr
            check(label, float(pr.returncode == 0 or needle not in out), 0.0, "{:.0f}")
            if pr.returncode == 0 or needle not in out:
                print(out)
        fails(["-reliability", "within"], "needs -cv run or -cv loso", "E: -reliability within without -cv")
        fails(["-reliability", "within", "-cv", "run"], "needs repeats: -repeated_stimulus", "E: -reliability within on time data without repeats")
        fails(["-repeated_stimulus", "-cv", "run", "-score_by", "Run"], "every repeat must give a time point the same level",
              "E: -repeated_stimulus with -score_by Run (the runs are averaged)")

        # -score_by with -repeated_stimulus: the same as fitting the averaged runs given as one run
        lab = np.where((np.arange(L) // 10) % 2 == 0, 1, 2)
        np.savetxt(d / "cond_L.1D", lab, fmt="%d")
        np.savetxt(d / "cond_full.1D", np.tile(lab, nrep), fmt="%d")
        bad_lab = np.tile(lab, nrep); bad_lab[L + 3] = 3 - bad_lab[L + 3]
        np.savetxt(d / "cond_bad.1D", bad_lab, fmt="%d")
        cv = ["-cv", "run", "-cv_blocks", 2, "-match_window", 4]
        run([prog, "-prefix", d / "sb", "-nfeatures", K, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile",
             d / "table.txt", "-repeated_stimulus", "-time_labels", "cond", d / "cond_L.1D", "-score_by", "cond",
             "-progress", "off", "-overwrite"] + cv)
        run([prog, "-prefix", d / "sb_full", "-nfeatures", K, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile",
             d / "table.txt", "-repeated_stimulus", "-time_labels", "cond", d / "cond_full.1D", "-score_by", "cond",
             "-progress", "off", "-overwrite"] + cv)
        def lev(path):
            return {(r_["Subj"], r_["Fold"], r_["Level"]): [num(r_[c]) for c in ("nsamp", "isc", "match", "chance", "r2")]
                    for r_ in read_levels(path) if r_["Factor"] == "cond"}
        a, b_ = lev(d / "sb.scores_folds.txt"), lev(d / "sb_full.scores_folds.txt")
        want_l, n_ = {}, len(subs)
        for fi_, cols in enumerate([np.arange(0, L // 2), np.arange(L // 2, L)]):      # -cv run, -cv_blocks 2: two halves
            r = reference_fit(cli, work, f"repeated_sb_f{fi_}", [np.delete(a_, cols, axis=1) for a_ in avg], K, niter, "prob")
            P = [r["w"][i].T @ avg[i][:, cols] for i in range(n_)]
            for i, s_ in enumerate(subs):
                oth = [j for j in range(n_) if j != i]
                ref_u = sum(P[j] for j in oth) / len(oth)
                prec = np.linalg.inv(r["sigma"]) + (1.0 / r["rho2"][oth]).sum() * np.eye(K)
                ref_r = np.linalg.solve(prec, sum(P[j] / r["rho2"][j] for j in oth))
                for lv in (1, 2):
                    m_ = lab[cols] == lv
                    acc, ch = np_segment_match_obs(P[i], ref_u, 4, m_)
                    te, hv = avg[i][:, cols][:, m_], (r["w"][i] @ ref_r)[:, m_]
                    sse = ((te - hv) ** 2).sum(); sst = ((te - te.mean(1, keepdims=True)) ** 2).sum()
                    want_l[(s_["label"], f"block{fi_ + 1}", str(lv))] = [int(m_.sum()), np_isc(P[i][:, m_], ref_u[:, m_]), acc, ch, 1.0 - sse / sst]
        e = np.array([[abs(a[k_][q] - want_l[k_][q]) for q in range(5)] for k_ in want_l])
        check("-repeated_stimulus -score_by: each level's nsamp", float(e[:, 0].max()), 0.0, "{:.0f}")
        check("-repeated_stimulus -score_by: per-level isc vs the driver's fold fits of the averaged runs", float(e[:, 1].max()), 5e-4)
        check("-repeated_stimulus -score_by: per-level matching (windows inside the level) and chance", float(e[:, 2:4].max()), 1e-6)
        check("-repeated_stimulus -score_by: per-level R^2", float(e[:, 4].max()), 5e-4)
        check("... and -time_labels once per stimulus time point = once per input time point",
              float(a != b_ or len(a) != len(want_l)), 0.0, "{:.0f}")
        fails(["-repeated_stimulus", "-time_labels", "cond", d / "cond_bad.1D", "-score_by", "cond"] + cv,
              "time point 4 of the stimulus is", "E: -repeated_stimulus with labels that differ between repeats")

        # -cenmode missing with -repeated_stimulus: each time point averaged over the runs observed there
        miss = []
        for i, s_ in enumerate(subs):
            m = rng.uniform(size=nrep * L) < 0.1
            m[[4, L + 4, 2 * L + 4]] = (i == 0)              # subject 1 misses stimulus time point 5 in every run
            miss.append(m)
            np.savetxt(d / f"{s_['label']}_cens.1D", (~m).astype(int), fmt="%d")
        (d / "miss.txt").write_text("Subj Group Run Censor InputFile\n" + "\n".join(
            f"{s_} {g} {r} {d / (s_ + '_cens.1D')} {f}" for s_, g, r, _, f in rows) + "\n")
        p = run([prog, "-prefix", d / "rm", "-nfeatures", K, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile",
                 d / "miss.txt", "-repeated_stimulus", "-cenmode", "missing", "-reliability", "within", "-cv", "run",
                 "-match_window", 4, "-progress", "off", "-overwrite"])
        avg_m, obs_m, rel_m = [], [], []
        for i, s_ in enumerate(subs):
            z = preprocess_obs(s_["x"], miss[i], RUNS)
            zr = z.reshape(z.shape[0], nrep, L)
            o = (~miss[i]).reshape(nrep, L)
            cnt = o.sum(0)
            avg_m.append(np.where(cnt > 0, (zr * o[None]).sum(1) / np.maximum(cnt, 1), 0.0))
            obs_m.append(cnt > 0)
            oa, ob = o[0::2], o[1::2]
            na, nb = oa.sum(0), ob.sum(0)
            both = (na > 0) & (nb > 0)
            xa = (zr[:, 0::2] * oa[None]).sum(1)[:, both] / na[both]
            xb = (zr[:, 1::2] * ob[None]).sum(1)[:, both] / nb[both]
            rel_m.append(np_split_rel(xa, xb))
        ref = run_ours(cli, work, "repeated_miss_ref", "prob", [a_.astype(np.float32) for a_ in avg_m], K, niter, False,
                       extra=["--canon"] + obs_extra(d, "rm", obs_m))
        check("-repeated_stimulus -cenmode missing: S vs the driver on observed-only averages", relfro(np.loadtxt(d / "rm.S.1D").T, ref["s"]), 2e-4)
        nstill = sum(int((~o_).any()) for o_ in obs_m)
        check("... the log says how many subjects stay missing (missing in every run)",
              float((f"{nstill} subject is still" if nstill == 1 else f"{nstill} subjects are still") not in p.stdout + p.stderr), 0.0, "{:.0f}")
        rows_m = {r_["Subj"]: r_ for r_ in read_levels(d / "rm.scores.txt") if r_["Factor"] == "all"}
        worst = max(max(abs(float(rows_m[s_["label"]]["rel_pattern"]) - rel_m[i][0]), abs(float(rows_m[s_["label"]]["rel_series"]) - rel_m[i][1]))
                    for i, s_ in enumerate(subs))
        check("... -reliability within over the time points both halves observed", worst, 1e-5)
        fails(["-nsplit", 5], "goes with -reliability split", "E: -nsplit without -reliability split")
        fails(["-reliability", "sideways"], "is not within or split", "E: an unknown -reliability")
    finally:
        RUNS = old


def np_welch(y, g):
    a, b = y[(g == 0) & np.isfinite(y)], y[(g == 1) & np.isfinite(y)]
    if len(a) < 2 or len(b) < 2:
        return 0.0, float("nan")
    se = np.sqrt(a.var(ddof=1) / len(a) + b.var(ddof=1) / len(b))
    return (float((a.mean() - b.mean()) / se) if se > 0 else 0.0), float(a.mean() - b.mean())


def np_onesample(y):
    y = y[np.isfinite(y)]
    if len(y) < 2:
        return 0.0, float("nan")
    se = y.std(ddof=1) / np.sqrt(len(y))
    return (float(y.mean() / se) if se > 0 else 0.0), float(y.mean())


def np_perm_family(Y, stat, relabelings):
    """Y: subjects x elements. Every relabeling (a permutation and signs) of the subjects; p = share of relabelings at
    least as extreme (the identity among them), p_fwe from the maximum over the elements."""
    obs = np.array([stat(Y[:, e])[0] for e in range(Y.shape[1])])
    cnt, maxes = np.zeros(Y.shape[1]), []
    for perm, sign in relabelings:
        t = np.abs(np.array([stat(Y[perm, e] * sign)[0] for e in range(Y.shape[1])], dtype=np.float32))
        cnt += t >= np.abs(obs).astype(np.float32)
        maxes.append(t.max())
    maxes = np.array(maxes)
    p = cnt / len(relabelings)
    pf = np.array([(maxes >= np.float32(abs(o))).mean() for o in obs])
    return obs, p, pf


def case_nperm(prog, cli, work, rng, win=4, niter=30):
    """-nperm: the group, level and interaction tests, against every relabeling enumerated in numpy (6 subjects:
    720 permutations, 64 sign patterns; 3dSRM enumerates them too, -nperm being larger)."""
    print("\n[-nperm permutation tests]", flush=True)
    import itertools
    d = work / "nperm"
    _, subs, common = make_cv_world(rng, RUNS, nsub=6)
    rows = write_world(d, subs)
    write_table(d / "table.txt", rows, False)
    write_nifti(d / "mask.nii.gz", common.astype(np.float32)[..., None])
    T = sum(RUNS)
    labs = np.array([(1 + (t // 12) % 2) if (t % 12) < 10 else 0 for t in range(T)])
    np.savetxt(d / "blk.1D", labs, fmt="%d")
    run([prog, "-prefix", d / "out", "-nfeatures", K, "-niter", niter, "-tol", 0, "-mask", d / "mask.nii.gz", "-dataTableFile",
         d / "table.txt", "-cv", "run", "-match_window", win, "-score_by", "blk", "-time_labels", "blk", d / "blk.1D",
         "-nperm", 1000, "-progress", "off", "-overwrite"])
    sc = read_levels(d / "out.scores.txt")
    names = [s_["label"] for s_ in subs]
    g = np.array([0 if s_["group"] == "ctl" else 1 for s_ in subs])
    mets = ["isc", "match", "r2"]

    def val(subj, level, met):
        r_ = next(x for x in sc if x["Subj"] == subj and x["Level"] == level)
        return num(r_[met])
    tests = read_levels(d / "out.tests.txt")
    perms = [(np.array(p), np.ones(6)) for p in itertools.permutations(range(6))]
    flips = [(np.arange(6), np.array(s)) for s in itertools.product((1.0, -1.0), repeat=6)]
    worst_t, worst_p, nchk = 0.0, 0.0, 0
    # groups: every score, overall and per level
    els = [(lv, m) for lv in ("all", "1", "2") for m in mets]
    Y = np.array([[val(nm, lv, m) for lv, m in els] for nm in names])
    obs, p, pf = np_perm_family(Y, lambda y: np_welch(y, g), perms)
    for e, (lv, m) in enumerate(els):
        nmx = m if lv == "all" else f"{m}[blk={lv}]"
        r_ = next(x for x in tests if x["Test"] == "group" and x["Score"] == nmx)
        worst_t = max(worst_t, abs(float(r_["t"]) - obs[e]) / max(1.0, abs(obs[e])))
        worst_p = max(worst_p, abs(float(r_["p"]) - p[e]), abs(float(r_["p_fwe"]) - pf[e]))
        nchk += int(r_["nperm"]) == 720 and r_["exact"] == "1"
    # levels: within-subject differences, sign flips; and the interaction, group labels over the differences
    D = np.array([[val(nm, "1", m) - val(nm, "2", m) for m in mets] for nm in names])
    for test, stat, rel in (("level", np_onesample, flips), ("interaction", lambda y: np_welch(y, g), perms)):
        obs, p, pf = np_perm_family(D, stat, rel)
        for e, m in enumerate(mets):
            r_ = next(x for x in tests if x["Test"] == test and x["Score"] == m)
            worst_t = max(worst_t, abs(float(r_["t"]) - obs[e]) / max(1.0, abs(obs[e])))
            worst_p = max(worst_p, abs(float(r_["p"]) - p[e]), abs(float(r_["p_fwe"]) - pf[e]))
            nchk += int(r_["nperm"]) == len(rel) and r_["exact"] == "1"
    check("-nperm: t of the group, level and interaction tests (Welch / one-sample) vs numpy", worst_t, 1e-4)
    check("-nperm: p and family-wise p vs every relabeling enumerated in numpy", worst_p, 1e-6)
    check("-nperm: enumerated exactly (720 permutations, 64 sign patterns)", float(nchk != len(els) + 2 * len(mets)), 0.0, "{:.0f}")
    pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-mask", d / "mask.nii.gz",
              "-dataTableFile", d / "table.txt", "-cv", "run", "-nperm", 10], expect_ok=False)
    check("E: -nperm below 20", float(pr.returncode == 0 or "at least 20" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")
    pr = run([prog, "-progress", "off", "-prefix", d / "bad", "-overwrite", "-nfeatures", K, "-mask", d / "mask.nii.gz",
              "-dataTableFile", d / "table.txt", "-nperm", 100], expect_ok=False)
    check("E: -nperm without -cv", float(pr.returncode == 0 or "needs -cv" not in pr.stdout + pr.stderr), 0.0, "{:.0f}")


def main_items(args, cli, rng):
    case_time_zscore_all(args.prog, cli, args.workdir, rng)
    for algo in ("prob", "det"):
        case_items(args.prog, cli, args.workdir, rng, algo)
    case_items(args.prog, cli, args.workdir, rng, "prob", zmode="run")
    case_items(args.prog, cli, args.workdir, rng, "prob", zmode="none")
    case_items(args.prog, cli, args.workdir, rng, "prob", use_runs=False)
    for mode in ("run", "loso"):
        for algo in ("prob", "det"):
            case_items_cv(args.prog, cli, args.workdir, rng, mode, algo, "run", score_by=(mode == "run"))
    case_items_cv(args.prog, cli, args.workdir, rng, "run", "prob", "category")
    case_xmat_items(args.prog, cli, args.workdir, rng)
    case_score_time(args.prog, cli, args.workdir, rng)
    case_items_cv(args.prog, cli, args.workdir, rng, "loso", "prob", "own")
    case_items_cv(args.prog, cli, args.workdir, rng, "run", "prob", "own")
    case_items_cv(args.prog, cli, args.workdir, rng, "run", "det", "own")


def main_cv(args, cli, rng):
    for mode in ("run", "loso"):
        for algo in ("prob", "det"):
            case_cv(args.prog, cli, args.workdir, rng, mode, algo, [60, 50, 70])
    case_cv(args.prog, cli, args.workdir, rng, "run", "prob", [140], label="_blocks")
    case_nfeat_scan(args.prog, cli, args.workdir, rng)
    for algo in ("prob", "det"):
        case_cv_adapt(args.prog, cli, args.workdir, rng, algo)
    case_compress(args.prog, cli, args.workdir, rng)


def main_stage3(args, cli, rng):
    for algo in ("prob", "det"):
        case_model_in(args.prog, cli, args.workdir, rng, algo)
    case_apply_W(args.prog, cli, args.workdir, rng, "run")
    case_apply_W(args.prog, cli, args.workdir, rng, "none")
    case_1D_apply_W(args.prog, cli, args.workdir, rng)
    for algo in ("prob", "det"):
        case_items_model_in(args.prog, cli, args.workdir, rng, algo)
    case_items_apply_W(args.prog, cli, args.workdir, rng)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--prog", type=Path, default=REPO / "build" / "targets_built" / "3dSRM")
    ap.add_argument("--workdir", type=Path, default=Path("check_3dsrm"))
    ap.add_argument("--srm-cli", help="a built srm_cli; compiled from the repo if omitted")
    ap.add_argument("--only-stage3", action="store_true", help="only the -model_in, -update_S and -apply_W cases")
    ap.add_argument("--only-items", action="store_true", help="only the -axis item cases")
    ap.add_argument("--only-concat", action="store_true", help="only the -concat / Xmat cases")
    ap.add_argument("--only-missing", action="store_true", help="only the missing-data cases")
    ap.add_argument("--only-xmat", action="store_true", help="only the Xmat item case")
    ap.add_argument("--only-cvfit", action="store_true", help="only the -cv_fit / -balance cases")
    ap.add_argument("--only-adapt", action="store_true", help="only the -cv_adapt cases")
    ap.add_argument("--only-repeat", action="store_true", help="only the -repeated_stimulus case")
    ap.add_argument("--only-compress", action="store_true", help="only the -compress cases")
    ap.add_argument("--require-deps", action="store_true", help="fail, not skip, when numpy or nibabel is missing")
    args = ap.parse_args()
    args.workdir.mkdir(parents=True, exist_ok=True)
    cli = build_cli(args.workdir, args.srm_cli)
    rng = np.random.default_rng(12)
    if args.only_compress:
        case_compress(args.prog, cli, args.workdir, rng)
    elif args.only_repeat:
        case_repeated_stimulus(args.prog, cli, args.workdir, rng)
    elif args.only_adapt:
        for algo in ("prob", "det"):
            case_cv_adapt(args.prog, cli, args.workdir, rng, algo)
    elif args.only_cvfit:
        main_cvfit(args, cli, rng)
    elif args.only_xmat:
        case_xmat_items(args.prog, cli, args.workdir, rng)
        case_score_time(args.prog, cli, args.workdir, rng)
    elif args.only_missing:
        case_missing_lib(cli, args.workdir, rng)
        main_missing(args, cli, rng)
    elif args.only_concat:
        case_concat(args.prog, cli, args.workdir, rng)
        case_censor(args.prog, cli, args.workdir, rng)
    elif args.only_items:
        main_items(args, cli, rng)
    elif args.only_stage3:
        main_stage3(args, cli, rng)
    else:
        case_basic(args.prog, cli, args.workdir, rng, with_mask_col=False, zcensor=False, algo="prob")
        case_basic(args.prog, cli, args.workdir, rng, with_mask_col=True, zcensor=True, algo="det")
        case_1D_and_errors(args.prog, cli, args.workdir, rng)
        main_items(args, cli, rng)
        case_concat(args.prog, cli, args.workdir, rng)
        case_censor(args.prog, cli, args.workdir, rng)
        case_missing_lib(cli, args.workdir, rng)
        main_missing(args, cli, rng)
        main_cv(args, cli, rng)
        main_cvfit(args, cli, rng)
        main_stage3(args, cli, rng)
    print()
    if failures:
        print(f"FAILED ({len(failures)}): " + "; ".join(failures))
        sys.exit(1)
    print("all checks passed")


if __name__ == "__main__":
    main()
