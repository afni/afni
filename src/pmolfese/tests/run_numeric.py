#!/usr/bin/env python3
"""Automated numeric regression runner for 3dRSA.

Generates small planted/null fixtures, runs the important modes, and checks the
results either against an independently computed numpy reference (the observed
statistics, which do NOT depend on the permutation and so must match exactly) or
against the calibration/monotonicity properties the permutation machinery must
satisfy.  Among its targeted regressions for audit and follow-on fixes are:

  1. degenerate-regression FWE null      (constant ROIs must not corrupt max-null)
  2. distance-aware LOO weighting         (-neural_metric euclid must not invert)
  3. no-permutation z typing              (-nperm 0 IS-RSA writes an untyped _FZ)
  4. -block rejected under classic RSA    (sign-flip has no exchangeability blocks)
  5. multi-model dataset searchlights     (streaming diagnostics must not use atlas data)
  6. dataset-model save-RDM hints         (must not advertise an unwritten fixed RDM)
  7. subject-bootstrap confidence bounds  (independent classic/IS-RSA references)
  8. condition-bootstrap confidence bounds (synchronized/grouped classic references)
 9. circular-shift IS-RSA null             (independent offsets/p/FWE reference)
 10. searchlight memory preflight            (limit refusal and explicit override)
 11. fixed-model Mantel cache                 (atlas equivalence and OMP identity)
 12. per-location model contrasts             (paired exact-null/map/OMP reference)
 13. contrast subject-bootstrap bounds        (IS/classic/map/OMP references)
 14. commonality subject-bootstrap bounds      (component/map/OMP references)
 15. commonality partial-R2 / reduced nulls     (value/exhaustive-p/FWE/map/OMP)
 16. pattern reliability split contract          (rejects an invalid flattened split)
 17. duplicate-column LOO families               (one computation/FWE family)
 18. longest-prefix model contrasts              (hyphenated-name disambiguation)
 19. typed pattern extraction                     (mixed scalar datums equal float)
 20. ordinary classic-RSA searchlight              (same-data atlas/map/reference)
 21. second-order IS-RSA                            (ordinary/crossnobis outer RDMs)
 22. time-resolved model-series fusion              (joint time x space FDR/FWE)
 23. runwise crossnobis noise ceilings               (LOO/upper/map/whitening/OMP)
 24. classic-RSA commonality null                     (condition FL/exhaustive/map/OMP)
 25. circular-shift relative-lag engine                (all neural metrics/provenance)
 26. circular-shift contrast/regression extensions     (reference/FWE/map/OMP)
 27. ROI phase-randomization null                         (spectrum/stat/FWE/OMP)
 28. model-aware/multivariate LOO                         (exact/map/FWE/OMP)
 29. nested fitted component models                         (classic/IS/map/OMP)
 30. paired held-out fitted-model contrasts                   (effect/FWE/map/OMP)
 31. three-predictor commonality                       (IS/classic/null/bootstrap/map)
 32. dual subject x condition bootstrap              (formula/group/joint/contrast/map)
 33. unbalanced runwise condition mapping              (missing/repeated/pair-valid/map)
 34. fitted-model subject x condition CV               (held dyads/ref/null/map/contracts)
 35. covariance-whitened RDM comparison                (WUC/corr_cov/ref/map/contracts)
 36. expected Spearman rho-a                             (closed form/ties/cache/contracts)
 37. phase-randomization searchlights                    (local spectra/ref/FWE/OMP)
 38. classic fixed-effects condition null       (exact/reference/single-sub/map/OMP)
 39. condition-pattern re-meaning              (angle refs/invariance/map/contracts)
 40. seed representational connectivity       (IS/classic/null/overlap/map/OMP)
 41. trial-beta descriptors                    (nesting/aggregation/xnobis/map/OMP)
 42. completed subject-bootstrap extensions    (strata/fixed-OOF LOO/map/OMP)
 43. staged progress reporting                  (line/bar/off/quiet contracts)
 44. repeated-run conditional regression       (beta/partial/contrast/null/OMP)

Usage:
    python run_numeric.py [--bin PATH] [--threads N] [--work DIR]
                          [--require-deps] [-v]

Exit status is non-zero if any check fails.  Requires numpy, scipy, and nibabel
for the fixtures and independent references (the run is SKIPPED, not failed, if
any is missing, unless --require-deps is given).  CTest uses --require-deps so a
missing test environment cannot silently pass CI.  AFNI is not required --
typed-brick checks read the plain-text +orig.HEAD directly.
"""
import argparse, glob, itertools, os, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))

# ---- planted-effect fixture constants (kept in step with mktest.py so the
#      golden observed r below stays reproducible) ------------------------------
SEED = 20260726
NSUB, NT = 20, 60
NX, NY, NZ = 8, 8, 4
NVOX = NX * NY * NZ
GOLDEN_R1 = 0.997379          # planted-ROI spearman Mantel r, corr neural metric


# ============================================================================
# small helpers
# ============================================================================
class Skip(Exception):
    pass


def sh(cmd, cwd=None, check=False):
    """Run a command, capturing output; never raises on non-zero unless check."""
    p = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE,
                       stderr=subprocess.STDOUT, universal_newlines=True)
    if check and p.returncode != 0:
        raise RuntimeError("command failed: %s\n%s" % (" ".join(cmd), p.stdout))
    return p.returncode, p.stdout


def read_table(path, model):
    """Parse a PPP.rsa.1D into a list of {colname: value} dicts.

    Columns are looked up by exact name (the header carries '<model>_r' etc.), so
    the parser is robust to added columns like '_pfwe' / '_looPfwe'."""
    header, rows = None, []
    with open(path) as f:
        for line in f:
            if line.startswith("#ROI"):
                header = line[1:].split()
            elif line.startswith("#") or not line.strip():
                continue
            elif header is not None:
                rows.append(line.split())
    if header is None:
        raise RuntimeError("no header in %s" % path)
    out = []
    for r in rows:
        d = {}
        for name, tok in zip(header, r):
            try:
                d[name] = float(tok)
            except ValueError:
                d[name] = tok
        out.append(d)
    return header, out


def head_attr_present(head_path, attr):
    """True if a named attribute block exists in an AFNI .HEAD text file."""
    with open(head_path) as f:
        return re.search(r"name\s*=\s*%s\b" % re.escape(attr), f.read()) is not None


def head_brick_labs(head_path):
    """Return the ~-separated BRICK_LABS as a list.  The AFNI .HEAD lays this out
    as 'name = BRICK_LABS' / 'count = N' / a quoted '<lab>~<lab>~' value line."""
    with open(head_path) as f:
        lines = f.read().splitlines()
    for i, l in enumerate(lines):
        if re.search(r"name\s*=\s*BRICK_LABS\b", l):
            for j in range(i + 1, min(i + 5, len(lines))):
                s = lines[j].strip()
                if s.startswith("'"):
                    s = s.strip("'")
                    return [x for x in s.split("~") if x]
    return []


def spearman_tri(A, B):
    """Spearman correlation of the strict upper triangles of two square mats."""
    import numpy as np
    iu = np.triu_indices(A.shape[0], 1)
    a, b = A[iu], B[iu]
    # average ranks with ties, to match scipy.rankdata / THD_rank_avg
    def rankavg(x):
        _, inv, cnt = np.unique(x, return_inverse=True, return_counts=True)
        csum = np.cumsum(cnt)
        start = csum - cnt
        meanrank = (start + csum + 1) / 2.0
        return meanrank[inv]
    ra, rb = rankavg(a), rankavg(b)
    ra -= ra.mean(); rb -= rb.mean()
    d = (np.sqrt((ra**2).sum()) * np.sqrt((rb**2).sum()))
    return float((ra * rb).sum() / d) if d > 0 else 0.0


def rhoa_vec(a, b):
    """Expected Spearman correlation under independent random tie breaking.

    This is the closed-form rho-a reference from Schuett et al. (2023): use
    average ranks for the expected random ranks, but retain the fixed untied
    rank variance m(m^2-1)/12 in the denominator.
    """
    import numpy as np
    from scipy.stats import rankdata
    a = rankdata(np.asarray(a, float), method="average")
    b = rankdata(np.asarray(b, float), method="average")
    m = a.size
    if m < 2:
        return 0.0
    mid = (m + 1.0) / 2.0
    return float(12.0 * np.dot(a - mid, b - mid) / (m ** 3 - m))


def bootstrap_indices(nobs, nboot, seed):
    """Independent mirror of THD_resample_set's documented SplitMix64 stream."""
    import numpy as np
    mask = (1 << 64) - 1
    state = (int(seed) & mask) ^ 0xD1B54A32D192ED03
    out = np.empty((nboot, nobs), dtype=np.int64)
    lim = mask - (mask % nobs)
    for k in range(nboot * nobs):
        while True:
            state = (state + 0x9E3779B97F4A7C15) & mask
            z = state
            z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & mask
            z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & mask
            z ^= z >> 31
            if z < lim:
                out.flat[k] = z % nobs
                break
    return out


def bootstrap_indices_stratified(block, nboot, seed):
    """Mirror THD_resample_set_build_stratified's documented stream."""
    import numpy as np
    block = np.asarray(block)
    members = [np.flatnonzero(block == b) for b in dict.fromkeys(block.tolist())]
    owner = np.empty(len(block), dtype=int)
    for ib, ix in enumerate(members):
        owner[ix] = ib
    mask = (1 << 64) - 1
    state = (int(seed) & mask) ^ 0xD1B54A32D192ED03
    out = np.empty((nboot, len(block)), dtype=np.int64)
    for ir in range(nboot):
        for i in range(len(block)):
            pool = members[owner[i]]
            lim = mask - (mask % len(pool))
            while True:
                state = (state + 0x9E3779B97F4A7C15) & mask
                z = state
                z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & mask
                z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & mask
                z ^= z >> 31
                if z < lim:
                    out[ir, i] = pool[z % len(pool)]
                    break
    return out


def dual_boot_ci(vals, cval, six, ngroup, ci=90.0, do_tanh=True):
    """Independent F6 corrected subject x condition bootstrap interval.

    vals is one observed working-scale effect per subject; cval[s, b] is the
    same subject's effect under condition draw b, with NaN for unusable draws.
    """
    import numpy as np
    from scipy.stats import t
    vals = np.asarray(vals, float); cval = np.asarray(cval, float)
    valid = np.all(np.isfinite(cval), axis=0)
    sdraw = vals[six].mean(axis=1)
    cdraw = cval[:, valid].mean(axis=0)
    use = np.flatnonzero(valid)
    scdraw = np.asarray([cval[six[b], b].mean() for b in use])
    vs, vc, vsc = (np.var(x, ddof=1) for x in (sdraw, cdraw, scdraw))
    ns = len(vals); fs = ns / (ns - 1.0); fc = ngroup / (ngroup - 1.0)
    var = fs * vs + fc * vc - fs * fc * (vsc - vs - vc)
    var = min(max(var, fs * vs, fc * vc), vsc)
    crit = t.ppf(0.5 + ci / 200.0, min(ns, ngroup) - 1)
    lo, hi = vals.mean() + np.asarray([-1.0, 1.0]) * crit * np.sqrt(max(var, 0.0))
    return np.tanh([lo, hi]) if do_tanh else np.asarray([lo, hi])


def timeshift_offsets(nobs, ntime, nshift, min_shift, seed):
    """Independent mirror of THD_timeshift_set's SplitMix64 offset stream."""
    import numpy as np
    mask = (1 << 64) - 1
    state = (int(seed) & mask) ^ 0xA0761D6478BD642F
    nallow = ntime - 2 * min_shift + 1
    out = np.zeros((nshift, nobs), dtype=np.int64)  # identity is slot zero
    lim = mask - (mask % nallow)
    for k in range(nobs, nshift * nobs):
        while True:
            state = (state + 0x9E3779B97F4A7C15) & mask
            z = state
            z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & mask
            z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & mask
            z ^= z >> 31
            if z < lim:
                out.flat[k] = min_shift + z % nallow
                break
    return out


def phase_factors(nobs, ntime, nphase, seed):
    """Independent mirror of THD_phase_set's stateless SplitMix64 phases."""
    import numpy as np
    mask = (1 << 64) - 1
    base = (int(seed) & mask) ^ 0xE7037ED1A0B428DB
    nfreq = (ntime - 1) // 2
    out = np.ones((nphase, nobs, nfreq), dtype=np.complex128)
    for iphase in range(1, nphase):
        for iobs in range(nobs):
            for k0 in range(nfreq):
                flat = ((iphase - 1) * nobs + iobs) * nfreq + k0
                state = (base + 0x9E3779B97F4A7C15 * (flat + 1)) & mask
                z = state
                z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & mask
                z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & mask
                z ^= z >> 31
                ang = 2.0 * np.pi * float(z >> 11) * 2.0 ** -53
                out[iphase, iobs, k0] = np.cos(ang) + 1j * np.sin(ang)
    return out


def percentile_linear(x, probs):
    """NumPy-version-independent linear percentile, matching rsa_percentile."""
    import numpy as np
    x = np.sort(np.asarray(x, float))
    ans = []
    for p in probs:
        pos = (len(x) - 1) * p
        lo, hi = int(np.floor(pos)), int(np.ceil(pos))
        ans.append((1 - (pos - lo)) * x[lo] + (pos - lo) * x[hi])
    return np.asarray(ans)


# ============================================================================
# fixtures
# ============================================================================
def make_planted(work, degenerate=False, second_behav=False):
    """Planted NN IS-RSA effect in atlas ROI 1; ROIs 2,3 null.  If degenerate,
    ROI 3 is made constant across subjects (to exercise the degenerate-regression
    path).  Returns (atlas_path, table_path)."""
    import numpy as np, nibabel as nib
    from numpy.random import default_rng
    rng = default_rng(SEED)

    atlas = np.zeros(NVOX, dtype=np.int16)
    atlas[0:80] = 1; atlas[80:170] = 2; atlas[170:] = 3

    behav = rng.normal(size=NSUB) * 10 + 50
    rk = (np.argsort(np.argsort(behav)) + 1).astype(float)
    theta = (rk - 1) / (NSUB - 1) * (np.pi / 2)
    S1 = rng.normal(size=NT); S1 -= S1.mean(); S1 /= np.linalg.norm(S1)
    S2 = rng.normal(size=NT); S2 -= S2.dot(S1) * S1; S2 -= S2.mean(); S2 /= np.linalg.norm(S2)

    data = np.zeros((NSUB, NVOX, NT), dtype=np.float32)
    const_pattern = rng.normal(size=NT).astype(np.float32)
    for i in range(NSUB):
        sig = np.cos(theta[i]) * S1 + np.sin(theta[i]) * S2
        for v in range(NVOX):
            if atlas[v] == 1:
                data[i, v] = sig + 0.05 * rng.normal(size=NT)
            elif atlas[v] == 3 and degenerate:
                data[i, v] = const_pattern          # identical for every subject
            else:
                data[i, v] = rng.normal(size=NT)

    aff = np.diag([3.0, 3.0, 3.0, 1.0])
    ap = os.path.join(work, "atlas.nii.gz")
    nib.save(nib.Nifti1Image(atlas.reshape(NX, NY, NZ), aff), ap)
    for i in range(NSUB):
        nib.save(nib.Nifti1Image(data[i].reshape(NX, NY, NZ, NT), aff),
                 os.path.join(work, "sub%02d.nii.gz" % i))
    behav2 = rng.normal(size=NSUB) if second_behav else None
    tp = os.path.join(work, "table.txt")
    with open(tp, "w") as f:
        cols = "Subj behav" + (" behav2" if second_behav else "") + " InputFile\n"
        f.write(cols)
        for i in range(NSUB):
            row = "s%02d %.6f " % (i, behav[i])
            if second_behav:
                row += "%.6f " % behav2[i]
            row += os.path.join(work, "sub%02d.nii.gz" % i) + "\n"
            f.write(row)
    return ap, tp


def make_null(work, nroi=200, vpr=10):
    import numpy as np, nibabel as nib
    from numpy.random import default_rng
    rng = default_rng(7)
    nvox = nroi * vpr
    nz = vpr; nx = 20; ny = nvox // (nx * nz)   # 200 ROIs x 10 vox -> 20x10x10
    assert nx * ny * nz == nvox, (nx, ny, nz, nvox)
    atlas = (np.arange(nvox) // vpr + 1).astype(np.int16)
    aff = np.diag([3.0, 3.0, 3.0, 1.0])
    ap = os.path.join(work, "natlas.nii.gz")
    nib.save(nib.Nifti1Image(atlas.reshape(nx, ny, nz), aff), ap)
    behav = rng.normal(size=NSUB)
    for i in range(NSUB):
        vol = rng.normal(size=(nvox, NT)).astype(np.float32).reshape(nx, ny, nz, NT)
        nib.save(nib.Nifti1Image(vol, aff), os.path.join(work, "nsub%02d.nii.gz" % i))
    tp = os.path.join(work, "ntable.txt")
    with open(tp, "w") as f:
        f.write("Subj behav InputFile\n")
        for i in range(NSUB):
            f.write("s%02d %.6f %s\n" % (i, behav[i],
                    os.path.join(work, "nsub%02d.nii.gz" % i)))
    return ap, tp


# ============================================================================
# the checks -- each appends (name, ok, detail) to RESULTS
# ============================================================================
RESULTS = []


def check(name, ok, detail=""):
    RESULTS.append((name, bool(ok), detail))


def run_checks(BIN, work, threads, verbose):
    import numpy as np
    import nibabel as nib

    env1 = dict(os.environ, OMP_NUM_THREADS="1")
    envN = dict(os.environ, OMP_NUM_THREADS=str(threads))

    def rsa(args, env=None, cwd=work):
        p = subprocess.run([BIN] + args, cwd=cwd, env=env or os.environ,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           universal_newlines=True)
        return p.returncode, p.stdout

    # ---- fixtures -----------------------------------------------------------
    atlas, table = make_planted(work)
    natlas, ntable = make_null(work)

    # =====================================================================
    # 1. planted IS-RSA: observed r matches golden AND an independent numpy
    #    Mantel recomputation from the tool's own saved RDMs
    # =====================================================================
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-nperm", "2000",
                   "-seed", "1", "-save_rdm", os.path.join(work, "sv"),
                   "-prefix", os.path.join(work, "planted"), "-quiet"])
    if rc != 0:
        check("planted IS-RSA runs", False, out.strip()[-300:]); return
    check("planted IS-RSA runs", True)
    hdr, rows = read_table(os.path.join(work, "planted.rsa.1D"), "behav_nn")
    r1 = rows[0]["behav_nn_r"]
    check("planted r == golden %.6f" % GOLDEN_R1, abs(r1 - GOLDEN_R1) < 1e-4,
          "got %.6f" % r1)

    # independent Mantel: spearman of the tool's saved neural vs model triangle
    try:
        neu = np.loadtxt(os.path.join(work, "sv_roi0001.1D"))
        mod = np.loadtxt(os.path.join(work, "sv_model_behav_nn.1D"))
        ref = spearman_tri(neu, mod)
        check("planted r == numpy Mantel on saved RDMs",
              abs(r1 - ref) < 1e-4, "tool %.6f vs numpy %.6f" % (r1, ref))
    except Exception as e:
        check("planted r == numpy Mantel on saved RDMs", False, "save_rdm load: %r" % e)

    # Progress is operational output, but its suppression and redirected-log
    # contracts are stable enough to guard.  Keep these runs inference-light;
    # the numerical paths receive their full checks below.
    pbase = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
             "-model", "NN", "behav:nn",
             "-model", "AnnaK", "behav:annak",
             "-metric", "spearman", "-nperm", "0",
             "-no_dset"]
    rpl, opl = rsa(pbase + ["-progress", "line", "-prefix",
                           os.path.join(work, "progress_line")])
    rpb, opb = rsa(pbase + ["-progress", "bar", "-prefix",
                           os.path.join(work, "progress_bar")])
    rpo, opo = rsa(pbase + ["-progress", "off", "-prefix",
                           os.path.join(work, "progress_off")])
    rpq, opq = rsa(pbase + ["-progress", "line", "-quiet", "-prefix",
                           os.path.join(work, "progress_quiet")])
    check("progress line reports all five stages and completed locations",
          rpl == 0 and all("[%d/5]" % i in opl for i in range(1, 6)) and
          "100.0%" in opl and "completed in" in opl, opl.strip()[-300:])
    # universal_newlines translates the raw carriage return to a newline, so
    # verify the distinct initial/final bar states rather than Python's capture.
    check("progress bar reports updating initial and final states",
          rpb == 0 and "[--------------------] 0/" in opb and
          "[####################]" in opb and "100.0%" in opb,
          repr(opb[-240:]))
    check("progress off and quiet suppress staged progress",
          rpo == 0 and rpq == 0 and "3dRSA [" not in opo and "3dRSA [" not in opq,
          "off=%r quiet=%r" % (opo[-100:], opq[-100:]))
    check("NN and AnnaK diagnostics describe their distinct estimands",
          "subjects closer in behavioral rank" in opl and
          "higher mean behavioral rank across a pair" in opl,
          opl.strip()[-500:])

    # S7 centralized input-domain validation.  An analyzed NaN must fail with
    # subject/brick/voxel context, while an identical NaN outside the union of
    # requested ROIs is irrelevant and must not force whole-volume cleaning.
    vdir = os.path.join(work, "finite_validation"); os.makedirs(vdir, exist_ok=True)
    # AFNI's NIfTI reader intentionally sanitizes source NaNs. Clone the native
    # float dataset just written above and alter one raw BRIK value, avoiding an
    # external AFNI-tool dependency while testing exactly what reaches 3dRSA.
    import gzip, struct, hashlib
    native_src = os.path.join(work, "planted+orig")
    htxt = open(native_src + ".HEAD").read()
    endian = ">" if "MSB_FIRST" in htxt else "<"
    bsrc = native_src + ".BRIK"
    if os.path.exists(bsrc): raw = open(bsrc, "rb").read()
    else: raw = gzip.open(bsrc + ".gz", "rb").read()

    def native_clone(tag, bad_vox=None, bad_brick=0):
        stem = os.path.join(vdir, tag + "+orig")
        # AFNI caches datasets by IDCODE, so clones must not alias one another.
        code = hashlib.sha256(tag.encode("utf-8")).hexdigest()[:22]
        hout = re.sub(r"'AFN_[^~']+~", "'AFN_" + code + "~", htxt, count=1)
        with open(stem + ".HEAD", "w") as hf: hf.write(hout)
        blob = bytearray(raw)
        if bad_vox is not None:
            struct.pack_into(endian + "f", blob, 4 * (bad_brick * NVOX + bad_vox), float("nan"))
        with open(stem + ".BRIK", "wb") as bf: bf.write(blob)
        return stem + ".HEAD"

    vmask = np.zeros((NX, NY, NZ), np.int16); vmask.reshape(-1)[:80] = 1
    vmaskfn = os.path.join(vdir, "mask.nii.gz")
    nib.save(nib.Nifti1Image(vmask, np.diag([3.0, 3.0, 3.0, 1.0])), vmaskfn)
    vs = 6

    def finite_table(tag, bad_flat):
        tf = os.path.join(vdir, tag + ".txt")
        with open(tf, "w") as fo:
            fo.write("Subj behav InputFile\n")
            for sj in range(vs):
                fn = native_clone("%s_s%d" % (tag, sj), bad_flat if sj == 0 else None)
                fo.write("s%02d %g %s\n" % (sj, sj, fn))
        return tf

    vbase = ["-mask", vmaskfn, "-mode", "IS-RSA", "-model", "behav_nn", "behav:nn",
             "-featuretype", "pattern", "-metric", "spearman", "-nperm", "0",
             "-no_dset", "-quiet"]
    fin_env = dict(os.environ, AFNI_FLOATSCAN="NO", AFNI_NOMMAP="YES",
                   AFNI_ENVIRON_WARNINGS="NO")
    rvin, ovin = rsa(["-dataTableFile", finite_table("inside", 0)] + vbase +
                     ["-prefix", os.path.join(vdir, "inside_out")], env=fin_env)
    rvout, ovout = rsa(["-dataTableFile", finite_table("outside", 7)] + vbase +
                       ["-prefix", os.path.join(vdir, "outside_out")], env=fin_env)
    check("S7 non-finite analyzed data fail with owner/brick/voxel context",
          rvin != 0 and "non-finite neural data" in ovin and "s00" in ovin and
          "brick 0" in ovin and "voxel/node 0" in ovin, ovin.strip()[-240:])
    check("S7 non-finite data outside the requested analysis domain are ignored",
          rvout == 0 and os.path.exists(os.path.join(vdir, "outside_out.rsa.1D")),
          ovout.strip()[-180:])

    ntab = finite_table("nan_column_data", 100)
    lines = open(ntab).read().splitlines(); tok = lines[1].split(); tok[1] = "nan"
    lines[1] = " ".join(tok)
    with open(ntab, "w") as fo: fo.write("\n".join(lines) + "\n")
    rvcol, ovcol = rsa(["-dataTableFile", ntab] + vbase +
                       ["-prefix", os.path.join(vdir, "nan_column_out")])
    check("S7 non-finite used numeric table columns are rejected explicitly",
          rvcol != 0 and "non-finite model value" in ovcol and "behav" in ovcol and
          "s00" in ovcol, ovcol.strip()[-200:])

    fracmask = vmask.astype(np.float32); fracmask.reshape(-1)[0] = 1.5
    nanmaskfn = native_clone("nanmask", 0)
    fracmaskfn = os.path.join(vdir, "fracmask.nii.gz")
    nib.save(nib.Nifti1Image(fracmask, np.diag([3.0, 3.0, 3.0, 1.0])), fracmaskfn)
    cleantab = finite_table("clean", 100)
    rvm, ovm = rsa(["-dataTableFile", cleantab, "-mask", nanmaskfn, "-mode", "IS-RSA",
                    "-featuretype", "pattern", "-searchlight", "SPHERE(2)",
                    "-model", "behav_nn", "behav:nn", "-nperm", "0", "-prefix", os.path.join(vdir, "nanmask_out")],
                   env=fin_env)
    rvf, ovf = rsa(["-dataTableFile", cleantab, "-mask", fracmaskfn, "-mode", "IS-RSA",
                    "-model", "behav_nn", "behav:nn", "-nperm", "0", "-prefix", os.path.join(vdir, "fracmask_out")])
    check("S7 atlas masks reject non-finite and non-integer positive labels",
          rvm != 0 and "non-finite" in ovm and rvf != 0 and "non-integer label" in ovf,
          "nan=%s frac=%s" % (ovm.strip()[-100:], ovf.strip()[-100:]))

    optbase = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "behav_nn", "behav:nn", "-no_dset", "-prefix", os.path.join(vdir, "badopt")]
    badopts = [("-nperm", "20junk"), ("-seed", "1x"), ("-boot_ci", "nan"),
               ("-memory_limit", "inf"), ("-searchlight", "SPHERE(2)junk")]
    ores = [rsa(optbase + [o, v]) for o, v in badopts]
    check("S7 malformed numeric and neighborhood tokens are rejected in full",
          all(rc0 != 0 for rc0, _ in ores) and
          all(("finite" in oo.lower() or "integer" in oo.lower() or "malformed" in oo.lower())
              for _, oo in ores), " | ".join(oo.strip()[-80:] for _, oo in ores))

    # A4d: THD_roi_pattern dispatches once per brick and reads its typed array
    # directly.  Exercise every ordinary scalar AFNI storage type with identical
    # integer-valued data; a mixed-datum searchlight must be exactly equivalent
    # to float32 copies of the same datasets.
    tdir = os.path.join(work, "a4d_typed")
    os.makedirs(tdir, exist_ok=True)
    tx, ty, tz, tt, ts = 4, 4, 2, 8, 10
    tmask = os.path.join(tdir, "mask.nii.gz")
    nib.save(nib.Nifti1Image(np.ones((tx, ty, tz), dtype=np.int16), np.eye(4)), tmask)
    dtypes = (np.uint8, np.int16, np.int32, np.float32, np.float64)
    tflo = os.path.join(tdir, "float.txt")
    tmix = os.path.join(tdir, "mixed.txt")
    with open(tflo, "w") as ff, open(tmix, "w") as fm:
        ff.write("Subj behav InputFile\n"); fm.write("Subj behav InputFile\n")
        vv = np.arange(tx * ty * tz).reshape(tx, ty, tz, 1)
        tr = np.arange(tt).reshape(1, 1, 1, tt)
        for sj in range(ts):
            dat = ((7 * vv + 11 * tr + 3 * sj + (sj + 1) * (tr % 3)) % 241)
            ffn = os.path.join(tdir, "f%02d.nii.gz" % sj)
            mfn = os.path.join(tdir, "m%02d.nii.gz" % sj)
            nib.save(nib.Nifti1Image(dat.astype(np.float32), np.eye(4)), ffn)
            nib.save(nib.Nifti1Image(dat.astype(dtypes[sj % len(dtypes)]), np.eye(4)), mfn)
            ff.write("s%02d %.6f %s\n" % (sj, np.sin(0.7 * sj), ffn))
            fm.write("s%02d %.6f %s\n" % (sj, np.sin(0.7 * sj), mfn))

    def typed_run(tabname, pre):
        rc0, out0 = rsa(["-dataTableFile", tabname, "-mask", tmask,
                         "-searchlight", "SPHERE(2)", "-mode", "IS-RSA",
                         "-featuretype", "pattern", "-model", "behav_nn", "behav:nn",
                         "-metric", "spearman", "-nperm", "31", "-seed", "91",
                         "-no_dset", "-quiet", "-prefix", os.path.join(tdir, pre)],
                        env=env1)
        fn = os.path.join(tdir, pre + ".rsa.1D")
        return rc0, out0, read_table(fn, "behav_nn")[1] if rc0 == 0 else []

    rcf, of, rf = typed_run(tflo, "float")
    rcm, om, rm = typed_run(tmix, "mixed")
    tkeys = ("behav_nn_r", "behav_nn_p", "behav_nn_q", "behav_nn_pfwe")
    check("A4d mixed byte/short/int/float/double patterns equal float32",
          rcf == rcm == 0 and len(rf) == len(rm) == tx * ty * tz and
          all(all(a[k] == b[k] for k in tkeys) for a, b in zip(rf, rm)),
          "float_rc=%d mixed_rc=%d rows=%d/%d %s %s" %
          (rcf, rcm, len(rf), len(rm), of.strip()[-100:], om.strip()[-100:]))

    # Subject bootstrap.  Recompute every draw independently from the saved
    # subject matrices, explicitly excluding pairs of sampled positions that
    # refer to the same original subject.  Also show that the tempting naive
    # inclusion of those artificial diagonal dyads gives a different answer.
    NBOOT, BSEED = 401, 73
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-model", "AK", "behav:annak", "-model_contrast", "behav_nn-AK",
                   "-metric", "spearman", "-nperm", "0",
                   "-bootstrap", str(NBOOT), "-boot_ci", "90", "-seed", str(BSEED),
                   "-save_rdm", os.path.join(work, "bsv"),
                   "-prefix", os.path.join(work, "boot_is")], env=env1)
    if rc != 0:
        check("bootstrap IS-RSA runs", False, out.strip()[-240:])
    else:
        check("bootstrap IS-RSA runs", True)
        bh, br = read_table(os.path.join(work, "boot_is.rsa.1D"), "behav_nn")
        neu_b = np.loadtxt(os.path.join(work, "bsv_roi0001.1D"))
        mod_b = np.loadtxt(os.path.join(work, "bsv_model_behav_nn.1D"))
        mod_c = np.loadtxt(os.path.join(work, "bsv_model_AK.1D"))
        bix = bootstrap_indices(NSUB, NBOOT, BSEED)
        vals, naive, dvals = [], [], []
        for ix in bix:
            va, vb, vc, na, nb = [], [], [], [], []
            for a0 in range(NSUB):
                for b0 in range(a0 + 1, NSUB):
                    ia, ib = int(ix[a0]), int(ix[b0])
                    na.append(neu_b[ia, ib]); nb.append(mod_b[ia, ib])
                    if ia != ib:
                        va.append(neu_b[ia, ib]); vb.append(mod_b[ia, ib])
                        vc.append(mod_c[ia, ib])
            # scipy's tie-aware ranks keep the reference independent of the C code.
            from scipy.stats import spearmanr
            vals.append(float(spearmanr(va, vb).statistic))
            dvals.append(float(spearmanr(va, vb).statistic -
                               spearmanr(va, vc).statistic))
            naive.append(float(spearmanr(na, nb).statistic))
        ref_ci = percentile_linear(vals, (0.05, 0.95))
        naive_ci = percentile_linear(naive, (0.05, 0.95))
        got_ci = np.array([br[0]["behav_nn_bootLo"], br[0]["behav_nn_bootHi"]])
        check("bootstrap IS-RSA CI == independent repeated-dyad reference",
              np.allclose(got_ci, ref_ci, atol=3e-5),
              "3dRSA=%s reference=%s" % (got_ci, ref_ci))
        check("bootstrap IS-RSA excludes artificial repeated-subject diagonal",
              np.max(np.abs(ref_ci - naive_ci)) > 1e-4 and
              np.max(np.abs(got_ci - naive_ci)) > 1e-4,
              "correct=%s naive=%s" % (ref_ci, naive_ci))
        blabs = head_brick_labs(os.path.join(work, "boot_is+orig.HEAD"))
        check("bootstrap table/map labels expose both confidence bounds",
              "behav_nn_bootLo" in bh and "behav_nn_bootHi" in bh and
              all(x in blabs for x in ("behav_nn_bootLo", "behav_nn_bootHi")),
              "table=%s map=%s" % (bh, blabs))
        dref = percentile_linear(dvals, (0.05, 0.95))
        dgot = np.array([br[0]["behav_nn-AK_bootLo"],
                         br[0]["behav_nn-AK_bootHi"]])
        check("F17 fixed IS-RSA contrast CI == paired bootstrap reference",
              np.allclose(dgot, dref, atol=3e-5) and
              all(x in blabs for x in ("behav_nn-AK_bootLo",
                                       "behav_nn-AK_bootHi")),
              "3dRSA=%s reference=%s map=%s" % (dgot, dref, blabs))

    for tag, env in (("1", env1), ("N", envN)):
        rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
             "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-nperm", "0",
             "-bootstrap", "101", "-boot_ci", "90", "-seed", "17", "-no_dset",
             "-prefix", os.path.join(work, "boot_thr" + tag)], env=env)
    bt1 = read_table(os.path.join(work, "boot_thr1.rsa.1D"), "behav_nn")[1]
    btN = read_table(os.path.join(work, "boot_thrN.rsa.1D"), "behav_nn")[1]
    check("bootstrap CI thread-reproducible (1 vs %d)" % threads,
          all(x["behav_nn_bootLo"] == y["behav_nn_bootLo"] and
              x["behav_nn_bootHi"] == y["behav_nn_bootHi"] for x, y in zip(bt1, btN)))

    # F3 circular-shift null.  Rebuild the ROI mean series, the behavioral NN
    # model, every shifted neural subject matrix, and both uncorrected and
    # across-ROI max-stat p-values independently.
    import nibabel as nib
    from scipy.stats import rankdata, spearmanr
    TSP, TSMIN, TSSEED = 201, 7, 91
    with open(table) as f:
        beh = np.asarray([float(x.split()[1]) for x in f if x.startswith("s")])
    brank = rankdata(beh, method="average")
    bmod = 1.0 - np.abs(brank[:, None] - brank[None, :]) / (NSUB - 1.0)
    ti = np.triu_indices(NSUB, 1)
    atlas_vals = np.asarray(nib.load(atlas).dataobj).reshape(-1)
    vols = [np.asarray(nib.load(os.path.join(work, "sub%02d.nii.gz" % sj)).dataobj)
            .reshape(-1, NT) for sj in range(NSUB)]

    def shift_stat(series, off):
        shifted = np.asarray([np.roll(series[j], -int(off[j])) for j in range(NSUB)])
        neural = np.corrcoef(shifted)
        rv = float(spearmanr(neural[ti], bmod[ti]).statistic)
        return rv if np.isfinite(rv) else 0.0

    tsoff = timeshift_offsets(NSUB, NT, TSP, TSMIN, TSSEED)
    tsnull = np.empty((3, TSP), float)
    for ri, lab in enumerate((1, 2, 3)):
        series = np.asarray([v[atlas_vals == lab].mean(axis=0) for v in vols])
        tsnull[ri] = [shift_stat(series, off) for off in tsoff]
    tsobs = tsnull[:, 0]
    tsp = np.mean(np.abs(tsnull) >= np.abs(tsobs[:, None]), axis=1)
    tsmax = np.max(np.abs(tsnull), axis=0)
    tspf = np.asarray([np.mean(tsmax >= abs(x)) for x in tsobs])

    def run_shift(pre, env=None, nshift=TSP):
        args = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                "-model", "behav_nn", "behav:nn", "-metric", "spearman",
                "-null", "timeshift", "-min_shift", str(TSMIN),
                "-nperm", str(nshift), "-seed", str(TSSEED), "-no_dset",
                "-prefix", os.path.join(work, pre)]
        rc, out = rsa(args, env=env)
        rows = read_table(os.path.join(work, pre + ".rsa.1D"), "behav_nn")[1] if rc == 0 else []
        return rc, out, rows

    rct, ot, tsrows = run_shift("timeshift", env1)
    tsgot = np.asarray([[r["behav_nn_r"], r["behav_nn_p"], r["behav_nn_pfwe"]]
                        for r in tsrows]) if len(tsrows) == 3 else np.empty((0, 3))
    tsref = np.column_stack((tsobs, tsp, tspf))
    check("F3 circular-shift r/p/FWE match independent NumPy reference",
          rct == 0 and np.allclose(tsgot, tsref, atol=3e-5),
          "rc=%d 3dRSA=%s numpy=%s" % (rct, tsgot, tsref))

    _, _, tst1 = run_shift("timeshift_t1", env1, 101)
    _, _, tstN = run_shift("timeshift_tN", envN, 101)
    check("F3 circular-shift null thread-reproducible (1 vs %d)" % threads,
          len(tst1) == len(tstN) == 3 and
          all(a["behav_nn_p"] == b["behav_nn_p"] and
              a["behav_nn_pfwe"] == b["behav_nn_pfwe"] for a, b in zip(tst1, tstN)))

    # A radius covering the volume makes every searchlight use the same whole-
    # mask series.  This independently pins the streaming path and proves the
    # same offset set drives both its pointwise and max-statistic nulls.
    SLP = 20
    slargs = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
              "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-searchlight", "SPHERE(100)",
              "-null", "timeshift", "-min_shift", str(TSMIN), "-nperm", str(SLP),
              "-seed", str(TSSEED), "-prefix", os.path.join(work, "timeshift_sl")]
    rcsl, osl = rsa(slargs, env=envN)
    slrows = read_table(os.path.join(work, "timeshift_sl.rsa.1D"), "behav_nn")[1] if rcsl == 0 else []
    whole = np.asarray([v.mean(axis=0) for v in vols])
    slnull = np.asarray([shift_stat(whole, off)
                         for off in timeshift_offsets(NSUB, NT, SLP, TSMIN, TSSEED)])
    slobs = slnull[0]; slp = np.mean(np.abs(slnull) >= abs(slobs))
    slok = len(slrows) == NVOX and all(abs(r["behav_nn_r"] - slobs) < 3e-5 and
                                      abs(r["behav_nn_p"] - slp) < 1e-12 and
                                      abs(r["behav_nn_pfwe"] - slp) < 1e-12 for r in slrows)
    check("F3 circular-shift searchlight matches whole-mask streaming reference",
          rcsl == 0 and slok, "rc=%d rows=%d ref=(%.6f,%.6f) %s" %
          (rcsl, len(slrows), slobs, slp, osl.strip()[-100:]))

    # F19 replaces every shifted-series matrix rebuild with a pair x relative-
    # lag lookup table.  Pin that algebra for every supported neural metric;
    # the outer comparison remains the requested Spearman Mantel statistic.
    def shift_stat_metric(series, off, metric):
        shifted = np.asarray([np.roll(series[j], -int(off[j])) for j in range(NSUB)])
        if metric == "corr":
            neural = np.corrcoef(shifted)
        elif metric == "scorr":
            neural = np.corrcoef(np.asarray([rankdata(x, method="average") for x in shifted]))
        elif metric == "cosine":
            nr = np.linalg.norm(shifted, axis=1)
            den = nr[:, None] * nr[None, :]
            neural = np.divide(shifted @ shifted.T, den,
                               out=np.zeros_like(den), where=den > 0)
        else:
            neural = np.sqrt(np.maximum(
                ((shifted[:, None, :] - shifted[None, :, :]) ** 2).sum(axis=2), 0.0))
        rv = float(spearmanr(neural[ti], bmod[ti]).statistic)
        return rv if np.isfinite(rv) else 0.0

    f19n = 41
    f19off = timeshift_offsets(NSUB, NT, f19n, TSMIN, TSSEED)
    f19ok = True; f19detail = []
    for metric in ("corr", "scorr", "cosine", "euclid"):
        if metric == "corr":
            rc19, out19, rows19 = run_shift("f19_" + metric, env1, f19n)
        else:
            rc19, out19 = rsa([
                "-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                "-model", "behav_nn", "behav:nn", "-metric", "spearman",
                "-neural_metric", metric, "-null", "timeshift",
                "-min_shift", str(TSMIN), "-nperm", str(f19n),
                "-seed", str(TSSEED), "-no_dset", "-prefix",
                os.path.join(work, "f19_" + metric)], env=env1)
            tf19 = os.path.join(work, "f19_" + metric + ".rsa.1D")
            rows19 = read_table(tf19, "behav_nn")[1] if rc19 == 0 else []
        null19 = np.asarray([[shift_stat_metric(
            np.asarray([v[atlas_vals == lab].mean(axis=0) for v in vols]), off, metric)
            for off in f19off] for lab in (1, 2, 3)])
        obs19 = null19[:, 0]
        p19 = np.mean(np.abs(null19) >= np.abs(obs19[:, None]), axis=1)
        mx19 = np.max(np.abs(null19), axis=0)
        pf19 = np.asarray([np.mean(mx19 >= abs(x)) for x in obs19])
        got19 = np.asarray([[x["behav_nn_r"], x["behav_nn_p"], x["behav_nn_pfwe"]]
                            for x in rows19]) if len(rows19) == 3 else np.empty((0, 3))
        ref19 = np.column_stack((obs19, p19, pf19))
        mok = rc19 == 0 and np.allclose(got19, ref19, atol=3e-5)
        f19ok &= mok; f19detail.append((metric, mok, got19, ref19))
    check("F19 relative-lag engine matches corr/scorr/cosine/euclid references",
          f19ok, "%s" % f19detail)

    f19meta = open(os.path.join(work, "f19_corr.rsa.1D")).read()
    check("F19 table records lag lookup and fixed model-side contract",
          "subject-pair relative-lag lookup; model side unshifted" in f19meta)

    # F18: the same shifted neural matrices now drive paired model contrasts,
    # joint regression, and separately fitted nuisance-adjusted regression.
    # Models and nuisance dyads remain fixed.  Recompute all three statistic
    # families independently, including spatial max-FWE over the three ROIs.
    tsmotion = np.sin(1.37 * np.arange(NSUB)) + 0.07 * np.arange(NSUB)
    tsregtab = os.path.join(work, "timeshift_reg.txt")
    with open(table) as fi, open(tsregtab, "w") as fo:
        rows0 = [x.split() for x in fi if x.strip()]
        fo.write("Subj behav motion InputFile\n")
        for ii, row in enumerate(rows0[1:]):
            fo.write("%s %s %.9g %s\n" % (row[0], row[1], tsmotion[ii], row[2]))

    akmod = (brank[:, None] + brank[None, :]) / (2.0 * NSUB)
    odiff = np.abs(tsmotion[:, None] - tsmotion[None, :])
    osum = tsmotion[:, None] + tsmotion[None, :]

    def rz18(v):
        v = rankdata(np.asarray(v, float), method="average")
        v -= v.mean(); sd = np.sqrt(np.mean(v * v))
        return v / sd if sd > 0 else np.zeros_like(v)

    xjoint = np.column_stack([rz18(x[ti]) for x in (bmod, akmod, odiff, osum)])
    xort = np.column_stack([rz18(x[ti]) for x in (bmod, odiff, osum)])
    f18con = np.empty((3, TSP), float)
    f18joint = np.empty((3, TSP, 2), float)
    f18ort = np.empty((3, TSP), float)
    for ri, lab in enumerate((1, 2, 3)):
        series = np.asarray([v[atlas_vals == lab].mean(axis=0) for v in vols])
        for ss, off in enumerate(tsoff):
            shifted = np.asarray([np.roll(series[j], -int(off[j]))
                                  for j in range(NSUB)])
            ytri = np.corrcoef(shifted)[ti]
            f18con[ri, ss] = (float(spearmanr(ytri, bmod[ti]).statistic) -
                              float(spearmanr(ytri, akmod[ti]).statistic))
            zy = rz18(ytri)
            f18joint[ri, ss] = (np.linalg.pinv(xjoint) @ zy)[:2]
            f18ort[ri, ss] = (np.linalg.pinv(xort) @ zy)[0]

    def f18_ref(null):
        obs = null[:, 0]
        pu = np.mean(np.abs(null) >= np.abs(obs[:, None]), axis=1)
        mx = np.max(np.abs(null), axis=0)
        pf = np.asarray([np.mean(mx >= abs(v)) for v in obs])
        return np.column_stack((obs, pu, pf))

    f18base = ["-dataTableFile", tsregtab, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "NN", "behav:nn",
               "-model", "AK", "behav:annak",
               "-ortvec", "motion", "-metric", "spearman",
               "-null", "timeshift", "-min_shift", str(TSMIN),
               "-nperm", str(TSP), "-seed", str(TSSEED), "-no_dset"]
    rc18, o18 = rsa(f18base + ["-model_joint", "-model_contrast", "NN-AK",
                     "-prefix", os.path.join(work, "f18_joint")], env=env1)
    r18 = read_table(os.path.join(work, "f18_joint.rsa.1D"), "NN")[1] if rc18 == 0 else []
    gotj = np.asarray([[[r["NN_b"], r["NN_p"], r["NN_pfwe"]],
                        [r["AK_b"], r["AK_p"], r["AK_pfwe"]]] for r in r18])
    refj = np.stack([f18_ref(f18joint[:, :, mm]) for mm in range(2)], axis=1)
    gotc = np.asarray([[r["NN-AK_diff"], r["NN-AK_p"], r["NN-AK_pfwe"]]
                       for r in r18])
    refc = f18_ref(f18con)
    check("F18 timeshift joint+nuisance beta/p/FWE match NumPy reference",
          rc18 == 0 and np.allclose(gotj, refj, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s %s" % (rc18, gotj, refj, o18.strip()[-100:]))
    check("F18 timeshift paired contrast/p/FWE matches NumPy reference",
          rc18 == 0 and np.allclose(gotc, refc, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s" % (rc18, gotc, refc))
    f18meta = open(os.path.join(work, "f18_joint.rsa.1D")).read() if rc18 == 0 else ""
    check("F18 table distinguishes complete-series from residual-label null",
          "fixed conditional design; complete neural-series alignment null" in f18meta)

    def z18(v):
        v = np.asarray(v, float); v = v - v.mean()
        sd = np.sqrt(np.mean(v * v))
        return v / sd if sd > 0 else np.zeros_like(v)

    xjoint_p = np.column_stack([z18(x[ti]) for x in (bmod, akmod, odiff, osum)])
    f18joint_p = np.empty((3, TSP, 2), float)
    f18con_p = np.empty((3, TSP), float)
    for ri, lab in enumerate((1, 2, 3)):
        series = np.asarray([v[atlas_vals == lab].mean(axis=0) for v in vols])
        for ss, off in enumerate(tsoff):
            shifted = np.asarray([np.roll(series[j], -int(off[j]))
                                  for j in range(NSUB)])
            ytri = np.corrcoef(shifted)[ti]
            f18joint_p[ri, ss] = (np.linalg.pinv(xjoint_p) @ z18(ytri))[:2]
            f18con_p[ri, ss] = (float(np.corrcoef(ytri, bmod[ti])[0, 1]) -
                                float(np.corrcoef(ytri, akmod[ti])[0, 1]))
    p18args = f18base.copy(); p18args[p18args.index("spearman")] = "pearson"
    rcp18, op18 = rsa(p18args + ["-model_joint", "-model_contrast", "NN-AK",
                      "-prefix", os.path.join(work, "f18_pearson")], env=env1)
    rp18 = read_table(os.path.join(work, "f18_pearson.rsa.1D"), "NN")[1] if rcp18 == 0 else []
    gotjp = np.asarray([[[r["NN_b"], r["NN_p"], r["NN_pfwe"]],
                         [r["AK_b"], r["AK_p"], r["AK_pfwe"]]] for r in rp18])
    refjp = np.stack([f18_ref(f18joint_p[:, :, mm]) for mm in range(2)], axis=1)
    gotcp = np.asarray([[r["NN-AK_diff"], r["NN-AK_p"], r["NN-AK_pfwe"]]
                        for r in rp18])
    refcp = f18_ref(f18con_p)
    check("F18 Pearson regression/contrast shift null matches NumPy reference",
          rcp18 == 0 and np.allclose(gotjp, refjp, atol=5e-5) and
          np.allclose(gotcp, refcp, atol=5e-5),
          "rc=%d beta=%s/%s contrast=%s/%s %s" %
          (rcp18, gotjp, refjp, gotcp, refcp, op18.strip()[-80:]))

    ortargs = ["-dataTableFile", tsregtab, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "NN", "behav:nn", "-ortvec", "motion",
               "-metric", "spearman", "-null", "timeshift", "-min_shift", str(TSMIN),
               "-nperm", str(TSP), "-seed", str(TSSEED), "-no_dset",
               "-prefix", os.path.join(work, "f18_ort")]
    rco18, oo18 = rsa(ortargs, env=env1)
    ro18 = read_table(os.path.join(work, "f18_ort.rsa.1D"), "NN")[1] if rco18 == 0 else []
    goto18 = np.asarray([[r["NN_b"], r["NN_p"], r["NN_pfwe"]] for r in ro18])
    refo18 = f18_ref(f18ort)
    check("F18 timeshift separate nuisance beta/p/FWE matches NumPy reference",
          rco18 == 0 and np.allclose(goto18, refo18, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s %s" % (rco18, goto18, refo18, oo18.strip()[-100:]))

    def f18_thread(tag, env):
        aa = f18base.copy(); aa[aa.index(str(TSP))] = "61"
        pre = os.path.join(work, "f18_thr" + tag)
        rc0, out0 = rsa(aa + ["-model_joint", "-model_contrast", "NN-AK",
                              "-prefix", pre], env=env)
        return rc0, out0, read_table(pre + ".rsa.1D", "NN")[1] if rc0 == 0 else []
    _, _, f18t1 = f18_thread("1", env1)
    _, _, f18tN = f18_thread("N", envN)
    f18keys = ("NN_b", "NN_p", "NN_pfwe", "AK_b", "AK_p", "AK_pfwe",
               "NN-AK_diff", "NN-AK_p", "NN-AK_pfwe")
    check("F18 timeshift regression/contrast thread-reproducible (1 vs %d)" % threads,
          len(f18t1) == len(f18tN) == 3 and
          all(all(a[k] == b[k] for k in f18keys) for a, b in zip(f18t1, f18tN)))

    # F5 ROI-first phase randomization.  Mirror the stateless SplitMix64 phase
    # family independently, rotate only positive-frequency bins, reconstruct
    # conjugate-real series, and verify the primary, contrast, joint, nuisance,
    # spatial max-FWE, provenance, and OpenMP contracts.
    F5N, F5SEED = 81, 137
    f5fac = phase_factors(NSUB, NT, F5N, F5SEED)

    def phase_series(series, iphase):
        if iphase == 0:
            return np.asarray(series, float).copy()
        ft = np.fft.rfft(np.asarray(series, float), axis=1)
        ft[:, 1:1 + f5fac.shape[2]] *= f5fac[iphase]
        return np.fft.irfft(ft, n=NT, axis=1)

    f5primary = np.empty((3, F5N), float)
    f5joint = np.empty((3, F5N, 2), float)
    f5ort = np.empty((3, F5N), float)
    f5con = np.empty((3, F5N), float)
    spectral_ok = True
    for ri, lab in enumerate((1, 2, 3)):
        series = np.asarray([v[atlas_vals == lab].mean(axis=0) for v in vols])
        original_power = np.abs(np.fft.rfft(series, axis=1))
        for ss in range(F5N):
            surrogate = phase_series(series, ss)
            if ss > 0:
                spectral_ok &= np.allclose(surrogate.mean(axis=1), series.mean(axis=1),
                                           atol=1e-6)
                spectral_ok &= np.allclose(np.abs(np.fft.rfft(surrogate, axis=1)),
                                           original_power, atol=1e-5)
            ytri = np.corrcoef(surrogate)[ti]
            f5primary[ri, ss] = float(spearmanr(ytri, bmod[ti]).statistic)
            f5con[ri, ss] = (float(spearmanr(ytri, bmod[ti]).statistic) -
                              float(spearmanr(ytri, akmod[ti]).statistic))
            zy = rz18(ytri)
            f5joint[ri, ss] = (np.linalg.pinv(xjoint) @ zy)[:2]
            f5ort[ri, ss] = (np.linalg.pinv(xort) @ zy)[0]
    check("F5 reference surrogates preserve each subject's mean and power spectrum",
          spectral_ok)

    f5args = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
              "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-null", "phase",
              "-nperm", str(F5N), "-seed", str(F5SEED), "-no_dset"]
    rcf5, of5 = rsa(f5args + ["-prefix", os.path.join(work, "f5_primary")], env=env1)
    rf5 = read_table(os.path.join(work, "f5_primary.rsa.1D"), "behav_nn")[1] if rcf5 == 0 else []
    gotf5 = np.asarray([[r["behav_nn_r"], r["behav_nn_p"], r["behav_nn_pfwe"]]
                        for r in rf5]) if len(rf5) == 3 else np.empty((0, 3))
    reff5 = f18_ref(f5primary)
    check("F5 phase primary r/p/FWE match independent NumPy reference",
          rcf5 == 0 and np.allclose(gotf5, reff5, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s %s" % (rcf5, gotf5, reff5, of5.strip()[-100:]))

    f5metric_ok = True; f5metric_detail = []
    for metric in ("scorr", "cosine", "euclid"):
        nullm = np.empty((3, F5N), float)
        for ri, lab in enumerate((1, 2, 3)):
            series = np.asarray([v[atlas_vals == lab].mean(axis=0) for v in vols])
            for ss in range(F5N):
                surrogate = phase_series(series, ss)
                if metric == "scorr":
                    feat = np.asarray([rankdata(x, method="average") for x in surrogate])
                    neuralm = np.corrcoef(feat)
                elif metric == "cosine":
                    nr = np.linalg.norm(surrogate, axis=1)
                    den = nr[:, None] * nr[None, :]
                    neuralm = np.divide(surrogate @ surrogate.T, den,
                                        out=np.zeros_like(den), where=den > 0)
                else:
                    neuralm = np.sqrt(np.maximum(
                        ((surrogate[:, None, :] - surrogate[None, :, :]) ** 2).sum(axis=2),
                        0.0))
                rv = float(spearmanr(neuralm[ti], bmod[ti]).statistic)
                nullm[ri, ss] = rv if np.isfinite(rv) else 0.0
        prem = os.path.join(work, "f5_" + metric)
        rcm, om = rsa(f5args + ["-neural_metric", metric, "-prefix", prem], env=env1)
        rm = read_table(prem + ".rsa.1D", "behav_nn")[1] if rcm == 0 else []
        gotm = np.asarray([[r["behav_nn_r"], r["behav_nn_p"], r["behav_nn_pfwe"]]
                           for r in rm]) if len(rm) == 3 else np.empty((0, 3))
        refm = f18_ref(nullm)
        mok = rcm == 0 and np.allclose(gotm, refm, atol=5e-5)
        f5metric_ok &= mok; f5metric_detail.append((metric, mok, gotm, refm, om[-80:]))
    check("F5 phase matrix rebuild matches scorr/cosine/euclid references",
          f5metric_ok, "%s" % f5metric_detail)

    f5regbase = ["-dataTableFile", tsregtab, "-mask", atlas, "-mode", "IS-RSA",
                 "-model", "NN", "behav:nn",
                 "-model", "AK", "behav:annak",
                 "-ortvec", "motion", "-metric", "spearman", "-null", "phase",
                 "-nperm", str(F5N), "-seed", str(F5SEED), "-no_dset"]
    rcf5r, of5r = rsa(f5regbase + ["-model_joint", "-model_contrast", "NN-AK",
                         "-prefix", os.path.join(work, "f5_joint")], env=env1)
    rf5r = read_table(os.path.join(work, "f5_joint.rsa.1D"), "NN")[1] if rcf5r == 0 else []
    gotf5j = np.asarray([[[r["NN_b"], r["NN_p"], r["NN_pfwe"]],
                           [r["AK_b"], r["AK_p"], r["AK_pfwe"]]] for r in rf5r])
    reff5j = np.stack([f18_ref(f5joint[:, :, mm]) for mm in range(2)], axis=1)
    gotf5c = np.asarray([[r["NN-AK_diff"], r["NN-AK_p"], r["NN-AK_pfwe"]]
                         for r in rf5r])
    reff5c = f18_ref(f5con)
    check("F5 phase joint+nuisance beta/p/FWE match NumPy reference",
          rcf5r == 0 and np.allclose(gotf5j, reff5j, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s %s" % (rcf5r, gotf5j, reff5j, of5r.strip()[-100:]))
    check("F5 phase paired contrast/p/FWE match NumPy reference",
          rcf5r == 0 and np.allclose(gotf5c, reff5c, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s" % (rcf5r, gotf5c, reff5c))

    f5ortargs = ["-dataTableFile", tsregtab, "-mask", atlas, "-mode", "IS-RSA",
                 "-model", "NN", "behav:nn", "-ortvec", "motion",
                 "-metric", "spearman", "-null", "phase", "-nperm", str(F5N),
                 "-seed", str(F5SEED), "-no_dset", "-prefix",
                 os.path.join(work, "f5_ort")]
    rcf5o, of5o = rsa(f5ortargs, env=env1)
    rf5o = read_table(os.path.join(work, "f5_ort.rsa.1D"), "NN")[1] if rcf5o == 0 else []
    gotf5o = np.asarray([[r["NN_b"], r["NN_p"], r["NN_pfwe"]] for r in rf5o])
    reff5o = f18_ref(f5ort)
    check("F5 phase separate-nuisance beta/p/FWE match NumPy reference",
          rcf5o == 0 and np.allclose(gotf5o, reff5o, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s %s" % (rcf5o, gotf5o, reff5o, of5o.strip()[-100:]))

    f5meta = open(os.path.join(work, "f5_joint.rsa.1D")).read() if rcf5r == 0 else ""
    check("F5 table records spectral, fixed-model, and shared-family contracts",
          "local-spectrum real FFT cache; DC/Nyquist and model side unrandomized" in f5meta and
          "stateless subject x frequency draws shared over ROIs/searchlights; identity slot 0" in f5meta and
          "fixed conditional design; complete neural-series phase-alignment null" in f5meta)

    def f5_thread(tag, env):
        aa = f5regbase.copy(); aa[aa.index(str(F5N))] = "41"
        pre = os.path.join(work, "f5_thr" + tag)
        rc0, out0 = rsa(aa + ["-model_joint", "-model_contrast", "NN-AK",
                              "-prefix", pre], env=env)
        return rc0, out0, read_table(pre + ".rsa.1D", "NN")[1] if rc0 == 0 else []
    _, _, f5t1 = f5_thread("1", env1)
    _, _, f5tN = f5_thread("N", envN)
    check("F5 phase null thread-reproducible (1 vs %d)" % threads,
          len(f5t1) == len(f5tN) == 3 and
          all(all(a[k] == b[k] for k in f18keys) for a, b in zip(f5t1, f5tN)))

    # F23 expected Spearman rho-a.  Verify the analytic random-tie expectation
    # against an independent rankdata implementation, its no-tie identity with
    # ordinary Spearman, the intended shrinkage for a tied categorical model,
    # and the explicit boundary at least-squares model fitting.
    f23pre = os.path.join(work, "f23_tied")
    f23save = os.path.join(work, "f23save")
    f23base = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "NN", "behav:nn",
               "-model", "AK", "behav:annak",
               "-model_contrast", "NN-AK", "-metric", "rhoa", "-nperm", "0",
               "-no_dset", "-save_rdm", f23save, "-prefix", f23pre]
    rc23, o23 = rsa(f23base, env=env1)
    r23 = read_table(f23pre + ".rsa.1D", "NN")[1] if rc23 == 0 else []
    m23 = np.loadtxt(f23save + "_model_NN.1D") if rc23 == 0 else np.empty((0, 0))
    ref23 = []
    if rc23 == 0:
        for ri in range(3):
            n23 = np.loadtxt(f23save + "_roi%04d.1D" % (ri + 1))
            ref23.append(rhoa_vec(n23[ti], m23[ti]))
    got23 = np.asarray([r["NN_r"] for r in r23])
    check("F23 rho-a equals independent closed-form random-tie expectation",
          rc23 == 0 and len(r23) == 3 and np.allclose(got23, ref23, atol=2e-6),
          "rc=%d 3dRSA=%s ref=%s %s" % (rc23, got23, ref23, o23.strip()[-100:]))

    rc23s, o23s = rsa([x if x != "rhoa" else "spearman" for x in f23base[:-2]] +
                      ["-prefix", os.path.join(work, "f23_spearman")], env=env1)
    r23s = read_table(os.path.join(work, "f23_spearman.rsa.1D"), "NN")[1] \
        if rc23s == 0 else []
    check("F23 tied model is not rewarded by ordinary Spearman normalization",
          len(r23) == len(r23s) == 3 and abs(r23[0]["NN_r"]) < abs(r23s[0]["NN_r"]),
          "rhoa=%s spearman=%s" %
          (got23, [r["NN_r"] for r in r23s] if r23s else o23s.strip()[-100:]))

    f23unique = np.zeros((NSUB, NSUB), float); uq = 1.0
    for ui in range(NSUB):
        for uj in range(ui + 1, NSUB):
            f23unique[ui, uj] = f23unique[uj, ui] = uq; uq += 1.0
    f23uf = os.path.join(work, "f23_unique.1D"); np.savetxt(f23uf, f23unique)
    def f23_unique_run(metric, tag):
        pre = os.path.join(work, "f23_unique_" + tag)
        rc0, out0 = rsa(["-dataTableFile", table, "-mask", atlas,
                         "-mode", "IS-RSA", "-model_mat", "U", f23uf,
                         "-metric", metric, "-nperm", "0", "-no_dset",
                         "-prefix", pre], env=env1)
        rows0 = read_table(pre + ".rsa.1D", "U")[1] if rc0 == 0 else []
        return rc0, out0, rows0
    rcur, our, rur = f23_unique_run("rho-a", "rhoa")
    rcus, ous, rus = f23_unique_run("spearman", "spearman")
    check("F23 rho-a equals Spearman when both rank vectors have no ties",
          rcur == rcus == 0 and len(rur) == len(rus) == 3 and
          all(abs(a["U_r"] - b["U_r"]) < 2e-6
              for a, b in zip(rur, rus)),
          "rhoa_rc=%d spearman_rc=%d %s %s" % (rcur, rcus, our[-80:], ous[-80:]))

    rc23reg, o23reg = rsa(f23base[:-4] + ["-model_joint", "-prefix",
                              os.path.join(work, "bad_f23_reg")], env=env1)
    check("F23 rho-a rejects an undefined least-squares regression estimand",
          rc23reg != 0 and "scalar RDM comparator" in o23reg and
          "metric spearman" in o23reg)

    # F5b phase-randomization searchlights.  Four one-voxel searchlights make
    # the local spectral cache and synchronized spatial max-null independently
    # reconstructable without making the regression suite expensive.
    f5bcoord = ((1, 0, 0), (3, 0, 0), (5, 0, 0), (7, 0, 0))
    f5bmaskv = np.zeros((NX, NY, NZ), np.int16)
    for xyz in f5bcoord: f5bmaskv[xyz] = 1
    f5bmask = os.path.join(work, "f5b_mask.nii.gz")
    nib.save(nib.Nifti1Image(f5bmaskv, nib.load(atlas).affine), f5bmask)
    f5bnull = np.empty((len(f5bcoord), F5N), float)
    for ci, xyz in enumerate(f5bcoord):
        series = np.asarray([v.reshape(NX, NY, NZ, NT)[xyz] for v in vols])
        for ss in range(F5N):
            ytri = np.corrcoef(phase_series(series, ss))[ti]
            f5bnull[ci, ss] = float(spearmanr(ytri, bmod[ti]).statistic)

    def f5b_run(tag, env):
        pre = os.path.join(work, "f5b_" + tag)
        aa = f5args.copy(); aa[aa.index(atlas)] = f5bmask
        rc0, out0 = rsa(aa + ["-searchlight", "SPHERE(1)", "-prefix", pre], env=env)
        rows0 = read_table(pre + ".rsa.1D", "behav_nn")[1] if rc0 == 0 else []
        meta0 = open(pre + ".rsa.1D").read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0
    rcb5, ob5, rb5, mb5 = f5b_run("reference", env1)
    gotb5 = np.asarray([[r["behav_nn_r"], r["behav_nn_p"], r["behav_nn_pfwe"]]
                        for r in rb5]) if len(rb5) == len(f5bcoord) else np.empty((0, 3))
    refb5 = f18_ref(f5bnull)
    check("F5b searchlight phase r/p/spatial-FWE match independent NumPy reference",
          rcb5 == 0 and np.allclose(gotb5, refb5, atol=5e-5),
          "rc=%d 3dRSA=%s ref=%s %s" % (rcb5, gotb5, refb5, ob5.strip()[-100:]))
    check("F5b output records reusable local-spectrum and shared-family contracts",
          "local-spectrum real FFT cache" in mb5 and
          "shared over ROIs/searchlights; identity slot 0" in mb5)
    _, _, f5bt1, _ = f5b_run("thread1", env1)
    _, _, f5btN, _ = f5b_run("threadN", envN)
    f5bkeys = ("behav_nn_r", "behav_nn_p", "behav_nn_q", "behav_nn_pfwe")
    check("F5b phase searchlight thread-reproducible (1 vs %d)" % threads,
          len(f5bt1) == len(f5btN) == len(f5bcoord) and
          all(all(a[k] == b[k] for k in f5bkeys) for a, b in zip(f5bt1, f5btN)))

    # Contract failures are explicit: this null has a different estimand and
    # must never silently fall back to subject-label permutation.
    base_ts = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "behav_nn", "behav:nn", "-nperm", "20"]
    rcpat, opat = rsa(base_ts + ["-featuretype", "pattern", "-null", "timeshift",
                                  "-prefix", os.path.join(work, "bad_ts_pattern")])
    rcmin, omin = rsa(base_ts + ["-min_shift", "3",
                                  "-prefix", os.path.join(work, "bad_ts_min")])
    rcbig, obig = rsa(base_ts + ["-null", "timeshift", "-min_shift", str(NT // 2),
                                  "-prefix", os.path.join(work, "bad_ts_big")])
    rcloo, oloo = rsa(base_ts + ["-null", "timeshift", "-loo",
                                  "-prefix", os.path.join(work, "bad_ts_loo")])
    rcf5pat, of5pat = rsa(base_ts + ["-null", "phase", "-featuretype", "pattern",
                                        "-prefix", os.path.join(work, "bad_f5_pattern")])
    rcf5loo, of5loo = rsa(base_ts + ["-null", "phase", "-loo",
                                        "-prefix", os.path.join(work, "bad_f5_loo")])
    check("F3 timeshift rejects pattern features",
          rcpat != 0 and "featuretype mean" in opat)
    check("F3 min_shift without timeshift is rejected",
          rcmin != 0 and "only applies" in omin)
    check("F3 timeshift rejects an impossible minimum offset",
          rcbig != 0 and "fewer than two" in obig)
    check("F18 timeshift still rejects undefined LOO extension",
          rcloo != 0 and "LOO" in oloo and "not yet" in oloo)
    check("F5 phase null rejects pattern features",
          rcf5pat != 0 and "featuretype mean" in of5pat)
    check("F5 phase null still rejects undefined LOO extension",
          rcf5loo != 0 and "LOO" in of5loo and "not yet" in of5loo)

    # F9 fixed-model searchlight cache.  A one-label whole-volume atlas and a
    # SPHERE(100) searchlight see the identical voxel set.  The atlas therefore
    # exercises the established uncached path while every searchlight center
    # exercises the cache; primary and paired-contrast nulls must agree exactly.
    allmask = os.path.join(work, "f9_allmask.nii.gz")
    onemask = os.path.join(work, "f9_onemask.nii.gz")
    am = nib.load(atlas)
    nib.save(nib.Nifti1Image(np.ones((NX, NY, NZ), dtype=np.int16), am.affine), allmask)
    om = np.zeros((NX, NY, NZ), dtype=np.int16); om[0, 0, 0] = 1
    nib.save(nib.Nifti1Image(om, am.affine), onemask)

    def f9_run(metric, pre, searchlight=False, env=None, nper=101, maskname=allmask):
        aa = ["-dataTableFile", table, "-mask", maskname, "-mode", "IS-RSA",
              "-model", "nn", "behav:nn",
              "-model", "ak", "behav:annak",
              "-model_contrast", "nn-ak", "-metric", metric,
              "-nperm", str(nper), "-seed", "73", "-no_dset",
              "-prefix", os.path.join(work, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc, out = rsa(aa, env=env)
        rows = read_table(os.path.join(work, pre + ".rsa.1D"), "nn")[1] if rc == 0 else []
        return rc, out, rows

    f9keys = ("nn_r", "nn_p", "nn_pfwe", "ak_r", "ak_p", "ak_pfwe",
              "nn-ak_diff", "nn-ak_p", "nn-ak_pfwe")
    for metric, nper in (("spearman", 101), ("rhoa", 61), ("pearson", 41)):
        fm = allmask if metric != "pearson" else onemask
        nr = NVOX if metric != "pearson" else 1
        rca, oa, ar = f9_run(metric, "f9_%s_atlas" % metric,
                             nper=nper, maskname=fm)
        rcs, os_, sr = f9_run(metric, "f9_%s_sl" % metric, True, envN, nper, fm)
        same = (rca == rcs == 0 and len(ar) == 1 and len(sr) == nr and
                all(all(row[k] == ar[0][k] for k in f9keys) for row in sr))
        activated = "cached 2 fixed models x %d relabelings" % nper in os_
        tag = "F23" if metric == "rhoa" else "F9"
        check("%s %s cache matches uncached atlas for r/p/FWE + contrast" % (tag, metric),
              same and activated, "atlas_rc=%d sl_rc=%d rows=%d cache=%s" %
              (rca, rcs, len(sr), activated))

    _, _, f9t1 = f9_run("spearman", "f9_thr1", True, env1, 61)
    _, _, f9tN = f9_run("spearman", "f9_thrN", True, envN, 61)
    check("F9 cached searchlight thread-reproducible (1 vs %d)" % threads,
          len(f9t1) == len(f9tN) == NVOX and
          all(all(x[k] == y[k] for k in f9keys) for x, y in zip(f9t1, f9tN)))

    # F17 regression bootstrap: add a deterministic nuisance to the planted
    # table, then independently refit the ranked/z-scored compact dyadic design
    # for every subject draw.  Cover both code paths: all models jointly with
    # nuisances, and one model at a time with the same nuisances projected out.
    regtab = os.path.join(work, "table_reg.txt")
    motion = np.sin(1.37 * np.arange(NSUB)) + 0.07 * np.arange(NSUB)
    with open(table) as fi, open(regtab, "w") as fo:
        rows0 = [x.split() for x in fi if x.strip()]
        fo.write("Subj behav motion InputFile\n")
        for ii, row in enumerate(rows0[1:]):
            fo.write("%s %s %.9g %s\n" % (row[0], row[1], motion[ii], row[2]))

    neu_r = np.loadtxt(os.path.join(work, "bsv_roi0001.1D"))
    nn_rdm = np.loadtxt(os.path.join(work, "bsv_model_behav_nn.1D"))
    ak_rdm = np.loadtxt(os.path.join(work, "bsv_model_AK.1D"))
    mdiff = np.abs(motion[:, None] - motion[None, :])
    msum = motion[:, None] + motion[None, :]
    bixr = bootstrap_indices(NSUB, 401, 37)

    def compact_reg_draw(ix, mats):
        from scipy.stats import rankdata
        cols = [[] for _ in mats]; yy = []
        for aa in range(NSUB):
            for bb in range(aa + 1, NSUB):
                ia, ib = int(ix[aa]), int(ix[bb])
                if ia == ib:
                    continue
                yy.append(neu_r[ia, ib])
                for cc, mat in enumerate(mats):
                    cols[cc].append(mat[ia, ib])

        def rz(v):
            v = rankdata(np.asarray(v, float)); v -= v.mean()
            sd = np.sqrt(np.mean(v * v))
            return v / sd if sd > 0 else np.zeros_like(v)

        return np.linalg.pinv(np.column_stack([rz(x) for x in cols])) @ rz(yy)

    jdraw = np.vstack([compact_reg_draw(ix, [nn_rdm, ak_rdm, mdiff, msum])[:2]
                       for ix in bixr if len(set(map(int, ix))) >= 3])
    odraw = np.asarray([compact_reg_draw(ix, [nn_rdm, mdiff, msum])[0]
                        for ix in bixr if len(set(map(int, ix))) >= 3])
    jref = np.column_stack([percentile_linear(jdraw[:, mm], (0.05, 0.95))
                            for mm in range(2)])
    oref = percentile_linear(odraw, (0.05, 0.95))

    jbase = ["-dataTableFile", regtab, "-mask", atlas, "-mode", "IS-RSA",
             "-model", "NN", "behav:nn",
             "-model", "AK", "behav:annak",
             "-model_joint", "-ortvec", "motion", "-metric", "spearman",
             "-nperm", "0", "-bootstrap", "401", "-boot_ci", "90",
             "-seed", "37", "-prefix", os.path.join(work, "boot_joint_is")]
    rcj, oj = rsa(jbase, env=env1)
    jrow = read_table(os.path.join(work, "boot_joint_is.rsa.1D"), "NN")[1] if rcj == 0 else []
    jgot = (np.array([[jrow[0]["NN_bootLo"], jrow[0]["AK_bootLo"]],
                      [jrow[0]["NN_bootHi"], jrow[0]["AK_bootHi"]]])
            if len(jrow) else np.full((2, 2), np.nan))
    check("F17 IS-RSA joint+nuisance bootstrap refits compact regression",
          np.allclose(jgot, jref, atol=5e-4),
          "rc=%d 3dRSA=%s reference=%s %s" % (rcj, jgot, jref, oj.strip()[-100:]))

    obase = ["-dataTableFile", regtab, "-mask", atlas, "-mode", "IS-RSA",
             "-model", "NN", "behav:nn",
             "-ortvec", "motion", "-metric", "spearman", "-nperm", "0",
             "-bootstrap", "401", "-boot_ci", "90", "-seed", "37",
             "-prefix", os.path.join(work, "boot_ort_is")]
    rco, oo = rsa(obase, env=env1)
    orow = read_table(os.path.join(work, "boot_ort_is.rsa.1D"), "NN")[1] if rco == 0 else []
    ogot = (np.array([orow[0]["NN_bootLo"], orow[0]["NN_bootHi"]])
            if len(orow) else np.array([np.nan, np.nan]))
    check("F17 IS-RSA -ortvec bootstrap refits nuisance-adjusted model",
          np.allclose(ogot, oref, atol=5e-4),
          "rc=%d 3dRSA=%s reference=%s %s" % (rco, ogot, oref, oo.strip()[-100:]))

    for tag, env in (("1", env1), ("N", envN)):
        aa = jbase.copy()
        aa[aa.index("401")] = "101"
        aa[aa.index("37")] = "43"
        aa[-1] = os.path.join(work, "boot_joint_thr" + tag)
        rsa(aa, env=env)
    jt1 = read_table(os.path.join(work, "boot_joint_thr1.rsa.1D"), "NN")[1]
    jtN = read_table(os.path.join(work, "boot_joint_thrN.rsa.1D"), "NN")[1]
    jrkeys = ("NN_bootLo", "NN_bootHi", "AK_bootLo", "AK_bootHi")
    check("F17 IS-RSA regression bootstrap thread-reproducible (1 vs %d)" % threads,
          len(jt1) == len(jtN) and
          all(all(x[k] == y[k] for k in jrkeys) for x, y in zip(jt1, jtN)))

    # Every behavior value is unique here, so treating it as a bootstrap stratum
    # makes every resample the identity.  This is a sharp contract check that
    # -block drives within-stratum resampling rather than being ignored.
    rcb, ob = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-block", "behav", "-bootstrap", "20",
                   "-nperm", "0", "-no_dset",
                   "-prefix", os.path.join(work, "boot_singleton_block")])
    brow = (read_table(os.path.join(work, "boot_singleton_block.rsa.1D"),
                       "behav_nn")[1] if rcb == 0 else [])
    check("F17 stratified bootstrap preserves singleton strata exactly",
          rcb == 0 and len(brow) and all(
              abs(r["behav_nn_bootLo"] - r["behav_nn_r"]) < 1e-6 and
              abs(r["behav_nn_bootHi"] - r["behav_nn_r"]) < 1e-6 for r in brow),
          "rc=%d %s" % (rcb, ob.strip()[-160:]))

    # =====================================================================
    # 2. null-200 FWE calibration + monotonicity
    # =====================================================================
    rc, out = rsa(["-dataTableFile", ntable, "-mask", natlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-nperm", "3000",
                   "-seed", "1", "-no_dset",
                   "-prefix", os.path.join(work, "null")], env=env1)
    if rc != 0:
        check("null-200 runs", False, out.strip()[-300:])
    else:
        check("null-200 runs", True)
        _, nrows = read_table(os.path.join(work, "null.rsa.1D"), "behav_nn")
        p = np.array([x["behav_nn_p"] for x in nrows])
        pf = np.array([x["behav_nn_pfwe"] for x in nrows])
        check("FWE p in [0,1]", np.all((pf >= 0) & (pf <= 1)))
        check("FWE p >= uncorrected p (monotone)", np.all(pf >= p - 1e-9),
              "%d violations" % int(np.sum(pf < p - 1e-9)))
        check("FWE controls: 0/200 null survive p_fwe<.05",
              int(np.sum(pf < 0.05)) == 0, "%d survived" % int(np.sum(pf < 0.05)))

    # =====================================================================
    # 3. thread reproducibility of p and p_fwe
    # =====================================================================
    for tag, env in (("1", env1), ("N", envN)):
        rsa(["-dataTableFile", ntable, "-mask", natlas, "-mode", "IS-RSA",
             "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-nperm", "1500",
             "-seed", "9", "-no_dset",
             "-prefix", os.path.join(work, "thr%s" % tag)], env=env)
    _, a = read_table(os.path.join(work, "thr1.rsa.1D"), "behav_nn")
    _, b = read_table(os.path.join(work, "thrN.rsa.1D"), "behav_nn")
    same = all(abs(x["behav_nn_p"] - y["behav_nn_p"]) < 1e-12 and
               abs(x["behav_nn_pfwe"] - y["behav_nn_pfwe"]) < 1e-12
               for x, y in zip(a, b))
    check("p and p_fwe identical at 1 vs %d threads" % threads, same)

    # =====================================================================
    # 4. LOO: positive on planted, monotone FWE
    # =====================================================================
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-nperm", "2000",
                   "-seed", "1", "-loo", "-no_dset",
                   "-prefix", os.path.join(work, "loo")])
    _, lrows = read_table(os.path.join(work, "loo.rsa.1D"), "behav_nn")
    check("LOO planted looR > 0.5", lrows[0]["behav_nn_looR"] > 0.5,
          "got %.4f" % lrows[0]["behav_nn_looR"])
    lp = np.array([x["behav_nn_looP"] for x in lrows])
    lpf = np.array([x["behav_nn_looPfwe"] for x in lrows])
    check("LOO-FWE p >= looP (monotone)", np.all(lpf >= lp - 1e-9))

    # A4a/A4b + F10: exact duplicate target/estimand pairs share a LOO family,
    # but AnnaK and NN now implement different held-subject hypotheses.  The
    # contrast spec below has two
    # valid parses: a-(b-c) and (a-b)-c; the documented longest A prefix wins.
    a4pre = os.path.join(work, "a4ab")
    rca4, oa4 = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                     "-model", "a", "behav:nn",
                     "-model", "b-c", "behav:nn",
                     "-model", "a-b", "behav:nn",
                     "-model", "c", "behav:annak",
                     "-model_contrast", "a-b-c", "-loo", "-metric", "spearman",
                     "-nperm", "200", "-seed", "7", "-no_dset", "-prefix", a4pre])
    a4rows = (read_table(a4pre + ".rsa.1D", "a")[1] if rca4 == 0 else [])
    a4text = open(a4pre + ".rsa.1D").read() if rca4 == 0 else ""
    check("F10 duplicate estimands share but AnnaK/NN use distinct LOO families",
          rca4 == 0 and
          "4 model outputs from 2 distinct target/estimand families" in a4text,
          "rc=%d %s" % (rca4, oa4.strip()[-200:]))
    check("F10 exact duplicate NN LOO outputs/FWE remain identical",
          len(a4rows) > 0 and all(
              row["a_" + suffix] == row[name + "_" + suffix]
              for row in a4rows for name in ("b-c", "a-b")
              for suffix in ("looR", "looP", "looQ", "looPfwe")),
          "rows=%d" % len(a4rows))
    check("F10 AnnaK prediction differs from NN on the same target",
          len(a4rows) > 0 and
          any(abs(row["a_looR"] - row["c_looR"]) > 1e-4 for row in a4rows))
    if a4rows:
        row = a4rows[0]
        longdiff = row["a-b_r"] - row["c_r"]
        shortdiff = row["a_r"] - row["b-c_r"]
        gotdiff = row["a-b-c_diff"]
    else:
        longdiff = shortdiff = gotdiff = float("nan")
    check("A4b contrast resolver chooses longest valid A prefix",
          abs(gotdiff - longdiff) < 1e-6 and abs(gotdiff - shortdiff) > 1e-3,
          "got=%.6f long=%.6f short=%.6f" % (gotdiff, longdiff, shortdiff))

    # =====================================================================
    # AUDIT FIX 2: -neural_metric euclid must NOT invert LOO
    # =====================================================================
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-neural_metric", "euclid",
                   "-metric", "spearman", "-nperm", "500", "-seed", "1", "-loo",
                   "-no_dset", "-prefix", os.path.join(work, "le")])
    _, erows = read_table(os.path.join(work, "le.rsa.1D"), "behav_nn")
    check("FIX2 euclid-LOO planted looR > 0 (not inverted)",
          erows[0]["behav_nn_looR"] > 0.5, "got %.4f" % erows[0]["behav_nn_looR"])

    # F10: independent observed and exhaustive blocked-null references for the
    # neural-neighbor scalar, AnnaK typicality, and multivariate-profile
    # estimands.  Pair blocks make the relabeling group exactly 2^10=1024.
    from scipy.stats import rankdata, spearmanr
    with open(table) as f:
        dat = [x.split() for x in f if x.startswith("s")]
    f10beh = np.asarray([float(x[1]) for x in dat], dtype=np.float32)
    f10beh2 = np.asarray(np.sin(0.73 * np.arange(NSUB)) +
                         0.03 * np.arange(NSUB), dtype=np.float32)
    f10tab = os.path.join(work, "f10_table.txt")
    with open(f10tab, "w") as f:
        f.write("Subj behav behav2 Pair InputFile\n")
        for i, x in enumerate(dat):
            f.write("%s %s %.9g p%02d %s\n" %
                    (x[0], x[1], f10beh2[i], i // 2, x[2]))
    # Re-read the printed values and retain C's float storage/rounding in the
    # independent reference; rare null-draw rank crossings can otherwise differ.
    f10beh2 = np.asarray([float("%.9g" % x) for x in f10beh2], dtype=np.float32)
    # Rebuild the ROI means and Pearson matrices from the input arrays.  Keep
    # scalar float32 accumulation to mirror C's stored neural matrix without
    # depending on the text precision of -save_rdm near rank boundaries.
    f10atlas = np.asarray(nib.load(atlas).dataobj).reshape(-1)
    f10vol = [np.asarray(nib.load(os.path.join(work, "sub%02d.nii.gz" % i)).dataobj,
                         dtype=np.float32).reshape(NVOX, NT) for i in range(NSUB)]

    def f10_pearson(a, b):
        xm = np.float32(0); ym = np.float32(0)
        for k in range(NT):
            xm = np.float32(xm + a[k]); ym = np.float32(ym + b[k])
        xm = np.float32(xm / np.float32(NT)); ym = np.float32(ym / np.float32(NT))
        xv = np.float32(0); yv = np.float32(0); xy = np.float32(0)
        for k in range(NT):
            da = np.float32(a[k] - xm); db = np.float32(b[k] - ym)
            xv = np.float32(xv + np.float32(da * da))
            yv = np.float32(yv + np.float32(db * db))
            xy = np.float32(xy + np.float32(da * db))
        return np.float32(xy / np.sqrt(np.float32(xv * yv))) if xv > 0 and yv > 0 else np.float32(0)

    f10neu = []
    for lab in (1, 2, 3):
        vi = np.flatnonzero(f10atlas == lab)
        series = np.zeros((NSUB, NT), dtype=np.float32)
        for i in range(NSUB):
            for v in vi:
                series[i] = np.asarray(series[i] + f10vol[i][v], dtype=np.float32)
            series[i] = np.asarray(series[i] / np.float32(len(vi)), dtype=np.float32)
        S = np.eye(NSUB, dtype=np.float32)
        for i in range(NSUB):
            for j in range(i + 1, NSUB):
                S[i, j] = S[j, i] = f10_pearson(series[i], series[j])
        f10neu.append(S)

    def f10_corr(a, b):
        ra = np.asarray(rankdata(np.asarray(a, dtype=np.float32),
                                 method="average") - 1.0, dtype=np.float32)
        rb = np.asarray(rankdata(np.asarray(b, dtype=np.float32),
                                 method="average") - 1.0, dtype=np.float32)
        meanrank = np.float32(0.5 * (len(ra) - 1))
        ssa = np.float32(0); ssb = np.float32(0); dot = np.float32(0)
        for k in range(len(ra)):
            ra[k] = np.float32(ra[k] - meanrank)
            rb[k] = np.float32(rb[k] - meanrank)
            ssa = np.float32(ssa + np.float32(ra[k] * ra[k]))
            ssb = np.float32(ssb + np.float32(rb[k] * rb[k]))
        if ssa <= 0 or ssb <= 0:
            return 0.0
        for k in range(len(ra)):
            dot = np.float32(dot + np.float32(ra[k] * rb[k]))
        return float(np.float32(dot / np.sqrt(np.float32(ssa * ssb))))

    def f10_nn_pred(S, targets):
        targets = np.asarray(targets, dtype=np.float32)
        if targets.ndim == 1:
            targets = targets[None, :]
        pred = np.zeros_like(targets, dtype=np.float32)
        for i in range(NSUB):
            keep = np.arange(NSUB) != i
            w = rankdata(S[i, keep], method="average")
            for v in range(targets.shape[0]):
                pred[v, i] = np.float32(np.sum(targets[v, keep].astype(float) * w) /
                                        w.sum())
        return pred

    def f10_nn(S, targets):
        targets = np.asarray(targets, dtype=np.float32)
        if targets.ndim == 1:
            targets = targets[None, :]
        pred = f10_nn_pred(S, targets)
        return float(np.mean([f10_corr(pred[v], targets[v])
                              for v in range(targets.shape[0])]))

    def f10_annak_pred(S, target):
        target = np.asarray(target, dtype=np.float32)
        pred = np.zeros(NSUB, dtype=np.float32)
        for i in range(NSUB):
            train = np.arange(NSUB) != i
            held_typ = np.float32(np.sum(S[i, train].astype(float)) / (NSUB - 1))
            x, y = [], target[train]
            for j in np.flatnonzero(train):
                others = train.copy(); others[j] = False
                x.append(np.float32(np.sum(S[j, others].astype(float)) / (NSUB - 2)))
            x = np.asarray(x, dtype=np.float32)
            mx, my = float(np.mean(x.astype(float))), float(np.mean(y.astype(float)))
            xd = x.astype(float) - mx; yd = y.astype(float) - my
            sxx = np.sum(xd ** 2)
            slope = np.sum(xd * yd) / sxx if sxx > 0 else 0.0
            pred[i] = np.float32(my + slope * (held_typ - mx))
        return pred

    def f10_annak(S, target):
        return f10_corr(f10_annak_pred(S, target), target)

    f10perms = []
    for bits in range(1 << (NSUB // 2)):
        pi = np.arange(NSUB)
        for j in range(NSUB // 2):
            if (bits >> j) & 1:
                pi[2*j], pi[2*j+1] = pi[2*j+1], pi[2*j]
        f10perms.append(pi)
    f10null = np.empty((3, 3, len(f10perms)))
    for ri, S in enumerate(f10neu):
        for pk, pi in enumerate(f10perms):
            f10null[0, ri, pk] = f10_nn(S, f10beh[pi])
            f10null[1, ri, pk] = f10_annak(S, f10beh[pi])
            f10null[2, ri, pk] = f10_nn(S, np.vstack((f10beh[pi], f10beh2[pi])))
    f10obs = f10null[:, :, 0]
    f10p = np.mean(np.abs(f10null) >= np.abs(f10obs[:, :, None]), axis=2)
    f10max = np.max(np.abs(f10null), axis=1)
    f10pf = np.asarray([[np.mean(f10max[m] >= abs(f10obs[m, r]))
                         for r in range(3)] for m in range(3)])

    f10args = ["-dataTableFile", f10tab, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "NN", "behav:nn",
               "-model", "AK", "behav:annak",
               "-model", "MV", "behav,behav2:euclid",
               "-metric", "spearman", "-block", "Pair", "-nperm", "1024",
               "-seed", "71", "-loo", "-no_dset"]
    rc10, o10 = rsa(f10args + ["-prefix", os.path.join(work, "f10_exact")], env=env1)
    f10rows = (read_table(os.path.join(work, "f10_exact.rsa.1D"), "NN")[1]
               if rc10 == 0 else [])
    f10got = np.asarray([[row[name + "_looR"] for row in f10rows]
                         for name in ("NN", "AK", "MV")]) if f10rows else np.empty((3, 0))
    f10gotp = np.asarray([[row[name + "_looP"] for row in f10rows]
                          for name in ("NN", "AK", "MV")]) if f10rows else np.empty((3, 0))
    f10gotpf = np.asarray([[row[name + "_looPfwe"] for row in f10rows]
                           for name in ("NN", "AK", "MV")]) if f10rows else np.empty((3, 0))
    check("F10 NN/AnnaK/profile observed LOO match independent references",
          rc10 == 0 and np.allclose(f10got, f10obs, atol=3e-5),
          "rc=%d got=%s ref=%s %s" % (rc10, f10got, f10obs, o10.strip()[-120:]))
    check("F10 joint-row profile and model-aware exhaustive null p match reference",
          rc10 == 0 and np.allclose(f10gotp, f10p, atol=1.1/1024),
          "got=%s ref=%s" % (f10gotp, f10p))
    check("F10 each estimand's spatial max-FWE matches exhaustive reference",
          rc10 == 0 and np.allclose(f10gotpf, f10pf, atol=1.1/1024),
          "got=%s ref=%s" % (f10gotpf, f10pf))

    # F17: bootstrap the completed OOF prediction/target rows.  Pair is a
    # stratum, so every destination row samples only from its original pair;
    # profile measures use the same row draw and retain the mean-correlation
    # estimand.  This independently checks NN, AnnaK, and multivariate bounds.
    NBLOO, SDLOO = 401, 79
    f17ix = bootstrap_indices_stratified(np.arange(NSUB) // 2, NBLOO, SDLOO)
    f17ref = np.empty((3, 3, 2))
    for ri, S in enumerate(f10neu):
        preds = (f10_nn_pred(S, f10beh)[0],
                 f10_annak_pred(S, f10beh),
                 f10_nn_pred(S, np.vstack((f10beh, f10beh2))))
        targets = (f10beh[None, :], f10beh[None, :],
                   np.vstack((f10beh, f10beh2)))
        for mi, (pred, targ) in enumerate(zip(preds, targets)):
            if pred.ndim == 1:
                pred = pred[None, :]
            draws = [np.mean([f10_corr(pred[v, ix], targ[v, ix])
                              for v in range(pred.shape[0])])
                     for ix in f17ix if len(set(map(int, ix))) >= 3]
            f17ref[mi, ri] = percentile_linear(draws, (0.05, 0.95))
    f17args = ["-dataTableFile", f10tab, "-mask", atlas, "-mode", "IS-RSA",
               "-model", "NN", "behav:nn",
               "-model", "AK", "behav:annak",
               "-model", "MV", "behav,behav2:euclid",
               "-metric", "spearman", "-block", "Pair", "-nperm", "0",
               "-seed", str(SDLOO), "-loo", "-bootstrap", str(NBLOO),
               "-boot_ci", "90", "-no_dset",
               "-prefix", os.path.join(work, "f17_loo")]
    rc17, o17 = rsa(f17args, env=env1)
    f17rows = (read_table(os.path.join(work, "f17_loo.rsa.1D"), "NN")[1]
               if rc17 == 0 else [])
    f17got = (np.asarray([[[row[name + "_looBootLo"], row[name + "_looBootHi"]]
                           for row in f17rows] for name in ("NN", "AK", "MV")])
                if f17rows else np.empty((3, 0, 2)))
    f17meta = open(os.path.join(work, "f17_loo.rsa.1D")).read() if rc17 == 0 else ""
    check("F17 stratified fixed-OOF LOO intervals match independent reference",
          rc17 == 0 and np.allclose(f17got, f17ref, atol=3e-5) and
          "predictions held fixed" in f17meta and "not a cluster bootstrap" in f17meta,
          "rc=%d got=%s ref=%s %s" % (rc17, f17got, f17ref, o17.strip()[-140:]))

    # A one-label binary mask lets a huge sphere see exactly the atlas target at
    # every center.  Verify searchlight values/map labels and byte-identical LOO
    # inference across OpenMP worker counts.
    f10mask = os.path.join(work, "f10_mask.nii.gz")
    ai = nib.load(atlas)
    nib.save(nib.Nifti1Image((np.asarray(ai.dataobj) == 1).astype(np.int16),
                             ai.affine), f10mask)
    f10slbase = ["-dataTableFile", f10tab, "-mask", f10mask,
                 "-searchlight", "SPHERE(100)", "-mode", "IS-RSA",
                 "-model", "NN", "behav:nn",
                 "-model", "AK", "behav:annak",
                 "-model", "MV", "behav,behav2:mahal",
                 "-metric", "spearman", "-block", "Pair", "-nperm", "64",
                 "-seed", "73", "-loo", "-bootstrap", "101", "-boot_ci", "90"]
    sl10 = []
    for tag, env in (("1", env1), ("N", envN)):
        pre = os.path.join(work, "f10_sl" + tag)
        rcs, os_ = rsa(f10slbase + ["-prefix", pre], env=env)
        rows_ = read_table(pre + ".rsa.1D", "NN")[1] if rcs == 0 else []
        sl10.append((rcs, os_, rows_, head_brick_labs(pre + "+orig.HEAD") if rcs == 0 else []))
    f10keys = tuple(name + "_" + suffix for name in ("NN", "AK", "MV")
                    for suffix in ("looR", "looP", "looQ", "looPfwe",
                                   "looBootLo", "looBootHi"))
    check("F17 profile LOO bootstrap maps under searchlight with labeled bounds",
          sl10[0][0] == 0 and len(sl10[0][2]) == 80 and
          all(name in sl10[0][3] for name in
              ("NN_looR", "AK_looR", "MV_looR",
               "NN_looZFWE", "AK_looZFWE", "MV_looZFWE",
               "NN_looBootLo", "AK_looBootLo", "MV_looBootLo")),
          "rc=%d rows=%d labels=%s" % (sl10[0][0], len(sl10[0][2]), sl10[0][3]))
    check("F17 searchlight LOO bounds are thread-reproducible (1 vs %d)" % threads,
          sl10[0][0] == sl10[1][0] == 0 and len(sl10[0][2]) == len(sl10[1][2]) and
          all(all(a[k] == b[k] for k in f10keys)
              for a, b in zip(sl10[0][2], sl10[1][2])))

    # =====================================================================
    # AUDIT FIX 3: -nperm 0 IS-RSA => untyped _FZ; classic => typed _Z
    # =====================================================================
    rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
         "-model", "behav_nn", "behav:nn", "-nperm", "0",
         "-prefix", os.path.join(work, "z0"), "-quiet"])
    z0h = os.path.join(work, "z0+orig.HEAD")
    labs = head_brick_labs(z0h)
    check("FIX3 IS-RSA nperm0 second brick is _FZ",
          any(l.endswith("_FZ") for l in labs), "labs=%s" % labs)
    check("FIX3 IS-RSA nperm0 has no FIZT typing (no BRICK_STATAUX)",
          not head_attr_present(z0h, "BRICK_STATAUX"))

    # classic RSA needs a model matrix over the NT conditions
    mm = (np.array([0] * (NT // 2) + [1] * (NT - NT // 2)))
    M = (mm[:, None] != mm[None, :]).astype(float)
    np.savetxt(os.path.join(work, "cat.1D"), M, fmt="%.1f")
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                   "-model_mat", "cat", os.path.join(work, "cat.1D"), "-nperm", "0",
                   "-prefix", os.path.join(work, "rc0"), "-quiet"])
    rc0h = os.path.join(work, "rc0+orig.HEAD")
    check("FIX3 classic RSA nperm0 keeps FIZT _Z (real t-test)",
          rc == 0 and head_attr_present(rc0h, "BRICK_STATAUX")
          and any(l.endswith("_Z") for l in head_brick_labs(rc0h)))

    # Ordinary (non-runwise) classic RSA searchlight.  A whole-volume sphere
    # and a one-label atlas consume the identical condition patterns.  Compare
    # both paths with an independent condition-correlation -> Spearman ->
    # subject Fisher-z reference, verify the output records the same-data
    # estimator, and retain the shared max-FWE/thread invariants.
    osd = os.path.join(work, "ordinary_sl"); os.makedirs(osd, exist_ok=True)
    OSHAPE, OSUB, OCOND = (3, 2, 1), 8, 6
    orng = np.random.default_rng(20260825)
    omask = os.path.join(osd, "mask.nii.gz")
    nib.save(nib.Nifti1Image(np.ones(OSHAPE, dtype=np.int16), np.eye(4)), omask)
    ogrp = np.array([0, 0, 1, 1, 2, 2])
    opat = orng.normal(size=(2, 3, np.prod(OSHAPE)))
    obeh = np.linspace(-2.0, 2.0, OSUB) + 0.03 * np.arange(OSUB)
    omodel = (ogrp[:, None] != ogrp[None, :]).astype(float)
    omodfn = os.path.join(osd, "model.1D")
    np.savetxt(omodfn, omodel, fmt="%.1f")
    otab = os.path.join(osd, "table.txt")
    oz, oinner, minner, opattern, mpattern = [], [], [], [], []
    with open(otab, "w") as f:
        f.write("Subj behav ModFile InputFile\n")
        for sj in range(OSUB):
            th = (sj + 1) / (OSUB + 1) * np.pi / 2
            B = np.stack([np.cos(th) * opat[0, ogrp[c]] +
                          np.sin(th) * opat[1, ogrp[c]] +
                          0.20 * orng.normal(size=np.prod(OSHAPE))
                          for c in range(OCOND)]).astype(np.float32)
            C = (B + 0.12 * orng.normal(size=B.shape)).astype(np.float32)
            fn = os.path.join(osd, "s%02d.nii.gz" % sj)
            mn = os.path.join(osd, "m%02d.nii.gz" % sj)
            nib.save(nib.Nifti1Image(B.T.reshape(OSHAPE + (OCOND,)), np.eye(4)), fn)
            nib.save(nib.Nifti1Image(C.T.reshape(OSHAPE + (OCOND,)), np.eye(4)), mn)
            f.write("s%02d %.8g %s %s\n" % (sj, obeh[sj], mn, fn))
            oz.append(np.arctanh(spearman_tri(np.corrcoef(B), omodel)))
            oinner.append(np.corrcoef(B))
            minner.append(np.corrcoef(C))
            opattern.append(B.copy())
            mpattern.append(C.copy())
    oref = np.tanh(np.mean(oz))

    def ordinary_sl_run(pre, searchlight=False, env=None):
        aa = ["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
              "-model_mat", "condition", omodfn,
              "-metric", "spearman", "-nperm", "61", "-seed", "103",
              "-no_dset", "-prefix", os.path.join(osd, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(osd, pre + ".rsa.1D")
        rows0 = read_table(tf, "condition")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    roa, ooa, ora, oma = ordinary_sl_run("atlas", False, env1)
    ros, oos, ors, oms = ordinary_sl_run("search", True, envN)
    okeys = ("condition_r", "condition_p", "condition_q", "condition_pfwe")
    check("ordinary classic searchlight runs and labels same-data estimator",
          ros == 0 and "ordinary same-data classic-RSA searchlight" in oos and
          "# estimator: same-data condition-pattern RDM" in oms and
          "# null     : subject sign flips" in oms,
          "rc=%d %s" % (ros, oos.strip()[-180:]))
    check("ordinary classic searchlight matches independent NumPy RSA",
          len(ors) == np.prod(OSHAPE) and
          all(abs(row["condition_r"] - oref) < 1e-5 for row in ors),
          "rows=%d 3dRSA=%s numpy=%.6f" %
          (len(ors), ors[0]["condition_r"] if ors else None, oref))
    check("ordinary classic searchlight equals identical whole-atlas analysis",
          roa == ros == 0 and len(ora) == 1 and
          all(all(row[k] == ora[0][k] for k in okeys) for row in ors))

    # Long traditional-RSA input: each arbitrarily ordered row selects one
    # condition brick.  -condition_order is explicitly the row/column order of
    # the unlabeled model matrix, never an assertion about physical table order.
    olong = os.path.join(osd, "condition_long.txt")
    olong_rows = []
    for sj in range(OSUB):
        for cc in range(OCOND):
            olong_rows.append((sj, cc))
    np.random.default_rng(20260830).shuffle(olong_rows)
    with open(olong, "w") as f:
        f.write("Subj cond behav ModFile InputFile\n")
        for sj, cc in olong_rows:
            f.write("s%02d c%d %.8g %s %s[%d]\n" %
                    (sj, cc, obeh[sj], os.path.join(osd, "m%02d.nii.gz" % sj),
                     os.path.join(osd, "s%02d.nii.gz" % sj), cc))

    def condition_long_run(table_path, pre):
        aa = ["-dataTableFile", table_path, "-condition_column", "cond",
              "-condition_order", ",".join("c%d" % c for c in range(OCOND)),
              "-mask", omask, "-mode", "RSA", "-model_mat", "condition", omodfn, "-metric", "spearman", "-nperm", "61",
              "-seed", "103", "-no_dset", "-prefix", os.path.join(osd, pre)]
        rc0, out0 = rsa(aa, env=env1)
        rows0 = (read_table(os.path.join(osd, pre + ".rsa.1D"), "condition")[1]
                 if rc0 == 0 else [])
        return rc0, out0, rows0

    olrc, olout, olrows = condition_long_run(olong, "condition_long")
    oreverse = os.path.join(osd, "condition_reverse.txt")
    oreverse_lines = open(olong).read().splitlines()
    with open(oreverse, "w") as f:
        f.write("\n".join(oreverse_lines[:1] +
                          list(reversed(oreverse_lines[1:]))) + "\n")
    orrc, orout, orrows = condition_long_run(oreverse, "condition_reverse")
    check("traditional RSA accepts shuffled one-brick condition rows exactly",
          olrc == orrc == 0 and olrows == orrows == ora and
          "condition order for -model_mat: c0,c1,c2,c3,c4,c5" in olout,
          "rc=%d/%d long=%s reverse=%s wide=%s %s" %
          (olrc, orrc, olrows[:1], orrows[:1], ora[:1],
           (olout + orout).strip()[-180:]))

    odup = os.path.join(osd, "condition_duplicate.txt")
    omiss = os.path.join(osd, "condition_missing.txt")
    omulti = os.path.join(osd, "condition_multibrick.txt")
    olines = open(olong).read().splitlines()
    with open(odup, "w") as f:
        f.write("\n".join(olines + [olines[1]]) + "\n")
    with open(omiss, "w") as f:
        f.write("\n".join(olines[:-1]) + "\n")
    multi = list(olines)
    multi[1] = re.sub(r"\.nii\.gz\[\d+\]$", ".nii.gz", multi[1])
    with open(omulti, "w") as f:
        f.write("\n".join(multi) + "\n")
    drc, dout, _ = condition_long_run(odup, "condition_duplicate")
    mrc, mout, _ = condition_long_run(omiss, "condition_missing")
    brc, bout, _ = condition_long_run(omulti, "condition_multibrick")
    check("condition-table grid validation rejects duplicate and missing cells",
          drc != 0 and "duplicate key" in dout and
          mrc != 0 and "incomplete Cartesian table" in mout and "missing" in mout,
          "dup=%d missing=%d %s" % (drc, mrc, (dout + mout).strip()[-220:]))
    check("condition-table rows must resolve to exactly one brick",
          brc != 0 and "resolves to 6 bricks" in bout and "exactly one" in bout,
          "rc=%d %s" % (brc, bout.strip()[-180:]))

    check("ordinary classic searchlight FWE is valid and monotone",
          len(ors) > 0 and
          all(0 <= row["condition_p"] <= row["condition_pfwe"] <= 1 for row in ors))
    _, _, ort1, _ = ordinary_sl_run("thread1", True, env1)
    _, _, ortn, _ = ordinary_sl_run("threadN", True, envN)
    check("ordinary classic searchlight thread-reproducible (1 vs %d)" % threads,
          len(ort1) == len(ortn) == np.prod(OSHAPE) and
          all(all(a[k] == b[k] for k in okeys) for a, b in zip(ort1, ortn)))

    # S2 subject-wise condition re-meaning.  Remove the voxelwise mean over
    # conditions before constructing each subject's ordinary condition RDM.
    # Correlation and cosine are checked against independent NumPy references;
    # Euclidean condition differences must remain exactly on the legacy path.
    def s2_inner(B, metric):
        X = B - B.mean(axis=0, keepdims=True)
        if metric == "corr":
            return np.corrcoef(X)
        den = np.linalg.norm(X, axis=1)
        return (X @ X.T) / np.outer(den, den)

    s2refs = {}
    for met in ("corr", "cosine"):
        zz2 = [np.arctanh(spearman_tri(s2_inner(B, met), omodel)) for B in opattern]
        s2refs[met] = np.tanh(np.mean(zz2))

    def s2_run(pre, inner="corr", center="subject", searchlight=False, env=None):
        aa = ["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
              "-model_mat", "condition", omodfn,
              "-neural_metric", inner, "-metric", "spearman",
              "-center_conditions", center, "-nperm", "61", "-seed", "103",
              "-no_dset", "-prefix", os.path.join(osd, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(osd, pre + ".rsa.1D")
        rows0 = read_table(tf, "condition")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    s2c, s2co, s2cr, s2cm = s2_run("s2_corr", "corr", env=env1)
    s2k, s2ko, s2kr, s2km = s2_run("s2_cosine", "cosine", env=env1)
    check("S2 centered corr/cosine ordinary RDMs match independent NumPy references",
          s2c == s2k == 0 and len(s2cr) == len(s2kr) == 1 and
          abs(s2cr[0]["condition_r"] - s2refs["corr"]) < 1e-5 and
          abs(s2kr[0]["condition_r"] - s2refs["cosine"]) < 1e-5,
          "corr=%s/%.6f cosine=%s/%.6f %s" %
          (s2cr[0].get("condition_r") if s2cr else None, s2refs["corr"],
           s2kr[0].get("condition_r") if s2kr else None, s2refs["cosine"],
           (s2co + s2ko).strip()[-120:]))
    check("S2 centering provenance distinguishes subject re-meaning from raw default",
          "# condition centering: subject-wise voxel mean" in s2cm and
          "# condition centering: subject-wise voxel mean" in s2km and
          "# condition centering: none (raw-pattern compatibility default)" in oma)

    s2n, _, s2nr, _ = s2_run("s2_none", "corr", "none", env=env1)
    check("S2 explicit none preserves the compatibility default exactly",
          s2n == roa == 0 and len(s2nr) == len(ora) == 1 and s2nr == ora)

    s2ea, _, s2eraw, _ = s2_run("s2_euclid_raw", "euclid", "none", env=env1)
    s2eb, _, s2ecen, s2em = s2_run("s2_euclid_center", "euclid", "subject", env=env1)
    check("S2 Euclidean ordinary RDM inference is exactly centering-invariant",
          s2ea == s2eb == 0 and s2eraw == s2ecen and
          "Euclidean distances are invariant (exact legacy computation retained)" in s2em)

    s2sa, _, s2sar, _ = s2_run("s2_center_atlas", "corr", "subject", env=env1)
    s2ss, _, s2ssr, _ = s2_run("s2_center_search", "corr", "subject", True, envN)
    check("S2 centered ordinary searchlight equals the whole-volume atlas reference",
          s2sa == s2ss == 0 and len(s2sar) == 1 and
          len(s2ssr) == np.prod(OSHAPE) and
          all(all(row[k] == s2sar[0][k] for k in okeys) for row in s2ssr))
    _, _, s2t1, _ = s2_run("s2_center_t1", "corr", "subject", True, env1)
    _, _, s2tn, _ = s2_run("s2_center_tN", "corr", "subject", True, envN)
    check("S2 centered searchlight is thread-reproducible (1 vs %d)" % threads,
          len(s2t1) == len(s2tn) == np.prod(OSHAPE) and s2t1 == s2tn)
    s2bi, s2bio = rsa([
        "-dataTableFile", otab, "-mask", omask, "-mode", "IS-RSA",
        "-model", "behav_nn", "behav:nn", "-center_conditions", "subject", "-nperm", "20",
        "-no_dset", "-prefix", os.path.join(osd, "s2_bad_is")])
    s2bv, s2bvo = rsa([
        "-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
        "-model_mat", "condition", omodfn, "-center_conditions", "partition", "-nperm", "20",
        "-no_dset", "-prefix", os.path.join(osd, "s2_bad_value")])
    check("S2 invalid feature and option-value contracts are explicit",
          s2bi != 0 and "needs an ordinary condition" in s2bio and
          s2bv != 0 and "must be 'none' or 'subject'" in s2bvo)

    # S1 fixed-effects classic inference: enumerate all 6! condition label
    # permutations independently.  The statistic is mean subject Fisher z;
    # one relabeling is shared across subjects and, for a contrast, both models.
    # The whole-volume sphere makes every center identical, so its exact
    # max-stat FWE p must equal the atlas/raw p as well.
    operms = list(itertools.permutations(range(OCOND)))

    def s1_stat(neurals, A, B=None, perm=None):
        ix = np.arange(OCOND) if perm is None else np.asarray(perm)
        Ap = A[np.ix_(ix, ix)]
        Bp = None if B is None else B[np.ix_(ix, ix)]
        z = []
        for N in neurals:
            za = np.arctanh(np.clip(spearman_tri(N, Ap), -.999329, .999329))
            zb = 0.0 if Bp is None else np.arctanh(
                np.clip(spearman_tri(N, Bp), -.999329, .999329))
            z.append(za - zb)
        return float(np.mean(z))

    s1null = np.array([s1_stat(oinner, omodel, perm=p) for p in operms])
    s1obs = s1_stat(oinner, omodel)
    s1p = np.mean(np.abs(s1null) >= abs(s1obs) - 1e-12)

    def s1_run(pre, searchlight=False, env=None, tablefile=otab, extra=()):
        aa = ["-dataTableFile", tablefile, "-mask", omask, "-mode", "RSA",
              "-model_mat", "condition", omodfn,
              "-metric", "spearman", "-classic_null", "conditions",
              "-nperm", "720", "-seed", "307", "-no_dset",
              "-prefix", os.path.join(osd, pre)] + list(extra)
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(osd, pre + ".rsa.1D")
        rows0 = read_table(tf, "condition")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    rs1a, os1a, s1a, ms1a = s1_run("s1_atlas", False, env1)
    rs1s, os1s, s1s, ms1s = s1_run("s1_search", True, envN)
    check("S1 classic condition-null matches exhaustive NumPy effect and p",
          rs1a == 0 and len(s1a) == 1 and
          abs(s1a[0]["condition_r"] - np.tanh(s1obs)) < 1e-5 and
          abs(s1a[0]["condition_p"] - s1p) <= 1.0 / 720 + 1e-9,
          "rc=%d r/p=%s ref=%.6f/%.6g %s" %
          (rs1a, ((s1a[0]["condition_r"], s1a[0]["condition_p"])
                  if s1a else None), np.tanh(s1obs), s1p, os1a.strip()[-120:]))
    check("S1 condition-null provenance states fixed subjects and mean Fisher z",
          "# null     : condition labels (fixed observed subjects)" in ms1a and
          "# classic condition-null statistic: mean subject Fisher z" in ms1a)
    check("S1 condition-null searchlight equals atlas with exact max-FWE",
          rs1s == 0 and len(s1s) == np.prod(OSHAPE) and
          all(all(row[k] == s1a[0][k] for k in okeys) and
              row["condition_pfwe"] == row["condition_p"] for row in s1s))
    _, _, s1t1, _ = s1_run("s1_thread1", True, env1)
    _, _, s1tn, _ = s1_run("s1_threadN", True, envN)
    check("S1 condition-null searchlight thread-reproducible (1 vs %d)" % threads,
          len(s1t1) == len(s1tn) == np.prod(OSHAPE) and
          all(all(a[k] == b[k] for k in okeys) for a, b in zip(s1t1, s1tn)))

    # One observed subject is a legitimate fixed-effects sample.  The default
    # population-subject null must reject it instead of silently changing scope.
    otab1 = os.path.join(osd, "table_one.txt")
    with open(otab) as fi, open(otab1, "w") as fo:
        lines = [ln for ln in fi if ln.strip()]
        fo.writelines(lines[:2])
    s1one_null = np.array([s1_stat(oinner[:1], omodel, perm=p) for p in operms])
    s1one_obs = s1_stat(oinner[:1], omodel)
    s1one_p = np.mean(np.abs(s1one_null) >= abs(s1one_obs) - 1e-12)
    rs1o, os1o, s1o, _ = s1_run("s1_one", False, env1, otab1)
    check("S1 single-subject fixed-effects inference matches exhaustive reference",
          rs1o == 0 and len(s1o) == 1 and
          abs(s1o[0]["condition_r"] - np.tanh(s1one_obs)) < 1e-5 and
          abs(s1o[0]["condition_p"] - s1one_p) <= 1.0 / 720 + 1e-9,
          "rc=%d got=%s ref=%.6f/%.6g %s" %
          (rs1o, ((s1o[0]["condition_r"], s1o[0]["condition_p"])
                  if s1o else None), np.tanh(s1one_obs), s1one_p,
           os1o.strip()[-120:]))
    rbad1, obad1 = rsa(["-dataTableFile", otab1, "-mask", omask, "-mode", "RSA",
                        "-model_mat", "condition", omodfn, "-nperm", "20", "-no_dset",
                        "-prefix", os.path.join(osd, "s1_bad_subjects")])
    check("S1 single-subject population null is rejected explicitly",
          rbad1 != 0 and "needs at least 2 independent" in obad1)

    # Paired fixed-model contrast: same permutation on A and B.
    ogrp2 = np.array([0, 1, 0, 1, 2, 2])
    omodel2 = (ogrp2[:, None] != ogrp2[None, :]).astype(float)
    omodfn2 = os.path.join(osd, "model2.1D")
    np.savetxt(omodfn2, omodel2, fmt="%.1f")
    s1cnull = np.array([s1_stat(oinner, omodel, omodel2, p) for p in operms])
    s1cobs = s1_stat(oinner, omodel, omodel2)
    s1cp = np.mean(np.abs(s1cnull) >= abs(s1cobs) - 1e-12)
    s1crdiff = np.mean([
        spearman_tri(N, omodel) - spearman_tri(N, omodel2) for N in oinner
    ])
    extra = ("-model_mat", "alternate", omodfn2,
             "-model_contrast", "condition-alternate",
             "-contrast_hypothesis", "alignment")
    rs1c, os1c, s1c, ms1c = s1_run("s1_contrast", False, env1, otab, extra)
    check("S1 paired contrast matches exhaustive shared-condition null",
          rs1c == 0 and len(s1c) == 1 and
          abs(s1c[0]["condition-alternate_zDiff"] - s1cobs) < 1e-5 and
          abs(s1c[0]["condition-alternate_rDiff"] - s1crdiff) < 1e-5 and
          abs(s1c[0]["condition-alternate_p"] - s1cp) <= 1.0 / 720 + 1e-9,
          "rc=%d got=%s ref=%.6f/%.6f/%.6g %s" %
          (rs1c, ((s1c[0]["condition-alternate_zDiff"],
                   s1c[0]["condition-alternate_rDiff"],
                   s1c[0]["condition-alternate_p"]) if s1c else None),
           s1cobs, s1crdiff, s1cp, os1c.strip()[-120:]))
    check("S1 alignment contrast provenance names its sharp null and both effects",
          "# contrast hypothesis: alignment" in ms1c and
          "# contrast estimand: zDiff=mean subject Fisher-z difference; "
          "rDiff=mean subject raw-correlation difference" in ms1c and
          "# contrast null construction: shared condition-label relabeling "
          "(sharp alignment null; not an equal-performance null)" in ms1c)
    rs1sup, os1sup = s1_run(
        "s1_bad_superiority", False, env1, otab,
        ("-model_mat", "alternate", omodfn2,
         "-model_contrast", "condition-alternate",
         "-contrast_hypothesis", "superiority"))[:2]
    check("S1 fixed-condition superiority rejects the alignment permutation",
          rs1sup != 0 and "does not test equal nonzero model performance" in os1sup)

    # First-contract exclusions are explicit rather than silently borrowing a
    # global complete-null interpretation for conditional regression effects.
    rbj, obj = s1_run("s1_bad_joint", False, env1, otab,
                      ("-model_mat", "alternate", omodfn2,
                       "-model_joint"))[:2]
    rbi, obi = rsa(["-dataTableFile", otab, "-mask", omask, "-mode", "IS-RSA",
                    "-model", "behav_nn", "behav:nn", "-classic_null", "conditions",
                    "-nperm", "20", "-no_dset",
                    "-prefix", os.path.join(osd, "s1_bad_is")])
    rbg, obg = s1_run("s1_bad_group", False, env1, otab,
                      ("-group_test", "signedrank"))[:2]
    check("S1 invalid joint/IS/signed-rank combinations are rejected",
          rbj != 0 and "does not yet support" in obj and
          rbi != 0 and "applies only to classic" in obi and
          rbg != 0 and "cannot be combined" in obg)

    # S5 seed representational connectivity.  Use a one-dimensional spatial
    # fixture so seed/target voxel membership has no array-order ambiguity.
    # The seed has two voxels and the target four; both are built from the same
    # subjects, condition order, feature metric, and preprocessing contract.
    scd = os.path.join(osd, "seed_conn"); os.makedirs(scd, exist_ok=True)
    scshape = (6, 1, 1)
    scseed = np.zeros(scshape, np.int16); scseed[:2, 0, 0] = 7
    sctarget = np.zeros(scshape, np.int16); sctarget[2:, 0, 0] = 1
    scover = np.ones(scshape, np.int16); scover[:2, 0, 0] = 7
    scmulti = np.zeros(scshape, np.int16)
    scmulti[0, 0, 0] = 7; scmulti[1, 0, 0] = 8
    scseedfn = os.path.join(scd, "seed.nii.gz")
    sctargetfn = os.path.join(scd, "target.nii.gz")
    scoverfn = os.path.join(scd, "overlap_target.nii.gz")
    scmfn = os.path.join(scd, "multi_seed.nii.gz")
    for arr, fn in ((scseed, scseedfn), (sctarget, sctargetfn),
                    (scover, scoverfn), (scmulti, scmfn)):
        nib.save(nib.Nifti1Image(arr, np.eye(4)), fn)
    sctab = os.path.join(scd, "table.txt")
    with open(sctab, "w") as f:
        f.write("Subj Pair InputFile\n")
        for sj, B in enumerate(opattern):
            fn = os.path.join(scd, "s%02d.nii.gz" % sj)
            nib.save(nib.Nifti1Image(B.T.reshape(scshape + (OCOND,)), np.eye(4)), fn)
            f.write("s%02d p%d %s\n" % (sj, sj // 2, fn))

    iu_sc = np.triu_indices(OCOND, 1)

    def sc_euclid(B):
        d = B[:, None, :] - B[None, :, :]
        return np.sqrt(np.sum(d * d, axis=2))

    def sc_pearson_tri(A, B):
        a, b = np.asarray(A)[np.triu_indices(A.shape[0], 1)], \
               np.asarray(B)[np.triu_indices(B.shape[0], 1)]
        a = a - a.mean(); b = b - b.mean()
        den = np.sqrt(np.dot(a, a) * np.dot(b, b))
        return float(np.dot(a, b) / den) if den > 0 else 0.0

    scseedrdm = [sc_euclid(B[:, :2]) for B in opattern]
    sctargrdm = [sc_euclid(B[:, 2:]) for B in opattern]
    scz = [np.arctanh(np.clip(sc_pearson_tri(T, S), -.999329, .999329))
           for S, T in zip(scseedrdm, sctargrdm)]
    scobs = float(np.mean(scz)); scref = float(np.tanh(scobs))
    scnull = []
    for pi in operms:
        ix = np.asarray(pi)
        z = [np.arctanh(np.clip(sc_pearson_tri(T, S[np.ix_(ix, ix)]),
                                -.999329, .999329))
             for S, T in zip(scseedrdm, sctargrdm)]
        scnull.append(np.mean(z))
    scp = float(np.mean(np.abs(scnull) >= abs(scobs) - 1e-12))

    def sc_classic(pre, search=False, env=None, mask=sctargetfn, extra=()):
        aa = ["-dataTableFile", sctab, "-mask", mask, "-seed_mask", scseedfn,
              "-mode", "RSA", "-neural_metric", "euclid", "-metric", "pearson",
              "-classic_null", "conditions", "-nperm", "720", "-seed", "409",
              "-no_dset", "-prefix", os.path.join(scd, pre)] + list(extra)
        if search: aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(scd, pre + ".rsa.1D")
        rows0 = read_table(tf, "seedROI7")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    sca = sc_classic("classic_atlas", False, env1,
                     extra=("-save_rdm", os.path.join(scd, "saved")))
    scs = sc_classic("classic_search", True, envN)
    sck = ("seedROI7_r", "seedROI7_p", "seedROI7_q", "seedROI7_pfwe")
    check("S5 classic seed connectivity matches exhaustive condition-null reference",
          sca[0] == 0 and len(sca[2]) == 1 and
          abs(sca[2][0]["seedROI7_r"] - scref) < 2e-5 and
          abs(sca[2][0]["seedROI7_p"] - scp) <= 1.0 / 720 + 1e-9 and
          sca[2][0]["seedROI7_pfwe"] == sca[2][0]["seedROI7_p"],
          "rc=%d got=%s ref=%.6f/%.6g %s" %
          (sca[0], sca[2][0] if sca[2] else None, scref, scp, sca[1][-120:]))
    check("S5 seed provenance and subject-specific saved RDMs are explicit",
          "# representational connectivity: fixed seed ROI" in sca[3] and
          "# seed estimand: subject-specific seed vs target" in sca[3] and
          "# seed condition null: relabel seed condition axes" in sca[3] and
          all(os.path.exists(os.path.join(scd, "saved_seed_subj%04d.1D" % sj))
              for sj in range(OSUB)) and
          np.allclose(np.loadtxt(os.path.join(scd, "saved_seed_subj0000.1D")),
                      scseedrdm[0], atol=2e-5))
    check("S5 classic seed searchlight equals atlas with joint spatial max-FWE",
          scs[0] == 0 and len(scs[2]) == 4 and
          all(all(row[k] == sca[2][0][k] for k in sck) for row in scs[2]))
    sct1 = sc_classic("classic_t1", True, env1)
    sctn = sc_classic("classic_tN", True, envN)
    check("S5 classic seed searchlight is thread-reproducible (1 vs %d)" % threads,
          sct1[0] == sctn[0] == 0 and sct1[2] == sctn[2])

    # IS-RSA feature-pattern seed connectivity, with four two-subject blocks.
    # The exact group has 2^4 relabelings; independently enumerate it.
    Sfeat = np.asarray([B[:, :2].reshape(-1) for B in opattern])
    Tfeat = np.asarray([B[:, 2:].reshape(-1) for B in opattern])
    Smat, Tmat = np.corrcoef(Sfeat), np.corrcoef(Tfeat)
    isobs = sc_pearson_tri(Tmat, Smat)
    isnull = []
    for bits in range(1 << (OSUB // 2)):
        pi = np.arange(OSUB)
        for j in range(OSUB // 2):
            if (bits >> j) & 1:
                pi[2*j], pi[2*j+1] = pi[2*j+1], pi[2*j]
        isnull.append(sc_pearson_tri(Tmat, Smat[np.ix_(pi, pi)]))
    isp = float(np.mean(np.abs(isnull) >= abs(isobs) - 1e-12))

    def sc_is(pre, search=False, env=None):
        aa = ["-dataTableFile", sctab, "-mask", sctargetfn,
              "-seed_mask", scseedfn, "-mode", "IS-RSA",
              "-featuretype", "pattern", "-neural_metric", "corr",
              "-metric", "pearson", "-block", "Pair", "-nperm", "50",
              "-seed", "419", "-no_dset", "-prefix", os.path.join(scd, pre)]
        if search: aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(scd, pre + ".rsa.1D")
        rows0 = read_table(tf, "seedROI7")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    sia = sc_is("is_atlas", False, env1)
    sis = sc_is("is_search", True, envN)
    check("S5 IS-RSA seed connectivity matches exhaustive blocked Mantel reference",
          sia[0] == 0 and len(sia[2]) == 1 and
          abs(sia[2][0]["seedROI7_r"] - isobs) < 2e-5 and
          abs(sia[2][0]["seedROI7_p"] - isp) < 1e-12,
          "rc=%d got=%s ref=%.6f/%.6g %s" %
          (sia[0], sia[2][0] if sia[2] else None, isobs, isp, sia[1][-120:]))
    check("S5 IS-RSA seed atlas/searchlight and FWE families agree",
          sis[0] == 0 and len(sis[2]) == 4 and
          all(abs(row["seedROI7_r"] - sia[2][0]["seedROI7_r"]) < 2e-6 and
              all(row[k] == sia[2][0][k]
                  for k in ("seedROI7_p", "seedROI7_q", "seedROI7_pfwe"))
              for row in sis[2]) and
          "seed vs target subject-geometry Mantel" in sia[3])
    si1 = sc_is("is_t1", True, env1); sin = sc_is("is_tN", True, envN)
    check("S5 IS-RSA seed searchlight is thread-reproducible (1 vs %d)" % threads,
          si1[0] == sin[0] == 0 and si1[2] == sin[2])

    sco = sc_classic("overlap", False, env1, mask=scoverfn)
    check("S5 overlap policy removes seed-sharing targets before inference",
          sco[0] == 0 and len(sco[2]) == 1 and sco[2][0]["ROI"] == 1 and
          "excluded 1 location before inference" in sco[3] and
          "family contains 1 non-overlapping target" in sco[3],
          "rc=%d rows=%s %s" % (sco[0], sco[2], sco[1][-120:]))

    rbm, obm = rsa(["-dataTableFile", sctab, "-mask", sctargetfn,
                    "-seed_mask", scseedfn, "-mode", "IS-RSA",
                    "-model_mat", "condition", omodfn, "-nperm", "20",
                    "-prefix", os.path.join(scd, "bad_mixed")])
    rbr, obr = rsa(["-dataTableFile", sctab, "-mask", sctargetfn,
                    "-seed_mask", scmfn, "-mode", "IS-RSA", "-nperm", "20",
                    "-prefix", os.path.join(scd, "bad_multi")])
    rbs, obs_ = rsa(["-dataTableFile", sctab, "-mask", sctargetfn,
                     "-seed_roi", "7", "-mode", "IS-RSA", "-nperm", "20",
                     "-prefix", os.path.join(scd, "bad_selector")])
    rbc, obc = rsa(["-dataTableFile", sctab, "-mask", sctargetfn,
                    "-seed_mask", scseedfn, "-mode", "RSA",
                    "-cond_bootstrap", "20", "-nperm", "20",
                    "-prefix", os.path.join(scd, "bad_cboot")])
    check("S5 mixed/multiple/orphan-selector/condition-bootstrap contracts reject",
          rbm != 0 and "defines the one" in obm and
          rbr != 0 and "selected 2 ROIs" in obr and
          rbs != 0 and "give both options" in obs_ and
          rbc != 0 and "not yet defined for seed connectivity" in obc)

    # F7 constrained fitted components.  This independent implementation uses
    # the documented standardized nonnegative ridge objective and outer
    # held-subject folds.  It intentionally never calls scipy.optimize (or any
    # 3dRSA helper), so an accidental train/test leak changes the answer.
    def f7_solve(X, y, ridge=0.01):
        xm, xs = X.mean(0), X.std(0)
        ym, ys = y.mean(), y.std()
        Xz, yz = (X - xm) / xs, (y - ym) / ys
        w = np.zeros(X.shape[1]); pred = np.zeros(len(y))
        for _ in range(1000):
            md = 0.0
            for c in range(X.shape[1]):
                rho = np.dot(Xz[:, c], yz - pred + Xz[:, c] * w[c])
                nw = max(0.0, rho / (np.dot(Xz[:, c], Xz[:, c]) + ridge * len(y)))
                pred += Xz[:, c] * (nw - w[c]); md = max(md, abs(nw - w[c])); w[c] = nw
            if md < 1e-6: break
        return w, xm, xs

    def f7_ref_classic(neurals, comps, ridge=0.01):
        iu = np.triu_indices(comps[0].shape[0], 1)
        cx = np.column_stack([x[iu] for x in comps])
        z, ww = [], []
        for hold in range(len(neurals)):
            y = np.concatenate([neurals[s][iu] for s in range(len(neurals)) if s != hold])
            X = np.tile(cx, (len(neurals) - 1, 1))
            w, xm, xs = f7_solve(X, y, ridge)
            z.append(np.arctanh(np.clip(np.corrcoef(neurals[hold][iu],
                                                    ((cx - xm) / xs).dot(w))[0, 1],
                                          -0.999329, 0.999329)))
            ww.append(w / w.sum() if w.sum() else w)
        return np.tanh(np.mean(z)), np.mean(ww, axis=0)

    def f7_foldz_classic(neurals, comps, ridge=0.01):
        """Independent outer-subject fold scores retained for superiority."""
        iu = np.triu_indices(comps[0].shape[0], 1)
        cx = np.column_stack([x[iu] for x in comps]); z = []
        for hold in range(len(neurals)):
            y = np.concatenate([neurals[s][iu] for s in range(len(neurals)) if s != hold])
            X = np.tile(cx, (len(neurals) - 1, 1))
            w, xm, xs = f7_solve(X, y, ridge)
            rr = np.corrcoef(neurals[hold][iu], ((cx - xm) / xs).dot(w))[0, 1]
            z.append(np.arctanh(np.clip(rr if np.isfinite(rr) else 0.0,
                                       -0.999329, 0.999329)))
        return np.asarray(z)

    f7grp = np.array([0, 1, 0, 1, 2, 2])
    f7alt = (f7grp[:, None] != f7grp[None, :]).astype(float)
    f7altfn = os.path.join(osd, "f7_alt.1D"); np.savetxt(f7altfn, f7alt, fmt="%.1f")
    f7grp2 = np.array([0, 1, 2, 0, 2, 1])
    f7alt2 = (f7grp2[:, None] != f7grp2[None, :]).astype(float)
    f7alt2fn = os.path.join(osd, "f7_alt2.1D"); np.savetxt(f7alt2fn, f7alt2, fmt="%.1f")
    f7ref, f7wref = f7_ref_classic(oinner, [omodel, f7alt])
    f14badref, _ = f7_ref_classic(oinner, [f7alt, f7alt2])
    f14ref = np.arctanh(np.clip(f7ref, -0.999329, 0.999329)) - \
             np.arctanh(np.clip(f14badref, -0.999329, 0.999329))
    f7base = ["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
              "-model_mat", "TRUE", omodfn,
              "-model_mat", "ALT", f7altfn,
              "-model_mat", "ALT2", f7alt2fn,
              "-metric", "pearson", "-model_fit", "MIX=TRUE,ALT",
              "-model_fit", "NULLFIT=ALT,ALT2", "-model_contrast", "MIX-NULLFIT",
              "-fit_ridge", "0.01", "-nperm", "37", "-seed", "211", "-quiet"]
    f7runs = []
    for tag, env in (("1", env1), ("N", envN)):
        pre = os.path.join(osd, "f7_" + tag)
        rc7, out7 = rsa(f7base + ["-prefix", pre], env=env)
        row7 = read_table(pre + ".rsa.1D", "TRUE")[1] if rc7 == 0 else []
        f7runs.append((rc7, out7, row7,
                       head_brick_labs(pre + "+orig.HEAD") if rc7 == 0 else []))
    f7row = f7runs[0][2][0] if len(f7runs[0][2]) == 1 else {}
    check("F7 classic nested fit matches independent CV/weight reference",
          f7runs[0][0] == 0 and abs(f7row.get("MIX_cvR", 9) - f7ref) < 2e-5 and
          np.allclose([f7row.get("MIX_w_TRUE", 9), f7row.get("MIX_w_ALT", 9)],
                      f7wref, atol=2e-5),
          "3dRSA=%s weights=%s ref=%.6f/%s %s" %
          (f7row.get("MIX_cvR"), [f7row.get("MIX_w_TRUE"), f7row.get("MIX_w_ALT")],
           f7ref, f7wref, f7runs[0][1].strip()[-100:]))
    f7keys = ("MIX_cvR", "MIX_cvP", "MIX_cvQ", "MIX_cvPfwe",
              "MIX_w_TRUE", "MIX_w_ALT")
    check("F7 maps carry CV/FWE/weight diagnostics",
          all(k in f7runs[0][3] for k in
              ("MIX_cvR", "MIX_cvZ", "MIX_cvZFWE", "MIX_w_TRUE", "MIX_w_ALT")),
          "labels=%s" % f7runs[0][3])
    check("F14 fitted contrast matches paired held-fold Fisher-z reference",
          f7runs[0][0] == 0 and
          abs(f7row.get("MIX-NULLFIT_cvDiff", 9) - f14ref) < 3e-5 and
          0 <= f7row.get("MIX-NULLFIT_cvP", -1) <=
          f7row.get("MIX-NULLFIT_cvPfwe", -1) <= 1,
          "3dRSA=%s p/pfwe=%s/%s ref=%.6f %s" %
          (f7row.get("MIX-NULLFIT_cvDiff"), f7row.get("MIX-NULLFIT_cvP"),
           f7row.get("MIX-NULLFIT_cvPfwe"), f14ref, f7runs[0][1].strip()[-100:]))
    check("F14 maps carry paired CV difference/FWE diagnostics",
          all(k in f7runs[0][3] for k in
              ("MIX-NULLFIT_cvDiff", "MIX-NULLFIT_cvZdiff",
               "MIX-NULLFIT_cvZdiffFWE")),
          "labels=%s" % f7runs[0][3])
    check("F7 classic fit is thread-reproducible (1 vs %d)" % threads,
          f7runs[0][0] == f7runs[1][0] == 0 and
          all(a[k] == b[k] for a, b in zip(f7runs[0][2], f7runs[1][2]) for k in f7keys))
    f14keys = ("MIX-NULLFIT_cvDiff", "MIX-NULLFIT_cvP",
               "MIX-NULLFIT_cvQ", "MIX-NULLFIT_cvPfwe")
    check("F14 fitted contrast is thread-reproducible (1 vs %d)" % threads,
          f7runs[0][0] == f7runs[1][0] == 0 and
          all(a[k] == b[k] for a, b in zip(f7runs[0][2], f7runs[1][2]) for k in f14keys))

    # Explicit fitted superiority: form one paired common-fold difference per
    # outer subject, then mirror the ordinary (non-rejection) subject bootstrap.
    f7dz = f7_foldz_classic(oinner, [omodel, f7alt]) - \
           f7_foldz_classic(oinner, [f7alt, f7alt2])
    f7dobs = float(np.mean(f7dz))
    f7bd = np.asarray([np.mean(f7dz[ix]) for ix in bootstrap_indices(OSUB, 37, 211)])
    f7bp = (1 + np.sum(np.abs(f7bd - f7dobs) >= abs(f7dobs))) / 38.0
    f7sup = []
    for tag, env in (("1", env1), ("N", envN)):
        pre = os.path.join(osd, "f14_sup_" + tag)
        rc0, out0 = rsa(f7base + ["-contrast_hypothesis", "superiority", "-prefix", pre], env=env)
        row0 = read_table(pre + ".rsa.1D", "TRUE")[1] if rc0 == 0 else []
        meta0 = open(pre + ".rsa.1D").read() if rc0 == 0 else ""
        f7sup.append((rc0, out0, row0, meta0))
    fsrow = f7sup[0][2][0] if len(f7sup[0][2]) == 1 else {}
    check("F14 fitted superiority matches common-fold centered subject bootstrap",
          f7sup[0][0] == 0 and abs(fsrow.get("MIX-NULLFIT_cvDiff", 9) - f7dobs) < 3e-5 and
          abs(fsrow.get("MIX-NULLFIT_cvP", -1) - f7bp) < 5e-7 and
          abs(fsrow.get("MIX-NULLFIT_cvPfwe", -1) - f7bp) < 5e-7 and
          "same folds valid for both models" in f7sup[0][3] and
          "centered paired outer-subject bootstrap" in f7sup[0][3] and
          "(1 + exceedances)/(1 + draws)" in f7sup[0][3],
          "got=%s p=%s/%s ref=%.6f/%.7g %s" %
          (fsrow.get("MIX-NULLFIT_cvDiff"), fsrow.get("MIX-NULLFIT_cvP"),
           fsrow.get("MIX-NULLFIT_cvPfwe"), f7dobs, f7bp, f7sup[0][1].strip()[-120:]))
    check("F14 fitted superiority is thread-reproducible (1 vs %d)" % threads,
          f7sup[0][0] == f7sup[1][0] == 0 and f7sup[0][2] == f7sup[1][2])

    # F22 strict two-axis fitted-model CV.  For every held subject and explicit
    # condition fold, training contains only other-subject train/train dyads;
    # scoring contains only held-subject held/held dyads.  Cross-boundary dyads
    # are absent from both arrays.
    def f22_ref_classic(neurals, comps, fold, ridge=0.01):
        fold = np.asarray(fold); z, ww = [], []
        for hold in range(len(neurals)):
            for hf in np.unique(fold):
                trc = np.flatnonzero(fold != hf); tec = np.flatnonzero(fold == hf)
                tr = [(a, b) for ii, a in enumerate(trc) for b in trc[ii + 1:]]
                te = [(a, b) for ii, a in enumerate(tec) for b in tec[ii + 1:]]
                X0 = np.asarray([[x[a, b] for x in comps] for a, b in tr])
                X = np.tile(X0, (len(neurals) - 1, 1))
                y = np.concatenate([np.asarray([neurals[s][a, b] for a, b in tr])
                                    for s in range(len(neurals)) if s != hold])
                w, xm, xs = f7_solve(X, y, ridge)
                Xt = np.asarray([[x[a, b] for x in comps] for a, b in te])
                yt = np.asarray([neurals[hold][a, b] for a, b in te])
                yp = ((Xt - xm) / xs).dot(w)
                rr = 0.0 if np.std(yt) == 0 or np.std(yp) == 0 else np.corrcoef(yt, yp)[0, 1]
                z.append(np.arctanh(np.clip(rr, -0.999329, 0.999329)))
                ww.append(w / w.sum() if w.sum() else w)
        return np.tanh(np.mean(z)), np.mean(ww, axis=0)

    f22fold = np.array([0, 0, 0, 1, 1, 1])
    f22foldfn = os.path.join(osd, "f22_fold.txt")
    with open(f22foldfn, "w") as f:
        f.write("# held-condition folds in model order\nA\nA\nA\nB\nB\nB\n")
    f22rng = np.random.default_rng(20260828)
    f22noise = f22rng.normal(size=(OCOND, OCOND))
    f22noise = 0.5 * (f22noise + f22noise.T); np.fill_diagonal(f22noise, 0.0)
    f22noisefn = os.path.join(osd, "f22_noise.1D")
    np.savetxt(f22noisefn, f22noise, fmt="%.9g")
    f22ref, f22wref = f22_ref_classic(oinner, [omodel, f7alt], f22fold)
    f22badref, _ = f22_ref_classic(oinner, [f7alt, f22noise], f22fold)
    f22diffref = np.arctanh(np.clip(f22ref, -0.999329, 0.999329)) - \
                 np.arctanh(np.clip(f22badref, -0.999329, 0.999329))
    f22base = ["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
               "-model_mat", "TRUE", omodfn,
               "-model_mat", "ALT", f7altfn,
               "-model_mat", "NOISE", f22noisefn,
               "-metric", "pearson", "-model_fit", "MIX=TRUE,ALT",
               "-model_fit", "BAD=ALT,NOISE", "-model_contrast", "MIX-BAD",
               "-fit_condfold", f22foldfn, "-fit_ridge", "0.01",
               "-nperm", "37", "-seed", "223", "-quiet"]

    f22out = []
    for tag, search, env in (("atlas", False, env1), ("sl1", True, env1),
                             ("slN", True, envN)):
        pre = os.path.join(osd, "f22_" + tag)
        aa = f22base + (["-searchlight", "SPHERE(100)"] if search else []) + ["-prefix", pre]
        rc22, out22 = rsa(aa, env=env)
        rows22 = read_table(pre + ".rsa.1D", "TRUE")[1] if rc22 == 0 else []
        meta22 = open(pre + ".rsa.1D").read() if rc22 == 0 else ""
        labs22 = head_brick_labs(pre + "+orig.HEAD") if rc22 == 0 else []
        f22out.append((rc22, out22, rows22, meta22, labs22))
    f22row = f22out[0][2][0] if len(f22out[0][2]) == 1 else {}
    check("F22 held-condition CV matches independent effect/weight reference",
          f22out[0][0] == 0 and abs(f22row.get("MIX_cvR", 9) - f22ref) < 3e-5 and
          np.allclose([f22row.get("MIX_w_TRUE", 9), f22row.get("MIX_w_ALT", 9)],
                      f22wref, atol=3e-5) and
          "train=train/train, test=held/held; cross-boundary dyads excluded" in f22out[0][3] and
          "A:3 B:3" in f22out[0][3],
          "got=%s/%s ref=%.6f/%s %s" %
          (f22row.get("MIX_cvR"), [f22row.get("MIX_w_TRUE"), f22row.get("MIX_w_ALT")],
           f22ref, f22wref, f22out[0][1].strip()[-120:]))
    check("F22 paired fitted comparison uses the same subject x condition folds",
          abs(f22row.get("MIX-BAD_cvDiff", 9) - f22diffref) < 4e-5 and
          0 <= f22row.get("MIX-BAD_cvP", -1) <=
          f22row.get("MIX-BAD_cvPfwe", -1) <= 1,
          "got=%s ref=%.6f" % (f22row.get("MIX-BAD_cvDiff"), f22diffref))
    f22keys = ("MIX_cvR", "MIX_cvP", "MIX_cvQ", "MIX_cvPfwe",
               "MIX_w_TRUE", "MIX_w_ALT", "MIX-BAD_cvDiff",
               "MIX-BAD_cvP", "MIX-BAD_cvQ", "MIX-BAD_cvPfwe")
    check("F22 atlas/searchlight effects agree and threads are identical",
          all(x[0] == 0 for x in f22out) and len(f22out[1][2]) == len(f22out[2][2]) == np.prod(OSHAPE) and
          all(all(abs(row[k] - f22row[k]) < 2e-6 for k in f22keys)
              for row in f22out[1][2]) and
          all(a[k] == b[k] for a, b in zip(f22out[1][2], f22out[2][2]) for k in f22keys) and
          all(k in f22out[2][4] for k in
              ("MIX_cvR", "MIX_cvZ", "MIX_cvZFWE", "MIX-BAD_cvDiff",
               "MIX-BAD_cvZdiffFWE")),
          "rows=%d/%d labs=%s" % (len(f22out[1][2]), len(f22out[2][2]), f22out[2][4]))

    f22small = os.path.join(osd, "f22_small.txt")
    with open(f22small, "w") as f:
        f.write("A\nA\nB\nB\nB\nB\n")
    f22short = os.path.join(osd, "f22_short.txt")
    with open(f22short, "w") as f:
        f.write("A\nA\nA\nB\nB\n")
    rc22s, o22s = rsa(f22base[:f22base.index("-fit_condfold")] +
                      ["-fit_condfold", f22small, "-prefix", os.path.join(osd, "f22_small_out")])
    rc22l, o22l = rsa(f22base[:f22base.index("-fit_condfold")] +
                      ["-fit_condfold", f22short, "-prefix", os.path.join(osd, "f22_short_out")])
    rc22i, o22i = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                       "-model", "NN", "behav:nn",
                       "-model", "AK", "behav:annak",
                       "-metric", "pearson", "-model_fit", "MIX=NN,AK",
                       "-fit_condfold", f22foldfn,
                       "-prefix", os.path.join(osd, "f22_is_bad")])
    rc22n, o22n = rsa(["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
                       "-model_mat", "condition", omodfn, "-fit_condfold", f22foldfn,
                       "-prefix", os.path.join(osd, "f22_no_fit")])
    check("F22 rejects malformed folds, IS-RSA, and descriptors without a fitted model",
          rc22s != 0 and "holds 2 and leaves 4" in o22s and
          rc22l != 0 and "has 5 labels; need 6" in o22l and
          rc22i != 0 and "only to classic '-mode RSA'" in o22i and
          rc22n != 0 and "without a -model_fit" in o22n,
          "small=%s short=%s isrsa=%s nofit=%s" %
          (o22s.strip()[-100:], o22l.strip()[-100:], o22i.strip()[-100:],
           o22n.strip()[-100:]))

    rcmix14, omix14 = rsa(["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
                           "-model_mat", "TRUE", omodfn,
                           "-model_mat", "ALT", f7altfn,
                           "-metric", "pearson", "-model_fit", "MIX=TRUE,ALT",
                           "-model_contrast", "TRUE-MIX", "-no_dset",
                           "-prefix", os.path.join(osd, "f14_mixed")])
    check("F14 rejects fixed-versus-fitted contrasts",
          rcmix14 != 0 and "estimands cannot be paired" in omix14,
          omix14.strip()[-180:])
    rcbad7, obad7 = rsa(f7base[:f7base.index("-metric")] +
                        ["-metric", "spearman", "-model_fit", "MIX=TRUE,ALT",
                         "-prefix", os.path.join(osd, "f7_bad")])
    check("F7 rejects rank-metric weighted fitting",
          rcbad7 != 0 and "requires '-metric pearson'" in obad7)

    def f7_ref_isrsa(neu, comps, ridge=0.01):
        n = neu.shape[0]; z, ww = [], []
        for hold in range(n):
            tr = [(a, b) for a in range(n) if a != hold
                  for b in range(a + 1, n) if b != hold]
            te = [(hold, a) for a in range(n) if a != hold]
            X = np.asarray([[x[a, b] for x in comps] for a, b in tr])
            y = np.asarray([neu[a, b] for a, b in tr])
            w, xm, xs = f7_solve(X, y, ridge)
            Xt = np.asarray([[x[a, b] for x in comps] for a, b in te])
            yt = np.asarray([neu[a, b] for a, b in te])
            yp = ((Xt - xm) / xs).dot(w)
            rr = 0.0 if np.std(yt) == 0 or np.std(yp) == 0 else np.corrcoef(yt, yp)[0, 1]
            z.append(np.arctanh(np.clip(rr, -0.999329, 0.999329)))
            ww.append(w / w.sum() if w.sum() else w)
        return np.tanh(np.mean(z)), np.mean(ww, axis=0)

    f7iref, f7iwref = f7_ref_isrsa(neu_r, [nn_rdm, ak_rdm])
    f14irng = np.random.default_rng(20260827)
    f14inoise = f14irng.normal(size=(NSUB, NSUB))
    f14inoise = 0.5 * (f14inoise + f14inoise.T); np.fill_diagonal(f14inoise, 1.0)
    f14inoisefn = os.path.join(work, "f14_is_noise.1D")
    np.savetxt(f14inoisefn, f14inoise, fmt="%.8g")
    f14ibadref, _ = f7_ref_isrsa(neu_r, [ak_rdm, f14inoise])
    f14iref = np.arctanh(np.clip(f7iref, -0.999329, 0.999329)) - \
              np.arctanh(np.clip(f14ibadref, -0.999329, 0.999329))
    f7ipre = os.path.join(work, "f7_is")
    f14ibase = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                "-model", "NN", "behav:nn",
                "-model", "AK", "behav:annak",
                "-model_mat", "NOISE", f14inoisefn,
                "-metric", "pearson", "-model_fit", "MIX=NN,AK",
                "-model_fit", "BAD=AK,NOISE", "-model_contrast", "MIX-BAD",
                "-nperm", "37", "-seed", "212", "-no_dset", "-quiet"]
    rci7, oi7 = rsa(f14ibase + ["-prefix", f7ipre], env=envN)
    f7ir = read_table(f7ipre + ".rsa.1D", "NN")[1] if rci7 == 0 else []
    f7irow = f7ir[0] if f7ir else {}
    check("F7 IS-RSA nested dyad exclusion matches independent reference",
          rci7 == 0 and abs(f7irow.get("MIX_cvR", 9) - f7iref) < 2e-5 and
          np.allclose([f7irow.get("MIX_w_NN", 9), f7irow.get("MIX_w_AK", 9)],
                      f7iwref, atol=2e-5),
          "3dRSA=%s weights=%s ref=%.6f/%s %s" %
          (f7irow.get("MIX_cvR"), [f7irow.get("MIX_w_NN"), f7irow.get("MIX_w_AK")],
           f7iref, f7iwref, oi7.strip()[-100:]))
    check("F14 IS-RSA paired nested-fit contrast matches independent reference",
          rci7 == 0 and abs(f7irow.get("MIX-BAD_cvDiff", 9) - f14iref) < 3e-5 and
          0 <= f7irow.get("MIX-BAD_cvP", -1) <= f7irow.get("MIX-BAD_cvPfwe", -1) <= 1,
          "3dRSA=%s p/pfwe=%s/%s ref=%.6f" %
          (f7irow.get("MIX-BAD_cvDiff"), f7irow.get("MIX-BAD_cvP"),
           f7irow.get("MIX-BAD_cvPfwe"), f14iref))
    f14ipre1 = os.path.join(work, "f14_is_1")
    rci71, oi71 = rsa(f14ibase + ["-prefix", f14ipre1], env=env1)
    f14ir1 = read_table(f14ipre1 + ".rsa.1D", "NN")[1] if rci71 == 0 else []
    check("F14 IS-RSA fitted contrast is thread-reproducible (1 vs %d)" % threads,
          rci7 == rci71 == 0 and len(f14ir1) == len(f7ir) and
          all(a[k] == b[k] for a, b in zip(f14ir1, f7ir)
              for k in ("MIX-BAD_cvDiff", "MIX-BAD_cvP",
                        "MIX-BAD_cvQ", "MIX-BAD_cvPfwe")),
          oi71.strip()[-120:])

    # F20 time-resolved model-RDM fusion.  Treat four condition RDMs as an
    # ordered M/EEG latency series.  Independently calculate every subject x
    # time fit, exhaust all 2^8 subject sign patterns, and take one maximum over
    # the complete time x space family.  The SPHERE(100) fixture has identical
    # whole-volume neighborhoods, so it also gives an exact atlas/map invariant.
    f20_groups = [
        np.array([0, 1, 0, 1, 2, 2]),
        np.array([0, 0, 1, 2, 1, 2]),
        ogrp,
        np.array([0, 1, 2, 0, 1, 2]),
    ]
    f20_times = ["-100ms", "0ms", "100ms", "200ms"]
    f20_mats = [(g[:, None] != g[None, :]).astype(float) for g in f20_groups]
    f20_list = os.path.join(osd, "f20_series.txt")
    with open(f20_list, "w") as f:
        f.write("Time ModelFile\n")
        for ti, (tlab, mat) in enumerate(zip(f20_times, f20_mats)):
            mfn = "f20_%02d.1D" % ti
            np.savetxt(os.path.join(osd, mfn), mat, fmt="%.1f")
            f.write("%s %s\n" % (tlab, mfn))  # relative to the list, not cwd

    f20_z = np.asarray([[np.arctanh(spearman_tri(D, mat)) for D in oinner]
                        for mat in f20_mats], dtype=np.float32)

    def f20_t(v):
        """Mirror THD_onesamp_t's sequential float32 arithmetic."""
        v = np.asarray(v, dtype=np.float32)
        bar = np.float32(0)
        for x in v:
            bar = np.float32(bar + x)
        bar = np.float32(bar / np.float32(len(v)))
        ss = np.float32(0)
        for x in v:
            d = np.float32(x - bar)
            ss = np.float32(ss + np.float32(d * d))
        sd = np.float32(np.sqrt(np.float32(ss / np.float32(len(v) - 1))))
        return float(np.float32(bar / np.float32(sd / np.sqrt(np.float32(len(v))))))

    f20_obs_t = np.asarray([f20_t(x) for x in f20_z])
    f20_effect = np.asarray([np.tanh(np.mean(x, dtype=np.float32)) for x in f20_z])
    f20_null = np.empty((len(f20_times), 1 << OSUB), float)
    for bits in range(1 << OSUB):
        sg = np.asarray([1 if bits & (1 << j) else -1 for j in range(OSUB)],
                        dtype=np.float32)
        for ti in range(len(f20_times)):
            f20_null[ti, bits] = abs(f20_t(f20_z[ti] * sg))
    f20_p = np.mean(f20_null >= np.abs(f20_obs_t)[:, None], axis=1)
    f20_max = np.max(f20_null, axis=0)
    f20_pf = np.asarray([np.mean(f20_max >= abs(t)) for t in f20_obs_t])
    order = np.argsort(f20_p)
    f20_q = np.empty_like(f20_p); qmin = 1.0
    for rank in range(len(order) - 1, -1, -1):
        qmin = min(qmin, f20_p[order[rank]] * len(order) / (rank + 1))
        f20_q[order[rank]] = min(1.0, qmin)

    def f20_run(pre, searchlight=False, env=None, dset=False):
        aa = ["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
              "-model_series", f20_list, "-metric", "spearman",
              "-nperm", "256", "-seed", "211", "-prefix", os.path.join(osd, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        if not dset:
            aa += ["-no_dset"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(osd, pre + ".rsa.1D")
        rows0 = read_table(tf, "effect")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    rf20, of20, af20, mf20 = f20_run("f20_atlas", env=env1, dset=True)
    sf20, sof20, slf20, smf20 = f20_run("f20_search", True, envN)
    check("F20 model_series effects/time labels match independent reference",
          rf20 == 0 and len(af20) == len(f20_times) and
          [r["time_label"] for r in af20] == f20_times and
          np.allclose([r["effect"] for r in af20], f20_effect, atol=1e-5),
          "rc=%d got=%s ref=%s %s" %
          (rf20, [r.get("effect") for r in af20], f20_effect, of20.strip()[-120:]))
    check("F20 exact joint time x space p/FDR/FWE matches exhaustive signs",
          len(af20) == len(f20_times) and
          np.allclose([r["p"] for r in af20], f20_p, atol=1e-8) and
          np.allclose([r["q"] for r in af20], f20_q, atol=1e-8) and
          np.allclose([r["pfwe"] for r in af20], f20_pf, atol=1e-8) and
          np.all(f20_pf >= f20_p) and np.any(f20_pf > f20_p),
          "p=%s/%s q=%s/%s pf=%s/%s" %
          ([r.get("p") for r in af20], f20_p, [r.get("q") for r in af20], f20_q,
           [r.get("pfwe") for r in af20], f20_pf))
    check("F20 atlas equals whole-volume time x space searchlight",
          rf20 == sf20 == 0 and len(slf20) == len(f20_times) * np.prod(OSHAPE) and
          all(all(row[k] == af20[int(row["time_index"])][k]
                  for k in ("effect", "stat", "p", "q", "pfwe")) for row in slf20),
          "atlas=%d search=%d %s" % (len(af20), len(slf20), sof20.strip()[-120:]))
    f20_labs = head_brick_labs(os.path.join(osd, "f20_atlas+orig.HEAD"))
    check("F20 long-form provenance and time-labeled AFNI bricks",
          "joint time x space family" in mf20 and
          "t#### maps to time_label" in mf20 and
          f20_labs[:8] == [x for ti in range(4)
                            for x in ("t%04d_r" % ti, "t%04d_Z" % ti)] and
          f20_labs[8:12] == ["t%04d_ZFWE" % ti for ti in range(4)],
          "labs=%s" % f20_labs)
    _, _, f20t1, _ = f20_run("f20_t1", True, env1)
    _, _, f20tn, _ = f20_run("f20_tN", True, envN)
    check("F20 model_series thread-reproducible (1 vs %d)" % threads,
          len(f20t1) == len(f20tn) > 0 and f20t1 == f20tn)

    rcmix, omix = rsa(["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
                       "-model_series", f20_list, "-model_mat", "condition", omodfn,
                       "-nperm", "20", "-no_dset", "-prefix", os.path.join(osd, "f20_mix")])
    badlist = os.path.join(osd, "f20_bad.txt")
    with open(badlist, "w") as f:
        f.write("0ms f20_00.1D\n100ms ../cat.1D\n")
    rcbad, obad = rsa(["-dataTableFile", otab, "-mask", omask, "-mode", "RSA",
                       "-model_series", badlist, "-nperm", "20", "-no_dset",
                       "-prefix", os.path.join(osd, "f20_bad")])
    check("F20 rejects mixed-model and malformed series contracts",
          rcmix != 0 and "complete ordered model set" in omix and
          rcbad != 0 and ("must be" in obad or "needed" in obad),
          "mix=%d bad=%d %s" % (rcmix, rcbad, obad.strip()[-160:]))

    # A3 second-order task-fMRI IS-RSA.  Each subject's condition RDM triangle
    # becomes the feature vector used to construct the outer subject matrix.
    # Independently reproduce both the main modality and a per-location
    # -model_dset modality, then require whole-atlas/searchlight equivalence.
    from scipy.stats import rankdata
    oiu = np.triu_indices(OCOND, 1)
    oit = np.stack([x[oiu] for x in oinner])
    mit = np.stack([x[oiu] for x in minner])
    outer_ref = np.corrcoef(oit)
    mouter_ref = np.corrcoef(mit)
    br = rankdata(obeh)
    bdist = np.abs(br[:, None] - br[None, :])
    bmodel = 1.0 - bdist / bdist.max()
    a3ref = {"behav_nn": spearman_tri(outer_ref, bmodel),
             "modality": spearman_tri(outer_ref, mouter_ref)}

    ocinner = [np.corrcoef(B - B.mean(axis=0, keepdims=True)) for B in opattern]
    mcinner = [np.corrcoef(B - B.mean(axis=0, keepdims=True)) for B in mpattern]
    ocit = np.stack([x[oiu] for x in ocinner])
    mcit = np.stack([x[oiu] for x in mcinner])
    ocouter_ref = np.corrcoef(ocit)
    mcouter_ref = np.corrcoef(mcit)
    s2a3ref = {"behav_nn": spearman_tri(ocouter_ref, bmodel),
               "modality": spearman_tri(ocouter_ref, mcouter_ref)}

    def a3_run(pre, searchlight=False, env=None, save=False, extra=()):
        aa = ["-dataTableFile", otab, "-mask", omask, "-mode", "IS-RSA",
              "-featuretype", "rdm", "-condition_metric", "corr",
              "-neural_metric", "corr", "-model", "behav_nn", "behav:nn",
              "-model_dset", "modality", "ModFile",
              "-metric", "spearman", "-nperm", "61", "-seed", "107",
              "-no_dset", "-prefix", os.path.join(osd, pre)] + list(extra)
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        if save:
            aa += ["-save_rdm", os.path.join(osd, pre + "_rdm")]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(osd, pre + ".rsa.1D")
        rows0 = read_table(tf, "behav_nn")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    rca3, oa3, ara3, ama3 = a3_run("a3_atlas", False, env1, True)
    rcs3, os3, ars3, ams3 = a3_run("a3_search", True, envN)
    a3keys = ("behav_nn_r", "behav_nn_p", "behav_nn_pfwe",
              "modality_r", "modality_p", "modality_pfwe")
    saved_outer = (np.loadtxt(os.path.join(osd, "a3_atlas_rdm_roi0001.1D"))
                   if rca3 == 0 else np.empty((0, 0)))
    check("A3 ordinary second-order IS-RSA matches independent outer RDM",
          rca3 == 0 and len(ara3) == 1 and
          abs(ara3[0]["behav_nn_r"] - a3ref["behav_nn"]) < 1e-5 and
          np.allclose(saved_outer, outer_ref, atol=1e-5) and
          "(feature=rdm)" in ama3 and "# condition estimator: corr" in ama3,
          "rc=%d got=%s ref=%.6f maxmat=%.3g" %
          (rca3, ara3[0]["behav_nn_r"] if ara3 else None, a3ref["behav_nn"],
           np.max(np.abs(saved_outer - outer_ref)) if saved_outer.size else -1))
    check("A3 second-order model_dset matches independent modality RDM",
          len(ara3) == 1 and
          abs(ara3[0]["modality_r"] - a3ref["modality"]) < 1e-5,
          "got=%s ref=%.6f" %
          (ara3[0]["modality_r"] if ara3 else None, a3ref["modality"]))
    check("A3 ordinary atlas equals whole-volume searchlight",
          rca3 == rcs3 == 0 and len(ars3) == np.prod(OSHAPE) and
          all(all(row[k] == ara3[0][k] for k in a3keys) for row in ars3))
    _, _, a3t1, _ = a3_run("a3_t1", True, env1)
    _, _, a3tn, _ = a3_run("a3_tN", True, envN)
    check("A3 ordinary second-order thread-reproducible (1 vs %d)" % threads,
          len(a3t1) == len(a3tn) == np.prod(OSHAPE) and
          all(all(a[k] == b[k] for k in a3keys) for a, b in zip(a3t1, a3tn)))

    s2a3c, s2a3o, s2a3r, s2a3m = a3_run(
        "s2_a3_center", False, env1, False, ("-center_conditions", "subject"))
    check("S2 centered second-order neural and model-dataset RDMs match NumPy",
          s2a3c == 0 and len(s2a3r) == 1 and
          abs(s2a3r[0]["behav_nn_r"] - s2a3ref["behav_nn"]) < 1e-5 and
          abs(s2a3r[0]["modality_r"] - s2a3ref["modality"]) < 1e-5 and
          "# condition centering: subject-wise voxel mean" in s2a3m,
          "rc=%d got=%s/%s ref=%.6f/%.6f %s" %
          (s2a3c, s2a3r[0].get("behav_nn_r") if s2a3r else None,
           s2a3r[0].get("modality_r") if s2a3r else None,
           s2a3ref["behav_nn"], s2a3ref["modality"], s2a3o.strip()[-120:]))

    # F2 ordinary classic RSA: independently rebuild each subject's condition
    # similarity matrix from ROI-1 voxel patterns, apply the exact shared
    # condition draws, omit duplicated-original diagonal dyads, and compare the
    # group Fisher-z percentile interval.
    OCB, OSEED = 201, 31
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                   "-model_mat", "cat", os.path.join(work, "cat.1D"),
                   "-metric", "spearman", "-nperm", "0",
                   "-cond_bootstrap", str(OCB), "-boot_ci", "90", "-seed", str(OSEED),
                   "-no_dset", "-prefix", os.path.join(work, "ordinary_cboot")])
    if rc == 0:
        import nibabel as nib
        roi1 = np.asarray(nib.load(atlas).dataobj).reshape(-1) == 1
        odm = []
        for sj in range(NSUB):
            vol = np.asarray(nib.load(os.path.join(work, "sub%02d.nii.gz" % sj)).dataobj)
            pat = vol.reshape(-1, NT)[roi1].T
            odm.append(np.corrcoef(pat))
        oval = []
        for ix in bootstrap_indices(NT, OCB, OSEED):
            zz = []
            for D in odm:
                dy, mx = [], []
                for aa in range(NT):
                    for bb in range(aa + 1, NT):
                        ia, ib = int(ix[aa]), int(ix[bb])
                        if ia != ib:
                            dy.append(D[ia, ib]); mx.append(M[ia, ib])
                from scipy.stats import spearmanr
                rv = float(spearmanr(dy, mx).statistic)
                zz.append(np.arctanh(rv if np.isfinite(rv) else 0.0))
            oval.append(np.tanh(np.mean(zz)))
        oref = percentile_linear(oval, (0.05, 0.95))
        orow = read_table(os.path.join(work, "ordinary_cboot.rsa.1D"), "cat")[1][0]
        ogot = np.array([orow["cat_cbootLo"], orow["cat_cbootHi"]])
        check("F2 ordinary classic-RSA condition CI matches NumPy reference",
              np.allclose(ogot, oref, atol=3e-5),
              "3dRSA=%s numpy=%s" % (ogot, oref))
    else:
        check("F2 ordinary classic-RSA condition CI matches NumPy reference",
              False, out.strip()[-200:])

    # =====================================================================
    # AUDIT FIX 4: -block rejected under classic RSA
    # =====================================================================
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                   "-model_mat", "cat", os.path.join(work, "cat.1D"), "-block", "behav",
                   "-nperm", "100", "-prefix", os.path.join(work, "blk"), "-quiet"])
    check("FIX4 -block + classic RSA errors out",
          rc != 0 and "block" in out.lower(), "rc=%d" % rc)

    rc_is, o_is = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                       "-model", "behav_nn", "behav:nn", "-cond_bootstrap", "20",
                       "-prefix", os.path.join(work, "bad_cboot_is")])
    rc_dual, o_dual = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                           "-model_mat", "cat", os.path.join(work, "cat.1D"),
                           "-bootstrap", "20", "-cond_bootstrap", "21",
                           "-prefix", os.path.join(work, "bad_dual")])
    lone_group = os.path.join(work, "lone_group.txt")
    with open(lone_group, "w") as f:
        f.write("g\n" * NT)
    rc_grp, o_grp = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                         "-model_mat", "cat", os.path.join(work, "cat.1D"),
                         "-cond_group", lone_group,
                         "-prefix", os.path.join(work, "bad_group")])
    rc_gn, o_gn = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                       "-model_mat", "cat", os.path.join(work, "cat.1D"),
                       "-cond_bootstrap", "20", "-cond_group", lone_group,
                       "-prefix", os.path.join(work, "bad_group_count")])
    check("F2 condition bootstrap is classic-RSA-only",
          rc_is != 0 and "cond_bootstrap" in o_is and "mode RSA" in o_is)
    check("F6 dual bootstrap requires equal synchronized draw counts",
          rc_dual != 0 and "same number" in o_dual and "equal N" in o_dual)
    check("F2 condition descriptor without bootstrap is rejected",
          rc_grp != 0 and "cond_group" in o_grp and "cond_bootstrap" in o_grp)
    check("F2 condition descriptor needs at least three groups",
          rc_gn != 0 and "at least 3" in o_gn, "rc=%d" % rc_gn)

    # =====================================================================
    # AUDIT FIX 1e: -model_mat finiteness + symmetry validated at read time.
    #   A valid symmetric matrix must run; an asymmetric or NaN one must be
    #   rejected (not silently analyzed on one triangle).
    # =====================================================================
    # classic RSA reads an nitem x nitem model, nitem = NT conditions; a wrong
    # size would trip the size check before symmetry/finiteness, so match NT
    base = M.copy()                                 # the NTxNT symmetric 0/1 model
    asym = base.copy(); asym[0, 1] = asym[0, 1] + 9.0     # break symmetry at (0,1)
    np.savetxt(os.path.join(work, "asym.1D"), asym, fmt="%.4f")
    # 1e40 overflows float32 to +inf on storage, so it passes AFNI's text reader
    # and specifically exercises the read-time finiteness check
    ovf = base.copy(); ovf[1, 2] = ovf[2, 1] = 1e40
    np.savetxt(os.path.join(work, "ovf.1D"), ovf, fmt="%.6g")

    def vm(fname):
        return rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                    "-model_mat", "mat", os.path.join(work, fname), "-nperm", "0",
                    "-prefix", os.path.join(work, "vm"), "-quiet"])
    rc_a, o_a = vm("asym.1D")
    rc_o, o_o = vm("ovf.1D")
    check("FIX1e asymmetric -model_mat rejected (symmetry not AFNI-checked)",
          rc_a != 0 and "symmetr" in o_a.lower(), "rc=%d out=%s" % (rc_a, o_a[-120:]))
    check("FIX1e non-finite -model_mat rejected at read time",
          rc_o != 0 and "finite" in o_o.lower(), "rc=%d out=%s" % (rc_o, o_o[-120:]))

    # =====================================================================
    # AUDIT FIX 1: degenerate ROIs must not corrupt the shared max-null.
    #   Run a joint regression (forces THD_rdm_regress) with ROI 3 constant.
    #   The degenerate ROI must (a) not crash, (b) yield in-range monotone
    #   p_fwe for the OTHER ROIs, and (c) be thread-reproducible -- the stale
    #   scratch bug was order/thread dependent and broke exactly these.
    # =====================================================================
    make_planted(work, degenerate=True, second_behav=True)
    outs = {}
    for tag, env in (("1", env1), ("N", envN)):
        rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                       "-model_joint", "-model", "behav_nn", "behav:nn", "-model", "behav2_nn", "behav2:nn",
                       "-metric", "spearman", "-nperm", "2000", "-seed", "3",
                       "-no_dset", "-prefix", os.path.join(work, "deg%s" % tag)],
                      env=env)
        outs[tag] = (rc, out)
    rc1 = outs["1"][0]
    check("FIX1 degenerate joint run does not crash", rc1 == 0,
          outs["1"][1].strip()[-200:])
    if rc1 == 0:
        _, d1 = read_table(os.path.join(work, "deg1.rsa.1D"), "behav_nn")
        _, dN = read_table(os.path.join(work, "degN.rsa.1D"), "behav_nn")
        pf1 = np.array([x["behav_nn_pfwe"] for x in d1])
        pp1 = np.array([x["behav_nn_p"] for x in d1])
        pfN = np.array([x["behav_nn_pfwe"] for x in dN])
        check("FIX1 degenerate: p_fwe finite in [0,1]",
              np.all(np.isfinite(pf1)) and np.all((pf1 >= 0) & (pf1 <= 1)))
        check("FIX1 degenerate: p_fwe >= p_unc (max-null not corrupted)",
              np.all(pf1 >= pp1 - 1e-9),
              "%d violations" % int(np.sum(pf1 < pp1 - 1e-9)))
        check("FIX1 degenerate: p_fwe thread-reproducible (no stale scratch)",
              np.allclose(pf1, pfN, atol=1e-12))

    # =====================================================================
    # 3b. Model contrasts (paired A-B).  IS-RSA: the difference must equal the
    #     two models' own r's; the sign must flip on label swap; FWE monotone;
    #     identical models give exactly 0 / p=1.  Classic RSA: both group tests
    #     run.  Thread reproducible.
    # =====================================================================
    def con_run(prefix, args, env=None):
        base = ["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                "-model", "nn", "behav:nn",
                "-model", "ak", "behav:annak",
                "-metric", "spearman", "-nperm", "2000", "-seed", "1",
                "-no_dset", "-prefix", os.path.join(work, prefix)]
        return rsa(base + args, env=env)

    rc, out = con_run("con", ["-model_contrast", "nn-ak",
                              "-contrast_hypothesis", "alignment"])
    if rc != 0:
        check("3b IS-RSA contrast runs", False, out.strip()[-200:])
    else:
        check("3b IS-RSA contrast runs", True)
        _, cr = read_table(os.path.join(work, "con.rsa.1D"), "nn")
        cmeta = open(os.path.join(work, "con.rsa.1D")).read()
        r0 = cr[0]
        # self-consistency: diff == r_A - r_B exactly (both in the same table)
        check("3b IS-RSA diff == nn_r - ak_r (paired, exact)",
              abs(r0["nn-ak_diff"] - (r0["nn_r"] - r0["ak_r"])) < 1e-5,
              "diff=%.6f vs %.6f" % (r0["nn-ak_diff"], r0["nn_r"] - r0["ak_r"]))
        cpf = np.array([x["nn-ak_pfwe"] for x in cr])
        cp = np.array([x["nn-ak_p"] for x in cr])
        check("3b contrast FWE p >= p (monotone)", np.all(cpf >= cp - 1e-9))
        check("3b true model wins: nn-ak diff>0 and p<.05 on planted ROI",
              r0["nn-ak_diff"] > 0 and r0["nn-ak_p"] < 0.05,
              "diff=%.4f p=%.4f" % (r0["nn-ak_diff"], r0["nn-ak_p"]))
        check("3b IS-RSA alignment provenance does not claim superiority",
              "# contrast hypothesis: alignment" in cmeta and
              "sharp alignment null; not an equal-performance null" in cmeta)

    supstem = os.path.join(work, "con_superiority_rdm")
    rcsup, osup = con_run("con_superiority", [
        "-model_contrast", "nn-ak", "-contrast_hypothesis", "superiority",
        "-save_rdm", supstem])
    rcbad, obad = con_run("con_bad_hypothesis", [
        "-model_contrast", "nn-ak", "-contrast_hypothesis", "mystery"])
    suprows = (read_table(os.path.join(work, "con_superiority.rsa.1D"), "nn")[1]
               if rcsup == 0 else [])
    supmeta = (open(os.path.join(work, "con_superiority.rsa.1D")).read()
               if rcsup == 0 else "")
    if rcsup == 0:
        from scipy.stats import spearmanr
        sma = np.loadtxt(supstem + "_model_nn.1D")
        smb = np.loadtxt(supstem + "_model_ak.1D")
        snall = [np.loadtxt(supstem + "_roi%04d.1D" % int(row["ROI"]))
                 for row in suprows]
        sobsa = np.asarray([spearman_tri(sn, sma) - spearman_tri(sn, smb)
                            for sn in snall])
        # The implementation draws a reserve and keeps the first 2000 usable
        # samples.  With n=20 rejection is rare, but mirroring compaction makes
        # this reference exact even for an unusually duplicate-heavy stream.
        six = [ix for ix in bootstrap_indices(NSUB, 4000, 1)
               if len(np.unique(ix)) >= 3][:2000]
        sd = []
        for ix in six:
            xa, xb, pairs = [], [], []
            for aa in range(NSUB):
                for bb in range(aa + 1, NSUB):
                    ia, ib = int(ix[aa]), int(ix[bb])
                    if ia != ib:
                        pairs.append((ia, ib)); xa.append(sma[ia, ib]); xb.append(smb[ia, ib])
            one = []
            for sn in snall:
                ra = float(spearmanr([sn[a, b] for a, b in pairs], xa).statistic)
                rb = float(spearmanr([sn[a, b] for a, b in pairs], xb).statistic)
                # THD correlation kernels define a constant-vector result as 0.
                if not np.isfinite(ra): ra = 0.0
                if not np.isfinite(rb): rb = 0.0
                one.append(ra - rb)
            sd.append(one)
        sd = np.asarray(sd)
        centered = np.abs(sd - sobsa[None, :])
        spref = (1 + np.sum(centered[:, 0] >= abs(sobsa[0]))) / 2001.0
        smx = centered.max(axis=1)
        sfref = np.asarray([(1 + np.sum(smx >= abs(x))) / 2001.0 for x in sobsa])
    else:
        sobsa = np.asarray([np.nan]); spref = np.nan; sfref = np.asarray([np.nan])
    check("3b IS-RSA superiority matches centered paired subject-bootstrap null",
          rcsup == 0 and len(suprows) >= 1 and
          abs(suprows[0]["nn-ak_diff"] - sobsa[0]) < 3e-5 and
          len(six) == 2000 and
          abs(suprows[0]["nn-ak_p"] - spref) < 5e-7 and
          np.allclose([row["nn-ak_pfwe"] for row in suprows], sfref, atol=5e-7) and
          supmeta.find("centered paired subject bootstrap") >= 0 and
          "duplicate-copy diagonal dyads omitted" in supmeta and
          "(1 + exceedances)/(1 + draws)" in supmeta,
          "rc=%d got=%s ref=%.6f/%.6g %s" %
          (rcsup, suprows[0] if suprows else None, sobsa[0], spref, osup[-100:]))
    check("3b unknown contrast hypothesis rejects clearly",
          rcbad != 0 and "superiority, alignment, or legacy" in obad)
    rs1, _ = con_run("con_sup_t1", ["-model_contrast", "nn-ak",
                                    "-contrast_hypothesis", "superiority"], env=env1)
    rsn, _ = con_run("con_sup_tN", ["-model_contrast", "nn-ak",
                                    "-contrast_hypothesis", "superiority"], env=envN)
    sr1 = (read_table(os.path.join(work, "con_sup_t1.rsa.1D"), "nn")[1]
           if rs1 == 0 else [])
    srn = (read_table(os.path.join(work, "con_sup_tN.rsa.1D"), "nn")[1]
           if rsn == 0 else [])
    sk = ("nn-ak_diff", "nn-ak_p", "nn-ak_q", "nn-ak_pfwe")
    check("3b centered superiority null is thread-reproducible (1 vs %d)" % threads,
          rs1 == rsn == 0 and len(sr1) == len(srn) and
          all(a[k] == b[k] for a, b in zip(sr1, srn) for k in sk))

    # sign reversal on label swap
    con_run("conr", ["-model_contrast", "ak-nn"])
    _, cr2 = read_table(os.path.join(work, "conr.rsa.1D"), "nn")
    check("3b contrast sign reverses on swap",
          abs(cr2[0]["ak-nn_diff"] + cr[0]["nn-ak_diff"]) < 1e-5)

    # thread reproducibility of the contrast columns
    con_run("con1", ["-model_contrast", "nn-ak"], env=env1)
    con_run("conN", ["-model_contrast", "nn-ak"], env=envN)
    _, a1 = read_table(os.path.join(work, "con1.rsa.1D"), "nn")
    _, aN = read_table(os.path.join(work, "conN.rsa.1D"), "nn")
    check("3b contrast diff/p/pfwe identical at 1 vs %d threads" % threads,
          all(abs(x["nn-ak_diff"] - y["nn-ak_diff"]) < 1e-12 and
              abs(x["nn-ak_p"]    - y["nn-ak_p"])    < 1e-12 and
              abs(x["nn-ak_pfwe"] - y["nn-ak_pfwe"]) < 1e-12
              for x, y in zip(a1, aN)))

    # identical models -> exactly zero difference, p == 1
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "x", "behav:nn",
                   "-model", "y", "behav:nn",
                   "-model_contrast", "x-y", "-metric", "spearman",
                   "-nperm", "1000", "-seed", "1", "-no_dset",
                   "-prefix", os.path.join(work, "coni")])
    _, ci = read_table(os.path.join(work, "coni.rsa.1D"), "x")
    check("3b identical models: diff==0 and p==1",
          all(abs(x["x-y_diff"]) < 1e-6 and x["x-y_p"] >= 1.0 - 1e-9 for x in ci))

    # classic RSA contrast, both group tests, over NT condition models
    vis = (np.arange(NT) // 20)[:, None]
    visM = (vis != vis.T).astype(float); np.savetxt(os.path.join(work, "vis.1D"), visM, fmt="%.1f")
    sem = (np.arange(NT) // 30)[:, None]
    semM = (sem != sem.T).astype(float); np.savetxt(os.path.join(work, "sem.1D"), semM, fmt="%.1f")
    for gt, tag in (("signflip", "csf"), ("signedrank", "csr")):
        rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                       "-model_mat", "vis", os.path.join(work, "vis.1D"),
                       "-model_mat", "sem", os.path.join(work, "sem.1D"),
                       "-model_contrast", "vis-sem", "-group_test", gt,
                       "-contrast_hypothesis", "superiority",
                       "-nperm", "2000", "-seed", "1", "-no_dset",
                       "-prefix", os.path.join(work, tag), "-quiet"])
        ok = rc == 0 and os.path.exists(os.path.join(work, tag + ".rsa.1D"))
        if ok:
            _, cc = read_table(os.path.join(work, tag + ".rsa.1D"), "vis")
            meta = open(os.path.join(work, tag + ".rsa.1D")).read()
            ok = (all(0.0 <= x["vis-sem_p"] <= 1.0 and
                      np.isfinite(x["vis-sem_zDiff"]) and
                      np.isfinite(x["vis-sem_rDiff"]) for x in cc) and
                  "# contrast hypothesis: superiority" in meta and
                  "paired subject sign" in meta)
        check("3b classic RSA contrast runs (-group_test %s)" % gt, ok,
              "rc=%d" % rc)

    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                   "-model_mat", "vis", os.path.join(work, "vis.1D"),
                   "-model_mat", "sem", os.path.join(work, "sem.1D"),
                   "-model_joint", "-bootstrap", "101", "-boot_ci", "90",
                   "-nperm", "100", "-seed", "31", "-no_dset",
                   "-prefix", os.path.join(work, "boot_joint"), "-quiet"])
    jr = read_table(os.path.join(work, "boot_joint.rsa.1D"), "vis")[1] if rc == 0 else []
    check("classic joint-regression subject bootstrap runs with finite ordered CIs",
          len(jr) == 3 and all(np.isfinite(x["vis_bootLo"]) and
                              x["vis_bootLo"] <= x["vis_bootHi"] for x in jr),
          "rc=%d" % rc)

    # =====================================================================
    # CA. Commonality analysis (variance partitioning).  IS-RSA over two
    #     behavioral models; check the decomposition matches an independent
    #     numpy computation from the tool's own saved RDMs, the identities
    #     (uniq+uniq+common == R2_AB; identical models -> uniq=0, common=R2),
    #     that it runs ALONGSIDE -model_contrast, and thread-reproducibility.
    # =====================================================================
    def zt(v):
        r = rankdata(v); r = r - r.mean(); s = np.sqrt((r * r).mean())
        return r / s if s > 0 else r

    def tri(M):
        iu = np.triu_indices(M.shape[0], 1); return M[iu]

    def prepared_commonality(yy, aa, bb):
        if float(yy @ yy) <= 0.0:
            return np.zeros(5, float)
        q2a = float((yy @ aa) ** 2 / ((yy @ yy) * (aa @ aa)))
        q2b = float((yy @ bb) ** 2 / ((yy @ yy) * (bb @ bb)))
        XX = np.column_stack([aa, bb])
        bt = np.linalg.lstsq(XX, yy, rcond=None)[0]
        q2ab = 1.0 - float(((yy - XX @ bt) ** 2).sum() / (yy * yy).sum())
        ua, ub = q2ab - q2b, q2ab - q2a
        pa = ua / (1.0 - q2b) if 1.0 - q2b > 1e-12 else 0.0
        pb = ub / (1.0 - q2a) if 1.0 - q2a > 1e-12 else 0.0
        return np.array([ua, ub, q2a + q2b - q2ab, pa, pb])

    def compact_commonality(yv, av, bv):
        return prepared_commonality(zt(yv), zt(av), zt(bv))

    def prepared_commonality3(yy, aa, bb, cc):
        """Seven exhaustive raw regions, then partial R2 A|BC/B|AC/C|AB."""
        if float(yy @ yy) <= 0.0:
            return np.zeros(10, float)
        def r2(*xx):
            X = np.column_stack(xx)
            be = np.linalg.lstsq(X, yy, rcond=None)[0]
            return 1.0 - float(((yy - X @ be) ** 2).sum() / (yy @ yy))
        ra, rb, rc = r2(aa), r2(bb), r2(cc)
        rab, rac, rbc, rabc = r2(aa, bb), r2(aa, cc), r2(bb, cc), r2(aa, bb, cc)
        raw = np.array([rabc-rbc, rabc-rac, rabc-rab,
                        rac+rbc-rc-rabc, rab+rbc-rb-rabc,
                        rab+rac-ra-rabc, ra+rb+rc-rab-rac-rbc+rabc])
        den = np.array([1-rbc, 1-rac, 1-rab])
        partial = np.divide(raw[:3], den, out=np.zeros(3), where=den > 1e-12)
        return np.r_[raw, partial]

    def compact_commonality3(yv, av, bv, cv):
        return prepared_commonality3(zt(yv), zt(av), zt(bv), zt(cv))

    def fl_commonality3_reference(neural, amat, bmat, cmat, perms):
        n = neural.shape[0]; iu = np.triu_indices(n, 1)
        yy, aa, bb, cc = [zt(tri(x)) for x in (neural, amat, bmat, cmat)]
        obs = prepared_commonality3(yy, aa, bb, cc)
        null = np.zeros((len(perms), 10), float)
        cols = (aa, bb, cc)
        for which in range(3):
            red = np.column_stack([cols[j] for j in range(3) if j != which])
            fit = red @ np.linalg.lstsq(red, yy, rcond=None)[0]
            resid = yy - fit
            er = np.zeros((n, n), float); er[iu] = resid; er[(iu[1], iu[0])] = resid
            for pk, pi in enumerate(perms):
                cp = prepared_commonality3(fit + tri(er[np.ix_(pi, pi)]), aa, bb, cc)
                null[pk, which] = cp[which]; null[pk, 7+which] = cp[7+which]
        for pk, pi in enumerate(perms):
            cp = prepared_commonality3(zt(tri(neural[np.ix_(pi, pi)])), aa, bb, cc)
            null[pk, 3:7] = cp[3:7]
        return obs, null

    def fl_commonality_reference(neural, amat, bmat, perms):
        """Hybrid A1 null: unique/partial use A|B and B|A Freedman-Lane;
        common retains complete neural relabeling.  Inputs are full RDMs."""
        n = neural.shape[0]
        yy, aa, bb = zt(tri(neural)), zt(tri(amat)), zt(tri(bmat))
        obs = prepared_commonality(yy, aa, bb)
        null = np.zeros((len(perms), 5), float)
        iu = np.triu_indices(n, 1)
        for which, red in ((0, bb), (1, aa)):
            fit = red * float((red @ yy) / (red @ red))
            resid = yy - fit
            er = np.zeros((n, n), float)
            er[iu] = resid; er[(iu[1], iu[0])] = resid
            for pk, pi in enumerate(perms):
                yp = fit + tri(er[np.ix_(pi, pi)])
                cp = prepared_commonality(yp, aa, bb)
                null[pk, which] = cp[which]
                null[pk, which + 3] = cp[which + 3]
        for pk, pi in enumerate(perms):
            yp = zt(tri(neural[np.ix_(pi, pi)]))
            null[pk, 2] = prepared_commonality(yp, aa, bb)[2]
        return obs, null

    def ca_run(pre, extra, tab=table, env=None):
        base = ["-dataTableFile", tab, "-mask", atlas, "-mode", "IS-RSA",
                "-model", "nn", "behav:nn",
                "-model", "ak", "behav:annak",
                "-metric", "spearman", "-nperm", "1000", "-seed", "1", "-no_dset",
                "-prefix", os.path.join(work, pre)]
        return rsa(base + extra, env=env)

    # run commonality AND contrast together (the composability the user asked for)
    rc, out = ca_run("ca", ["-model_commonality", "nn,ak", "-model_contrast", "nn-ak",
                            "-bootstrap", "401", "-boot_ci", "90",
                            "-save_rdm", os.path.join(work, "carg")])
    if rc != 0:
        check("CA commonality + contrast run together", False, out.strip()[-200:])
    else:
        check("CA commonality + contrast run together", True)
        _, cr = read_table(os.path.join(work, "ca.rsa.1D"), "nn")
        r0 = cr[0]
        # both feature sets present in the one table
        have = all(k in r0 for k in
                   ("uniq_nn", "uniq_ak", "common_nn_ak",
                    "partialR2_nn", "partialR2_ak", "nn-ak_diff"))
        check("CA commonality and contrast columns coexist in one table", have)
        # independent numpy decomposition from the saved RDMs
        try:
            neu = np.loadtxt(os.path.join(work, "carg_roi0001.1D"))
            A = np.loadtxt(os.path.join(work, "carg_model_nn.1D"))
            B = np.loadtxt(os.path.join(work, "carg_model_ak.1D"))
            y, a, b = zt(tri(neu)), zt(tri(A)), zt(tri(B))
            r2A = np.corrcoef(y, a)[0, 1] ** 2; r2B = np.corrcoef(y, b)[0, 1] ** 2
            X = np.column_stack([a, b]); be = np.linalg.lstsq(X, y, rcond=None)[0]
            r2AB = 1 - ((y - X @ be) ** 2).sum() / (y * y).sum()
            ref = {"uniq_nn": r2AB - r2B, "uniq_ak": r2AB - r2A,
                   "common_nn_ak": r2A + r2B - r2AB,
                   "partialR2_nn": (r2AB - r2B) / (1.0 - r2B),
                   "partialR2_ak": (r2AB - r2A) / (1.0 - r2A)}
            mx = max(abs(r0[k] - ref[k]) for k in ref)
            check("A1 raw and partial-R2 commonality match NumPy (ROI 1)",
                  mx < 1e-4, "max|diff|=%.2e" % mx)
            check("CA identity: uniq_nn+uniq_ak+common == R2_AB",
                  abs(r0["uniq_nn"] + r0["uniq_ak"] + r0["common_nn_ak"] - r2AB) < 1e-4)
            check("A1 partial-R2 has permutation/FWE inference",
                  all(0.0 <= r0[k] <= 1.0 for k in
                      ("partialR2_nn_p", "partialR2_nn_q", "partialR2_nn_pfwe",
                       "partialR2_ak_p", "partialR2_ak_q", "partialR2_ak_pfwe")))

            # Give adjacent subjects pair blocks, making the relabeling group
            # exactly 2^10.  Independently form each reduced fit in ranked RDM
            # space, relabel its residual as rows+columns, and verify both the
            # uncorrected hybrid null and per-quantity map-max FWE across ROIs.
            pairtab = os.path.join(work, "ca_pair_table.txt")
            with open(table) as fi, open(pairtab, "w") as fo:
                lines = [x.strip() for x in fi if x.strip()]
                hd = lines[0].split(); fo.write(" ".join(hd[:1] + ["Pair"] + hd[1:]) + "\n")
                for sj, line in enumerate(lines[1:]):
                    z = line.split()
                    fo.write(" ".join(z[:1] + ["p%02d" % (sj // 2)] + z[1:]) + "\n")
            rcfl, ofl = ca_run("ca_fl", ["-model_commonality", "nn,ak",
                                          "-block", "Pair", "-nperm", "1024"],
                                  tab=pairtab)
            flrows = (read_table(os.path.join(work, "ca_fl.rsa.1D"), "nn")[1]
                      if rcfl == 0 else [])
            perms = []
            for bits in range(1 << (NSUB // 2)):
                pi = np.arange(NSUB)
                for jj in range(NSUB // 2):
                    if (bits >> jj) & 1:
                        pi[2*jj], pi[2*jj+1] = pi[2*jj+1], pi[2*jj]
                perms.append(pi)
            allobs, allnull = [], []
            for roi in range(1, 4):
                nr = np.loadtxt(os.path.join(work, "carg_roi%04d.1D" % roi))
                ob, nu = fl_commonality_reference(nr, A, B, perms)
                allobs.append(ob); allnull.append(nu)
            allobs, allnull = np.asarray(allobs), np.asarray(allnull)
            pref = np.mean(np.abs(allnull[0]) >= np.abs(allobs[0])[None, :] - 1e-10,
                           axis=0)
            maxnull = np.max(np.abs(allnull), axis=0)
            pfref = np.mean(maxnull >= np.abs(allobs[0])[None, :] - 1e-10, axis=0)
            names = ("uniq_nn", "uniq_ak", "common_nn_ak",
                     "partialR2_nn", "partialR2_ak")
            pgot = np.asarray([flrows[0][n + "_p"] for n in names]) if flrows else []
            pfgot = np.asarray([flrows[0][n + "_pfwe"] for n in names]) if flrows else []
            a1ix = np.array([0, 1, 3, 4])
            check("A1 reduced-model p matches exhaustive NumPy null",
                  rcfl == 0 and np.allclose(pgot[a1ix], pref[a1ix], atol=5e-6),
                  "rc=%d 3dRSA=%s reference=%s %s" %
                  (rcfl, pgot[a1ix], pref[a1ix], ofl.strip()[-100:]))
            check("A1 reduced-model max-FWE matches exhaustive NumPy",
                  rcfl == 0 and np.allclose(pfgot[a1ix], pfref[a1ix], atol=5e-6),
                  "3dRSA=%s reference=%s" % (pfgot, pfref))

            # F8: add a third fixed subject-geometry model.  Exhaust the same
            # 2^10 block-preserving subject relabelings and independently form
            # all seven raw regions plus the three conditional partial-R2s.
            cv = np.sin(np.arange(NSUB) * 0.73) + 0.15 * np.arange(NSUB)
            C = np.exp(-np.abs(cv[:, None] - cv[None, :]))
            cfn = os.path.join(work, "f8_c.1D"); np.savetxt(cfn, C, fmt="%.8g")
            C = np.loadtxt(cfn)  # reference the exact values parsed by 3dRSA
            f8pre = os.path.join(work, "f8_is")
            f8args = ["-dataTableFile", pairtab, "-mask", atlas, "-mode", "IS-RSA",
                      "-model", "nn", "behav:nn",
                      "-model", "ak", "behav:annak",
                      "-model_mat", "c", cfn,
                      "-model_commonality", "nn,ak,c", "-metric", "spearman",
                      "-block", "Pair", "-nperm", "1024", "-seed", "231"]
            rcf8, of8 = rsa(f8args + ["-prefix", f8pre], env=env1)
            f8rows = read_table(f8pre + ".rsa.1D", "nn")[1] if rcf8 == 0 else []
            f8row = f8rows[0] if f8rows else {}
            f8names = ("uniq_nn_given_ak_c", "uniq_ak_given_nn_c",
                       "uniq_c_given_nn_ak", "common_nn_ak_not_c",
                       "common_nn_c_not_ak", "common_ak_c_not_nn",
                       "common_nn_ak_c", "partialR2_nn_given_ak_c",
                       "partialR2_ak_given_nn_c", "partialR2_c_given_nn_ak")
            f8obs, f8null = [], []
            for roi in range(1, 4):
                nr = np.loadtxt(os.path.join(work, "carg_roi%04d.1D" % roi))
                ob, nu = fl_commonality3_reference(nr, A, B, C, perms)
                f8obs.append(ob); f8null.append(nu)
            f8obs, f8null = np.asarray(f8obs), np.asarray(f8null)
            f8got = np.asarray([f8row.get(n, np.nan) for n in f8names])
            check("F8 IS-RSA seven-region decomposition/partial-R2 match NumPy",
                  rcf8 == 0 and np.allclose(f8got, f8obs[0], atol=3e-5) and
                  abs(f8got[:7].sum() - f8obs[0][:7].sum()) < 3e-6,
                  "rc=%d got=%s ref=%s %s" %
                  (rcf8, f8got, f8obs[0], of8.strip()[-120:]))
            f8tol = 64 * np.finfo(np.float32).eps * (1 + np.abs(f8obs))
            f8p = np.mean(np.abs(f8null[0]) >=
                           np.abs(f8obs[0])[None, :] - f8tol[0], axis=0)
            # Mirror the C path: each location first snaps float32 null ties to
            # its float32 observed value, then the spatial maximum is formed.
            f8nf = np.abs(f8null).astype(np.float32)
            f8of = f8obs.astype(np.float32)
            for ri in range(f8nf.shape[0]):
                near = np.abs(f8nf[ri] - np.abs(f8of[ri])[None, :]) <= f8tol[ri]
                f8nf[ri][near] = np.broadcast_to(np.abs(f8of[ri]), f8nf[ri].shape)[near]
                f8nf[ri, 0] = np.abs(f8of[ri])
            f8mx = np.max(f8nf, axis=0)
            f8pf = np.mean(f8mx >= np.abs(f8of[0])[None, :], axis=0)
            check("F8 IS-RSA reduced/complete null p and spatial FWE match exhaustive NumPy",
                  np.allclose([f8row.get(n + "_p", np.nan) for n in f8names], f8p,
                              atol=1e-8) and
                  np.allclose([f8row.get(n + "_pfwe", np.nan) for n in f8names], f8pf,
                              atol=1e-8),
                  "p=%s/%s pfwe=%s/%s" %
                  ([f8row.get(n + "_p") for n in f8names], f8p,
                   [f8row.get(n + "_pfwe") for n in f8names], f8pf))
            f8bc = []
            for ix in bootstrap_indices(NSUB, 101, 231):
                yv, av, bv, cvv = [], [], [], []
                for ia0 in range(NSUB):
                    for ib0 in range(ia0 + 1, NSUB):
                        ia, ib = int(ix[ia0]), int(ix[ib0])
                        if ia != ib:
                            yv.append(neu[ia, ib]); av.append(A[ia, ib])
                            bv.append(B[ia, ib]); cvv.append(C[ia, ib])
                if len(set(map(int, ix))) >= 3:
                    f8bc.append(compact_commonality3(yv, av, bv, cvv))
            f8bc = np.asarray(f8bc)
            f8bref = np.asarray([percentile_linear(f8bc[:, q], (0.05, 0.95))
                                 for q in range(10)])
            f8bpre = os.path.join(work, "f8_is_boot")
            f8bargs = [x for x in f8args]
            bix0 = f8bargs.index("-dataTableFile"); f8bargs[bix0+1] = table
            del f8bargs[f8bargs.index("-block"):f8bargs.index("-block")+2]
            nix0 = f8bargs.index("-nperm"); f8bargs[nix0+1] = "0"
            rcf8b, of8b = rsa(f8bargs + ["-bootstrap", "101", "-boot_ci", "90",
                                        "-no_dset", "-prefix", f8bpre], env=env1)
            f8brows = read_table(f8bpre + ".rsa.1D", "nn")[1] if rcf8b == 0 else []
            f8brow = f8brows[0] if f8brows else {}
            f8bgot = np.asarray([[f8brow.get(n + "_bootLo", np.nan),
                                  f8brow.get(n + "_bootHi", np.nan)] for n in f8names])
            check("F8 IS-RSA bootstrap recomputes all ten quantities",
                  rcf8b == 0 and np.allclose(f8bgot, f8bref, atol=7e-4),
                  "got=%s ref=%s %s" % (f8bgot, f8bref, of8b.strip()[-100:]))
            f8preN = os.path.join(work, "f8_is_N")
            rcf8n, of8n = rsa(f8args + ["-prefix", f8preN], env=envN)
            f8rowsN = read_table(f8preN + ".rsa.1D", "nn")[1] if rcf8n == 0 else []
            f8labs = head_brick_labs(f8pre + "+orig.HEAD") if rcf8 == 0 else []
            check("F8 IS-RSA maps and 1-vs-%d thread output are complete" % threads,
                  rcf8 == rcf8n == 0 and f8rows == f8rowsN and
                  all(n in f8labs and n + "_ZFWE" in f8labs for n in f8names),
                  "labels=%s %s" % (f8labs, of8n.strip()[-100:]))

            bcomp = []
            for ix in bootstrap_indices(NSUB, 401, 1):
                yv, av, bv = [], [], []
                for ii in range(NSUB):
                    for jj in range(ii + 1, NSUB):
                        ia, ib = int(ix[ii]), int(ix[jj])
                        if ia != ib:
                            yv.append(neu[ia, ib]); av.append(A[ia, ib]); bv.append(B[ia, ib])
                if len(set(map(int, ix))) >= 3:
                    bcomp.append(compact_commonality(yv, av, bv))
            bcomp = np.vstack(bcomp)
            bcref = np.vstack([percentile_linear(bcomp[:, cc], (0.05, 0.95))
                               for cc in range(5)])
            cnames = ("uniq_nn", "uniq_ak", "common_nn_ak",
                      "partialR2_nn", "partialR2_ak")
            bcgot = np.asarray([[r0[n + "_bootLo"], r0[n + "_bootHi"]]
                                for n in cnames])
            check("F17 commonality bootstrap matches compact NumPy decomposition",
                  np.allclose(bcgot, bcref, atol=5e-4),
                  "3dRSA=%s reference=%s" % (bcgot, bcref))
        except Exception as e:
            check("CA decomposition matches numpy (ROI 1)", False, repr(e))

    # identical models (same model under two labels) -> unique == 0, common == R2
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "p", "behav:nn",
                   "-model", "q", "behav:nn",
                   "-model_commonality", "p,q", "-metric", "spearman",
                   "-nperm", "200", "-seed", "1", "-no_dset",
                   "-prefix", os.path.join(work, "cai")])
    _, ii2 = read_table(os.path.join(work, "cai.rsa.1D"), "p")
    r1 = ii2[0]
    check("CA identical models: unique ~ 0",
          abs(r1["uniq_p"]) < 1e-4 and abs(r1["uniq_q"]) < 1e-4,
          "uniq_p=%.6f uniq_q=%.6f" % (r1["uniq_p"], r1["uniq_q"]))
    check("CA identical models: common == R2 of the model",
          abs(r1["common_p_q"] - r1["p_r"] ** 2) < 1e-4,
          "common=%.6f R2=%.6f" % (r1["common_p_q"], r1["p_r"] ** 2))
    check("A1 identical models: partial R2 ~ 0",
          abs(r1["partialR2_p"]) < 1e-4 and abs(r1["partialR2_q"]) < 1e-4,
          "partialR2_p=%.6f partialR2_q=%.6f" %
          (r1["partialR2_p"], r1["partialR2_q"]))

    # thread reproducibility
    ca_run("ca1", ["-model_commonality", "nn,ak", "-bootstrap", "101",
                         "-boot_ci", "90"], env=env1)
    ca_run("caN", ["-model_commonality", "nn,ak", "-bootstrap", "101",
                         "-boot_ci", "90"], env=envN)
    _, t1 = read_table(os.path.join(work, "ca1.rsa.1D"), "nn")
    _, tN = read_table(os.path.join(work, "caN.rsa.1D"), "nn")
    check("CA commonality thread-reproducible (1 vs %d)" % threads,
          all(abs(x[k] - y[k]) < 1e-12
              for x, y in zip(t1, tN)
              for k in ("uniq_nn", "common_nn_ak", "partialR2_nn",
                        "uniq_nn_p", "common_nn_ak_p", "partialR2_nn_p",
                        "uniq_nn_bootLo", "uniq_nn_bootHi",
                        "common_nn_ak_bootLo", "common_nn_ak_bootHi",
                        "partialR2_nn_bootLo", "partialR2_nn_bootHi")))

    # F15 parser/smoke: classic RSA now accepts the same five-component
    # decomposition.  The small-condition exhaustive inference reference is
    # exercised below on the runwise fixture.
    rcx, outx = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                     "-model_mat", "vis", os.path.join(work, "vis.1D"),
                     "-model_mat", "s2", os.path.join(work, "sem.1D"),
                     "-model_commonality", "vis,s2", "-nperm", "0", "-no_dset",
                     "-prefix", os.path.join(work, "cax"), "-quiet"])
    _, cxrows = (read_table(os.path.join(work, "cax.rsa.1D"), "vis")
                 if rcx == 0 else ([], []))
    check("F15 classic commonality point estimates run",
          rcx == 0 and len(cxrows) > 0 and
          all(k in cxrows[0] for k in ("uniq_vis", "uniq_s2", "common_vis_s2",
                                       "partialR2_vis", "partialR2_s2")),
          "rc=%d %s" % (rcx, outx.strip()[-120:]))

    # =====================================================================
    # 3a. Noise-ceiling sub-brick maps.  IS-RSA writes a 'reliability' brick;
    #     classic RSA writes 'nc_low'/'nc_high'.  The painted value must equal
    #     the text-table column, and the bricks must be plain (not FIZT).  A2
    #     rejects pattern-mode IS-RSA because its flattened condition x voxel
    #     vector has no matched-repetition reliability axis.
    # =====================================================================
    rcp, outp = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                     "-featuretype", "pattern", "-model", "behav_nn", "behav:nn",
                     "-noise_ceiling", "-nperm", "0", "-no_dset",
                     "-prefix", os.path.join(work, "bad_pattern_nc"), "-quiet"])
    check("A2 noise ceiling rejects pattern-mode flattened split",
          rcp != 0 and "no valid reliability split" in outp and
          "matched-repetition axis" in outp, "rc=%d %s" % (rcp, outp.strip()[-240:]))

    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "IS-RSA",
                   "-model", "behav_nn", "behav:nn", "-noise_ceiling", "-nperm", "500",
                   "-seed", "1", "-prefix", os.path.join(work, "ncd"), "-quiet"])
    hd = os.path.join(work, "ncd+orig.HEAD")
    labs = head_brick_labs(hd)
    check("3a IS-RSA writes a 'reliability' sub-brick", "reliability" in labs,
          "labs=%s" % labs)
    if "reliability" in labs:
        _, nr = read_table(os.path.join(work, "ncd.rsa.1D"), "behav_nn")
        d = np.asarray(nib.load(hd).dataobj)
        av = np.asarray(nib.load(atlas).dataobj).ravel()
        bidx = labs.index("reliability")
        brick_roi1 = np.unique(d[..., bidx].ravel()[av == 1])
        check("3a reliability brick value == text-table column",
              len(brick_roi1) == 1 and abs(brick_roi1[0] - nr[0]["reliability"]) < 1e-5,
              "brick=%s table=%.6f" % (brick_roi1, nr[0]["reliability"]))
    rc, out = rsa(["-dataTableFile", table, "-mask", atlas, "-mode", "RSA",
                   "-model_mat", "vis", os.path.join(work, "vis.1D"), "-noise_ceiling",
                   "-nperm", "500", "-seed", "1",
                   "-prefix", os.path.join(work, "ncc"), "-quiet"])
    clabs = head_brick_labs(os.path.join(work, "ncc+orig.HEAD"))
    check("3a classic RSA writes 'nc_low' and 'nc_high' sub-bricks",
          "nc_low" in clabs and "nc_high" in clabs, "labs=%s" % clabs)

    # =====================================================================
    # 5. Mahalanobis behavioral profiles.  Reuse the planted subject datasets;
    #    write tables of behavioral measures and compare the saved model RDM to
    #    an independent numpy Mahalanobis (LW shrinkage + floored inverse).
    # =====================================================================
    import glob
    rng = np.random.default_rng(5)
    subs = sorted(glob.glob(os.path.join(work, "sub*.nii.gz")))
    nsub = len(subs)

    def mahal_ref(cols):
        Z = np.array(cols, float).T
        Z = (Z - Z.mean(0)) / Z.std(0)
        n, p = Z.shape
        R = (Z.T @ Z) / n
        d2 = ((R - np.eye(p))**2).sum()
        bb = sum(((np.outer(Z[i], Z[i]) - R)**2).sum() for i in range(n)) / (n*n)
        delta = min(1.0, max(0.0, (min(bb, d2)/d2) if d2 > 0 else 1.0))
        Rs = (1-delta)*R + delta*np.eye(p)
        w, V = np.linalg.eigh(Rs)
        wf = np.maximum(w, max(w[-1]*1e-8, 1e-12))
        Rinv = (V*(1/wf)) @ V.T
        D = np.zeros((n, n))
        for i in range(n):
            for j in range(i+1, n):
                dv = Z[i]-Z[j]; D[i, j] = D[j, i] = np.sqrt(max(dv @ Rinv @ dv, 0))
        dmax = D.max() or 1.0
        S = 1 - D/dmax; np.fill_diagonal(S, 1.0)
        return S

    def mk_tab(cols, names, fname):
        with open(fname, "w") as f:
            f.write("Subj " + " ".join(names) + " InputFile\n")
            for i in range(nsub):
                f.write("s%02d " % i + " ".join("%.6f" % cols[k][i] for k in range(len(cols)))
                        + " " + subs[i] + "\n")

    def save_rdm(model, tab, pre):
        rsa(["-dataTableFile", tab, "-mask", atlas, "-mode", "IS-RSA",
             "-model", "profile", model, "-metric", "spearman", "-nperm", "2", "-seed", "1",
             "-no_dset", "-save_rdm", os.path.join(work, pre),
             "-prefix", os.path.join(work, pre), "-quiet"])
        g = glob.glob(os.path.join(work, pre + "_model_*.1D"))
        return np.loadtxt(g[0]) if g else None

    A = rng.normal(size=nsub); B = rng.normal(size=nsub); C = A + 0.05*rng.normal(size=nsub)
    mk_tab([A, B, C], ["A", "B", "C"], os.path.join(work, "t3.txt"))
    M = save_rdm("A,B,C:mahal", os.path.join(work, "t3.txt"), "m3")
    check("5 Mahalanobis RDM matches numpy reference",
          M is not None and np.abs(M - mahal_ref([A, B, C])).max() < 1e-4,
          "max|diff|=%.2e" % (np.abs(M - mahal_ref([A, B, C])).max() if M is not None else -1))

    # exactly-orthogonal columns -> mahal == euclid
    Q, _ = np.linalg.qr(rng.normal(size=(nsub, 2)))
    mk_tab([Q[:, 0], Q[:, 1]], ["O1", "O2"], os.path.join(work, "to.txt"))
    Mm = save_rdm("O1,O2:mahal", os.path.join(work, "to.txt"), "mo")
    Me = save_rdm("O1,O2:euclid", os.path.join(work, "to.txt"), "eo")
    check("5 orthogonal measures: mahal reduces to standardized Euclidean",
          Mm is not None and Me is not None and np.abs(Mm - Me).max() < 1e-4,
          "max|diff|=%.2e" % (np.abs(Mm - Me).max() if Mm is not None else -1))

    # column-order invariance
    mk_tab([C, A, B], ["C", "A", "B"], os.path.join(work, "t3b.txt"))
    Mo = save_rdm("C,A,B:mahal", os.path.join(work, "t3b.txt"), "mob")
    check("5 Mahalanobis is column-order invariant",
          Mo is not None and np.abs(M - Mo).max() < 1e-5,
          "max|diff|=%.2e" % (np.abs(M - Mo).max() if Mo is not None else -1))

    # constant column -> clear rejection
    mk_tab([A, np.full(nsub, 3.0)], ["A", "K"], os.path.join(work, "tk.txt"))
    rck, ok = rsa(["-dataTableFile", os.path.join(work, "tk.txt"), "-mask", atlas,
                   "-mode", "IS-RSA", "-model", "A+K_mvM", "A,K:mahal", "-nperm", "2",
                   "-prefix", os.path.join(work, "mk"), "-quiet"])
    check("5 constant measure in :mahal rejected with a clear message",
          rck != 0 and "constant" in ok.lower(), "rc=%d" % rck)

    # =====================================================================
    # 4a. Runwise input contract.  Build a small subject x run fixture (betas +
    #     residuals), check valid input runs through the crossnobis estimator,
    #     and verify the loader rejects malformed tables.
    # =====================================================================
    rwd = os.path.join(work, "rw"); os.makedirs(rwd, exist_ok=True)
    NXr, NYr, NZr, NC, NTr = 6, 6, 4, 6, 40   # >= 6 conditions (matrix-size floor)
    aff = np.diag([3., 3., 3., 1.])

    def sav(fname, nv, shape=(NXr, NYr, NZr)):
        nib.save(nib.Nifti1Image(rng.normal(size=shape + (nv,)).astype(np.float32), aff),
                 os.path.join(rwd, fname))
    nib.save(nib.Nifti1Image(np.ones((NXr, NYr, NZr), np.int16), aff),
             os.path.join(rwd, "mask.nii.gz"))
    rmask = os.path.join(rwd, "mask.nii.gz")
    rows = []
    for s in range(3):
        for r in (1, 2, 3):
            b, e = "s%d_r%d_b.nii.gz" % (s, r), "s%d_r%d_e.nii.gz" % (s, r)
            sav(b, NC); sav(e, NTr)
            rows.append(("s%02d" % s, r, os.path.join(rwd, b), os.path.join(rwd, e)))

    def wr(fn, rws, cols="Subj Run InputFile ResidFile"):
        with open(os.path.join(rwd, fn), "w") as f:
            f.write(cols + "\n")
            for sj, r, b, e in rws:
                v = {"Subj": sj, "Run": str(r), "InputFile": b, "ResidFile": e}
                f.write(" ".join(v[c] for c in cols.split()) + "\n")

    wr("good.txt", rows)
    wr("noresid.txt", rows, "Subj Run InputFile")
    wr("onerun.txt", [r for r in rows if not (r[0] == "s00" and r[1] in (2, 3))])
    dup = list(rows); dup[1] = (dup[1][0], 1, dup[1][2], dup[1][3])
    wr("duprun.txt", dup)
    # a NC-condition model so a valid table runs end to end
    gm = (np.arange(NC) // 2)[:, None]
    np.savetxt(os.path.join(rwd, "m.1D"), (gm != gm.T).astype(float), fmt="%.1f")
    mm_arg = ["-model_mat", "m", os.path.join(rwd, "m.1D"), "-nperm", "200",
              "-seed", "1", "-no_dset"]

    def rw(fn, mask=rmask, mode="RSA", model=False):
        args = ["-mask", mask, "-runwiseTable", os.path.join(rwd, fn)]
        if mode: args = ["-mode", mode] + args
        if model: args += mm_arg + ["-prefix", os.path.join(rwd, "o_" + fn[:-4])]
        return rsa(args)

    rc, out = rw("good.txt", model=True)
    check("4a valid runwiseTable loads, validates, and runs",
          rc == 0 and "3 subjects" in out, out.strip()[-160:])
    rc, out = rw("noresid.txt", model=True)
    check("4a ResidFile is optional (runs without it)", rc == 0)
    rc, out = rw("onerun.txt")
    check("4a rejects < 2 runs per subject",
          rc != 0 and "only 1 run" in out)
    rc, out = rw("duprun.txt")
    check("4a rejects duplicate run label within a subject",
          rc != 0 and "two runs labeled" in out)
    rc, out = rw("good.txt", mode=None)
    check("4a requires -mode RSA", rc != 0 and "mode RSA" in out)
    rc, out = rw("good.txt", mask=atlas)   # 8x8x4 atlas vs 6x6x4 runwise grid
    check("4a rejects mask/runwise grid mismatch",
          rc != 0 and "voxels" in out.lower())

    # =====================================================================
    # 4b. Cross-validated squared Euclidean (crossnobis).  Plant a group
    #     structure in the condition betas across runs; the crossnobis RDM
    #     correlated with the matching block model must equal an independent
    #     numpy computation of the whole pipeline, be significant, produce
    #     negative distances (unbiasedness), and be thread-reproducible.
    # =====================================================================
    from scipy.stats import rankdata
    NXc, NYc, NZc, NCc, NRc, NSc = 5, 5, 3, 6, 4, 8
    NV = NXc * NYc * NZc
    cnd = os.path.join(work, "cn"); os.makedirs(cnd, exist_ok=True)
    nib.save(nib.Nifti1Image(np.ones((NXc, NYc, NZc), np.int16), aff),
             os.path.join(cnd, "mask.nii.gz"))
    grp = np.array([0, 0, 1, 1, 2, 2])
    G = rng.normal(size=(3, NV))
    beta = {}
    with open(os.path.join(cnd, "rw.txt"), "w") as f:
        f.write("Subj Run InputFile\n")
        for sj in range(NSc):
            for r in range(1, NRc + 1):
                B = np.stack([G[grp[c]] + 0.8 * rng.normal(size=NV) for c in range(NCc)])
                beta[(sj, r)] = B
                fn = os.path.join(cnd, "s%d_r%d.nii.gz" % (sj, r))
                nib.save(nib.Nifti1Image(B.T.reshape(NXc, NYc, NZc, NCc).astype(np.float32), aff), fn)
                f.write("s%02d %d %s\n" % (sj, r, fn))
    Mblk = (grp[:, None] != grp[None, :]).astype(float)
    np.savetxt(os.path.join(cnd, "block.1D"), Mblk, fmt="%.1f")

    def xnobis(runs):
        nr = len(runs); D = np.zeros((NCc, NCc))
        for i in range(NCc):
            for j in range(i + 1, NCc):
                dl = [runs[r][i] - runs[r][j] for r in range(nr)]
                S = np.sum(dl, 0); slf = sum((d * d).sum() for d in dl)
                D[i, j] = D[j, i] = (S @ S - slf) / (nr * (nr - 1) * NV)
        return D

    def tri(A):
        iu = np.triu_indices(A.shape[0], 1); return A[iu]

    def spear(a, b):
        ra, rb = rankdata(a) - np.mean(rankdata(a)), rankdata(b) - np.mean(rankdata(b))
        den = np.sqrt((ra * ra).sum()) * np.sqrt((rb * rb).sum())
        return (ra @ rb) / den if den > 0 else 0.0

    mt = tri(Mblk); zs = []; Ds = []; neg = False
    for sj in range(NSc):
        D = xnobis([beta[(sj, r)] for r in range(1, NRc + 1)])
        Ds.append(D)
        if (tri(D) < 0).any(): neg = True
        zs.append(np.arctanh(spear(tri(D), mt)))
    rr_ref = np.tanh(np.mean(zs))

    # F21: map local run bricks into a canonical condition set.  Individual
    # runs omit different conditions, reorder their bricks, and one run carries
    # two noisy repetitions whose mean is the run-level condition estimate.
    clabs = ["c%02d" % c for c in range(NCc)]
    urows, ubeta, upresent = [], {}, {}
    urw = os.path.join(cnd, "rw_unbalanced.txt")
    with open(urw, "w") as tf:
        tf.write("Subj Run ConditionFile InputFile\n")
        for sj in range(NSc):
            for r in range(1, NRc + 1):
                if r == 1:
                    order = [c for c in range(NCc) if c != sj % NCc]
                elif r == 2:
                    order = list(range(NCc)) + [(sj + 1) % NCc]
                elif r == 3:
                    order = [c for c in reversed(range(NCc)) if c != (sj + 2) % NCc]
                else:
                    order = list(np.roll(np.arange(NCc), sj % NCc))
                local = []
                dupc = (sj + 1) % NCc
                delta = 0.05 * rng.normal(size=NV)
                seen_dup = 0
                for c in order:
                    if r == 2 and c == dupc:
                        local.append(beta[(sj, r)][c] + (delta if seen_dup == 0 else -delta))
                        seen_dup += 1
                    else:
                        local.append(beta[(sj, r)][c])
                local = np.asarray(local)
                bfn = os.path.join(cnd, "u_s%d_r%d.nii.gz" % (sj, r))
                cfn = os.path.join(cnd, "u_s%d_r%d.cond" % (sj, r))
                nib.save(nib.Nifti1Image(local.T.reshape(NXc, NYc, NZc, len(local)).astype(np.float32),
                                         aff), bfn)
                with open(cfn, "w") as cf:
                    cf.write("# one label per local brick\n" +
                             "\n".join(clabs[int(c)] for c in order) + "\n")
                tf.write("s%02d %d %s %s\n" % (sj, r, cfn, bfn))
                canon = np.zeros((NCc, NV)); present = np.zeros(NCc, bool)
                for c in set(map(int, order)):
                    canon[c] = local[np.asarray(order) == c].mean(axis=0)
                    present[c] = True
                ubeta[(sj, r)] = canon; upresent[(sj, r)] = present
                urows.append((sj, r, cfn, bfn, list(map(int, order))))

    def xnobis_valid(runs, present):
        D = np.zeros((NCc, NCc))
        for i in range(NCc):
            for j in range(i + 1, NCc):
                use = [r for r in range(len(runs)) if present[r][i] and present[r][j]]
                dl = [runs[r][i] - runs[r][j] for r in use]
                S = np.sum(dl, axis=0); slf = sum((d * d).sum() for d in dl)
                D[i, j] = D[j, i] = (S @ S - slf) / (len(use) * (len(use) - 1) * NV)
        return D

    UDs, uzs = [], []
    for sj in range(NSc):
        D = xnobis_valid([ubeta[(sj, r)] for r in range(1, NRc + 1)],
                          [upresent[(sj, r)] for r in range(1, NRc + 1)])
        UDs.append(D); uzs.append(np.arctanh(spear(tri(D), mt)))
    urr_ref = np.tanh(np.mean(uzs))

    def nili_ceiling(rdms):
        """Nili lower/upper bounds over independently supplied subject RDMs."""
        st = np.stack([tri(D) for D in rdms])
        gm = st.mean(0)
        hi = np.mean([spear(st[s], gm) for s in range(len(st))])
        lo = np.mean([spear(st[s], (len(st) * gm - st[s]) / (len(st) - 1))
                      for s in range(len(st))])
        return lo, hi

    nc_ref = nili_ceiling(Ds)

    # A3 with runwise input: crossnobis produces each subject's inner condition
    # RDM, then correlation of their compact triangles produces the outer neural
    # subject matrix.  The runwise table has no behavioral columns, so exercise
    # the documented fixed subject-by-subject -model_mat contract.
    a3rt = np.stack([tri(D) for D in Ds])
    a3ro = np.corrcoef(a3rt)
    six = np.arange(NSc, dtype=float)
    a3rm = 1.0 - np.abs(six[:, None] - six[None, :]) / (NSc - 1)
    a3rmfn = os.path.join(cnd, "subject_model.1D")
    np.savetxt(a3rmfn, a3rm, fmt="%.8g")
    a3rr = spear(tri(a3ro), tri(a3rm))

    # F21 end-to-end classic and second-order paths.  A balanced ConditionFile
    # table must reduce to the old input exactly; malformed mappings and pairs
    # with fewer than two common runs must fail before data loading.
    fullcf = os.path.join(cnd, "all_conditions.cond")
    with open(fullcf, "w") as f:
        f.write("\n".join(clabs) + "\n")
    brw = os.path.join(cnd, "rw_balanced_mapped.txt")
    with open(brw, "w") as f:
        f.write("Subj Run ConditionFile InputFile\n")
        for sj in range(NSc):
            for r in range(1, NRc + 1):
                f.write("s%02d %d %s %s\n" %
                        (sj, r, fullcf, os.path.join(cnd, "s%d_r%d.nii.gz" % (sj, r))))

    # S6: already-estimated trial betas. Each run contains two independently
    # labeled trials per condition in shuffled brick order. Their average is
    # the run-level condition pattern consumed by crossnobis. Trial identity is
    # explicit and unique within subject; subject/run remain in runwiseTable.
    trw = os.path.join(cnd, "rw_trials.txt")
    trial_ids, trial_conds, trial_avg = {}, {}, {}
    trng = np.random.default_rng(20260829)  # isolate S6 from later legacy fixtures
    with open(trw, "w") as tf:
        tf.write("Subj Run TrialFile InputFile\n")
        for sj in range(NSc):
            for r in range(1, NRc + 1):
                local, tids, tconds = [], [], []
                for c in range(NCc):
                    delta = (0.06 * trng.normal(size=NV)).astype(np.float32)
                    base = beta[(sj, r)][c].astype(np.float32)
                    for rep, sign in enumerate((1.0, -1.0)):
                        local.append((base + sign * delta).astype(np.float32))
                        tids.append("s%02d_trial_r%02d_c%02d_%d" % (sj, r, c, rep + 1))
                        tconds.append(clabs[c])
                order = np.roll(np.arange(len(local))[::-1], sj + r)
                local = np.asarray(local, np.float32)[order]
                tids = [tids[i] for i in order]
                tconds = [tconds[i] for i in order]
                bfn = os.path.join(cnd, "trial_s%d_r%d.nii.gz" % (sj, r))
                dfn = os.path.join(cnd, "trial_s%d_r%d.txt" % (sj, r))
                nib.save(nib.Nifti1Image(
                    local.T.reshape(NXc, NYc, NZc, len(local)), aff), bfn)
                with open(dfn, "w") as df:
                    df.write("Trial Condition\n")
                    for tid, condition in zip(tids, tconds):
                        df.write("%s %s\n" % (tid, condition))
                tf.write("s%02d %d %s %s\n" % (sj, r, dfn, bfn))
                trial_ids[(sj, r)] = tids
                trial_conds[(sj, r)] = tconds
                trial_avg[(sj, r)] = np.stack([
                    local[np.asarray(tconds) == clabs[c]].mean(axis=0)
                    for c in range(NCc)])

    TDs, tzs = [], []
    for sj in range(NSc):
        D = xnobis([trial_avg[(sj, r)] for r in range(1, NRc + 1)])
        TDs.append(D); tzs.append(np.arctanh(spear(tri(D), mt)))
    trr_ref = np.tanh(np.mean(tzs))

    # Invalid S6 descriptors: a duplicated within-subject trial ID, the wrong
    # number of descriptor rows, and conflicting old/new mapping columns.
    dupdf = os.path.join(cnd, "trial_duplicate.txt")
    with open(dupdf, "w") as df:
        df.write("Trial Condition\n")
        dupids = list(trial_ids[(0, 2)])
        dupids[0] = trial_ids[(0, 1)][0]
        for tid, condition in zip(dupids, trial_conds[(0, 2)]):
            df.write("%s %s\n" % (tid, condition))
    shortdf = os.path.join(cnd, "trial_short.txt")
    with open(shortdf, "w") as df:
        df.write("Trial Condition\n")
        for tid, condition in zip(trial_ids[(0, 1)][:-1], trial_conds[(0, 1)][:-1]):
            df.write("%s %s\n" % (tid, condition))

    def trial_variant(fn, replacement=None, both=False):
        with open(fn, "w") as f:
            f.write("Subj Run TrialFile%s InputFile\n" %
                    (" ConditionFile" if both else ""))
            for sj in range(NSc):
                for r in range(1, NRc + 1):
                    dfn = os.path.join(cnd, "trial_s%d_r%d.txt" % (sj, r))
                    if replacement is not None and (sj, r) == replacement[0]:
                        dfn = replacement[1]
                    bfn = os.path.join(cnd, "trial_s%d_r%d.nii.gz" % (sj, r))
                    vals = ["s%02d" % sj, str(r), dfn]
                    if both:
                        vals.append(fullcf)
                    vals.append(bfn)
                    f.write(" ".join(vals) + "\n")

    trw_dup = os.path.join(cnd, "rw_trials_duplicate.txt")
    trw_short = os.path.join(cnd, "rw_trials_short.txt")
    trw_both = os.path.join(cnd, "rw_trials_both.txt")
    trial_variant(trw_dup, ((0, 2), dupdf))
    trial_variant(trw_short, ((0, 1), shortdf))
    trial_variant(trw_both, both=True)

    badrw = os.path.join(cnd, "rw_bad_pair.txt")
    with open(badrw, "w") as f:
        f.write("Subj Run ConditionFile InputFile\n")
        for sj, r, _, bfn, order in urows:
            badorder = list(order)
            if sj == 0 and r < 4:
                badorder = [4 if c == 5 else c for c in badorder]
            cfn = os.path.join(cnd, "bad_s%d_r%d.cond" % (sj, r))
            with open(cfn, "w") as cf:
                cf.write("\n".join(clabs[c] for c in badorder) + "\n")
            f.write("s%02d %d %s %s\n" % (sj, r, cfn, bfn))
    shortcf = os.path.join(cnd, "short.cond")
    with open(shortcf, "w") as f:
        f.write("\n".join(clabs[:-2]) + "\n")
    shortrw = os.path.join(cnd, "rw_short_map.txt")
    with open(shortrw, "w") as f:
        f.write("Subj Run ConditionFile InputFile\n")
        for sj, r, cfn, bfn, _ in urows:
            f.write("s%02d %d %s %s\n" % (sj, r, shortcf if (sj, r) == (0, 1) else cfn, bfn))

    def f21run(pre, tablefile, mode="RSA", search=False, env=None, dset=False):
        aa = ["-runwiseTable", tablefile, "-mask", os.path.join(cnd, "mask.nii.gz"),
              "-mode", mode]
        if mode == "RSA":
            aa += ["-model_mat", "block", os.path.join(cnd, "block.1D")]
        else:
            aa += ["-featuretype", "rdm", "-model_mat", "subject", a3rmfn]
        aa += ["-metric", "spearman", "-nperm", "61", "-seed", "313",
               "-prefix", os.path.join(cnd, pre)]
        if search:
            aa += ["-searchlight", "SPHERE(100)"]
        if not dset:
            aa += ["-no_dset"]
        rc0, out0 = rsa(aa, env=env)
        model = "block" if mode == "RSA" else "subject"
        tf = os.path.join(cnd, pre + ".rsa.1D")
        rows0 = read_table(tf, model)[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    furc, fuo, furows, fumeta = f21run("f21_unbalanced", urw, env=env1, dset=True)
    fbrc, fbo, fbrows, _ = f21run("f21_balanced_map", brw, env=env1)
    forigc, forigo, forigrows, _ = f21run("f21_balanced_old", os.path.join(cnd, "rw.txt"),
                                           env=env1)
    check("F21 unbalanced/repeated run conditions match pair-valid NumPy crossnobis",
          furc == 0 and len(furows) == 1 and
          abs(furows[0]["block_r"] - urr_ref) < 1e-4 and
          "pair-specific valid-run denominators" in fumeta and
          "# condition order: " + " ".join(clabs) in fumeta,
          "rc=%d got=%s ref=%.6f %s" %
          (furc, furows[0].get("block_r") if furows else None, urr_ref, fuo.strip()[-140:]))
    check("F21 balanced ConditionFile path equals legacy balanced input exactly",
          fbrc == forigc == 0 and fbrows == forigrows,
          "mapped=%s old=%s %s" % (fbrows[:1], forigrows[:1], (fbo + forigo).strip()[-120:]))
    rb, ob = f21run("f21_bad_pair", badrw)[:2]
    rs, os_ = f21run("f21_short_map", shortrw)[:2]
    check("F21 rejects condition pairs with fewer than two common runs",
          rb != 0 and "only 1 run" in ob and "crossnobis needs at least 2" in ob,
          "rc=%d %s" % (rb, ob.strip()[-160:]))
    check("F21 rejects ConditionFile/InputFile length mismatch",
          rs != 0 and "has 4 labels" in os_ and "InputFile has" in os_,
          "rc=%d %s" % (rs, os_.strip()[-160:]))

    Ua3ro = np.corrcoef(np.stack([tri(D) for D in UDs]))
    ua3ref = spear(tri(Ua3ro), tri(a3rm))
    uirc, uio, uirows, _ = f21run("f21_second_order", urw, mode="IS-RSA", env=env1)
    check("F21 unbalanced run mapping feeds second-order IS-RSA",
          uirc == 0 and len(uirows) == 1 and
          abs(uirows[0]["subject_r"] - ua3ref) < 1e-4,
          "rc=%d got=%s ref=%.6f %s" %
          (uirc, uirows[0].get("subject_r") if uirows else None, ua3ref, uio.strip()[-120:]))
    fs1, fso1, fsr1, _ = f21run("f21_sl_t1", urw, search=True, env=env1)
    fsn, fson, fsrn, _ = f21run("f21_sl_tN", urw, search=True, env=envN, dset=True)
    f21labs = head_brick_labs(os.path.join(cnd, "f21_sl_tN+orig.HEAD")) if fsn == 0 else []
    f21keys = ("block_r", "block_p", "block_q", "block_pfwe")
    check("F21 unbalanced atlas/searchlight maps and threads are identical",
          fs1 == fsn == 0 and len(fsr1) == len(fsrn) == NV and fsr1 == fsrn and
          all(all(row[k] == furows[0][k] for k in f21keys) for row in fsrn) and
          "block_r" in f21labs and "block_ZFWE" in f21labs,
          "rows=%d/%d labs=%s %s" %
          (len(fsr1), len(fsrn), f21labs, (fso1 + fson).strip()[-140:]))

    trc, tro, trrows, trmeta = f21run("s6_trials", trw, env=env1)
    check("S6 trial-beta descriptors match independent within-run aggregation/crossnobis",
          trc == 0 and len(trrows) == 1 and
          abs(trrows[0]["block_r"] - trr_ref) < 1e-4 and
          "# trial-beta descriptor: TrialFile (Subj/Run inherited)" in trmeta and
          "trial IDs unique within subject" in trmeta and
          "condition trials averaged within run" in trmeta,
          "rc=%d got=%s ref=%.6f %s" %
          (trc, trrows[0].get("block_r") if trrows else None,
           trr_ref, tro.strip()[-140:]))

    trirc, trio, trirows, _ = f21run("s6_trials_second_order", trw,
                                      mode="IS-RSA", env=env1)
    tr_outer = np.corrcoef(np.stack([tri(D) for D in TDs]))
    tr_isref = spear(tri(tr_outer), tri(a3rm))
    check("S6 trial-beta descriptors feed second-order IS-RSA",
          trirc == 0 and len(trirows) == 1 and
          abs(trirows[0]["subject_r"] - tr_isref) < 1e-4,
          "rc=%d got=%s ref=%.6f %s" %
          (trirc, trirows[0].get("subject_r") if trirows else None,
           tr_isref, trio.strip()[-120:]))

    trs1, trso1, trsrows1, _ = f21run("s6_trials_sl_t1", trw, search=True, env=env1)
    trsn, trson, trsrowsn, _ = f21run("s6_trials_sl_tN", trw, search=True,
                                       env=envN, dset=True)
    check("S6 trial-beta searchlight is atlas-equivalent and thread-reproducible",
          trs1 == trsn == 0 and len(trsrows1) == len(trsrowsn) == NV and
          trsrows1 == trsrowsn and
          all(all(row[k] == trrows[0][k] for k in f21keys) for row in trsrowsn),
          "rows=%d/%d %s" %
          (len(trsrows1), len(trsrowsn), (trso1 + trson).strip()[-140:]))

    trdc, trdo = f21run("s6_bad_duplicate", trw_dup)[:2]
    trsc, trso = f21run("s6_bad_short", trw_short)[:2]
    trbc, trbo = f21run("s6_bad_both", trw_both)[:2]
    check("S6 duplicate/count/conflicting descriptor contracts reject",
          trdc != 0 and "duplicate Trial ID" in trdo and "unique within subject" in trdo and
          trsc != 0 and "has 11 trial rows" in trso and "InputFile has 12 bricks" in trso and
          trbc != 0 and "both ConditionFile and TrialFile" in trbo,
          "dup=%d short=%d both=%d %s" %
          (trdc, trsc, trbc, (trdo + trso + trbo).strip()[-180:]))

    def a3_runwise(pre, searchlight=False, env=None, extra=None):
        aa = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
              os.path.join(cnd, "mask.nii.gz"), "-mode", "IS-RSA",
              "-featuretype", "rdm", "-model_mat", "subject", a3rmfn, "-metric", "spearman",
              "-nperm", "61", "-seed", "109", "-no_dset",
              "-prefix", os.path.join(cnd, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        if extra:
            aa += extra
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(cnd, pre + ".rsa.1D")
        rows0 = read_table(tf, "subject")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    a3ra, a3rao, a3rar, a3ram = a3_runwise(
        "a3_run_atlas", False, env1,
        ["-save_rdm", os.path.join(cnd, "a3_run_saved")])
    a3rs, a3rso, a3rsr, a3rsm = a3_runwise("a3_run_search", True, envN)
    a3saved = (np.loadtxt(os.path.join(cnd, "a3_run_saved_roi0001.1D"))
               if a3ra == 0 else np.empty((0, 0)))
    a3rkeys = ("subject_r", "subject_p", "subject_q", "subject_pfwe")
    check("A3 runwise crossnobis second-order matches independent outer RDM",
          a3ra == 0 and len(a3rar) == 1 and
          abs(a3rar[0]["subject_r"] - a3rr) < 1e-5 and
          np.allclose(a3saved, a3ro, atol=1e-5) and
          "# condition estimator: crossnobis (unwhitened)" in a3ram,
          "rc=%d got=%s ref=%.6f maxmat=%.3g %s" %
          (a3ra, a3rar[0]["subject_r"] if a3rar else None, a3rr,
           np.max(np.abs(a3saved - a3ro)) if a3saved.size else -1,
           a3rao.strip()[-120:]))
    check("A3 runwise atlas equals whole-volume searchlight",
          a3ra == a3rs == 0 and len(a3rsr) == NV and
          all(all(row[k] == a3rar[0][k] for k in a3rkeys) for row in a3rsr))
    _, _, a3r1, _ = a3_runwise("a3_run_t1", True, env1)
    _, _, a3rn, _ = a3_runwise("a3_run_tN", True, envN)
    check("A3 runwise second-order thread-reproducible (1 vs %d)" % threads,
          len(a3r1) == len(a3rn) == NV and
          all(all(a[k] == b[k] for k in a3rkeys) for a, b in zip(a3r1, a3rn)))
    a3bad, a3bado = rsa([
        "-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
        os.path.join(cnd, "mask.nii.gz"), "-mode", "IS-RSA",
        "-featuretype", "rdm", "-model", "behav_nn", "behav:nn",
        "-prefix", os.path.join(cnd, "a3_bad_model")])
    check("A3 runwise rejects unavailable subject-column models clearly",
          a3bad != 0 and "subject-level columns" in a3bado and "-model_mat" in a3bado)

    def runx(pre, extra=None, env=None):
        a = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
             os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA", "-model_mat", "block",
             os.path.join(cnd, "block.1D"), "-metric", "spearman", "-nperm",
             "2000", "-seed", "1", "-no_dset", "-prefix", os.path.join(cnd, pre)]
        return rsa(a + (extra or []), env=env)

    # F16: every subject RDM is already an independent-run crossnobis estimate.
    # The lower Nili bound excludes that subject from its group template; the
    # upper bound includes it intentionally as the conventional optimistic
    # ceiling.  No condition data are fitted because the model RDM is fixed.
    rc, ncout = runx("f16_nc", ["-noise_ceiling"])
    _, ncrows = (read_table(os.path.join(cnd, "f16_nc.rsa.1D"), "block")
                 if rc == 0 else ([], []))
    ncmeta = (open(os.path.join(cnd, "f16_nc.rsa.1D")).read()
              if rc == 0 else "")
    check("F16 runwise crossnobis ceiling matches independent Nili LOO reference",
          rc == 0 and len(ncrows) == 1 and
          np.allclose([ncrows[0]["nc_low"], ncrows[0]["nc_high"]], nc_ref,
                      atol=1e-5) and
          "run-independent crossnobis" in ncmeta,
          "rc=%d got=%s ref=%s %s" %
          (rc, ([ncrows[0].get("nc_low"), ncrows[0].get("nc_high")]
                if ncrows else None), nc_ref, ncout.strip()[-120:]))

    ncdpre = os.path.join(cnd, "f16_nc_dset")
    rc, ncdout = rsa([
        "-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
        os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA", "-model_mat", "block", os.path.join(cnd, "block.1D"), "-metric", "spearman",
        "-noise_ceiling", "-nperm", "200", "-seed", "1", "-prefix", ncdpre])
    nclabs = head_brick_labs(ncdpre + "+orig.HEAD") if rc == 0 else []
    if rc == 0 and "nc_low" in nclabs and "nc_high" in nclabs:
        ncar = np.asarray(nib.load(ncdpre + "+orig.HEAD").dataobj).reshape((NV, -1))
        ncmap = [np.unique(ncar[:, nclabs.index(x)]) for x in ("nc_low", "nc_high")]
    else:
        ncmap = []
    check("F16 runwise ceiling writes plain nc_low/nc_high AFNI bricks",
          len(ncmap) == 2 and all(len(x) == 1 for x in ncmap) and
          np.allclose([ncmap[0][0], ncmap[1][0]], nc_ref, atol=1e-5),
          "rc=%d labs=%s values=%s %s" % (rc, nclabs, ncmap, ncdout.strip()[-120:]))

    # F15 classic commonality: the five group effects are means of the subject
    # decompositions.  Unique/partial nulls relabel each subject's reduced-fit
    # residual RDM with the SAME condition permutation; common uses a complete
    # neural-condition relabeling.  Six conditions permit exhaustive 6! testing.
    grp_b = np.array([0, 1, 0, 1, 2, 2])
    Malt = (grp_b[:, None] != grp_b[None, :]).astype(float)
    maltfn = os.path.join(cnd, "alternate.1D")
    np.savetxt(maltfn, Malt, fmt="%.1f")

    # F22 consumes the subject RDM cache, so the same strict condition folds
    # must compose with F21's missing/repeated run mapping and pair-valid
    # crossnobis estimator without changing the CV contract.
    f22rwfold = os.path.join(cnd, "f22_fold.txt")
    with open(f22rwfold, "w") as f:
        f.write("left\nleft\nleft\nright\nright\nright\n")
    f22rwref, f22rwwref = f22_ref_classic(UDs, [Mblk, Malt], f22fold)
    f22rwpre = os.path.join(cnd, "f22_unbalanced")
    f22rwrc, f22rwout = rsa([
        "-runwiseTable", urw, "-mask", os.path.join(cnd, "mask.nii.gz"),
        "-mode", "RSA", "-model_mat", "A", os.path.join(cnd, "block.1D"), "-model_mat", "B", maltfn,
        "-metric", "pearson", "-model_fit", "MIX=A,B",
        "-fit_condfold", f22rwfold, "-nperm", "37", "-seed", "229",
        "-no_dset", "-prefix", f22rwpre])
    f22rwrows = (read_table(f22rwpre + ".rsa.1D", "A")[1] if f22rwrc == 0 else [])
    f22rwrow = f22rwrows[0] if len(f22rwrows) == 1 else {}
    check("F22 two-axis fit composes with F21 unbalanced crossnobis input",
          f22rwrc == 0 and abs(f22rwrow.get("MIX_cvR", 9) - f22rwref) < 3e-5 and
          np.allclose([f22rwrow.get("MIX_w_A", 9), f22rwrow.get("MIX_w_B", 9)],
                      f22rwwref, atol=3e-5),
          "got=%s/%s ref=%.6f/%s %s" %
          (f22rwrow.get("MIX_cvR"), [f22rwrow.get("MIX_w_A"), f22rwrow.get("MIX_w_B")],
           f22rwref, f22rwwref, f22rwout.strip()[-120:]))

    cperms = [np.asarray(p, int) for p in itertools.permutations(range(NCc))]

    # S5 runwise seed connectivity: both sides must be estimated with the same
    # independent-run crossnobis contract, rather than comparing a crossnobis
    # target with an ordinary or group-averaged seed RDM.  Keep seed and target
    # features disjoint, enumerate the complete condition-label null, and also
    # exercise the moving-neighborhood path.
    s5rw_seed = np.zeros((NXc, NYc, NZc), np.int16)
    s5rw_targ = np.zeros((NXc, NYc, NZc), np.int16)
    s5rw_seed.reshape(-1)[:10] = 9
    s5rw_targ.reshape(-1)[10:20] = 1
    s5rw_seedfn = os.path.join(cnd, "s5_seed_mask.nii.gz")
    s5rw_targfn = os.path.join(cnd, "s5_target_mask.nii.gz")
    nib.save(nib.Nifti1Image(s5rw_seed, aff), s5rw_seedfn)
    nib.save(nib.Nifti1Image(s5rw_targ, aff), s5rw_targfn)

    def xnobis_feat(runs):
        nr, nf = len(runs), runs[0].shape[1]
        D = np.zeros((NCc, NCc))
        for i in range(NCc):
            for j in range(i + 1, NCc):
                dl = [runs[r][i] - runs[r][j] for r in range(nr)]
                S = np.sum(dl, axis=0)
                slf = sum(np.dot(d, d) for d in dl)
                D[i, j] = D[j, i] = (np.dot(S, S) - slf) / (nr * (nr - 1) * nf)
        return D

    s5rw_sr, s5rw_tr, s5rw_z = [], [], []
    for sj in range(NSc):
        runs = [beta[(sj, r)].astype(np.float32) for r in range(1, NRc + 1)]
        SD = xnobis_feat([B[:, :10] for B in runs])
        TD = xnobis_feat([B[:, 10:20] for B in runs])
        s5rw_sr.append(SD); s5rw_tr.append(TD)
        s5rw_z.append(np.arctanh(np.clip(sc_pearson_tri(TD, SD), -.999329, .999329)))
    s5rw_obs = float(np.mean(s5rw_z))
    s5rw_null = []
    for p in cperms:
        s5rw_null.append(np.mean([
            np.arctanh(np.clip(sc_pearson_tri(TD, SD[np.ix_(p, p)]),
                               -.999329, .999329))
            for SD, TD in zip(s5rw_sr, s5rw_tr)]))
    s5rw_p = float(np.mean(np.abs(s5rw_null) >= abs(s5rw_obs) - 1e-12))

    def s5rw_run(pre, search=False, nperm=720, env=None):
        aa = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask", s5rw_targfn,
              "-seed_mask", s5rw_seedfn, "-mode", "RSA", "-metric", "pearson",
              "-classic_null", "conditions", "-nperm", str(nperm), "-seed", "431",
              "-no_dset", "-prefix", os.path.join(cnd, pre)]
        if search:
            aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(cnd, pre + ".rsa.1D")
        rows0 = read_table(tf, "seedROI9")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    s5rwa = s5rw_run("s5_runwise_exact", env=env1)
    s5rws = s5rw_run("s5_runwise_search", search=True, nperm=61, env=envN)
    check("S5 runwise seed connectivity matches crossnobis condition-null reference",
          s5rwa[0] == 0 and len(s5rwa[2]) == 1 and
          abs(s5rwa[2][0]["seedROI9_r"] - np.tanh(s5rw_obs)) < 3e-5 and
          abs(s5rwa[2][0]["seedROI9_p"] - s5rw_p) <= 1.0 / 720 + 1e-9 and
          s5rwa[2][0]["seedROI9_pfwe"] == s5rwa[2][0]["seedROI9_p"] and
          "subject-specific seed vs target condition-RDM" in s5rwa[3] and
          "# estimator: crossnobis (unwhitened)" in s5rwa[3],
          "rc=%d got=%s ref=%.6f/%.6g %s" %
          (s5rwa[0], s5rwa[2][0] if s5rwa[2] else None,
           np.tanh(s5rw_obs), s5rw_p, s5rwa[1].strip()[-120:]))
    check("S5 runwise seed connectivity composes with searchlight max-FWE",
          s5rws[0] == 0 and len(s5rws[2]) == 10 and
          all(abs(row["seedROI9_r"] - np.tanh(s5rw_obs)) < 3e-5 and
              0.0 <= row["seedROI9_pfwe"] <= 1.0 for row in s5rws[2]),
          "rc=%d nrow=%d %s" % (s5rws[0], len(s5rws[2]), s5rws[1].strip()[-120:]))

    f15_sub = []; f15_null = np.zeros((len(cperms), 5), float)
    for D in Ds:
        ob, nu = fl_commonality_reference(D, Mblk, Malt, cperms)
        f15_sub.append(ob); f15_null += nu
    f15_sub = np.asarray(f15_sub); f15_obs = f15_sub.mean(0)
    f15_null /= NSc
    # Model symmetries create exact permutation ties.  Compare them at the
    # implementation's float precision so inconsequential accumulation order
    # cannot split one equivalence class across the observed threshold.
    f15_tol = 64 * np.finfo(np.float32).eps * (1 + np.abs(f15_obs))
    f15_p = np.mean(np.abs(f15_null) >= np.abs(f15_obs) - f15_tol, axis=0)
    f15_keys = ("uniq_A", "uniq_B", "common_A_B", "partialR2_A", "partialR2_B")

    def f15_run(pre, nper=720, search=False, env=None, dset=False, boot=False):
        aa = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
              os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
              "-model_mat", "A", os.path.join(cnd, "block.1D"),
              "-model_mat", "B", maltfn,
              "-model_commonality", "A,B", "-metric", "spearman",
              "-nperm", str(nper), "-seed", "223", "-prefix", os.path.join(cnd, pre)]
        if search: aa += ["-searchlight", "SPHERE(100)"]
        if boot: aa += ["-bootstrap", "401", "-boot_ci", "90"]
        if not dset: aa += ["-no_dset"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(cnd, pre + ".rsa.1D")
        rows0 = read_table(tf, "A")[1] if rc0 == 0 else []
        meta0 = open(tf).read() if rc0 == 0 else ""
        return rc0, out0, rows0, meta0

    f15rc, f15out, f15rows, f15meta = f15_run("f15_exact", boot=True, dset=True,
                                               env=env1)
    f15row = f15rows[0] if len(f15rows) == 1 else {}
    check("F15 classic commonality effects and exhaustive condition-null p/FDR/FWE",
          f15rc == 0 and
          np.allclose([f15row.get(k, np.nan) for k in f15_keys], f15_obs, atol=2e-5) and
          np.allclose([f15row.get(k + "_p", np.nan) for k in f15_keys], f15_p,
                      atol=1e-8) and
          np.allclose([f15row.get(k + "_q", np.nan) for k in f15_keys], f15_p,
                      atol=1e-8) and
          np.allclose([f15row.get(k + "_pfwe", np.nan) for k in f15_keys], f15_p,
                      atol=1e-8) and
          "shared condition-label Freedman-Lane" in f15meta,
          "rc=%d got=%s p=%s ref=%s/%s %s" %
          (f15rc, [f15row.get(k) for k in f15_keys],
           [f15row.get(k + "_p") for k in f15_keys], f15_obs, f15_p,
           f15out.strip()[-120:]))

    bix = bootstrap_indices(NSc, 401, 223)
    f15_ci = np.asarray([percentile_linear([f15_sub[ix, c].mean() for ix in bix],
                                           (0.05, 0.95)) for c in range(5)])
    f15_got_ci = np.asarray([[f15row.get(k + "_bootLo", np.nan),
                              f15row.get(k + "_bootHi", np.nan)] for k in f15_keys])
    check("F15 classic commonality subject-bootstrap bounds match component means",
          np.allclose(f15_got_ci, f15_ci, atol=3e-5),
          "got=%s ref=%s" % (f15_got_ci, f15_ci))

    f15a, _, f15ar, _ = f15_run("f15_a61", nper=61, env=env1)
    f15s, f15so, f15sr, _ = f15_run("f15_sl61", nper=61, search=True,
                                     env=envN, dset=True)
    f15labs = (head_brick_labs(os.path.join(cnd, "f15_sl61+orig.HEAD"))
               if f15s == 0 else [])
    f15cmp = tuple(k + suf for k in f15_keys for suf in ("", "_p", "_q", "_pfwe"))
    check("F15 classic commonality atlas equals whole-volume searchlight maps",
          f15a == f15s == 0 and len(f15ar) == 1 and len(f15sr) == NV and
          all(all(row[k] == f15ar[0][k] for k in f15cmp) for row in f15sr) and
          all(k in f15labs and k + "_ZFWE" in f15labs for k in f15_keys),
          "atlas=%d search=%d labs=%s %s" %
          (len(f15ar), len(f15sr), f15labs, f15so.strip()[-120:]))

    _, _, f15t1, _ = f15_run("f15_t1", nper=61, search=True, env=env1)
    _, _, f15tN, _ = f15_run("f15_tN", nper=61, search=True, env=envN)
    check("F15 classic commonality thread-reproducible (1 vs %d)" % threads,
          len(f15t1) == len(f15tN) == NV and f15t1 == f15tN)

    # F8 classic path: subject decompositions are averaged, while the same
    # exhaustive condition permutation drives every subject and raw region.
    grp_c = np.array([0, 1, 2, 0, 2, 1])
    Mthird = (grp_c[:, None] != grp_c[None, :]).astype(float)
    mthirdfn = os.path.join(cnd, "third.1D"); np.savetxt(mthirdfn, Mthird, fmt="%.1f")
    f8c_sub, f8c_null = [], np.zeros((len(cperms), 10), float)
    for D in Ds:
        ob, nu = fl_commonality3_reference(D, Mblk, Malt, Mthird, cperms)
        f8c_sub.append(ob); f8c_null += nu
    f8c_sub = np.asarray(f8c_sub); f8c_obs = f8c_sub.mean(0); f8c_null /= NSc
    f8c_tol = 64 * np.finfo(np.float32).eps * (1 + np.abs(f8c_obs))
    f8c_p = np.mean(np.abs(f8c_null) >= np.abs(f8c_obs)[None, :] - f8c_tol, axis=0)
    f8c_names = ("uniq_A_given_B_C", "uniq_B_given_A_C", "uniq_C_given_A_B",
                 "common_A_B_not_C", "common_A_C_not_B", "common_B_C_not_A",
                 "common_A_B_C", "partialR2_A_given_B_C",
                 "partialR2_B_given_A_C", "partialR2_C_given_A_B")

    def f8c_run(pre, nper=720, search=False, env=None, dset=False, boot=False):
        aa = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
              os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
              "-model_mat", "A", os.path.join(cnd, "block.1D"),
              "-model_mat", "B", maltfn,
              "-model_mat", "C", mthirdfn,
              "-model_commonality", "A,B,C", "-metric", "spearman",
              "-nperm", str(nper), "-seed", "239", "-prefix", os.path.join(cnd, pre)]
        if search: aa += ["-searchlight", "SPHERE(100)"]
        if boot: aa += ["-bootstrap", "101", "-boot_ci", "90"]
        if not dset: aa += ["-no_dset"]
        rc0, out0 = rsa(aa, env=env); tf = os.path.join(cnd, pre + ".rsa.1D")
        rows0 = read_table(tf, "A")[1] if rc0 == 0 else []
        return rc0, out0, rows0

    f8crc, f8cout, f8crows = f8c_run("f8c_exact", dset=True, boot=True, env=env1)
    f8crow = f8crows[0] if len(f8crows) == 1 else {}
    f8c_ci = np.asarray([percentile_linear([f8c_sub[ix, q].mean()
                                            for ix in bootstrap_indices(NSc, 101, 239)],
                                           (0.05, 0.95)) for q in range(10)])
    f8cgot_ci = np.asarray([[f8crow.get(n + "_bootLo", np.nan),
                             f8crow.get(n + "_bootHi", np.nan)] for n in f8c_names])
    check("F8 classic effects/exhaustive condition null/bootstrap match NumPy",
          f8crc == 0 and
          np.allclose([f8crow.get(n, np.nan) for n in f8c_names], f8c_obs, atol=3e-5) and
          np.allclose([f8crow.get(n + "_p", np.nan) for n in f8c_names], f8c_p,
                      atol=1e-8) and
          np.allclose([f8crow.get(n + "_pfwe", np.nan) for n in f8c_names], f8c_p,
                      atol=1e-8) and np.allclose(f8cgot_ci, f8c_ci, atol=4e-5),
          "rc=%d p=%s/%s %s" %
          (f8crc, [f8crow.get(n + "_p") for n in f8c_names], f8c_p,
           f8cout.strip()[-120:]))
    f8ca, _, f8car = f8c_run("f8c_a61", nper=61, env=env1)
    f8cs1, _, f8csr1 = f8c_run("f8c_s1", nper=61, search=True, env=env1)
    f8csn, f8cso, f8csrn = f8c_run("f8c_sN", nper=61, search=True, env=envN, dset=True)
    f8clabs = (head_brick_labs(os.path.join(cnd, "f8c_sN+orig.HEAD"))
               if f8csn == 0 else [])
    f8ccmp = tuple(n + suf for n in f8c_names for suf in ("", "_p", "_q", "_pfwe"))
    check("F8 classic atlas/searchlight maps and threads are identical",
          f8ca == f8cs1 == f8csn == 0 and len(f8car) == 1 and
          len(f8csr1) == len(f8csrn) == NV and f8csr1 == f8csrn and
          all(all(row[k] == f8car[0][k] for k in f8ccmp) for row in f8csrn) and
          all(n in f8clabs and n + "_ZFWE" in f8clabs for n in f8c_names),
          "labels=%s %s" % (f8clabs, f8cso.strip()[-100:]))
    f8bad, f8bado = rsa([
        "-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
        os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
        "-model_mat", "A", os.path.join(cnd, "block.1D"),
        "-model_mat", "B", maltfn,
        "-model_mat", "C", mthirdfn,
        "-model_commonality", "A,B,C,A", "-nperm", "0", "-no_dset",
        "-prefix", os.path.join(cnd, "f8_bad")])
    check("F8 rejects more than three commonality predictors",
          f8bad != 0 and "more than three models" in f8bado,
          f8bado.strip()[-120:])

    f15bad, f15bado = rsa([
        "-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
        os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
        "-model_mat", "A", os.path.join(cnd, "block.1D"),
        "-model_mat", "B", maltfn, "-model_commonality", "A,B",
        "-metric", "ktaub", "-nperm", "20", "-no_dset",
        "-prefix", os.path.join(cnd, "f15_bad")])
    check("F15 classic commonality rejects non-regression metrics",
          f15bad != 0 and "pearson or spearman" in f15bado,
          "rc=%d %s" % (f15bad, f15bado.strip()[-120:]))

    plain_r = None
    rc, out = runx("out")
    if rc != 0:
        check("4b crossnobis runs", False, out.strip()[-200:])
    else:
        check("4b crossnobis runs", True)
        _, cr = read_table(os.path.join(cnd, "out.rsa.1D"), "block")
        plain_r = cr[0]["block_r"]
        check("4b crossnobis effect == numpy pipeline (exact)",
              abs(cr[0]["block_r"] - rr_ref) < 1e-4,
              "3dRSA=%.6f numpy=%.6f" % (cr[0]["block_r"], rr_ref))
        check("4b planted group structure is significant (p<.05)",
              cr[0]["block_p"] < 0.05, "p=%.4f" % cr[0]["block_p"])
        check("4b unbiased: crossnobis produces negative distances", neg)

    # S2 crossnobis safeguard: a run-specific pattern shared by every condition
    # cancels from each within-run contrast.  Verify that identity independently
    # and end to end, while rejecting the ordinary-only preprocessing switch so
    # it cannot be mistaken for a defined partition-wise policy.
    s2rw = os.path.join(cnd, "s2_shifted_rw.txt")
    s2Ds = []
    with open(s2rw, "w") as f:
        f.write("Subj Run InputFile\n")
        for sj in range(NSc):
            sruns = []
            for r in range(1, NRc + 1):
                base = (0.75 * np.sin(np.arange(NV) * 0.17 + sj + 2 * r))[None, :]
                Bs = beta[(sj, r)] + base
                sruns.append(Bs)
                fn = os.path.join(cnd, "s2_s%d_r%d.nii.gz" % (sj, r))
                nib.save(nib.Nifti1Image(
                    Bs.T.reshape(NXc, NYc, NZc, NCc).astype(np.float32), aff), fn)
                f.write("s%02d %d %s\n" % (sj, r, fn))
            s2Ds.append(xnobis(sruns))
    s2xpre = os.path.join(cnd, "s2_crossnobis_shift")
    s2xrc, s2xout = rsa([
        "-runwiseTable", s2rw, "-mask", os.path.join(cnd, "mask.nii.gz"),
        "-mode", "RSA", "-model_mat", "block", os.path.join(cnd, "block.1D"), "-metric", "spearman", "-nperm", "61",
        "-seed", "1", "-no_dset", "-prefix", s2xpre])
    s2xrows = (read_table(s2xpre + ".rsa.1D", "block")[1] if s2xrc == 0 else [])
    s2xmeta = open(s2xpre + ".rsa.1D").read() if s2xrc == 0 else ""
    check("S2 crossnobis condition contrasts are invariant to shared run patterns",
          s2xrc == 0 and len(s2xrows) == 1 and plain_r is not None and
          np.allclose(s2Ds, Ds, atol=1e-12) and
          abs(s2xrows[0]["block_r"] - plain_r) < 2e-5 and
          "crossnobis uses within-run condition contrasts" in s2xmeta,
          "rc=%d got=%s raw=%s maxref=%.3g %s" %
          (s2xrc, s2xrows[0].get("block_r") if s2xrows else None, plain_r,
           np.max(np.abs(np.asarray(s2Ds) - np.asarray(Ds))), s2xout.strip()[-120:]))
    s2xr, s2xro = runx("s2_bad_runwise_center", ["-center_conditions", "subject"])
    check("S2 ordinary subject centering rejects an implied runwise partition policy",
          s2xr != 0 and "currently applies only" in s2xro and
          "partition-wise preprocessing contract" in s2xro,
          "rc=%d %s" % (s2xr, s2xro.strip()[-160:]))

    # S1 on independently estimated runwise crossnobis RDMs.  The RDM
    # estimator is unchanged; only the group null moves from subject signs to
    # one shared condition relabeling.  Enumerate all 6! model relabelings.
    s1rw_null = []
    for p in cperms:
        Mp = Mblk[np.ix_(p, p)]
        s1rw_null.append(np.mean([np.arctanh(np.clip(spear(tri(D), tri(Mp)),
                                                     -.999329, .999329))
                                  for D in Ds]))
    s1rw_obs = float(np.mean(zs))
    s1rw_p = np.mean(np.abs(s1rw_null) >= abs(s1rw_obs) - 1e-12)
    r1rw, o1rw = runx("s1_rw_exact", ["-classic_null", "conditions",
                                       "-nperm", "720", "-seed", "317"])
    row1rw = (read_table(os.path.join(cnd, "s1_rw_exact.rsa.1D"), "block")[1][0]
              if r1rw == 0 else {})
    check("S1 runwise crossnobis condition null matches exhaustive reference",
          r1rw == 0 and abs(row1rw.get("block_r", np.nan) - np.tanh(s1rw_obs)) < 1e-5 and
          abs(row1rw.get("block_p", np.nan) - s1rw_p) <= 1.0 / 720 + 1e-9 and
          row1rw.get("block_pfwe") == row1rw.get("block_p"),
          "rc=%d got=%s ref=%.6f/%.6g %s" %
          (r1rw, (row1rw.get("block_r"), row1rw.get("block_p")),
           np.tanh(s1rw_obs), s1rw_p, o1rw.strip()[-120:]))

    # Classic RSA bootstrap resamples the independent subject Fisher-z effects,
    # then maps each resampled mean back to r.  Match every RNG draw and the
    # percentile definition against the independent subject-level NumPy values.
    rc, out = runx("boot", ["-bootstrap", "401", "-boot_ci", "90"])
    if rc == 0:
        _, bro = read_table(os.path.join(cnd, "boot.rsa.1D"), "block")
        bix = bootstrap_indices(NSc, 401, 1)
        bd = [np.tanh(np.mean(np.asarray(zs)[ix])) for ix in bix]
        bref = percentile_linear(bd, (0.05, 0.95))
        bgot = np.array([bro[0]["block_bootLo"], bro[0]["block_bootHi"]])
        check("4b classic subject-bootstrap CI == Fisher-z NumPy reference",
              np.allclose(bgot, bref, atol=3e-5),
              "3dRSA=%s numpy=%s" % (bgot, bref))
    else:
        check("4b classic subject-bootstrap CI == Fisher-z NumPy reference",
              False, out.strip()[-200:])

    # F2: resample condition indices synchronously through every subject neural
    # RDM and the model.  Duplicate selections of the same original condition
    # are artificial diagonal dyads and must be omitted.
    NCB = 401
    rc, out = runx("cboot", ["-cond_bootstrap", str(NCB), "-boot_ci", "90"])
    if rc == 0:
        _, cro = read_table(os.path.join(cnd, "cboot.rsa.1D"), "block")
        cix = bootstrap_indices(NCc, NCB, 1)
        cd, naive = [], []
        cz = np.full((NSc, NCB), np.nan)
        for iboot, ix in enumerate(cix):
            if len(set(map(int, ix))) < 3:
                continue
            zz, zn = [], []
            for D in Ds:
                da, ma, dn, mn = [], [], [], []
                for aa in range(NCc):
                    for bb in range(aa + 1, NCc):
                        ia, ib = int(ix[aa]), int(ix[bb])
                        dn.append(D[ia, ib]); mn.append(Mblk[ia, ib])
                        if ia != ib:
                            da.append(D[ia, ib]); ma.append(Mblk[ia, ib])
                zz.append(np.arctanh(spear(np.asarray(da), np.asarray(ma))))
                zn.append(np.arctanh(spear(np.asarray(dn), np.asarray(mn))))
            cz[:, iboot] = zz
            cd.append(np.tanh(np.mean(zz))); naive.append(np.tanh(np.mean(zn)))
        cref = percentile_linear(cd, (0.05, 0.95))
        nref = percentile_linear(naive, (0.05, 0.95))
        cgot = np.array([cro[0]["block_cbootLo"], cro[0]["block_cbootHi"]])
        check("F2 condition-bootstrap CI == synchronized NumPy reference",
              np.allclose(cgot, cref, atol=3e-5),
              "3dRSA=%s numpy=%s" % (cgot, cref))
        check("F2 condition bootstrap excludes duplicate-condition diagonals",
              np.max(np.abs(cref - nref)) > 1e-4 and
              np.max(np.abs(cgot - nref)) > 1e-4,
              "correct=%s naive=%s" % (cref, nref))

        # F6 uses all subject-only draws plus the usable condition-only and
        # paired simultaneous draws.  The corrected variance is bounded by its
        # one-axis components and Vsc, then converted to a small-sample t CI.
        rd, od = runx("dual", ["-bootstrap", str(NCB),
                                "-cond_bootstrap", str(NCB), "-boot_ci", "90"])
        drow = (read_table(os.path.join(cnd, "dual.rsa.1D"), "block")[1][0]
                if rd == 0 else {})
        dref = dual_boot_ci(zs, cz, bootstrap_indices(NSc, NCB, 1), NCc)
        dgot = np.asarray([drow.get("block_dualLo", np.nan),
                           drow.get("block_dualHi", np.nan)])
        dmeta = open(os.path.join(cnd, "dual.rsa.1D")).read() if rd == 0 else ""
        check("F6 crossnobis dual CI matches corrected NumPy reference",
              rd == 0 and np.allclose(dgot, dref, atol=4e-5) and
              "# dual_bootstrap:" in dmeta and "block_bootLo" not in drow and
              "block_cbootLo" not in drow,
              "3dRSA=%s numpy=%s %s" % (dgot, dref, od.strip()[-120:]))
    else:
        check("F2 condition-bootstrap CI == synchronized NumPy reference",
              False, out.strip()[-200:])
        check("F6 crossnobis dual CI matches corrected NumPy reference",
              False, out.strip()[-200:])

    # Group descriptor: variable-size groups are sampled as units and expanded
    # in original condition order.  Mirror that exact expansion independently.
    glab = ["A", "A", "A", "B", "B", "C"]
    gfn = os.path.join(cnd, "cond_groups.txt")
    with open(gfn, "w") as f:
        f.write("# one descriptor per condition\n" + "\n".join(glab) + "\n")
    rc, out = runx("cboot_group", ["-cond_bootstrap", str(NCB), "-boot_ci", "90",
                                    "-cond_group", gfn])
    if rc == 0:
        _, gro = read_table(os.path.join(cnd, "cboot_group.rsa.1D"), "block")
        members = [[0, 1, 2], [3, 4], [5]]
        gix = bootstrap_indices(len(members), NCB, 1)
        gd = []
        gz = np.full((NSc, NCB), np.nan)
        for iboot, gs in enumerate(gix):
            ix = [c for g in gs for c in members[int(g)]]
            if len(set(ix)) < 3:
                continue
            zz = []
            for D in Ds:
                da, ma = [], []
                for aa in range(len(ix)):
                    for bb in range(aa + 1, len(ix)):
                        ia, ib = ix[aa], ix[bb]
                        if ia != ib:
                            da.append(D[ia, ib]); ma.append(Mblk[ia, ib])
                zz.append(np.arctanh(spear(np.asarray(da), np.asarray(ma))))
            gz[:, iboot] = zz
            gd.append(np.tanh(np.mean(zz)))
        gref = percentile_linear(gd, (0.05, 0.95))
        ggot = np.array([gro[0]["block_cbootLo"], gro[0]["block_cbootHi"]])
        check("F2 variable-size condition groups match NumPy unit resampling",
              np.allclose(ggot, gref, atol=3e-5),
              "3dRSA=%s numpy=%s" % (ggot, gref))
        rg, og = runx("dual_group", ["-bootstrap", str(NCB),
                                      "-cond_bootstrap", str(NCB), "-boot_ci", "90",
                                      "-cond_group", gfn])
        grow = (read_table(os.path.join(cnd, "dual_group.rsa.1D"), "block")[1][0]
                if rg == 0 else {})
        gdref = dual_boot_ci(zs, gz, bootstrap_indices(NSc, NCB, 1), len(members))
        gdgot = np.asarray([grow.get("block_dualLo", np.nan),
                            grow.get("block_dualHi", np.nan)])
        check("F6 grouped-condition dual CI uses independent group count",
              rg == 0 and np.allclose(gdgot, gdref, atol=4e-5),
              "3dRSA=%s numpy=%s %s" % (gdgot, gdref, og.strip()[-120:]))
    else:
        check("F2 variable-size condition groups match NumPy unit resampling",
              False, out.strip()[-200:])
        check("F6 grouped-condition dual CI uses independent group count",
              False, out.strip()[-200:])

    _, _ = runx("cboot_t1", ["-cond_bootstrap", "101", "-boot_ci", "90"], env1)
    _, _ = runx("cboot_tN", ["-cond_bootstrap", "101", "-boot_ci", "90"], envN)
    ct1 = read_table(os.path.join(cnd, "cboot_t1.rsa.1D"), "block")[1][0]
    ctN = read_table(os.path.join(cnd, "cboot_tN.rsa.1D"), "block")[1][0]
    check("F2 condition-bootstrap CI thread-reproducible (1 vs %d)" % threads,
          ct1["block_cbootLo"] == ctN["block_cbootLo"] and
          ct1["block_cbootHi"] == ctN["block_cbootHi"])
    rc1, _ = runx("c1", env=env1); rcN, _ = runx("cN", env=envN)
    _, a1 = read_table(os.path.join(cnd, "c1.rsa.1D"), "block")
    _, aN = read_table(os.path.join(cnd, "cN.rsa.1D"), "block")
    check("4b crossnobis thread-reproducible (1 vs %d)" % threads,
          all(abs(x["block_r"] - y["block_r"]) < 1e-12 and
              abs(x["block_p"] - y["block_p"]) < 1e-12 for x, y in zip(a1, aN)))

    # Runwise model contrasts reuse the same subject-level crossnobis RDMs, but
    # their paired statistic is the mean within-subject Fisher-z difference.
    # Check that quantity independently and verify the contrast null/FWE is
    # thread-stable rather than merely checking that the command exits.
    grp_alt = np.array([0, 1, 0, 1, 2, 2])
    Malt = (grp_alt[:, None] != grp_alt[None, :]).astype(float)
    altfn = os.path.join(cnd, "alt.1D")
    np.savetxt(altfn, Malt, fmt="%.1f")
    mt_alt = tri(Malt)
    dz = []
    for sj in range(NSc):
        D = xnobis([beta[(sj, r)] for r in range(1, NRc + 1)])
        dz.append(np.arctanh(spear(tri(D), mt)) -
                  np.arctanh(spear(tri(D), mt_alt)))
    diff_ref = np.mean(dz)

    # F4: under exchangeable condition noise, the zero-distance covariance of
    # the compact RDM is V=(C C') o (C C').  Compute the quadratic form directly
    # (independent of 3dRSA's centered-second-moment shortcut), then exercise
    # both mean-removed corr_cov and origin-sensitive cosine_cov.
    C4 = np.zeros((len(mt), NCc)); iq = 0
    for ia in range(NCc):
        for ib in range(ia + 1, NCc):
            C4[iq, ia] = 1.0; C4[iq, ib] = -1.0; iq += 1
    V4 = (C4 @ C4.T) ** 2

    def f4cmp(a, b, center):
        a, b = np.asarray(a, float).copy(), np.asarray(b, float).copy()
        if center:
            a -= a.mean(); b -= b.mean()
        wa, wb = np.linalg.solve(V4, a), np.linalg.solve(V4, b)
        den = np.sqrt(a @ wa) * np.sqrt(b @ wb)
        return (a @ wb) / den if den > 0 else 0.0

    def f4_refs(center):
        zt = np.asarray([np.arctanh(np.clip(f4cmp(tri(D), mt, center),
                                                  -0.999329, 0.999329)) for D in Ds])
        za = np.asarray([np.arctanh(np.clip(f4cmp(tri(D), mt_alt, center),
                                                  -0.999329, 0.999329)) for D in Ds])
        meanD = np.mean(Ds, axis=0); hi, lo = [], []
        for sj, D in enumerate(Ds):
            loo_D = (meanD * NSc - D) / (NSc - 1)
            hi.append(f4cmp(tri(D), tri(meanD), center))
            lo.append(f4cmp(tri(D), tri(loo_D), center))
        bd = [np.tanh(np.mean(zt[ix])) for ix in bootstrap_indices(NSc, 101, 251)]
        return (np.tanh(zt.mean()), np.mean(zt - za),
                np.mean(lo), np.mean(hi), percentile_linear(bd, (0.05, 0.95)))

    f4out = {}
    for metric, center in (("corr_cov", True), ("cosine_cov", False)):
        pre = os.path.join(cnd, "f4_" + metric)
        aa = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
              os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
              "-model_mat", "true", os.path.join(cnd, "block.1D"),
              "-model_mat", "alt", altfn,
              "-model_contrast", "true-alt", "-metric", metric,
              "-noise_ceiling", "-bootstrap", "101", "-boot_ci", "90",
              "-nperm", "101", "-seed", "251", "-no_dset", "-prefix", pre]
        rc4, o4 = rsa(aa, env=env1)
        row4 = (read_table(pre + ".rsa.1D", "true")[1][0] if rc4 == 0 else {})
        meta4 = open(pre + ".rsa.1D").read() if rc4 == 0 else ""
        ref4 = f4_refs(center)
        check("F4 %s matches direct V^-1 primary/contrast/ceiling/bootstrap reference" % metric,
              rc4 == 0 and
              np.allclose([row4.get("true_r", np.nan),
                           row4.get("true-alt_zDiff", np.nan),
                           row4.get("nc_low", np.nan), row4.get("nc_high", np.nan)],
                          ref4[:4], atol=4e-5) and
              np.allclose([row4.get("true_bootLo", np.nan),
                           row4.get("true_bootHi", np.nan)], ref4[4], atol=4e-5) and
              ("%s comparison" % metric) in meta4 and "V=(C C') o (C C')" in meta4,
              "got=%s ref=%s %s" %
              ([row4.get("true_r"), row4.get("true-alt_zDiff"), row4.get("nc_low"),
                row4.get("nc_high"), row4.get("true_bootLo"), row4.get("true_bootHi")],
               ref4, o4.strip()[-120:]))
        f4out[metric] = (aa, row4)

        # S1 covariance-weighted fixed effects use the same transformed
        # subject RDMs but rebuild the jointly permuted model form each draw.
        # Direct dense V^-1 calculations remain independent of the C shortcut.
        sn, scn = [], []
        for p in cperms:
            Mp = Mblk[np.ix_(p, p)]; Map = Malt[np.ix_(p, p)]
            za = [np.arctanh(np.clip(f4cmp(tri(D), tri(Mp), center),
                                     -.999329, .999329)) for D in Ds]
            zb = [np.arctanh(np.clip(f4cmp(tri(D), tri(Map), center),
                                     -.999329, .999329)) for D in Ds]
            sn.append(np.mean(za)); scn.append(np.mean(np.asarray(za)-np.asarray(zb)))
        s1f4obs, s1f4cobs = sn[0], scn[0]
        tol4 = 64*np.finfo(np.float32).eps*(1+max(abs(s1f4obs),abs(s1f4cobs)))
        s1f4p = np.mean(np.abs(sn) >= abs(s1f4obs)-tol4)
        s1f4cp = np.mean(np.abs(scn) >= abs(s1f4cobs)-tol4)
        aac = list(aa)
        aac[aac.index("-prefix")+1] = os.path.join(cnd, "s1_" + metric)
        aac += ["-classic_null", "conditions", "-nperm", "720", "-seed", "319"]
        rc1f4, o1f4 = rsa(aac, env=env1)
        row1f4 = (read_table(os.path.join(cnd, "s1_" + metric + ".rsa.1D"),
                             "true")[1][0] if rc1f4 == 0 else {})
        check("S1 %s condition null matches direct V^-1 primary/contrast p" % metric,
              rc1f4 == 0 and
              abs(row1f4.get("true_p", np.nan)-s1f4p) <= 1.0/720+1e-9 and
              abs(row1f4.get("true-alt_p", np.nan)-s1f4cp) <= 1.0/720+1e-9,
              "got=%s ref=%s %s" %
              ((row1f4.get("true_p"),row1f4.get("true-alt_p")),
               (s1f4p,s1f4cp),o1f4.strip()[-120:]))

    # Whole-volume spheres are identical to the atlas fixture.  This checks
    # that transformed RDM caches are per-thread and read-only where intended.
    f4rows = []
    for tag, env in (("s1", env1), ("sN", envN)):
        aa = list(f4out["cosine_cov"][0])
        aa[aa.index("-prefix") + 1] = os.path.join(cnd, "f4_" + tag)
        aa += ["-searchlight", "SPHERE(100)"]
        rc4, o4 = rsa(aa, env=env)
        rows4 = (read_table(os.path.join(cnd, "f4_" + tag + ".rsa.1D"), "true")[1]
                 if rc4 == 0 else [])
        f4rows.append((rc4, o4, rows4))
    f4keys = ("true_r", "true_p", "true_q", "true_pfwe", "true-alt_rDiff", "true-alt_zDiff",
              "true-alt_p", "true-alt_q", "true-alt_pfwe", "nc_low", "nc_high",
              "true_bootLo", "true_bootHi")
    check("F4 cosine_cov atlas/searchlight agree and threads are identical",
          all(x[0] == 0 for x in f4rows) and
          len(f4rows[0][2]) == len(f4rows[1][2]) == NV and
          f4rows[0][2] == f4rows[1][2] and
          all(all(abs(row[k] - f4out["cosine_cov"][1][k]) < 2e-6 for k in f4keys)
              for row in f4rows[0][2]),
          "rows=%d/%d %s" % (len(f4rows[0][2]), len(f4rows[1][2]),
                              f4rows[1][1].strip()[-120:]))

    f4common = ["-mask", os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
                "-model_mat", "true", os.path.join(cnd, "block.1D"),
                "-metric", "corr_cov", "-nperm", "20", "-no_dset"]
    rcu, ou = rsa(["-runwiseTable", urw] + f4common +
                  ["-prefix", os.path.join(cnd, "f4_bad_unbalanced")])
    rct, ot = rsa(["-runwiseTable", trw] + f4common +
                  ["-prefix", os.path.join(cnd, "f4_bad_trials")])
    rcb, ob = rsa(["-runwiseTable", os.path.join(cnd, "rw.txt")] + f4common +
                  ["-cond_bootstrap", "20", "-prefix", os.path.join(cnd, "f4_bad_cboot")])
    rcj4, oj4 = rsa(["-runwiseTable", os.path.join(cnd, "rw.txt")] + f4common +
                    ["-model_joint", "-prefix", os.path.join(cnd, "f4_bad_joint")])
    rci4, oi4 = rsa(["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
                     os.path.join(cnd, "mask.nii.gz"), "-mode", "IS-RSA",
                     "-featuretype", "rdm", "-model_mat", "block", os.path.join(cnd, "block.1D"),
                     "-metric", "corr_cov", "-prefix", os.path.join(cnd, "f4_bad_is")])
    check("F4 rejects unequal-support, condition-bootstrap, regression, and IS-RSA misuse",
          rcu != 0 and "ConditionFile/TrialFile mappings" in ou and
          rct != 0 and "ConditionFile/TrialFile mappings" in ot and
          rcb != 0 and "cannot yet be combined with -cond_bootstrap" in ob and
          rcj4 != 0 and "fixed-model scalar comparison" in oj4 and
          rci4 != 0 and "balanced -runwiseTable crossnobis" in oi4,
          "unbalanced=%s trials=%s cboot=%s joint=%s is=%s" %
          (ou.strip()[-80:], ot.strip()[-80:], ob.strip()[-80:],
           oj4.strip()[-80:], oi4.strip()[-80:]))

    # Joint model: each condition draw refits the standardized/ranked compact
    # dyadic regression within every subject, then averages beta over subjects.
    jargs = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
             os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
             "-model_mat", "true", os.path.join(cnd, "block.1D"),
             "-model_mat", "alt", altfn, "-model_joint",
             "-metric", "spearman", "-nperm", "100", "-seed", "1",
             "-cond_bootstrap", "401", "-boot_ci", "90", "-no_dset",
             "-prefix", os.path.join(cnd, "cboot_joint")]
    rcj, oj = rsa(jargs)
    if rcj == 0:
        _, jro = read_table(os.path.join(cnd, "cboot_joint.rsa.1D"), "true")
        jd = []
        jbval = np.full((NSc, 401), np.nan)
        for iboot, ix in enumerate(bootstrap_indices(NCc, 401, 1)):
            if len(set(map(int, ix))) < 3:
                continue
            bs = []
            for D in Ds:
                y, xa, xb = [], [], []
                for aa in range(NCc):
                    for bb in range(aa + 1, NCc):
                        ia, ib = int(ix[aa]), int(ix[bb])
                        if ia != ib:
                            y.append(D[ia, ib]); xa.append(Mblk[ia, ib]); xb.append(Malt[ia, ib])
                def rz(v):
                    v = rankdata(np.asarray(v, float)); v -= v.mean()
                    sd = np.sqrt(np.mean(v * v))
                    return v / sd if sd > 0 else np.zeros_like(v)
                X = np.column_stack((rz(xa), rz(xb)))
                bs.append(np.linalg.pinv(X) @ rz(y))
            jbval[:, iboot] = np.asarray(bs)[:, 0]
            jd.append(np.mean(bs, axis=0))
        jref = np.vstack(jd)
        jlo = percentile_linear(jref[:, 0], (0.05,))[0]
        jhi = percentile_linear(jref[:, 0], (0.95,))[0]
        jgot = np.array([jro[0]["true_cbootLo"], jro[0]["true_cbootHi"]])
        check("F2 joint condition-bootstrap refits compact regression per draw",
              np.allclose(jgot, [jlo, jhi], atol=5e-4),
              "3dRSA=%s numpy=%s" % (jgot, [jlo, jhi]))

        jvals = []
        for D in Ds:
            X = np.column_stack((rz(tri(Mblk)), rz(tri(Malt))))
            jvals.append((np.linalg.pinv(X) @ rz(tri(D)))[0])
        djargs = list(jargs)
        djargs[djargs.index(os.path.join(cnd, "cboot_joint"))] = os.path.join(cnd, "dual_joint")
        djargs += ["-bootstrap", "401"]
        rdj, odj = rsa(djargs)
        djrow = (read_table(os.path.join(cnd, "dual_joint.rsa.1D"), "true")[1][0]
                 if rdj == 0 else {})
        djref = dual_boot_ci(jvals, jbval, bootstrap_indices(NSc, 401, 1), NCc,
                             do_tanh=False)
        djgot = np.asarray([djrow.get("true_dualLo", np.nan),
                            djrow.get("true_dualHi", np.nan)])
        check("F6 joint-regression dual CI matches corrected NumPy reference",
              rdj == 0 and np.allclose(djgot, djref, atol=6e-4),
              "3dRSA=%s numpy=%s %s" % (djgot, djref, odj.strip()[-120:]))
    else:
        check("F2 joint condition-bootstrap refits compact regression per draw",
              False, oj.strip()[-200:])
        check("F6 joint-regression dual CI matches corrected NumPy reference",
              False, oj.strip()[-200:])

    def runxc(pre, env=None, extra=None):
        a = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
             os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
             "-model_mat", "true", os.path.join(cnd, "block.1D"),
             "-model_mat", "alt", altfn,
             "-model_contrast", "true-alt", "-metric", "spearman",
             "-nperm", "500", "-seed", "1", "-no_dset",
             "-prefix", os.path.join(cnd, pre)]
        rc, out = rsa(a + (extra or []), env=env)
        tf = os.path.join(cnd, pre + ".rsa.1D")
        rows = read_table(tf, "true")[1] if rc == 0 and os.path.exists(tf) else []
        return rc, out, rows

    rcc, cout, crow = runxc("contrast")
    check("4b runwise contrast matches paired Fisher-z numpy reference",
          rcc == 0 and len(crow) == 1 and
          abs(crow[0]["true-alt_zDiff"] - diff_ref) < 1e-4 and
          0 <= crow[0]["true-alt_p"] <= crow[0]["true-alt_pfwe"] <= 1,
          "rc=%d ref=%.6f" % (rcc, diff_ref))
    _, _, cc1 = runxc("contrast_t1", env1)
    _, _, ccN = runxc("contrast_tN", envN)
    ccols = ("true-alt_rDiff", "true-alt_zDiff", "true-alt_p", "true-alt_q", "true-alt_pfwe")
    check("4b runwise contrast thread-reproducible (1 vs %d)" % threads,
          len(cc1) == len(ccN) == 1 and
          all(cc1[0][k] == ccN[0][k] for k in ccols))

    _, bout, bcrow = runxc("contrast_boot", extra=["-bootstrap", "401",
                                                     "-boot_ci", "90"])
    dzdraw = [np.mean(np.asarray(dz)[ix])
              for ix in bootstrap_indices(NSc, 401, 1)]
    dzref = percentile_linear(dzdraw, (0.05, 0.95))
    dzgot = (np.array([bcrow[0]["true-alt_zDiff_bootLo"],
                       bcrow[0]["true-alt_zDiff_bootHi"]])
             if len(bcrow) == 1 else np.array([np.nan, np.nan]))
    check("F17 classic crossnobis contrast CI == paired Fisher-z reference",
          np.allclose(dzgot, dzref, atol=3e-5),
          "3dRSA=%s reference=%s %s" % (dzgot, dzref, bout.strip()[-120:]))

    # F6 fixed-model contrasts retain pairing on both axes: every condition
    # draw evaluates A and B on the same compact dyads and every simultaneous
    # draw selects the same subjects for the two sides.
    dcval = np.full((NSc, NCB), np.nan)
    for iboot, ix in enumerate(bootstrap_indices(NCc, NCB, 1)):
        if len(set(map(int, ix))) < 3:
            continue
        for sj, D in enumerate(Ds):
            dy, ma, mb = [], [], []
            for aa in range(NCc):
                for bb in range(aa + 1, NCc):
                    ia, ib = int(ix[aa]), int(ix[bb])
                    if ia != ib:
                        dy.append(D[ia, ib]); ma.append(Mblk[ia, ib]); mb.append(Malt[ia, ib])
            dcval[sj, iboot] = (np.arctanh(spear(np.asarray(dy), np.asarray(ma))) -
                                np.arctanh(spear(np.asarray(dy), np.asarray(mb))))
    _, dout, dcrow = runxc("contrast_dual", extra=["-bootstrap", str(NCB),
                                                       "-cond_bootstrap", str(NCB),
                                                       "-boot_ci", "90"])
    dcref = dual_boot_ci(dz, dcval, bootstrap_indices(NSc, NCB, 1), NCc,
                         do_tanh=False)
    dcgot = (np.asarray([dcrow[0].get("true-alt_zDiff_dualLo", np.nan),
                         dcrow[0].get("true-alt_zDiff_dualHi", np.nan)])
             if len(dcrow) == 1 else np.asarray([np.nan, np.nan]))
    check("F6 paired fixed-model contrast dual CI matches NumPy reference",
          np.allclose(dcgot, dcref, atol=4e-5),
          "3dRSA=%s numpy=%s %s" % (dcgot, dcref, dout.strip()[-120:]))

    # =====================================================================
    # 4c. Noise-normalized crossnobis.  Add residuals with spatially-correlated,
    #     heterogeneous-variance noise; whitening must match an independent numpy
    #     computation for both diag and shrinkage, reduce to plain crossnobis on
    #     white noise, be run-label-swap invariant, and reject a missing ResidFile.
    # =====================================================================
    NTr2 = 80
    L = rng.normal(size=(NV, NV)) * 0.3 + np.eye(NV)      # spatial mixing
    sc = np.exp(rng.normal(size=NV) * 0.5)                # per-voxel variance spread
    resid = {}
    with open(os.path.join(cnd, "rww.txt"), "w") as f:
        f.write("Subj Run InputFile ResidFile\n")
        for sj in range(NSc):
            for r in range(1, NRc + 1):
                E = (rng.normal(size=(NTr2, NV)) @ L.T) * sc
                resid[(sj, r)] = E
                fe = os.path.join(cnd, "s%d_r%d_e.nii.gz" % (sj, r))
                nib.save(nib.Nifti1Image(E.T.reshape(NXc, NYc, NZc, NTr2).astype(np.float32), aff), fe)
                fb = os.path.join(cnd, "s%d_r%d.nii.gz" % (sj, r))  # reuse 4b betas
                f.write("s%02d %d %s %s\n" % (sj, r, fb, fe))

    def whiten(sj, mode):
        R = np.vstack([resid[(sj, r)] - resid[(sj, r)].mean(0) for r in range(1, NRc + 1)])
        n = R.shape[0]
        if mode == "diag":
            var = (R * R).sum(0) / n; med = np.median(var[var > 0])
            w = 1 / np.sqrt(np.maximum(var, med))
            return [beta[(sj, r)] * w for r in range(1, NRc + 1)]
        S = R.T @ R / n; mu = np.trace(S) / NV
        d2 = ((S - mu * np.eye(NV)) ** 2).sum()
        bb = sum(((np.outer(R[t], R[t]) - S) ** 2).sum() for t in range(n)) / (n * n)
        dl = min(1.0, max(0.0, (min(bb, d2) / d2) if d2 > 0 else 1.0))
        Sr = (1 - dl) * S + dl * mu * np.eye(NV); w, V = np.linalg.eigh(Sr)
        Wh = (V * (1 / np.sqrt(np.maximum(w, max(w[-1] * 1e-8, 1e-12))))) @ V.T
        return [(Wh @ beta[(sj, r)].T).T for r in range(1, NRc + 1)]

    def ref_w(mode):
        zs = [np.arctanh(spear(tri(xnobis(whiten(sj, mode))), mt)) for sj in range(NSc)]
        return np.tanh(np.mean(zs))

    # F21 composes the local-to-global condition mapping with subject-pooled
    # residual whitening.  Missing conditions remain excluded pairwise after
    # the present run-level patterns have been transformed.
    with open(os.path.join(cnd, "urww.txt"), "w") as f:
        f.write("Subj Run ConditionFile InputFile ResidFile\n")
        for sj, r, cfn, bfn, _ in urows:
            efn = os.path.join(cnd, "s%d_r%d_e.nii.gz" % (sj, r))
            f.write("s%02d %d %s %s %s\n" % (sj, r, cfn, bfn, efn))

    def ref_unbalanced_diag():
        zs = []
        for sj in range(NSc):
            R = np.vstack([resid[(sj, r)] - resid[(sj, r)].mean(0)
                           for r in range(1, NRc + 1)])
            var = (R * R).sum(0) / R.shape[0]
            med = np.median(var[var > 0])
            w = 1 / np.sqrt(np.maximum(var, med))
            D = xnobis_valid([ubeta[(sj, r)] * w for r in range(1, NRc + 1)],
                              [upresent[(sj, r)] for r in range(1, NRc + 1)])
            zs.append(np.arctanh(spear(tri(D), mt)))
        return np.tanh(np.mean(zs))

    def runw(pre, mode, tbl="rww.txt", env=None):
        a = ["-runwiseTable", os.path.join(cnd, tbl), "-mask", os.path.join(cnd, "mask.nii.gz"),
             "-mode", "RSA", "-model_mat", "block", os.path.join(cnd, "block.1D"), "-metric", "spearman",
             "-nperm", "500", "-seed", "1", "-no_dset", "-prefix", os.path.join(cnd, pre)]
        if mode != "none": a += ["-noise_norm", mode]
        rsa(a, env=env)
        g = os.path.join(cnd, pre + ".rsa.1D")
        return read_table(g, "block")[1][0]["block_r"] if os.path.exists(g) else None

    # A requested whitening mode without residuals is a contract error, not an
    # implicit fallback to unwhitened crossnobis.
    rc, out = rsa(["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
                   os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
                   "-model_mat", "block", os.path.join(cnd, "block.1D"),
                   "-noise_norm", "diag", "-nperm", "20", "-no_dset",
                   "-prefix", os.path.join(cnd, "missing_resid")])
    check("4c noise_norm rejects runwiseTable without ResidFile",
          rc != 0 and "ResidFile" in out, "rc=%d" % rc)

    # Exact white residual covariance: +/- basis vectors have zero column means,
    # zero off-diagonal covariance, and equal variance.  Both whitening modes
    # may apply a common scalar, but RDM correlation must equal plain crossnobis.
    Ew = np.vstack([np.eye(NV), -np.eye(NV)]).astype(np.float32)
    ewfn = os.path.join(cnd, "white_resid.nii.gz")
    nib.save(nib.Nifti1Image(Ew.T.reshape(NXc, NYc, NZc, 2 * NV), aff), ewfn)
    with open(os.path.join(cnd, "rwi.txt"), "w") as f:
        f.write("Subj Run InputFile ResidFile\n")
        for sj in range(NSc):
            for r in range(1, NRc + 1):
                fb = os.path.join(cnd, "s%d_r%d.nii.gz" % (sj, r))
                f.write("s%02d %d %s %s\n" % (sj, r, fb, ewfn))
    wid = runw("w_identity_diag", "diag", tbl="rwi.txt")
    wis = runw("w_identity_shrink", "shrinkage", tbl="rwi.txt")
    check("4c identity-noise whitening reduces to plain crossnobis",
          plain_r is not None and wid is not None and wis is not None and
          abs(wid - plain_r) < 1e-12 and abs(wis - plain_r) < 1e-12,
          "plain=%s diag=%s shrink=%s" % (plain_r, wid, wis))

    for mode in ("diag", "shrinkage"):
        got = runw("w_" + mode, mode)
        ref = ref_w("diag" if mode == "diag" else "shrink")
        check("4c %s whitening == numpy reference (exact)" % mode,
              got is not None and abs(got - ref) < 1e-4,
              "3dRSA=%s numpy=%.6f" % (got, ref))

    # F4 comparison-space whitening composes after (and is distinct from) the
    # residual-derived voxel-space whitening used to estimate each crossnobis.
    Dwd = [xnobis(whiten(sj, "diag")) for sj in range(NSc)]
    f4wref = np.tanh(np.mean([np.arctanh(np.clip(f4cmp(tri(D), mt, False),
                                                     -0.999329, 0.999329)) for D in Dwd]))
    f4wpre = os.path.join(cnd, "f4_diag_cosine_cov")
    rc4w, o4w = rsa([
        "-runwiseTable", os.path.join(cnd, "rww.txt"), "-mask",
        os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
        "-model_mat", "block", os.path.join(cnd, "block.1D"), "-metric", "cosine_cov",
        "-noise_norm", "diag", "-nperm", "101", "-seed", "251", "-no_dset",
        "-prefix", f4wpre])
    f4wrow = (read_table(f4wpre + ".rsa.1D", "block")[1][0] if rc4w == 0 else {})
    check("F4 covariance comparison composes with residual voxel whitening",
          rc4w == 0 and abs(f4wrow.get("block_r", np.nan) - f4wref) < 4e-5,
          "3dRSA=%s numpy=%.6f %s" %
          (f4wrow.get("block_r"), f4wref, o4w.strip()[-120:]))

    f21w = runw("f21_w_diag", "diag", tbl="urww.txt")
    f21w_ref = ref_unbalanced_diag()
    check("F21 unbalanced condition mapping composes with residual whitening",
          f21w is not None and abs(f21w - f21w_ref) < 1e-4,
          "3dRSA=%s numpy=%.6f" % (f21w, f21w_ref))

    wnc_ref = nili_ceiling([
        xnobis(whiten(sj, "diag")) for sj in range(NSc)
    ])
    wncpre = os.path.join(cnd, "f16_nc_diag")
    rc, wncout = rsa([
        "-runwiseTable", os.path.join(cnd, "rww.txt"), "-mask",
        os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA", "-model_mat", "block", os.path.join(cnd, "block.1D"), "-metric", "spearman", "-noise_norm", "diag",
        "-noise_ceiling", "-nperm", "200", "-seed", "1", "-no_dset",
        "-prefix", wncpre])
    _, wncrows = (read_table(wncpre + ".rsa.1D", "block")
                  if rc == 0 else ([], []))
    check("F16 residual-whitened crossnobis ceiling matches independent reference",
          rc == 0 and len(wncrows) == 1 and
          np.allclose([wncrows[0]["nc_low"], wncrows[0]["nc_high"]], wnc_ref,
                      atol=1e-5),
          "rc=%d got=%s ref=%s %s" %
          (rc, ([wncrows[0].get("nc_low"), wncrows[0].get("nc_high")]
                if wncrows else None), wnc_ref, wncout.strip()[-120:]))

    # run-label swap invariance: relabel runs 1..R as R..1, same result
    with open(os.path.join(cnd, "rww.txt")) as fh:
        L2 = fh.read().splitlines()
    with open(os.path.join(cnd, "rsw.txt"), "w") as fh:
        fh.write(L2[0] + "\n")
        for ln in L2[1:]:
            p = ln.split(); p[1] = str(NRc + 1 - int(p[1])); fh.write(" ".join(p) + "\n")
    sw = runw("w_sw", "shrinkage", tbl="rsw.txt")
    ba = runw("w_ba", "shrinkage")
    check("4c whitening invariant to run-label swap",
          sw is not None and abs(sw - ba) < 1e-9)

    # thread reproducibility of the whitened result
    t1 = runw("w_t1", "shrinkage", env=env1)
    tN = runw("w_tN", "shrinkage", env=envN)
    check("4c whitened crossnobis thread-reproducible", abs(t1 - tN) < 1e-12)

    # =====================================================================
    # 6. Runwise classic-RSA searchlights.  SPHERE(100) covers this entire
    #    small volume, so every moving neighborhood must reproduce the same
    #    independently computed NumPy crossnobis result used above.  Exercise
    #    unwhitened, diagonal, and shrinkage estimates plus map/FWE/thread output.
    # =====================================================================
    cmask = np.asarray(nib.load(os.path.join(cnd, "mask.nii.gz")).dataobj).ravel() > 0

    def runx_sl(pre, mode="none", env=None, extra=None):
        tbl = "rw.txt" if mode == "none" else "rww.txt"
        a = ["-runwiseTable", os.path.join(cnd, tbl), "-mask",
             os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
             "-model_mat", "block", os.path.join(cnd, "block.1D"),
             "-searchlight", "SPHERE(100)", "-metric", "spearman",
             "-nperm", "200", "-seed", "1", "-prefix", os.path.join(cnd, pre)]
        if mode != "none":
            a += ["-noise_norm", mode]
        if extra:
            a += extra
        rc, out = rsa(a, env=env)
        hd = os.path.join(cnd, pre + "+orig.HEAD")
        tf = os.path.join(cnd, pre + ".rsa.1D")
        if rc != 0 or not os.path.exists(hd) or not os.path.exists(tf):
            return None, out
        ar = np.asarray(nib.load(hd).dataobj)
        ar = ar.reshape((NV, -1))[cmask]
        labs = head_brick_labs(hd)
        rows = read_table(tf, "block")[1]
        return {"map": ar, "labs": labs, "rows": rows}, out

    sl = {}
    slref = {"none": rr_ref, "diag": ref_w("diag"), "shrinkage": ref_w("shrink")}
    for wmode in ("none", "diag", "shrinkage"):
        sl[wmode], out = runx_sl("sl_" + wmode, wmode)
        vals = None if sl[wmode] is None else sl[wmode]["map"][:, 0]
        check("6 crossnobis searchlight %s == numpy everywhere" % wmode,
              vals is not None and np.allclose(vals, slref[wmode], atol=1e-5),
              "rc/output=%s ref=%.6f" % (out.strip()[-120:], slref[wmode]))

    sn = sl.get("none")
    check("6 crossnobis searchlight labels and FWE are valid",
          sn is not None and sn["labs"][:3] == ["block_r", "block_Z", "block_ZFWE"] and
          all(0 <= r["block_p"] <= r["block_pfwe"] <= 1 for r in sn["rows"]))

    fnc, fncout = runx_sl("f16_nc_sl", "none", env=envN,
                          extra=["-noise_ceiling"])
    if fnc is not None and "nc_low" in fnc["labs"] and "nc_high" in fnc["labs"]:
        flo, fhi = fnc["labs"].index("nc_low"), fnc["labs"].index("nc_high")
        fncmap = fnc["map"][:, [flo, fhi]]
        fncrow = np.asarray([[r["nc_low"], r["nc_high"]] for r in fnc["rows"]])
    else:
        fncmap = fncrow = np.empty((0, 2))
    check("F16 runwise ceiling atlas equals whole-volume searchlight maps/table",
          fnc is not None and fncmap.shape == (NV, 2) and
          np.allclose(fncmap, nc_ref, atol=1e-5) and
          np.allclose(fncrow, nc_ref, atol=1e-5),
          "map=%s rows=%s ref=%s %s" %
          (fncmap.shape, fncrow.shape, nc_ref, fncout.strip()[-120:]))

    fnc1, _ = runx_sl("f16_nc_t1", "none", env=env1,
                      extra=["-noise_ceiling"])
    fncN, _ = runx_sl("f16_nc_tN", "none", env=envN,
                      extra=["-noise_ceiling"])
    check("F16 runwise ceiling thread-reproducible (1 vs %d)" % threads,
          fnc1 is not None and fncN is not None and
          np.array_equal(fnc1["map"], fncN["map"]) and
          fnc1["rows"] == fncN["rows"])

    st1, _ = runx_sl("sl_t1", "none", env=env1)
    stN, _ = runx_sl("sl_tN", "none", env=envN)
    check("6 crossnobis searchlight thread-reproducible (1 vs %d)" % threads,
          st1 is not None and stN is not None and
          np.array_equal(st1["map"], stN["map"]) and
          all(x["block_p"] == y["block_p"] and
              x["block_pfwe"] == y["block_pfwe"]
              for x, y in zip(st1["rows"], stN["rows"])))

    csl, cout = runx_sl("sl_cboot", "none", extra=["-cond_bootstrap", "401",
                                                          "-boot_ci", "90"])
    car = read_table(os.path.join(cnd, "cboot.rsa.1D"), "block")[1][0]
    if csl is not None:
        ilo, ihi = csl["labs"].index("block_cbootLo"), csl["labs"].index("block_cbootHi")
        okmap = (np.allclose(csl["map"][:, ilo], car["block_cbootLo"], atol=1e-5) and
                 np.allclose(csl["map"][:, ihi], car["block_cbootHi"], atol=1e-5))
    else:
        okmap = False
    check("F2 crossnobis searchlight condition-CI maps equal whole-ROI reference",
          okmap, cout.strip()[-160:])

    _, _ = runx("dual101", ["-bootstrap", "101", "-cond_bootstrap", "101",
                              "-boot_ci", "90"])
    dcar = read_table(os.path.join(cnd, "dual101.rsa.1D"), "block")[1][0]
    dsl1, dslo1 = runx_sl("sl_dual_t1", "none", env=env1,
                          extra=["-bootstrap", "101", "-cond_bootstrap", "101",
                                 "-boot_ci", "90"])
    dslN, dsloN = runx_sl("sl_dual_tN", "none", env=envN,
                          extra=["-bootstrap", "101", "-cond_bootstrap", "101",
                                 "-boot_ci", "90"])
    if dsl1 is not None and dslN is not None and "block_dualLo" in dslN["labs"]:
        dlo, dhi = dslN["labs"].index("block_dualLo"), dslN["labs"].index("block_dualHi")
        dualmap_ok = (np.array_equal(dsl1["map"], dslN["map"]) and
                      np.allclose(dslN["map"][:, dlo], dcar["block_dualLo"], atol=1e-5) and
                      np.allclose(dslN["map"][:, dhi], dcar["block_dualHi"], atol=1e-5) and
                      dsl1["rows"] == dslN["rows"])
    else:
        dualmap_ok = False
    check("F6 dual CI atlas/searchlight maps and threads are identical",
          dualmap_ok, (dslo1 + dsloN).strip()[-180:])

    # F11 runs before resident searchlight inputs are loaded.  A deliberately
    # tiny explicit limit makes refusal deterministic on every machine; the
    # override must acknowledge the same estimate and allow the job to finish.
    mem_base = ["-runwiseTable", os.path.join(cnd, "rw.txt"), "-mask",
                os.path.join(cnd, "mask.nii.gz"), "-mode", "RSA",
                "-model_mat", "block", os.path.join(cnd, "block.1D"),
                "-searchlight", "SPHERE(100)", "-metric", "spearman",
                "-nperm", "20", "-seed", "1", "-no_dset",
                "-memory_limit", "0.000001"]
    rc, mout = rsa(mem_base + ["-prefix", os.path.join(cnd, "mem_refuse")])
    check("F11 searchlight memory limit refuses before loading",
          rc != 0 and "searchlight memory preflight" in mout and
          "exceeds the" in mout and "-memory_override" in mout,
          "rc=%d %s" % (rc, mout.strip()[-180:]))

    rc, mout = rsa(mem_base + ["-memory_override", "-prefix",
                               os.path.join(cnd, "mem_override")])
    check("F11 explicit memory override continues",
          rc == 0 and "searchlight memory preflight" in mout and
          "continuing because -memory_override was given" in mout and
          os.path.exists(os.path.join(cnd, "mem_override.rsa.1D")),
          "rc=%d %s" % (rc, mout.strip()[-180:]))

    # =====================================================================
    # 7. -model_dset under searchlight.  Stream a second modality through the
    #    moving neighborhood.  With a radius covering the whole volume every
    #    sphere equals the whole ROI, so every searchlight voxel must equal the
    #    atlas single-ROI cross-modal result -- an exact consistency check.
    # =====================================================================
    md = os.path.join(work, "md"); os.makedirs(md, exist_ok=True)
    NXm, NYm, NZm, NTm, NSm = 4, 4, 3, 40, 12
    NVm = NXm * NYm * NZm
    nib.save(nib.Nifti1Image(np.ones((NXm, NYm, NZm), np.int16), aff),
             os.path.join(md, "mask.nii.gz"))
    theta = np.sort(rng.normal(size=NSm))
    S1, S2 = rng.normal(size=NTm), rng.normal(size=NTm)
    with open(os.path.join(md, "tab.txt"), "w") as f:
        f.write("Subj Theta Pair Mod2 Mod3 InputFile\n")
        for i in range(NSm):
            s1 = np.cos(theta[i]) * S1 + np.sin(theta[i]) * S2
            d1 = np.stack([s1 + 0.3 * rng.normal(size=NTm) for _ in range(NVm)])
            T1, T2 = rng.normal(size=NTm), rng.normal(size=NTm)
            s2 = np.cos(theta[i]) * T1 + np.sin(theta[i]) * T2
            d2 = np.stack([s2 + 0.3 * rng.normal(size=NTm) for _ in range(NVm)])
            f1 = os.path.join(md, "s%02d_m1.nii.gz" % i)
            f2 = os.path.join(md, "s%02d_m2.nii.gz" % i)
            nib.save(nib.Nifti1Image(d1.T.reshape(NXm, NYm, NZm, NTm).astype(np.float32), aff), f1)
            nib.save(nib.Nifti1Image(d2.T.reshape(NXm, NYm, NZm, NTm).astype(np.float32), aff), f2)
            # Mod3 reuses the main-modality file: it is still a second
            # per-location model and keeps this already-large fixture compact.
            f.write("s%02d %.9g p%02d %s %s %s\n" %
                    (i, theta[i], i // 2, f2, f1, f1))
    mmk = os.path.join(md, "mask.nii.gz"); mtab = os.path.join(md, "tab.txt")

    def md_atlas(ft):
        a = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA", "-model_dset",
             "Mod2", "Mod2", "-metric", "spearman", "-nperm", "100", "-seed", "1", "-no_dset",
             "-prefix", os.path.join(md, "a_" + ft)]
        if ft == "pattern": a += ["-featuretype", "pattern"]
        rsa(a)
        return read_table(os.path.join(md, "a_" + ft + ".rsa.1D"), "Mod2")[1][0]["Mod2_r"]

    def md_sl(ft):
        a = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA", "-model_dset",
             "Mod2", "Mod2", "-searchlight", "SPHERE(100)", "-metric", "spearman", "-nperm",
             "100", "-seed", "1", "-prefix", os.path.join(md, "s_" + ft)]
        if ft == "pattern": a += ["-featuretype", "pattern"]
        rc, out = rsa(a)
        hd = os.path.join(md, "s_" + ft + "+orig.HEAD")
        if rc != 0 or not os.path.exists(hd):
            return None, out
        d = np.asarray(nib.load(hd).dataobj)[..., 0].ravel()
        m = np.asarray(nib.load(mmk).dataobj).ravel() > 0
        return d[m], out

    for ft in ("mean", "pattern"):
        va = md_atlas(ft)
        vs, out = md_sl(ft)
        check("7 model_dset searchlight (%s) == atlas everywhere" % ft,
              vs is not None and np.allclose(vs, va, atol=1e-5),
              "atlas=%.6f sl=[%.6f,%.6f]" % (va, (vs.min() if vs is not None else -9),
                                             (vs.max() if vs is not None else -9)))

    # The same shared subject draws must also work when both neural and model
    # RDMs are rebuilt per searchlight from separate modalities.
    ba = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
          "-model_dset", "M2", "Mod2", "-metric", "spearman",
          "-nperm", "0", "-bootstrap", "101", "-boot_ci", "90", "-seed", "19",
          "-no_dset", "-prefix", os.path.join(md, "boot_atlas")]
    bs = ba[:-3] + ["-searchlight", "SPHERE(100)",
                    "-prefix", os.path.join(md, "boot_sl")]
    rca, oa = rsa(ba); rcs, os_ = rsa(bs)
    ar = read_table(os.path.join(md, "boot_atlas.rsa.1D"), "M2")[1] if rca == 0 else []
    sr = read_table(os.path.join(md, "boot_sl.rsa.1D"), "M2")[1] if rcs == 0 else []
    check("7 model_dset bootstrap atlas == searchlight everywhere",
          len(ar) == 1 and len(sr) == NVm and
          all(abs(x["M2_bootLo"] - ar[0]["M2_bootLo"]) < 1e-6 and
              abs(x["M2_bootHi"] - ar[0]["M2_bootHi"]) < 1e-6 for x in sr),
          "atlas_rc=%d sl_rc=%d" % (rca, rcs))

    # F3 keeps a cross-modal model fixed while shifting the main modality.
    # A whole-volume sphere must therefore match the atlas result for the
    # observed effect and both synchronized-null p-values.
    ta = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
          "-model_dset", "M2", "Mod2", "-metric", "spearman",
          "-null", "timeshift", "-min_shift", "3", "-nperm", "20", "-seed", "23",
          "-no_dset", "-prefix", os.path.join(md, "shift_atlas")]
    ts = ta[:-3] + ["-searchlight", "SPHERE(100)", "-no_dset",
                    "-prefix", os.path.join(md, "shift_sl")]
    rcta, ota = rsa(ta); rcts, ots = rsa(ts)
    tar = read_table(os.path.join(md, "shift_atlas.rsa.1D"), "M2")[1] if rcta == 0 else []
    tsr = read_table(os.path.join(md, "shift_sl.rsa.1D"), "M2")[1] if rcts == 0 else []
    skeys = ("M2_r", "M2_p", "M2_pfwe")
    check("F3 model_dset timeshift atlas == searchlight everywhere",
          len(tar) == 1 and len(tsr) == NVm and
          all(all(x[k] == tar[0][k] for k in skeys) for x in tsr),
          "atlas_rc=%d sl_rc=%d %s %s" % (rcta, rcts, ota[-80:], ots[-80:]))

    # F18 keeps each per-location modality unshifted while the main InputFile
    # series moves.  Exercise the joint coefficients and paired contrast
    # together, including the volumetric output labels and threaded searchlight.
    def f18md_run(pre, searchlight=False, env=None):
        aa = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
              "-model_dset", "M2", "Mod2",
              "-model_dset", "M3", "Mod3",
              "-model_joint", "-model_contrast", "M3-M2", "-metric", "spearman",
              "-null", "timeshift", "-min_shift", "3", "-nperm", "31",
              "-seed", "29", "-prefix", os.path.join(md, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(md, pre + ".rsa.1D")
        return rc0, out0, read_table(tf, "M2")[1] if rc0 == 0 else []

    rca18m, oa18m, a18m = f18md_run("f18md_atlas", False, env1)
    rcs18m, os18m, s18m = f18md_run("f18md_sl", True, envN)
    k18m = ("M2_b", "M2_p", "M2_pfwe", "M3_b", "M3_p", "M3_pfwe",
            "M3-M2_diff", "M3-M2_p", "M3-M2_pfwe")
    l18m = head_brick_labs(os.path.join(md, "f18md_sl+orig.HEAD")) if rcs18m == 0 else []
    check("F18 model_dset regression/contrast timeshift atlas == searchlight maps",
          len(a18m) == 1 and len(s18m) == NVm and
          all(all(x[k] == a18m[0][k] for k in k18m) for x in s18m) and
          all(x in l18m for x in ("M2_b", "M2_Z", "M2_ZFWE", "M3-M2_diff",
                                  "M3-M2_Zdiff", "M3-M2_ZdiffFWE")),
          "atlas_rc=%d sl_rc=%d rows=%d labels=%s %s %s" %
          (rca18m, rcs18m, len(s18m), l18m, oa18m[-80:], os18m[-80:]))

    _, _, s18m1 = f18md_run("f18md_t1", True, env1)
    _, _, s18mN = f18md_run("f18md_tN", True, envN)
    check("F18 model_dset timeshift thread-reproducible (1 vs %d)" % threads,
          len(s18m1) == len(s18mN) == NVm and
          all(all(x[k] == y[k] for k in k18m) for x, y in zip(s18m1, s18mN)))

    # The pre-run model-correlation diagnostic samples locations before the
    # analysis.  In a searchlight it must reduce each sampled sphere on the fly;
    # atlas-only cmean storage does not exist in streaming mode.
    ma = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
          "-model_dset", "M2", "Mod2",
          "-model_dset", "M3", "Mod3",
          "-searchlight", "SPHERE(100)", "-metric", "spearman",
          "-nperm", "20", "-seed", "1", "-no_dset",
          "-prefix", os.path.join(md, "multi")]
    rc, out = rsa(ma)
    mh = []
    if rc == 0 and os.path.exists(os.path.join(md, "multi.rsa.1D")):
        mh = read_table(os.path.join(md, "multi.rsa.1D"), "M2")[0]
    check("7 multi-model model_dset searchlight runs streaming diagnostic",
          rc == 0 and "mean model correlations over" in out and
          "M2_r" in mh and "M3_r" in mh, "rc=%d" % rc)

    # F13: paired contrasts may compare two RDMs rebuilt independently at each
    # location.  Pair blocks make the permutation group exactly 2^(NSm/2), so
    # an independent exhaustive reference can verify the paired same-relabeling
    # null rather than merely checking that the formerly rejected command runs.
    def md_whole_rdm(stem):
        ss = []
        for i in range(NSm):
            x = np.asarray(nib.load(os.path.join(md, "s%02d_%s.nii.gz" % (i, stem))).dataobj)
            x = x.reshape(NVm, NTm).astype(np.float32)
            ss.append(x.sum(axis=0, dtype=np.float32) / np.float32(NVm))
        return np.corrcoef(np.asarray(ss))

    neu13 = md_whole_rdm("m1")
    m213 = md_whole_rdm("m2")
    m313 = neu13.copy()
    d13 = spearman_tri(neu13, m313) - spearman_tri(neu13, m213)
    null13 = []
    for bits in range(1 << (NSm // 2)):
        pi = np.arange(NSm)
        for j in range(NSm // 2):
            if (bits >> j) & 1:
                pi[2*j], pi[2*j+1] = pi[2*j+1], pi[2*j]
        null13.append(spearman_tri(neu13, m313[np.ix_(pi, pi)]) -
                      spearman_tri(neu13, m213[np.ix_(pi, pi)]))
    p13 = float(np.mean(np.abs(null13) >= abs(d13)))

    def f13_run(pre, searchlight=False, env=None):
        aa = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
              "-model_dset", "M2", "Mod2",
              "-model_dset", "M3", "Mod3",
              "-model_contrast", "M3-M2", "-metric", "spearman",
              "-block", "Pair", "-nperm", "100", "-seed", "41",
              "-prefix", os.path.join(md, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc, out = rsa(aa, env=env)
        tf = os.path.join(md, pre + ".rsa.1D")
        rows = read_table(tf, "M2")[1] if rc == 0 and os.path.exists(tf) else []
        return rc, out, rows

    rc13, o13, a13 = f13_run("f13_atlas")
    got13 = a13[0] if len(a13) == 1 else {}
    check("F13 model_dset contrast matches exhaustive paired-null reference",
          rc13 == 0 and abs(got13.get("M3-M2_diff", -9) - d13) < 3e-5 and
          abs(got13.get("M3-M2_p", -9) - p13) < 1e-12 and
          got13.get("M3-M2_pfwe", -9) == got13.get("M3-M2_p", -8),
          "rc=%d got=%s ref=(%.6f,%.6f)" % (rc13, got13, d13, p13))

    rcs13, os13, s13 = f13_run("f13_sl", True, envN)
    labs13 = head_brick_labs(os.path.join(md, "f13_sl+orig.HEAD")) if rcs13 == 0 else []
    k13 = ("M3-M2_diff", "M3-M2_p", "M3-M2_pfwe")
    check("F13 model_dset contrast atlas == searchlight maps everywhere",
          len(a13) == 1 and len(s13) == NVm and
          all(all(x[k] == a13[0][k] for k in k13) for x in s13) and
          all(x in labs13 for x in ("M3-M2_diff", "M3-M2_Zdiff", "M3-M2_ZdiffFWE")),
          "atlas_rc=%d sl_rc=%d rows=%d labels=%s" % (rc13, rcs13, len(s13), labs13))

    _, _, s13t1 = f13_run("f13_t1", True, env1)
    _, _, s13tN = f13_run("f13_tN", True, envN)
    check("F13 model_dset contrast thread-reproducible (1 vs %d)" % threads,
          len(s13t1) == len(s13tN) == NVm and
          all(all(x[k] == y[k] for k in k13) for x, y in zip(s13t1, s13tN)))

    # Mixed fixed/per-location contrasts must use the ordinary paired path even
    # though F9 caches the fixed model's primary searchlight test.
    mix13 = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
             "-model", "T", "Theta:nn",
             "-model_dset", "M2", "Mod2",
             "-model_contrast", "T-M2", "-metric", "spearman",
             "-searchlight", "SPHERE(100)", "-nperm", "20", "-seed", "19",
             "-no_dset", "-prefix", os.path.join(md, "f13_mixed")]
    rcmix, omix = rsa(mix13, env=envN)
    mixrows = read_table(os.path.join(md, "f13_mixed.rsa.1D"), "T")[1] if rcmix == 0 else []
    check("F13 fixed + model_dset contrast coexists with F9 cache",
          len(mixrows) == NVm and all(abs(x["T-M2_diff"] -
                                            (x["T_r"] - x["M2_r"])) < 2e-6
                                      for x in mixrows),
          "rc=%d rows=%d %s" % (rcmix, len(mixrows), omix[-100:]))

    # F17: per-location contrast bounds must rebuild and resample both model
    # RDMs jointly at each center.  The whole-volume sphere makes every center
    # equal to the atlas result, while NumPy independently reproduces each draw.
    NB17, SD17 = 201, 29
    bd17 = []
    from scipy.stats import spearmanr
    for ix in bootstrap_indices(NSm, NB17, SD17):
        yn, xa, xb = [], [], []
        for aa in range(NSm):
            for bb in range(aa + 1, NSm):
                ia, ib = int(ix[aa]), int(ix[bb])
                if ia != ib:
                    yn.append(neu13[ia, ib])
                    xa.append(m313[ia, ib])
                    xb.append(m213[ia, ib])
        if len(set(map(int, ix))) >= 3 and len(yn) >= 3:
            bd17.append(float(spearmanr(yn, xa).statistic -
                              spearmanr(yn, xb).statistic))
    bref17 = percentile_linear(bd17, (0.05, 0.95))

    def f17_run(pre, searchlight=False, env=None):
        aa = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
              "-model_dset", "M2", "Mod2",
              "-model_dset", "M3", "Mod3",
              "-model_contrast", "M3-M2", "-metric", "spearman",
              "-nperm", "0", "-bootstrap", str(NB17), "-boot_ci", "90",
              "-seed", str(SD17), "-prefix", os.path.join(md, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc, out = rsa(aa, env=env)
        tf = os.path.join(md, pre + ".rsa.1D")
        rows = read_table(tf, "M2")[1] if rc == 0 and os.path.exists(tf) else []
        return rc, out, rows

    rcb17, ob17, ab17 = f17_run("f17_atlas", env=env1)
    bgot17 = (np.array([ab17[0]["M3-M2_bootLo"], ab17[0]["M3-M2_bootHi"]])
              if len(ab17) == 1 else np.array([np.nan, np.nan]))
    check("F17 model_dset contrast CI == paired subject-bootstrap reference",
          np.allclose(bgot17, bref17, atol=5e-4),
          "rc=%d 3dRSA=%s reference=%s %s" %
          (rcb17, bgot17, bref17, ob17.strip()[-100:]))

    rcs17, os17, ss17 = f17_run("f17_sl", True, env=envN)
    labs17 = head_brick_labs(os.path.join(md, "f17_sl+orig.HEAD")) if rcs17 == 0 else []
    bk17 = ("M3-M2_bootLo", "M3-M2_bootHi")
    check("F17 model_dset contrast bootstrap atlas == searchlight maps",
          len(ab17) == 1 and len(ss17) == NVm and
          all(all(x[k] == ab17[0][k] for k in bk17) for x in ss17) and
          all(k in labs17 for k in bk17),
          "atlas_rc=%d sl_rc=%d rows=%d labels=%s" %
          (rcb17, rcs17, len(ss17), labs17))

    _, _, s17t1 = f17_run("f17_t1", True, env=env1)
    _, _, s17tN = f17_run("f17_tN", True, env=envN)
    check("F17 contrast bootstrap thread-reproducible (1 vs %d)" % threads,
          len(s17t1) == len(s17tN) == NVm and
          all(all(x[k] == y[k] for k in bk17) for x, y in zip(s17t1, s17tN)))

    # The regression bootstrap must also consume model_dset RDMs rebuilt at the
    # current center.  Exercise joint + nuisance together; a whole-volume sphere
    # makes the atlas and every searchlight coefficient interval identical.
    def f17_reg_run(pre, searchlight=False, env=None):
        aa = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
              "-model_dset", "M2", "Mod2",
              "-model_dset", "M3", "Mod3", "-model_joint",
              "-ortvec", "Theta", "-metric", "spearman", "-nperm", "0",
              "-bootstrap", "101", "-boot_ci", "90", "-seed", "47",
              "-prefix", os.path.join(md, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc, out = rsa(aa, env=env)
        tf = os.path.join(md, pre + ".rsa.1D")
        rows = read_table(tf, "M2")[1] if rc == 0 and os.path.exists(tf) else []
        return rc, out, rows

    rra, ora, rra_rows = f17_reg_run("f17_reg_atlas", env=env1)
    rrs, ors, rrs_rows = f17_reg_run("f17_reg_sl", True, env=envN)
    rrlabs = head_brick_labs(os.path.join(md, "f17_reg_sl+orig.HEAD")) if rrs == 0 else []
    rrkeys = ("M2_bootLo", "M2_bootHi", "M3_bootLo", "M3_bootHi")
    check("F17 model_dset joint+nuisance bootstrap atlas == searchlight maps",
          len(rra_rows) == 1 and len(rrs_rows) == NVm and
          all(all(x[k] == rra_rows[0][k] for k in rrkeys) for x in rrs_rows) and
          all(k in rrlabs for k in rrkeys),
          "atlas_rc=%d sl_rc=%d rows=%d labels=%s %s" %
          (rra, rrs, len(rrs_rows), rrlabs, (ora + ors).strip()[-100:]))

    _, _, rrt1 = f17_reg_run("f17_reg_t1", True, env=env1)
    _, _, rrtN = f17_reg_run("f17_reg_tN", True, env=envN)
    check("F17 model_dset regression bootstrap thread-reproducible (1 vs %d)" % threads,
          len(rrt1) == len(rrtN) == NVm and
          all(all(x[k] == y[k] for k in rrkeys) for x, y in zip(rrt1, rrtN)))

    # Commonality keeps its three-part raw decomposition and appends two partial
    # R2 effects; every subject draw must recompute all five from one compact
    # neural/A/B sample.
    NBC, SDC = 201, 53
    mdc = []
    for ix in bootstrap_indices(NSm, NBC, SDC):
        yv, av, bv = [], [], []
        for aa in range(NSm):
            for bb in range(aa + 1, NSm):
                ia, ib = int(ix[aa]), int(ix[bb])
                if ia != ib:
                    yv.append(neu13[ia, ib])
                    av.append(m313[ia, ib])
                    bv.append(m213[ia, ib])
        if len(set(map(int, ix))) >= 3:
            mdc.append(compact_commonality(yv, av, bv))
    mdc = np.vstack(mdc)
    mdcref = np.vstack([percentile_linear(mdc[:, cc], (0.05, 0.95))
                        for cc in range(5)])

    def f17_ca_run(pre, searchlight=False, env=None):
        aa = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
              "-model_dset", "M3", "Mod3",
              "-model_dset", "M2", "Mod2",
              "-model_commonality", "M3,M2", "-metric", "spearman",
              "-nperm", "0", "-bootstrap", str(NBC), "-boot_ci", "90",
              "-seed", str(SDC), "-prefix", os.path.join(md, pre)]
        if searchlight:
            aa += ["-searchlight", "SPHERE(100)"]
        rc, out = rsa(aa, env=env)
        tf = os.path.join(md, pre + ".rsa.1D")
        rows = read_table(tf, "M3")[1] if rc == 0 and os.path.exists(tf) else []
        return rc, out, rows

    rcca, occa, carows = f17_ca_run("f17_ca_atlas", env=env1)
    canames = ("uniq_M3", "uniq_M2", "common_M3_M2",
               "partialR2_M3", "partialR2_M2")
    mcgot = (np.asarray([[carows[0][n + "_bootLo"], carows[0][n + "_bootHi"]]
                         for n in canames]) if len(carows) == 1
             else np.full((5, 2), np.nan))
    check("F17 model_dset commonality bootstrap matches compact reference",
          np.allclose(mcgot, mdcref, atol=5e-4),
          "rc=%d 3dRSA=%s reference=%s %s" %
          (rcca, mcgot, mdcref, occa.strip()[-100:]))

    rccs, occs, csrows = f17_ca_run("f17_ca_sl", True, env=envN)
    calabs = head_brick_labs(os.path.join(md, "f17_ca_sl+orig.HEAD")) if rccs == 0 else []
    cakeys = tuple(n + s for n in canames for s in ("_bootLo", "_bootHi"))
    caeffectlabs = tuple(k for n in canames for k in (n, n + "_FZ"))
    check("F17 commonality bootstrap atlas == searchlight maps",
          len(carows) == 1 and len(csrows) == NVm and
          all(all(x[k] == carows[0][k] for k in cakeys) for x in csrows) and
          all(k in calabs for k in cakeys + caeffectlabs),
          "atlas_rc=%d sl_rc=%d rows=%d labels=%s %s" %
          (rcca, rccs, len(csrows), calabs, occs.strip()[-100:]))

    _, _, cat1 = f17_ca_run("f17_ca_t1", True, env=env1)
    _, _, catN = f17_ca_run("f17_ca_tN", True, env=envN)
    check("F17 commonality bootstrap thread-reproducible (1 vs %d)" % threads,
          len(cat1) == len(catN) == NVm and
          all(all(x[k] == y[k] for k in cakeys) for x, y in zip(cat1, catN)))

    # -save_rdm writes one file for each fixed model, but a -model_dset changes
    # by ROI and therefore has no single fixed model file to plot.
    sb = os.path.join(md, "saved")
    ha = ["-dataTableFile", mtab, "-mask", mmk, "-mode", "IS-RSA",
          "-model_dset", "M2", "Mod2", "-nperm", "0",
          "-no_dset", "-save_rdm", sb, "-prefix", os.path.join(md, "hints")]
    rc, out = rsa(ha)
    phantom = sb + "_model_M2.1D"
    check("7 model_dset save_rdm hint omits nonexistent fixed model file",
          rc == 0 and not os.path.exists(phantom) and phantom not in out and
          "varies by ROI; no single model RDM file was written" in out and
          os.path.exists(sb + "_roi0001.1D"), "rc=%d" % rc)

    # =====================================================================
    # 44. Stage-5 repeated-run conditional regression.  Independently form
    # each run's neural/model triangles, z-score every design column, fit the
    # joint standardized coefficients and partial correlations, then combine
    # coefficients with the fixed happy-minus-sad weights.  A shuffled long
    # table and a different OpenMP count must be bit-identical.
    # =====================================================================
    s5j = os.path.join(work, "stage5_joint"); os.makedirs(s5j, exist_ok=True)
    NSj, NRj, NTj = 6, 4, 29
    runlab = ("mov1", "mov2", "mov3", "mov4")
    cond = ("happy", "happy", "sad", "sad")
    group = np.array(["A", "A", "A", "B", "B", "B"])
    happiness = np.array([[1.0, 2.2, -0.7, 0.1],
                          [2.1, 1.3,  0.2, 1.0],
                          [3.2, 2.5,  1.2, 1.8],
                          [2.7, 3.1,  0.8, 2.2],
                          [4.0, 2.8,  2.1, 1.4],
                          [3.5, 4.2,  1.7, 2.7]])
    jmask = os.path.join(s5j, "mask.nii.gz")
    nib.save(nib.Nifti1Image(np.ones((2, 2, 2), np.int16), np.eye(4)), jmask)
    local_rng = np.random.default_rng(4405)
    jfiles = {}
    for ru in range(NRj):
        b1, b2, b3 = (local_rng.normal(size=NTj) for _ in range(3))
        for ss in range(NSj):
            gv = 1.0 if group[ss] == "B" else -1.0
            hv = happiness[ss, ru]
            # Coefficient content deliberately changes with condition/run.
            ts = ((0.65 + 0.18 * (ru < 2)) * gv * b1 +
                  (0.42 - 0.07 * ru) * hv * b2 +
                  0.35 * (gv * hv) * b3 + 0.18 * local_rng.normal(size=NTj))
            fn = os.path.join(s5j, "s%02d_%s.nii.gz" % (ss, runlab[ru]))
            nib.save(nib.Nifti1Image(np.tile(ts, (8, 1)).reshape(2, 2, 2, NTj).astype(np.float32),
                                     np.eye(4)), fn)
            jfiles[ss, ru] = fn

    def write_jtable(path, order):
        with open(path, "w") as f:
            f.write("Subj Run Condition Group Happiness InputFile\n")
            for ss, ru in order:
                f.write("s%02d %s %s %s %.9g %s\n" %
                        (ss, runlab[ru], cond[ru], group[ss],
                         happiness[ss, ru], jfiles[ss, ru]))

    canonical = [(ss, ru) for ss in range(NSj) for ru in range(NRj)]
    shuffled = list(canonical); local_rng.shuffle(shuffled)
    jt1 = os.path.join(s5j, "table.txt"); jt2 = os.path.join(s5j, "table_shuffle.txt")
    write_jtable(jt1, canonical); write_jtable(jt2, shuffled)

    def nn_rdm(v):
        from scipy.stats import rankdata
        z = rankdata(np.asarray(v), method="average")
        d = np.abs(z[:, None] - z[None, :]); dm = d.max()
        return 1.0 - d / (dm if dm > 0 else 1.0)

    iu = np.triu_indices(NSj, 1)
    G = (group[:, None] == group[None, :]).astype(float)
    Xrun, Yrun, bref, pref = [], [], [], []
    for ru in range(NRj):
        T = np.vstack([np.asarray(nib.load(jfiles[ss, ru]).dataobj).ravel()
                       for ss in range(NSj)])
        Y = np.corrcoef(T)[iu]
        X = np.column_stack((G[iu], nn_rdm(happiness[:, ru])[iu]))
        yz = (Y - Y.mean()) / Y.std()
        Xz = (X - X.mean(0)) / X.std(0)
        b = np.linalg.lstsq(Xz, yz, rcond=None)[0]
        pr = []
        for mm0 in range(2):
            xo = Xz[:, 1 - mm0]
            ry = yz - xo * (np.dot(xo, yz) / np.dot(xo, xo))
            rx = Xz[:, mm0] - xo * (np.dot(xo, Xz[:, mm0]) / np.dot(xo, xo))
            pr.append(np.corrcoef(ry, rx)[0, 1])
        Xrun.append(Xz); Yrun.append(yz); bref.append(b); pref.append(pr)
    bref, pref = np.asarray(bref), np.asarray(pref)
    cw = np.array([0.5, 0.5, -0.5, -0.5])
    perms = list(itertools.permutations(range(NSj)))
    nullb = np.zeros((len(perms), NRj, 2))
    for ru in range(NRj):
        for mi in range(2):
            xo = Xrun[ru][:, [1 - mi]]
            fit0 = xo @ np.linalg.lstsq(xo, Yrun[ru], rcond=None)[0]
            resid = Yrun[ru] - fit0
            R = np.zeros((NSj, NSj)); R[iu] = resid; R[(iu[1], iu[0])] = resid
            for pi, perm in enumerate(perms):
                P = np.asarray(perm)
                yp = fit0 + R[np.ix_(P, P)][iu]
                nullb[pi, ru, mi] = np.linalg.lstsq(Xrun[ru], yp, rcond=None)[0][mi]
    def tied_abs(a, obs):
        av = np.abs(a).copy(); ao = np.abs(np.asarray(obs))
        tol = 64.0 * np.finfo(np.float32).eps * (1.0 + ao)
        return np.where(np.abs(av - ao) <= tol, ao, av)
    run_abs = tied_abs(nullb, bref[None, :, :])
    run_p_ref = np.mean(run_abs >= np.abs(bref[None, :, :]), axis=0)
    mean_null = nullb.mean(axis=1); mean_ref = bref.mean(axis=0)
    mean_abs = tied_abs(mean_null, mean_ref[None, :])
    mean_p_ref = np.mean(mean_abs >= np.abs(mean_ref[None, :]), axis=0)
    con_null = np.einsum("r,prm->pm", cw, nullb)
    con_ref = cw @ bref
    con_abs = tied_abs(con_null, con_ref[None, :])
    con_p_ref = np.mean(con_abs >= np.abs(con_ref[None, :]), axis=0)
    fam_null = np.maximum(np.max(run_abs, axis=1), con_abs)

    def s5j_run(pre, tabfile, env):
        aa = ["-dataTableFile", tabfile, "-mask", jmask, "-mode", "IS-RSA",
              "-run_column", "Run", "-run_analysis", "separate",
              "-model", "Group_match", "Group:match", "-run_model", "Happiness:NN",
              "-run_factor", "Condition",
              "-run_contrast", "HappyMinusSad=Condition:happy-sad",
              "-model_joint", "-metric", "pearson", "-nperm", "720",
              "-seed", "4405", "-prefix", os.path.join(s5j, pre)]
        rc0, out0 = rsa(aa, env=env)
        tf = os.path.join(s5j, pre + ".rsa.1D")
        rows0 = read_table(tf, "Group_match")[1] if rc0 == 0 and os.path.exists(tf) else []
        return rc0, out0, rows0

    jr1, jo1, jrows1 = s5j_run("joint_t1", jt1, env1)
    jrN, joN, jrowsN = s5j_run("joint_tN_shuffle", jt2, envN)
    bykey = {(r["model"], r["summary"]): r for r in jrows1}
    model_ix = {"Group_match": 0, "Happiness_run_nn": 1}
    numeric_ok = jr1 == 0 and len(bykey) == 2 * (NRj + 2)
    for mn, mi in model_ix.items():
        for ru, rl0 in enumerate(runlab):
            q = bykey.get((mn, rl0), {})
            numeric_ok &= (abs(q.get("beta", 99) - bref[ru, mi]) < 2e-5 and
                           abs(q.get("partial_r", 99) - pref[ru, mi]) < 2e-5 and
                           abs(q.get("p", 99) - run_p_ref[ru, mi]) < 6e-7 and
                           abs(q.get("pfwe", 99) -
                               np.mean(fam_null[:, mi] >= abs(bref[ru, mi]))) < 6e-7)
        qm = bykey.get((mn, "MEAN"), {})
        qc = bykey.get((mn, "HappyMinusSad"), {})
        numeric_ok &= (abs(qm.get("beta", 99) - bref[:, mi].mean()) < 2e-5 and
                       abs(qm.get("partial_r", 99) - pref[:, mi].mean()) < 2e-5 and
                       abs(qm.get("p", 99) - mean_p_ref[mi]) < 6e-7 and
                       abs(qm.get("pfwe", 99) - mean_p_ref[mi]) < 6e-7 and
                       abs(qc.get("beta", 99) - con_ref[mi]) < 2e-5 and
                       abs(qc.get("partial_r", 99) - np.dot(cw, pref[:, mi])) < 2e-5 and
                       abs(qc.get("p", 99) - con_p_ref[mi]) < 6e-7 and
                       abs(qc.get("pfwe", 99) -
                           np.mean(fam_null[:, mi] >= abs(con_ref[mi]))) < 6e-7)
    check("Stage 5 run-resolved joint beta/partial/contrast match NumPy",
          numeric_ok, "rc=%d rows=%d %s" % (jr1, len(jrows1), jo1.strip()[-180:]))
    jsort = lambda rows: sorted(rows, key=lambda x: (str(x["model"]), str(x["summary"])))
    check("Stage 5 shuffled rows and OpenMP count are exactly reproducible",
          jrN == 0 and jsort(jrows1) == jsort(jrowsN),
          "rc1=%d rcN=%d rows=%d/%d %s" %
          (jr1, jrN, len(jrows1), len(jrowsN), joN.strip()[-140:]))
    jheads = glob.glob(os.path.join(s5j, "joint_t1+*.HEAD"))
    jlabs = head_brick_labs(jheads[0]) if jr1 == 0 and jheads else []
    check("Stage 5 dataset labels distinguish coefficients and coefficient contrasts",
          "Group_match_MEAN_b" in jlabs and
          "Group_match_HappyMinusSad_bDiff" in jlabs and
          "Happiness_run_nn_mov1_b" in jlabs,
          "labels=%s" % jlabs)

    # =====================================================================
    # Mask-optional surface searchlight.  Only meaningful in a -DUSE_SUMA
    # build; probe for that first and SKIP (not fail) this block otherwise --
    # the rest of the runner still exercises the non-surface build fully.
    # A synthetic flat GIFTI mesh (a small triangulated grid, written with
    # nibabel) stands in for a real cortical surface: omitting -mask must give
    # EXACTLY the same result as an explicit all-nodes-in mask.
    # =====================================================================
    sfd = os.path.join(work, "sf"); os.makedirs(sfd, exist_ok=True)
    # a throwaway command that reaches the -surf/-DUSE_SUMA branch (real
    # dataTable + a model, so nothing earlier in validation short-circuits it)
    probe_rc, probe_out = rsa(["-dataTableFile", table, "-model", "behav_nn", "behav:nn",
                               "-nperm", "0", "-surf", os.devnull, "-searchlight", "5",
                               "-prefix", os.path.join(sfd, "probe")])
    if "compile with -DUSE_SUMA" in probe_out:
        print("SKIP: mask-optional surface searchlight (no -DUSE_SUMA in this binary)")
    else:
        Gs = 5; NNs = Gs * Gs; NSs, NTs = 10, 30
        xs, ys = np.meshgrid(np.arange(Gs), np.arange(Gs))
        coords = np.stack([xs.ravel(), ys.ravel(), np.zeros(NNs)], 1).astype(np.float32) * 3.0
        tris = []
        for j in range(Gs - 1):
            for i in range(Gs - 1):
                a0 = j * Gs + i; a1 = a0 + 1; b0 = a0 + Gs; b1 = b0 + 1
                tris += [[a0, a1, b1], [a0, b1, b0]]
        from nibabel.gifti import GiftiImage, GiftiDataArray
        nib.save(GiftiImage(darrays=[
            GiftiDataArray(coords, intent="NIFTI_INTENT_POINTSET", datatype="NIFTI_TYPE_FLOAT32"),
            GiftiDataArray(np.array(tris, np.int32), intent="NIFTI_INTENT_TRIANGLE",
                           datatype="NIFTI_TYPE_INT32")]), os.path.join(sfd, "mesh.gii"))
        beh = rng.normal(size=NSs)
        with open(os.path.join(sfd, "tab.txt"), "w") as f:
            f.write("Subj behav InputFile\n")
            for i in range(NSs):
                v = [GiftiDataArray(rng.normal(size=NNs).astype(np.float32),
                                    intent="NIFTI_INTENT_NONE", datatype="NIFTI_TYPE_FLOAT32")
                     for _ in range(NTs)]
                fn = os.path.join(sfd, "s%02d.gii" % i)
                nib.save(GiftiImage(darrays=v), fn)
                f.write("s%02d %.6f %s\n" % (i, beh[i], fn))
        allmask = GiftiDataArray(np.ones(NNs, np.int32), intent="NIFTI_INTENT_NONE",
                                 datatype="NIFTI_TYPE_INT32")
        nib.save(GiftiImage(darrays=[allmask]), os.path.join(sfd, "allmask.gii"))

        def sfrun(pre, use_mask):
            a = ["-dataTableFile", os.path.join(sfd, "tab.txt"), "-surf",
                 os.path.join(sfd, "mesh.gii"), "-searchlight", "4", "-mode", "IS-RSA",
                 "-model", "behav_nn", "behav:nn", "-metric", "spearman", "-nperm", "50", "-seed", "1",
                 "-no_dset", "-prefix", os.path.join(sfd, pre)]
            if use_mask: a = ["-mask", os.path.join(sfd, "allmask.gii")] + a
            return rsa(a)

        rc_n, out_n = sfrun("nomask", False)
        rc_a, out_a = sfrun("allmask", True)
        fn_n = os.path.join(sfd, "nomask.rsa.1D"); fn_a = os.path.join(sfd, "allmask.rsa.1D")
        ok = (rc_n == 0 and rc_a == 0 and os.path.exists(fn_n) and os.path.exists(fn_a))
        check("surf mask-optional searchlight runs (no -mask)", ok, out_n.strip()[-200:])
        if ok:
            with open(fn_n) as f1, open(fn_a) as f2:
                rows_n = [l for l in f1 if not l.startswith("#") and l.strip()]
                rows_a = [l for l in f2 if not l.startswith("#") and l.strip()]
            check("surf: 25-node whole-mesh search (no -mask)", len(rows_n) == NNs,
                  "got %d rows" % len(rows_n))
            check("surf: no -mask == explicit all-ones -mask (exact)",
                  rows_n == rows_a, "first differing row shown on -v")


# ============================================================================
# main
# ============================================================================
def main():
    default_bin = os.environ.get("RSA_BIN") or shutil.which("3dRSA") or "3dRSA"
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bin", default=default_bin,
                    help="path to 3dRSA (default: RSA_BIN, then PATH)")
    ap.add_argument("--threads", type=int, default=6,
                    help="thread count for the multi-thread reproducibility check")
    ap.add_argument("--work", default=None, help="work dir (default: a temp dir)")
    ap.add_argument("--require-deps", action="store_true",
                    help="fail instead of skip if numpy/scipy/nibabel are unavailable")
    ap.add_argument("-v", "--verbose", action="store_true")
    a = ap.parse_args()

    try:
        import numpy  # noqa: F401
        import scipy  # noqa: F401
        import nibabel  # noqa: F401
    except Exception as e:
        tag = "FAIL" if a.require_deps else "SKIP"
        print("%s: numeric runner needs numpy + scipy + nibabel (%s)" % (tag, e))
        return 2 if a.require_deps else 0

    if not os.path.exists(a.bin):
        print("FAIL: no binary at %s (pass --bin or set RSA_BIN)" % a.bin); return 2

    work = a.work or tempfile.mkdtemp(prefix="rsa_numeric_")
    os.makedirs(work, exist_ok=True)
    print("binary : %s" % a.bin)
    print("workdir: %s\n" % work)

    try:
        run_checks(a.bin, work, a.threads, a.verbose)
    except Skip as e:
        print("SKIP: %s" % e); return 0
    except Exception as e:
        import traceback; traceback.print_exc()
        check("runner completed", False, repr(e))

    npass = sum(1 for _, ok, _ in RESULTS if ok)
    nfail = len(RESULTS) - npass
    print("results:")
    for name, ok, detail in RESULTS:
        tag = "PASS" if ok else "FAIL"
        line = "  [%s] %s" % (tag, name)
        if detail and (not ok or a.verbose):
            line += "  -- %s" % detail
        print(line)
    print("\n%d passed, %d failed" % (npass, nfail))

    if a.work is None:
        shutil.rmtree(work, ignore_errors=True)
    return 1 if nfail else 0


if __name__ == "__main__":
    sys.exit(main())
