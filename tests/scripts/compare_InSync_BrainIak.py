#!/usr/bin/env python3
"""Compare 3dInSync voxelwise ISC against BrainIAK on fMRIPrep-style data.

Pipeline:
  1. Build a common mask (voxels with nonzero temporal SD in every subject).
  2. Run 3dInSync for each ISC method / summary combination.
  3. Run brainiak.isc.isc on the same masked time series.
  4. Report voxelwise agreement (max/mean abs difference, Pearson r).
  5. Unless --skip-groups: split the subjects into two alternating groups and
     compare the group difference (exactly), then permutation p-values and
     bootstrap intervals (statistically, since the random streams differ), on
     a random subset of mask voxels.

Run with the BrainIAK environment's Python, with AFNI programs on PATH:

  /Users/molfesepj/micromamba/envs/isccomp/bin/python compare_InSync_BrainIak.py \
      --fmriprep-dir /path/to/derivatives/fmriprep --run 1 --workdir ./insync_vs_brainiak
"""

import argparse
import subprocess
import time
from pathlib import Path

import nibabel as nib
import numpy as np
from brainiak.isc import bootstrap_isc, isc, permutation_isc

# (3dInSync -isc_method, 3dInSync -summary, BrainIAK pairwise, BrainIAK summary)
COMBOS = [
    ("pairwise", "median", True, "median"),
    ("pairwise", "fisher_mean", True, "mean"),
    ("loo", "median", False, "median"),
    ("loo", "fisher_mean", False, "mean"),
]


def find_inputs(fmriprep_dir: Path, run: int) -> dict:
    files = {}
    for subdir in sorted(fmriprep_dir.glob("sub-*")):
        matches = sorted(subdir.glob(f"func/*_run-{run}_*bold.nii*"))
        if len(matches) != 1:
            raise RuntimeError(f"Expected one run-{run} file for {subdir.name}; found {matches}")
        files[subdir.name] = matches[0]
    if len(files) < 3:
        raise RuntimeError(f"Need at least 3 subjects under {fmriprep_dir}")
    return files


def load_ts(path: Path) -> np.ndarray:
    return np.asarray(nib.load(str(path)).dataobj, dtype=np.float32)


def build_mask(files: dict, mask_path: Path) -> np.ndarray:
    """Voxels with nonzero temporal SD in every subject."""
    mask = None
    ref_img = None
    for path in files.values():
        img = nib.load(str(path))
        ref_img = ref_img or img
        sd = np.asarray(img.dataobj, dtype=np.float32).std(axis=-1)
        good = sd > 1e-3
        mask = good if mask is None else (mask & good)
    nib.save(nib.Nifti1Image(mask.astype(np.uint8), ref_img.affine), str(mask_path))
    return mask


def write_table(files: dict, table: Path) -> None:
    lines = ["Subj InputFile"] + [f"{s} {p}" for s, p in files.items()]
    table.write_text("\n".join(lines) + "\n")


def run_insync(table: Path, mask_path: Path, prefix: Path, method: str, summary: str) -> float:
    cmd = [
        "3dInSync", "-quiet", "-overwrite",
        "-prefix", str(prefix), "-mask", str(mask_path),
        "-isc_method", method, "-summary", summary,
        "-dataTableFile", str(table),
    ]
    t0 = time.time()
    subprocess.run(cmd, check=True)
    return time.time() - t0


def read_insync(prefix: Path, mask: np.ndarray) -> np.ndarray:
    """Return the first sub-brick (ISC) inside the mask."""
    out = next(prefix.parent.glob(prefix.name + ".nii*"), None)
    if out is None:
        out = next(prefix.parent.glob(prefix.name + "+*.HEAD"))
    data = np.asarray(nib.load(str(out)).dataobj, dtype=np.float64)
    if data.ndim == 3:
        data = data[..., None]
    return data[..., 0][mask]


def compare(name: str, a: np.ndarray, b: np.ndarray) -> None:
    ok = np.isfinite(a) & np.isfinite(b)
    diff = np.abs(a[ok] - b[ok])
    r = np.corrcoef(a[ok], b[ok])[0, 1]
    print(f"{name:28s} nvox={ok.sum():7d}  r={r:.8f}  "
          f"mean|d|={diff.mean():.3e}  max|d|={diff.max():.3e}")


def group_labels(n: int) -> list:
    return ["A" if i % 2 == 0 else "B" for i in range(n)]


def read_brick(prefix: Path, label: str, mask: np.ndarray) -> np.ndarray:
    out = next(prefix.parent.glob(prefix.name + "+*.HEAD"), None) or next(prefix.parent.glob(prefix.name + ".nii*"))
    labels = subprocess.run(["3dinfo", "-label", str(out)], check=True, capture_output=True,
                            text=True).stdout.strip().split("|")
    data = np.asarray(nib.load(str(out)).dataobj, dtype=np.float64)
    return data[..., labels.index(label)][mask]


def compare_p(name: str, a: np.ndarray, b: np.ndarray, nperm: int) -> None:
    """Compare Monte Carlo p-values; each has SE ~ sqrt(p(1-p)/nperm)."""
    pm = np.clip((a + b) / 2, 1e-12, 1)
    se = np.sqrt(2 * pm * (1 - pm) / nperm)
    z = np.abs(a - b) / np.maximum(se, 1e-12)
    r = np.corrcoef(a, b)[0, 1]
    print(f"{name:28s} r={r:.5f} mean|d|={np.abs(a - b).mean():.4f} mean(afni)={a.mean():.4f} "
          f"mean(brainiak)={b.mean():.4f} frac(|d|>3.5SE)={np.mean(z > 3.5):.4f}")


def group_inference(files: dict, full_mask: np.ndarray, mask_path: Path, workdir: Path,
                    nvox: int, nperm: int, nboot: int, seed: int) -> None:
    rng = np.random.RandomState(seed)
    idx = np.flatnonzero(full_mask.ravel())
    sel = rng.choice(idx, min(nvox, idx.size), replace=False)
    sub = np.zeros(full_mask.size, np.uint8)
    sub[sel] = 1
    sub = sub.reshape(full_mask.shape)
    sub_path = workdir / "mask_subset.nii.gz"
    nib.save(nib.Nifti1Image(sub, nib.load(str(mask_path)).affine), str(sub_path))
    smask = sub > 0

    grp = group_labels(len(files))
    table = workdir / "table_groups.txt"
    table.write_text("Subj Group InputFile\n" + "".join(
        f"{s} {g} {p}\n" for (s, p), g in zip(files.items(), grp)))
    data = np.stack([load_ts(p)[smask].T for p in files.values()], axis=-1)
    iscs = isc(data, pairwise=True, summary_statistic=None)
    print(f"\n== group inference: {sum(g == 'A' for g in grp)} vs {sum(g == 'B' for g in grp)} subjects, "
          f"{int(smask.sum())} voxels, nperm={nperm}, nboot={nboot}")

    for tail, side in (("1sided", "right"), ("2sided", "two-sided")):
        prefix = workdir / f"InSync_groups_{tail}"
        cmd = ["3dInSync", "-quiet", "-overwrite", "-prefix", str(prefix), "-mask", str(sub_path),
               "-isc_method", "pairwise", "-summary", "median",
               "-nperm", str(nperm), "-nboot", str(nboot), "-seed", str(seed),
               f"-{tail}", "-dataTableFile", str(table)]
        t0 = time.time()
        subprocess.run(cmd, check=True)
        t_afni = time.time() - t0
        t0 = time.time()
        obs, p, _ = permutation_isc(iscs, group_assignment=grp, pairwise=True,
                                    summary_statistic="median", n_permutations=nperm,
                                    side=side, random_state=seed)
        print(f"[{tail}] 3dInSync {t_afni:.1f}s, BrainIAK permutation {time.time() - t0:.1f}s")
        compare("group difference (A-B)", read_brick(prefix, "ISC_A-B", smask), np.ravel(obs))
        compare_p(f"permutation P ({tail})", read_brick(prefix, "P_A-B", smask), np.ravel(p), nperm)

    # Bootstrap intervals for group A and B (95%, matching -boot_alpha 0.05)
    for g in ("A", "B"):
        sel_g = [i for i, x in enumerate(grp) if x == g]
        iscs_g = isc(data[..., sel_g], pairwise=True, summary_statistic=None)
        obs_g, ci, _, _ = bootstrap_isc(iscs_g, pairwise=True, summary_statistic="median",
                                     n_bootstraps=nboot, ci_percentile=95, random_state=seed)
        lo, hi = read_brick(prefix, f"BootLo_{g}", smask), read_brick(prefix, f"BootHi_{g}", smask)
        width = (ci[1] - ci[0]).mean()
        print(f"bootstrap CI group {g}: mean width BrainIAK={width:.4f} 3dInSync={(hi - lo).mean():.4f}")
        compare(f"BootLo_{g} vs BrainIAK lower", lo, ci[0])
        compare(f"BootHi_{g} vs BrainIAK upper", hi, ci[1])


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--fmriprep-dir", type=Path, required=True)
    ap.add_argument("--run", type=int, default=1)
    ap.add_argument("--workdir", type=Path, default=Path("insync_vs_brainiak"))
    ap.add_argument("--skip-groups", action="store_true", help="skip the two-group permutation/bootstrap comparison")
    ap.add_argument("--skip-basic", action="store_true", help="skip the whole-mask ISC comparison")
    ap.add_argument("--nvox", type=int, default=5000, help="random mask voxels for group inference")
    ap.add_argument("--nperm", type=int, default=10000)
    ap.add_argument("--nboot", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=1234)
    args = ap.parse_args()

    args.workdir.mkdir(parents=True, exist_ok=True)
    files = find_inputs(args.fmriprep_dir, args.run)
    print(f"{len(files)} subjects, run-{args.run}")

    mask_path = args.workdir / "mask.nii.gz"
    mask = build_mask(files, mask_path)
    print(f"mask voxels: {int(mask.sum())}")

    table = args.workdir / "table.txt"
    write_table(files, table)

    if not args.skip_basic:
        # BrainIAK input: (n_TRs, n_voxels, n_subjects)
        data = np.stack([load_ts(p)[mask].T for p in files.values()], axis=-1)
        print(f"BrainIAK input shape: {data.shape}")

        for method, summary, pairwise, bsummary in COMBOS:
            label = f"{method}_{summary}"
            prefix = args.workdir / f"InSync_{label}"
            t_afni = run_insync(table, mask_path, prefix, method, summary)
            t0 = time.time()
            ref = isc(data, pairwise=pairwise, summary_statistic=bsummary, tolerate_nans=False)
            t_brain = time.time() - t0
            print(f"[{label}] 3dInSync {t_afni:.1f}s, BrainIAK {t_brain:.1f}s")
            compare(label, read_insync(prefix, mask), np.asarray(ref).ravel())

    if not args.skip_groups:
        group_inference(files, mask, mask_path, args.workdir, args.nvox, args.nperm,
                        args.nboot, args.seed)


if __name__ == "__main__":
    main()
