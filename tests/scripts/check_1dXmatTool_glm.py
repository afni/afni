#!/usr/bin/env python3
"""Check GLM table export against actual Deconvolve and REMLfit coefficients.

All fits and negative-case datasets are temporary. Requires numpy/nibabel and
an AFNI bin directory; --tool selects the exact 1dXmatTool build under test.
"""
import argparse
import os
import re
from pathlib import Path
import subprocess
import tempfile

import nibabel as nib
import numpy as np
from check_1dXmatTool_betas import read_table


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tool", required=True, type=Path)
    parser.add_argument("--afni-bin", required=True, type=Path)
    parser.add_argument("--mvpa", type=Path)
    args = parser.parse_args()
    tool, bindir = args.tool.resolve(), args.afni_bin.resolve()
    env = dict(os.environ, AFNI_VERSION_CHECK="NO", OMP_NUM_THREADS="1")
    with tempfile.TemporaryDirectory(prefix="xmat-glm-") as directory:
        work = Path(directory)

        def run(prog, *opts, success=True, contains=None):
            result = subprocess.run(
                [str(prog), *map(str, opts)],
                cwd=work,
                env=env,
                text=True,
                capture_output=True,
            )
            assert (result.returncode == 0) == success, (
                opts,
                result.stdout,
                result.stderr,
            )
            if contains:
                assert contains in result.stdout + result.stderr, result.stderr
            return result

        def export(prefix, beta="ols.nii.gz", design="X.xmat.1D", extra=(), **kw):
            existed = (work / (prefix + ".trials.1D")).exists()
            result = run(
                tool,
                "-infile",
                design,
                "-beta",
                beta,
                "-beta_layout",
                "glm",
                "-export_manifest",
                "-subject",
                "s01",
                "-prefix",
                prefix,
                *extra,
                **kw,
            )
            if kw.get("success") is False and not existed:
                assert not (work / (prefix + ".trials.1D")).exists()
            return result

        rng = np.random.default_rng(741)
        # Random data suffice: correspondence is checked against the actual
        # fitted bucket values, independently of the new exporter's indices.
        image = nib.Nifti1Image(
            rng.normal(100, 2, (2, 2, 2, 160)).astype("float32"), np.eye(4)
        )
        image.header.set_zooms((1, 1, 1, 2))
        nib.save(image, work / "data.nii.gz")
        (work / "face.1D").write_text("10 50\n10 50\n")
        (work / "house.1D").write_text("30 70\n30 70\n")
        run(
            bindir / "3dDeconvolve",
            "-input",
            "data.nii.gz",
            "-concat",
            "1D: 0 80",
            "-polort",
            "1",
            "-local_times",
            "-num_stimts",
            "2",
            "-stim_times_IM",
            "1",
            "face.1D",
            "GAM",
            "-stim_label",
            "1",
            "face",
            "-stim_times_IM",
            "2",
            "house.1D",
            "GAM",
            "-stim_label",
            "2",
            "house",
            "-x1D",
            "X.xmat.1D",
            "-cbucket",
            "ols.nii.gz",
            "-bucket",
            "stats.nii.gz",
            "-tout",
        )
        run(
            bindir / "3dREMLfit",
            "-input",
            "data.nii.gz",
            "-matrix",
            "X.xmat.1D",
            "-Rbeta",
            "reml.nii.gz",
            "-nobout",
        )
        # Four baseline columns precede the IM columns; -nobout removes them.
        expected_runs = [1, 1, 2, 2, 1, 1, 2, 2]
        for beta, offset, prefix in [
            ("ols.nii.gz", 4, "ols"),
            ("reml.nii.gz", 0, "reml"),
        ]:
            export(prefix, beta=beta, extra=("-select_stimuli", "face,house"))
            rows = read_table(work / (prefix + ".trials.1D"))
            assert [int(r["Volume"]) for r in rows] == list(range(offset, offset + 8))
            assert [int(r["XColumn"]) for r in rows] == list(range(4, 12))
            assert [int(r["Trial"]) for r in rows] == list(range(4, 12))
            assert [int(r["Run"]) for r in rows] == expected_runs
            assert [r["Condition"] for r in rows] == ["face"] * 4 + ["house"] * 4
            assert all(Path(r["InputFile"]).is_absolute() for r in rows)
        if args.mvpa:
            nib.save(
                nib.Nifti1Image(np.ones((2, 2, 2), dtype="uint8"), np.eye(4)),
                work / "mask.nii.gz",
            )
            run(
                args.mvpa.resolve(),
                "-mode",
                "classify",
                "-trial_manifest",
                "ols.trials.1D",
                "-target_column",
                "Condition",
                "-cv_column",
                "Run",
                "-mask",
                "mask.nii.gz",
                "-validate_only",
            )
        run(bindir / "3dbucket", "-prefix", "reverse.nii.gz", "ols.nii.gz[11..0]")
        export("reverse", beta="reverse.nii.gz")
        reverse = read_table(work / "reverse.trials.1D")
        assert [int(r["Volume"]) for r in reverse] == list(range(7, -1, -1))
        original_data = nib.load(work / "ols.nii.gz").get_fdata()
        reversed_data = nib.load(work / "reverse.nii.gz").get_fdata()
        for row in reverse:
            np.testing.assert_array_equal(
                reversed_data[..., int(row["Volume"])],
                original_data[..., int(row["XColumn"])],
            )
        export(
            "selected", extra=("-select_columns", "4,6", "-manifest_format", "selected")
        )
        selected = read_table(work / "selected.trials.1D")
        assert [r["SourceColumn"] for r in selected] == ["4", "6"]
        assert selected[0]["InputFile"].endswith("ols.nii.gz[4]")
        export("rsa", extra=("-manifest_format", "rsa_runwise"))
        assert (work / "rsa.runwise.1D").exists()
        export("ols", success=False, contains="already exists")
        # Existing output remains intact after a no-overwrite failure.
        assert len(read_table(work / "ols.trials.1D")) == 8
        export(
            "baseline",
            extra=("-select_columns", "0"),
            success=False,
            contains="baseline/nuisance",
        )
        export(
            "stats",
            beta="stats.nii.gz",
            success=False,
            contains="missing coefficient label",
        )
        run(bindir / "3dbucket", "-prefix", "duplicate.nii.gz", "ols.nii.gz[4,4]")
        export(
            "duplicate",
            beta="duplicate.nii.gz",
            extra=("-select_columns", "4"),
            success=False,
            contains="duplicate beta label",
        )
        export(
            "missing",
            beta="duplicate.nii.gz",
            extra=("-select_columns", "5"),
            success=False,
            contains="missing coefficient label",
        )
        annotations = "Volume Condition Run Trial Basis Item\n" + "".join(
            f'{i+4} {"face" if i<4 else "house"} {expected_runs[i]} {100+i} 0 item{i}\n'
            for i in range(8)
        )
        (work / "events.1D").write_text(annotations)
        export("annotated", extra=("-events", "events.1D"))
        (work / "conditions.1D").write_text(
            "Volume Condition\n"
            + "".join(f'{i+4} {"face" if i<4 else "house"}\n' for i in range(8))
        )
        export("conditions", extra=("-events", "conditions.1D"))
        assert [
            int(r["Trial"]) for r in read_table(work / "conditions.trials.1D")
        ] == list(range(4, 12))
        assert [
            int(r["Trial"]) for r in read_table(work / "annotated.trials.1D")
        ] == list(range(100, 108))
        (work / "events.1D").write_text(annotations.replace("4 face 1", "4 face 2"))
        export(
            "contradiction",
            extra=("-events", "events.1D"),
            success=False,
            contains="contradicts",
        )
        # Censoring cannot silently change run ownership. A full reference is
        # mandatory; modifying retained values makes the reference invalid.
        text = (work / "X.xmat.1D").read_text()
        lines = text.splitlines()
        data_indices = [
            i
            for i, line in enumerate(lines)
            if line.strip() and not line.startswith("#")
        ]
        censored = [line for i, line in enumerate(lines) if i != data_indices[0]]
        censored = "\n".join(censored) + "\n"
        censored = re.sub(r'ni_dimen\s*=\s*"160"', 'ni_dimen = "159"', censored)
        censored = re.sub(r'GoodList\s*=\s*"[^"]*"', 'GoodList = "1..159"', censored)
        (work / "censored.1D").write_text(censored)
        export(
            "censored-no-ref",
            design="censored.1D",
            success=False,
            contains="requires -uncensored",
        )
        export("censored", design="censored.1D", extra=("-uncensored", "X.xmat.1D"))
        # A synthetic cross-run column must fail regardless of annotations.
        values = np.loadtxt(work / "X.xmat.1D")
        values[100, 4] = 1
        header = "\n".join(
            line for line in lines if line.startswith("#") and "</matrix>" not in line
        )
        (work / "cross.1D").write_text(
            header
            + "\n"
            + "\n".join(" ".join(map(str, r)) for r in values)
            + "\n# </matrix>\n"
        )
        export("cross", design="cross.1D", success=False, contains="spans runs")
        values[:, 4] = 0
        (work / "zero.1D").write_text(
            header
            + "\n"
            + "\n".join(" ".join(map(str, r)) for r in values)
            + "\n# </matrix>\n"
        )
        export("zero", design="zero.1D", success=False, contains="no retained support")
        export(
            "mismatched",
            design="censored.1D",
            extra=("-uncensored", "zero.1D"),
            success=False,
            contains="reference differs",
        )
        missing_timeline = (
            "\n".join(line for line in lines if "RunStart" not in line) + "\n"
        )
        (work / "no-runs.1D").write_text(missing_timeline)
        export(
            "no-runs", design="no-runs.1D", success=False, contains="explicit RunStart"
        )
        # Unknown/multi-basis formulas need annotations, never guessed trial IDs.
        (work / "basis.1D").write_text(text.replace("GAM", "TENT(0,12,3)"))
        export(
            "basis", design="basis.1D", success=False, contains="explicit Trial/Basis"
        )
        (work / "events.1D").write_text(annotations)
        export("basis-annotated", design="basis.1D", extra=("-events", "events.1D"))
        (work / "events.1D").write_text(
            annotations.replace("101 0 item1", "100 1 item0")
        )
        export(
            "basis-duplicate",
            design="basis.1D",
            extra=("-events", "events.1D"),
            success=False,
            contains="one basis coefficient",
        )
        print(
            "GLM export checks passed (Deconvolve, REMLfit, mapping and rejection cases)."
        )


if __name__ == "__main__":
    main()
