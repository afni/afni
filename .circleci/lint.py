#!/usr/bin/env python3
"""Cheap incremental checks, plus a portable change manifest for Docker jobs."""
import ast
import json
from pathlib import Path
import subprocess
import sys

# Bundled implementations have their own maintenance and warning policies.
THIRD_PARTY = (
    "src/3DEdge/",
    "src/f2c/",
    "src/jpeg-6b/",
    "src/XmHTML/",
    "src/nifti/",
    "src/gifti/",
    "src/qhulldir/",
    "src/mpeg_encodedir/",
    "src/mpegtoppm_dir/",
    "src/SUMA/gts/",
    "src/SUMA/GLUT/",
    "src/SUMA/GLw_local/",
    "src/crorden/",
)


def first_party(path):
    return not path.startswith(THIRD_PARTY)


def git_paths(*args):
    return (
        subprocess.check_output(["git", *args, "-z"]).decode().strip("\0").split("\0")
    )


def write_changes(base, destination):
    """Record tracked inputs and changed/added paths without inspecting build output."""
    tracked = [p for p in git_paths("ls-files") if p]
    if base:
        files = git_paths("diff", "--name-only", "--diff-filter=ACMR", base, "HEAD")
        added = git_paths("diff", "--name-only", "--diff-filter=A", base, "HEAD")
    else:
        files, added = tracked, []
    result = {
        "base": base,
        "files": [p for p in files if p],
        "added": [p for p in added if p],
        "tracked": tracked,
    }
    Path(destination).write_text(json.dumps(result, indent=2) + "\n")


def check_files(changes):
    """Gate syntax/whitespace regressions; retain static-analysis findings as reports."""
    import yaml
    import black

    reports = Path("/tmp/afni-lint")
    reports.mkdir(exist_ok=True)
    base = changes["base"]
    if base:
        subprocess.run(["git", "diff", "--check", base, "HEAD"], check=True)
    with open(".circleci/config.yml") as stream:
        config = yaml.safe_load(stream)
    assert config["version"] == 2.1 and "workflows" in config
    files = [p for p in changes["files"] if Path(p).is_file() and first_party(p)]
    for name in files:
        path = Path(name)
        if path.suffix == ".py":
            try:
                ast.parse(path.read_bytes(), filename=name)
            except SyntaxError:
                # A touched Python-2-only utility is not a new Python-3 regression.
                if not base:
                    raise
                old = subprocess.run(
                    ["git", "show", f"{base}:{name}"], capture_output=True
                )
                try:
                    ast.parse(old.stdout, filename=name)
                except SyntaxError:
                    print(f"Legacy Python syntax (advisory): {name}")
                else:
                    raise
            if name.startswith((".circleci/", "tests/")):
                text = path.read_text()
                formatted = black.format_str(text, mode=black.Mode())
                if formatted != text:
                    old = (
                        subprocess.run(
                            ["git", "show", f"{base}:{name}"],
                            capture_output=True,
                            text=True,
                        )
                        if base
                        else None
                    )
                    if (
                        old
                        and old.returncode == 0
                        and black.format_str(old.stdout, mode=black.Mode())
                        != old.stdout
                    ):
                        print(f"Existing formatting drift (advisory): {name}")
                    else:
                        raise ValueError(f"Run black==24.10.0 on {name}")
        else:
            # Some shell helpers, including fix-permissions, have no suffix.
            with path.open("rb") as stream:
                first_line = (
                    stream.readline(256).decode("ascii", errors="ignore").strip()
                )
            if first_line.startswith("#!") and (
                "bash" in first_line or first_line.endswith("/sh")
            ):
                subprocess.run(["bash", "-n", name], check=True)
    # Only added text is checked for spelling. Existing legacy prose must not
    # suddenly become a blocker when a nearby numerical line changes.
    if base:
        diff = (
            subprocess.check_output(
                ["git", "diff", "--unified=0", base, "HEAD", "--", *files]
            )
            if files
            else b""
        )
        added_text = "\n".join(
            line[1:]
            for line in diff.decode(errors="replace").splitlines()
            if line.startswith("+") and not line.startswith("+++")
        )
        subprocess.run(
            [sys.executable, "-m", "codespell_lib", "-"],
            input=added_text,
            text=True,
            check=True,
        )
    sources = [p for p in files if Path(p).suffix in (".c", ".h")]
    if sources:
        # Diagnostic-only until a pinned baseline exists. A package update must
        # not change whether an otherwise valid scientific PR can merge.
        with (reports / "cppcheck.txt").open("w") as stream:
            subprocess.run(
                ["cppcheck", "--version"], stdout=stream, stderr=subprocess.STDOUT
            )
            try:
                subprocess.run(
                    [
                        "cppcheck",
                        "--language=c",
                        "--enable=warning,performance,portability",
                        "--max-configs=8",
                        "--inline-suppr",
                        *sources,
                    ],
                    stdout=stream,
                    stderr=subprocess.STDOUT,
                    timeout=300,
                )
            except subprocess.TimeoutExpired:
                stream.write("\nAdvisory analysis stopped after five minutes.\n")


if __name__ == "__main__":
    if sys.argv[1] == "--write-changes":
        write_changes(sys.argv[2], sys.argv[3])
    else:
        check_files(json.loads(Path(sys.argv[1]).read_text()))
