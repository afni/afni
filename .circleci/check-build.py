#!/usr/bin/env python3
"""Inspect real build inventories; gate new source omissions and C diagnostics.

Existing Make/CMake target drift is reported, not assumed to be accidental.
Explicit exclusions for new first-party sources require a reason in parity-exceptions.json.
"""
import argparse
import json
from pathlib import Path
import re
import shlex
import subprocess
import tarfile
import tempfile

from lint import first_party


C_ERRORS = [
    "-Werror=implicit-function-declaration",
    "-Werror=incompatible-pointer-types",
    "-Werror=return-type",
    "-Wformat",
    "-Werror=format-security",
]


def load_changes(reports):
    path = reports / "changes.json"
    return (
        json.loads(path.read_text())
        if path.exists()
        else {"files": [], "added": [], "tracked": []}
    )


def archive_check(source, archive, changes, reports, exceptions):
    """Compare the actual release archive with tracked C/header inputs; gate additions."""
    with tarfile.open(archive) as stream:
        included = {
            str(Path(member.name).relative_to("afni_src"))
            for member in stream.getmembers()
            if member.isfile() or member.issym() or member.islnk()
        }
    relevant = {
        p
        for p in changes["tracked"]
        if p.startswith("src/")
        and Path(p).suffix in (".c", ".h", ".f", ".inc")
        and first_party(p)
    }
    missing = sorted(p for p in relevant if p[4:] not in included)
    (reports / "archive-missing.json").write_text(json.dumps(missing, indent=2) + "\n")
    failures = set(missing) & set(changes["added"]) - set(exceptions)
    if failures:
        raise RuntimeError(
            "New sources missing from afni_src.tgz: " + ", ".join(sorted(failures))
        )


def make_inventory(source):
    """Evaluate a no-dependency inventory target in a disposable source directory.

    This does not invoke all/install or depend on the developer's local Makefile.
    The caller is a CI container; Makefile expansion can still execute $(shell).
    """
    targets, inputs = set(), set()
    # The parent program list excludes the separate ptaylor executable list.
    inventories = (
        (
            "",
            "Makefile.linux_ubuntu_24_64",
            "$(PROGRAMS) $(SUMA_PROGS) $(SUMA_GTS_PROGS)",
        ),
        ("ptaylor", "Makefile.ptaylor.INCLUDE", "$(FC_PROGRAMS)"),
    )
    for prefix, makefile, programs in inventories:
        with tempfile.NamedTemporaryFile(mode="w", suffix=".mk") as stream:
            stream.write(
                ".PHONY: afni_ci_inventory\nafni_ci_inventory:\n"
                f"\t@echo {programs}\n"
            )
            stream.flush()
            command = [
                "make",
                "--no-print-directory",
                "-f",
                makefile,
                "-f",
                stream.name,
            ]
            directory = source / "src" / prefix
            targets.update(
                subprocess.check_output(
                    [*command, "-s", "afni_ci_inventory"], cwd=directory, text=True
                ).split()
            )
            database = subprocess.check_output(
                [*command, "-np", "afni_ci_inventory"], cwd=directory, text=True
            )
        # These are evaluated prerequisites, not raw Makefile variable definitions.
        for line in database.splitlines():
            if line.startswith(("\t", "#", " ")) or ":" not in line or "=" in line:
                continue
            for token in line.split(":", 1)[1].split():
                if token.endswith((".o", ".c")):
                    inputs.add(str(Path(prefix) / token))
    return targets, inputs


def cmake_inventory(build):
    """Read executable names and compiled sources from CMake's generated codemodel."""
    reply = build / ".cmake/api/v1/reply"
    index = json.loads(sorted(reply.glob("index-*.json"))[-1].read_text())
    model = json.loads((reply / index["reply"]["codemodel-v2"]["jsonFile"]).read_text())
    targets = {}
    for ref in model["configurations"][0]["targets"]:
        target = json.loads((reply / ref["jsonFile"]).read_text())
        if target["type"] == "EXECUTABLE" and "install" in target:
            targets[target.get("nameOnDisk", target["name"])] = [
                item["path"] for item in target.get("sources", [])
            ]
    return targets


def syntax_command(entry):
    """Remove output/dependency-generation flags while preserving per-file flags."""
    args = entry.get("arguments") or shlex.split(entry["command"])
    result = []
    skip = False
    for arg in args:
        if skip:
            skip = False
        elif arg in ("-o", "-MF", "-MT", "-MQ"):
            skip = True
        elif arg in ("-c", "-MD", "-MMD", "-MP"):
            continue
        else:
            result.append(arg)
    return [*result, "-fsyntax-only", *C_ERRORS]


def target_drift(inventory, baseline):
    """Report target differences not observed in the reviewed CI configuration."""
    return [
        f"New target drift ({category}): {name}"
        for category, names in inventory.items()
        for name in sorted(set(names) - set(baseline[category]))
    ]


def cmake_check(source, build, changes, reports, exceptions):
    """Gate new build omissions and check changed C units with their actual flags."""
    targets = cmake_inventory(build)
    make_targets, make_inputs = make_inventory(source)
    package_targets = {
        line.split(",")[0].strip()
        for line in (source / "packaging/installation_components.txt")
        .read_text()
        .splitlines()
        if "," in line
    }
    inventory = {
        "cmake_only": sorted(set(targets) - make_targets),
        "make_only": sorted(make_targets - set(targets)),
        "unpackaged_cmake": sorted(set(targets) - package_targets),
    }
    (reports / "target-parity.json").write_text(json.dumps(inventory, indent=2) + "\n")
    entries = json.loads((build / "compile_commands.json").read_text())
    compiled = {}
    for entry in entries:
        path = Path(entry["file"])
        if not path.is_absolute():
            path = Path(entry["directory"]) / path
        if path.is_relative_to(source):
            compiled.setdefault(str(path.relative_to(source)), []).append(entry)
    baseline = json.loads(
        (source / ".circleci/target-parity-baseline.json").read_text()
    )
    failures = target_drift(inventory, baseline["differences"])
    new_sources = {
        p
        for p in changes["added"]
        if p.startswith("src/")
        and p.endswith(".c")
        and first_party(p)
        and p not in exceptions
        and "/tests/" not in p
    }
    for name in sorted(new_sources):
        if name not in compiled:
            failures.append(f"{name}: missing from configured CMake build")
        obj = str(Path(name[4:]).with_suffix(".o"))
        # SUMA uses a recursive makefile with its own database; target parity
        # below checks newly introduced executables there.
        if (
            not name.startswith("src/SUMA/")
            and obj not in make_inputs
            and name[4:] not in make_inputs
        ):
            failures.append(
                f"{name}: no source/object prerequisite in Make inventory ({obj})"
            )
    for target, sources in targets.items():
        if new_sources.intersection(sources):
            if target not in make_targets:
                failures.append(f"{target}: new executable is missing from Make")
            if target not in package_targets:
                failures.append(
                    f"{target}: new executable has no installation component"
                )
    changed = {p for p in changes["files"] if first_party(p)}
    # A header change can affect any translation unit. Use the compiler's real
    # dependency lists from Ninja instead of guessing #include relationships.
    selected = changed & set(compiled)
    headers = {p for p in changed if p.endswith(".h")}
    if headers:
        dependencies = subprocess.check_output(
            ["ninja", "-t", "deps"], cwd=build, text=True
        )
        affected_objects = set()
        current_object = None
        header_paths = {str((source / p).resolve()) for p in headers}
        # Shared headers occur in hundreds of objects; resolve each path once.
        resolved_dependencies = {}
        for line in dependencies.splitlines():
            if line and not line[0].isspace():
                current_object = line.split(":", 1)[0]
            elif line.strip():
                dependency_name = line.strip()
                if dependency_name not in resolved_dependencies:
                    dependency = Path(dependency_name)
                    if not dependency.is_absolute():
                        dependency = build / dependency
                    resolved_dependencies[dependency_name] = str(dependency.resolve())
                if resolved_dependencies[dependency_name] in header_paths:
                    affected_objects.add(current_object)
        for name, commands in compiled.items():
            for entry in commands:
                args = entry.get("arguments") or shlex.split(entry["command"])
                if "-o" in args and args[args.index("-o") + 1] in affected_objects:
                    selected.add(name)
    with (reports / "c-syntax.log").open("w") as log:
        for name in sorted(selected):
            if not name.endswith(".c") or not first_party(name):
                continue
            for entry in compiled[name]:
                log.write(f"Checking {name}\n")
                log.flush()
                result = subprocess.run(
                    syntax_command(entry),
                    cwd=entry["directory"],
                    stdout=log,
                    stderr=subprocess.STDOUT,
                )
                if result.returncode:
                    failures.append(f"C diagnostics: {name} (see c-syntax.log)")
    if failures:
        raise RuntimeError("\n".join(failures))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=["cmake", "archive"])
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--build", type=Path)
    parser.add_argument("--archive", type=Path)
    parser.add_argument("--reports", type=Path, required=True)
    args = parser.parse_args()
    args.reports.mkdir(parents=True, exist_ok=True)
    exclusions = json.loads(
        (args.source / ".circleci/parity-exceptions.json").read_text()
    )
    if any(
        not isinstance(reason, str) or not reason.strip()
        for reason in exclusions.values()
    ):
        parser.error("Every parity exception must have a reason")
    changes = load_changes(args.reports)
    if args.mode == "archive":
        archive_check(args.source, args.archive, changes, args.reports, exclusions)
    else:
        cmake_check(args.source, args.build, changes, args.reports, exclusions)
