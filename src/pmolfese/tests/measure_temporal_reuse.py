#!/usr/bin/env python3
"""Reproduce the S4 direct-code-reuse inventory.

This is deliberately a conservative measurement: it counts complete existing
function bodies that a temporal-RSA companion can call without changing their
scientific meaning.  It does not count static 3dRSA helpers that first require
extraction, AFNI program boilerplate, headers, comments, or tests.
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path


SIMMATRIX_APIS = (
    "THD_simmat_new",
    "THD_simmat_free",
    "THD_rank_avg",
    "THD_simmat_fill_from_features",
    "THD_simmat_from_features",
    "THD_simmat_crossnobis",
    "THD_simmat_crossnobis_valid",
    "THD_noise_wdiag",
    "THD_noise_whalf",
    "THD_simmat_read_1D",
    "THD_simmat_write_1D",
    "THD_simmat_to_tri",
    "THD_simmat_to_tri_perm",
    "THD_tri_to_simmat",
    "THD_rdm_cov_transform",
    "THD_rdm_cov_cosine",
    "THD_tri_corr",
    "THD_simmat_metric_label",
    "THD_simmat_cmp_label",
    "THD_rdm_ws_new",
    "THD_rdm_ws_free",
    "THD_onesamp_t",
    "THD_p_to_z",
    "THD_mantel_cache_build",
    "THD_mantel_cache_free",
    "THD_mantel_cache_bytes",
    "THD_mantel_corr",
    "THD_mantel_corr_cached",
    "THD_signflip_t",
    "THD_signrank_signflip",
)

PERMUTE_APIS = (
    "THD_perm_scheme_new",
    "THD_perm_scheme_free",
    "THD_perm_scheme_set_blocks",
    "THD_perm_scheme_set_eqclass",
    "THD_perm_group_size",
    "THD_perm_set_build",
    "THD_perm_set_free",
    "THD_perm_set_apply",
    "THD_resample_set_build",
    "THD_resample_set_free",
    "THD_perm_result_new",
    "THD_perm_result_free",
    "THD_perm_emp_pvalue",
    "THD_perm_signed_z",
    "THD_perm_result_finish",
)


def strip_c_lexical_noise(text: str) -> str:
    """Replace comments and literals with spaces while retaining newlines."""
    pattern = re.compile(
        r"//[^\n]*|/\*.*?\*/|\"(?:\\.|[^\"\\])*\"|'(?:\\.|[^'\\])*'",
        re.DOTALL,
    )

    def blank(match: re.Match[str]) -> str:
        value = match.group(0)
        return "".join("\n" if char == "\n" else " " for char in value)

    return pattern.sub(blank, text)


def function_spans(path: Path) -> dict[str, tuple[int, int]]:
    raw = path.read_text(encoding="utf-8")
    clean = strip_c_lexical_noise(raw)
    lines = clean.splitlines()
    spans: dict[str, tuple[int, int]] = {}

    for line_index, line in enumerate(lines):
        match = re.match(
            r"^(?:static\s+)?[A-Za-z_][\w\s*]*\b(THD_[A-Za-z0-9_]+)\s*\(",
            line,
        )
        if not match:
            continue
        name = match.group(1)
        offset = sum(len(item) + 1 for item in lines[:line_index])
        brace = clean.find("{", offset)
        semicolon = clean.find(";", offset)
        if brace < 0 or (0 <= semicolon < brace):
            continue
        depth = 0
        end = None
        for pos in range(brace, len(clean)):
            if clean[pos] == "{":
                depth += 1
            elif clean[pos] == "}":
                depth -= 1
                if depth == 0:
                    end = pos
                    break
        if end is None:
            raise RuntimeError(f"unterminated function {name} in {path}")
        start_line = clean.count("\n", 0, offset) + 1
        end_line = clean.count("\n", 0, end) + 1
        spans[name] = (start_line, end_line)
    return spans


def measure(path: Path, names: tuple[str, ...]) -> tuple[int, list[str]]:
    spans = function_spans(path)
    missing = [name for name in names if name not in spans]
    total = sum(spans[name][1] - spans[name][0] + 1 for name in names if name in spans)
    return total, missing


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--src",
        type=Path,
        default=Path(__file__).resolve().parents[2],
        help="AFNI src directory (default: inferred from this script)",
    )
    args = parser.parse_args()

    sim_path = args.src / "thd_simmatrix.c"
    perm_path = args.src / "thd_permute.c"
    sim_loc, sim_missing = measure(sim_path, SIMMATRIX_APIS)
    perm_loc, perm_missing = measure(perm_path, PERMUTE_APIS)
    if sim_missing or perm_missing:
        print(f"missing simmatrix APIs: {sim_missing}")
        print(f"missing permutation APIs: {perm_missing}")
        return 1

    sim_total = len(sim_path.read_text(encoding="utf-8").splitlines())
    perm_total = len(perm_path.read_text(encoding="utf-8").splitlines())
    reusable = sim_loc + perm_loc
    library_total = sim_total + perm_total
    print(f"simmatrix: {len(SIMMATRIX_APIS)} functions, {sim_loc} body LOC")
    print(f"permutation: {len(PERMUTE_APIS)} functions, {perm_loc} body LOC")
    print(f"direct reuse: {len(SIMMATRIX_APIS) + len(PERMUTE_APIS)} functions, "
          f"{reusable} body LOC")
    print(f"source share: {reusable}/{library_total} = "
          f"{100.0 * reusable / library_total:.1f}% of the two core .c files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
