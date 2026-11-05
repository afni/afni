# Guidance for AI coding agents

AFNI (Analysis of Functional NeuroImages) is a collection of programs and
libraries for analyzing and displaying neuroimaging data. It includes C/C++,
Python, R, and shell code, with both volume-based AFNI and surface-based SUMA
tools. Scientific correctness and compatibility with existing analysis pipelines
matter more than stylistic modernization.

## AI assistance and attribution

- Every AI-assisted pull request must name the agent/tool used, the model when
  known, and the scope of its assistance (for example, implementation, tests,
  documentation, or review). List multiple tools when applicable. Do not guess
  model names or versions that are not exposed by the tool.
- The human contributor is responsible for understanding, reviewing, and
  validating the contribution. Disclosure does not replace review or testing.
- AI attribution in source code is optional. Before adding a comment crediting
  an agent in a function or file, ask the user for permission unless they have
  already authorized it. If the user declines, omit the credit without objection
  or repeated requests. If no answer is given, omit the credit and continue the
  work. Honor the user's stated preference throughout the task; PR disclosure
  remains required regardless of their source-attribution preference.
- When source attribution is authorized, keep it short and use the file's comment
  syntax in the header or above the relevant function. Identify the tool/model
  and what it helped with; add a PR reference when available. Avoid per-line
  labels and repetitive notices. Explanatory comments about how and why code
  works are still expected and do not require attribution permission.
- Preserve existing authorship, copyright, license, and scientific attribution.
  AI disclosure is separate from attribution for an algorithm or external code.

Example PR disclosure (replace the placeholders with accurate details):

```text
AI assistance: <agent/tool> (<model, if known>) helped with <scope>.
Validation: <checks actually run and any checks not run>.
```

Example optional source comment (only with the user's permission):

```c
/* AI assistance: <agent/tool> (<model, if known>) helped implement
   <specific routine or algorithm>. See PR #<number> for context. */
```

## Project map and naming

- `src/`: most compiled programs, shared libraries, headers, and scripts.
  This is a long-lived, largely flat source tree; search for related code before
  introducing a new directory or abstraction.
- `src/thd_*.c`: shared dataset operations, including dataset metadata, I/O,
  geometry, and processing. Related APIs commonly use `THD_` and `DSET_` names.
  These files generally contribute to the MRI library; they are not separate
  executable programs or necessarily separate libraries.
- `src/mri_*.c`, `src/mrilib.h`, and `src/3ddata.h`: image operations and core
  image/dataset types and interfaces. Look here for existing reusable machinery.
- `src/3d*.c`: generally volume/dataset command programs, often with a matching
  executable name, such as `3dcalc`. The prefix does not mean the input is limited
  to three dimensions; many programs handle time series or multiple sub-bricks.
  `1d*` programs commonly operate on text matrices and time series.
- `src/SUMA/`: surface visualization and processing. `Surf*` programs usually
  live here, alongside `SUMA_*` implementation files and some `3d*` programs.
  Follow the nearest existing program's naming and build arrangement.
- `src/python_scripts/scripts/`: Python command programs;
  `src/python_scripts/afnipy/`: reusable Python modules.
- `src/afni_history*.c`: AFNI's program history records. Follow the existing
  entry format and maintainer convention when a change needs a history entry;
  do not invent a contributor identity or rewrite older entries.
- `tests/scripts/` and `tests/afni_test_utils/`: pytest tests and their helpers.
- `cmake/`, root and source `CMakeLists*` files, and `packaging/`: CMake support
  and installation/component definitions.
- `doc/`: bundled and historical documentation. The main documentation is
  maintained separately in `afni/afni_doc`; command help often lives directly
  in the program source.
- Some subdirectories contain bundled third-party code. Preserve their license
  notices and avoid unrelated edits or reformatting in those trees.

These are conventions, not a complete classification. Confirm a file's role
from its callers and build definitions before changing it.

## Both build systems must be maintained

AFNI uses **both platform Makefiles and CMake**. A change that works with one
build system is not sufficient evidence that it works with the other.

- The Makefile build runs in `src/`. Platform configurations are named
  `Makefile.*` and normally include `Makefile.INCLUDE`, which holds shared
  targets and object lists. SUMA also has `src/SUMA/SUMA_Makefile`.
- The local `src/Makefile` may contain machine-specific settings. Inspect it
  before building; do not overwrite it or commit local compiler/install paths.
  For a fresh checkout, choose an appropriate platform Makefile as described
  in `README.rst` and set `INSTALLDIR` deliberately.
- CMake uses the root `CMakeLists.txt`, `src/CMakeLists.txt`,
  `src/CMakeLists_mri.txt`, `src/CMakeLists_binaries.txt`, and subdirectory
  definitions such as `src/SUMA/CMakeLists.txt`. Options are documented in
  `cmake/afni_cmake_build_options.cmake`.
- When adding or renaming a source file, library, or executable, update both
  systems' applicable source/object lists, dependencies, link settings, and
  install rules. Check `packaging/installation_components.txt` for new installed
  programs. Do not assume that either system discovers files automatically.
- Use a separate CMake build directory. Keep compiler/SDK/architecture choices
  in toolchain settings and target-specific settings on the relevant targets.
  For Apple Silicon, see `cmake/README.macos-arm64.md`.
- Validate affected targets in both systems when the environment supports it.
  If one cannot be tested, report which one and why; do not claim build parity.

Typical commands, after configuring the local toolchain and dependencies:

```sh
# Makefile: a focused target, or the full build AND install.
make -C src 3dinfo
make -C src vastness

# CMake: add platform/toolchain/component options as needed.
cmake -S . -B build
cmake --build build --target 3dinfo --parallel 4
```

`vastness` installs into `INSTALLDIR`; check the destination first. Prefer a
focused target during development. Do not run broad cleanup targets in a
working tree containing someone else's build products or work.

## Implementation and scientific behavior

- Keep changes focused. Reuse existing AFNI/SUMA helpers and follow the local
  code style; avoid unrelated formatting, renaming, or dependency additions.
- Preserve command names, options, defaults, output naming, exit behavior, and
  machine-readable output unless changing them is part of the requested work.
  Update embedded help and examples whenever user-facing behavior changes.
- Treat geometry and metadata as part of correctness: voxel orientation and
  coordinate conventions, grid/affine transforms, sub-brick indexing and labels,
  scaling, data types, time units, and statistical metadata. Reuse dataset APIs
  rather than reconstructing these conventions separately.
- For numerical changes, explain the algorithm and assumptions. Check relevant
  edge cases such as empty masks, constant signals, non-finite values, short
  time series, and rank-deficient inputs. Use appropriate reference results and
  justified tolerances; do not loosen tolerances just to make tests pass.
- Follow existing allocation/ownership and error-reporting patterns. Check size
  calculations, bounds, and cleanup paths when changing C/C++ memory handling.
- Preserve OpenMP support where applicable. Check shared state, reductions,
  reproducibility, and memory costs when modifying parallel numerical code.
- Keep Linux and macOS in mind. Do not propagate workstation-specific paths,
  compiler flags, or assumptions about a shell into shared code.

## Readable code and comments

- Write for the next human maintainer. Prefer descriptive names, explicit data
  flow, and straightforward control flow over clever expressions or compactness.
  Match the surrounding indentation, braces, naming, and language conventions;
  do not impose a new style on an existing file.
- Keep functions cohesive and nesting manageable. Extract a helper when it gives
  a meaningful operation a name or removes substantial duplication, not merely
  to split code into arbitrary small pieces. Name non-obvious constants and
  explain their meaning, units, or derivation.
- Give each new or substantially changed nontrivial function a short comment or
  docstring describing its purpose and contract. Document relevant input shapes,
  units, coordinate conventions, ownership, mutation, return values, and failure
  behavior. Scale the detail to the function; a trivial, self-explanatory helper
  does not need a comment that just repeats its name. Keep shared API contracts
  at the declaration when appropriate, and implementation details by the code.
- Comment complex operations locally. Explain the reasoning, assumptions, and
  invariants behind numerical formulas, indexing, transforms, tolerances,
  parallel synchronization, and unusual edge-case handling. For scientific
  algorithms, include a reference or enough derivation to connect the code to
  the method. Explain why an operation is needed, rather than narrating syntax.
- Keep comments accurate when code changes. Do not add line-by-line narration,
  commented-out implementations, or unsupported claims of correctness or speed.
  If a block needs a long explanation just to follow its control flow, first
  consider simplifying the code.
- In C programs, strongly prefer structured control flow (`if`, loops, early
  returns, and small helpers) and not `goto`. A forward jump to a clearly named
  cleanup/error label is acceptable when it makes resource release more reliable
  and avoids duplicated cleanup or deep nesting within a function. Keep the
  cleanup path local and easy to audit, ensure resources are initialized before
  they can be cleaned up, and preserve the intended error status. 
  Explain any non-obvious cleanup ordering. Do not use `goto` for ordinary
  branching or loops, or introduce jumps into nested scopes.
  In C++, prefer existing RAII/resource-owning types for cleanup. Do not rewrite
  established cleanup patterns solely to remove a `goto`.

## Validation and delivery

- Inspect the working-tree status first and preserve unrelated changes.
  Do not commit generated binaries, object files, build trees, downloaded test
  data, or local analysis outputs.
- Read `tests/README.rst` for test setup. The pytest suite has data and environment
  dependencies; some tests also need a display or Xvfb. Reuse existing fixtures
  and helpers, and prefer small synthetic cases when suitable.
- Test the code you changed: verify the executable path and, for Python, the
  imported module location. An older AFNI installation on `PATH` can make a
  test appear to pass without exercising the new build.
- Extend relevant tests for fixes and new behavior. Start with focused tests
  and expand coverage according to the impact, especially for shared libraries.
  A successful compile or `-help` invocation alone does not validate numerics.
- Useful test commands after environment setup:

  ```sh
  # From tests/, with PATH pointing to the intended AFNI build/installation:
  pytest scripts/test_3dinfo.py
  python run_afni_tests.py -h
  python run_afni_tests.py examples -v
  ```

- For performance claims, compare equivalent inputs/options against an unchanged
  baseline, record compiler and thread settings, and verify output correctness.
- Before handing off, review the diff and run `git diff --check`. Summarize the
  behavior changed, tests actually run, results, and any remaining limitations.
  Include the AI-assistance disclosure when preparing a PR description.
