AFNI CircleCI maintenance
========================

The normal workflow is::

    lint -> cmake_build (build, compiler checks, installed smoke test)
                         |-> test_cmake_build (two shards)
                         |-> macos_build
         -> make_build  (release tarball, build, installed smoke test)
                         |-> test_make_build (two shards)

    both builds -> containers_check
    both test jobs + macOS + containers_check -> deploy (release tags only)

The lint job runs without remote Docker. Linux builds can run concurrently;
macOS waits for the CMake installation and synthetic numerical smoke test.
The existing scientific pytest selection, including --runveryslow, runs in both
builds. The two repository-wide formatting tests in test_python_style.py are
replaced in CI by incremental Black checks in the initial lint job. Existing
formatting drift is advisory; new formatting regressions in previously clean
test/CI files block builds. The standalone formatting tests remain available
for local use. Test files are split using CircleCI's filename timing data, falling
back to name-based splitting on the first run. JUnit uses xunit1 so filenames
match the relative scripts/... paths supplied to the splitter.

The dedicated CI test runner uses docker cp rather than remote bind mounts.
It checks out the exact afni_ci_test_data gitlink revision, runs pytest directly
with an explicit file list, and recovers output/JUnit after failures. Empty
shards never invoke pytest. Tests run sequentially within each shard, with two
OpenMP threads and one BLAS thread by default; tests explicitly exercising
other thread counts retain their overrides. The interactive run_afni_tests.py
interface is unchanged.

The Python 3.12 upgrade also requires explicit asyncio tasks in the test harness.
The alternate CMake Python installation now marks command scripts executable.
Test-data recovery stages a complete copy before replacing a cache and preserves
read-only annex files while keeping directories writable for the CI user.
The Ubuntu 24 Make configuration uses bundled GLw, matching CMake and avoiding
duplicate global definitions from the distro GLw header with modern GCC.

Images and caches
-----------------

NeuroDebian noble and the CircleCI Python executor are pinned by digest.
Python's direct dependencies are pinned in .docker/requirements-ci.txt;
transitive dependencies and Ubuntu packages are not fully locked. Refreshing
an image digest or dependency pin requires numerical and container validation.
The legacy standalone executor Dockerfile is modernized too, but CI no longer
requires publishing that image before a configuration change can run.

The dependency-image cache fingerprints the Dockerfile, requirements, entrypoint,
and permission helper. Both build systems import/export ccache explicitly;
ccache checks compiler content and compilation flags. Cache entries are separated
by build type and base-image fingerprint. Runtime images contain the installation
and source/tests, but no compiler cache or build tree. Coverage retains its build
objects separately. The macOS cache includes architecture, compiler, and installed
Homebrew versions; Homebrew itself is not cached.
Make also uses a BuildKit cache mount so a failed build can reuse compilation
on the same Docker builder; successful builds export it for later CircleCI jobs.

By default, compressed images are transferred through pipeline-specific caches,
so fork PRs need no registry write credentials. Set pipeline parameter
registry_transfer=true only for trusted pipelines, with these environment variables::

    AFNI_CI_REGISTRY=registry.example.org/afni-ci
    AFNI_CI_REGISTRY_USER=...
    AFNI_CI_REGISTRY_PASSWORD=...

Use an explicit registry hostname, even for Docker Hub (docker.io/...). Tags
include the source SHA and pipeline ID (stable across workflow reruns). Configure registry retention for these
transient images. Never enable secret forwarding to untrusted fork PRs. Normal
release publishing still uses DOCKER_USER/DOCKER_PASS, after all gates pass.

Checks and their limits
-----------------------

Incremental lint resolves the real PR base via GitHub's merge ref, or the master
merge-base on non-PR branches. Master and tag builds inspect HEAD^..HEAD. A missing
comparison revision fails explicitly. Python syntax regressions, shell syntax,
new whitespace errors, and spelling in added text are blocking. Existing
Python-2-only utilities are reported without imposing a Python-3 migration.
Cppcheck is advisory, bounded to eight configurations and five minutes, and
excludes bundled source trees. Its distro package is not a blocking version pin.

C checks use compile_commands.json and generated headers after the CMake build,
not guessed include paths. Changed headers select their consumers from Ninja's
dependency database. Compiler errors for implicit declarations, incompatible
pointers, return types and unsafe format strings are blocking for those files.
These checks follow compilation because some required headers are generated by
build targets; they are not part of the lightweight initial lint gate.

Build-parity checks use CMake's File API and explicit root/ptaylor Make inventory
targets, never a developer's local Makefile. Historical target differences are
recorded in target-parity-baseline.json with their source revision/configuration.
New target differences or unpackaged CMake executables fail; resolved differences
are allowed. Intentional baseline additions need review. Every run saves a JSON
report. New first-party C sources must appear in CMake and, outside recursive
SUMA rules, in Make's source/object prerequisites. New CMake executables must appear
in Make's program list and packaging/installation_components.txt. Exceptions
need a source-path key and explanatory reason in parity-exceptions.json.

The release archive is inspected before the Make build. Existing missing C/header
inputs are reported, and new omissions fail. The Make build still compiles from
that archive. Generated headers are prepared first. Make compilation remains
serial: library recipes create groups of object files as side effects, and
recursive program recipes invoke clean alongside build goals. Both patterns
race under -j. Workflow jobs and test shards still run in parallel; broad Make
dependency repairs are separate work. This incremental policy is not a complete
source/link equivalence proof: per-target source/link relationships, recursive
SUMA source membership, all header prerequisites, and intentional platform
differences need further review. Do not
interpret an empty exception file as proof that the older build systems match.

Coverage and validation
-----------------------

Set pipeline parameter coverage=true to run a separate instrumented CMake build.
It retains both C and Python coverage as artifacts, without downloading or running
a Codecov upload script. It is opt-in, not an automatically scheduled nightly job.

Local helper validation (PyYAML, codespell, and Black required)::

    python -m unittest discover -s .circleci/tests -v
    bash -n .circleci/changed-files.sh .circleci/build-image.sh .circleci/image-transfer.sh
    circleci config validate .circleci/config.yml
    git diff --check

Container validation should use a clean source snapshot (not a tree containing
local generated binaries), and should verify both Make and CMake builds, smoke
tests, container permissions, and numerical tests. Do not claim timing improvements
until measuring equivalent runs on CircleCI. Useful measurements include dependency
setup, compilation, cache transfer, per-shard execution, and macOS queue/runtime.
