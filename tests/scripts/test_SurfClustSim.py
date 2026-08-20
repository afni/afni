"""Tier-1 tests for SurfClustSim: exact invariants, no test data required.

Unlike most simulation tests in this suite, nothing here compares against a
stored reference with a tolerance.  Every assertion is exact, which is possible
because:

  * CreateIcosahedron builds a surface from nothing, so each test makes its own
    input and no datalad fetch is needed;
  * SurfClustSim seeds its noise from the global simulation index rather than
    from anything about scheduling, so its output is required to be
    bit-identical across thread counts and block sizes.

That last property is the thing worth guarding.  It is easy to lose by
accident -- any change that makes a worker's random stream depend on which
thread or block picked up the work will break it -- and a tolerance-based test
would not notice.
"""

from afni_test_utils.misc import is_omp
from afni_test_utils import tools
import hashlib
import pytest
import re

OMP = is_omp("SurfClustSim")

# No data_paths: these tests generate their own surface.


def make_surface(data, prefix="ico", rd=4):
    """Build a small icosahedral surface and return its spec file name.

    rd=4 gives 2562 nodes -- big enough for clusters to form and merge at the
    thresholds used here, small enough that the whole module runs in seconds.
    """
    cmd = f"CreateIcosahedron -rad 100 -rd {rd} -prefix {prefix}"
    tools.run_cmd(data, cmd, workdir=data.outdir)
    spec = data.outdir / f"{prefix}.spec"
    assert spec.exists(), "CreateIcosahedron did not produce a spec file"
    return prefix


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def sim(data, surf, prefix, extra="", nthreads=1, niter=60, add_env_vars=None):
    """Run one simulation and return the raw max-area file for p=0.01.

    -maxarea_1D is used rather than the alpha table because it is the raw
    per-simulation output: one line per simulation, no quantile interpolation
    in between, so a byte comparison means the simulations themselves matched.
    """
    cmd = (
        f"SurfClustSim -spec {surf}.spec -surf_A {surf} -on_surface"
        f" -fixed -sigma 5 -Niter 4 -niter {niter} -pthr 0.01"
        f" -legacy_sided -maxarea_1D -prefix {prefix}"
        f" -nthreads {nthreads} {extra}"
    )
    tools.run_cmd(data, cmd, workdir=data.outdir, add_env_vars=add_env_vars)
    out = data.outdir / f"{prefix}.legacy.max.area.0.01"
    assert out.exists(), f"no max-area output from: {cmd}"
    return out


def test_selfcheck(data):
    """The union-find threshold sweep must agree exactly with a flood fill.

    SurfClustSim gets every threshold out of one sorted pass by activating
    nodes in descending order and merging components with union-find, which
    relies on max cluster area being monotone as the threshold falls.
    -selfcheck re-derives every area with an independent breadth-first search
    and exits nonzero on any disagreement, so this exercises the real
    clustering code against an oracle rather than against a stored number.
    """
    surf = make_surface(data)
    cmd = (
        f"SurfClustSim -spec {surf}.spec -surf_A {surf} -on_surface"
        f" -fixed -sigma 4 -Niter 6 -niter 60"
        f" -1sided -2sided -bisided -legacy_sided"
        f" -selfcheck -prefix sc -nthreads 2"
    )
    tools.run_cmd(data, cmd, workdir=data.outdir)


def test_deterministic(data):
    """The same command twice must give the same bytes."""
    surf = make_surface(data)
    assert digest(sim(data, surf, "d1")) == digest(sim(data, surf, "d2"))


@pytest.mark.skipif(not OMP, reason="requires an OpenMP build")
def test_thread_count_does_not_change_results(data):
    """Results must not depend on -nthreads.

    Noise is seeded from the global simulation index, so which thread runs a
    given simulation is irrelevant.  Exact equality, not a tolerance.
    """
    surf = make_surface(data)
    one = digest(sim(data, surf, "t1", nthreads=1))
    assert one == digest(sim(data, surf, "t4", nthreads=4))
    assert one == digest(sim(data, surf, "t8", nthreads=8))


@pytest.mark.skipif(not OMP, reason="requires an OpenMP build")
def test_omp_num_threads_env_does_not_change_results(data):
    """Same invariant, driven through the environment instead of the option."""
    surf = make_surface(data)
    a = sim(data, surf, "e1", nthreads=0, add_env_vars={"OMP_NUM_THREADS": "1"})
    b = sim(data, surf, "e4", nthreads=0, add_env_vars={"OMP_NUM_THREADS": "4"})
    assert digest(a) == digest(b)


def test_itersize_does_not_change_fixed_mode(data):
    """In -fixed, -itersize is pure scheduling and must not move results.

    Blocks are distributed across threads, so -itersize sets how work is
    grouped, not what the work is.  Sizes chosen so that one divides -niter
    evenly and one does not, to catch mishandling of the short final block.
    """
    surf = make_surface(data)
    ref = digest(sim(data, surf, "i10", extra="-itersize 10"))
    assert ref == digest(sim(data, surf, "i3", extra="-itersize 3"))
    assert ref == digest(sim(data, surf, "i25", extra="-itersize 25"))


def test_itersize_does_change_compat_mode(data):
    """In -compat, -itersize IS a statistical parameter -- guard that it shows.

    This asserts a wart rather than a virtue.  The block is the adaptive
    smoothing master: SurfSmooth averaged FWHM across the columns it was given
    and applied one Niter to all of them, and the detrending order is derived
    from the block length, so slow_surf_clustsim.py's "block size (speed-up)"
    knob silently changed the null distribution too.  -compat reproduces that
    on purpose.  If this test ever starts failing, -compat has stopped being
    faithful and the change needs to be deliberate.
    """
    surf = make_surface(data)
    common = "-compat -target_fwhm 20 -pthr 0.01 -legacy_sided -maxarea_1D"
    for prefix, isize in (("c5", 5), ("c20", 20)):
        cmd = (
            f"SurfClustSim -spec {surf}.spec -surf_A {surf} -on_surface"
            f" -niter 60 {common} -prefix {prefix} -itersize {isize} -nthreads 2"
        )
        tools.run_cmd(data, cmd, workdir=data.outdir)
    a = data.outdir / "c5.legacy.max.area.0.01"
    b = data.outdir / "c20.legacy.max.area.0.01"
    assert digest(a) != digest(b), (
        "-compat results no longer depend on -itersize; either the adaptive "
        "smoothing block changed, or -compat is no longer reproducing "
        "slow_surf_clustsim.py"
    )


def test_maxarea_has_one_line_per_simulation(data):
    """Every simulation contributes a line, including empty ones.

    slow_surf_clustsim.py appended a line only when a cluster existed, so its
    z.max.area.* files were short by the number of empty simulations.  This
    writes an explicit 0 instead, which is why quick.alpha.vals.py must be
    given -niter when comparing the two.
    """
    surf = make_surface(data)
    out = sim(data, surf, "n", niter=60)
    lines = out.read_text().split()
    assert len(lines) == 60
    assert all(float(v) >= 0.0 for v in lines)


def read_table(path):
    """Parse a SurfClustSim .1D table into {pthr: [cutoff per alpha]}."""
    out = {}
    for line in path.read_text().splitlines():
        if not line or line.startswith("#"):
            continue
        parts = line.split()
        out[float(parts[0])] = [float(v) for v in parts[1:]]
    return out


def test_multithresh_cutoffs_never_below_independent(data):
    """Jointly calibrated cutoffs must be >= the per-threshold ones.

    -multithresh calls a result significant if a cluster survives at ANY
    p-threshold, so it is paying for a family of tests rather than one.  Its
    cutoffs must therefore be at least as large as the independently
    calibrated ones at the same alpha; anything smaller would mean the joint
    family is easier to pass than a single test, which cannot be right.

    This is an ordering invariant, not a stored number, so it holds regardless
    of surface, seed, or simulation count.
    """
    surf = make_surface(data)
    cmd = (
        f"SurfClustSim -spec {surf}.spec -surf_A {surf} -on_surface"
        f" -fixed -sigma 4 -Niter 6 -niter 400 -1sided"
        f" -multithresh -prefix mt -nthreads 2"
    )
    tools.run_cmd(data, cmd, workdir=data.outdir)

    indep = read_table(data.outdir / "mt.1sided.1D")
    joint = read_table(data.outdir / "mt.1sided.mthresh.1D")
    assert set(indep) == set(joint)
    for pthr, jrow in joint.items():
        for alpha_index, (jval, ival) in enumerate(zip(jrow, indep[pthr])):
            assert jval >= ival, (
                f"joint cutoff {jval} < independent {ival} at pthr={pthr}, "
                f"alpha index {alpha_index}"
            )


@pytest.mark.slow
def test_acf_generates_requested_autocorrelation(data):
    """-acf must actually produce noise with the requested ACF shape.

    This is the acceptance test for the whole basis-mixture scheme, and it
    checks the thing that matters rather than an internal fit residual: the
    program generates fields exactly as a simulation would, measures their
    autocorrelation, and reports what it achieved.

    'a' is the parameter under test -- it is the Gaussian-versus-heavy-tail
    split, the entire reason -acf exists. 'b' and 'c' are deliberately not
    asserted on: each is weakly identified whenever its component carries
    little weight, so they move a lot without the curve moving much.

    Marked slow because calibration measures autocorrelations out to a
    couple of decay lengths, which is tens of seconds.
    """
    surf = make_surface(data, rd=4)
    requested_a = 0.5
    cmd = (
        f"SurfClustSim -spec {surf}.spec -surf_A {surf} -on_surface"
        f" -acf {requested_a} 8 24 -acf_nbasis 6 -niter 20 -pthr 0.01"
        f" -1sided -prefix acfgen -nthreads 4 -verb"
    )
    differ = tools.run_cmd(
        data, cmd, workdir=data.outdir, merge_error_with_output=True, timeout=600
    )
    text = str(differ.stdout) if hasattr(differ, "stdout") else ""
    # The program prints "generated a=... b=... c=..."; parse the achieved a.
    match = re.search(r"generated a=([0-9.eE+-]+)", text)
    assert match, f"no ACF verification line in output:\n{text}"
    achieved = float(match.group(1))
    assert (
        abs(achieved - requested_a) < 0.15
    ), f"requested a={requested_a}, generated a={achieved}"
