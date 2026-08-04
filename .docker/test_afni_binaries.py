# Verify that AFNI's compiled binaries are native to the container's own
# architecture and actually execute. This is deliberately architecture-agnostic:
# it reads the expected architecture from the running container rather than
# hardcoding x86_64, so the same test guards both the amd64 and arm64 images.
#
# It exists because "the image built" is not the same as "the binaries run":
# a multi-arch build can silently fall back to emulation, or link/compile for
# the wrong architecture, and still produce an image that pushes successfully.
# Running a binary and comparing its ELF machine type to the container's own
# `uname -m` catches exactly that class of failure.
import pytest

# The binaries fmriprep-base copies out of afni_make_build. They are pure
# command-line tools (no X11/OpenGL), so `-help` is a safe execution probe.
FMRIPREP_BINARIES = (
    "3dTshift",
    "3dvolreg",
    "3dAutomask",
    "3dUnifize",
)

# Map `uname -m` output to the machine string `file` reports for an ELF binary.
_UNAME_TO_ELF_MACHINE = {
    "x86_64": "x86-64",
    "aarch64": "ARM aarch64",
}


@pytest.mark.parametrize(
    "named_container",
    (
        "afni/afni_make_build",
        "afni/afni_cmake_build",
    ),
    indirect=True,
)
def test_fmriprep_binaries_run_natively(named_container):
    """The fmriprep-critical binaries must exist, match the container's own
    architecture, and execute successfully."""
    c = named_container.run(tty=True, detach=True)

    arch = c.exec_run(["uname", "-m"]).output.decode("utf-8").strip()
    expected_machine = _UNAME_TO_ELF_MACHINE.get(arch)
    assert expected_machine is not None, f"Unhandled container architecture: {arch!r}"

    for prog in FMRIPREP_BINARIES:
        which = c.exec_run(["which", prog])
        path = which.output.decode("utf-8").strip()
        assert path.endswith(prog), f"{prog} not found on PATH (arch {arch})"

        # The binary's ELF machine type must match the container's architecture,
        # i.e. it was genuinely compiled for this platform, not emulated.
        file_out = c.exec_run(["file", "-L", path]).output.decode("utf-8")
        assert expected_machine in file_out, (
            f"{prog} is not a native {arch} binary: {file_out.strip()}"
        )

        # And it must actually execute on this platform.
        res = c.exec_run(["bash", "-lc", f"{prog} -help > /dev/null"])
        assert res.exit_code == 0, (
            f"{prog} -help exited {res.exit_code} on {arch}"
        )
