#!/usr/bin/env python

# should be executed from within the development docker image
import subprocess as sp
import os
from pathlib import Path
import re
import shutil

build_dir = Path("/build")
src_dir = Path("/opt/afni/src")
nifti_dir = src_dir.parent / 'nifti_clib'
gifti_dir = src_dir.parent / 'gifti_clib'
components = Path("components")


# Get to a useful state (fresh or uninstalled)
if not build_dir.exists():
    raise EnvironmentError(
        "This script should run inside the afni development docker image"
    )
else:
    os.chdir(build_dir)
res = sp.run("""ninja uninstall""",shell=True)
if res.returncode != 0:
    for x in build_dir.iterdir():
        if x.is_file():
            x.unlink()
        else:
            shutil.rmtree(x)
if not components.exists():
    components.mkdir()


# Perform a full build initially
sp.check_output(
    f"""
cmake -GNinja {src_dir} \
    -DUSE_SYSTEM_ALL=ON \
    -DGENERATE_PACKAGING_COMPONENTS=ON \
    -DCOMP_SUMA=ON \
    -DCOMP_GUI=ON \
    -DCOMP_RSTATS=ON \
    -DCOMP_TCSH=ON \
    -DCOMP_COREBINARIES=ON
""",
    # using system nifti should be removed              !!!!!! \
    shell=True,
)
sp.check_output("ninja")


###### Install (with logging) an increasing proportion of the suite to
#      determine the different components

# without afni_binaries, just corelibs
sp.check_output(
    f"""
cmake {src_dir}\
    -DUSE_SYSTEM_ALL=ON \
    -DUSE_SYSTEM_DCM2NIIX=ON \
    -DCOMP_SUMA=OFF \
    -DCOMP_GUI=OFF \
    -DCOMP_RSTATS=OFF \
    -DCOMP_PYTHON=OFF \
    -DCOMP_TCSH=OFF \
    -DCOMP_COREBINARIES=OFF
ninja
ninja install  > components/0_corelibs.txt
""",
    shell=True,
)
print("Built without binaries")

# without afnipy, just corelibs and core binaries
sp.check_output(
    f"""
ninja uninstall
cmake {src_dir}\
    -DCOMP_TCSH=OFF \
    -DCOMP_COREBINARIES=ON
ninja
ninja install  > components/1_corebinaries.txt
""",
    shell=True,
)
print("Built without tcsh scripts")

# without afni_gui, libs;binaries;tcsh
sp.check_output(
    f"""
ninja uninstall
cmake {src_dir}\
    -DCOMP_TCSH=ON \
    -DCOMP_PYTHON=OFF
ninja
ninja install  > components/2_tcsh.txt
""",
    shell=True,
)
print("Built without python")

# without afni_scripts, libs;binaries;tcsh;python
sp.check_output(
    f"""
ninja uninstall
cmake {src_dir}\
    -DCOMP_PYTHON=ON
ninja
ninja install  > components/3_python.txt
""",
    shell=True,
)
print("Built without gui")


# without afni_suma, libs;binaries;tcsh;afni
sp.check_output(
    f"""
ninja uninstall
cmake {src_dir}\
    -DCOMP_GUI=ON
ninja
ninja install  > components/4_gui.txt
""",
    shell=True,
)
print("Built without suma")

# without rstats, libs;binaries;tcsh;afni;suma
sp.check_output(
    f"""
ninja uninstall
cmake -GNinja {src_dir}\
    -DUSE_SYSTEM_GTS=ON \
    -DCOMP_SUMA=ON
ninja install  > components/5_suma.txt
""",
    shell=True,
)
print("Built without rstats")

# full, libs;binaries;tcsh;afni;suma;rstats
# rstats component dependent on corelibs and R packages from CRAN
sp.check_output(
    f"""
ninja uninstall
cmake {src_dir}\
    -DCOMP_RSTATS=ON
ninja
ninja install  > components/6_rstats.txt
""",
    shell=True,
)
print("Built full build")


# full with external, the full build and the external libraries that can be
# built using the cmake build system:
# e.g. nifti,gifti,jpeg,xmhtml,gts,glut,qhull,dcm2niix,f2c
sp.check_output(
    f"""
ninja uninstall
cmake /opt/afni/src \
    -DCOMP_RSTATS=ON \
    -DUSE_SYSTEM_NIFTI=OFF \
    -DUSE_SYSTEM_GIFTI=OFF \
    -DFETCHCONTENT_SOURCE_DIR_FETCH_NIFTI_CLIB_GIT_REPO={nifti_dir} -DUSE_SYSTEM_NIFTI=OFF -DFETCHCONTENT_SOURCE_DIR_GIFTI_CLIB={gifti_dir} \
    -DUSE_SYSTEM_JPEG=OFF \
    -DUSE_SYSTEM_XMHTML=OFF \
    -DUSE_SYSTEM_GTS=OFF \
    -DUSE_SYSTEM_GLUT=OFF \
    -DUSE_SYSTEM_QHULL=OFF \
    -DUSE_SYSTEM_DCM2NIIX=OFF \
    -DUSE_SYSTEM_F2C=OFF
ninja
ninja install  > components/7_external_dependencies.txt
""",
    shell=True,
)
print("Built full build")


line_pats = ["Installing", "Up-to-date"]
# generate useful diff files
cumulative = set()
all_outvals = []
for f in sorted(components.glob("[0-9]*txt")):
    component = f.name[2:-4]

    # Filter the install log for the installation elements (progs etc)
    p = re.compile(r'(lib(?P<library>[0-9a-zA-Z_]*)\.so[.0-9]*)')
    txt = p.sub(r'\g<library>',f.read_text())
    txt_list = [ln for ln in txt.splitlines() if any(x in ln for x in line_pats)]

    # extract the target names
    t_names = set(Path(x.split(":")[-1].strip()).name for x in txt_list)
    if not t_names:
        raise ValueError()
    # print(t_names)

    if cumulative:
        t_names = t_names.difference(cumulative)

    outvals = [x + ", " + component for x in t_names]
    all_outvals += sorted(outvals)
    cumulative = cumulative.union(t_names)

# Modify targets
targs_to_remove = ['dcm2niix']
cumulative = {entry for entry in cumulative if entry not in targs_to_remove}
(components / "components.txt").write_text("\n".join(all_outvals))
    