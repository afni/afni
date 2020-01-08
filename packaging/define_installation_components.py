#!/usr/bin/env python

# should be executed from within the development docker image
import subprocess as sp
import os
from pathlib import Path
import re
import shutil

build_dir = Path("/build")
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
    """
cmake -GNinja /opt/afni \
    -DGENERATE_PACKAGING_COMPONENTS=ON \
    -DBUILD_OPENGL_DEPENDENT_GUI_PROGS=ON \
    -DBUILD_X_DEPENDENT_GUI_PROGS=ON \
    -DADD_RSTATS=ON \
    -DDO_NOT_INSTALL_SCRIPTS=OFF \
    -DBUILD_BINARIES=ON
""",
    shell=True,
)
sp.check_output("ninja")


###### Install (with logging) an increasing proportion of the suite to
#      determine the different components

# without afni_binaries, just corelibs
sp.check_output(
    """
cmake /opt/afni \
    -DBUILD_OPENGL_DEPENDENT_GUI_PROGS=OFF \
    -DBUILD_X_DEPENDENT_GUI_PROGS=OFF \
    -DADD_RSTATS=OFF \
    -DADD_PYTHON=OFF \
    -DDO_NOT_INSTALL_SCRIPTS=ON \
    -DBUILD_BINARIES=OFF
ninja
ninja install  > components/0_corelibs.txt
""",
    shell=True,
)
print("Built without binaries")

# without afni_python, just corelibs and core binaries
sp.check_output(
    """
ninja uninstall
cmake /opt/afni \
    -DDO_NOT_INSTALL_SCRIPTS=ON \
    -DBUILD_BINARIES=ON
ninja
ninja install  > components/1_corebinaries.txt
""",
    shell=True,
)
print("Built without tcsh scripts")

# without afni_gui, libs;binaries;tcsh
sp.check_output(
    """
ninja uninstall
cmake /opt/afni \
    -DDO_NOT_INSTALL_SCRIPTS=OFF
    -DADD_PYTHON=OFF
ninja
ninja install  > components/2_tcsh.txt
""",
    shell=True,
)
print("Built without python")

# without afni_scripts, libs;binaries;tcsh;python
sp.check_output(
    """
ninja uninstall
cmake /opt/afni \
    -DADD_PYTHON=ON
ninja
ninja install  > components/3_python.txt
""",
    shell=True,
)
print("Built without gui")


# without afni_suma, libs;binaries;tcsh;afni
sp.check_output(
    """
ninja uninstall
cmake /opt/afni \
    -DBUILD_X_DEPENDENT_GUI_PROGS=ON
ninja
ninja install  > components/4_gui.txt
""",
    shell=True,
)
print("Built without suma")

# without rstats, libs;binaries;tcsh;afni;suma
sp.check_output(
    """
ninja uninstall
cmake -GNinja /opt/afni \
    -DBUILD_OPENGL_DEPENDENT_GUI_PROGS=ON
ninja install  > components/5_suma.txt
""",
    shell=True,
)
print("Built without rstats")

# full, libs;binaries;tcsh;afni;suma;rstats
# rstats component dependent on corelibs and R packages from CRAN
sp.check_output(
    """
ninja uninstall
cmake /opt/afni \
    -DADD_RSTATS=ON
ninja
ninja install  > components/6_rstats.txt
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


(components / "components.txt").write_text("\n".join(all_outvals))
