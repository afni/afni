# file "example_build.py"

# Note: we instantiate the same 'cffi.FFI' class as in the previous
# example, but call the result 'ffibuilder' now instead of 'ffi';
# this is to avoid confusion with the other 'ffi' object you get below
from pathlib import Path
import os
import re
import subprocess
from cffi import FFI
import pycparser
ffibuilder = FFI()


if __name__ == "__main__":
    header_file = Path('mrilib.h')
    # header_file = Path('cs.h')
    pp = subprocess.run("gcc -E -P -D'__attribute__(x)=' -I. -Ipycparser/utils/fake_libc_include -Inifti/nifti2 -Inifti/niftilib -Inifti/nifticdf -Inifti/znzlib -Irickr -I/sw/include -I/usr/X11/include -nostdinc mrilib.h > mrilib_processed.h",
       shell =True,
        stdout= subprocess.PIPE,
        stderr = subprocess.PIPE)

    # pycparser.parse_file('mrilib_processed.h')
    ffibuilder.cdef(Path('mrilib_processed.h').read_text())
    ffibuilder.set_source("_pyafni",'#include "mrilib_processed.h"',
         libraries=["mri"],
         library_dirs=[Path.cwd().absolute()],
         include_dirs=[".","nifti/nifti2","nifti/niftilib",
                       "nifti/nifticdf","nifti/znzlib",
                       "rickr","/usr/X11R6/include","/sw/include"],
         extra_compile_args=["-std=c99"]
    )

    ffibuilder.compile(verbose=True)
