.. contents:: 
    :depth: 4 

****************
@build_afni_Xlib
****************

.. code-block:: none

    
    @build_afni_Xlib     - compile and install new lesstif or libXt tree
    
    This will compile lesstif, openmotif and/or libXt, were each
    of those directories should be under this 'X' directory.
    
        usage: @build_afni_Xlib [options] dir1 dir2 ...
    
    There are 3 options for where the install will be:
    
        1. X/install          - this is the default
        2. /usr/local/afniX   - via the -afniX option
        3. X/PACKAGE/install  - via the -localinstall option
    
    This allows for complete building of any package without
    overwriting an existing one (e.g. since libXm.a is not unique).
    
    options:
    
        -afniX        : install under /usr/local/afniX
                        (default is ../install)
        -g            : compile with -g to add symbols
                        (no longer the default)
        -lib32        : install libs under lib, and force 32-bit compile
                        (on Linux: add --target=i386)
        -lib64        : install libs under lib64
                        (default is lib)
        -localinstall : install under each package directory
    
    
    examples:
    
        @build_afni_Xlib -help
    
        @build_afni_Xlib lesstif
        @build_afni_Xlib -afniX -lib64 openmotif libXt
        @build_afni_Xlib -lib64 -localinstall -g lesstif
    
    note: do not install both lesstif and openmotif (of course :)
    
    note: for compiling AFNI, set XROOT to the install dir in Makefile
