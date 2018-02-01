***************
@make_plug_diff
***************

.. _@make_plug_diff:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @make_plug_diff -vtk VTKDIR -xm XMDIR -asrc ASRCDIR -abin ABINDIR 
    Compiles AFNI's diffusion plugin.  
    I used it as a way to log what is needed to compile the plugin.
    We should work closely with Greg Balls and Larry Frank to make the
    need for this script obsolete
     Options:
       -comments: output comments only
       -linux: flag for doing linuxy things 
       -vtk VTKDIR: Directory where vtk is installed
       -xm XMDIR: Directory where motif is installed
       -asrc ASRCDIR: Full path to AFNI's src/ directory 
       -abin ABINDIR: Path, relative to ASRCDIR, to abin
       -diff DIFFDIR: name of directory containing diffusion code
    
    Sample compilation on GIMLI (OSX 10.5)
       @make_plug_diff         -vtk /sw    -xm /sw  \
                               -asrc /Users/ziad/b.AFNI.now/src \
                               -abin ../abin  -diff afni-diff-plugin-0.86
    
    Sample compilation on linux (FC 10)
       @make_plug_diff         -xm /usr -asrc /home/ziad/b.AFNI.now/src \
                               -abin ../abin -diff afni-diff-plugin-0.86 \
                               -linux
