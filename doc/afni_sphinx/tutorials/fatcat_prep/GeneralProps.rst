.. _FATPREP_genprops:

General fat_proc\* script properties
==================================

.. contents::
   :depth: 3

Specifying output location and prefix
-------------------------------------

| *The Naming of Cats is a difficult matter,*
| *It isn't just one of your holiday games;*
| *You may think at first I'm as mad as a hatter*
| *When I tell you, a cat must have THREE DIFFERENT NAMES.*
| --T. S. Eliot

Fortunately, the naming of outputs isn't *so* complicated as that, but
one can essentially output with two names: a new subdirectory and a
prefix for filenames.  For each `fat_proc*` function, one can use
`-prefix *` to either put files of a given prefix into a preexisting
directory, or to both create a new subdirectory and put files of a
given prefix there.  Thus, consider an example using `-prefix
/somewhere/group/AAA/bbb`:

* if there is an existing directory "/somewhere/group/AAA/", then this
  option would put files starting with "bbb" into it;

* if there were just an empty, preexisting directory called
  "/somewhere/group/", then this option would first create a new
  subdirectory "/somewhere/group/AAA/" and then populate it with files
  starting with "bbb";

* if there were just an empty directory called "/somewhere/", then
  this option would lead to failure-- it can only create a new output
  directory one layer deep.


Automatic QC imaging
--------------------

Most `fat_proc*` functions automatically generate sets of montaged
images (PNG files by default).  In each case the choice of image(s) is
mainly just due to what I thought might be useful or have found myself
creating often in order to evaluate a given step or the state of a
particular data set.  By default, a set of axial, sagittal and coronal
views are made.  

For 3D volumes, the montage size is typically 15 slices as evenly
spaced as possible across each FOV dimension.  These may show just an
underlay, or underlays with either a translucent/masked overlay or an
"edge-ified" image (esp. for judging alignment).  

For 4D data, the montage typically shows a single slice across all
time points (which, for DWI, might just represent different DW
volumes).  The *N* images are arranged in something close to a golden
ratio array, padded at the end as necessary to have a solid grid of
images.  Additionally, a GIF or MPG movie may be created.


Temporary working directories
-----------------------------

Most `fat_proc*` functions perform several sub-steps, and therefore
they make use of temporary working directories.  By default, these are
deleted or "cleaned up."  If you want to keep them, for example if
some step is failing and you want more information about why, you can
instruct them to not be deleted using the `-no_clean` switch.


