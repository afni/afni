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
