This is a brief note or reminder on using quotize to take
simple-formatted text files in the afni/doc/README/ direectory and
turn them into C-code-usable ones (which are also updated+saved in the
code repository).

The quotize program is run using the README.<something> file as input,
and the user specifies both the name of the string array that the text
will have within the file, as well as the file name for it to be saved
to.  In general, it is recommended to give the file name a temporary
name initially, to check it over before deciding to put it into the
repository.

Therefore in the end, generally 2 files will be updated in the
repository when using quotize: the input README.* file, and the newly
created/updated text file (like a *.h file to be placed in afni/src/).

--------

For example, the afni/src/license.h file can be made as follows (NB:
the '>' and '<' operations are literally used in the command line
here; they are not suggesting a generic name):

  quotize license < SOME_PATH/afni/doc/README/README.copyright > tmp_license.h

Then, if one has happy with the looks of tmp_license.h, one can copy
it over to the proper location in the repository:

  cp tmp_license.h ANOTHER_PAT/afni/src/license.h

---------
