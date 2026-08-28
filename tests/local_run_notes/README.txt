These are the ramblings of PT from trying to run tests locally, to
help himself (and maybe others) figure out some things more easily.

On CircleCI, it seems like Python 3.6 is used. Locally, that is hard
to install, but then some syntaxes in Python have changed since that
break some functionality.

Also, datalad has changed function syntax that breaks things, and we
didn't try to install a very old version of _that_, in case some fixes
were useful.

So, we have a new environment.yml that tries to strike a balance
between these things, which is in this directory. We also have updated
versions of some *.py files that will help run things better in
place. Those copies are here, with the same file name. We don't commit
those into their normal locations, because we don't want to break
things on CircleCI, which have string version constraints.

Current version of datalad locally is (apparently): '1.6.2'.
=============================================================================

environment_local-2026-08-28.yml

  YML file to use that causes least woe. Forces kind of old Python to
  avoid getting too far ahead of CircleCI's Python 3.6, while still
  being installable.

  Still needs to be installed in the same afni/tests/ dir.

  Note that some datalad syntax differs from what is needed on
  CircleCI at present. Hence the update to data_management.py

data_management.py

  This will still need to be put into its usual ../afni_test_utils/
  home, but this file works better with the datalad syntax that gets
  installed with this environment.

=============================================================================

When running locally, to be able to get some details about ptaylor/*
tests failing, this was a useful command to run for starters:

  ./run_afni_tests.py --file  scripts/test_ptaylor.py     \
       --runslow --runveryslow --verbosity diarrhetic     \
       --extra-args="--tb=long --show-capture=all -vv"    \
       local                                              \
       2>&1 | tee log_test_output_v1.txt

To run just a single test, this was useful:

  ./run_afni_tests.py --file  scripts/test_ptaylor.py     \
       --runslow --runveryslow --verbosity diarrhetic     \
       --extra-args="-k 3dNetCorr --tb=long --show-capture=all -vv"    \
       local                                              \
       2>&1 | tee log_test_output_v1.txt





