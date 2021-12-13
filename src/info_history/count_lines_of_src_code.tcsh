#!/bin/tcsh


# Run the following in the git repo to count the numbers of lines of
# code present:
# --------------------------------------------------------------------------

# Run from afni/src 
cd ..

git ls-files | xargs wc -l
