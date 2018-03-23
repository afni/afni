.. _ahelp_@diff.tree:

**********
@diff.tree
**********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
       ----------------------------------------------------------------------
       @diff.tree - show file differences between 2 directories
    
          Given: 2 directory names
    
          If desired, list files that do not exist in one of the directories.
          For the files that exist in both directories, list those that differ.
          If desired, show the actual differences.
    
          This is similar to @diff.files, which only looks at files in a
          specified list.
    
       ----------------------------------------------------------------------
       usage: @diff.tree [OPTIONS] new_dir old_dir"
    
       ----------------------------------------------------------------------
       options:
    
          -diff_opts 'OPTS'      : apply OPTS as options in diff commands
          -ignore_append i1 ...  : append to ignore_list (list in quotes)
          -ia                    : short for -ignore_append
          -ignore_list i1 ...    : create new ignore_list (list in quotes)
          -il                    : short for -ignore_list
          -ignore_missing        : only compare overlapping files
                                   If different files, fail.
          -no_diffs              : only compare existence of files
          -quiet                 : only list files with diffs
          -save                  : save actual file differences (txt and pdf)
          -show                  : show actual file differences
          -skip_data             : skip binary diff of select data files
                                   (.BRIK, .dcm, .BRIK.gz)
          -verb LEVEL            : set verbosity level (0,1,2)
                                   (default 1)
          -xxdiff                : use xxdiff to show diffs
          -X                     : implies -xxdiff -ignore_missing
    
    
       ----------------------------------------------------------------------
       examples:
    
       ----------------------------------------------------------------------
       R Reynolds    written ages ago, but added 10 Jun, 2015
       ----------------------------------------
