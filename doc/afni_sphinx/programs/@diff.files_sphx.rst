.. _ahelp_@diff.files:

***********
@diff.files
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
       ----------------------------------------------------------------------
       @diff.files - show file differences (between "these" files and "those" files)
    
          Given:
             - a list of files
             - a directory name
    
          Show files that differ (and/or their differences) between each file
          in the given list and its corresponding file in the other directory.
    
          This is similar to @diff.tree, except that one main input is a list
          of files.
    
       ----------------------------------------------------------------------
       usage: @diff.files [options] file1 file2 ... old_dir
    
       ----------------------------------------------------------------------
       options:
          -diff_opts 'OPTS'  : add options to diff command
                               e.g. -diff_opts -w
          -diff_prog DPROG   : display diffs using DPROG (probably graphical)
                               e.g. -diff_opts meld
                               e.g. -diff_opts xxdiff    [same as -xxdiff]
                               Consider also: kdiff3, tkdiff.
          -ignore_missing    : continue even if files are missing
                               alt: -im
          -longlist          : instead of listing file, run 'ls -l' on both
                               alt: -ll
          -save              : create pdfs of diffs
          -show              : show diffs using 'diff'
          -xxdiff            : show diffs using 'xxdiff'
          -X                 : implies -xxdiff and -ignore_missing'
    
       ----------------------------------------------------------------------
       examples:
    
          @diff.files file1             some/other/directory
          @diff.files file1 file2 file3 some/other/directory
          @diff.files *                 some/other/directory
    
          @diff.files -im *             some/other/directory
          @diff.files -X  *             some/other/directory
    
       ----------------------------------------------------------------------
       R Reynolds    written ages ago, but added 10 Jun, 2015
       ----------------------------------------
