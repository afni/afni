.. contents:: 
    :depth: 4 

******
3dcopy
******

.. code-block:: none

    Usage 1: 3dcopy [-verb] [-denote] old_prefix new_prefix
      Will copy all datasets using the old_prefix to use the new_prefix;
    
        3dcopy fred ethel
    
      will copy   fred+orig.HEAD    to ethel+orig.HEAD
                  fred+orig.BRIK    to ethel+orig.BRIK
                  fred+tlrc.HEAD    to ethel+tlrc.HEAD
                  fred+tlrc.BRIK.gz to ethel+tlrc.BRIK.gz
    
    Usage 2: 3dcopy old_prefix+view new_prefix
      Will copy only the dataset with the given view (orig, acpc, tlrc).
    
    Usage 3: 3dcopy old_dataset new_prefix
      Will copy the non-AFNI formatted dataset (e.g., MINC, ANALYZE, CTF)
      to the AFNI formatted dataset with the given new prefix.
    
    
    Notes:
    * This is to copy entire datasets, possibly with multiple views.
      So sub-brick selection is not allowed.  Please use 3dbucket or
      3dTcat for that purpose.
    
    * The new datasets have new ID codes.  If you are renaming
       multiple datasets (as in Usage 1), then if the old +orig
       dataset is the warp parent of the old +acpc and/or +tlrc
       datasets, then the new +orig dataset will be the warp
       parent of the new +acpc and +tlrc datasets.  If any other
       datasets point to the old datasets as anat or warp parents,
       they will still point to the old datasets, not these new ones.
    
    * The BRIK files are copied if they exist, keeping the compression
       suffix unchanged (if any).
    
    * The old_prefix may have a directory name attached in front,
       as in 'gerard/manley/hopkins'.
    
    * If the new_prefix does not have a directory name attached
       (i.e., does NOT look like 'homer/simpson'), then the new
       datasets will be written in the current directory ('./').
    
    * The new can JUST be a directory now (like the Unix
       utility 'cp'); in this case the output has the same prefix
       as the input.
    
    * The '-verb' option will print progress reports; otherwise, the
       program operates silently (unless an error is detected).
    
    * The '-denote' option will remove any Notes from the file.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
