*********
ParseName
*********

.. _ParseName:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:  ParseName [OPTIONS] <FName> 
    Parses filename FName into components useful for AFNI
    OPTIONS:
       -cwd: Specify the working directory, from which relative
             path is constructed. Default is the program's CWD
       -pre PRE: Change the name so that you prepend PRE to the prefix
       -app APP: Change the name so that you append APP to the prefix
    
       -out OUT: Output only one component of the parsed file name
                 By default the whole parsed filename structure is
                 displayed.
                 OUT is one of the following:
            FullName: ABSOLUTE_PATH/FName
            RelName : RELATIVE_PATH/FName
            AbsPath : ABSOLUTE_PATH/
            RelPath : RELATIVE_PATH/
            HeadName: RELATIVE_PATH/HEADNAME
            Prefix  : PREFIX
            uPrefix : USER_PATH/PREFIX
            pPrefix : RELATIVE_PATH/PREFIX
            PPrefix : ABSOLUTE_PATH/PREFIX
            *PrefixView: Append view string (if any) to all prefix options
                         listed above.
            OnDisk  : 1 if file is on disk, 0 otherwise
            FName   : Filename, no paths
            FNameNoAfniExt : File name without any AFNI extensions
                             e.g.: ParseName -out FNameNoAfniExt test.nii.gz
            trim    : Trim the name to 20 characters.
                      First the path goes, then extension, then view,
                      then characters from the left. '~' indicates clipping.
         If you want to output multiple parameters, list them all between 
         quotes with something like:
            -out 'HeadName RelPath'
    
      -outsep SEP: When outputing multiple components, use SEP as a separator
                   between them. Default is ' ', one space character
    
    Tests:
        ParseName -cwd /hello/Joe /hello/Joe/afni.c
        ParseName -cwd /hello/Joe/ /hello/Jane/afni.c
        ParseName -out Prefix something.nii
        ParseName -out uPrefixView something.nii
        ParseName -out uPrefixView something+orig
        ParseName -pre Need_ -out Prefix something.nii
        ParseName -pre Need_  something.nii'[65-88]'
        ParseName -pre Need_  something+orig.HEAD'{2-10}[4-6]'
        ParseName -pre Need_ -out HeadName  something+orig.HEAD'{2-10}[4-6]'
    
         Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov 
