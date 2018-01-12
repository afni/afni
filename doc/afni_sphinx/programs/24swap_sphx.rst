.. contents:: 
    :depth: 4 

******
24swap
******

.. code-block:: none

    Usage: 24swap [options] file ...
    Swaps bytes pairs and/or quadruples on the files listed.
    Options:
     -q            Operate quietly
     -pattern pat  'pat' determines the pattern of 2 and 4
                     byte swaps.  Each element is of the form
                     2xN or 4xN, where N is the number of
                     bytes to swap as pairs (for 2x) or
                     as quadruples (for 4x).  For 2x, N must
                     be divisible by 2; for 4x, N must be
                     divisible by 4.  The whole pattern is
                     made up of elements separated by colons,
                     as in '-pattern 4x39984:2x0'.  If bytes
                     are left over after the pattern is used
                     up, the pattern starts over.  However,
                     if a byte count N is zero, as in the
                     example below, then it means to continue
                     until the end of file.
    
     N.B.: You can also use 1xN as a pattern, indicating to
             skip N bytes without any swapping.
     N.B.: A default pattern can be stored in the Unix
             environment variable AFNI_24SWAP_PATTERN.
             If no -pattern option is given, the default
             will be used.  If there is no default, then
             nothing will be done.
     N.B.: If there are bytes 'left over' at the end of the file,
             they are written out unswapped.  This will happen
             if the file is an odd number of bytes long.
     N.B.: If you just want to swap pairs, see program 2swap.
             For quadruples only, see program 4swap.
     N.B.: This program will overwrite the input file!
             You might want to test it first.
    
     Example: 24swap -pat 4x8:2x0 fred
              If fred contains 'abcdabcdabcdabcdabcd' on input,
