********
1dAstrip
********

.. _1dAstrip:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 1dAstrip < input > output
    
    This very simple program strips non-numeric characters
    from a file, so that it can be processed by other AFNI
    1d programs.  For example, if your input is
      x=3.6 y=21.6 z=14.2
    then your output would be
        3.6   21.6   14.2
    
    * Non-numeric characters are replaced with blanks.
    * The letter 'e' is preserved if it is preceeded
      or followed by a numeric character.  This is
      to allow for numbers like '1.2e-3'.
    * Numeric characters, for the purpose of this
      program, are defined as the digits '0'..'9',
      and '.', '+', '-'.
    * The program is simple and can easily end up leaving
      undesired junk characters in the output.  Sorry.
    * This help string is longer than the rest of the
