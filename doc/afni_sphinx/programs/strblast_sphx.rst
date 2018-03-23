.. _ahelp_strblast:

********
strblast
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: strblast [options] TARGETSTRING filename ...
    Finds exact copies of the target string in each of
    the input files, and replaces all characters with
    some junk string.
    
    options:
    
      -help              : show this help
    
      -new_char CHAR     : replace TARGETSTRING with CHAR (repeated)
    
          This option is used to specify what TARGETSTRING is
          replaced with.  In this case, replace it with repeated
          copies of the character CHAR.
    
      -new_string STRING : replace TARGETSTRING with STRING
    
          This option is used to specify what TARGETSTRING is
          replaced with.  In this case, replace it with the string
          STRING.  If STRING is not long enough, then CHAR from the
          -new_char option will be used to complete the overwrite
          (or the character 'x', by default).
    
      -unescape          : parse TARGETSTRING for escaped characters
                           (includes '\t', '\n', '\r')
    
          If this option is given, strblast will parse TARGETSTRING
          replacing any escaped characters with their encoded ASCII
          values.
    
      -quiet : Do not report files with no strings found.
               use -quiet -quiet to avoid any reporting.
    
    Examples:
      strings I.001 | more # see if Subject Name is present
      strblast 'Subject Name' I.*
    
      strblast -unescape "END OF LINE\n"       infile.txt
      strblast -new_char " " "BAD STRING"      infile.txt
      strblast -new_string "GOOD" "BAD STRING" infile.txt
    
    Notes and Warnings:
      * strblast will modify the input files irreversibly!
          You might want to test if they are still usable.
      * strblast reads files into memory to operate on them.
          If the file is too big to fit in memory, strblast
          will fail.
      * strblast  will do internal wildcard expansion, so
          if there are too many input files for your shell to
          handle, you can do something like
             strblast 'Subject Name' 'I.*'
