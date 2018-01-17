*****************
@DeblankFileNames
*****************

.. _@DeblankFileNames:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    A script to remove blanks and other annoying characters from filenames.
     in the current directory.
    The default set of characters to replace is ' []()'
    Spaces are replaced with _. 
    If resultant name exists, more _ are used until new name
    is found.
    
       @DeblankFileNames [-move] [FILES]
    
    OPTIONS
       -dry_run: Just show what would be done. Don't rename files.
                 This is the default option
       -move: Actually rename the files (opposite of -dry_run)
       -nobrac: Do not replace () and [] in filenames, just spaces
       -demo_set: Create a toy directory with bad names for testing.
       -echo: Turn on script echo
       -help: This message
       FILES: Specify files to fix as opposed to letting it fix all
              the names in the current directory.
    
    Examples:
       1- @DeblankFileNames 
    
       2- @DeblankFileNames -move 
    
       3- Run the command below and follow its suggestions
          @DeblankFileNames -demo_set
