************
@ScriptCheck
************

.. _ahelp_@ScriptCheck:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @ScriptCheck [-clean] [-suffix SUFF] <Script1> [Script2 ...]
    
    Checks script(s) for improperly terminated lines
       -clean: Clean bad line breaks
       -suffix SUFF: Rename uncleaned file Script1.SUFF
                     The default for SUFF is .uncln
    
    Example:
       echo "A good line" > ./___toy
       echo "A good break \" >> ./___toy
       echo "A harmless \ slash" >> ./___toy
       echo "A bad break \  " >> ./___toy
       echo "The end" >> ./___toy
    
    To find the bad line breaks
       @ScriptCheck ___toy
    
    To find and clean the bad line breaks
       @ScriptCheck -clean ___toy
    
    The uncleaned (original) file goes into ___toy.uncln
    
    Use file_tool -show_file_type -infiles YOURFILE
    To check for non-printable characters, and a whole lot more.
