.. _ahelp_@AfniEnv:

********
@AfniEnv
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Script to set an AFNI environment variable in your afni resource file 
    
    Usage: @AfniEnv <-set NAME VALUE> [<-unset NAME>]
    
       -set  NAME VALUE: Set environment variable NAME  to value VALUE
       -get  NAME: Get the value (same as apsearch -Vname option)
       -unset NAME : The opposite of -set
       -help: this message
    
    Note that this script only modifies the contents of your .afnirc
       file which is determined to be: /home/ptaylor/.afnirc
    
    See also:
       apsearch -afni_rc_file
       apsearch -view_readme env
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
