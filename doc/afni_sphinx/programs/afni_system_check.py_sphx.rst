********************
afni_system_check.py
********************

.. _afni_system_check.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    =============================================================================
    afni_system_check.py    - perform various system checks
    
    This program is intended to be helpful for figuring out AFNI installation
    issues.
    
    examples
    
       1.  afni_system_check.py -check_all
       2a. afni_system_check.py -find_prog python
       2b. afni_system_check.py -find_prog python -exact yes
    
    -----------------------------------------------------------------------------
    terminal options:
    
       -help                : show this help
       -help_dot_files      : show help on shell setup files
       -help_rc_files       : SAME
       -hist                : show program history
       -show_valid_opts     : show valid options for program
       -todo                : show current todo list
       -ver                 : show current version
    
       NOTE: either a terminal or an action option is required
    
    action options:
    
       -check_all           : perform all system checks
                              - see section, "details displayed via -check_all"
       -dot_file_list       : list all found dot files (startup files)
       -dot_file_show       : display contents of all found dot files
       -dot_file_pack NAME  : create a NAME.tgz packge containing dot files
       -find_prog PROG      : search PATH for PROG
                              - default is *PROG*, case-insensitive
                              - see also -casematch, -exact
    
    other options:
    
       -casematch yes/no    : match case in -find_prog
       -data_root DDIR      : search for class data under DDIR
       -exact yes/no        : search for PROG without wildcards in -find_prog
    
    -----------------------------------------------------------------------------
    details displayed via -check_all (just run to see):
    
       general information:
          - CPU, operating system and version, # CPUs, login shell
    
       AFNI and related tests:
          - which afni, python, R and tcsh, along with versions
          - check for multiple afni packages in PATH
          - check that various AFNI programs run
    
       python libs:
          - check that various python libraries are found and loaded
    
       path vars:
          - show some environment variables related to the PATH
    
    -----------------------------------------------------------------------------
    R Reynolds    July, 2013
    =============================================================================
