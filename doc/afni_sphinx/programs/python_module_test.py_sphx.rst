.. contents:: 
    :depth: 4 

*********************
python_module_test.py
*********************

.. code-block:: none

    
    ===========================================================================
    python_module_test.py   - test the loading of python modules
    
       The default behavior of this program is to verify whether a 'standard'
       list of python modules can be loaded.  The 'standard' list amounds to
       what is needed for the python programs in AFNI.
    
       The user may specify a list of python modules to test.
    
    ------------------------------------------------------------
    examples:
    
       a. Use the default behavior to test modules in standard list.
    
          python_module_test.py
    
       b. Test a specific list of modules in verbose mode.
    
          python_module_test.py -test_modules sys os numpy scipy R wx -verb 2
    
       c. Show the python version and platform information.
    
          python_module_test.py -python_ver -platform_info
    
       d. Perform a complete test (applies commands a and c).
    
          python_module_test.py -full_test
    
    ------------------------------------------------------------
    informational options:
    
       -help                        : display this help
       -hist                        : display the modification history
       -show_valid_opts             : display all valid options (short format)
       -ver                         : display the version number
    
    ----------------------------------------
    other options:
    
       -full_test                   : perform all of the standard tests
    
          This option applies -platform_info, -python_ver and -test_defaults.
    
       -platform_info               : display system information
    
          Platform information can include the OS and version, along with the
          CPU type.
    
       -python_ver                  : display the version of python in use
    
          Show which version of python is being used by the software.
    
       -test_defaults               : test the default module list
    
          The default module list will include (hopefully) all python modules
          used by AFNI programs.
    
          Note that most programs will not need all of these python libraries.
    
        -test_modules MOD1 MOD2 ... : test the specified module list
    
          Perform the same test, but on the modules specified with this option.
    
        -verb LEVEL                 : specify a verbose level
    
    ----------------------------------------
    R Reynolds  30 Oct 2008
    ===========================================================================
