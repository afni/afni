******************
uber_align_test.py
******************

.. _ahelp_uber_align_test.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    ===========================================================================
    uber_align_test.py      - generate script to test anat/EPI alignment
    
            usage:  uber_align_test.py
    
    ---
    
    This help describes only the command line options to this program, which
    enables one to:
    
            - initialize user variables (for GUI or command line)
            - initialize control variables (for GUI or command line)
            - pass PyQt4 options directly to the GUI
            - run without the GUI
    
    ----------------------------------------------------------------------
    Examples:
    
       GUI examples:
    
          uber_align_test.py
          uber_align_test.py -qt_opts -style=motif
    
       Informational examples:
    
          uber_align_test.py -help
          uber_align_test.py -help_gui
          uber_align_test.py -hist
          uber_align_test.py -show_valid_opts
          uber_align_test.py -ver
    
       Non-GUI examples (all have -no_gui):
    
          uber_align_test.py -no_gui -print_script            \
             -uvar anat FT/FT_anat+orig                       \
             -uvar epi  FT/FT_epi_r1+orig
    
          uber_align_test.py -no_gui -save_script align.test  \
             -uvar anat FT/FT_anat+orig                       \
             -uvar epi  FT/FT_epi_r1+orig                     \
             -uvar epi_base 2                                 \
             -uvar epi_strip_meth 3dAutomask                  \
             -uvar align_centers yes                          \
             -uvar giant_move yes                             \
             -uvar cost ls                                    \
             -uvar multi_list lpc lpc+ lpc+ZZ lpa
    
    ----------------------------------------------------------------------
    
    - R Reynolds  Apr, 2011
    ===========================================================================
