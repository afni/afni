************
uber_skel.py
************

.. _uber_skel.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    ===========================================================================
    uber_skel.py      - sample uber processing program
                        (based on uber_align_test.py, version 0.2)
    
            usage:  uber_skel.py
    
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
    
          uber_skel.py
          uber_skel.py -qt_opts -style=motif
    
       Informational examples:
    
          --------------------------------------------------
          There are some programming comments available here:
    
                uber_skel.py -help_howto_program
          --------------------------------------------------
    
          uber_skel.py -help
          uber_skel.py -help_gui
          uber_skel.py -hist
          uber_skel.py -show_valid_opts
          uber_skel.py -ver
    
       Non-GUI examples (all have -no_gui):
    
          uber_skel.py -no_gui -print_script                  \
             -uvar anat FT/FT_anat+orig                       \
             -uvar epi  FT/FT_epi_r1+orig
    
          uber_skel.py -no_gui -save_script align.test        \
             -uvar anat FT/FT_anat+orig                       \
             -uvar epi  FT/FT_epi_r1+orig                     \
             -uvar epi_base 2                                 \
             -uvar epi_strip_meth 3dAutomask                  \
             -uvar align_centers yes                          \
             -uvar giant_move yes                             \
             -uvar cost ls                                    \
             -uvar multi_list lpc lpc+ lpc+ZZ lpa
    
    ----------------------------------------------------------------------
    
    - R Reynolds  May, 2011
    ===========================================================================
