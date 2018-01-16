*************
uber_ttest.py
*************

.. _uber_ttest.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    ===========================================================================
    uber_ttest.py      - GUI for group ttest
    
            usage:  uber_ttest.py
    
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
    
          uber_ttest.py
          uber_ttest.py -qt_opts -style=motif
    
       Informational examples:
    
          uber_ttest.py -help
          uber_ttest.py -help_gui
          uber_ttest.py -hist
          uber_ttest.py -show_valid_opts
          uber_ttest.py -ver
    
       Non-GUI examples (all have -no_gui):
    
          uber_ttest.py -no_gui -print_script               \
             -dsets_A $ddir/OLSQ.*.HEAD
    
          uber_ttest.py -no_gui -save_script cmd.ttest      \
            -mask mask+tlrc                                 \
            -set_name_A vrel                                \
            -set_name_B arel                                \
            -dsets_A REML.*.HEAD                            \
            -dsets_B REML.*.HEAD                            \
            -beta_A 0                                       \
            -beta_B 2                                       \
            -results_dir ''
    
          Note that the 3dMEMA command should have t-stat indices as well.
    
          uber_ttest.py -no_gui -save_script cmd.MEMA       \
            -program 3dMEMA                                 \
            -mask mask+tlrc                                 \
            -set_name_A vrel                                \
            -set_name_B arel                                \
            -dsets_A REML.*.HEAD                            \
            -dsets_B REML.*.HEAD                            \
            -beta_A 0 -tstat_A 1                            \
            -beta_B 2 -tstat_B 3                            \
            -results_dir ''
    
    ----------------------------------------------------------------------
    
    - R Reynolds  Aug, 2011
    ===========================================================================
