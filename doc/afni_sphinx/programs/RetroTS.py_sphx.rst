.. contents:: 
    :depth: 4 

**********
RetroTS.py
**********

.. code-block:: none

    
    This function creates slice-based regressors for regressing out components of
        heart rate, respiration and respiration volume per time.
    
    Windows Example:
    C:\afni\python RetroTS.py -r resp_file.dat -c card_file.dat -p 50 -n 20 -v 2
    
    Mac/Linux Example:
    /usr/afni/python RetroTS.py -r resp_file.dat -c card_file.dat -p 50 -n 20 -v 2
    
    Input
    ================================================================================
        Following are the mandatory and optional parameters that can be entered
        after RetroTS.py, each separated by a space.
    
        Mandatory:
        ----------
        :param -r: (respiration_file) Respiration data file
        :param -c: (cardiac_file) Cardiac data file
        :param -p: (phys_fs) Physiological signal sampling frequency in Hz.
        :param -n: (number_of_slices) Number of slices
        :param -v: (volume_tr) Volume TR in seconds
        Note:   These parameters are the only single-letter parameters, as they are
                mandatory and frequently typed. The following optional parameters
                must be fully spelled out.
    
        Optional:
        ---------
        :param -prefix: Prefix of output file
        ============================================================================
        :param -rvt_shifts: Vector of shifts in seconds of RVT signal.
                (default is [0:5:20])
        :param -rvt_out: Flag for writing RVT regressors
                (default is 1)
        ============================================================================
        :param -respiration_cutoff_frequency: Cut off frequency in Hz for
                respiratory lowpass filter
                (default 3 Hz)
        :param -cardiac_cutoff_frequency: Cut off frequency in Hz for
                cardiac lowpass filter
                (default 3 Hz)
        :param -cardiac_out: Flag for writing Cardiac regressors
                (default is 1)
        :param -respiration_out: Flag for writing Respiratory regressors
                (default is 1)
        ============================================================================
        :param -interpolation_style: Resampling kernel.
                (default is 'linear', see help interp1 for more options)
        :param -fir_order: Order of FIR filter.
                (default is 40)
        ============================================================================
        :param -quiet: Show talkative progress as the program runs
                (default is 1)
        :param -demo: Run demonstration of RetroTS
                (default is 0)
        :param -show_graphs:
        ============================================================================
        :param -slice_offset: Vector of slice acquisition time offsets in seconds.
                (default is equivalent of alt+z)
        :param -slice_major: ? (default is 1)
        :param -slice_order: Slice timing information in seconds. The default is
               alt+z. See 3dTshift help for more info.
                   alt+z    = alternating in the plus direction
                   alt-z    = alternating in the minus direction
                   seq+z    = sequential in the plus direction
                   seq-z    = sequential in the minus direction
                   custom   = allows the program to use the values stored in the
                                -slice_offset list
                   filename = read temporal offsets from 'filename', including file
                                extension; e.g. slice_file.dat
                                (expecting a 1D / text file containing the times for
                                each slice in seconds)
        ============================================================================
        :param -zero_phase_offset:
        ============================================================================
        :param legacy_transform: Important-this will specify whether you use the
               original Matlab code's version (1) or the potentially bug-corrected
               version (0) for the final phase correction in
               lib_RetroTS/RVT_from_PeakFinder.py
               (default is 0)
    
    Output:
    ================================================================================
        Files saved to same folder based on selection for "-respiration_out" and
        "-cardiac_out". If these options are enabled, than the data will be written
        to a single output file based on the filename assigned to the
        option "-prefix".
    
        Example:
        C:\afni\python RetroTS.py -r resp_file.dat -c card_file.dat -p 50 -n 20
            -v 2 -prefix subject12_regressors -respiration_out 1 -cardiac_out 1
    
            Output:
            The file "subject12_regressors.slibase.1D" will be saved to current
            directory, including respiratory regressors and cardiac regressors.
    
