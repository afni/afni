.. _ahelp_1dgrayplot:

**********
1dgrayplot
**********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dgrayplot [options] tsfile
    Graphs the columns of a *.1D type time series file to the screen,
    sort of like 1dplot, but in grayscale.
    
    Options:
     -install   = Install a new X11 colormap (for X11 PseudoColor)
     -ignore nn = Skip first 'nn' rows in the input file
                    [default = 0]
     -flip      = Plot x and y axes interchanged.
                    [default: data columns plotted DOWN the screen]
     -sep       = Separate scales for each column.
     -use mm    = Plot 'mm' points
                    [default: all of them]
     -ps        = Don't draw plot in a window; instead, write it
                  to stdout in PostScript format.
                  N.B.: If you view this result in 'gv', you should
                        turn 'anti-alias' off, and switch to
                        landscape mode.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
