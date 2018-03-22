********
rtfeedme
********

.. _ahelp_rtfeedme:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: rtfeedme [options] dataset [dataset ...]
    Test the real-time plugin by sending all the bricks in 'dataset' to AFNI.
     * 'dataset' may include a sub-brick selector list.
     * If more than one dataset is given, multiple channel acquisition
        will be simulated.  Each dataset must then have the same datum
        and dimensions.
     * If you put the flag '-break' between datasets, then the datasets
        in each group will be transmitted in parallel, but the groups
        will be transmitted serially (one group, then another, etc.).
        + For example:
            rtfeedme A+orig B+orig -break C+orig -break D+orig
           will send the A and B datasets in parallel, then send
           the C dataset separately, then send the D dataset separately.
           (That is, there will be 3 groups of datasets.)
        + There is a 1 second delay between the end transmission for
           a group and the start transmission for the next group.
        + You can extend the inter-group delay by using a break option
           of the form '-break_20' to indicate a 20 second delay.
        + Within a group, each dataset must have the same datum and
           same x,y,z,t dimensions.  (Different groups don't need to
           be conformant to each other.)
        + All the options below apply to each group of datasets;
           i.e., they will all get the same notes, drive commands, ....
    
    Options:
      -host sname =  Send data, via TCP/IP, to AFNI running on the
                     computer system 'sname'.  By default, uses the
                     current system, and transfers data using shared
                     memory.  To send on the current system using
                     TCP/IP, use the system 'localhost'.
    
      -dt ms      =  Tries to maintain an inter-transmit interval of
                     'ms' milliseconds.  The default is to send data
                     as fast as possible.
    
      -3D         =  Sends data in 3D bricks.  By default, sends in
                     2D slices.
    
      -buf m      =  When using shared memory, sets the interprocess
                     communications buffer to 'm' megabytes.  Has no
                     effect if using TCP/IP.  Default is m=1.
                     If you use m=0, then a 50 Kbyte buffer is used.
    
      -verbose    =  Be talkative about actions.
      -swap2      =  Swap byte pairs before sending data.
    
      -nzfake nz  =  Send 'nz' as the value of nzz (for debugging).
    
      -drive cmd  =  Send 'cmd' as a DRIVE_AFNI command; e.g.,
                       -drive 'OPEN_WINDOW A.axialimage'
                     If cmd contains blanks, it must be in 'quotes'.
                     Multiple -drive options may be used.
    
      -note sss   =  Send 'sss' as a NOTE to the realtime plugin.
                     Multiple -note options may be used.
    
      -gyr v      =  Send value 'v' as the y-range for realtime motion
