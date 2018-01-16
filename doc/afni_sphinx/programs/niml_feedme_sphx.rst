***********
niml_feedme
***********

.. _niml_feedme:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: niml_feedme [options] dataset
    
    * Sends volumes from the dataset to AFNI via the NIML socket interface.
    * You must run AFNI with the command 'afni -niml' so that the program
      will be listening for the socket connection.
    * Inside AFNI, the transmitted dataset will be named 'niml_feedme'.
    * For another way to send image data to AFNI, see progam rtfeedme.
    * At present, there is no way to attach statistical parameters to
      a transmitted volume.
    * This program sends all volumes in float format, simply because
      that's easy for me.  But you can also send byte, short, and
      complex valued volumes.
    * This program is really just a demo; it has little practical use.
    
    OPTIONS:
      -host sname =  Send data, via TCP/IP, to AFNI running on the
                     computer system 'sname'.  By default, uses the
                     current system (localhost), if you don't use this
                     option.
    
      -dt ms      =  Tries to maintain an inter-transmit interval of 'ms'
                     milliseconds.  The default is 1000 msec per volume.
    
      -verb       =  Be (very) talkative about actions.
    
      -accum      =  Send sub-bricks so that they accumulate in AFNI.
                     The default is to create only a 1 volume dataset
                     inside AFNI, and each sub-brick just replaces
                     that one volume when it is received.
    
      -target nam =  Change the dataset name transmitted to AFNI from
                     'niml_feedme' to 'nam'.
    
      -drive cmd  =  Send 'cmd' as a DRIVE_AFNI command.
                    * If cmd contains blanks, it must be in 'quotes'.
                    * Multiple -drive options may be used.
                    * These commands will be sent to AFNI just after
                      the first volume is transmitted.
                    * See file README.driver for a list of commands.
    
    EXAMPLE: Send volumes from a 3D+time dataset to AFNI:
    
      niml_feedme -dt 1000 -verb -accum -target Elvis \
                  -drive 'OPEN_WINDOW axialimage'     \
                  -drive 'OPEN_WINDOW axialgraph'     \
                  -drive 'SWITCH_UNDERLAY Elvis'      \
                  timeseries+orig
    
    Author: RW Cox -- July 2009
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
