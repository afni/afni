********************
realtime_receiver.py
********************

.. _realtime_receiver.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    =============================================================================
    realtime_receiver.py - program to receive and display real-time plugin data
    
       This program receives motion parameters and optionally ROI averages
       or voxel data each TR from the real-time plugin to afni.  Which data
       will get sent is controlled by the real-time plugin.  All data is
       sent as floats.
    
       Motion parameters: 6 values per TR
       ROI averages:      N values per TR, where N is the number of ROIs
       All voxel data:    8 values per voxel per TR (might be a lot of data!)
                            The 8 values include voxel index, 3 ijk indices,
                            the 3 xyz coordinates, and oh yes, the data
    
       Examples:
    
         1a. Run in test mode to display verbose data on the terminal window.
    
            realtime_receiver.py -show_data yes
    
         1b. Run in test mode to just display motion to the terminal.
    
            realtime_receiver.py -write_text_data stdout
    
         1c. Write all 'extra' parameters to file my_data.txt, one set
             per line.
    
            realtime_receiver.py -write_text_data my_data.txt \
                                 -data_choice all_extras
    
         2. Provide a serial port, sending the Euclidean norm of the motion params.
    
            realtime_receiver.py -show_data yes -serial_port /dev/ttyS0  \
                                 -data_choice motion_norm
    
         3. Run a feedback demo.  Assume that the realtime plugin will send 2
            values per TR.  Request the receiver to plot (a-b)/(a+b), scaled
            to some small integral range.
    
            realtime_receiver.py -show_demo_gui yes -data_choice diff_ratio
    
         4. Adjust the defaults of the -data_choice diff_ratio parameters from
            those for AFNI_data6/realtime.demos/demo.2.fback.1.receiver, to those
            for the s620 demo:
            
            realtime_receiver.py -show_demo_gui yes -data_choice diff_ratio                              -dc_params 0.008 43.5
    
       TESTING NOTE:
    
            This following setup can be tested off-line using Dimon, afni and this
            realtime_receiver.py program.  Note that while data passes from Dimon
            to afni to realtime_receiver.py, the programs essentially should be
            started in the reverse order (so that the listener is always ready for
            the talker, say).
    
            See the sample scripts:
    
                 AFNI_data6/realtime.demos/demo.2.fback.*
    
            step 1. start the receiver: demo.2.fback.1.receiver
    
                 realtime_receiver.py -show_data yes -show_demo_gui yes \
                                      -data_choice diff_ratio
    
            step 2. start realtime afni: demo.2.fback.2.afni
    
                 Note: func_slim+orig is only loaded to ensure a multiple
                       volume overlay dataset, so that the rtfeedme command
                       "DRIVE_AFNI SET_SUBBRICKS 0 1 1" finds sub-brick 1.
    
                 # set many REALTIME env vars or in afni's realtime plugin
                 setenv AFNI_REALTIME_Registration  3D:_realtime
                 setenv AFNI_REALTIME_Base_Image    2
                 setenv AFNI_REALTIME_Graph         Realtime
                 setenv AFNI_REALTIME_MP_HOST_PORT  localhost:53214
                 setenv AFNI_REALTIME_SEND_VER      YES
                 setenv AFNI_REALTIME_SHOW_TIMES    YES
                 setenv AFNI_REALTIME_Mask_Vals     ROI_means
                 setenv AFNI_REALTIME_Function      FIM
    
                 cd ../afni
                 afni -rt -yesplugouts                     \
                      -com "SWITCH_UNDERLAY epi_r1+orig"   \
                      -com "SWITCH_OVERLAY func_slim+orig" &
    
                 # at this point, the user should open a graph window and:
                 #    FIM->Ignore->2 
                 #    FIM->Pick Ideal->epi_r1_ideal.1D
    
            step 3. feed data to afni (can be repeated): demo.2.fback.3.feedme
    
                 cd ../afni
                 set episet  = epi_r1+orig
                 set maskset = mask.left.vis.aud+orig
    
                 plugout_drive -com "SETENV AFNI_REALTIME_Mask_Dset $maskset" -quit
    
                 rtfeedme                                                        \
                   -drive 'DRIVE_AFNI OPEN_WINDOW axialimage geom=285x285+3+533' \
                   -drive 'DRIVE_AFNI OPEN_WINDOW axialgraph keypress=A'         \
                   -drive 'DRIVE_AFNI SET_SUBBRICKS 0 1 1'                       \
                   -drive 'DRIVE_AFNI SET_DICOM_XYZ 52 4 12'                     \
                   -drive 'DRIVE_AFNI SET_FUNC_RANGE 0.9'                        \
                   -drive 'DRIVE_AFNI SET_THRESHNEW 0.4'                         \
                   -dt 200 -3D $episet
    
    
       COMMUNICATION NOTE:
    
            This program listens for connections at TCP port 53214, unless an
            alternate port is specified.  The real-time plugin (or some other
            program) connects at that point, opening a new data socket.  There
            is a "handshake" on the data socket, and then data is recieved until
            a termination signal is received (or the socket goes bad).
    
            Data is sent per run, meaning the connection should be terminated
            and restarted at the end of each run.
    
            The handshake should be the first data on the data socket (per run).
            The real-time plugin (or other program) will send the hello bytes:
            0xabcdefab, where the final byte may be incremented by 0, 1 or 2
            to set the version number, e.g. use 0xabcdefac for version 1.
    
               Version 0: only motion will be sent
               Version 1: motion plus N ROI averages will be sent
               Version 2: motion plus all voxel data for N voxels will be sent
    
            If the version is 1 or 2, the 4-byte handshake should be followed
            by a 4-byte integer, specifying the value of N.  Hence, the 
            combination of the version number and any received N will determine
            how much data will be sent to the program each TR.
    
            At the end of the run, the sending program should send the 4-byte
            good-bye sequence: 0xdeaddead.
    
       This program is based on the structure of serial_helper, but because
       it is meant as a replacement, it will have different options.
    
       ------------------------------------------
       Options:
    
       terminal options:
    
          -help                     : show this help
          -hist                     : show module history
          -show_valid_opts          : list valid options
          -ver                      : show current version
    
       other options
          -data_choice CHOICE       : pick which data to send as feedback
                       motion       : send the 6 motion parameters
                       motion_norm  : send the Euclidean norm of them
                       all_extras   : send all 'extra' values (ROI or voxel values)
                       diff_ratio   :  (a-b)/(abs(a)+abs(b)) for 2 'extra' values
             * To add additional CHOICE methods, see the function compute_TR_data().
          -dc_params P1 P2 ...      : set data_choice parameters
                                      e.g. for diff_ratio, parmas P1 P2
                                         P1 = dr low limit, P2 = scalar -> [0,1]
                                         result is (dr-P1)*P2  {applied in [0,1]}
          -serial_port PORT         : specify serial port file for feedback data
          -show_comm_times          : display communication times
          -show_data yes/no         : display incoming data in terminal window
          -show_demo_data           : display feedback data in terminal window
          -show_demo_gui            : demonstrate a feedback GUI
          -swap                     : swap bytes incoming data
          -tcp_port PORT            : specify TCP port for incoming connections
          -verb LEVEL               : set the verbosity level
          -write_text_data FNAME    : write data to text file 'FNAME'
    
    -----------------------------------------------------------------------------
    R Reynolds    July 2009
    =============================================================================
