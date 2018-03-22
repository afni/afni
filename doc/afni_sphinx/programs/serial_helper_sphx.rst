*************
serial_helper
*************

.. _ahelp_serial_helper:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ------------------------------------------------------------
    serial_helper - pass motion parameters from socket to serial port
    
        This program is meant to receive registration (motion?)
        correction parameters from afni's realtime plugin, and to
        pass that data on to a serial port.
    
        The program is meant to run as a tcp server.  It listens
        for a connection, then processes data until a termination
        flag is received (sending data from the tcp socket to the
        serial port), closes the new connection, and goes back
        to a listening state.
    
        The basic outline is:
    
        open tcp server socket
        repeat forever:
            wait for a tcp client connection
            open a serial port
            while the client sends new data
                write that data to the serial port
            close the serial port and client socket
    
        The expected client is the realtime plugin to afni,
        plug_realtime.so.  If the afni user has their environment
        variable AFNI_REALTIME_MP_HOST_PORT set as HOST:PORT,
        then for EACH RUN, the realtime plugin will open a tcp
        connection to the given HOST and PORT, pass the magic hello
        data (0xabcdefab), pass the 6 motion parameters for each
        time point, and signal a closure by passing the magic bye
        data (0xdeaddead).
    
        On this server end, the 'repeat forever' loop will do the
        following.  First it will establish the connection by
        checking for the magic hello data.  If that data is found,
        the serial port will be opened.
    
        Then it will repeatedly check the incoming data for the
        magic bye data.  As long as that check fails, the data is
        assumed to be valid motion parameters.  And so 6 floats at a
        time are read from the incoming socket and passed to the
        serial port.
    
      usage: serial_helper [options] -serial_port FILENAME
    ------------------------------------------------------------
      examples:
    
        1. display this help :
    
            serial_helper -help
    
        2. display the module history :
    
            serial_helper -hist
    
        3. display the current version number :
    
            serial_helper -ver
    
      * 4. run normally, using the serial port file /dev/ttyS0 :
    
            serial_helper -serial_port /dev/ttyS0
    
      * 5. same as 4, but specify socket number 53214 :
    
            serial_helper -serial_port /dev/ttyS0 -sock_num 53214
    
        6. same as 5, but specify minmum and maximum bounds on
           the values :
    
            serial_helper                       \
                -serial_port /dev/ttyS0            \
                -sock_num 53214                    \
                -mp_min -12.7                      \
                -mp_max  12.7
    
        7. run the program in socket test mode, without serial
           communication, and printing all the incoming data
    
            serial_helper -no_serial -debug 3
    
        7a.run the program in socket test mode, without serial
           communication, and showing incoming via -disp_all
           (assumes real-time plugin mask has 2 voxels set)
    
            serial_helper -no_serial -disp_all 2
    
        8. same as 4, but use debug level 3 to see the parameters
           that will be passed on, and duplicate all output to the
           file, helper.output
    
           note: this command is for the t-shell, and will not work
                 under bash (for bash do the 2>&1 thingy...)
    
            serial_helper -serial_port /dev/ttyS0 -debug 3 |& tee helper.out
    
        9. same as 4, but will receive 3 extra floats per TR
    
            serial_helper -serial_port /dev/ttyS0 -num_extra 3
    
     * See 'example F' from 'Dimon -help' for a complete real-time
       testing example.
    
    ------------------------------------------------------------
      program setup:
    
        1. Start 'serial_helper' on the computer with the serial port that
           the motion parameters should be written to.  Example 3
           is the most likely case, though it might be useful to
           use example 8.
    
        2. On the computer which will be used to run 'afni -rt',
           set the environment variable AFNI_REALTIME_MP_HOST_PORT
           to the appropriate host:port pair.  See the '-sock_num'
           option below for more details.
    
           This variable can also be set in the ~/.cshrc file, or
           as part of the AFNI environment via the ~/.afnirc file.
    
        3. Start 'afni -rt'.  Be sure to request 'realtime' graphing
           of the '3D: realtime' Registration parameters.
    
        4. Start receiving data (sending it to the realtime plugin).
    
           Note that for testing purposes, I may work well to get a
           set of I-files (say, in directories 003, 023, etc.), and
           to use Imon to send not-so-real-time data to afni.  An
           example of Imon for this purpose might be:
    
               Imon -start_dir 003 -quit -rt -host localhost
    
           See 'Imon -help' for more information.
    
    ------------------------------------------------------------
     HELLO versions:
    
        The version number is computed by subtracting 0xab from the
        last byte of the HELLO string (so that the default HELLO
        string means version 0).
    
        version 0: This is the default, which means serial_helper
                   must be told what to expect from the real-time
                   plugin via -num_extra or -disp_all.
    
        version 1: A 4-byte int will follow the HELLO string.  This
                   number will be used as with -num_extra.
    
        version 2: A 4-byte int will follow the HELLO string.  This
                   number will be used as with -disp_all.
    
        These versions can change with each new HELLO string.
    
    ------------------------------------------------------------
      'required' parameter:
    
        -serial_port FILENAME : specify output serial port
                              : -serial_port /dev/ttyS0
    
            If the user is not using any of the 'special' options,
            below, then this parameter is required.
    
            The FILENAME is the device file for the serial port
            which will be used for output.
    ------------------------------
      special options (for information or testing):
    
        -help            : show this help information
    
        -hist            : show the module history
    
        -debug LEVEL     : set the debugging level to LEVEL
                         : e.g. -debug 2
                         : default is 0, max is 3
    
        -no_serial       : turn of serial port output
    
            This option is used for testing the incoming data,
            when output to a serial port is not desired.  The
            program will otherwise operate normally.
    
        -version         : show the current version number
    ------------------------------
      'normal' options:
    
        -mp_max MAX_VAL  : limit the maximum value of the MP data
                         : e.g. -mp_max 12.7
                         : default is 12.7
    
            If any incoming data is greater than this value, it will
            be set to this value.  The default of 12.7 is used to
            scale incoming floats to signed bytes.
    
        -mp_min MIN_VAL  : limit the minimum value of the MP data
                         : e.g. -mp_min -12.7
                         : default is -12.7
    
            If any incoming data is less than this value, it will
            be set to this value.  The default of -12.7 is used to
            scale incoming floats to signed bytes.
    
        -show_times      : show communication times
                         : e.g. -show_times
    
            Each time data is recived, display the current time.
            Time is at millisecond resolution, and wraps per hour.
    
        -sock_num SOCK   : specify socket number to serve
                         : e.g. -sock_num 53214
                         : default is 53214
    
            This is the socket the program will use to listen for
            new connections.  This is the socket number that should
            be provided to the realtime plugin via the environment
            variable, AFNI_REALTIME_MP_HOST_PORT.
    
            On the machine the user run afni from, that environment
            variable should have the form HOST:PORT, where a basic
            example might be localhost:53214.
    
        -num_extra NVALS : will receive NVALS extra floats per TR
                         : e.g. -num_extra 5
                         : default is 0
    
            Extra floats may arrive if, for instance, afni's RT
            plugin has a mask with 3 ROIs in it (numbered 1,2,3).
            The plugin would compute averages over each ROI per TR,
            and send that data after the MP vals.
    
            In such a case, specify '-num_extra 3', so the program
            knows 3 floats will be received after the MP data.
    
            Note that -disp_all cannot be used with -num_extra.
    
        -disp_all NVOX   : will receive NVOX*8 extra floats per TR
                         : e.g. -disp_all 5
                         : default is 0
    
            Similar to -num_extra, here the program expect data on
            a per voxel basis, not averaged over ROIs.
    
            Here the users specifies the number of voxels for which
            ALL_DATA will be sent (to serial_helper).  The 8 values
            per voxel are (still in float):
    
                index  i  j  k  x  y  z data_value
    
            Currently, serial_helper will output this inforamtion
            simply as 1 row per voxel.
    
            Note that -disp_all cannot be used with -num_extra.
    
    ------------------------------------------------------------
      Authors: R. Reynolds, T. Ross  (March, 2004)
