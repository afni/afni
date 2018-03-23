.. _ahelp_plugout_drive:

*************
plugout_drive
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: plugout_drive [-host name] [-v]
    
    This program connects to AFNI and sends commands
     that the user specifies interactively or on command line
     over to AFNI to be executed.
    
    NOTE:
     If you quit plugout_drive and then re-start it immediately
     (as in a script), you might run into problems re-connecting
     to AFNI. The reason is that the TCP/IP system doesn't hang
     up a socket instantly when commanded to do so; the socket
     takes about a second to close down completely. If you are
     writing a script that starts plugout_drive repeatedly, you
     should insert a command 'sleep 1' between each start, to
     give the operating system time to clean the socket up.
     Otherwise, AFNI might not be able to open the socket,
     and plugout_drive will output an error message:
       ** AFNI didn't like control information!
    
    OPTIONS:
      -host name    Means to connect to AFNI running on the computer
                    'name' using TCP/IP.  The default is to connect
                    on the current host 'localhost' using TCP/IP.
    
      -shm          Means to connect to the current host using shared
                    memory.  There is no reason to do this unless
                    you are transferring huge quantities of data.
                    N.B.:  '-host .' is equivalent to '-shm'.
    
      -v            Verbose mode.
    
      -port pp      Use TCP/IP port number 'pp'.  The default is
                    8099, but if two plugouts are running on the
                    same computer, they must use different ports.
                    For a list of currently used ports use afni -list_ports
    
      -maxwait t    Wait a maximum of 't' seconds for AFNI to connect;
                    if the connection doesn't happen in that time, exit.
                    [default wait time is 9 seconds]
    
      -name sss     Use the string 'sss' for the name that AFNI assigns
                    to this plugout.  The default is something stupid.
    
      -com 'ACTION DATA'  Execute the following command. For example:
                           -com 'SET_FUNCTION SomeFunction'
                           will switch AFNI's function (overlay) to
                           dataset with prefix SomeFunction. 
                          Make sure ACTION and DATA are together enclosed
                           in one pair of single quotes.
                          There are numerous actions listed in AFNI's
                           README.driver file.
                          You can use the option -com repeatedly. 
    
      -quit         Quit after you are done with all the -com commands.
                    The default is for the program to wait for more
                    commands to be typed at the terminal's prompt.
    
    NOTES:
    You will need to turn plugouts on in AFNI using one of the
    following methods: 
     1. Including '-yesplugouts' as an option on AFNI's command line
     2. From AFNI GUI: Define Datamode->Misc->Start Plugouts
     3. From AFNI GUI: Press the 'NIML+PO' button (near 'Overlay')
     4. Set environment variable AFNI_YESPLUGOUTS to YES in .afnirc
    Otherwise, AFNI won't be listening for a plugout connection.
      [AFNI does't listen for socket connections, unless]
      [it is told to,  in order to avoid the overhead of]
      [checking for incoming data every few milliseconds]
    
    This program's exit status will be 1 if it couldn't connect
    to AFNI at all.  Otherwise, the exit status will be 0.
    You could use this feature in a script to check if a copy of
    AFNI is ready to rumble, and if not then start one, as in the
    following csh fragment:
        plugout_drive -maxwait 1 -com 'OPEN_WINDOW axialimage'
        if( $status == 1 )then
          afni -yesplugouts &
          sleep 2 ; plugout_drive -com 'OPEN_WINDOW axialimage'
        endif
    
    To have different plugout_* programs talking to different
    AFNI, use the -np* options below
       -np PORT_OFFSET: Provide a port offset to allow multiple instances of
                        AFNI <--> SUMA, AFNI <--> 3dGroupIncorr, or any other
                        programs that communicate together to operate on the same
                        machine. 
                        All ports are assigned numbers relative to PORT_OFFSET.
             The same PORT_OFFSET value must be used on all programs
               that are to talk together. PORT_OFFSET is an integer in
               the inclusive range [1025 to 65500]. 
             When you want to use multiple instances of communicating programs, 
               be sure the PORT_OFFSETS you use differ by about 50 or you may
               still have port conflicts. A BETTER approach is to use -npb below.
       -npq PORT_OFFSET: Like -np, but more quiet in the face of adversity.
       -npb PORT_OFFSET_BLOC: Similar to -np, except it is easier to use.
                              PORT_OFFSET_BLOC is an integer between 0 and
                              MAX_BLOC. MAX_BLOC is around 4000 for now, but
                              it might decrease as we use up more ports in AFNI.
                              You should be safe for the next 10 years if you 
                              stay under 2000.
                              Using this function reduces your chances of causing
                              port conflicts.
    
             See also afni and suma options: -list_ports and -port_number for 
                information about port number assignments.
    
             You can also provide a port offset with the environment variable
                AFNI_PORT_OFFSET. Using -np overrides AFNI_PORT_OFFSET.
    
       -max_port_bloc: Print the current value of MAX_BLOC and exit.
                       Remember this value can get smaller with future releases.
                       Stay under 2000.
       -max_port_bloc_quiet: Spit MAX_BLOC value only and exit.
       -num_assigned_ports: Print the number of assigned ports used by AFNI 
                            then quit.
       -num_assigned_ports_quiet: Do it quietly.
    
         Port Handling Examples:
         -----------------------
             Say you want to run three instances of AFNI <--> SUMA.
             For the first you just do: 
                suma -niml -spec ... -sv ...  &
                afni -niml &
             Then for the second instance pick an offset bloc, say 1 and run
                suma -niml -npb 1 -spec ... -sv ...  &
                afni -niml -npb 1 &
             And for yet another instance:
                suma -niml -npb 2 -spec ... -sv ...  &
                afni -niml -npb 2 &
             etc.
    
             Since you can launch many instances of communicating programs now,
                you need to know wich SUMA window, say, is talking to which AFNI.
                To sort this out, the titlebars now show the number of the bloc 
                of ports they are using. When the bloc is set either via 
                environment variables AFNI_PORT_OFFSET or AFNI_PORT_BLOC, or  
                with one of the -np* options, window title bars change from 
                [A] to [A#] with # being the resultant bloc number.
             In the examples above, both AFNI and SUMA windows will show [A2]
                when -npb is 2.
    
    
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
       -overwrite: Overwrite existing output dataset.
                   Equivalent to setting env. AFNI_DECONFLICT=OVERWRITE
       -ok_1D_text: Zero out uncommented text in 1D file.
                    Equivalent to setting env. AFNI_1D_ZERO_TEXT=YES
       -Dname=val: Set environment variable 'name' to value 'val'
                 For example: -DAFNI_1D_ZERO_TEXT=YES
       -Vname=: Print value of environment variable 'name' to stdout and quit.
                This is more reliable that the shell's env query because it would
                include envs set in .afnirc files and .sumarc files for SUMA
                programs.
                 For example: -VAFNI_1D_ZERO_TEXT=
       -skip_afnirc: Do not read the afni resource (like ~/.afnirc) file.
       -pad_to_node NODE: Output a full dset from node 0 to MAX_NODE-1
                       ** Instead of directly setting NODE to an integer you 
                          can set NODE to something like:
                       ld120 (or rd17) which sets NODE to be the maximum 
                          node index on an Icosahedron with -ld 120. See 
                          CreateIcosahedron for details.
                       d:DSET.niml.dset which sets NODE to the maximum node found
                          in dataset DSET.niml.dset.
                       ** This option is for surface-based datasets only.
                          Some programs may not heed it, so check the output if
                          you are not sure.
       -pif SOMETHING: Does absolutely nothing but provide for a convenient
                       way to tag a process and find it in the output of ps -a
       -echo_edu: Echos the entire command line to stdout (without -echo_edu)
                  for edification purposes
    
    Example 1:
        afni -yesplugouts
        plugout_drive  -com 'SWITCH_SESSION A.afni'                       \
                       -com 'OPEN_WINDOW A.axialimage geom=600x600+416+44 \
                             ifrac=0.8 opacity=9'                         \
                       -com 'OPEN_WINDOW A.sagittalimage geom=+45+430     \
                             ifrac=0.8 opacity=9'                         \
                       -com 'SWITCH_UNDERLAY anat'                        \
                       -com 'SWITCH_OVERLAY strip'                        \
                       -com 'SEE_OVERLAY +'                               \
                       -com 'SET_DICOM_XYZ 7 12 2'                        \
                       -com 'OPEN_WINDOW A.axialimage keypress=v'         \
                       -quit             
    
    More help in: README.driver
    More Demos is: @DriveAfni
