**********
plugout_tt
**********

.. _plugout_tt:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: plugout_tt [-host name] [-v]
    This program connects to AFNI and receives notification
    whenever the user changes Talairach coordinates.
    
    Options:
      -host name  Means to connect to AFNI running on the
                    computer 'name' using TCP/IP.  The default is to
                    connect on the current host using shared memory.
      -ijk        Means to get voxel indices from AFNI, rather
                    than Talairach coordinates.
      -v          Verbose mode: prints out lots of stuff.
      -port pp    Use TCP/IP port number 'pp'.  The default is
                    8001, but if two copies of this are running on
                    the same computer, they must use different ports.
                    For a list of currently used ports use afni -list_ports
      -name sss   Use the string 'sss' for the name that AFNI assigns
                    to this plugout.  The default is something stupid.
    
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
       -npb PORT_OFFSET_BLOC: Simliar to -np, except it is easier to use.
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
    
