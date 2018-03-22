*******
balloon
*******

.. _ahelp_balloon:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    References (please cite both):
    
    THEORETICAL MODEL:
    RB Buxton, EC Wong, LR Frank. Dynamics of blood flow and oxygenation changes
    during brain activation: the balloon model. Magnetic Resonance in Medicine
    39(6):855-864 (1998).
    
    PRACTICAL IMPLEMENTATION:
    MK Belmonte. In preparation - for updated reference contact belmonte@mit.edu
    
    USAGE: balloon TR N event_times [ t_rise t_sustain t_fall ]
    TR: scan repetition time in seconds
    	(the output curve will be sampled at this interval)
    N: number of scans (the output curve will comprise this number of samples)
    event_times: The name of a file containing the event timings, in seconds, as
    	ASCII strings separated by white space, with time 0 being the time
    	at which the initial scan occurred.
    
    t_rise: haemodynamic rise time in seconds (typically between 4s and 6s)
    t_sustain: haemodynamic sustain in seconds (typically between 0s and 4s)
    t_fall: haemodynamic fall time in seconds (typically between 4s and 6s)
    	If t_rise, t_sustain, and t_fall aren't specified on the command
    	line, then the program will expect to find event-related values of
    	these parameters to the right of each entry in the event file,
    	separated by spaces: in this case each line of the event file must
    	contain exactly four numbers - the event time, the haemodynamic
    	rise time for this event, the haemodynamic sustain time for this
    	event, and the haemodynamic fall time for this event.  (These
    	event-related values could for example be made to depend on a
