*********
quickspec
*********

.. _quickspec:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:  quickspec 
            <-tn TYPE NAME> ...
            <-tsn TYPE STATE NAME> ...
            [<-spec specfile>] [-h/-help]
      Use this spec file for quick and dirty way of 
      loading a surface into SUMA or the command line programs.
    
    Options:
     Specifying surfaces using -t* options: 
       -tn TYPE NAME: specify surface type and name.
                      See below for help on the parameters.
       -tsn TYPE STATE NAME: specify surface type state and name.
            TYPE: Choose from the following (case sensitive):
               1D: 1D format
               FS: FreeSurfer ascii format
               PLY: ply format
               MNI: MNI obj ascii format
               BYU: byu format
               SF: Caret/SureFit format
               BV: BrainVoyager format
               GII: GIFTI format
            NAME: Name of surface file. 
               For SF and 1D formats, NAME is composed of two names
               the coord file followed by the topo file
            STATE: State of the surface.
               Default is S1, S2.... for each surface.
    
       -tsnad TYPE STATE NAME ANATFLAG LDP: 
                     specify surface type, state, name, anatomical correctness, 
                     and its Local Domain Parent.
            ANATFLAG: 'Y' if surface is anatomically correct (default).
                      'N' if it is not anatomically correct.
            LDP: Name of Local Domain Parent surface.
                 Use SAME (default) if surface is its own LDP.
       -tsnadm TYPE STATE NAME ANATFLAG LDP MARKER: 
                     specify surface type, state, name, anatomical correctness, 
                     Local Domain Parent, and node marker file.
            MARKER: A niml.do Displayable Object (DO) to put at every
                    node of the surface. See @DO.examples for information
                    about displayable objects
       -tsnadl TYPE STATE NAME ANATFLAG LDP LABELDSET: 
                     specify surface type, state, name, anatomical correctness, 
                     Local Domain Parent, and a label dataset file.
            LABELDSET: A surface dataset containing node labels.
       -spec specfile: Name of spec file output.
                       Default is quick.spec
                       The program will only overwrite 
                       quick.spec (the default) spec file.
       -h or -help: This message here.
    
      You can use any combinaton of -tn and -tsn options.
      Fields in the spec file that are (or cannot) be specified
      by this program are set to default values.
    
       This program was written to ward off righteous whiners and is
      not meant to replace the venerable @SUMA_Make_Spec_XX scripts.
    
    
    Compile Date:
       Jan 29 2018
    
          Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov 
    		 Tue Dec 30
