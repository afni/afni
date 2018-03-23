.. _ahelp_suma_change_spec:

****************
suma_change_spec
****************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    suma_change_spec:
     This program changes SUMA's surface specification (Spec) files.
     At minimum, the flags input and state are required.
    Available flags:
      input: Which is the SUMA Spec file you want to change.
      state: The state within the Spec file you want to change.
      domainparent: The new Domain Parent for the state within the 
    	Spec file you want to change.
      output: The name to which your new Spec file will be temporarily
    	written to. (this flag is optional, if omitted the new Spec
    	file will be temporarily written to 'input_file.change').
      remove: This flag will remove the automatically created backup.
      anatomical: This will add 'Anatomical = Y' to the selected
    	SurfaceState.
    Usage:
     This program will take the user given flags and create a spec file,
     named from the output flag or <input>.change.  It will then take
     this new spec file and overwrite the original input file.  If the -remove
     flag is not used the original input file can be found at <inputfile>.bkp.
     If the -remove is used the .bkp file will be  automatically deleted.
    
     ex. suma_change_spec -input <file> -state <statename> 
