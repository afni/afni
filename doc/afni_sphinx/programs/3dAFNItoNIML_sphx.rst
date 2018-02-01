************
3dAFNItoNIML
************

.. _3dAFNItoNIML:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAFNItoNIML [options] dset
     Dumps AFNI dataset header information to stdout in NIML format.
     Mostly for debugging and testing purposes!
    
     OPTIONS:
      -data          == Also put the data into the output (will be huge).
      -ascii         == Format in ASCII, not binary (even huger).
      -tcp:host:port == Instead of stdout, send the dataset to a socket.
                        (implies '-data' as well)
    
    -- RWCox - Mar 2005
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
