.. contents:: 
    :depth: 4 

************
3dAFNItoNIML
************

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
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
