.. contents:: 
    :depth: 4 

*****
nicat
*****

.. code-block:: none

    Usage: nicat [-reopen rr] [-rR] streamspec
    Copies stdin to the NIML stream, which will be opened
    for writing.
    
    -reopen rr == reopen the stream after connection
                   to the stream specified by 'rr'
    -r         == Copy the stream to stdout instead; the
                   'streamspec' will be opened for reading.
    -R         == Read the stream but don't copy to stdout.
    
    Intended for testing other programs that use NIML for
    various services.  Example:
      aiv -p 4444 &
      im2niml zork.jpg | nicat tcp:localhost:4444
    Starts aiv listening on TCP/IP port 444, then sends image
