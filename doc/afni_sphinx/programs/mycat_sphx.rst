*****
mycat
*****

.. _ahelp_mycat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    mycat fileA ...
    
    Copies text files to stdout, like the system 'cat', but with changes:
    * To copy stdin, you must use '-' for a filename
    * Microsoft end-of-line characters are changed to Unix format
    * Because of the above, mycat should only be used with text files!
