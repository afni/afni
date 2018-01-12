.. contents:: 
    :depth: 4 

*************
@FSlabel2dset
*************

.. code-block:: none

    @FSlabel2dset A script to take a FreeSurfer ascii label file and 
                  turn it to a SUMA dataset and a SUMA ROI
        Two datasets are written out, one assigns VAL to each node
        the other assigns the last column in FS_LABEL_FILE, presumably
        a probability value to each of the nodes.
    
    Options:
       -fs FS_LABEL_FILE: Specify the ascii label file from FreeSurfer
       -val VAL: Assign integer VAL to the nodes in FS_LABEL_FILE
                 Default is 1
       -help: This message
       -echo: Turn echo for debugging
       -keep_tmp: Don't cleanup temp files
    
    Example:
       @FSlabel2dset -fs lh.FSFILE
