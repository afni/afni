********
3dbucket
********

.. _ahelp_3dbucket:

.. contents:: 
    :depth: 4 

| 


Concatenate sub-bricks from input datasets into one big 'bucket' dataset.
=========================================================================

.. code-block:: none

    Usage: 3dbucket options

where the options are:
======================

.. code-block:: none

         -prefix pname = Use 'pname' for the output dataset prefix name.
     OR  -output pname     [default='buck']
    
         -session dir  = Use 'dir' for the output dataset session directory.
                           [default='./'=current working directory]
         -glueto fname = Append bricks to the end of the 'fname' dataset.
                           This command is an alternative to the -prefix 
                           and -session commands.
                         * Note that fname should include the view, as in
                             3dbucket -glueto newset+orig oldset+orig'[7]'
         -aglueto fname= If fname dset does not exist, create it (like -prefix).
                         Otherwise append to fname (like -glueto).
                         This option is useful when appending in a loop.
                         * As with -glueto, fname should include the view, e.g.
                             3dbucket -aglueto newset+orig oldset+orig'[7]'
         -dry          = Execute a 'dry run'; that is, only print out
                           what would be done.  This is useful when
                           combining sub-bricks from multiple inputs.
         -verb         = Print out some verbose output as the program
                           proceeds (-dry implies -verb).
         -fbuc         = Create a functional bucket.
         -abuc         = Create an anatomical bucket.  If neither of
                           these options is given, the output type is
                           determined from the first input type.
    
    Command line arguments after the above are taken as input datasets.
    A dataset is specified using one of these forms:
       'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.
    You can also add a sub-brick selection list after the end of the
    dataset name.  This allows only a subset of the sub-bricks to be
    included into the output (by default, all of the input dataset
    is copied into the output).  A sub-brick selection list looks like
    one of the following forms:
      fred+orig[5]                     ==> use only sub-brick #5
      fred+orig[5,9,17]                ==> use #5, #9, and #17
      fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8
      fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13
    Sub-brick indexes start at 0.  You can use the character '$'
    to indicate the last sub-brick in a dataset; for example, you
    can select every third sub-brick by using the selection list
      fred+orig[0..$(3)]
    

Notes:
======

.. code-block:: none

    N.B.: The sub-bricks are output in the order specified, which may
     not be the order in the original datasets.  For example, using
      fred+orig[0..$(2),1..$(2)]
     will cause the sub-bricks in fred+orig to be output into the
     new dataset in an interleaved fashion.  Using
      fred+orig[$..0]
     will reverse the order of the sub-bricks in the output.
    
    N.B.: Bucket datasets have multiple sub-bricks, but do NOT have
     a time dimension.  You can input sub-bricks from a 3D+time dataset
     into a bucket dataset.  You can use the '3dinfo' program to see
     how many sub-bricks a 3D+time or a bucket dataset contains.
    
    N.B.: The '$', '(', ')', '[', and ']' characters are special to
     the shell, so you will have to escape them.  This is most easily
     done by putting the entire dataset plus selection list inside
     single quotes, as in 'fred+orig[5..7,9]'.
    
    N.B.: In non-bucket functional datasets (like the 'fico' datasets
     output by FIM, or the 'fitt' datasets output by 3dttest), sub-brick
     [0] is the 'intensity' and sub-brick [1] is the statistical parameter
     used as a threshold.  Thus, to create a bucket dataset using the
     intensity from dataset A and the threshold from dataset B, and
     calling the output dataset C, you would type
        3dbucket -prefix C -fbuc 'A+orig[0]' -fbuc 'B+orig[1]'
    

WARNING:
========

.. code-block:: none

             Using this program, it is possible to create a dataset that
             has different basic datum types for different sub-bricks
             (e.g., shorts for brick 0, floats for brick 1).
             Do NOT do this!  Very few AFNI programs will work correctly
             with such datasets!
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
