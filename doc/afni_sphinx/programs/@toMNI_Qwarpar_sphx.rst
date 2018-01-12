.. contents:: 
    :depth: 4 

**************
@toMNI_Qwarpar
**************

.. code-block:: none

     
    ** This script is similar to @toMNI_Qwarp -- but it spawns **
    ** jobs in parallel (on the same system).  To use it, you  **
    ** must edit the script and set the 2 variables            **
     
    Script to take a collection of datasets and transform them
    to MNI space, then collectively re-transform them to produce
    a more refined average.  This script is usually used AFTER
    running @toMNI_Awarp to do the affine alignments, and that
    script is run AFTER skull-stripping the original volumes.
     
    This script spawns jobs to run in parallel (on the same system).
    Before using it, copy it into the data directory, and edit it
    to set the 2 variables:
         set numcpu = TOTAL NUMBER OF CPUS TO USE
         set numjob = MAX NUMBER OF JOBS TO USE
    numcpu should not exceed the number of CPUs (cores) on the system;
    it is often simplest to set numjob to the same value as numcpu,
    so that 1 dataset is processed in 1 core, and numcpu jobs are
    run at a time.
     
    Usage: @toMNI_Qwarpar    (... and then wait a long time)
     
    It should be run inside the directory created by @toMNI_Awarp, and
