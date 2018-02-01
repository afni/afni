************
@RenamePanga
************

.. _@RenamePanga:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @RenamePanga <Dir #> <First Image #> <# slices> <# reps> <Output Root>
                       [-kp] [-i] [-oc] [-sp Pattern] [-od Output Directory]
    
    Creates AFNI bricks from RealTime GE EPI series.
    
    This script is designed to run from the directory where the famed RT image directories are copied to.
    If the data were copied from fim3T-adw using @RTcp, this directory should be something like:
    /mnt/arena/03/users/sdc-nfs/Data/RTime/2018.01.31/<PID>/<Exam #>/
    
    <Dir #> : (eg: 3) The directory number where the first image of the series is stored.
    <First Image #> : (eg: 19) The number of the first image in the series.
    <# slices> : (eg: 18) The number of slices making up the imaged volume.
    <# reps> : (eg: 160) The number of samples in your time series.
    <Output Root> : (eg: PolcCw) The prefix for the output brick.
                     Bricks are automatically saved into the output directory
                     Unless you use -kp option, bricks are automatically named
                     <Output Root>_r# where # is generated each time you 
                     run the script and successfully create a new brick.
    
    Optional Parameters:
    -i : Launches to3d in interactive mode. This allows you to double check the automated settings.
     -kp: Forces @RenamePanga to use the prefix you designate without modification.
     -oc: Performs outliers check. This is useful to do but it slows to3d down and
      maybe annoying when checking your data while scanning. If you choose -oc, the
      outliers are written to a .1D file and placed in the output directory.
     -sp Pattern: Sets the slice acquisition pattern. The default option is alt+z.
      see to3d -help for various acceptable options.
     -od <Output Directory>: Directory where the output (bricks and 1D files) will
      be stored. The default directory is ./afni
    
    
    A log file (MAPLOG_Panga) is created in the current directory.
    
    Panga: A state of revenge.
    ***********
    Dec 4 2001 Changes:
    - No longer requires the program pad_str.
    - Uses to3d to read geometric slice information.
    - Allows for bypassing the default naming convention.
    - You need to be running AFNI built after Dec 3 2001 to use this script.
    - Swapping needs are now determined by to3d.
    If to3d complains about not being able to determine swapping needs, check the data manually
    - Geom parent option (-gp) has been removed.
    - TR is no longer set from command line, it is obtained from the image headers.
    Thanks to Jill W., Mike B. and Shruti J. for reporting bugs and testing the scripts.
    ***********
    
    Usage: @RenamePanga <Dir #> <First Image #> <# slices> <# reps> <Output Root>
                       [-kp] [-i] [-oc] [-sp Pattern] [-od Output Directory]
    
