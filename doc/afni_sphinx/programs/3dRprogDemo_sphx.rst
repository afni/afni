***********
3dRprogDemo
***********

.. _ahelp_3dRprogDemo:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ¸b–
    Usage:
    ------ 
     3dRprogDemo is a template program to help users write their own R
     processing routines on MRI volumes without having to deal with
     things like volume I/O or command line argument parsing.
     
     This template program shows rudimentary command line option parsing,
     volume reading, calling a silly processing function on each voxel time series,
     and writing the output. 
     
     This 3dRprogDemo.R file is paired with the script 3dRprogDemo which
     allows users to run R programs directlt from the shell. To create your
     own 3dSOMETHING program you would do at least the following:
     
     cp 3dRprogDemo.R 3dSOMETHING.R 
     cp 3dRprogDemo 3dSOMETHING
     Modify the variable ExecName in 3dSOMETHING.R to reflect your program name
     Replace the function  RprogDemo.Scale() with your own function 
    
     Unfortunately at this stage, there is little help for the AFNI R API
     beyond this sample code. If you find yourself using this and need
     to ask questions about other dataset utility functions contact the author 
     for help. The AFNIio.R file in the AFNI distribution contains most of the IO
     functions. Below are some notable ones, grep for them in the .R files for 
     usage examples.
       
       dset.attr() for getting and setting attributes, such as the TR in seconds
                   e.g. dset$NI_head <- dset.attr(dset$NI_head, "TR", val = 1.5)
       read.AFNI()
       write.AFNI()
       show.dset.attr()
       dset.index3Dto1D()
       dset.index1Dto3D()
       dset.dimBRKarray()
       dset.3DBRKarrayto1D()
       dset.1DBRKarrayto3D()
       
       parse.AFNI.name() for parsing a filename into AFNI relevant parameters
       exists.AFNI.name()
       note.AFNI(), err.AFNI(), warn.AFNI(), exit.AFNI()
          
     Debugging Note:
     ===============
     When running the program from the shell prompt, you cannot use R's
     browser() function to halt execution and step through the code.
     However, the utility function load.debug.AFNI.args() makes it very easy
     for you to run the command line equivalent from the R prompt. Doing so 
     would make available the browser() functionality. To use load.debug.AFNI.args()
     follow these steps: 
     1- Run the program from the shell command line. The program will
     automatically create a hidden file called .YOUR_PROGRAM_NAME.dbg.AFNI.args
     2- Start R from the same directory or change to the directory where 
     you ran the program if you started R elesewhere
     3- Run the function:  load.debug.AFNI.args() and follow the prompts.
     The function will look for possible debug files, prompt you to pick
     the one you want, and start the execution from the R shell.
    
    
    Example 1 --- Read a dataset, scale it, then write the results:
    -----------------------------------------------------------------------------
          3dRprogDemo       -input epi.nii    
                            -mask mask.nii    
                            -scale 7          
                            -prefix toy.nii
    
    
    
    Options in alphabetical order:
    ------------------------------
    
       -h_aspx: like -h_spx, with autolabeling
    
       -help: this help message, in simple text.
    
       -h_raw: this help message, as is in the code.
    
       -h_spx: this help message, in sphinx format
    
       -h_txt: this help message, in simple text
    
       -input DSET1                       \
          Specify the dataset to be scaled. Note that you can use
          the various sub-brick selectors used by AFNI
          e.g: -input pb05.Regression+tlrc'[face#0_Beta]'  \
          You can use multiple instances of -input in one command line
          to process multiple datasets in the same manner.
    
       -mask MASK: Process voxels inside this mask only.
                 Default is no masking.
    
       -prefix PREFIX: Output prefix (just prefix, no view+suffix needed)
    
       -scale SS: Multiply each voxel by SS 
    
       -show_allowed_options: list of allowed options
    
       -verb VERB: VERB is an integer specifying verbosity level.
