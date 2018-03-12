*****************
@snapshot_volreg3
*****************

.. _@snapshot_volreg3:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    -----------------------------------------------------------------
    
    This script will make a JPEG image showing the edges of an
    EPI dataset overlay-ed on an anatomical dataset.  The purpose is
    to let the user (you) judge the quality of the 3D registration.
    
    Three images from each of the coronal, axial, and sagittal
    AFNI image viewers are used, laid out in a 3x3 grid.
    
    @snapshot_volreg works by running the AFNI GUI inside a "virtual"
    X11 display server program named "Xvfb", and saving images from
    that copy of AFNI.  The script also uses programs from the netpbm11
    software library to put the saved images together into a pleasing
    layout.  If the script cannot find the netpbm11 software, it will
    not run :(
    
    -----------------------------------------------------------------
    Usage: @snapshot_volreg ANATdataset EPIdataset [jname] [xdisplay]
    
    Sample (from an afni_proc.py results directory):
    
      @snapshot_volreg anat_final.sub-10506+tlrc      \
                       pb02.sub-10506.r01.volreg+tlrc sub-10506
    
    The output file from this example is "sub-10506.jpg".
    -----------------------------------------------------------------
    
    Do NOT put a sub-brick index (such as "[0]") on the EPIdataset
    name -- the script will automatically only use the "[0]" volume.
    
    (( Although the original use was for visualizing how well EPI ))
    (( and anatomical datasets were aligned by align_epi_anat.py, ))
    (( it is also useful to see how well 3dQwarp aligned an       ))
    (( anatomical dataset to a template dataset.                  ))
    
    The optional third argument is the name of the output JPEG
    file -- if it does not end in ".jpg", that suffix will be added.
    If you do NOT supply a 3rd argument, the script will invent a name:
    it is probably better for you to supply a 3rd argument.
    
    The fourth (and very optional) argument is the display number
    of an ALREADY RUNNING copy of Xvfb, as in
      Xvfb :88 -screen 0 1024x768x24 &
    If you do NOT supply this number (88 in the example), then
    the script will start its own Xvfb (on a display of its choosing),
    use it once, and then stop it. If you are going to run this script
    many times in a row, starting and stopping your own Xvfb
    instance will speed things up a little. Normally, you do not
    need to use this 4th argument.
    
    -----------------------------------------------------------------
    
    The edges from a typical EPI dataset are usually broken up and
    do not completely outline sulci, ventricles, etc.  In judging
    the quality of alignment, I usually start by looking at the
    outlines of the large lateral ventricles -- if those are very
    wrong, the alignment is not good.  After that, I look at the
    sulci in the superior part of the brain -- if the EPI edges
    there seem to be mostly aligned with the sulci, then I am
    usually happy.  The base of the brain, where lots of EPI
    dropout happens, often does not not show good edge alignment
    even when the rest of the brain alignment looks good.
    
    -----------------------------------------------------------------
    
    If this script crashes, then it might leave behind files with
    names that start with "zzerm".  Delete these files.
    It is also possible that the Xvfb program will still be running
    if this script crashes.  A command such as that below can
    be used to see if you have any stray Xvfb programs running:
    
      ps X | grep Xvfb | grep -v grep
    
    If there are any such programs, the command below can be used
    to kill all of them:
    
      killall Xvfb
    
    -------------- Author: The Madd Allineator ----------------------
