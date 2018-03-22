*********
@fast_roi
*********

.. _ahelp_@fast_roi:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: @fast_roi <-region REGION1> [<-region REGION2> ...]
                         <-base TLRC_BASE> <-anat ANAT> 
                         <-roi_grid GRID >
                         <-prefix PREFIX >
                         [-time] [-twopass] [-help]
    Creates Atlas-based ROI masked in ANAT's original space.
    The script is meant to work rapidly for realtime fmri applications
    Parameters:
      -region REGION: Symbolic atlas-based region name. 
                      See whereami -help for details.
                     You can use repeated instances of this option
                     to specify a mask of numerous regions.
                     Each region is assigned a power of 2 integer
                     in the output mask
      -drawn_roi ROI+tlrc: A user drawn ROI in standard (tlrc) space.
                           This ROI gets added with the REGION roi.
    
      -anat ANAT: Anat is the volume to be put in std space. It does not
                  need to be a T1 weighted volume but you need to choose
                  a similarly weighted TLRC_BASE.
                  If ANAT is already in TLRC space then there is no need
                  for -base option below.
      -anat_ns ANAT: Same as above, but it indicates that the skull
                     has been removed already.
      -base TLRC_BASE:  Name of reference TLRC volume. See @auto_tlrc
                        for more details on this option. Note that
                        for the purposes of speeding up the process,
                        you might want to create a lower resolution
                        version of the templates in the AFNI. In the
                        example shown below, TT_N27_r2+tlrc was created
                        with: 
               3dresample  -dxyz 2 2 2 -rmode Li -prefix ./TT_N27_r2 \
                           -input /home/ptaylor/afni_src/linux_ubuntu_12_64/TT_N27+tlrc. 
                        where TT_N27+tlrc is usually in the directory 
                        under which afni resides.
      -roi_grid GRID: The volume that defines the final ROI's grid.
      -prefix PREFIX: PREFIX is used to tag the names the ROIs output.
      -time: A flag to make the script output elapsed time reports.
      -twopass: Make TLRC transformation more robust. Use it if TLRC 
                transform step fails.
      -help: Output this message.
    
    The ROI of interest is in a volume called ROI.PREFIX+orig.
    
    The script follows the following steps:
      1- Strip skull off of ANAT+orig 
         Output is called nosk.ANAT+orig and is reused if present.
      2- Transform nosk.ANAT+orig to TLRC space.
         Output is called nosk.ANAT+tlrc and is reused if present.
      3- Create ROI in TLRC space using 3dcalc.
         Output is ROIt.PREFIX+tlrc and is overwritten if present.
      4- Create ROI in GRID's orig space using 3dFractionize.
         Output is ROI.PREFIX+orig and is overwritten if present.
    
    Examples ( require AFNI_data3/afni, and 
               3dresample's output from command shown above):
         @fast_roi  -region CA_N27_ML::Hip -region CA_N27_ML::Amygda \
                     -base TT_N27_r2+tlrc. -anat anat1+orig.HEAD  \
                     -roi_grid epi_r1+orig -prefix toy -time
    
        If you want another ROI given the same -anat and -base volumes:
         @fast_roi  -region CA_N27_ML::Superior_Temporal_Gyrus \
                     -region CA_N27_ML::Putamen \
                     -base TT_N27_r2+tlrc. -anat anat1+orig.HEAD  \
                     -roi_grid epi_r1+orig -prefix toy -time
