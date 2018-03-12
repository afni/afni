*********************
ClustExp_StatParse.py
*********************

.. _ClustExp_StatParse.py:

.. contents:: 
    :depth: 4 

| 

    usage: /home/ptaylor/afni_src/linux_ubuntu_12_64/ClustExp_StatParse.py
           [-h] -StatDSET STATDSET -MeanBrik MEANBK -ThreshBrik THRESHBK -SubjDSET
           SUBJDSET -SubjTable SUBJTABLE -master MASTER [-prefix PREFIX] [-p PVAL]
           [-MinVox MINVOX] [-atlas ATLAS] [-session SESSION] [-NoShiny]
           [-overwrite] [-help]
    

## Overview
===========

    

## Input datasets
+++++++++++++++++

.. code-block:: none

    All data must be in the same space and aligned to the same template.
    And must be +tlrc or .nii or .nii.gz, +orig should fail.
    For the master, you need the full path.
    It does not have to be the same voxel size as the subject and stats data sets.
    This will resample the grid of the master to match the other data sets.
    I will add a lookup for the built ins later.
    

## Subject table
++++++++++++++++

.. code-block:: none

    The -SubjTable needs to be 3 columns.
    1: Subject ID
    2: Data set and current location path.
    3: Data set and path at the time of running the analysis (to match the history).
    The input files to your 3dttest++ or 3dMVM must be included in your input
    subjects table -SubjTable and match EXACTLY!
    If you put ./subjects/subj1.nii.gz[0] in the analysis, the -SubjTable
    must have the same exact string.
    This is to take care of paths like: ./subjects/subj1/data.nii.gz[0].
    

## Caveats
++++++++++

.. code-block:: none

    Statistics image must be DIRECTLY from 3dttest++ or 3dMVM.
    3dttest++ must have been run with no covariates.
    
    For now only some simple models will work with the shiny app.
    GLTs included in the 3dMVM command will be ignored in the shiny app.
    But the data table from the output should still be useful.
    
    If you did 3dcopy or something else to your data set after analysis,
    you may not have the history information necessary for this process.
    
    Only outputs NIfTI images, as they are easier for the shiny app.
    

## Outputs
==========

.. code-block:: none

    
    Outputs files named with your -prefix and some with the -p
    (as example -prefix disco -p 0.01):
    
    disco_p_uncor_0.01_mean.csv:
        Table with all data extracted from all of your subjects.
        The column headers are the coordinates of the center of mass of the cluster.
        The values are means of each cluster for that subject.
    
    disco_GroupTable.csv:
        Table with information parsed from the statistics data set history.
        Includes subject ID, any grouping variables, and input data sets.
    
    disco_p_uncor_0.01_3dclust.1D:
        Output directly from 3dclust with orientation of LPI.
    
    disco_p_uncor_0.01_clusters.csv:
        Cleaned up version of the whereami output. Includes labels the FIRST entry
        of your search atlas. The default atlas is TT_Daemon. If nothing is found,
        there is an NA, but this gets replaced by the coordinate in the shiny app.
    
    disco_StatInfo.csv:
        Some summary info for the shiny app. Includes most of the command line
        arguments and things parsed from the statistics data set history.
    
    disco_p_uncor_0.01.nii.gz:
        A new data set from your input statistics data set, thresholded at your
        uncorrected p value using the selected subbriks.
    
    disco_p_uncor_0.01_mask.nii.gz:
        An integer labeled mask of the above image with cluster sizes at least
        as big as the -MinVox (default 100 may be too much for larger voxel sizes).
    
    disco_master.nii.gz:
        A NIfTI copy of the master file provided that may have been resampled.
        This is for the shiny app.
    
    

## Options
==========

.. code-block:: none

    
    required:
      -StatDSET STATDSET    Statistics dataset.
      -MeanBrik MEANBK      Mean subbrik (integer >= 0).
      -ThreshBrik THRESHBK  Threshold subbrik. Might be the same as MeanBrik
                            (integer >= 0).
      -SubjDSET SUBJDSET    Labeled dataset with all subjects (from
                            @ClustExp_CatLab).
      -SubjTable SUBJTABLE  Table with subject labels and input datasets.
      -master MASTER        Master data set for underlay.
    
    optional:
      -prefix PREFIX        Name for output (no path). [MyOutput]
      -p PVAL               Uncorrected p value for thresholding. [0.005]
      -MinVox MINVOX        Minimum voxels in cluster. [100]
      -atlas ATLAS          Atlas name for lookup. (list at: whereami -help)
                            [TT_Daemon]
      -session SESSION      Output parent folder if you don't want the current
                            working directory. [./]
      -NoShiny              Do not create shiny app.
      -overwrite            Remove previous folder with same PREFIX
    
    Justin Rajendra circa 09/2017
    I hope this will be useful for someone...
    Keep on keeping on!
