
.. _tutorial_3dMVM_validator_setup:

.. contents:: :local:
    :depth: 2

Walking in your footsteps. [#f4]_
---------------------------------

Start Up.
+++++++++

.. topic:: After everything opens, you should see something like this:

    .. figure:: media/3dMVM_validator_start.png
        :width: 60%
        :align: left

    * Helpful prompts will show up in the lower right corner of the window.
    * The afni gui will load the first dataset from your table and display windows to the left of the screen.
    * The shiny app will have read your data table and parsed the variables and info boxes will be filled with information.
        * Categorical variables, numerical variables, and number of subjects.
    * The first input box will be pre filled with a categorical variable.
    * The app will guess if there are within subject variables and give suggestions.
    * Same for the quantitative variables.

Location location location.
+++++++++++++++++++++++++++

There are two ways to select a location from which to extract data.

#. Single voxel coordinate.
#. Region of interest sphere.

Any time you change the crosshair location in the afni gui, you will need to
push the "Get Coordinate" button again to load the new location.
The coordinates are **NOT** live as in `InstaCorr <https://afni.nimh.nih.gov/pub/dist/doc/misc/instacorr.pdf>`_.

.. topic:: Choose a single voxel coordinate:

    .. figure:: media/3dMVM_validator_get_coor.png
        :width: 60%
        :align: left

    * After selecting a coordinate in the afni gui, push the "Get Coordinate" button.
    * This will get the current crosshairs position and display the coordinates in the info box.

.. topic:: Create a spherical ROI:

    .. figure:: media/3dMVM_validator_make_ROI.png
        :width: 60%
        :align: left

    * After selecting a coordinate in the afni gui, click on the "Get Coordinate" button.
    * With the coordinate loaded, change "Extract Data From:" to "ROI".
    * This will display an input for the "Seed Radius:" in mm for a sphere.
    * The "Make ROI" button will make an ROI mask and load it as an overlay


Extract the soul from human kind. [#f5]_
++++++++++++++++++++++++++++++++++++++++

.. topic:: Extract and load the data:

    When you push the "Extract Data" button, the app will extract that voxel or ROI
    from each dataset in your data table and merge the extracted value with the
    rest of the variables.

    +------------------------------------------------+---------------------------------------------------------------------------+
    | Extract:                                       | May take awhile depending on the voxel resolution and number of datasets: |
    +------------------------------------------------+---------------------------------------------------------------------------+
    | .. figure:: media/3dMVM_validator_extract.png  | .. figure:: media/3dMVM_validator_wait.png                                |
    |    :width: 50%                                 |    :width: 100%                                                           |
    |    :align: left                                |    :align: left                                                           |
    +------------------------------------------------+---------------------------------------------------------------------------+
    | Helpful prompt:                                | Load:                                                                     |
    +------------------------------------------------+---------------------------------------------------------------------------+
    | .. figure:: media/3dMVM_validator_ok_load.png  | .. figure:: media/3dMVM_validator_load.png                                |
    |    :width: 100%                                |    :width: 50%                                                            |
    |    :align: left                                |    :align: left                                                           |
    +------------------------------------------------+---------------------------------------------------------------------------+

|
|

-----------

:ref:`On to model building! <tutorial_3dMVM_validator_model>`
=============================================================

|
|

.. rubric:: Footnotes

.. [#f4] The Police
.. [#f5] Incredible by Joss Stone
