
.. _tutorial_3dMVM_validator_main:


**3dMVM_validator**
====================

| **THIS TUTORIAL IS NOT YET COMPLETE!!!**

.. contents:: :local:
    :depth: 2

Overkill [#f1]_
---------------

The ``3dMVM_validator`` is a `shiny app <https://shiny.rstudio.com/>`_
that can be used to help test a
`3dMVM <https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dMVM.html>`_ model.
It will validate your model *before* you run 3dMVM to make sure that the model
makes some sense.
The app also helps with the model and GLT specifications and will generate an output
script that can be run from the terminal.
Hopefully this will save some time instead of waiting to see if 3dMVM likes
your specifications and you spelled everything correctly.

Briefly, here is what will happen when you run ``3dMVM_validator -dataTable my_data.csv``:

* afni will launch and display a dataset.
* The default browser will launch the shiny app and load your data table.
* Choose a coordinate in an afni window.
* Extract and load the data from your input data sets.
* Specify a model and get results instantly.

+-------------------------------------------+---------------------------+
| **This should only be used to make sure** | .. image:: media/baby.jpg |
| **that the model is specified correctly** |        :width: 50%        |
| **NOT to test for any kind of**           |        :align: center     |
| **significance.**                         |                           |
+-------------------------------------------+---------------------------+


Ready, ready, ready, ready, ready to run. [#f2]_
-----------------------------------------------

#. Make sure AFNI and R are installed and configured correctly.
    `See installation
    <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/main_toc.html>`_.
#. Run ``@afni_R_package_install -shiny`` to install necessary R libraries.
#. Prepare a data table of the format required for `3dMVM <https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dMVM.html>`_.
#. Have your datasets handy.

We gave her everything we owned just to sit at her table. [#f3]_
----------------------------------------------------------------

The only input to ``3dMVM_validator`` is a file containing a table
similar to the "-dataTable" input for `3dMVM <https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dMVM.html>`_.

* The table **MUST** be saved as a comma, space, or tab separated values file, but can have any extension.
* The first column header **MUST** be "Subj". Spelling and capitalization counts!
* The last column header **MUST** be "InputFile".
    * This column should have a path to your datasets.
    * Relative paths are ok, but for portability sake, full paths are recommended.
    * Subbrik selectors are allowed.
* The middle columns are what ever variables you have that may go into your model.
    * They can be categorical or quantitative variables.
* All other rules for 3dMVM -dataTable must be followed as well.

For example:

======== ========= ======== ===== ================
Subj     treatment response Score InputFile
subj0060 CBT       nonresp  0.4   subj.0060.nii.gz
subj0082 CBT       nonresp  0.5   subj.0082.nii.gz
subj0049 CBT       remit    0.3   subj.0049.nii.gz
subj0076 CBT       remit    0.2   subj.0076.nii.gz
subj0213 Drug      nonresp  0.4   subj.0213.nii.gz
subj0231 Drug      nonresp  0.7   subj.0231.nii.gz
subj0061 Drug      remit    0.8   subj.0061.nii.gz
subj0075 Drug      remit    0.8   subj.0075.nii.gz
======== ========= ======== ===== ================

Walking in your footsteps. [#f4]_
--------------------------------

Start Up
++++++++

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

Location location location
++++++++++++++++++++++++++

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


Positive Role Model. [#f6]_
---------------------------

By Default by Design. [#f7]_
++++++++++++++++++++++++++++

+----------------------------------------------------------------------+
| After loading the data, the default variables are tested as a model. |
| The a summary of the model results is shown below. The text is a     |
| print out of the R function that calculates the model (aov_car).     |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_specify_01.png                     |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

Spelling counts in large amounts. [#f8]_
++++++++++++++++++++++++++++++++++++++++

+----------------------------------------------------------------------+
| Start typing a model as you would for the -bsVars argument in 3dMVM. |
| The program will make sure that you are entering a valid model.      |
| It will check your spelling and match your text with the variables   |
| that are in your data table. The "Specification errors" box will     |
| show you want is wrong with the text strings.                        |
| If the model is valid, the "Model result" box will display a summary.|
| If not, you will see the "Enter a valid model" warning.              |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_specify_02.png                     |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

Rudie Can't Fail. [#f9]_
++++++++++++++++++++++++

+----------------------------------------------------------------------+
| This model shows a failure. The BaselineScore is a numerical         |
| variable, but it was not specified as a quantitative variable -qVars.|
| Since it tests the model as you type, you know where the mistake is. |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_specify_fail.png                   |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

+----------------------------------------------------------------------+
| Specifying the BaselineScore as a -qVars gives a successful model.   |
| Gender is also numerical, but it would not be quantitative.          |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_specify_success.png                |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

.. note::

    We don't care about the p value for this model!!!
    We only want to make sure it doesn't fail!!!

.. +-------------------------------------------------------+
.. | Choosing a strange region will also fail.             |
.. +-------------------------------------------------------+
.. | .. figure:: media/3dMVM_validator_specify_bad_ROI.png |
.. |    :width: 75%                                        |
.. |    :align: left                                       |
.. +-------------------------------------------------------+

And my script's been rearranged. [#f10]_
++++++++++++++++++++++++++++++++++++++++

+----------------------------------------------------------------------+
| This tab shows the script that is created as you specify options.    |
| Every change and addition you make will be updated live in the       |
| script. You can download the finalized script here to run in the     |
| terminal.                                                            |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_script.png                         |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+


More coming soon
----------------

.. rubric:: Footnotes

.. [#f1] Men at Work
.. [#f2] Ready to Run by The Dixie Chicks
.. [#f3] S... Sadie by The Beatles
.. [#f4] The Police
.. [#f5] Incredible by Joss Stone
.. [#f6] Pet Shop Boys
.. [#f7] ABC
.. [#f8] Everything Counts by Depeche Mode
.. [#f9] The Clash
.. [#f10] House of Pain by Faster Pussycat
