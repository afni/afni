
.. _tutorial_3dMVM_validator_overview:

============
**Overview**
============

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


.. rubric:: Footnotes

.. [#f1] Men at Work
.. [#f2] Ready to Run by The Dixie Chicks
.. [#f3] S... Sadie by The Beatles
