
.. _tutorial_3dMVM_validator_model:


| **THIS TUTORIAL IS NOT YET COMPLETE!!!**

.. contents:: :local:
    :depth: 2

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

.. warning::

    We don't care about the p value for this model!!!
    We only want to make sure it doesn't fail!!!

.. +-------------------------------------------------------+
.. | Choosing a strange region will also fail.             |
.. +-------------------------------------------------------+
.. | .. figure:: media/3dMVM_validator_specify_bad_ROI.png |
.. |    :width: 75%                                        |
.. |    :align: left                                       |
.. +-------------------------------------------------------+

|
|

-----------

:ref:`On to other tabs! <tutorial_3dMVM_validator_tabs>`
========================================================

|
|

.. rubric:: Footnotes

.. [#f6] Pet Shop Boys
.. [#f7] ABC
.. [#f8] Everything Counts by Depeche Mode
.. [#f9] The Clash

