
.. _tutorial_3dMVM_validator_GLT:

.. contents:: :local:
    :depth: 2

General Linear Tests.
---------------------

When the Generals Talk. [#f14]_
+++++++++++++++++++++++++++++++

+----------------------------------------------------------------------+
| The GLT tab opens blank. This will only function if you have a       |
| working model specified on the Model tab. The helpful warning box    |
| will show you what is wrong or missing before you push any buttons.  |
| The "Variable" selector will autofill with the first categorical     |
| variable.                                                            |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_blank.png                      |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

.. warning::

    If you change the model, the GLTs that you specified will disappear!!!
    So make sure you are done with your model BEFORE adding any GLTs!!!

|
Level 42. [#f15]_
+++++++++++++++++

+----------------------------------------------------------------------+
| After you type some reasonable label, choose a variable and the      |
| "Multi-select levels in order" drop-down will display the factor     |
| levels for that categorical variable. Just choose the ones you want  |
| in the order you want to add to the field.                           |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_levels.png                     |
|    :width: 75%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

+----------------------------------------------------------------------+
| Quantitative variables only require one number to be entered.        |
| So the levels drop-down will prompt you.                             |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_no_levels.png                  |
|    :width: 50%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

Carry That Weight. [#f16]_
++++++++++++++++++++++++++

+----------------------------------------------------------------------+
| With the levels selected, follow the prompt and enter numerical      |
| weights corresponding to your levels, separated by commas            |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_weights.png                    |
|    :width: 50%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

+----------------------------------------------------------------------+
| After entering appropriate weights, AND there are no errors, push    |
| the "Add variable" button to add a correctly formatted string        |
| to the "GLT code: -gltCode" field.                                   |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_add_variable.png               |
|    :width: 50%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

.. note::
    You can add more variables to the current GLT code by just repeating
    the same procedure.

|

A test, a test, a test, no rest. [#f17]_
++++++++++++++++++++++++++++++++++++++++

+----------------------------------------------------------------------+
| Push the "Test GLT" button to test the current GLT model using your  |
| data. If it does not fail, you should see some summary in the        |
| adjacent box. If it does fail, you will see "Model Fail!             |
| Try again..."                                                        |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_test_glt.png                   |
|    :width: 50%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

|

.. warning::

    You should not care if the result is significant, only make sure
    it does not fail!!!

+----------------------------------------------------------------------+
| When you are satisfied with the GLT, push the "Add GLT" button to    |
| add the current GLT code to the text box below. It will be formatted |
| and numbered correctly and added to the 3dMVM script on the          |
| "Script" tab. All of the selections above will be cleared for the    |
| next GLT to be entered                                               |
+----------------------------------------------------------------------+
| .. figure:: media/3dMVM_validator_GLT_add_glt.png                    |
|    :width: 50%                                                       |
|    :align: left                                                      |
| .. figure:: media/3dMVM_validator_GLT_script_update.png              |
|    :width: 50%                                                       |
|    :align: left                                                      |
+----------------------------------------------------------------------+

.. note::
    You can add more GLTs to the list by just repeating the same procedure.



More coming soon
----------------

.. rubric:: Footnotes

.. [#f14] Midnight Oil
.. [#f15] Level 42
.. [#f16] The Beatles
.. [#f17] Test by Little Dragon
