.. _WalnutBrain:

=============
Walnut Brain:
=============

.. _Prying_Brains_Apart:

Prying Brains Apart:
--------------------

Cortical surfaces can obsucre structures within the cortex such as tracts or surface renderings of subcortical regions. To improve visualization, you can render the surfaces transparent, e.g. :ref:`o <LC_o>`, but you still can't select through a transparent surface. One solution is to pry the hemispheres apart. For this to work, you will need to have both hemisphere loaded into SUMA. You can try the following, assuming you have the :ref:`MNI_N27` surfaces installed::

   suma -spec MNI_N27 &
   
.. figure:: media/suma_mni_n27.jpg
   :align: center
   :figwidth: 70%
   
..

Then with both surfaces in view, :ref:`drag while pressing button 1<Control+Button_1-Motion>` to pry open the two hemispheres. 

.. figure:: media/suma_mni_n27_pry1.jpg
   :align: center
   :figwidth: 70%

..

.. figure:: media/suma_mni_n27_pry2.jpg
   :align: center
   :figwidth: 70%

..

More details are found under the interactive help link for :ref:`this button combination<Control+Button_1-Motion>`. You can :ref:`undo the prying <Control+Button_1-DoubleClick>` with Control+Button_1-DoubleClick or :ref:`change the axis about which it is done with F10<F10>`.
    
.. figure:: media/suma_mni_n27_pry3.jpg
   :align: center
   :figwidth: 70%
      
..

For more exciting prying action, such as what is shown below, follow instructions in scripts :ref:`Do_09_VISdti_SUMA_visual_ex2.tcsh<Do_09_VISdti_SUMA_visual_ex2.tcsh>` and :ref:`Do_09_VISdti_SUMA_visual_ex3.tcsh<Do_09_VISdti_SUMA_visual_ex3.tcsh>` 

