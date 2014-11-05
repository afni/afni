
.. _MaskCont:

Network tracts mask controller
------------------------------

The mask controller is used for manipulating masks for network tractsYou can launch the :ref:`Mask Controller <MaskCont>` from the :ref:`tract controller <TractCont>` by clicking on :ref:`Masks<TractCont->Coloring_Controls->Masks>` twice.

.. figure:: media/MaskController.02.jpg
   :align: center


   ..




.. _MaskCont->Masks:

Create/delete masks and setup masking expression
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



.. figure:: media/MaskCont.auto.Masks.jpg
   :align: right

   ..



   .. _MaskCont->Masks->Mask_Eval.r00:

**Mask_Eval**: Evaluate mask expression


   A boolean expression evaluated per tract to determine whether or not a tract should be displayed. Each mask is assigned a letter from 'a' to 'z' and has an entry in the table below. Symbols for the OR operator are '|' or '+' while those for AND are '&' or '*'. The '|' is for the NOT operation. By default, the expression is blank, as indicated by '-', and the operation is an OR of all the masks.
   
   .. _Tract_Tinting:
   
Tract Tinting:
^^^^^^^^^^^^^^
   
   Tracts that go through any of the masks are displayed and they keep their own color, as shown in the figure below to the left.
   
   Say we now want to show tracts that go through both masks b and c or through mask a. The expression to evaluate at each tract would be: '( b & c ) | a'. Note that for the expression to take effect, you need to have the :ref:`v button<MaskCont->Masks->Mask_Eval->v>` selected.
   
   
   
      .. figure:: media/MaskedTracts.01.jpg
         :align: left
         :figwidth: 30%
   
         Tracts going through any of the three masks.
   
      .. figure:: media/MaskedTracts.02.jpg
         :align: right
         :figwidth: 30%
   
         Tracts evaluating to true per expression: '( b & c ) | a'.
   
      .. figure:: media/MaskController.02.jpg
         :align: center
         :figwidth: 30%
   
         Mask Controller.
   
   .. container:: clearer
   
      .. image:: media/blank.jpg
   
      .. Preceding block is a trick to keep upcoming text from wrapping around the figures.
   
   When using the the Mask Eval expression, the color of tracts that go though a set of regions is equal to the alpha weighted average of the colors of those regions. This can be seen in the figure on the right side above.
   
   The colors of a tract is given by::
   
      Ct = sum(AiCi)/sum(Ai)
   
   for all ROIs i the tract intersects.
   
   For example, say a tract goes through a blue region of color [0 0 1] with alpha of 0.5 (A ~ 5 in column A), and a red region of color [1 0 0] (alpha is 1.0, or in the table = 9). The tracts that go through both ROIs will be colored (1.0*([1 0 0]+0.5*([0 0 1])/1.5, which is purple. Similar averaging goes on if tracts go through more than 2 regions. Tracts that go though one region will get that region's color.
   
   Now, if you set alpha to 0 for a certain ROI, then that ROI does add to the tint of tracts that go thourough it at all. And for a tract that goes through that region only, it retains its original colors. See image on the right side.
   
   
   
     .. figure:: media/Masks.02.jpg
        :align: left
        :figwidth: 30%
   
        Tracts going through any of 2 masks 'a|b', with 'Mask Eval' ON.
   
     .. figure:: media/Masks.03.jpg
        :align: right
        :figwidth: 30%
   
        Tracts going through 'a|b' but with alpha of ROI 'a' - the blue one - set to 0. Tracts going through the blue ROI are not tinted by it at all.
   
     .. figure:: media/Masks.00.jpg
        :align: center
        :figwidth: 30%
   
        Mask Controller settings for image to the left.
   
   .. container:: clearer
   
      .. image:: media/blank.jpg
   
      .. Preceding block is a trick to keep upcoming text from wrapping around the figures.
   
   

   .. _MaskCont->Masks->Mask_Eval->v:

**v**: Enable (ON)/Disable Mask expression evaluation


   Enable (ON)/Disable Mask expression evaluation

   .. _MaskCont->Masks->Tract_Length.r00:

**Tract_Length**: Mask based on tract length


   Set Min Max length for tract masking. Use can scroll (mouse wheel) in Min and Max cells to change the value. The 'v' button must be selected for masking to take effect.

   .. _MaskCont->Masks->Tract_Length->v:

**v**: Enable (ON)/Disable Tract Length masking


   Enable (ON)/Disable Tract Length masking

   .. _MaskCont->Masks->Table.c00:

**Table.c00**: Add new mask


   Add one more row for a mask ROI

   .. _MaskCont->Masks->Table.c01:

**Table.c01**: Variable


   Variable symbol. Choose from 'a' to 'z'

   .. _MaskCont->Masks->Table.c02:

**Table.c02**: Label


   String label of ROI

   .. _MaskCont->Masks->Table.c03:

**Table.c03**: Type ('box' or 'sphere')


   Type of mask. For the moment, this string can only be one of 'box' or 'sphere'.

   .. _MaskCont->Masks->Table.c04:

**Table.c04**: Center X,Y,Z


   Set/Get coordinates in mm :term:`RAI` of the center of the mask 
   
   You can right click in cell to get back to the original center.
   
   You can also reposition the mask interactively in the SUMA viewer by :ref:`selecting <Button_3-Press>` something, if you are in :ref:`Mask Manipulation Mode<Mask_Manipulation_Mode>`.

   .. _MaskCont->Masks->Table.c05:

**Table.c05**: Size Sx,Sy,Sz


   Set/Get size along three dimensions of mask. You can enter a single value if the all three dimensions are equal. 
   
   Right click in cell to get back to the original size.
   
   Resizing in SUMA viewer can be done with :ref:`Ctrl+scroll<Ctrl+Scroll>` if you are in :ref:`Mask Manipulation Mode<Mask_Manipulation_Mode>`.
   
   You can also change values by scrolling with mouse pointer over the cell.
   

   .. _MaskCont->Masks->Table.c06:

**Table.c06**: Color R G B (A)


   Color of mask in RGB triplets between 0 and 1.0. You can also specify colors using the shorthands of:
   
      'b' or 'blue'
   
      'g' or 'green'
   
      'p' or 'pink'
   
      'r' or 'red'
   
      'w' or 'white'
   
      'y' or 'yellow'
   
   The final color depends also on the dim factor 'D'

   .. _MaskCont->Masks->Table.c07:

**Table.c07**: A


   Alpha of mask color. The Alpha value controls the contribution of an ROI'scolor to the tracts that pass through it. This tinting process is only used when 'Mask Eval' is in use, and when A is > 0. See the help for 'Mask Eval' for information on how tinting works.
   
   You can also change values by scrolling with mouse pointer over the cell.
   

   .. _MaskCont->Masks->Table.c08:

**Table.c08**: T


   Transparency of mask. A value of 0 renders a mask opaque. Consider using lower D values to avoid color saturation of rendered masks.
   
   You can also change values by scrolling with mouse pointer over the cell.
   

   .. _MaskCont->Masks->Table.c09:

**Table.c09**: D


   Dimming factor for color. Saturated colors may not look nice when rendered, so consider using the D parameter to dim a color's brightness without having to so directly in the color column. Setting D to 6 for example will scale a color by a factor of 6/9, so a saturated red of [1 0 0] becomes [0.67 0 0 ]. This makes masks render better when not in transparent mode T = 0.
   
   You can also change values by scrolling with mouse pointer over the cell.
   

   .. _MaskCont->Masks->Load_Masks:

**Load_Masks**: Load the masks 


   Load a set of masks previously saved by my neighboring button. The save operation also preserves the :ref:`Mask Eval<MaskCont->Masks->Mask_Eval.r00>` expression.
   
   Reloading a mask file will replace current masks.

   .. _MaskCont->Masks->Save_Masks:

**Save_Masks**: Save the masks 


   Save current set of masks for future loading by my neighborly button.
   

