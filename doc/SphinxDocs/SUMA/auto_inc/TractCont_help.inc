
.. _TractCont:

The Network/Tracts Controller:
------------------------------

The tract controller is for controlling the way tracts and values defined over them are displayed. Each network of tracts gets its own controller. You can launch the :ref:`Tract Controller <TractCont>` with: :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`

.. figure:: media/TractController.jpg
   :align: center


   ..




.. _TractCont->Tract_Properties:

Tract Properties:
=================

Name and number of bundles, tracts, and points making up the  selected network of tracts. 

   .. _TractCont->Tract_Properties->more:

**more**: More info on network of tracts


Opens a dialog with detailed information about the object in geek speak.

.. _TractCont->Xhair_Info:

Information at crosshair:
=========================


   .. _TractCont->Xhair_Info->Xhr.r00:

**Xhr**: Crosshair coordinates.


Set/Get crosshair location in mm :term:`RAI` on
this controller's selected object.
Entering new coordinates 
makes the crosshair jump
to that location (like :ref:`'ctrl+j' <LC_Ctrl+j>`).
Use :ref:`'alt+l'<LC_Alt+l>` to center the
cross hair in your viewer.

   .. _TractCont->Xhair_Info->Ind.r00:

**Ind**: Point index in whole network


Set/Get the :term:`1D index` of the selected elementary tract datum: the infinitesimal point.


   .. _TractCont->Xhair_Info->BTP.r00:

**BTP**: Bundle index in network, Tract index in bundle, Point index in tract


Set/Get the triplet of indices for the selection on the displayed tracts.
   The 1st index is that of the selected :term:`bundle` in the network

   The second is for the selected :term:`tract` in that bundle

   The third is the index of the :term:`point` selected along that tract.



   .. _TractCont->Xhair_Info->Val.c00:

**Val.c00**: Data values at tract point in focus


Data values at point in focus. At the moment, :term:`Intensity`, :term:`Threshold`, and :term:`Brightness` show the RGB values for the point selected. Eventually, they would represent the triplets of values at the point that correspond to the dataset column choices in :term:`I`, :term:`T`, :term:`B`.

   .. _TractCont->Xhair_Info->Lbl.r00:

**Lbl**: Label at selected point


Labels at selected point. For now, nothing more than a regurgitation of :ref:`BTP<TractCont->Xhair_Info->BTP.r00>`

.. _TractCont->Coloring_Controls:

Coloring Controls:
==================

Controls the final coloration of the tracts based on the tract datasets available. What's a tract dataset you say? It is a dataset defined over the collection of points that define the tracts of a network. And where do we get these sets? Nowhere at the moment. For now they are generated internally and they are only of the RGB variety. This will change in the future, when you would be able to drive a flying car and have arbitrary sets much like on surfaces or volumes.

   .. _TractCont->Coloring_Controls->Lbl.r00:

**Lbl**: Label of dataset displayed on tracts


Label of dataset currently selected. Note that what you're viewingat this moment maybe a blend of multiple datasets. See :ref:`color mixing<ColorMixing>` for details.

   .. _TractCont->Coloring_Controls->Dim:

**Dim**: Dimming factor to apply to colormap or color datasets.


Dimming factor to apply to colormap
before mapping the intensity (I) data.
The colormap, if displayed on the right,
is not visibly affected by Dim but the
colors mapped onto the surface, voxel grid, tracts, etc. are.
For RGB Dsets (e.g. .col files, or tract colors), Dim is
applied to the RGB colors directly.

Decreasing Dim is useful when the colors are too saturated
for lighting to reflect the object terrain.
When in doubt, just press the button and see what happens.

   .. _TractCont->Coloring_Controls->Ord:

**Ord**: Order of Dset's colorplane.


Order of this tract's dataset colorplane in the stack of all colorplanes available.
The datset with highest order number is 
on top of the stack.
See :ref:`color plane grouping <Color_Plane_Grouping>` for details on how colors are merged.

   .. _TractCont->Coloring_Controls->1:

**1**: Show ONLY selected set. 


If ON, view only the selected
Dset's colors.

If OFF, then mix the color planes
in the datasets stack.

This option makes it easy to view 
one Dset's colors at a time without
having to worry about color mixing,
opacity, and stacking order.

Needless to say, options such as
'Ord:' and 'Opa:' in this panel are 
of little use when this button is ON.

   .. _TractCont->Coloring_Controls->Opa:

**Opa**: Opacity of Dset's colorplane.


Opacity of Dset's colorplane.
Opaque planes have an opacity
of 1, transparent planes have
an opacity of 0. 

Opacity values are not applied
to the first plane in a group.
Consequently, if you have just
one plane to work with, or you have :ref:`1<TractCont->Coloring_Controls->1>` ON, 
the opacity  value is meaningless.

Color mixing can be done in two 
ways, use :ref:`F7<F7>` to toggle between 
mixing modes.


   .. _TractCont->Coloring_Controls->Ln:

**Ln**: Line drawing style


Choose the line drawing style.
   Digits specify number of pixels to mask out of each 16 pixels

   1 :   One pixel/16 off, almost solid

   ...

   15:   15/16 pixels off, almost invisible

   HDE: Hide all the tracts

   SLD: No stippling, solid line.



   .. _TractCont->Coloring_Controls->Masks:

**Masks**: Create/Switch to Masks controller


Opens controller for masks.
At the first click, this button creates a new interactive tract mask and activates menu items such as :ref:`Gry<TractCont->Coloring_Controls->Gry>`. A ball of a mask is added to the interface, and only tracts that go through it are displayed. 

.. figure:: media/MaskButtonInController.jpg
   :align: center

   ..

Clicking on Masks after the initialization brings up the :ref:`Mask Controller<MaskCont>`. See :ref:`mask manipulation mode<Mask_Manipulation_Mode>` for details on how to move the mask around.


   .. _TractCont->Coloring_Controls->Hde:

**Hde**: Choose how masked tracts are displayed.


That's not the name of the button, but its default value. This menu controls how tracts that fall outside of the masks are displayed
   Hde:   Hide 'em masked tracts

   Gry:   Gray 'em masked tracts (gray color set by :ref:`Gry<TractCont->Coloring_Controls->Gry>` arrow field)

   One:   A coding mistake that ended up looking cool. Each tract not in the mask is colored by one color extracted from the set of colors for the whole network.

   Ign:   Ignore 'em good for nothing masks, show tracts in all their unabashed glory



   .. _TractCont->Coloring_Controls->Gry:

**Gry**: Gray level (0--100) of tracts outside of mask (only for Msk --> Gry)


Set the gray level for masked tracts. 0 for black, 100 for white
   This arrow field only has an effect when 'Msk' menu is set to 'Gry'


   .. _TractCont->Coloring_Controls->Switch_Dset:

**Switch_Dset**: Switch between datasets


Select the dataset to which the Coloring Controls are being applied. For now you have three free RGB datasets per network that are created by SUMA. In the first one each node of a tract is colored based on the local orientation, with red, green, and blue values reflecting the X,Y, and Z components of the unit direction vector. In the second dataset all nodes of a tract are assigned the color of the middle node of that tract. In the third dataset, all nodes of a tract are colored based on the bundle in which that tract resides.The number of colors in such a dataset depend on the total number of bundles in the entire network.

.. _Mask_Manipulation_Mode:

Mask Manipulation Mode:
-----------------------

To move the mask interactively, right-double click on it to place SUMA in :term:`Mask Manipulation Mode` which is indicated by displaying the moveable mask in mesh mode (see help in SUMA, (:ref:`ctrl+h<LC_Ctrl+h>`), section :ref:`Button 3-DoubleClick<Button_3-DoubleClick>` for details.). Selecting a location on the tracts, the slices, or surfaces, will make the mask jump to that location. The mask should also be visibile in AFNI (if SUMA is launched with -dev option), and clicking in AFNI will make the mask move in SUMA also.

To turn off 'Mask Manipulation Mode' right-double click in open air, or on the manipulated mask itself.

.. figure:: media/MaskManipulationMode_OFF.jpg
   :align: left
   :figwidth: 45%

   Mask manipulation OFF.

.. figure:: media/MaskManipulationMode_ON.jpg
   :align: right
   :figwidth: 45%

   Mask manipulation ON.

.. container:: clearer

   .. image:: media/blank.jpg

   .. Preceding block is a trick to keep upcoming text from wrapping around the figures.




.. _MaskCont:

Network tracts mask controller:
-------------------------------

The mask controller is used for manipulating masks for network tractsYou can launch the :ref:`Mask Controller <MaskCont>` from the :ref:`tract controller <TractCont>` by clicking on :ref:`Masks<TractCont->Coloring_Controls->Masks>` twice.

.. figure:: media/MaskController.02.jpg
   :align: center


   ..




.. _MaskCont->Masks:

Create/delete masks and setup masking expression:
=================================================


   .. _MaskCont->Masks->Mask_Eval.r00:

**Mask_Eval**: Evaluate mask expression


A boolean expression evaluated per tract to determine whether or not a tract should be displayed. Each mask is assigned a letter from 'a' to 'z' and has an entry in the table below. Symbols for the OR operator are '|' or '+' while those for AND are '&' or '*'. The '|' is for the NOT operation. By default, the expression is blank, as indicated by '-', and the operation is an OR of all the masks.

.. _Tract_Tinting:

Tract_Tinting:
..............

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



   .. _MaskCont->Masks->Tract_Length.r00:

**Tract_Length**: Mask based on tract length


Set Min Max length for tract masking. Use can scroll (mouse wheel) in Min and Max cells to change the value. The 'v' button must be selected for masking to take effect.

   .. _MaskCont->Masks->Tract_Length->v:

**v**: Enable (ON)/Disable Tract Length masking



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


Load a set of masks previously saved by my neighboring button.


   .. _MaskCont->Masks->Save_Masks:

**Save_Masks**: Save the masks 


Save current set of masks for future loading by my neighborly button.


