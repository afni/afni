
.. _TractCont:

Network/Tracts Cont.
--------------------

The tract controller is for controlling the way tracts and values defined over them are displayed. Each network of tracts gets its own controller. You can launch the :ref:`Tract Controller <TractCont>` with: :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`

.. figure:: media/TractCont.auto.ALL.jpg
   :align: center


   ..




.. _TractCont->Tract_Properties:

Tract Properties
^^^^^^^^^^^^^^^^

Name and number of bundles, tracts, and points making up the  selected network of tracts. 

.. figure:: media/TractCont.auto.Tract_Properties.jpg
   :align: right

   ..



   .. _TractCont->Tract_Properties->more:

**more**: More info on network of tracts


   Opens a dialog with detailed information about the object in geek speak.

.. _TractCont->Xhair_Info:

Information at crosshair
^^^^^^^^^^^^^^^^^^^^^^^^



.. figure:: media/TractCont.auto.Xhair_Info.jpg
   :align: right

   ..



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

Coloring Controls
^^^^^^^^^^^^^^^^^

Controls the final coloration of the tracts based on the tract datasets available. What's a tract dataset you say? It is a dataset defined over the collection of points that define the tracts of a network. And where do we get these sets? Nowhere at the moment. For now they are generated internally and they are only of the RGB variety. This will change in the future, when you would be able to drive a flying car and have arbitrary sets much like on surfaces or volumes.

.. figure:: media/TractCont.auto.Coloring_Controls.jpg
   :align: right

   ..



   .. _TractCont->Coloring_Controls->Lbl.r00:

**Lbl**: Label of dataset displayed on tracts


   Label of dataset currently selected. Note that for some objects, like surfaces, what you're viewing at any moment maybe a blend of multiple datasets. See :ref:`color mixing<ColorMixing>` for details.

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


   That's not the name of the button, but its default value. This menu controls how tracts that fall outside of the masks are displayed:
   
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

Mask Manipulation Mode
----------------------

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



